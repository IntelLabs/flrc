(* COPYRIGHT_NOTICE_1 *)
(*
 * Translation from ANormLazy.t to ANormStrict.t.
 *)
signature ANORM_LAZY_TO_STRICT =
sig
(*  val valueTy : ANormLazy.ty -> ANormStrict.ty*)
  val pass : (ANormLazy.t, ANormStrict.t) Pass.t
end

structure ANormLazyToStrict :> ANORM_LAZY_TO_STRICT =
struct
  structure CH = CoreHs
  structure CU = CoreHsUtils
  structure CL = CoreHsLayout
  structure GP = GHCPrimType
  structure AL = ANormLazy
  structure AS = ANormStrict
  structure VD = Identifier.VariableDict
  structure IM = Identifier.Manager

  val passname = "ANormLazyToStrict"

  val fail = fn (f, msg) => Fail.fail (passname, f, msg)

  val variableSetInfo = 
   fn (im, v, ty) =>
      let 
        val () = IM.variableSetInfo (im, v, (ty, AS.VkLocal))
      in
        (v, ty)
      end

  val variableFresh =
   fn (im, s, vty) => IM.variableFresh (im, s, (vty, AS.VkLocal))

  val rec unCurryTy =
   fn t =>
      (case t
        of AL.Arr (t1, t2) =>
           let
             val (paramTys, resultTy) = unCurryTy t2
           in
             (t1 :: paramTys, resultTy)
           end
         | _ => ([], t))

  val rec multiValueTy =
   fn ty =>
      (case ty
        of AL.Prim (GP.Tuple ts) => List.map (ts, thunkTy)
         | _                     => [valueTy ty])

  and rec valueTy =
   fn ty =>
      (case ty 
        of AL.Data               => AS.Boxed
         | AL.Prim (GP.Tuple ts) => fail ("valueTy", "Got unexpected unboxed tuple")
         | AL.Prim t             => AS.Prim (GP.mapPrimTy (t, valueTy))
         | AL.Arr (t1, t2)       => AS.Arr ([thunkTy t1], multiValueTy t2)
         | AL.Tname v            => AS.Tname v
         | AL.Sum cons           => 
           AS.Sum (List.map (cons, fn (con, tys) => 
                                      (con, List.map (tys, fn (t, s) => if s then valueTy t else thunkTy t)))))

  and rec thunkTy' =
   fn ty =>
      (case ty
        of AS.Prim _ => ty
         | _         => AS.Thunk ty)

  and rec thunkTy =
   fn ty => thunkTy' (valueTy ty)

  val rec resultTys =
   fn (n, b) =>
      (case (n, b)
        of  (_, AS.Boxed)        => [AS.Boxed]
          | (0, t)               => [t]
          | (1, AS.Arr (_, ts))  => ts
          | (n, AS.Arr (_, [t])) => resultTys (n - 1, t)
          | (n, AS.Arr (_, _))   => fail ("resultTys", "Multiple results")
          | _                    => fail ("resultTys", "expect function type"))

  val lookupEnv =
   fn (env, v) =>
      (case VD.lookup (env, v)
        of SOME w => w
         | NONE   => AS.Thunk AS.Boxed (* TODO: shall we error here? *))
 
  val rec valueExp =
   fn (im, env, tys, e) =>
      (case e
        of AL.Var v => (case lookupEnv (env, v)
                         of AS.Thunk _ => AS.Eval v
                          | _          => AS.Return [v])
         | AL.PrimApp (f, vs) => AS.PrimApp (f, vs)
         | AL.ConApp (c, vs) => 
           let
             val folder = 
              fn ((v, s), (us, lets)) =>
                 case (s, lookupEnv (env, v))
                  of (true, AS.Thunk uty) =>
                     let 
                       val u = IM.variableClone (im, v)
                       val _ = variableSetInfo (im, u, uty)
                     in
                       (u :: us, fn e => AS.Let (AS.Vdef ([(u, uty)], AS.Eval v), lets e))
                     end
                   | _ => (v :: us, lets)
             val (us, lets) = List.foldr (vs, ([], fn e => e), folder)
           in 
             lets (AS.ConApp (c, us))
           end
         | AL.Multi vs => AS.Return vs  (* unboxed tuple is non-strict *)
         | AL.ExtApp (p, CH.Label, f, ty, vs) => AS.ExtApp (p, CH.Label, f, valueTy ty, vs)
         | AL.ExtApp (p, cc, f, ty, vs) => 
           let 
             val (paramTys, resultTy) = unCurryTy ty
             val paramTys = List.map (paramTys, valueTy)
             val resultTys = multiValueTy resultTy
             val lastIsState = List.length paramTys > 0 andalso 
                               (case List.last paramTys of (AS.Prim (GP.State _)) => true | _ => false)
             (* Deal both cases: with or without multiReturn enabled *)
             val uTuple = 
                 case resultTy
                  of AL.Prim (GP.Tuple _) => true
                   | _ => false
           in
             case (lastIsState, uTuple, resultTys)  (* handle IO externs differently *)
              of (true, true, _::vty::[]) =>  (* IO a *)
                 let
                   val paramTys = List.allButLast paramTys
                   val v = variableFresh (im, "tsepp", vty)
                   val fty = AS.Arr (paramTys, [vty])
                   val vdef = AS.Vdef ([(v, vty)], AS.ExtApp (p, cc, f, fty, List.allButLast vs))
                   val s = List.last vs
                 in
                   AS.Let (vdef, AS.Return [s,v])
                 end
            | (true, true, _::[]) => (* IO () *)
              let
                val paramTys = List.allButLast paramTys
                val fty = AS.Arr (paramTys, [])
                val vdef = AS.Vdef ([], AS.ExtApp (p, cc, f, fty, List.allButLast vs))
                val s = List.last vs
              in
                AS.Let (vdef, AS.Return [s])
              end
            | (true, true, _::wtys) => (* IO (# ... #) *)
              fail ("valueExp", "unhandled IO extern returns unboxed tuple: " ^ f)
              (*
              let
                val paramTys = List.allButLast paramTys
                val con' = (IM.nameMake (im, CL.qNameToString (CU.dcUtuple (List.length wtys))), 0)
                val uty = AS.Sum [(con', wtys)]
                val u = IM.variableFresh (im, "", uty)
                val vty = AS.Prim (GP.Tuple wtys)
                val v = IM.variableFresh (im, "", vty)
                val fty = AS.Arr (paramTys, vty)
                val vdef = AS.Vdef (v, vty, AS.ExtApp (p, cc, f, fty, List.allButLast vs))
                val udef = AS.Vdef (u, uty, AS.Cast (v, false))
                val s = List.last vs
                val ws = List.map (wtys, fn ty => IM.variableFresh (im, "", ty))
              in
                AS.Let (vdef, AS.Let (udef, 
                         AS.Case (u, [AS.Acon (con', List.zip (ws, wtys), AS.ConApp (con, s :: ws))])))
              end
              *)
            | (false, _, wtys) => (* (# ... #) FIXME: can it be Bool or other sum type? *)
              fail ("valueExp", "unhandled extern returns unboxed tuple: " ^ f) 
            (*
              let
                val vty = AS.Prim (GP.Tuple wtys)
                val v = IM.variableFresh (im, "", vty)
                val fty = AS.Arr (paramTys, vty)
                val vdef = AS.Vdef (v, vty, AS.ExtApp (p, cc, f, fty, vs))
              in
                AS.Let (vdef, AS.Cast (v, false))
              end
            *)
            | _ =>             
              let
                val fty = AS.Arr (paramTys, resultTys)
              in 
                AS.ExtApp (p, cc, f, fty, vs)
              end
           end
         | AL.App (e, v) => 
           let
             val vty = lookupEnv (env, v)
             val ety = AS.Arr ([vty], tys)
             val e = valueExp (im, env, [ety], e)
           in
             case e
              of AS.Return [f] => AS.App (f, [v])
               | _ => 
                 let 
                   val u = variableFresh (im, "tsapp", ety)
                 in
                   AS.Let (AS.Vdef ([(u, ety)], e), AS.App (u, [v]))
                 end                   
           end
         | AL.Lam ((v, vty), e) =>
           let
             val vty = thunkTy vty
             val _   = variableSetInfo (im, v, vty)
             val env = VD.insert (env, v, vty)
             val ty = case tys
                       of [ty] => ty
                        | _    => fail ("valueExp", "bad number of types for lambda")
             val etys = resultTys (1, ty)
             val e = valueExp (im, env, etys, e)
             val u  = variableFresh (im, "tslam", ty)
             (* Even syntactic non-rec functions might be semantically recursive *)
             val vd = AS.Vfun {name = u, ty = ty, escapes = true, recursive = true, 
                               fvs = [], args = [(v, vty)], body = e}
           in 
             AS.Let (AS.Nonrec vd, AS.Return [u])
           end
         | AL.Let (vdefg, e) =>
           let
             val (vdefg, env) = thunkVDefg (im, env, vdefg)
             val e = valueExp (im, env, tys, e)
           in 
             AS.Let (vdefg, e)
           end
         | AL.Case (e, (v, vty), ty, alts) =>
           let
             val tys = multiValueTy ty
             val vty = valueTy vty
             val e  = valueExp (im, env, [vty], e)
             val u = variableFresh (im, "tsscr", vty)
             val udef = AS.Vdef ([(u, vty)], e)
             (* val vty  = thunkTy' ety *)
             val vty = thunkTy' vty
             val _ = variableSetInfo (im, v, vty)
             val vdef = case vty
                         of AS.Thunk _ => 
                            (case e
                              of AS.Eval w => AS.Vdef ([(v, vty)], AS.Return [w])
                               | _         => AS.Nonrec (AS.Vthk {name = v, ty = vty, 
                                                                  escapes = true, recursive = false, 
                                                                  fvs = [], body = AS.Return [u]}))
                          | _ => AS.Vdef ([(v, vty)], AS.Return [u])
             val env = VD.insert (env, v, vty)
             val alts = List.map (alts, fn alt => valueAlt (im, env, tys, alt))
           in
             case alts
               of [] => AS.Let (udef, AS.Let (vdef, AS.Cast (AS.Bottom u)))
                | _  => AS.Let (udef, AS.Let (vdef, AS.Case (u, alts)))
           end
         | AL.Lit (l, ty) => let val ty = valueTy ty in AS.Lit (l, ty) end
         | AL.Cast (e, vty, ty) => 
           let
             val vtys = multiValueTy vty
             val e = valueExp (im, env, vtys, e)
             val bind =
              fn bodyF =>
                 (case (e, vtys)
                   of (AS.Return [u], _) => bodyF u
                    | (AS.Return _, _)   => fail ("valueExp", "Too many/few returns in cast")
                    | (e, [vty])         => 
                      let
                        val v = variableFresh (im, "tscst", vty)
                      in AS.Let (AS.Vdef ([(v, vty)], e), bodyF v)
                      end
                    | (e, _)             => fail ("valueExp", "Too many/few types in cast"))
             val cast =
                 case (vty, ty)
                  of (AL.Prim GP.Addr, AL.Prim (GP.StablePtr _)) => bind (fn v => AS.Return [v])
                   | (AL.Prim (GP.StablePtr _), AL.Prim GP.Addr) => bind (fn v => AS.Return [v])
                   | (AL.Prim _, AL.Prim GP.Addr)                => bind (fn v => AS.Cast (AS.ToAddr v))
                   | (AL.Prim _, AL.Prim (GP.StablePtr _))       => bind (fn v => AS.Cast (AS.ToAddr v))
                   | (AL.Prim (GP.StablePtr _), AL.Prim _)       => bind (fn v => AS.Cast (AS.FromAddr v))
                   | (AL.Prim GP.Addr, AL.Prim _)                => bind (fn v => AS.Cast (AS.FromAddr v))
                   | (AL.Prim Int, AL.Prim GP.Ref)               => AS.Cast AS.NullRef
                   | (AL.Prim (GP.Tuple _), _)                   => e
                   | (AL.Prim _, AL.Prim _)                      => bind (fn v => AS.Return [v]) (* Prevent this? *)
                   | (_, AL.Prim _)                              => bind (fn v => AS.Cast (AS.Bottom v))
                   | _                                           => bind (fn v => AS.Return [v])
           in cast
           end)

  and rec valueAlt =
   fn (im, env, etys, alt) =>
      (case alt 
        of AL.Acon (con, vbs, exp) =>
           let
             val isPrim = 
              fn t => (case t of AL.Prim _ => true | _ => false)
             val folder =
              fn ((v, ty, s), (ubs, vbs, lets)) => 
                 if s andalso not (isPrim ty) 
                 then 
                   let
                     val u = IM.variableClone (im, v)
                     val (u, uty) = variableSetInfo (im, u, valueTy ty)
                     val (v, vty) = variableSetInfo (im, v, thunkTy' uty)
                     val lets = fn e => AS.Let (AS.Nonrec (AS.Vthk {name = v, ty = vty, 
                                                                    escapes = true, recursive = false, 
                                                                    fvs = [], body = AS.Return [u]}), lets e)
                   in 
                     ((u, uty) :: ubs, (v, vty) :: vbs, lets)
                    end
                 else 
                   let
                     val vs = variableSetInfo (im, v, thunkTy ty)
                   in
                     (vs :: ubs, vs :: vbs, lets)
                   end
             val (ubs, vbs, lets) = 
                 List.foldr (vbs, ([], [], fn e => e), folder)
             val env = List.fold (vbs, env, fn ((v, ty), env') => VD.insert (env', v, ty))
             val exp = lets (valueExp (im, env, etys, exp))
           in 
             AS.Acon (con, ubs, exp)
           end
       | AL.Alit (lit, t, exp) => AS.Alit (lit, valueTy t, valueExp (im, env, etys, exp))
       | AL.Adefault exp => AS.Adefault (valueExp (im, env, etys, exp)))

  and rec recVDef = 
   fn (im, env, vd) => 
      (case vd
        of (AL.Vdef (AL.VbSingle (v, vty), e)) =>
           let
             val ety = valueTy vty
             val e = valueExp (im, env, [ety], e)
             val (f, ty)  = case ety
                             of AS.Prim _ => fail ("recVDef", "primitive value binding in let rec is not allowed")
                              | _         => (fn (v, ty, e) => AS.Vthk {name = v, ty = ty, 
                                                                        escapes = true, recursive = true, 
                                                                        fvs = [], body = e}, thunkTy' ety)
             val _ = variableSetInfo (im, v, ty)
             val env = VD.insert(env, v, ty)
           in
             (f (v, ty, e), env)
           end
         | (AL.Vdef (_, e)) => fail ("recVDef", "multi-return in let rec is not allowed"))

  and rec nonrecVDef =
   fn (im, env, alt) =>
      (case alt
        of (AL.Vdef (bind, e)) => 
           let
             fun setInfo ((v, ty), env) = let val _ = variableSetInfo (im, v, ty) in VD.insert (env, v, ty) end
           in
             case bind
              of AL.VbSingle (v, vty) => 
                 let
                   val vty = valueTy vty
                   val e = valueExp (im, env, [vty], e)
                 in 
                   (case vty
                     of AS.Prim _ => 
                        let
                          val vd = AS.Vdef ([(v, vty)], e)
                          val env = setInfo ((v, vty), env)
                        in (vd, env)
                        end
                      | _         => 
                        let
                          val tty = AS.Thunk vty
                          (* Even syntactic non-rec thunks might be semantically recursive *)
                          val vd = AS.Nonrec (AS.Vthk {name = v, ty = tty, escapes = true, recursive = true, 
                                                       fvs = [], body = e})
                          val env = setInfo ((v, tty), env)
                        in (vd, env)
                        end)
                 end
               | AL.VbMulti vts => 
                 let
                   val (vs, tys) = List.unzip vts
                   val ttys = List.map (tys, thunkTy)
                   val e = valueExp (im, env, ttys, e)
                   val vts = List.zip (vs, ttys)
                   val vd = AS.Vdef (vts, e)
                   val env = List.fold (vts, env, setInfo)
                 in (vd, env)
                 end
           end)

  and rec thunkVDefg =
   fn (im, env, vdg) => 
      (case vdg
        of AL.Rec vdefs =>
           let
             fun mkThunk (AL.Vdef (AL.VbSingle (v, ty), _)) = (v, thunkTy ty)
               | mkThunk _ = fail ("thunkVDefg", "multi-return cannot be recursively defined")
             val vartys = List.map (vdefs, mkThunk)
             val env    = List.fold (vartys, env, fn ((v, ty), env) => VD.insert (env, v, ty))
             val vdefs  = List.map (vdefs, fn vd => #1 (recVDef (im, env, vd)))
           in
             (AS.Rec vdefs, env)
           end
         | AL.Nonrec vdef =>
           let
             val (vdef, env) = nonrecVDef (im, env, vdef)
           in
             (vdef, env)
           end)

  fun doModule (AL.Module (main, vdefgs), im) =
      let 
        val im = IM.fromExistingNoInfo im 
        fun oneVDefg (vdefg, (vdefgs, env))
          = let 
              val (vdefg, env) = thunkVDefg (im, env, vdefg)
          in
            (vdefg :: vdefgs, env)
          end
        val (vdefgs, _) = List.fold (vdefgs, ([], VD.empty), oneVDefg)
      in 
        (AS.Module (main, List.rev vdefgs), IM.finish im)
      end

  fun toStrict (x, pd) = doModule x

  fun layoutLazy (module, config) = ANormLazyLayout.layoutModule module

  fun layoutStrict (module, config) = ANormStrictLayout.layout (config, module)

  val description = {name        = passname,
                     description = "Lazy A-Normal Form to strict A-Normal Form",
                     inIr        = { printer = layoutLazy,
                                     stater  = layoutLazy },
                     outIr       = { printer = layoutStrict,
                                     stater  = layoutStrict },
                     mustBeAfter = [],
                     stats       = []}

  val associates = {controls = [], debugs = [], features = [], subPasses = []}

  val pass = Pass.mkCompulsoryPass (description, associates, toStrict)

end
