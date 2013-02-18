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
  structure I  = Identifier
  structure ND = I.NameDict
  structure VD = I.VariableDict
  structure IM = I.Manager

  val passname = "ANormLazyToStrict"

  val fail = fn (f, msg) => Fail.fail (passname, f, msg)

  type state = { im : AS.symbolTableManager, tm : AS.typeManager } 

  type env = { cfg : Config.t, tyenv : AS.ty VD.t }

  val insertTyEnv = 
   fn ({ cfg, tyenv }, v, ty) => { cfg = cfg, tyenv = VD.insert (tyenv, v, ty) }

  val variableSetInfo = 
   fn (im, v, ty) =>
      let 
        val () = IM.variableSetInfo (im, v, (ty, AS.VkLocal))
      in
        (v, ty)
      end

  val variableFresh =
   fn (im, hint, s, vty) => IM.variableFresh (im, hint^"_"^s, (vty, AS.VkLocal))

  val rec unCurryTy =
   fn t =>
      (case TypeRep.repToBase t
        of AL.Arr (t1, t2, effect) => (* TODO: fix effect *)
           let
             val (paramTys, resultTy) = unCurryTy t2
           in
             (t1 :: paramTys, resultTy)
           end
         | _ => ([], t))

  val rec multiValueTy =
   fn (tm, ty) =>
      (case TypeRep.repToBase ty
        of AL.Prim (GP.Tuple ts) => List.map (ts, fn t => thunkTy (tm, t))
         | _                     => [valueTy (tm, ty)])

  and rec valueTy = 
   fn (tm, ty) =>
      let
        val ty = 
            case TypeRep.repToBase ty 
              of AL.Data               => AS.Boxed
               | AL.Prim (GP.Tuple ts) => fail ("valueTy", "Got unexpected unboxed tuple")
               | AL.Prim t             => AS.Prim (GP.mapPrimTy (t, fn t => valueTy (tm, t)))
               | AL.Arr (t1, t2, effect) => AS.Arr ([thunkTy (tm, t1)], multiValueTy (tm, t2), 
                                                    (* default effect is partial+throws+returns+fails *)
                                                    Utils.Option.out (effect, fn _ => Effect.Control))
               | AL.Sum cons           => 
                 AS.Sum (List.map (cons, fn (con, tys) => 
                                            (con, List.map (tys, fn (t, strict) => thunkTy (tm, t)))))
      in
        TypeRep.newRep (tm, ty)
      end

  and rec thunkTy' =
   fn (tm, ty) =>
      (case TypeRep.repToBase ty
        of AS.Prim _ => ty
         | _         => TypeRep.newRep (tm, AS.Thunk ty))

  and rec thunkTy = 
   fn (tm, ty) => thunkTy' (tm, valueTy (tm, ty))

  val boxedTy = TypeRep.newRep_ AS.Boxed

  val boxedThunkTy = TypeRep.newRep_ (AS.Thunk boxedTy)

  val rec resultTys =
   fn (n, b) =>
      (case (n, TypeRep.repToBase b)
        of  (_, AS.Boxed)           => [b]
          | (0, t)                  => [b]
          | (1, AS.Arr (_, ts, _))  => ts
          | (n, AS.Arr (_, [t], _)) => resultTys (n - 1, t)
          | (n, AS.Arr (_, _, _))   => fail ("resultTys", "Multiple results")
          (*
          | (_, AS.Prim _ ) => fail ("resultTys", "got prim type")
          | (_, AS.Sum _ ) => fail ("resultTys", "got sum type")
          | (_, AS.Tname _ ) => fail ("resultTys", "got Tname")
          | (_, AS.Thunk _ ) => fail ("resultTys", "got Thunk")
          *)
          | _                       => fail ("resultTys", "expect function type"))

  val lookupEnv =
   fn (state, env, v) =>
      (case VD.lookup (#tyenv env, v)
        of SOME w => w
         | NONE   => fail ("lookupEnv", "type of variable " ^ IM.variableString (#im state, v) ^ " not found")) (* TODO: shall we error here? *)

  (* handle recursive cast of function types *)
  val rec cast = 
   fn (x as (_, _, _, e, _, _)) => 
      case cast' x 
        of SOME e => e
         | NONE   => e

  and rec cast' =
   fn (state, env, hint, e, t1, t2) =>
    let
      val { im, tm } = state
    in
      case (List.map (t1, TypeRep.repToBase), List.map (t2, TypeRep.repToBase))
        of ([AS.Arr (t11, t12, _)], [AS.Arr (t21, t22, effect)]) =>
          let
            val t1 = hd t1
            val t2 = hd t2
            val f = variableFresh (im, hint, "f", t1)
            val g = variableFresh (im, hint, "g", t2)
            val xs = List.map (t21, fn t => variableFresh (im, hint, "x", t))
            val ys = List.map (t11, fn t => variableFresh (im, hint, "y", t))
            val zs = List.map (t22, fn t => variableFresh (im, hint, "z", t))
            val retxs  = AS.Return xs
            val retfys = AS.App (f, ys, effect)
            (* 
             * let f = e
             *     g = \x -> let y = cast (x, t21, t11)
             *                   z = cast (f y, t12, t22)
             *                in z
             * in g
             *)
            fun mk (retxs, retfys) = SOME (
                AS.Let (AS.Vdef ([(f, t1)], e),
                  AS.Let (AS.Nonrec (AS.Vfun { name = g, ty = t2,  escapes = true, recursive = true, 
                                               fvs = [], args = List.zip (xs, t21), body = 
                      AS.Let (AS.Vdef (List.zip (ys, t11), retxs),
                        AS.Let (AS.Vdef (List.zip (zs, t22), retfys), AS.Return zs))}), 
                    AS.Return [g])))
          in
            case (cast' (state, env, hint, retxs, t21, t11), cast' (state, env, hint, retfys, t12, t22))
              of (NONE, NONE)              => NONE
               | (SOME retxs, NONE)        => mk (retxs, retfys)
               | (NONE, SOME retfys)       => mk (retxs, retfys)
               | (SOME retxs, SOME retfys) => mk (retxs, retfys)
          end
         | ([_], [_]) => 
          let
            val t1 = hd t2
            val t2 = hd t2
            val bind =
             fn bodyF =>
                case e
                  of AS.Return [u] => SOME (bodyF u)
                   | e =>
                    let
                      val v = variableFresh (im, hint, "tscst", t1)
                    in 
                      SOME (AS.Let (AS.Vdef ([(v, t1)], e), bodyF v))
                    end
          in
            case (TypeRep.repToBase t1, TypeRep.repToBase t2)
              of (AS.Prim GP.Addr, AS.Prim (GP.StablePtr _)) => NONE
               | (AS.Prim (GP.StablePtr _), AS.Prim GP.Addr) => NONE
               | (AS.Prim _, AS.Prim GP.Addr)                => bind (fn v => AS.Cast (AS.ToAddr v))
               | (AS.Prim _, AS.Prim (GP.StablePtr _))       => bind (fn v => AS.Cast (AS.ToAddr v))
               | (AS.Prim (GP.StablePtr _), AS.Prim _)       => bind (fn v => AS.Cast (AS.FromAddr v))
               | (AS.Prim GP.Addr, AS.Prim _)                => bind (fn v => AS.Cast (AS.FromAddr v))
               | (AS.Prim Int, AS.Prim GP.Ref)               => SOME (AS.Cast AS.NullRef)
               | (AS.Prim _, AS.Prim _)                      => NONE (* shall we error here? *)
               | (_, AS.Prim _)                              => bind (fn v => AS.Cast (AS.Bottom v))
               | _                                           => NONE 
          end
         | _ => 
          if length t1 <> length t2 then 
            let 
              val ut = case t1 
                         of [t] => t
                          | _ => fail ("cast", "cast from multi to non-equal multi value not supported")
              val u = variableFresh (im, hint, "u", ut)
              val vs = List.map (t2, fn t => variableFresh (im, hint, "bot", t))
              val vts = List.zip (vs, t2)
              val lets = List.foldr (vts, AS.Return vs, 
                           fn ((v, t), l) => AS.Let (AS.Vdef ([(v,t)], AS.Cast (AS.Bottom u)), l))
            in
              SOME (AS.Let (AS.Vdef ([(u, ut)], e), lets))
            end
          else 
            let 
              val us = List.map (t1, fn t => variableFresh (im, hint, "u", t))
              val vs = List.map (t2, fn t => variableFresh (im, hint, "v", t))
              val uts = List.zip (us, t1)
              val vts = List.zip (vs, t2)
              val uvts = List.zip (uts, vts)
              val casts = List.map (uvts, fn ((v, vt), (u, ut)) => cast' (state, env, hint, AS.Return [u], [ut], [vt]))
            in
              if List.forall (casts, fn x => case x of NONE => true | SOME _ => false)
                then NONE
                else 
                  let
                    val uvs = List.map (List.zip (casts, List.zip (us, vs)),
                                fn (c, (u, v)) => case c of SOME _ => v | NONE => u)
                    val lets = List.foldr (List.zip (casts, vts), AS.Return uvs,
                          fn ((c, (v, vt)), l) => 
                            case c 
                              of SOME e => AS.Let (AS.Vdef ([(v, vt)], e), l)
                               | NONE   => l)
                    val lets = AS.Let (AS.Vdef (uts, e), lets)
                  in
                    SOME lets
                  end
            end
      end
 
  and rec valueExp =
   fn (state, env, hint, tys, e, effectful) =>
    let
      val { im, tm } = state
    in
      case e
        of AL.Var v => (case TypeRep.repToBase (lookupEnv (state, env, v))
                         of AS.Thunk _ => AS.Eval v
                          | _          => AS.Return [v])
         | AL.PrimApp (f, vs) => AS.PrimApp (f, vs)
         | AL.ConApp (c, vs) => 
           let
             val folder = 
              fn ((v, strict), (us, lets)) =>
                let
                   val uty = lookupEnv (state, env, v)
                in
                  case (strict, TypeRep.repToBase uty)
                    of (true, AS.Thunk wty) =>
                       let 
                         val u = IM.variableClone (im, v)
                         val w = IM.variableClone (im, v)
                         val _ = variableSetInfo (im, u, uty)
                         val _ = variableSetInfo (im, w, wty)
                       in
                         (u :: us, fn e => AS.Let (AS.Vdef ([(w, wty)], AS.Eval v), 
                                             AS.Let (AS.Nonrec (AS.Vthk { name = u, ty = uty, escapes = true, 
                                                                          recursive = false, fvs = [w], 
                                                                          body = AS.Return [w] }), lets e)))
                       end
                    | _ => (v :: us, lets)
               end
             val (us, lets) = List.foldr (vs, ([], fn e => e), folder)
           in 
             lets (AS.ConApp (c, us))
           end
         | AL.Multi vs => AS.Return vs  (* unboxed tuple is non-strict *)
         | AL.ExtApp (p, CH.Label, f, ty, vs) => AS.ExtApp (p, CH.Label, f, valueTy (tm, ty), vs)
         | AL.ExtApp (p, cc, f, ty, vs) => 
           let 
             val (paramTys, resultTy) = unCurryTy ty
             val paramTys = List.map (paramTys, fn t => valueTy (tm, t))
             val rTys = multiValueTy (tm, resultTy)
             val lastIsState = List.length paramTys > 0 andalso 
                               (case TypeRep.repToBase (List.last paramTys) of (AS.Prim (GP.State _)) => true | _ => false)
             (* Deal both cases: with or without multiReturn enabled *)
             val uTuple = 
                 case TypeRep.repToBase resultTy
                  of AL.Prim (GP.Tuple _) => true
                   | _ => false
           in
             case (lastIsState, uTuple, rTys)  (* handle IO externs differently *)
              of (true, true, _::vty::[]) =>  (* IO a *)
                 let
                   val paramTys = List.allButLast paramTys
                   val v = variableFresh (im, hint, "tsepp", vty)
                   val fty = TypeRep.newRep (tm, AS.Arr (paramTys, [vty], Effect.Any))
                   val vdef = AS.Vdef ([(v, vty)], AS.ExtApp (p, cc, f, fty, List.allButLast vs))
                   val s = List.last vs
                 in
                   AS.Let (vdef, AS.Return [s,v])
                 end
            | (true, true, _::[]) => (* IO () *)
              let
                val paramTys = List.allButLast paramTys
                val fty = TypeRep.newRep (tm, AS.Arr (paramTys, [], Effect.Any))
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
                val fty = TypeRep.newRep (tm, AS.Arr (paramTys, rTys, Effect.Any))
              in 
                AS.ExtApp (p, cc, f, fty, vs)
              end
           end
         | AL.App (e, v) => 
           let
             val vty = lookupEnv (state, env, v)
             val effect = if effectful then Effect.Any else Effect.Control
             val ety = TypeRep.newRep (tm, AS.Arr ([vty], tys, effect))
             val e = valueExp (state, env, hint, [ety], e, false)
           in
             case e
              of AS.Return [f] => AS.App (f, [v], effect)
               | _ => 
                 let 
                   val u = variableFresh (im, hint, "tsapp", ety)
                 in
                   AS.Let (AS.Vdef ([(u, ety)], e), AS.App (u, [v], effect))
                 end                   
           end
         | AL.Lam ((v, vty, strict), e) =>
           let
             val vty = thunkTy (tm, vty)
             val _   = variableSetInfo (im, v, vty)
             val env = insertTyEnv (env, v, vty)
             val ty = case tys
                       of [ty] => ty
                        | _    => fail ("valueExp", "bad number of types for lambda")
             val etys = resultTys (1, ty)
             val hint = IM.variableName (im, v)
             val e = valueExp (state, env, hint, etys, e, false)
             val u  = variableFresh (im, hint, "tslam", ty)
             (* Even syntactic non-rec functions might be semantically recursive *)
             val vd = AS.Vfun {name = u, ty = ty, escapes = true, recursive = true, 
                               fvs = [], args = [(v, vty)], body = e}
           in 
             AS.Let (AS.Nonrec vd, AS.Return [u])
           end
         | AL.Let (vdefg, e) =>
           let
             val (vdefgs, env) = thunkVDefg (state, env, hint, vdefg)
             val e = valueExp (state, env, hint, tys, e, false)
           in 
             List.foldr (vdefgs, e, AS.Let) 
           end
         | AL.Case (e, (v, vty), ty, alts) =>
           let
             val tys = multiValueTy (tm, ty)
             val vty = valueTy (tm, vty)
             val e  = valueExp (state, env, hint, [vty], e, false)
             val u = variableFresh (im, hint, "tsscr", vty)
             val udef = AS.Vdef ([(u, vty)], e)
             (* val vty  = thunkTy' ety *)
             val vty = thunkTy' (tm, vty)
             val _ = variableSetInfo (im, v, vty)
             val vdef = case TypeRep.repToBase vty
                         of AS.Thunk _ => 
                            (case e
                              of AS.Eval w => AS.Vdef ([(v, vty)], AS.Return [w])
                               | _         => AS.Nonrec (AS.Vthk {name = v, ty = vty, 
                                                                  escapes = true, recursive = false, 
                                                                  fvs = [u], body = AS.Return [u]}))
                          | _ => AS.Vdef ([(v, vty)], AS.Return [u])
             val env = insertTyEnv (env, v, vty)
             val alts = List.map (alts, fn alt => valueAlt (state, env, hint, tys, alt))
           in
             case alts
               of [] => AS.Let (udef, AS.Let (vdef, AS.Cast (AS.Bottom u)))
                | _  => AS.Let (udef, AS.Let (vdef, AS.Case (u, alts)))
           end
         | AL.Lit (l, ty) => let val ty = valueTy (tm, ty) in AS.Lit (l, ty) end
         | AL.Cast (e, t1, t2) => 
           let 
             val t1 = multiValueTy (tm, t1)
             val t2 = multiValueTy (tm, t2)
             val e  = valueExp (state, env, hint, t1, e, false)
           in
             cast (state, env, hint, e, t1, t2)
           end
    end

  and rec valueAlt =
   fn (state, env, hint, etys, alt) =>
    let
      val {im, tm} = state
    in
      case alt 
        of AL.Acon (con, vbs, exp) =>
           let
             val isPrim = 
              fn t => (case TypeRep.repToBase t of AL.Prim _ => true | _ => false)
             val folder =
              fn ((v, ty, s), (ubs, vbs, lets)) => 
                (* 
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
                *)
                   let
                     val vs = variableSetInfo (im, v, thunkTy (tm, ty))
                   in
                     (vs :: ubs, vs :: vbs, lets)
                   end
             val (ubs, vbs, lets) = 
                 List.foldr (vbs, ([], [], fn e => e), folder)
             val env = List.fold (vbs, env, fn ((v, ty), env) => insertTyEnv (env, v, ty))
             val exp = lets (valueExp (state, env, hint, etys, exp, false))
           in 
             AS.Acon (con, ubs, exp)
           end
       | AL.Alit (lit, t, exp) => AS.Alit (lit, valueTy (tm, t), valueExp (state, env, hint, etys, exp, false))
       | AL.Adefault exp => AS.Adefault (valueExp (state, env, hint, etys, exp, false))
    end

  and rec recVDef = 
   fn (state, env, hint, vd) => 
    let
      val {im, tm} = state
    in
      case vd
        of (AL.Vdef (AL.VbSingle (v, vty, _), e)) => 
           let
             val ety = valueTy (tm, vty)
             val hint = IM.variableName (im, v)
             val e = valueExp (state, env, hint, [ety], e, false)
             val (f, ty)  = case TypeRep.repToBase ety
                             of AS.Prim _ => fail ("recVDef", "primitive value binding in let rec is not allowed")
                              | _         => (fn (v, ty, e) => AS.Vthk {name = v, ty = ty, 
                                                                        escapes = true, recursive = true, 
                                                                        fvs = [], body = e}, thunkTy' (tm, ety))
             val _ = variableSetInfo (im, v, ty)
             val env = insertTyEnv (env, v, ty)
           in
             (f (v, ty, e), env)
           end
         | (AL.Vdef (_, e)) => fail ("recVDef", "multi-return in let rec is not allowed")
    end

  and rec nonrecVDef =
   fn (state, env, hint, alt) =>
    let 
      val { im, tm } = state
    in
      case alt
        of (AL.Vdef (bind, e)) => 
           let
             fun setInfo ((v, ty), env) = let val _ = variableSetInfo (im, v, ty) in insertTyEnv (env, v, ty) end
           in
             case bind
              of AL.VbSingle (v, vty, strict) => 
                 let
                   val vty = valueTy (tm, vty)
                   val hint = IM.variableName (im, v)
                   val e = valueExp (state, env, hint, [vty], e, false)
                 in 
                   (case TypeRep.repToBase vty
                     of AS.Prim _ => 
                        let
                          val vd = AS.Vdef ([(v, vty)], e)
                          val env = setInfo ((v, vty), env)
                        in ([vd], env)
                        end
                      | _         => 
                       if strict
                         then
                           let 
                             val u = IM.variableClone (im, v)
                             val _ = variableSetInfo (im, u, vty)
                             val vd0 = AS.Vdef ([(u, vty)], e)
                             val tty = TypeRep.newRep (tm, AS.Thunk vty)
                             (* Even syntactic non-rec thunks might be semantically recursive *)
                             val vd1 = AS.Nonrec (AS.Vthk {name = v, ty = tty, escapes = true, recursive = true, 
                                                          fvs = [], body = AS.Return [u]})
                             val env = setInfo ((v, tty), env)
                           in
                             ([vd0, vd1], env)
                           end
                         else
                           let
                             val tty = TypeRep.newRep (tm, AS.Thunk vty)
                             (* Even syntactic non-rec thunks might be semantically recursive *)
                             val vd = AS.Nonrec (AS.Vthk {name = v, ty = tty, escapes = true, recursive = true, 
                                                          fvs = [], body = e})
                             val env = setInfo ((v, tty), env)
                           in ([vd], env)
                           end)
                 end
               | AL.VbMulti (vts, effectful) => 
                 let
                   val (vs, tys) = List.unzip vts
                   val ttys = List.map (tys, fn t => thunkTy (tm, t))
                   val e = valueExp (state, env, hint, ttys, e, effectful)
                   val vts = List.zip (vs, ttys)
                   val vd = AS.Vdef (vts, e)
                   val env = List.fold (vts, env, setInfo)
                 in ([vd], env)
                 end
           end
    end

  and rec thunkVDefg =
   fn (state, env, hint, vdg) => 
    let
      val {im, tm} = state
    in
      case vdg
        of AL.Rec vdefs =>
           let
             fun mkThunk (AL.Vdef (AL.VbSingle (v, ty, strict), _)) = (v, thunkTy (tm, ty))
               | mkThunk _ = fail ("thunkVDefg", "multi-return cannot be recursively defined")
             val vartys = List.map (vdefs, mkThunk)
             val env    = List.fold (vartys, env, fn ((v, ty), env) => insertTyEnv (env, v, ty))
             val vdefs  = List.map (vdefs, fn vd => #1 (recVDef (state, env, hint, vd)))
           in
             ([AS.Rec vdefs], env)
           end
         | AL.Nonrec vdef =>
           let
             val (vdefs, env) = nonrecVDef (state, env, hint, vdef)
           in
             (vdefs, env)
           end
    end

  fun doModule ((AL.Module (main, vdefgs), im, otm), pd) =
      let 
        val cfg = PassData.getConfig pd
        val im = IM.fromExistingNoInfo im 
        val tm = TypeRep.newManager (AS.hashTy_, AS.eqTy_)
        val state = { im = im, tm = tm }
        val env = { cfg = cfg, tyenv = VD.empty }
        fun oneVDefg (vdefg, (vdefgs, env))
          = let 
              val (vdefg, env) = thunkVDefg (state, env, "top", vdefg)
          in
            (vdefg :: vdefgs, env)
          end
        val (vdefgs, _) = List.fold (vdefgs, ([], env), oneVDefg)
      in 
        (AS.Module (main, List.rev (List.concat vdefgs)), IM.finish im, tm)
      end

  fun layoutLazy (module, config) = ANormLazyLayout.layoutModule module

  fun layoutStrict (module, config) = ANormStrictLayout.layout (config, module)

  val description = {name        = passname,
                     description = "Lazy A-Normal Form to strict A-Normal Form",
                     inIr        = { printer = layoutLazy,
                                     stater  = ANormLazyStats.layout (ANormLazyStats.O {id = SOME passname}) },
                     outIr       = { printer = layoutStrict,
                                     stater  = ANormStrictStats.layout (ANormStrictStats.O {id = SOME passname}) },
                     mustBeAfter = [],
                     stats       = []}

  val associates = {controls = [], debugs = [], features = [], subPasses = []}

  val pass = Pass.mkCompulsoryPass (description, associates, doModule)

end
