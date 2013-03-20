(* COPYRIGHT_NOTICE_1 *)
(*
 * Translation from CoreHs.t to ANormLazy.t.
 *)
signature CORE_HS_NORMALIZE =
sig
  val pass : (CoreHs.t, CoreHs.t) Pass.t
end

structure CoreHsNormalize:> CORE_HS_NORMALIZE =
struct
  structure CH = CoreHs
  structure CU = CoreHsUtils
  structure CP = CoreHsPrims
  structure CL = CoreHsLayout
  structure GP = GHCPrimOp
  structure QD = DictF (struct type t = CH.identifier CH.qualified val compare = CU.compareQName end)
  structure SD = StringDict

  val passname = "CoreHsNormalize"
  fun failMsg (msg0, msg1) = Fail.fail (passname, msg0, msg1)

  structure Chat = ChatF (struct 
                            type env = Config.t
                            val extract = fn a => a
                            val name = passname
                            val indent = 2
                          end)

  (* 
   * We remember the definitions from Core in a dictionary
   *)
  type env = { cfg   : Config.t
             , vdict : CH.ty   QD.t
             , tdict : CH.tDef QD.t
             }

  type state = { wrapper : (CH.vDefg SD.t) ref (* for saturated prims, constructors and externs *)
               , varCount : int ref }

  fun emptyState () : state = { wrapper = ref SD.empty, varCount = ref 0 }

  (* make uniform newtype coercion prefix in the name of constructor witness *)
  fun fixCoercionPrefix (con as (mname, str)) = 
      if String.hasPrefix (str, {prefix="NTCoZC"}) orelse
         String.hasPrefix (str, {prefix="TFCoZC"})
        then (mname, "CoZC" ^ String.dropPrefix (str, 6))
        else con

  fun insertVar (env, v, vty) 
    = let
        val { cfg, vdict, tdict } = env
      in
        { cfg = cfg, vdict = QD.insert (vdict, v, vty), tdict = tdict }
      end

  fun freshVar (env, state, m, v)
    = let
        val { vdict, ... } = env
        val { varCount, ... } = state
        fun try n = 
            let 
              val u = (m, v ^ (Int.toString n))
            in
              case QD.lookup (vdict, u)
                of NONE => (u, n)
                 | _ => try (n + 1)
            end
        val (u, n) = try (!varCount)
        val () = varCount := n + 1
      in
        u
      end

  (*
   * New variable names are generated using name ^ n, where n is an unique integer. 
   * Variables subject to inline also comes with a inlinePrefix.
   *) 
  val varPrefix = "v"
  val typPrefix = "t"
  val inlinePrefix = "i"

  fun freshInlineableVar (env, state, v) = #2 (freshVar (env, state, NONE, inlinePrefix ^ v))
  fun isInlineable v = String.hasPrefix (v, { prefix = inlinePrefix })

  fun resultTy (env, ty)
    = let
        val msg = "expect function type for " ^ Layout.toString (CL.layoutTy (#cfg env, ty))
      in
        case ty 
          of CH.Tapp (t1, t2) =>
            (case t1
              of CH.Tapp (CH.Tcon a, t3) => if a = CU.tcArrow then t2 else failMsg ("resultTy/1", msg)
               | _ => failMsg ("resultTy/2", msg))
           | _ => failMsg ("resultTy/3", msg)
      end

  fun applyTy (env, fty, vty)
    = case fty
        of CH.Tforall ((v, _), t) => CU.substTy (v, vty, t)
         | _ => resultTy (env, fty)
                (* failMsg ("applyTy", Layout.toString (CL.layoutTy fty) ^ " doesn't start with forall") *)

  fun castTy (env, ty) 
    = let
        fun sep (t, s) = 
            case t 
              of CH.Tcon con => SOME (con, s)
               | CH.Tapp (t1, t2) => sep (t1, t2 :: s)
               | _ => NONE
        and cast (sym, ty) = 
            case sep (ty, [])
              of NONE => 
                (case ty 
                  of CH.SymCoercion ty => cast (true, ty)
                   | CH.TransCoercion (t1, t2) => cast (false, t2)
                   | CH.UnsafeCoercion (t1, t2) => cast (false, t2)
                   | CH.InstCoercion (t1, t2) => cast (false, t2)
                   | CH.LeftCoercion ty => cast (false, ty)
                   | CH.RightCoercion ty => cast (false, ty)
                   | CH.NthCoercion (_, ty) => cast (false, ty)
                   | CH.Tforall (tb, ty) => CH.Tforall (tb, cast (false, ty))
                   | CH.Tapp (t1, t2) => CH.Tapp (cast (false, t1), cast (false, t2))
                   | _ => ty)
               | SOME (con, s) => 
                let 
                  val con = fixCoercionPrefix con
                  val s  = List.map (s, fn t => cast (false, t))
                in
                  case QD.lookup (#tdict env, con)
                    of SOME (CH.Newtype (ncon, _, tbs, ty)) => 
                      if sym then List.fold (s, CH.Tcon ncon, fn (aty, fty) => CH.Tapp (fty, aty))
                        else List.fold (List.zip (List.map (tbs, #1), s), ty, fn ((u, v), ty) => CU.substTy (u, v, ty))
                     | _ => List.fold (s, CH.Tcon con, fn (aty, fty) => CH.Tapp (fty, aty))
                end
      in
        cast (false, ty)
      end

  fun saturate (env, state, e, ty)
    = let
        fun saturate0 (substTy, e, ty) 
          = case ty
              of CH.Tforall ((t, k), ty) =>
                let
                  val v = freshInlineableVar (env, state, typPrefix)
                  val substTy = fn ty => CU.substTy (t, CH.Tvar v, substTy ty)
                in
                  CH.Lam (CH.Tb (v, k), saturate0 (substTy, CH.Appt (e, CH.Tvar v), ty))
                end
              | CH.Tapp (t1, t2) =>
                (case t1
                  of CH.Tapp (CH.Tcon a, t3) => 
                    if a = CU.tcArrow 
                      then let 
                             val v = freshInlineableVar (env, state, varPrefix)
                             val t3 = substTy t3
                           in
                             CH.Lam (CH.Vb (v, t3), saturate0 (substTy, CH.App (e, CH.Var (NONE, v)), t2))
                           end
                      else e
                   | _ => e)
              | _ => e
      in
        saturate0 (fn ty => ty, e, ty)
      end

  fun saturateWrapper (env, state, e, ty)
    = let
        val { wrapper, ... } = state
        val name = case e 
                     of CH.Var u  => CL.qNameToString u
                      | CH.Dcon c => CL.qNameToString c
                      | CH.External (_, _, f, _) => f
                      | _ => failMsg ("saturateWrapper", "impossible")
        fun wrap () = 
            let
              val v = freshVar (env, state, SOME CU.mainMname, name)
              val def = CH.Nonrec (CH.Vdef (v, ty, saturate (env, state, e, ty)))
              val () = wrapper := SD.insert (!wrapper, name, def)
            in
              CH.Var v
            end
      in
        case SD.lookup (!wrapper, name)
          of SOME (CH.Nonrec (CH.Vdef (v, _, _))) => CH.Var v
           | _ => wrap ()
      end    

  fun tryBeta (f, e) =
      case f 
        of CH.Lam (CH.Vb (v, _), body) =>
          let
            (* a limited form of substitution that works only for saturated exp *) 
            fun subst exp = 
                case exp 
                  of CH.Var (NONE, u) => if u = v then e else exp
                   | CH.App (e1, e2) => CH.App (subst e1, subst e2)
                   | CH.Appt (e1, ty) => CH.Appt (subst e1, ty)
                   | CH.Lam (bind, e1) =>
                     (case bind
                       of CH.Vb (u, _) => if u = v then exp else CH.Lam (bind, subst e1)
                        | _ => CH.Lam (bind, subst e1))
                   | _ => exp
          in
            if isInlineable v then subst body else CH.App (f, e)
          end
        | CH.Let (def as (CH.Nonrec (CH.Vdef ((NONE, v), _, _))), g as (CH.Lam _)) =>
          (case e
            of CH.Var (NONE, w) => if w <> v then CH.Let (def, tryBeta (g, e)) else CH.App (f, e)
             | _ => CH.App (f, e))
        | _ => CH.App (f, e)

  fun tryBetaTy (f, ty) =
      case f 
        of CH.Lam (CH.Tb (v, _), body) =>
          let
            (* a limited form of substitution that works only for saturated exp *) 
            fun subst exp = 
                case exp 
                  of CH.App (e1, e2) => CH.App (subst e1, subst e2)
                   | CH.Appt (e1, t1) => 
                     (case t1
                       of CH.Tvar u => if u = v then CH.Appt (e1, ty) else CH.Appt (subst e1, t1)
                        | _ => CH.Appt (subst e1, t1))
                   | CH.Lam (bind, e1) =>
                     (case bind
                       of CH.Tb (u, _) => if u = v then exp else CH.Lam (bind, subst e1)
                        | _ => CH.Lam (bind, subst e1))
                   | _ => exp
          in
            if isInlineable v then subst body else CH.Appt (f, ty)
          end
        | _ => CH.Appt (f, ty)

  (* Extract nested let definition from its inner expression *)
  val rec letExtract =
    fn CH.Let (def, f) => 
      let 
        val (lets, e) = letExtract f
      in 
        (fn e => CH.Let (def, lets e), e)
      end
     | e => (fn e => e, e)

  (*
   * Normalize an expression when its type is not given and return the 
   * result expression and its type. 
   *)
  fun normExp (env, state, exp)
    = case exp 
        of CH.Var (u as (m, v)) =>
          (case QD.lookup (#vdict env, u)
            of SOME t => (exp, t)
             | NONE => if m = SOME CU.primMname 
                         then 
                           case GP.fromString v
                             of SOME p => let val ty = GP.getType p in (saturate (env, state, exp, ty), ty) end
                              | NONE => failMsg ("norm/Var", "primitive " ^ v ^ " not supported")
                         else failMsg ("norm/Var", "variable " ^ v ^ " not found"))
        | CH.Dcon con => 
          (case QD.lookup (#vdict env, con)
            of SOME t => (saturateWrapper (env, state, exp, t), t)
             | NONE => 
              (case CU.isUtupleDc con
                of SOME n => 
                  let
                    (* make up some dummy types *)
                    val vs = List.tabulate (n, fn i => "t" ^ Int.toString i)
                    val tvs = List.map (vs, CH.Tvar)
                    val ty = CU.tUtuple tvs
                    val ty = List.foldr (tvs, ty, CU.tArrow)
                    val ty = List.foldr (vs, ty, fn (v, ty) => CH.Tforall ((v, CH.Kopen), ty))
                  in 
                    (*
                     * Even unboxed tuple has to go through saturate because A-normalization may 
                     * result in them being partially applied. Note that we don't use saturateWrapper 
                     * here because constructors for unboxed tuple do not have a uniform type.
                     *)
                    (saturate (env, state, exp, ty), ty)
                  end
                 | NONE => failMsg ("norm/Dcon", "constructor " ^ CL.qNameToString con ^ " not found")))
        | CH.External (p, cc, _, ty) => 
          (case cc
            of CH.CCall   => (saturateWrapper (env, state, exp, ty), ty) 
             | CH.StdCall => (saturateWrapper (env, state, exp, ty), ty) 
             | _          => (saturate (env, state, exp, ty), ty))         (* do not wrap non-function calls *)
        | CH.Lit (CH.Literal (lit, ty)) => (exp, ty)
        | CH.App (f, e) =>
          let 
              val (f, fty) = normExp (env, state, f)
              val (e, ety) = normExp (env, state, e)
              val rty = resultTy (env, fty)
            in
              case e 
                of CH.Var v => (tryBeta (f, e), rty)
                | _ =>
                  let
                    val v = freshVar (env, state, NONE, varPrefix)
                    val vdef = CH.Nonrec (CH.Vdef (v, ety, e))
                    (* let floating inward *)
                    val (lets, f') = letExtract f
                  in      
                    (lets (CH.Let (vdef, tryBeta (f', CH.Var v))), rty) 
                  end
            end
        | CH.Case (e, (v, vty), ty, alts) =>
          let
            val (e, _) = normExp (env, state, e)
            val env = insertVar (env, (NONE, v), vty)
            val alts = List.map (alts, fn a => normAlt (env, state, a))
          in
            (CH.Case (e, (v, vty), ty, alts), ty)
          end
        | CH.Cast (e, ty) => 
          let
            val ty = castTy (env, ty)
            val (e, ety) = normExp (env, state, e)
            val v = freshVar (env, state, NONE, varPrefix)
          in
            (CH.Cast (CH.Let (CH.Nonrec (CH.Vdef (v, ety, e)), CH.Var v), ty), ty)
          end
        | CH.Lam (bind, e) =>
          (case bind 
            of CH.Vb (v, vty) => 
              let
                val env = insertVar (env, (NONE, v), vty)
                val (e, ety) = normExp (env, state, e)
              in
                (CH.Lam (bind, e), CU.tArrow (vty, ety))
              end
            | CH.Tb (v, vkind) => 
              let
                val env = insertVar (env, (NONE, v), CH.Tvar v) (* dummy type is not used *)
                val (e, ety) = normExp (env, state, e)
              in
                (CH.Lam (bind, e), CH.Tforall ((v, vkind), ety))
              end)
        | CH.Let (vdefg, e) =>
          let
            val (env, vdefg) = normVDefg (env, state, vdefg)
            val (e, ty) = normExp (env, state, e)
          in 
            (CH.Let (vdefg, e), ty)
          end
       | CH.Appt (e, ty) => 
         let
           val (e, ety) = normExp (env, state, e)
           val ety = applyTy (env, ety, ty)
         in
           (tryBetaTy (e, ty), ety)
         end
       | CH.Note (s, e) => 
         let
           val (e, ety) = normExp (env, state, e)
         in
           (CH.Note (s, e), ety)
         end

  and normAlt (env, state, alt)
    = (case alt
        of CH.Acon (con, tbs, vbs, e) =>
          let
            (* ignore tbs since we cannot handle gadt yet *)
            val env = List.fold (vbs, env, fn ((v, vty), env) => insertVar (env, (NONE, v), vty))
            val (e, _) = normExp (env, state, e)
          in 
            if con = CP.dcEq  (* match type on cases over Eq constructor *)
              then CH.Acon (con, List.map (vbs, fn (v, _) => (v, CH.Kopen)), [], e)
              else CH.Acon (con, tbs, vbs, e)
          end
        | CH.Alit (l, e) => CH.Alit (l, #1 (normExp (env, state, e)))
        | CH.Adefault e  => CH.Adefault (#1 (normExp (env, state, e))))

  and normVDefg (env, state, vdefg)
    = (case vdefg
        of CH.Rec vdefs =>
          let
            val env = List.fold (vdefs, env, fn (CH.Vdef (v, vty, _), env) => insertVar (env, v, vty))
            val vdefs = List.map (vdefs, fn CH.Vdef (v, vty, e) => CH.Vdef (v, vty, #1 (normExp (env, state, e))))
          in
            (env, CH.Rec vdefs)
          end
        | CH.Nonrec (CH.Vdef (v, vty, e)) =>
          let
            val (e, _) = normExp (env, state, e)
            val env = insertVar (env, v, vty)
          in
            (env, CH.Nonrec (CH.Vdef (v, vty, e)))
          end)

  fun doTDef (env, state, def)
    = (case def
        of CH.Data (name, tbs, cdefs) => 
          let
            val typ = List.fold (tbs, CH.Tcon name, fn ((t, _), ty) => CH.Tapp (ty, CH.Tvar t))
            fun insertCon (CH.Constr (con, tbs', tys), vdict) =
                let 
                  val ty = List.foldr (List.map (tys, #1), typ, CU.tArrow) 
                  val ty = List.foldr (tbs @ tbs', ty, CH.Tforall)
                in
                  QD.insert (vdict, con, ty)
                end
            val vdict = List.fold (cdefs, #vdict env, insertCon)
            (* val tdict = QD.insert (tdict, name, typ) *)
          in
            { cfg = #cfg env, vdict = vdict, tdict = #tdict env } 
          end
        | CH.Newtype (name, tcon, tbs, ty) => 
          let
            (* val tdict = QD.insert (tdict, name, typ) *)
            (* tcon seems to be only used in casting *)
            val tcon = fixCoercionPrefix tcon
            val tdict = QD.insert (#tdict env, tcon, def)
          in
            { cfg = #cfg env, vdict = #vdict env, tdict = tdict }
          end)

  fun doModule (cfg, CH.Module (name, tdefs, vdefgs))
    = let 
        (* Prepare a few built-in definitions *)
        (* realWorld# : State# RealWorld# *)
        val tdefs  = CH.Data (CP.tcStatezh, [("s", CH.Klifted)], [CH.Constr (CP.tcRealWorld, [], [])]) :: tdefs
        val tdefs  = CH.Data (CP.tcRealWorld, [], []) :: tdefs
        val vdefgs = CH.Nonrec (CH.Vdef (CP.vRealWorldzh, CP.tRWS, CH.Dcon CP.tcRealWorld)) :: vdefgs
        (* remove existing datatype definition for ~ (z7eU) *)
        val tdefs  = List.keepAll (tdefs, 
                       fn CH.Data ((_, s), _, _) => s <> "z7eU"
                        | _ => true)
        (* 
         * Simplify it to: data z7eU  = forall k a b . Eq# (forall c . c)
         * Note: this breaks 7.4, and only works for GHC 7.6 or above.
         *)
        val tdefs  = CH.Data (CP.tcEq, List.map (["k","a","b"], fn s => (s, CH.Kopen)),
                       [CH.Constr (CP.dcEq, [("c", CH.Kopen)], [])]) :: tdefs
        val state  = emptyState ()
        val env    = { cfg = cfg, vdict = QD.empty, tdict = QD.empty } 
        val env    = List.fold (tdefs, env, fn (def, env) => doTDef (env, state, def))
        fun oneVDefg (vdefg, (vdefgs, env))
          = let 
              val (env, vdefg) = normVDefg (env, state, vdefg)
            in
              (vdefg :: vdefgs, env)
            end
        val (vdefgs, _) = List.fold (vdefgs, ([], env), oneVDefg)
        val wrapper = #wrapper state
      in 
        CH.Module (name, tdefs, SD.range (!wrapper) @ (List.rev vdefgs))
      end

  fun normalize (x, pd) = doModule (PassData.getConfig pd, x)

  fun layout  (module, cfg) = CoreHsLayout.layoutModule (cfg, module)
  val helper = { printer = layout, stater = layout }

  val description = {name        = passname,
                     description = "Normalization of GHC Core",
                     inIr        = helper,
                     outIr       = helper,
                     mustBeAfter = [],
                     stats       = []}

  val associates = {controls = [], debugs = [], features = [], subPasses = []}

  val pass = Pass.mkCompulsoryPass (description, associates, normalize)

end
