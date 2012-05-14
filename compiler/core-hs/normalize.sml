(* COPYRIGHT_NOTICE_1 *)
(*
 * Translation from CoreHs.t to ANormLazy.t.
 *)
signature CORE_NORMALIZE =
sig
  val pass : (CoreHs.t, CoreHs.t) Pass.t
end

structure CoreNormalize:> CORE_NORMALIZE =
struct
  structure CH = CoreHs
  structure CU = CoreHsUtils
  structure CP = CoreHsPrims
  structure CL = CoreHsLayout
  structure GP = GHCPrim
  structure QD = DictF (struct type t = CH.identifier CH.qualified val compare = CU.compareQName end)
  structure SD = StringDict

  val passname = "CoreNormalize"
  fun failMsg (msg0, msg1) = Fail.fail (passname, msg0, msg1)

  (* 
   * We remember the definitions from Core in a dictionary
   *)
  type dict = { vdict : CH.ty   QD.t
              , tdict : CH.tDef QD.t
              , wrapper : (CH.vDefg SD.t) ref (* for saturated prims, constructors and externs *)
              , varCount : int ref
              }

  fun emptyDict () : dict = { vdict = QD.empty, tdict = QD.empty, wrapper = ref SD.empty, varCount = ref 0 }

  fun insertVar ({ vdict, tdict, wrapper, varCount }, v, vty) 
    = { vdict = QD.insert (vdict, v, vty), tdict = tdict, wrapper = wrapper, varCount = varCount }

  fun freshVar ({ vdict, varCount, ... }, m, v)
    = let
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

  fun freshInlineableVar (dict, v) = #2 (freshVar (dict, NONE, inlinePrefix ^ v))
  fun isInlineable v = String.hasPrefix (v, { prefix = inlinePrefix })

  fun resultTy ty 
    = let
        val msg = "expect function type for " ^ Layout.toString (CL.layoutTy ty)
      in
        case ty 
          of CH.Tapp (t1, t2) =>
            (case t1
              of CH.Tapp (CH.Tcon a, t3) => if a = CU.tcArrow then t2 else failMsg ("resultTy/1", msg)
               | _ => failMsg ("resultTy/2", msg))
           | _ => failMsg ("resultTy/3", msg)
      end

  (* dict lookup that fails when not found *)
  fun lookupMayFail (dict, v, msg)
    = case QD.lookup (dict, v)
        of SOME t => t 
        | _       => failMsg (msg, (CL.qNameToString v) ^ " not found")

  fun applyTy (fty, vty)
    = case fty
        of CH.Tforall ((v, _), t) => CU.substTy (v, vty, t)
         | _ => resultTy fty 
                (* failMsg ("applyTy", Layout.toString (CL.layoutTy fty) ^ " doesn't start with forall") *)

  fun castTy (tdict, ty) 
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
                  (* val () = print ("sep returns con = " ^ CL.qNameToString con ^ "\n")  *)
                  val s  = List.map (s, fn t => cast (false, t))
                in
                  case QD.lookup (tdict, con)
                    of SOME (CH.Newtype (ncon, _, tbs, ty)) => 
                      if sym then List.fold (s, CH.Tcon ncon, fn (aty, fty) => CH.Tapp (fty, aty))
                        else List.fold (List.zip (List.map (tbs, #1), s), ty, fn ((u, v), ty) => CU.substTy (u, v, ty))
                     | _ => List.fold (s, CH.Tcon con, fn (aty, fty) => CH.Tapp (fty, aty))
                end
      in
        cast (false, ty)
      end

  fun saturate (dict, e, ty)
    = case ty
        of CH.Tforall ((t, k), ty) =>
          let
            val v = freshInlineableVar (dict, typPrefix)
          in
            CH.Lam (CH.Tb (v, k), saturate (dict, CH.Appt (e, CH.Tvar v), ty))
          end
        | CH.Tapp (t1, t2) =>
          (case t1
            of CH.Tapp (CH.Tcon a, t3) => 
              if a = CU.tcArrow 
                then let 
                       val v = freshInlineableVar (dict, varPrefix)
                     in
                       CH.Lam (CH.Vb (v, t3), saturate (dict, CH.App (e, CH.Var (NONE, v)), t2))
                     end
                else e
             | _ => e)
        | _ => e

  fun saturateWrapper (dict, e, ty)
    = let
        val { wrapper, ... } = dict
        val name = case e 
                     of CH.Var u  => CL.qNameToString u
                      | CH.Dcon c => CL.qNameToString c
                      | CH.External (_, _, f, _) => f
                      | _ => failMsg ("saturateWrapper", "impossible")
        fun wrap () = 
            let
              val v = freshVar (dict, SOME CU.mainMname, name)
              val def = CH.Nonrec (CH.Vdef (v, ty, saturate (dict, e, ty)))
              val () = wrapper := SD.insert (!wrapper, name, def)
            in
              CH.Var v
            end
        (* val _ = print ("saturate " ^ name ^ " with type " ^ Layout.toString * (CL.layoutTy ty) ^ "\n") *)
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
            (* a limited form of subst that works only for saturated exp *) 
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


  (*
   * Normalize an expression when its type is not given and return the 
   * result expression and its type. 
   *)
  fun normExp (cfg, dict as { vdict, tdict, ... }, exp)
    = case exp 
        of CH.Var (u as (m, v)) =>
          (case QD.lookup (vdict, u)
            of SOME t => (exp, t)
             | NONE => if m = SOME CU.primMname 
                         then let val ty = GP.getTy (cfg, v) in (saturate (dict, exp, ty), ty) end
                         else failMsg ("norm/Var", "variable " ^ v ^ " not found"))
        | CH.Dcon con => 
          (case QD.lookup (vdict, con)
            of SOME t => (saturateWrapper (dict, exp, t), t)
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
                     * Even unboxed tuple has to go through saturate because A-normalizationmay 
                     * result in them being partially applied. Note that we don't use saturateWrapper 
                     * here because constructors for unboxed tuple do not have a uniform type.
                     *)
                    (saturate (dict, exp, ty), ty)
                  end
                 | NONE => failMsg ("norm/Dcon", "constructor " ^ CL.qNameToString con ^ " not found")))
        | CH.External (p, cc, _, ty) => 
          (case cc
            of CH.CCall   => (saturateWrapper (dict, exp, ty), ty) 
             | CH.StdCall => (saturateWrapper (dict, exp, ty), ty) 
             | _          => (saturate (dict, exp, ty), ty))         (* do not wrap non-function calls *)
        | CH.Lit (CH.Literal (lit, ty)) => (exp, ty)
        | CH.App (f, e) =>
          let 
              (*val _ = print ("doExp " ^ Layout.toString (CL.layoutExp exp) ^ "\n")*)
              val (f, fty) = normExp (cfg, dict, f)
              val (e, ety) = normExp (cfg, dict, e)
              (*val _ = print ("got f   = " ^ Layout.toString (CL.layoutExp f) ^ "\n")*)
              (*val _ = print ("    fty = " ^ Layout.toString (CL.layoutTy fty) ^ "\n")*)
              val rty = resultTy fty
            in
              case e 
                of CH.Var v => (tryBeta (f, e), rty)
                | _ =>
                  let
                    val v = freshVar (dict, NONE, varPrefix)
                  in 
                    (CH.Let (CH.Nonrec (CH.Vdef (v, ety, e)), tryBeta (f, CH.Var v)), rty) 
                  end
            end
        | CH.Case (e, (v, vty), ty, alts) =>
          let
            val (e, _) = normExp (cfg, dict, e)
            val dict = insertVar (dict, (NONE, v), vty)
            val alts = List.map (alts, fn a => normAlt (cfg, dict, a))
          in
            (CH.Case (e, (v, vty), ty, alts), ty)
          end
        | CH.Cast (e, ty) => 
          let
            val ty = castTy (tdict, ty)
            val (e, ety) = normExp (cfg, dict, e)
            val v = freshVar (dict, NONE, varPrefix)
          in
            (CH.Cast (CH.Let (CH.Nonrec (CH.Vdef (v, ety, e)), CH.Var v), ty), ty)
          end
        | CH.Lam (bind, e) =>
          (case bind 
            of CH.Vb (v, vty) => 
              let
                val dict = insertVar (dict, (NONE, v), vty)
                val (e, ety) = normExp (cfg, dict, e)
              in
                (CH.Lam (bind, e), CU.tArrow (vty, ety))
              end
            | CH.Tb (v, vkind) => 
              let
                val dict = insertVar (dict, (NONE, v), CH.Tvar v) (* dummy type is not used *)
                val (e, ety) = normExp (cfg, dict, e)
              in
                (CH.Lam (bind, e), CH.Tforall ((v, vkind), ety))
              end)
        | CH.Let (vdefg, e) =>
          let
            val (dict, vdefg) = normVDefg (cfg, dict, vdefg)
            val (e, ty) = normExp (cfg, dict, e)
          in 
            (CH.Let (vdefg, e), ty)
          end
       | CH.Appt (e, ty) => 
         let
           (*val _ = print ("doExp " ^ Layout.toString (CL.layoutExp exp) ^ "\n")*)
           val (e, ety) = normExp (cfg, dict, e)
           (*val _ = print ("got e   = " ^ Layout.toString (CL.layoutExp e) ^ "\n")*)
           (*val _ = print ("    ety = " ^ Layout.toString (CL.layoutTy ety) ^ "\n")*)
           val ety = applyTy (ety, ty)
         in
           (tryBetaTy (e, ty), ety)
         end
       | CH.Note (s, e) => 
         let
           val (e, ety) = normExp (cfg, dict, e)
         in
           (CH.Note (s, e), ety)
         end

  and normAlt (cfg, dict, alt)
    = (case alt
        of CH.Acon (con, tbs, vbs, e) =>
          let
            (* ignore tbs since we cannot handle gadt yet *)
            val dict = List.fold (vbs, dict, fn ((v, vty), dict) => insertVar (dict, (NONE, v), vty))
            val (e, _) = normExp (cfg, dict, e)
          in 
            CH.Acon (con, tbs, vbs, e)
          end
        | CH.Alit (l, e) => CH.Alit (l, #1 (normExp (cfg, dict, e)))
        | CH.Adefault e  => CH.Adefault (#1 (normExp (cfg, dict, e))))

  and normVDefg (cfg, dict, vdefg)
    = (case vdefg
        of CH.Rec vdefs =>
          let
            val dict = List.fold (vdefs, dict, fn (CH.Vdef (v, vty, _), dict) => insertVar (dict, v, vty))
            val vdefs = List.map (vdefs, fn CH.Vdef (v, vty, e) => CH.Vdef (v, vty, #1 (normExp (cfg, dict, e))))
          in
            (dict, CH.Rec vdefs)
          end
        | CH.Nonrec (CH.Vdef (v, vty, e)) =>
          let
            val (e, _) = normExp (cfg, dict, e)
            val dict = insertVar (dict, v, vty)
          in
            (dict, CH.Nonrec (CH.Vdef (v, vty, e)))
          end)

  fun doTDef (typ, { vdict, tdict, wrapper, varCount })
    = (case typ
        of CH.Data (name, tbs, cdefs) => 
          let
            (* make some fresh type variables *)
            val tbs = List.map (tbs, fn (t, k) => (typPrefix ^ t, k))
            val typ = List.fold (tbs, CH.Tcon name, fn ((t, _), ty) => CH.Tapp (ty, CH.Tvar t))
            fun insertCon (CH.Constr (con, tbs', tys), vdict) =
                let 
                  val ty = List.foldr (List.map (tys, #1), typ, CU.tArrow) 
                  val ty = List.foldr (tbs @ tbs', ty, CH.Tforall)
                in
                  QD.insert (vdict, con, ty)
                end
            val vdict = List.fold (cdefs, vdict, insertCon)
            (* val tdict = QD.insert (tdict, name, typ) *)
          in
            { vdict = vdict, tdict = tdict, wrapper = wrapper, varCount = varCount }
          end
        | CH.Newtype (name, tcon, tbs, ty) => 
          let
            (* val tdict = QD.insert (tdict, name, typ) *)
            (* tcon seems to be only used in casting *)
            val tdict = QD.insert (tdict, tcon, typ)
          in
            { vdict = vdict, tdict = tdict, wrapper = wrapper, varCount = varCount }
          end)

  fun doModule (cfg, CH.Module (name, tdefs, vdefgs))
    = let 
        (* Prepare a few built-in definitions *)
        val tdefs  = CH.Data (CP.tcStatezh, [("s", CH.Klifted)], [CH.Constr (CP.tcStatezh, [], [(CH.Tvar "s", true)])]) :: tdefs
        val tdefs  = CH.Data (CP.tcRealWorld, [], [CH.Constr (CP.tcRealWorld, [], [])]) :: tdefs
        val vdefgs = CH.Nonrec (CH.Vdef (CP.vRealWorldzh, CH.Tcon CP.tcRealWorld, CH.Dcon CP.tcRealWorld)) :: vdefgs
        val dict   = List.fold (tdefs, emptyDict (), doTDef)
        fun oneVDefg (vdefg, (vdefgs, dict))
          = let 
              val (dict, vdefg) = normVDefg (cfg, dict, vdefg)
            in
              (vdefg :: vdefgs, dict)
            end
        val (vdefgs, { wrapper, ...}) = List.fold (vdefgs, ([], dict), oneVDefg)
      in 
        CH.Module (name, tdefs, SD.range (!wrapper) @ (List.rev vdefgs))
      end

  fun normalize (x, pd) = doModule (PassData.getConfig pd, x)

  fun layout  (module, _) = CoreHsLayout.layoutModule module
  val helper = { printer = layout, stater = layout }

  val description = {name        = passname,
                     description = "Normalize Haskell Core",
                     inIr        = helper,
                     outIr       = helper,
                     mustBeAfter = [],
                     stats       = [(passname, "Normalize Haskell Core")]}

  val associates = {controls = [], debugs = [], features = [], subPasses = []}

  val pass = Pass.mkCompulsoryPass (description, associates, normalize)

end
