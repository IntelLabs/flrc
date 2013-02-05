(* COPYRIGHT_NOTICE_1 *)
(*
 * Translation from CoreHs.t to ANormLazy.t.
 *)
signature CORE_HS_TO_ANORM_LAZY =
sig
  val pass : (CoreHs.t, ANormLazy.t) Pass.t
end

structure CoreHsToANormLazy :> CORE_HS_TO_ANORM_LAZY =
struct
  structure I  = Identifier
  structure GP = GHCPrimType
  structure SD = StringDict
  structure CH = CoreHs
  structure CU = CoreHsUtils
  structure CP = CoreHsPrims
  structure CL = CoreHsLayout
  structure AL = ANormLazy
  structure IM = Identifier.Manager
  structure NS = Identifier.NameSet
  structure QD = DictF (struct type t = CH.identifier CH.qualified val compare = CU.compareQName end)
  structure QS = SetF (struct type t = CH.identifier CH.qualified val compare = CU.compareQName end)

  val passname = "CoreHsToANormLazy"

  fun failMsg (msg0, msg1) = Fail.fail (passname, msg0, msg1)

  (* 
   * We remember constructor, type and value definitions in a dictionary
   *)
  datatype ty = Sumtype of CH.tBind list * (AL.con * CH.tBind list * (CH.ty * CH.strictness) list) list
              | Newtype of CH.tBind list * CH.ty

  type dict = { ndict : (AL.con * AL.strictness list) QD.t (* constructor/name lookup *)
              , tdict : ty QD.t                                  (* type info look up *)
              , vdict : AL.var QD.t                          (* variable substitution *)
              , sdict : AL.exp QD.t                            (* direct substitution *)
              }

  val emptyDict = { ndict = QD.empty, tdict = QD.empty, vdict = QD.empty, sdict = QD.empty }

  type state = { im : AL.ty IM.t, tm : AL.tyMgr }

  type env = { cfg : Config.t, dict : dict, mname : CH.mName }

  fun updateEnvDict ({ cfg, mname, ...}, dict) = { cfg = cfg, dict = dict, mname = mname }
  fun updateEnvMName ({ cfg, dict, ...}, mname) = { cfg = cfg, dict = dict, mname = mname }

  (* dict lookup that fails when not found *)
  fun lookupMayFail (dict, v, msg)
    = case QD.lookup (dict, v)
        of SOME t => t 
        | _       => failMsg (msg, (CL.qNameToString v) ^ " not found")

  fun isUtupleTy t = 
      let
        fun getArgs (CH.Tapp (t2, t1)) = t1 :: getArgs t2
          | getArgs _ = []
      in
        case CU.isUtupleTy t 
         of SOME _ => SOME (List.rev (getArgs t)) 
          | _ => NONE
      end

  (* We annotate arrow type with effect if the argument type is State# *) 
  fun arrow tm (t1, t2) = 
      let
        val effect = case TypeRep.repToBase t1 
                       of AL.Prim (GP.State t) => 
                         SOME (case TypeRep.repToBase t 
                                 of AL.Sum _ => Effect.Any             (* assume t is RealWorld, then it is IO monad *)
                                  | _ => Effect.union (Effect.HeapInit, Effect.Control)) (* otherwise it is ST monad *)
                        | _ => NONE
      in
        TypeRep.newRep (tm, AL.Arr (t1, t2, effect))
      end


  fun splitTy args (CH.Tapp (t1, t2)) = splitTy (t2 :: args) t1
    | splitTy args t = (t, args)

(*
  val rec coreTyToName : state * env * (AL.var * CH.ty) SD.t * CH.ty -> I.name
    = fn (state, env, tyenv, ty) =>
      let
        val { im, tm, ... } = state 
        val { cfg, ... } = env
      in
        case ty
          of CH.Tvar v => (case SD.lookup (tyenv, v)
                            of SOME (_, t) => 
                              (case t 
                                 of CH.Tvar u => if u = v then IM.nameMake (im, v)
                                                 else coreTyToName (state, env, tyenv, t) 
                                  | _ => coreTyToName (state, env, tyenv, t))
                             | NONE => IM.nameMake (im, v) 
                              (* failMsg ("coreTyToName", "type variable " ^ v ^ " not found in tyenv") *)
                          )
           | CH.Tcon c => IM.nameMake (im, CL.qNameToString c)
           | CH.Tapp _ => 
            let
              val (fty, args) = splitTy [] ty
            in
              case (fty, args)
                of (CH.Tcon c, [fty, aty]) =>
                  if c = CU.tcArrow then coreTyToNameArr (state, env, tyenv, fty, aty) 
                    else coreTyToNameApp (state, env, tyenv, fty, [aty])
                 | _ => coreTyToNameApp (state, env, tyenv, fty, args)
            end
           | CH.Tforall ((v, _), ty) => 
            let
              (* map bounded type name to fresh names, a hack 
              val _ = print ("got forall '" ^ v ^ "'\n")
              val tyenv = SD.insert (tyenv, v, (IM.variableFresh (im, v, AL.Data), CH.Tcon (NONE, v)))
              *)
              val n = coreTyToName (state, env, tyenv, ty)
              val s = "%forall " ^ v ^ "." ^ IM.nameString (im, n)
            in
              IM.nameMake (im, "(" ^ s ^ ")")
            end
           | _ => failMsg ("coreTyToName", "unexpected type coercion")
      end

  and rec coreTyToNameArr : state * env * (AL.var * CH.ty) SD.t * CH.ty * CH.ty -> I.name
    = fn (state, env, tyenv, fty, aty) =>
      let 
        val { im, tm, ... } = state 
        val { cfg, ... } = env
        val m = coreTyToName (state, env, tyenv, fty)
        val n = coreTyToName (state, env, tyenv, aty)
        val s = IM.nameString (im, m) ^ " -> " ^ IM.nameString (im, n)
      in
        IM.nameMake (im, "(" ^ s ^ ")")
      end

  and rec coreTyToNameApp : state * env * (AL.var * CH.ty) SD.t * CH.ty * CH.ty list -> I.name
    = fn (state, env, tyenv, fty, atys) =>
      let 
        val { im, tm, ... } = state 
        val { cfg, ... } = env
        val m = coreTyToName (state, env, tyenv, fty)
        val n = List.map (atys, fn aty => coreTyToName (state, env, tyenv, aty))
        val s = List.fold (n, IM.nameString (im, m), fn (n, s) => s ^ " " ^ IM.nameString (im, n))
      in
        IM.nameMake (im, "(" ^ s ^ ")")
      end
*)

  val tyData = TypeRep.newRep_ AL.Data

  (*
   * Simplify the haskell type. 
   *
   * After CoreNormalize pass, we should only be get types that are fully 
   * applied. We just have to instantiate datatype and turn them into sums.
   *)
  val doTy : state * env * CH.ty -> AL.ty
    = fn (state, env, ty) =>
      let 
        val { im, tm, ... } = state 
        val { cfg, dict, ... } = env

        fun doSum (resolved, tyenv, con, args)
          = case QD.lookup (resolved, con)
            of SOME (SOME x) => x
             | SOME _ => tyData
             | _ =>
              (case QD.lookup (#tdict dict, con)
                (* Undefined types is allowed here because they are not declared in ExtCore *) 
                of NONE => tyData
                 | SOME x => 
                  let
                    val resolved = QD.insert (resolved, con, NONE)
                  in
                    case x 
                      of Sumtype (tbinds, arms) =>
                        let
                          val _ = if length tbinds <> length args 
                                    then failMsg ("doSum/Sumtype", CL.qNameToString con ^ 
                                                                   " has unbalanced tbinds and arguments")
                                    else ()
                          val us = List.map (tbinds, #1)
                          (* generate unique variable for type variables *)
                          val vs = List.map (us, fn u => IM.variableFresh (im, u, tyData))
                          val tyenv = SD.fromList (List.zip (us, List.zip (vs, args)))
                          fun doArm (tag, _, tys) 
                            = (tag, List.map (tys, fn (ty, strict) => (doTy0 (resolved, tyenv, ty), strict)))
                          val arms = List.map (arms, doArm)
                          val ty = TypeRep.newRep (tm, AL.Sum arms)
                        in
                          ty
                        end
                       | Newtype (tbinds, ty) => 
                        let
                          val _ = if length tbinds <> length args 
                                    then failMsg ("doSum/Newtype", CL.qNameToString con ^ 
                                                                   " has unbalanced tbinds and arguments")
                                    else ()
                          val us = List.map (tbinds, #1)
                          (* generate unique variable for type variables *)
                          val vs = List.map (us, fn u => IM.variableFresh (im, u, tyData))
                          val tyenv = SD.fromList (List.zip (us, List.zip (vs, args)))
                          val ty = doTy0 (resolved, tyenv, ty)
                        in
                          ty
                        end
                  end)

        and primTy (resolved, tyenv, t) 
          = case GP.hsToPrimTy (fn ty => doTy0 (resolved, tyenv, ty), t)
              of SOME t => SOME (TypeRep.newRep (tm, AL.Prim t))
               | NONE => 
                 (case isUtupleTy t 
                   of SOME tys => 
                     let
                       val tys = List.map (tys, fn t => doTy0 (resolved, tyenv, t)) 
                     in
                       SOME (TypeRep.newRep (tm, AL.Prim (GP.Tuple tys)))
                     end
                    | _ => NONE)

        and doTy0 (resolved, tyenv, t)
          = case primTy (resolved, tyenv, t)
              of SOME t => t
               | NONE => 
                (case t
                  of CH.Tvar v => (case SD.lookup (tyenv, v) 
                                    (* prevent circular lookup *)
                                    of SOME (_, t) => doTy0 (resolved, SD.remove (tyenv, v), t)
                                     | NONE => tyData)
                   | CH.Tcon c => doSum (resolved, tyenv, c, []) 
                   | CH.Tapp (t1, t2) =>
                    let
                      val (fty, args) = splitTy [] t
                      (*
                      val _ = print ("done split fty = " ^ Layout.toString (CL.layoutTy fty) ^ " args = " ^ 
                                     Layout.toString (Layout.seq (List.map (args, CL.layoutTy))) ^ "\n")
                      *)
                    in
                      case (fty, args)
                        of (CH.Tcon c, [fty, aty]) =>
                          if c = CU.tcArrow
                            then let 
                                   val u = doTy0 (resolved, tyenv, fty)
                                   val v = doTy0 (resolved, tyenv, aty)
                                 in
                                   case TypeRep.repToBase u 
                                     of AL.Prim (GP.EqTy _) => v
                                      (* remove unboxed tuple when it is a function argument *)
                                      | AL.Prim (GP.Tuple tys) => List.foldr (tys, v, arrow tm)
                                      | _ => arrow tm (u, v)
                                 end
                            else doSum (resolved, tyenv, c, args) 
                        | (CH.Tcon c, args) => doSum (resolved, tyenv, c, args) 
                        | _ => tyData (* TODO: shall we error here? *)
                    end
                  | CH.Tforall (_, ty) => doTy0 (resolved, tyenv, ty) 
                  | _ => failMsg ("doTy0", "unexcepted coercion type " ^ Layout.toString (CL.layoutTy t)))
      in
        doTy0 (QD.empty, SD.empty, ty)
      end

  fun makeVar (state, env, v, vty) = 
      let 
        val ty = doTy (state, env, vty)
        val { im, tm, ... } = state 
        val { cfg, dict, mname } = env
        val { ndict, tdict, vdict, sdict } = dict
        val s = case v 
                 of (SOME _, _) => CL.qNameToString v
                  | (NONE, s)   => CL.qNameToString (mname, s)
        val u  = IM.variableFresh (im, s, ty)
        val vdict = QD.insert (vdict, v, u)
        val dict = { ndict = ndict , tdict = tdict, vdict = vdict, sdict = sdict }
      in 
        (u, ty, updateEnvDict (env, dict))
      end

  fun makeVars (state, env, vbs)
    = let
        fun mkVbs ((v, vty), (vs, env)) 
          = let 
              val (v, vty, env) = makeVar (state, env, (NONE, v), vty) 
            in 
              ((v, vty) :: vs, env)
            end
        val (vbs, env) = List.foldr (vbs, ([], env), mkVbs)
      in
        (vbs, env)
      end

  fun addSubstitute (dict as { ndict, tdict, vdict, sdict }, v, e)
    = let
        val sdict = QD.insert (sdict, v, e)
      in
        { ndict = ndict, tdict = tdict, vdict = vdict, sdict = sdict }
      end

  fun isSaturated (state, env, e)
    = let
        val { im, tm, ... } = state 
        val { cfg, dict, ... } = env
        val { ndict, vdict, ... } = dict
        fun check args = 
            fn CH.Dcon con =>
              (case CU.isUtupleDc con
                of SOME n => SOME (AL.Multi args)
                | _ => 
                  let
                    val (con, stricts) = lookupMayFail (ndict, con, "isSaturated")
                    val _ = if length stricts = length args then ()
                             else failMsg ("isSaturated", "arity of strictness does not match that number of fields")
                  in
                    SOME (AL.ConApp (con, List.zip (args, stricts)))
                  end)
            | CH.Var (p, v) => if p = SOME CU.primMname then 
                                 case GHCPrimOp.fromString v
                                   of SOME p => SOME (AL.PrimApp (p, args)) 
                                    | NONE => NONE
                                 else NONE
            | CH.External (p, cc, s, ty) => SOME (AL.ExtApp (p, cc, s, doTy (state, env, ty), args))
            | CH.App (f, e) =>
              (case e
                of CH.Var v => check (lookupMayFail (vdict, v, "isSaturated") :: args) f
                | _ => failMsg ("isSaturated", "expression not in a-norm form " ^ Layout.toString(CL.layoutExp e)))
            | CH.Appt (f, t) => check args f
            | _ => NONE
        val r = check [] e
      in
        r
      end

  val rec doExp : state * env -> CH.exp -> AL.exp
    = fn (state, env) =>
      let
        val { im, tm, ... } = state 
        val { cfg, dict, mname } = env
      in
        fn CH.Var v => 
          (case QD.lookup (#sdict dict, v)
            of SOME e => e
             | _ => AL.Var (lookupMayFail (#vdict dict, v, "doExp")))
         | CH.Lit (CH.Literal (lit, ty)) => AL.Lit (lit, doTy (state, env, ty))
         | oe as CH.Case (e, (v, vty), ty, alts) =>
          let
            val ty = doTy (state, env, ty)
            val e = doExp (state, env) e
          in
            case (isSome (CU.isUtupleTy vty), alts)
              of (true, [CH.Acon (_, _, vbs, e')]) =>
                let
                  val (vbs, env) = makeVars (state, env, vbs)
                  val dict = addSubstitute (#dict env, (NONE, v), AL.Multi (List.map (vbs, #1))) 
                  val env = updateEnvDict (env, dict)
                  val e' = doExp (state, env) e'
                  val effectful = case TypeRep.repToBase (#2 (hd vbs))
                                    of AL.Prim (GP.State t) => true
                                     | _ => false
                in
                  AL.Let (AL.Nonrec (AL.Vdef (AL.VbMulti (vbs, effectful), e)), e')
                end
               | (true, []) =>
                 let
                   val tys = case TypeRep.repToBase (doTy (state, env, vty))
                              of AL.Prim (GP.Tuple tys) => tys
                               | _ => failMsg ("doExp/case", "expect primitive tuple type for " ^ v)
                   val vs = List.map (tys, fn ty => IM.variableFresh (im, CL.qNameToString (mname, v), ty))
                   val vbs = List.zip (vs, tys)
                 in
                   AL.Let (AL.Nonrec (AL.Vdef (AL.VbMulti (vbs, false), e)), AL.Multi vs)
                 end
               | (true, _) => failMsg ("doExp", "Bad case on unboxed tuple: " ^ Layout.toString (CL.layoutExp oe))
               | _ => 
                 let
                   val (v, vty, env) = makeVar (state, env, (NONE, v), vty)
                 in
                   AL.Case (e, (v, vty), ty, List.map (alts, doAlt (state, env)))
                 end
          end
         | CH.Cast (e, ty) => 
           (case e (* assume cast has been translated to the following form in CoreNormalize *)
             of CH.Let (CH.Nonrec (CH.Vdef (v, vty, e)), CH.Var _) => 
                AL.Cast (doExp (state, env) e, doTy (state, env, vty), doTy (state, env, ty))
              | _ => failMsg ("valueExp", "expect normalized cast expression "))
         | CH.Lam (bind, e) =>
          (case bind 
            of CH.Vb (vstr, vty) => 
                let 
                  val (v, vty, env) = makeVar (state, env, (NONE, vstr), vty)
                in
                  case TypeRep.repToBase vty (* we skip z7eUzh because it is a type variable *)
                    of AL.Prim (GP.EqTy _) => doExp (state, env) e
                     | AL.Prim (GP.Tuple tys) => (* remove unboxed tuple when it's function argument *)
                      let
                        val vs = List.map (tys, fn ty => IM.variableFresh (im, CL.qNameToString (mname, vstr), ty))
                        val dict = addSubstitute (dict, (NONE, vstr), AL.Multi vs)
                        val env = updateEnvDict (env, dict)
                      in
                        List.foldr (List.map (List.zip (vs, tys), fn (v, t) => (v, t, false)), 
                                    doExp (state, env) e, AL.Lam)
                      end
                     | _ => AL.Lam ((v, vty, false), doExp (state, env) e)
                end
             | CH.Tb (v, vkind) => doExp (state, env) e)       (* ignore type bindings *)
         | CH.Let (vdefg, e) =>
          let
            val (vdefg, env) = doVDefg (state, env) vdefg
          in
            AL.Let (vdefg, doExp (state, env) e)
          end
         | CH.Appt (e, ty) => doExp (state, env) e
         | CH.Note (s, e) => doExp (state, env) e                (* TODO: handle notes *)
         | e =>
          (case isSaturated (state, env, e)
            of SOME e => e
             | _ =>
              (case e 
                of CH.App (f, e) =>
                 (case doExp (state, env) e
                   of AL.Var v => AL.App (doExp (state, env) f, v)
                    (* remove unboxed tuple when it is function argument *)
                    | AL.Multi vs => List.fold (vs, doExp (state, env) f, fn (v, f) => AL.App (f, v))
                    | _ => failMsg ("doExp", "expresion not in a-norm form " ^ Layout.toString (CL.layoutExp e)))
                 | _ => failMsg ("doExp", "cannot handle " ^ Layout.toString (CL.layoutExp e))))
      end

  and rec doAlt : state * env -> CH.alt -> AL.alt
    = fn (state, env) =>
      let
        val { im, tm, ... } = state 
        val { cfg, dict, ... } = env
      in
        fn CH.Acon (con, tbs, vbs, e) =>
          let
            val (con, strictness) = 
                case QD.lookup (#ndict dict, con)
                  of SOME con => con
                   | NONE => ((IM.nameFromString (im, CL.qNameToString con), 0), [])   (* unboxed tuple *)
            val (vbs, env) = makeVars (state, env, vbs)
            fun annotate ((v, ty)::vbs) [] = (v, ty, false) :: annotate vbs []
              | annotate ((v, ty)::vbs) (strict::xs) = (v, ty, strict) ::annotate vbs xs
              | annotate _ _ = []
            val vbs = annotate vbs strictness
          in 
            AL.Acon (con, vbs, doExp (state, env) e)
          end
         | CH.Alit (CH.Literal (lit, ty), e) => AL.Alit (lit, doTy (state, env, ty), doExp (state, env) e)
         | CH.Adefault e => AL.Adefault (doExp (state, env) e)
      end

  and rec doVDefVar : state * env -> CH.vDef -> (AL.vBind * CH.mName * CH.exp) * env
    = fn (state, env) => fn (CH.Vdef (v as (mname, vstr), vty, e)) =>
      let
        val { im, tm, ... } = state 
        val { cfg, dict, ... } = env
      in
        case isUtupleTy vty
         of SOME tys => 
           let
             val vbs = List.map (tys, fn t => (vstr, t))
             val (vbs, env) = makeVars (state, env, vbs)
             val dict = addSubstitute (#dict env, v, AL.Multi (List.map (vbs, #1)))
             val env = updateEnvDict (env, dict)
           in
             ((AL.VbMulti (vbs, false), mname, e), env)
           end
          | NONE => 
           let
             val (v, vty, env) = makeVar (state, env, v, vty)
             val strict = case TypeRep.repToBase vty of AL.Prim _ => true | _ => false
           in
             ((AL.VbSingle (v, vty, strict), mname, e), env)
           end
      end

  and rec doVDef : state * env -> AL.vBind * CH.mName * CH.exp -> AL.vDef
    = fn (state, env) => fn (bind, mname, e) =>
      AL.Vdef (bind, doExp (state, updateEnvMName (env, mname)) e)

  and rec doVDefg : state * env -> CH.vDefg -> AL.vDefg * env
    = fn (state, env) =>
      let
        val { im, tm, ... } = state 
        val { cfg, ... } = env
      in
        fn CH.Rec vdefs =>
          let
            val (vdefs, env) = List.foldr (vdefs, ([], env), fn (vdef, (defs, env)) => 
                                 let val (def, env) = doVDefVar (state, env) vdef in (def :: defs, env) end)
            val vdefs = List.map (vdefs, doVDef (state, env))
          in
            (AL.Rec vdefs, env)
          end
        | CH.Nonrec vdef =>
          let
            val (vdef, env') = doVDefVar (state, env) vdef
            val vdef = doVDef (state, env) vdef
          in
            (AL.Nonrec vdef, env')
          end
      end

  fun doTDef (im, cfg) (typ, dict as { ndict, tdict, vdict, sdict })
    = (case typ
        of CH.Data (name, tbs, cdefs) => 
          let
            fun makeName (i, CH.Constr (con, tbinds, tys))
              = let 
                  val isPrimTy =
                    fn CH.Tcon (m, _) => m = SOME CU.primMname
                     | _ => false
                  val tys = List.map (tys, fn (ty, strict) => (ty, strict orelse isPrimTy ty))
                  val strictness = List.map (tys, #2)
                  val tag = (IM.nameMake (im, CL.qNameToString con), i)
                in
                  ((con, (tag, strictness)), (tag, tbinds, tys))
                end
            val (tags, arms) = List.unzip (List.mapi (cdefs, makeName))
            val ndict = List.fold (tags, ndict, fn ((con, tag), ndict) => QD.insert (ndict, con, tag))
            (*
            val _ = print ("remember sumtype for " ^ CL.qNameToString name ^ "\n")
            *)
            val tdict = QD.insert (tdict, name, Sumtype (tbs, arms)) 
          in
            { ndict = ndict, tdict = tdict, vdict = vdict, sdict = sdict }
          end
        | CH.Newtype (name, tcon, tbs, ty) => 
          let
            val tdict = QD.insert (tdict, name, Newtype (tbs, ty))
          in
            { ndict = ndict, tdict = tdict, vdict = vdict, sdict = sdict }
          end)

  fun doModule (CH.Module (name, tdefs, vdefgs), pd)
    = let 
        val cfg  = PassData.getConfig pd
        val im   = IM.new ""
        val si   = I.SymbolInfo.SiManager im
        val hashTy = Layout.toString o (ANormLazyLayout.layoutTy si)
        val tm   = TypeRep.newManager (AL.hashTy_ si)
        val name = String.dropLast (CL.qNameToString (SOME name, ""))
        val dict = List.fold (tdefs, emptyDict, doTDef (im, cfg))
        val state = { im = im, tm = tm } 
        val env  = { cfg = cfg, mname = NONE, dict = dict }
        fun oneVDefg (vdefg, (vdefgs, env))
          = let 
              val (vdefg, env) = doVDefg (state, env) vdefg
            in
              (vdefg :: vdefgs, env)
            end
        val (vdefgs, env) = List.fold (vdefgs, ([], env), oneVDefg)
        val mainVar = if CoreHsParse.noMainWrapper cfg then CU.mainVar else CU.wrapperMainVar
        val main = lookupMayFail (#vdict (#dict env), mainVar, "doModule") 
      in 
        (AL.Module (tm, main, List.rev vdefgs), IM.finish im)
      end

  fun layout  (module, _) = CoreHsLayout.layoutModule module

  fun layout' (module, _) = ANormLazyLayout.layoutModule module

  val description = {name        = passname,
                     description = "GHC Core to lazy A-Normal Form",
                     inIr        = { printer = layout,
                                     stater  = layout },
                     outIr       = { printer = layout',
                                     stater  = layout' },
                     mustBeAfter = [],
                     stats       = []}

  val associates = {controls = [], debugs = [], features = [], subPasses = []}

  val pass = Pass.mkCompulsoryPass (description, associates, doModule)

end
