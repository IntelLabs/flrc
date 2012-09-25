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
  structure GP = GHCPrimType
  structure SD = StringDict
  structure CH = CoreHs
  structure CU = CoreHsUtils
  structure CP = CoreHsPrims
  structure CL = CoreHsLayout
  structure AL = ANormLazy
  structure IM = Identifier.Manager
  structure ND = Identifier.NameDict
  structure QD = DictF (struct type t = CH.identifier CH.qualified val compare = CU.compareQName end)
  structure QS = SetF (struct type t = CH.identifier CH.qualified val compare = CU.compareQName end)

  val passname = "CoreHsToANormLazy"

  fun failMsg (msg0, msg1) = Fail.fail (passname, msg0, msg1)

  (* 
   * We remember constructor, type and value definitions in a dictionary
   *)
  datatype ty = Sumtype of CH.tBind list * (AL.con * CH.tBind list * (CH.ty * CH.strictness) list) list
              | Newtype of CH.tBind list * CH.ty

  type dict = { ndict : (AL.con * AL.strictness list) QD.t
              , tdict : ty QD.t
              , vdict : AL.var QD.t
              , sdict : AL.exp QD.t (* direct substitution *)
              }

  val emptyDict = { ndict = QD.empty, tdict = QD.empty, vdict = QD.empty, sdict = QD.empty }

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

  fun primTy doTy (im, cfg, t) = 
      let
      in
        case GP.hsToPrimTy (doTy, t)
          of SOME t => SOME (AL.Prim t)
           | NONE => 
             (case isUtupleTy t 
               of SOME tys => 
                 let
                   val tys = List.map (tys, doTy) 
                 in
                   SOME (AL.Prim (GP.Tuple tys))
                 end
                | _ => NONE)
      end

  fun doSum doTy resolve resolved (dict as { tdict, ... }, con, args)
    = if not resolve orelse QS.member (resolved, con)
        then AL.Data
        else
          (* Undefined types is allowed here because they are not declared in ExtCore *) 
          case QD.lookup (tdict, con)
            of NONE => AL.Data  
             | SOME x => 
              let
                val resolved = QS.insert (resolved, con)
              in
                case x 
                  of Sumtype (tbinds, arms) =>
                    let
                      val env = SD.fromList (List.zip (List.map (tbinds, #1), args))
                      fun doTy1 (ty, strict) = (doTy false resolved env ty, strict)
                      val arms = List.map (arms, fn (tag, _, tys) => (tag, List.map (tys, doTy1)))
                    in
                      AL.Sum arms
                    end
                   | Newtype (tbinds, ty) => 
                    let
                      val env = SD.fromList (List.zip (List.map (tbinds, #1), args))
                      val ty = doTy false resolved env ty
                    in
                      ty
                    end
              end
    
  (*
   * Simplify the haskell type. 
   *
   * After CoreNormalize pass, we should only be get types that are fully 
   * applied. We just have to instantiate datatype and turn them into sums.
   *)
  fun doTy (im, cfg, dict as { tdict, ... })
    = let
        fun doTy0 resolve resolved env t = 
            case primTy (doTy0 resolve resolved env) (im, cfg, t) 
              of SOME ty => ty 
               | NONE => doTy1 resolve resolved env t
        and doTy1 resolve resolved env t =
            case t 
              of CH.Tvar v => 
                (case SD.lookup (env, v) 
                  of SOME t => t
                   | NONE => AL.Data)
               | CH.Tcon c => doSum doTy0 resolve resolved (dict, c, []) 
               | CH.Tapp (t1, t2) =>
                 let
                   fun splitTy args (CH.Tapp (t1, t2)) = splitTy (t2 :: args) t1
                     | splitTy args t = (t, args)
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
                                val u = doTy0 resolve resolved env fty
                                val v = doTy0 resolve resolved env aty
                              in
                                case u 
                                  of AL.Prim (GP.EqTy _) => v
                                   | _ => AL.Arr (u, v)
                              end
                         else let
                                val args = List.map (args, doTy0 false resolved env) 
                              in
                                  doSum doTy0 resolve resolved (dict, c, args) 
                              end
                     | (CH.Tcon c, args) => 
                       let
                         val args = List.map (args, doTy0 false resolved env) 
                       in
                         doSum doTy0 resolve resolved (dict, c, args) 
                       end
                     | _ => AL.Data  (* TODO: shall we error here? *)
                 end
               | CH.Tforall (_, ty) => doTy0 resolve resolved env ty         (* Ignore type abstraction *)
               | _ => failMsg ("doTy", "unexcepted coercion type " ^ Layout.toString (CL.layoutTy t))
      in
        doTy0 true QS.empty SD.empty
      end

  fun makeVar (im, cfg, dict as { ndict, tdict, vdict, sdict }, v, vty)
    = let 
        val ty = doTy (im, cfg, dict) vty
        val u  = IM.variableFresh (im, CL.qNameToString v, ty)
        val vdict = QD.insert (vdict, v, u)
      in 
        ({ ndict = ndict, tdict = tdict, vdict = vdict, sdict = sdict }, u, ty)
      end

  fun makeVars (im, cfg, dict, vbs)
    = let
        fun mkVbs ((v, vty), (vs, dict)) 
          = let 
              val (dict, v, vty) = makeVar (im, cfg, dict, (NONE, v), vty) 
            in 
              ((v, vty) :: vs, dict)
            end
        val (vbs, dict) = List.foldr (vbs, ([], dict), mkVbs)
      in
        (dict, vbs)
      end

  fun addSubstitute (dict as { ndict, tdict, vdict, sdict }, v, e)
    = let
        val sdict = QD.insert (sdict, v, e)
      in
        { ndict = ndict, tdict = tdict, vdict = vdict, sdict = sdict }
      end

  fun isSaturated (im, cfg, dict as { ndict, vdict, ... }, e)
    = let
        fun check args = 
            fn CH.Dcon con =>
              (case CU.isUtupleDc con
                of SOME n => SOME (AL.Multi args)
                | _ => 
                  let
                    val (con, strictness) = lookupMayFail (ndict, con, "isSaturated")
                    val _ = if length strictness = length args then ()
                             else failMsg ("isSaturated", "arity of strictness does not match that number of fields")
                  in
                    SOME (AL.ConApp (con, List.zip (args, strictness)))
                  end)
            | CH.Var (p, v) => if p = SOME CU.primMname then 
                                 case GHCPrimOp.fromString v
                                   of SOME p => SOME (AL.PrimApp (p, args)) 
                                    | NONE => NONE
                                 else NONE
            | CH.External (p, cc, s, ty) => SOME (AL.ExtApp (p, cc, s, doTy (im, cfg, dict) ty, args))
            | CH.App (f, e) =>
              (case e
                of CH.Var v => check (lookupMayFail (vdict, v, "isSaturated") :: args) f
                | _ => failMsg ("isSaturated", "expression not in a-norm form " ^ Layout.toString(CL.layoutExp e)))
            | CH.Appt (f, t) => check args f
            | _ => NONE
      in
        check [] e
      end

  (* handle recursive cast of function types *)
  fun cast (im, e, t1, t2)
    = case (t1, t2)
        of (AL.Arr (t11, t12), AL.Arr (t21, t22)) =>
          let
            val f = IM.variableFresh (im, "f", t1)
            val g = IM.variableFresh (im, "g", t2)
            val x = IM.variableFresh (im, "x", t21)
            val y = IM.variableFresh (im, "y", t11)
            val (v, w) = case t22
                      of AL.Prim (GP.Tuple tys) =>
                        let
                          val vs = List.map (tys, fn t => IM.variableFresh (im, "z", t))
                        in
                          (AL.VbMulti (List.zip (vs, tys)), AL.Multi vs)
                        end
                       | _ => 
                        let 
                          val z = IM.variableFresh (im, "z", t22) 
                        in 
                          (AL.VbSingle (z, t22), AL.Var z) 
                        end

          in
            (* 
             * let f = e
             *     g = \x -> let y = cast (x, t21, t11)
             *                   v = cast (f y, t12, t22)
             *                in w
             * in g
             *)
            AL.Let (AL.Nonrec (AL.Vdef (AL.VbSingle (f, t1), e)),
              AL.Let (AL.Nonrec (AL.Vdef (AL.VbSingle (g, t2), AL.Lam ((x, t21), 
                AL.Let (AL.Nonrec (AL.Vdef (AL.VbSingle (y, t11), cast (im, AL.Var x, t21, t11))),
                    AL.Let (AL.Nonrec (AL.Vdef (v, cast (im, AL.App (AL.Var f, y), t12, t22))), w))))), AL.Var g))
          end
         | _ => AL.Cast (e, t1, t2)

  fun doExp (im, cfg, dict as { vdict, sdict, ... }) 
    = fn CH.Var v => 
        (case QD.lookup (sdict, v)
          of SOME e => e
           | _ => AL.Var (lookupMayFail (vdict, v, "doExp")))
       | CH.Lit (CH.Literal (lit, ty)) => AL.Lit (lit, doTy (im, cfg, dict) ty)
       | oe as CH.Case (e, (v, vty), ty, alts) =>
        let
          val ty = doTy (im, cfg, dict) ty
          val e = doExp (im, cfg, dict) e
        in
          case (isSome (CU.isUtupleTy vty), alts)
            of (true, [CH.Acon (_, _, vbs, e')]) =>
              let
                val (dict, vbs) = makeVars (im, cfg, dict, vbs)
                val dict = addSubstitute (dict, (NONE, v), AL.Multi (List.map (vbs, #1))) 
                val e' = doExp (im, cfg, dict) e'
              in
                AL.Let (AL.Nonrec (AL.Vdef (AL.VbMulti vbs, e)), e')
              end
             | (true, []) =>
               let
                 val tys = case doTy (im, cfg, dict) vty
                            of AL.Prim (GP.Tuple tys) => tys
                             | _ => failMsg ("doExp/case", "expect primitive tuple type for " ^ v)
                 val vs = List.map (tys, fn ty => IM.variableFresh (im, v, ty))
                 val vbs = List.zip (vs, tys)
               in
                 AL.Let (AL.Nonrec (AL.Vdef (AL.VbMulti vbs, e)), AL.Multi vs)
               end
             | (true, _) => failMsg ("doExp", "Bad case on unboxed tuple: " ^ Layout.toString (CL.layoutExp oe))
             | _ => 
               let
                 val (dict, v, vty) = makeVar (im, cfg, dict, (NONE, v), vty)
               in
                 AL.Case (e, (v, vty), ty, List.map (alts, doAlt (im, cfg, dict)))
               end
        end
       | CH.Cast (e, ty) => 
         (case e (* assume cast has been translated to the following form in CoreNormalize *)
           of CH.Let (CH.Nonrec (CH.Vdef (v, vty, e)), CH.Var _) => 
              cast (im, doExp (im, cfg, dict) e, doTy (im, cfg, dict) vty, doTy (im, cfg, dict) ty)
            | _ => failMsg ("valueExp", "expect normalized cast expression "))
       | CH.Lam (bind, e) =>
        (case bind 
          of CH.Vb (v, vty) => 
              let 
                val (dict, v, vty) = makeVar (im, cfg, dict, (NONE, v), vty)
                val e = doExp (im, cfg, dict) e
              in
                case vty (* we skip z7eUzh because it is a type variable *)
                  of AL.Prim (GP.EqTy _) => e
                   | _ => AL.Lam ((v, vty), e)
              end
           | CH.Tb (v, vkind) => doExp (im, cfg, dict) e)       (* ignore type bindings *)
       | CH.Let (vdefg, e) =>
        let
          val (vdefg, dict) = doVDefg (im, cfg, dict) vdefg
        in
          AL.Let (vdefg, doExp (im, cfg, dict) e)
        end
       | CH.Appt (e, ty) => doExp (im, cfg, dict) e
       | CH.Note (s, e) => doExp (im, cfg, dict) e                (* TODO: handle notes *)
       | e =>
        (case isSaturated (im, cfg, dict, e)
          of SOME e => e
           | _ =>
            (case e 
              of CH.App (f, e) =>
               (case doExp (im, cfg, dict) e
                 of AL.Var v => AL.App (doExp (im, cfg, dict) f, v)
                  | _ => failMsg ("doExp", "expresion not in a-norm form " ^ Layout.toString (CL.layoutExp e)))
               | _ => failMsg ("doExp", "cannot handle " ^ Layout.toString (CL.layoutExp e))))

  and doAlt (im, cfg, dict as { ndict, ... }) 
    = fn CH.Acon (con, tbs, vbs, e) =>
        let
          val (con, strictness) = 
              case QD.lookup (ndict, con)
                of SOME con => con
                 | NONE => ((IM.nameFromString (im, CL.qNameToString con), 0), [])   (* unboxed tuple *)
          val (dict, vbs) = makeVars (im, cfg, dict, vbs)
          fun annotate ((v, ty)::vbs) [] = (v, ty, false) :: annotate vbs []
            | annotate ((v, ty)::vbs) (strict::xs) = (v, ty, strict) ::annotate vbs xs
            | annotate _ _ = []
          val vbs = annotate vbs strictness
        in 
          AL.Acon (con, vbs, doExp (im, cfg, dict) e)
        end
       | CH.Alit (CH.Literal (lit, ty), e) => AL.Alit (lit, doTy (im, cfg, dict) ty, doExp (im, cfg, dict) e)
       | CH.Adefault e => AL.Adefault (doExp (im, cfg, dict) e)

  and doVDefVar (im, cfg, dict) (CH.Vdef (v, vty, e)) = 
      case isUtupleTy vty
       of SOME tys => failMsg ("doVDefVar", "Unboxed tuple bound to a variable")
        | NONE => 
          let
            val (dict, v, vty) = makeVar (im, cfg, dict, v, vty)
          in
            ((AL.VbSingle (v, vty), e), dict)
          end

  and doVDef (im, cfg, dict) (bind, e) = AL.Vdef (bind, doExp (im, cfg, dict) e)

  and doVDefg (im, cfg, dict)
    = fn CH.Rec vdefs =>
        let
          val (vdefs, dict) = List.foldr (vdefs, ([], dict), fn (vdef, (defs, dict)) => 
                               let val (def, dict) = doVDefVar (im, cfg, dict) vdef in (def :: defs, dict) end)
          val vdefs = List.map (vdefs, doVDef (im, cfg, dict))
        in
          (AL.Rec vdefs, dict)
        end
      | CH.Nonrec vdef =>
        let
          val (vdef, dict') = doVDefVar (im, cfg, dict) vdef
          val vdef = doVDef (im, cfg, dict) vdef
        in
          (AL.Nonrec vdef, dict')
        end

  fun doTDef im (typ, dict as { ndict, tdict, vdict, sdict })
    = (case typ
        of CH.Data (name, tbs, cdefs) => 
          let
            fun makeName (i, CH.Constr (con, tbinds, tys))
              = let 
                  val strictness = List.map (tys, #2)
                  val tag = (IM.nameMake (im, CL.qNameToString con), i)
                in
                  ((con, (tag, strictness)), (tag, tbinds, tys))
                end
            val (tags, arms) = List.unzip (List.mapi (cdefs, makeName))
            val ndict = List.fold (tags, ndict, fn ((con, tag), ndict) => QD.insert (ndict, con, tag))
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
        val name = String.dropLast (CL.qNameToString (SOME name, ""))
        val dict = List.fold (tdefs, emptyDict, doTDef im)
        fun oneVDefg (vdefg, (vdefgs, dict))
          = let 
              val (vdefg, dict) = doVDefg (im, cfg, dict) vdefg
            in
              (vdefg :: vdefgs, dict)
            end
        val (vdefgs, dict as { vdict, ... }) = List.fold (vdefgs, ([], dict), oneVDefg)
        val mainVar = if CoreHsParse.noMainWrapper cfg then CU.mainVar else CU.wrapperMainVar
        val main = lookupMayFail (vdict, mainVar, "doModule") 
      in 
        (AL.Module (main, List.rev vdefgs), IM.finish im)
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
