(* COPYRIGHT_NOTICE_1 *)
(*
 * Translation of Normalized Haskell Core into Mil.
 *)
signature ANORM_STRICT_TO_MIL =
sig
  val pass : (ANormStrict.t * (Identifier.VariableSet.t Identifier.VariableDict.t), Mil.t) Pass.t
end

structure ANormStrictToMil :> ANORM_STRICT_TO_MIL =
struct
  open ANormStrict
  structure AS = ANormStrict
  structure L  = Layout
  structure M  = Mil
  structure ML = MilLayout
  structure SS = StringSet
  structure SD = StringDict
  structure LD = M.LD
  structure LS = M.LS
  structure VD = M.VD
  structure VS = M.VS
  structure ND = M.ND
  structure I  = Identifier
  structure IS = Identifier.SymbolInfo
  structure IM = Identifier.Manager
  structure CL = CoreHsLayout
  structure CH = CoreHs
  structure CHU = CoreHsUtils
  structure CHP = CoreHsPrims
  structure GPO = GHCPrimOp
  structure GP = GHCPrim
  structure MP = Mil.Prims
  structure PK = MilUtils.PObjKind
  structure FK = MilUtils.FieldKind
  structure TMU = HsToMilUtils

  val passname = "ANormStrictToMil"

  structure MS = TMU.MS
  structure MF = TMU.MF
  structure MU = TMU.MU

  fun failMsg (f, s) = Fail.fail (passname, f, s)

  fun varIsGlobal (im, v) = TMU.variableKind (im, v) = M.VkGlobal

  fun doSumDCon (imcfg as (_, cfg)) (i, (con, tys)) 
    = (GP.tagToConst (Config.targetWordSize cfg, i), Vector.fromListMap (tys,  doType imcfg))

  and doType (x as (im, cfg)) 
    = fn Arr (ts, ts2) => M.TClosure { args = Vector.fromListMap (ts, doType x)
                                     , ress = Vector.fromListMap (ts2, doType x) }
      |  Prim t       => GP.primToMilTy (Config.targetWordSize cfg, doType x) t
      |  Sum cons     => M.TSum { tag = GP.tagTyp (Config.targetWordSize cfg), 
                                  arms = Vector.fromList (List.mapi (cons, doSumDCon x)) }
      |  Tname tcon   => if IM.variableExists (im, tcon) then TMU.variableTyp (im, tcon) else M.TRef
      |  Thunk t      => Mil.TThunk (doType x t)
      |  _            => M.TRef
  
  fun resultType typ
    = case typ
        of M.TClosure  { args, ress } => ress
        |  M.TCode { cc, args, ress } => ress
        |  _ => failMsg ("resultType", "Other type application are not handled")

  fun lookup (((v,value)::vs), w) = if v = w then SOME value else lookup (vs, w)
    | lookup ([], w)              = NONE

  fun lookup1 (im, env) w
    = case lookup (env, w)
        of SOME v => v
         | NONE   => failMsg ("lookup1", "impossible: " ^ Layout.toString (IM.layoutVariable (w, im)) ^ " not found in env\n")

  fun codePtrForFunction (state, f) = 
      case TMU.stateGetCodePtr (state, f)
       of SOME cptr => cptr
        | NONE      => failMsg ("codePtrForFunction", 
                                "No code ptr for function" ^ Layout.toString (IM.layoutVariable (f, #im state)))

  (*
   * doExp translates an expression into MIL code blocks.
   * It returns a code block that assigns rvar the value of this expression. 
   *)
  fun doExp (state : TMU.state, env : (var * var) list, rvar : var Vector.t, e : exp) : MS.t * Effect.set
    = let 
        val { im, cfg, globals, effects, stateful, worlds, ... } = state
        val ws = Config.targetWordSize cfg
      in
        case e
          of Return vs =>
             let
               val args = Vector.fromListMap (vs, lookup1 (im, env))
               fun sub (i, (u, v), blk) = MS.seq (blk, MS.bindRhs (u, M.RhsSimple (M.SVariable v)))
               val () = if Vector.length rvar = Vector.length args then ()
                          else failMsg ("doExp/Multi", "expect " ^ Int.toString (Vector.length rvar) ^ 
                                        " return value " ^ Layout.toString (List.layout (fn v => IM.layoutVariable (v, im)) vs) ^
                                        " but got " ^ Int.toString (Vector.length args))
               val blk = Vector.foldi (Vector.zip (rvar, args), MS.empty, sub)
             in 
               (blk, Effect.Total)
             end
          |  ConApp ((_, tag), vs)  =>
             let
               val args = Vector.fromListMap (vs, lookup1 (im, env))
               val typs = Vector.map (args, fn v => TMU.variableTyp (im, v))
               val fks  = Vector.map (typs, fn typ => FK.fromTyp (cfg, typ))
             in
               (MS.bindsRhs (rvar,
                            M.RhsSum { tag    = GP.tagToConst (ws, tag)
                                     , typs   = fks
                                     , ofVals = Vector.map (args, M.SVariable) }),
                Effect.Total)

             end
          |  ExtApp (pname, cc, fstr, fty, vs) => 
            (case cc 
              of CH.Prim => failMsg ("doExp/ExpApp", "external primitives is not supported")
              | CH.Label =>
              let
                (* 
                 * Do not call TMU.externalVariable on functions included in
                 * header files because we don't want to mix extern labels with
                 * code types. But we have no way to figure out which are
                 * included, so have to hard-code them.
                 *)
                val declaredSet = SS.fromList ["free", "n_capabilities"]
                val (fvar, typ) = 
                    if SS.member (declaredSet, fstr) then
                      (IM.variableFresh (im, fstr, M.VI { typ = M.TNonRefPtr, kind = M.VkExtern }), M.TNonRefPtr)
                    else
                      TMU.externVariable (state, pname, fstr, fn () => M.TNonRefPtr) before print ("declare " ^ fstr ^ "\n")
              in
                (MS.bindsRhs (rvar, M.RhsSimple (M.SVariable fvar)), Effect.Total)
              end
              | CH.Dynamic =>
              (case vs
                of [] => failMsg ("doExp/ExtApp", "dynmaic call must have at least one function and one argument")
                 | f::vs =>
                  let
                    val fvar = lookup1 (im, env) f
                    val args = Vector.fromListMap (vs, lookup1 (im, env))
                  in
                    (* assume all external call involve IO *)
                    (MU.call (im, cfg, M.CCode { ptr = fvar, code = TMU.noCode },
                              Vector.map (args, M.SVariable), TMU.noCut, Effect.Any, rvar), Effect.Any)
                  end)
              |  _ => 
              let
                val conv = if String.hasPrefix (String.toLower (pname), { prefix = "plsr" }) then M.CcCode 
                             else M.CcUnmanaged (case cc of CH.StdCall => M.AbiStdcall | _ => M.AbiCdecl)
                fun mkTyp () = 
                    case doType (im, cfg) fty
                      of M.TClosure { args, ress } => 
                         M.TCode { cc = conv
                                 , args = args 
                                   (* handle void return properly *)
                                 , ress = (*case Vector.sub (ress, 0) of M.TNone => Vector.new0 () | _ => *)ress 
                                 }
                      | _ => failMsg ("doExp:ExpApp", fstr ^ " is not of function type")
                val pname = CHU.zDecodeString pname
                val (fvar, typ) = TMU.externVariable (state, pname, fstr, mkTyp)
                val ress = case typ
                            of M.TCode { cc, args, ress } => ress
                             | _ => failMsg ("doExp:ExpApp", "impossible: " ^ fstr ^ " is not a Code type")
                val args = Vector.fromListMap (vs, lookup1 (im, env))
                val blk = MU.call (im, cfg, M.CCode { ptr = fvar, code = TMU.noCode },
                                    Vector.map (args, M.SVariable), TMU.noCut, Effect.Any, rvar)
              in
                (* assume all external call involve IO *)
                (blk, Effect.Any)
              end)
          | PrimApp (f, vs) =>
            let
              val argstyps = List.map (vs, fn v => (M.SVariable (lookup1 (im, env) v), TMU.variableTyp (im, v)))
            in
              GP.primOp (state, f, rvar, argstyps)
            end 
          | App (f, vs) =>
            let 
              val fvar = lookup1 (im, env) f
              val args = Vector.fromListMap(vs, lookup1 (im, env))
              val isStateful = case vs
                           of [s] => VS.member (stateful, s)
                            | _ => false
              val isIO = case vs
                           of [s] => VS.member (worlds, s)
                            | _ => false
              val fx = TMU.lookupEffect (effects, fvar, isStateful, isIO)
              (* val _ = print ("app " ^ IM.variableString (im, fvar) ^ " has * effect " ^ Layout.toString (Effect.layout fx) ^ "\n") *)
              val cuts = if Effect.contains (fx, Effect.Fails) then TMU.exitCut else TMU.noCut
              val code = TMU.stateGetCodesForFunction (state, f)
            in
              (MU.call (im, cfg, M.CClosure { cls = fvar, code = code},
                        Vector.map (args, M.SVariable), cuts, fx, rvar),
               fx)
            end
          | Let (vdefg, e) =>
              let 
                val (env, gblks, fx1) = doVDefg (state, env) vdefg
                val (eblks, fx2) = doExp (state, env, rvar, e)
              in
                (MS.seq (gblks, eblks), Effect.union (fx1, fx2))
              end
          | Lit (l, Prim ty) => 
            let 
              fun constant (v, typ) = MS.bindsRhs (rvar, M.RhsSimple (M.SConstant v))
              fun global (v, typ) 
                = let 
                    val cvar = TMU.globalVariableFresh0 (im, typ)
                    val _ = globals := VD.insert (!globals, cvar, v)
                  in 
                    MS.bindsRhs (rvar, M.RhsSimple (M.SVariable cvar))
                  end
              val blk = 
                  case l
                    of CH.Lstring s => global (M.GCString s, M.TCString) 
                     (*
                     | CH.Lrational r => (* handle rational differently to preserve precision *)
                      let
                        val (typ, mp) = case ty
                                     of AP.Float  => (GP.floatTyp,  GP.floatNumTyp)
                                      | AP.Double => (GP.doubleTyp, GP.doubleNumTyp)
                                      | _ => failMsg ("doExp", "cannot cast rational literal to non-floating point type")
                        val (n, d) = Rat.toInts r
                        val u = TMU.localVariableFresh0 (im, typ)
                        val v = TMU.localVariableFresh0 (im, typ)
                        (* TODO: to maintain high precision, we should really cast to big integer *)
                        val blk0 = MS.bindRhs (im, cfg, u, M.RhsSimple (M.SConstant (#1 (GP.castLiteral ws (CH.Lint n, ty)))))
                        val blk1 = MS.bindRhs (im, cfg, v, M.RhsSimple (M.SConstant (#1 (GP.castLiteral ws (CH.Lint d, ty)))))
                        val blk2 = MS.bindRhs (im, cfg, rvar, GP.arith (mp, MP.ADivide) [(M.SVariable u, typ), (M.SVariable v, typ)])
                      in
                        MS.seqn (im, cfg, [blk0, blk1, blk2])
                      end
                     *)
                     | _ => constant (GP.castLiteral ws (l, ty))
            in
              (blk, Effect.Total)
            end
          | Lit (l, _) => failMsg ("doExp", "literal of non-primitive type is not supported: " ^ Layout.toString (CoreHsLayout.layoutCoreLit l) ^ "\n")
          | Case (v, alts) =>
              let 
                val w = lookup1 (im, env) v
                (*
                val _ = print ("doSumAlt on variable " ^ Layout.toString (IM.layoutVariable (v, im)) ^ " mapped to type " ^ Layout.toString (ML.layoutTyp (cfg, IS.SiManager im, TMU.variableTyp (im, w))) ^ "\n")
                *)
              in
                doAlts (state, env, rvar, w, alts)
              end
          | Eval v => 
            let
              val fvar = lookup1 (im, env) v
              (* Aggressive assumption: thunk evals are also pure and stateless! *)
              val fx = TMU.lookupEffect (effects, fvar, false, false)
            in
              (TMU.kmThunk (state, rvar, fvar, fx), fx)
            end
          | Cast cast => 
            let
              fun castV v = 
                  let
                    val w = lookup1 (im, env) v
                    val wty = TMU.variableTyp (im, w)
                  in 
                    [(M.SVariable w, wty)]
                  end
              val blk = 
                  case cast
                    of FromAddr v => #1 (GP.primOp (state, GPO.CastFromAddrzh, rvar, castV v))
                     | ToAddr v => #1 (GP.primOp (state, GPO.CastToAddrzh, rvar, castV v))
                     | NullRef => MS.bindsRhs (rvar, M.RhsSimple (M.SConstant (M.CRef 0)))
                     | Bottom _ => 
                      let
                        fun mkBottom u = 
                            let
                              val ty = TMU.variableTyp (im, u)
                              val v  = TMU.globalVariableFresh0 (im, ty)
                              val _  = globals := VD.insert (!globals, v, M.GErrorVal ty)
                              val _  = effects := VD.insert (!effects, v, Effect.FailsS)
                              val _  = effects := VD.insert (!effects, u, Effect.FailsS)
                            in
                              MS.bindRhs (u, M.RhsSimple (M.SVariable v))
                            end
                      in
                        MS.seqn (Vector.toList (Vector.map (rvar, mkBottom)))
                      end
            in
              (blk, Effect.Total)
            end
      end

  and doAlts (state, env, rvar, v, alts) : MS.t * Effect.set
    = let
        val {im, cfg, ...} = state
        val ws = Config.targetWordSize cfg
        val vtyp = TMU.variableTyp (im, v)
        fun doSumAlt ((_, tag), vbinds, e)
          = let 
              val tag = GP.tagToConst (ws, tag)
              (*
              val arms = case vtyp
                           of M.TSum { tag, arms } => arms
                            | _ => failMsg ("doSumAlt", "variable " ^ Layout.toString (IM.layoutVariable (v, im)) ^ " is not a sum type")
              val _ = print ("case on type " ^ Layout.toString (CF.layoutType (cf, ttyp)) ^ "\n") *)
              (* instead of getting typs from arms, get them from vbinds 
                 val typs = lookupTagTyps (arms, tag)
                 val us   = Vector.map (typs, fn ty => TMU.localVariableFresh0 (im, ty))
              *)
              val (us, typs) = Vector.unzip (Vector.fromList (List.map (vbinds, fn (v, _) => (v, TMU.variableTyp (im, v)))))
              val fks  = Vector.map (typs, fn ty => FK.fromTyp (cfg, ty))
              val blks = Vector.mapi (us, fn (i, u) => MS.bindRhs (u, 
                                                         M.RhsSumProj { typs = fks , sum = v , tag = tag , idx = i }))
              (* we make sure the variable gets a more precise type in environment 
              val env = List.map (List.zip (vbinds, vs), fn ((n,t), (v,_)) => (n, (v, doType (cf, t)))) @ env
               *)
              val env = List.zip (map #1 vbinds, Vector.toList us) @ env
              val u = TMU.variablesClone (im, rvar)
              val (eblks, fx) = doExp (state, env, u, e) 
              val fx = Effect.union (fx, Effect.Total)
            in
              (u, tag, MS.seqn (Vector.toList blks @ [eblks]), fx)
            end

        fun doLitAlt (l, ty, e) 
          = let
              val u = TMU.variablesClone (im, rvar)
              val (eblk, fx) = doExp (state, env, u, e)
              val (c, _) = case ty 
                             of Prim ty => GP.castLiteral ws (l, ty) 
                              | _ => failMsg ("doLitAlt", "literal not of primitive type")
            in
              (u, c, eblk, fx)
            end

        fun doDefault e = 
            let
              val u = TMU.variablesClone (im, rvar)
              val (blk, fx) = doExp (state, env, u, e)
            in
              (u, blk, fx)
            end

        fun groupAlts [] (sumcase, litcase, defcase) = (List.rev sumcase, List.rev litcase, List.rev defcase)
          | groupAlts (x::xs) (s, l, d)
          = case x 
              of Acon x     => groupAlts xs (x::s, l, d)
               | Alit x     => groupAlts xs (s, x::l, d)
               | Adefault x => groupAlts xs (s, l, x::d)

        val (sumcase, litcase, defcase) = groupAlts alts ([], [], [])
        val (sts, tags, sblks, fx1) = Utils.List.unzip4 (map doSumAlt sumcase) 
        val (lts, csts, lblks, fx2) = Utils.List.unzip4 (map doLitAlt litcase)
        val (dts, dblks, fx3) = Utils.List.unzip3 (map doDefault defcase)
        val alts = Vector.fromList (List.zip (sblks @ lblks @ dblks, List.map (sts @ lts @ dts, fn v => Vector.map (v, M.SVariable))))
        val fx = List.fold (fx1 @ fx2 @ fx3, Effect.Total, Effect.union)
        fun blk0 labels = 
            let
              val gotos = List.map (Vector.toList labels, fn l => M.T { block = l, arguments = Vector.new0 () })
              val (cases, dcases) = List.splitAt (gotos, List.length gotos - List.length defcase)
              val dcase = case dcases of [] => NONE | (c::_) => SOME c
              val tr = case (sumcase, litcase)
                         of (_::_, []) => M.TCase
                                        { select = M.SeSum (FK.fromTyp (cfg, GP.tagTyp ws)) 
                                        , on = M.SVariable v 
                                        , cases = Vector.fromList (List.zip (tags, cases))
                                        , default = dcase
                                        }
                          | ([], _::_) => M.TCase
                                        { select = M.SeConstant
                                        , on = M.SVariable v 
                                        , cases = Vector.fromList (List.zip (csts, cases))
                                        , default = dcase
                                        }
                          | _          => M.TCase
                                        { select = M.SeConstant
                                        , on = M.SVariable v 
                                        , cases = Vector.new0 ()
                                        , default = dcase
                                        }
            in
              (MS.empty, tr)
            end
      in
        (MU.joinn (im, cfg, blk0, alts, rvar), fx)
      end

  (*
   * Handle either recursive or non-recursive value definitions.
  *) 
  and doVDef0 (state, recursiveB, env1, vd) = 
      let
        val {im, cfg, aliases, globals, prelude, effects, ...} = state
      in
        case vd
         of Vthk {name = lhs, ty = _, escapes, recursive, fvs, body = e} =>
            let 
              val fvs = if recursiveB then List.remove (fvs, fn s => s = lhs) else fvs
              val fvtyps = List.map (fvs, fn x => TMU.variableTyp (im, x))
              val ftyp = M.TThunk (M.TRef)
              val etyp = TMU.variableTyp (im, lhs)
              val etyp'= case etyp of M.TThunk ty => ty 
                                    | _ => failMsg ("doVDef", "Bad type for thunk")
              val () = 
                  case TMU.variableKind (im, lhs)
                   of M.VkLocal => 
                      let
                        (* Associates cptr with lhs *)
                        val cptr = TMU.mkThunkFunction0 (state, lhs, ftyp, Vector.fromList fvtyps, etyp')
                        (* Associates cptr with each alias of lhs *)
                        val () = case VD.lookup (aliases, lhs)
                                  of SOME s => VS.foreach (s, fn v => TMU.stateAddCodePtr (state, v, cptr))
                                   | NONE   => ()
                      in ()
                      end
                    | _         => ()
            in ()
           end
          | Vfun {name = lhs, ty = _, escapes, recursive, fvs, args, body = e} =>
            let 
              val ftyp   = M.TRef
              val fvs = if recursiveB then List.remove (fvs, fn s => s = lhs) else fvs
              val fvtyps = Vector.fromListMap (fvs, fn x => TMU.variableTyp (im, x))
              val argtyps = Vector.fromListMap (args, fn (x, _) => TMU.variableTyp (im, x))
              val etyp   = TMU.variableTyp (im, lhs)
              val rtyps   = resultType etyp
              (* Associates cptr with lhs *)
              val cptr = TMU.mkClosureFunction0 (state, lhs, ftyp, fvtyps, argtyps, rtyps)
              (* Associates cptr with each alias of lhs *)
              val () = case VD.lookup (aliases, lhs)
                        of SOME s => VS.foreach (s, fn v => TMU.stateAddCodePtr (state, v, cptr))
                         | NONE   => ()
            in ()
            end
      end
  (*
   * Handle either recursive or non-recursive value definitions.
  *) 
  and doVDef1 (state, recursiveB, env1, vd) =
      let
        val {im, cfg, globals, prelude, effects, ...} = state
      in
        case vd
         of Vthk {name = lhs, ty = _, escapes, recursive, fvs, body = e} =>
            (case TMU.variableKind (im, lhs)
              of M.VkLocal => 
                 let
                   val fvs0 = if recursiveB then List.remove (fvs, fn s => s = lhs) else fvs
                   val fvs = List.map (fvs0, lookup1 (im, env1))
                   val fvtyps = List.map (fvs, fn x => TMU.variableTyp (im, x))
                   val fvsA = List.map (fvs, fn x => IM.variableClone (im, x)) 
                   val ftyp = M.TThunk (M.TRef)
                   (* Inner closure name *)
                   val fvar = TMU.localVariableFresh (im, IM.variableName (im, lhs), ftyp)
                   val cptr = codePtrForFunction (state, lhs)
                   (* Associate the inner closure name with the code pointer*)
                   val () = TMU.stateAddCodePtr (state, fvar, cptr)
                   val env' = List.zip (fvs0, fvsA) @ env1
                   val env' = if recursiveB then (lhs, fvar) :: env' else env'
                   val etyp = TMU.variableTyp (im, lhs)
                   val etyp'= case etyp of M.TThunk ty => ty 
                                         | _ => failMsg ("doVDef", "Bad type for thunk")
                   val res  = TMU.localVariableFresh0 (im, etyp')
                   val (blk, fx) = doExp (state, env', Vector.new1 res, e)
                   val fx = if recursiveB orelse recursive then Effect.union (fx, Effect.PartialS) else fx
                   val tr   = M.TReturn (Vector.new1 (M.SVariable res))
                   val () = TMU.mkThunkFunction1 (state, fvar, cptr, escapes, recursive, 
                                                  Vector.fromList fvsA, etyp', blk, tr, fx)
                   val _ = effects := VD.insert (!effects, lhs, fx)
                   val fks = List.map (fvs, fn x => FK.fromTyp (cfg, TMU.variableTyp (im, x)))
                   val blk0 = MS.bindRhs (lhs, M.RhsThunkMk { typ = M.FkRef, fvs = Vector.fromList fks })
                   val blk1 = MS.doRhs 
                                (M.RhsThunkInit 
                                   { typ = M.FkRef, thunk = SOME lhs, fx = Effect.Total, 
                                     code = SOME cptr, fvs = Vector.fromList (List.zip (fks, map M.SVariable fvs)) })
                 in
                   (blk0, blk1)
                 end
               | M.VkGlobal => 
                 let
                   val () = if List.isEmpty fvs then () else failMsg ("bindRhsThunk", "args not empty")
                                                                     
                   val etyp = TMU.variableTyp (im, lhs)
                   val etyp'= case etyp of M.TThunk ty => ty 
                                         | _ => failMsg ("doVDef", "Bad type for thunk")
                   val res  = TMU.localVariableFresh0 (im, etyp')
                   val (blk, fx) = doExp (state, env1, Vector.new1 res, e)

                   val name = IM.variableName (im, lhs)
                   val () = prelude := MS.seq (!prelude, blk)
                   val () = globals := VD.insert (!globals, lhs, M.GThunkValue { typ = M.FkRef, ofVal = M.SVariable res })
                 in
                   (MS.empty, MS.empty)
                 end
               | M.VkExtern => failMsg ("bindRhsThunk", "impossible"))
          | Vfun {name = lhs, ty = _, escapes, recursive, fvs, args = bs, body = e} =>
            let 
              val global = varIsGlobal (im, lhs)
              (* val _ = print ("doVfun lhs = " ^ lhs ^ " fvs = " ^ String.concat (map (fn x => x ^ " ") fvs) ^ "\n") *)
              val fvs0 = if recursiveB then List.remove (fvs, fn s => s = lhs) else fvs
              val fvs = List.map (fvs0, lookup1 (im, env1))
              val fvsA = List.map (fvs, fn x => IM.variableClone (im, x))     (* free variables *)
              val argsA = List.map (bs, #1)                                    (* actual argument *)
              val ftyp   = M.TRef
              val fvar   = TMU.localVariableFresh (im, IM.variableName (im, lhs), ftyp)
              val env'   = List.zip (fvs0, fvsA) @ List.zip (argsA, argsA) @ env1
              val env'   = if recursiveB andalso not global then (lhs, fvar) :: env' else env'
              val etyp   = TMU.variableTyp (im, lhs)
              val rtyps   = resultType etyp
              val res    = Vector.map(rtyps, fn ty => TMU.localVariableFresh0 (im, ty))
              val cptr = codePtrForFunction (state, lhs)
              (* Associates cptr with fvar *)
              val () = TMU.stateAddCodePtr (state, fvar, cptr)
              val (blk, fx) = doExp (state, env', res, e)
              val fx = if recursiveB orelse recursive then Effect.union (fx, Effect.PartialS) else fx
              val tr = M.TReturn (Vector.map (res, M.SVariable))
              val () = TMU.mkClosureFunction1 (state, fvar, cptr, escapes, recursive, 
                                               Vector.fromList fvsA, Vector.fromList argsA, rtyps, blk, tr, fx)
              val _ = effects := VD.insert (!effects, lhs, fx)
              val fvs    = Vector.fromListMap (fvs, fn x => (FK.fromTyp (cfg, TMU.variableTyp (im, x)), M.SVariable x))
            in
              case TMU.variableKind (im, lhs)
               of M.VkLocal  => 
                  let
                    val blk0 = MS.bindRhs (lhs, M.RhsClosureMk { fvs = Vector.map (fvs, #1) })
                    val blk1 = MS.doRhs (M.RhsClosureInit { cls = SOME lhs, code = SOME cptr, fvs = fvs })
                  in
                    (blk0, blk1)
                  end
                | M.VkGlobal =>
                  let
                    val () = if Vector.length fvs = 0 then () else failMsg ("bindRhsClosure", "fvs is not empty")
                    val _ = globals := VD.insert (!globals, lhs, M.GClosure { code = SOME cptr, fvs = fvs})
                  in
                    (MS.empty, MS.empty)
                  end
                | M.VkExtern => failMsg ("bindRhsClosure", "impossible")
            end
      end
        
  and doVDefg (state, env) 
    = let
        val {im, cfg, globals, prelude, ...} = state
        fun getDefVar def = case def of Vfun {name, ...} => name | Vthk {name, ...} => name
        val ws = Config.targetWordSize cfg
      in
        fn Rec defs =>
          let
            val vars = List.map (defs, getDefVar)
            val env1 = List.zip (vars, vars) @ env
            val () = List.foreach (defs, fn vd => doVDef0 (state, true, env1, vd))
            val (blk0, blk1) = List.unzip (List.map (defs, fn vd => doVDef1 (state, true, env1, vd)))
          in
            (env1, MS.seqn (blk0 @ blk1), Effect.Total)
          end
        |  Nonrec def =>
          let 
            val var = getDefVar def
            val () = doVDef0 (state, false, env, def)
            val (blk0, blk1) = doVDef1 (state, false, env, def)
          in
            ((var, var) :: env, MS.seq (blk0, blk1), Effect.Total)
          end
        | Vdef (lhs, e) => 
         let
           val (lhs, tys) = List.unzip lhs
           val lhs = Vector.fromList lhs
           val n = Vector.length lhs
           val vks = Vector.map (lhs, fn v => TMU.variableKind (im, v))
           val kind = 
               if Vector.length vks > 0 then 
                 let
                   val kind = Vector.sub (vks, 0)
                   val () = if Vector.forall (vks, fn k => k = kind) then () 
                            else failMsg ("doVdefg/Vdef", "expect uniform variable kinds for lhs")
                 in kind
                 end
               else
                 M.VkLocal
           fun doRhs lhs = 
               let
                 val (blk, fx) = doExp (state, env, lhs, e)
               in
                 (Vector.toList (Vector.map (lhs, fn v => (v, v))) @ env, blk, fx)
               end
         in
           case kind
             of M.VkLocal => doRhs lhs
             (*
               if n > 1 andalso GP.noMultiReturn cfg 
                 then
                   let
                     val u = TMU.localVariableFresh0 (im, M.Ref)
                     val (env, blk, fx) = doRhs u
                     val fixed = Vector.map (tys, fn ty => M.FD { kind = FK.fromTyp (cfg, ty),
                                   alignment = M.Vs8, var = M.FvReadOnly })
                     val td = M.TD { fixed = fixed, array = NONE }
                     fun sub (i, v, blk) 
                       = MS.seq (im, cfg, blk, MS.bindRhs (im, cfg, v, 
                           M.RhsTupleSub (M.TF { tupDesc = td, tup = u, field = M.FiFixed i })
                     val blk = Vector.foldi (lhs, blk, sub)
                   in
                     (Vector.toList (Vector.map (lhs, fn v => (v, v))) @ env, blk, fx)
                   end
                 else doRhs lhs
                *)
              | M.VkGlobal => (* NOTE: unboxed tuples are never global *)
               let
                 val res = TMU.variablesClone (im, lhs)
                 val (blk, _) = doExp (state, env, res, e)
                 val () = prelude := MS.seq (!prelude, blk)
                 val () = globals := Vector.fold (Vector.zip (lhs, res), !globals,
                                       fn ((lhs, res), g) => VD.insert (g, lhs, M.GSimple (M.SVariable res)))
               in
                 (Vector.toList (Vector.map (lhs, fn v => (v, v))) @ env, MS.empty, Effect.Total)
               end
              | M.VkExtern => failMsg ("bindRhsExp", "impossible")
         end
      end

  fun globalizePrelude state =
      let
        val { im, cfg, globals, prelude, ... } = state
        val tr = M.TReturn (Vector.new0 ())
        val blocks = MF.toBlocksL (MS.finish (IM.labelFresh im, Vector.new0 (), !prelude, tr))
        val is = IS.SiManager im
        (* val () = L.outputWidth (L.align [L.str "globalize blks:", 
                                             L.indent (L.align (List.map (blocks, 
                                             fn b => ML.layoutBlock (cfg, is, b))), 2)], 78, Out.standard)
        *)
        fun doInstruction (instr as (M.I { dests, rhs, ... })) =
            let
              fun fail msg  = failMsg ("globalizePrelude", msg ^ ":" ^
                                        Layout.toString (ML.layoutInstruction (cfg, IS.SiManager im, instr)))
              val () = if Vector.length dests <> 1 then fail "lhs must be 1 variable" else ()
              val lhs = Vector.sub (dests, 0)
              val () = TMU.setVariableKind (im, lhs, M.VkGlobal)
              val rhs = case rhs
                          of M.RhsSimple v => M.GSimple v
                           | M.RhsTuple { mdDesc, inits } => M.GTuple { mdDesc = mdDesc, inits = inits }
                           | M.RhsTupleSub (M.TF { tupDesc, tup, field = M.FiFixed i }) =>
                             (case VD.lookup (!globals, tup)
                               of SOME (M.GTuple { mdDesc, inits }) => M.GSimple (Vector.sub (inits, i))
                                | _ => fail "global tuple sub used on a local tuple")
                           | M.RhsThunkValue { typ, thunk, ofVal } => M.GThunkValue { typ = typ, ofVal = ofVal } 
                           | M.RhsThunkGetValue { typ, thunk } =>
                             (case VD.lookup (!globals, thunk)
                               of SOME (M.GThunkValue { typ, ofVal }) => M.GSimple ofVal
                                | _ => fail "global thunkGetValue used on a local thunk")
                           | M.RhsClosureInit { cls, code = SOME code, fvs } => (* TODO: asserts fvs is empty *)
                             M.GClosure { code = SOME code, fvs = fvs }
                           | M.RhsSum { tag, typs, ofVals } => M.GSum { tag = tag, typs = typs, ofVals = ofVals }
                           | M.RhsSumProj { typs, sum, tag, idx } =>
                             (case VD.lookup (!globals, sum)
                               of SOME (M.GSum { tag, typs, ofVals }) => (* TODO: asserts same tag *)
                                 M.GSimple (Vector.sub (ofVals, idx))
                               | _ => fail "global rhsSumProj used on a local Sum")
                           | _ => fail "not handled"
            in
              globals := VD.insert (!globals, lhs, rhs)
            end
        fun doTransfer tr = 
            let
               fun fail msg  = failMsg ("globalizePrelude", msg ^ ":" ^
                                         Layout.toString (ML.layoutTransfer (cfg, IS.SiManager im, tr)))
               fun globalToString (v, g) = Layout.toString (MilLayout.layoutGlobal (cfg, IS.SiManager im, (v, g)))
            in
              case tr
                of M.TGoto _ => ()
                 | M.TReturn rets => if Vector.length rets = 0 then () else fail "not handled"
                 | M.TInterProc { callee = M.IpEval { typ, eval }, ret = M.RNormal { rets, ... }, fx } =>
                   let
                     val () = if Vector.length rets <> 1 then fail "lhs of eval must be 1 variable" else ()
                     val lhs = Vector.sub (rets, 0)
                     val () = TMU.setVariableKind (im, lhs, M.VkGlobal)
                     val thunk = case eval
                                   of M.EThunk { thunk, ... } => thunk
                                    | M.EDirectThunk { thunk, ... } => thunk
                     fun traceVar v = 
                         let
                           fun trace (traced, v) =
                               if VS.member (traced, v) then fail "recursivedly defined global"
                               else 
                                 case VD.lookup (!globals, v) 
                                   of SOME (e as (M.GErrorVal _)) => e
                                    | SOME (M.GSimple (M.SVariable u)) => trace (VS.insert (traced, v), u)
                                    | SOME u => fail ("global eval applied to a (non-bottom) global variable of value: " ^ 
                                                      globalToString (v, u) ^ " at instruction: ")
                                    | NONE => fail "non-existent global variable"
                         in
                           trace (VS.empty, v)
                         end
                     val rhs = 
                       case VD.lookup (!globals, thunk)
                         of SOME (M.GThunkValue { typ, ofVal }) => M.GSimple ofVal
                          | SOME (M.GSimple (M.SVariable v)) => traceVar v
                          | SOME (e as (M.GErrorVal _)) => e
                          | SOME u => fail ("global eval applied to: " ^ globalToString (thunk, u) ^ " at instruction: ")
                          | NONE => fail "global eval applied to a local thunk"
                   in
                     globals := VD.insert (!globals, lhs, rhs)
                   end
                | _ => fail "not handled"
            end

        fun doBlock (M.B { instructions, transfer, ... }) = 
            let 
              val _ = Vector.map (instructions, doInstruction) 
              val _ = doTransfer transfer
            in 
              () 
            end
        val _ = List.map (blocks, doBlock o #2)
        val () = prelude := MS.empty
      in
        ()
      end

  fun doGlobal (state, printf) 
    = let
        val { im, cfg, globals, ... } = state
        val ws = Config.targetWordSize cfg
        val entryLabel = IM.labelFresh im
        val blkE = MU.goto (im, cfg, entryLabel, Vector.new0 ())
        val (topBlk, cuts) =
            if CoreHsParse.noMainWrapper cfg then
              let
                (* top level exception handler *)
                val cutVar = TMU.localVariableFresh (im, "topHandler", GP.exnHandlerTyp)
                val blk1 = GP.setHandler (state, M.SVariable cutVar)
                val blk2 = blkE
                val evar = TMU.localVariableFresh0 (im, M.TRef)
                val mvar = TMU.globalVariableFresh0 (im, M.TCString)
                val g = M.GCString "Top level exception caught! Abort immediately!\n"
                val () = globals := VD.insert (!globals, mvar, g)
                val blk3 = MU.call (im, cfg, M.CCode { ptr = printf, code = TMU.noCode},
                                    Vector.new2 (M.SVariable mvar, M.SConstant (M.CInteger 0)), 
                                    TMU.noCut, Effect.IoS, Vector.new0 ())
                val (cutLabel, blk3) =
                    let
                      fun geTyp () = M.TCode {cc = M.CcCode, args = Vector.new0 (), ress = Vector.new1 M.TRef}
                      val (ge, _) = TMU.externVariable (state, GP.pkgName, "ihrExceptionExnGet", geTyp)
                      val call = M.CCode {ptr = ge, code = TMU.noCode}
                      val b = MU.call (im, cfg, call, Vector.new0 (), TMU.noCut, Effect.Heap, Vector.new1 evar)
                      val l = IM.labelFresh im
                      val b = MS.prependTL (M.TGoto (M.T { block = l, arguments = Vector.new0 () }), 
                                            l, Vector.new0 (), MS.seq (b, blk3))
                    in (l, b)
                    end
                (*val (cutLabel, blk3) = MS.label (im, cfg, Vector.new1 evar, blk3)*)
                val blk0 = MS.bindRhs (cutVar, M.RhsCont cutLabel)
                val blk4 = MU.return (im, cfg, Vector.new0 ())
                val topBlk = MS.seqn [blk0, blk1, blk2, blk3, blk4]
              in (topBlk, TMU.targetCut cutLabel)
              end
            else
              (blkE, TMU.exitCut)
        fun mkMain blks =
            let
              val blks = MS.prependTL (M.TGoto (M.T { block = entryLabel, arguments = Vector.new0 () }),
                                       entryLabel, Vector.new0 (), blks)
              val blks = MS.seq (topBlk, blks)
              val blks = GP.wrapThread (state, blks)
              val (blk0, _) = GP.externDo0 Effect.Any "ihrInit" (state, [])
              val tr = M.TReturn (Vector.new0 ())
              val blks = MS.seq (blk0, blks)
              val main = TMU.mkMainFunction (state, "main", blks, tr, Effect.Any)
            in main
            end
        fun iterate (env, main, blks) = 
          fn vdefg :: vdefgs =>
            let
              val (env, vblks, _) = doVDefg (state, env) vdefg
            in
              iterate (env, main, MS.seq (blks, vblks)) vdefgs
            end
         |  [] =>
           (case TMU.variableTyp (im, main)
              of M.TThunk ftyp => 
                let
                  val fvar = TMU.localVariableFresh0 (im, ftyp)
                  val eval = 
                      let
                        val code = TMU.stateGetCodesForFunction (state, main)
                      in M.EThunk { thunk = main, code = code }
                      end
                  val blk1 = MU.eval (im, cfg, FK.fromTyp (cfg, ftyp), eval, cuts, Effect.Total, fvar)
                  (* make a State# RealWorld# value *)
                  val worldC = GP.tagToConst (ws, 0)
                  val stateC = GP.tagToConst (ws, 0)
                  val wtyp = M.TSum {tag = GP.tagTyp ws, arms = Vector.new1 (worldC, Vector.new0 ())}
                  val wvar = TMU.localVariableFresh0 (im, wtyp)
                  val rhs = M.RhsSum {tag = worldC, ofVals = Vector.new0 (), typs = Vector.new0 ()}
                  val blk2 = MS.bindRhs (wvar, rhs)
                  val wvar' = TMU.localVariableFresh0 (im, M.TThunk wtyp)
                  val rhs = M.RhsThunkValue {typ = M.FkRef, thunk = NONE, ofVal = M.SVariable wvar}
                  val blk2' = MS.bindRhs (wvar', rhs)
                  val styp = M.TSum {tag = GP.tagTyp ws, arms = Vector.new1 (stateC, Vector.new0 ())}
                  val svar = TMU.localVariableFresh0 (im, styp)
                  val rhs = M.RhsSum {tag = stateC, ofVals = Vector.new0 (), typs = Vector.new0 ()}
                  val blk3 = MS.bindRhs (svar, rhs)
                  val svar' = TMU.localVariableFresh0 (im, M.TThunk styp)
                  val rhs = M.RhsThunkValue {typ = M.FkRef, thunk = NONE, ofVal = M.SVariable svar}
                  val blk3' = MS.bindRhs (svar', rhs)
                  (* call the main IO function with state *)
                  val rvar = Vector.new2 (TMU.localVariableFresh0 (im, M.TRef),
                                          TMU.localVariableFresh0 (im, M.TRef))
                  val blk4 = MU.call (im, cfg, M.CClosure {cls = fvar, code = TMU.noCode},
                                       Vector.new1 (M.SVariable svar'), cuts, Effect.Any, rvar)
                  val blks = MS.seqn [blks, blk1, blk2, blk2', blk3, blk3', blk4]
                  val main = mkMain blks
                in main
                end
               | M.TNumeric (MP.NtInteger _) =>
                let
                  val svar = TMU.globalVariableFresh0 (im, M.TCString)
                  val _ = globals := VD.insert (!globals, svar, M.GCString "%d\n")
                  val blk1 = MU.call (im, cfg, M.CCode { ptr = printf, code = TMU.noCode },
                                     Vector.new2 (M.SVariable svar, M.SVariable main), cuts, Effect.Any, Vector.new0 ())
                  val blks = MS.seq (blks, blk1)
                  val main = mkMain blks
                in main
                end
               | _ => failMsg ("doGlobal", "wrong type for main, expect IO () or Int#"))
      in
        iterate
      end

  (* Avoid declaring externs that's already declared in included C header files *)
  val declaredExterns = 
      [ ("stdio",  ["getenv", "printf", "performMajorGC", "gettimeofday"])
      , ("string", ["memcmp", "memchr", "memcpy", "strerror"])
      , ("wincon", ["GetConsoleCP"])
      , ("winnls", ["GetACP"])
      , ("winsock", ["closesocket", "recv", "send"])
      , ("winbase", ["LocalFree", "GetCurrentProcess", "GetProcessTimes", "FindFirstFileW"])
      , ("stdlib", ["malloc", "free", "realloc", "calloc", "realpath"])
      , ("math", 
          [ "truncf", "trunc", "sin", "sinf", "cos", "cosf", "tan"
          , "tanf", "atan", "atanf", "acos", "acosf", "asin", "asinf", "ctan"
          , "ctanf", "atan2", "atan2f", "floor", "floorf" ])
      , ("time", ["mktime", "localtime"])
      , ("plsr-prims-ghc-longlong", 
           [ "hs_gtWord64", "hs_geWord64", "hs_eqWord64", "hs_neWord64"
           , "hs_ltWord64", "hs_leWord64", "hs_gtInt64", "hs_geInt64", "hs_eqInt64"
           , "hs_neInt64", "hs_ltInt64", "hs_leInt64", "hs_remWord64"
           , "hs_quotWord64", "hs_remInt64", "hs_quotInt64", "hs_negateInt64"
           , "hs_plusInt64", "hs_minusInt64", "hs_timesInt64", "hs_and64", "hs_or64"
           , "hs_xor64", "hs_not64", "hs_uncheckedShiftL64", "hs_uncheckedShiftRL64"
           , "hs_uncheckedIShiftL64", "hs_uncheckedIShiftRA64"
           , "hs_uncheckedIShiftRL64", "hs_intToInt64", "hs_int64ToInt"
           , "hs_int64ToWord64", "hs_wordToWord64", "hs_word64ToWord", "hs_word64ToInt64"])
      ]

  fun filterDeclaredExterns (st, externs, declared) =
      let
        val hidden = VS.keepAll (externs, fn v => not (VS.member (declared, v)))
        val externs = VS.difference (externs, hidden)
        val nameMap = SD.fromList (List.map (VS.toList externs, fn v => (I.variableName (st, v), v)))
        fun declare (header, names) = 
            let
              val vs = VS.fromList (List.keepAllMap (names, fn n => SD.lookup (nameMap, n)))
              (* merge hidden in to stdio declaration, a hack to prevent them from showing up *)
              val vs = if header = "stdio" then VS.union (hidden, vs) else vs
            in
              (header, vs)
            end
        val declared = List.map (declaredExterns, declare)
        val declared = List.keepAll (declared, fn (headers, vars) => not (VS.isEmpty vars))
        val includes = List.map (declared, fn (header, vars) => M.IF { name = header, kind = M.IkC, externs = vars })
        val included = List.map (declared, #2)
        val externs = List.fold (included, externs, fn (vars, externs) => VS.difference (externs, vars))
      in
        (includes, externs)
      end

  fun compile (((Module (main, vdefgs), oldim), aliases), pd) =
      let
        val im = IM.fromExistingNoInfo oldim 
        val cfg = PassData.getConfig pd
        val stateful = ref VS.empty
        val worlds = ref VS.empty
        val realWorld = IM.nameFromString (im, CL.qNameToString (CHP.tcRealWorld))
        fun convertInfo v = 
            let 
              val (t, k) = I.variableInfo (oldim, v)
              val _ = case t
                        of Prim (GHCPrimType.State c) =>
                          let
                            val () = stateful := VS.insert (!stateful, v)
                            val () =
                                case c
                                 of Sum [(c, [])] => worlds := VS.insert(!worlds, v)
                                  | _             => ()
                          in ()
                          end
                         | _ => ()
            in
              M.VI { typ = doType (im, cfg) t, kind = case k of AS.VkGlobal => M.VkGlobal | AS.VkLocal => M.VkLocal }
            end
        val _ = List.map (I.listVariables oldim, fn v => IM.variableSetInfo (im, v, convertInfo v))
        val (rvar, rval) = GP.initStableRoot (im, cfg)
        val state = TMU.newState (im, cfg, aliases, VD.empty, VD.fromList [(rvar, rval)], 
                                  MS.empty, rvar, SD.empty, VD.empty, !stateful, !worlds)
        val ws = Config.targetWordSize cfg
        val v1_typ = M.TCode { cc   = M.CcUnmanaged M.AbiCdecl
                             , args = Vector.new2 (M.TCString, GP.intTyp ws)
                             , ress = Vector.new0 () }
        val v1_printf = IM.variableFresh (im, "printf", M.VI { typ = v1_typ, kind = M.VkExtern })
        val main = doGlobal (state, v1_printf) ([], main, MS.empty) vdefgs
        val () = globalizePrelude state
        fun finish (v, t as M.VI { typ, kind }, externs) =
            (t, case kind of M.VkExtern => VS.insert (externs, v) | _ => externs)
        val (symtable, externs) = IM.finishMapFold (im, VS.empty, finish)
        (* 
        val externs = VS.remove (externs, v1_printf)
        val externs = VS.keepAll (externs, fn v => not (SS.member (declaredExterns, I.variableName (symtable, v))))
        *)
        val primExterns = SD.fold (!(#externs state), VS.empty,
                                   fn (n, (p, v), s) => if p = GP.pkgName then VS.insert (s, v) else s)
        val stdio = M.IF { name = "stdio" , kind = M.IkC , externs = VS.singleton v1_printf }
        val declared = VS.fromList (List.map ((SD.range (!(#externs state))), #2))
        val (includes, externs) = filterDeclaredExterns (symtable, externs, declared)
        val prim = M.IF { name = GP.pkgName, kind = M.IkTarget, externs = primExterns }
        val externs = M.EG { kind = M.IkC, externs = VS.difference (externs, primExterns) }
        val prog = M.P { includes    = Vector.fromList (prim :: includes)
                       , externs     = Vector.new1 externs
                       , globals     = !(#globals state)
                       , symbolTable = symtable
                       , entry       = main }
      in
        prog
      end

  val layoutOut = Utils.Function.flipIn ANormStrictLayout.layout
  val description = {name        = passname,
                     description = "Strict A-Normal Form to Mil",
                     inIr        = { printer = fn ((p, fa), config) => ANormStrictLayout.layout (config, p),
                                     stater  = fn _ => Layout.str "No stats yet"},
                     outIr       = MilUtils2.irHelpers,
                     mustBeAfter = [],
                     stats       = []}

  val associates = {controls = [], debugs = [], features = [], subPasses = []}

  val pass = Pass.mkCompulsoryPass (description, associates, compile)

end
