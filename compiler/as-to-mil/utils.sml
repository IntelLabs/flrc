(* COPYRIGHT_NOTICE_1 *)

(* Some common code shared between ANormStrictToMil and GHCPrim *)
structure HsToMilUtils =
struct
  structure I  = Identifier
  structure IM = Identifier.Manager
  structure M  = Mil
  structure SD = StringDict
  structure SS = StringSet
  structure LS = M.LS
  structure VS = M.VS
  structure VD = M.VD
  structure ND = M.ND
  structure FK = MilUtils.FieldKind
  structure MS = MilStream
  structure MF = MilFragment
  structure MU = MilStreamUtilsF
               (type state    = M.symbolTableManager
                type env      = Config.t
                val getStm    = fn x => x
                val getConfig = fn x => x)
  structure Chat = ChatF (struct 
                            type env = Config.t
                            val extract = fn a => a
                            val name = "CoreHsParse"
                            val indent = 2
                          end)

  type state = { im         : Mil.symbolTableManager
               , cfg        : Config.t
               , globals    : M.globals ref
               , prelude    : MS.t ref
               , stableRoot : I.variable
               , externs    : (string * I.variable) SD.t ref
               , packages   : string VD.t ref
               , effects    : Effect.set VD.t ref
               , stateful   : VS.t
               , worlds     : VS.t
               }
  
  val noCode  = { possible = VS.empty, exhaustive = false }
  val noCut = M.C { exits = false, targets = LS.empty }
  val exitCut = M.C { exits = true, targets = LS.empty }
  fun targetCut target = M.C { exits = false, targets = LS.singleton target }

  fun localVariableFresh   (im, name, typ) = IM.variableFresh (im, name, M.VI { typ = typ, kind = M.VkLocal })
  fun localVariableFresh0  (im, typ)       = localVariableFresh (im, "", typ)
  fun globalVariableFresh  (im, name, typ) = IM.variableFresh (im, name, M.VI { typ = typ, kind = M.VkGlobal })
  fun globalVariableFresh0 (im, typ)       = globalVariableFresh (im, "", typ)
  fun variableTyp (im, v) = let val M.VI { typ, ... } = IM.variableInfo (im, v) in typ end
  fun variableKind (im, v) = let val M.VI { kind, ... } = IM.variableInfo (im, v) in kind end
  fun setVariableKind (im, v, kind) = 
      let 
        val M.VI { typ, ... } = IM.variableInfo (im, v) 
      in 
        IM.variableSetInfo (im, v, M.VI { typ = typ, kind = kind }) 
      end

  fun newState (im, cfg, globals, prelude, stableRoot, externs, packages, effects, stateful, worlds) : state = 
      { im = im, cfg = cfg, globals = ref globals, prelude = ref prelude, 
        stableRoot = stableRoot, externs = ref externs, 
        packages = ref packages, effects = ref effects, stateful = stateful, worlds = worlds }

  (* either lookup an existing extern variable, or make a new one *)
  fun externVariable (state : state, pname, str, mkTyp) = 
      let
        val { im, externs, packages, ... } = state
      in
        case SD.lookup (!externs, str)
          of SOME (_, fvar) => 
            let
              (* TODO: maybe give an error if there is a mismatch between typ and mkTyp () *)
              val M.VI { typ, ... } = IM.variableInfo (im, fvar)
            in
              (fvar, typ)
            end
          | NONE =>
            let
              val typ  = mkTyp ()
              val fvar = IM.variableFresh (im, str, M.VI { typ = typ, kind = M.VkExtern })
              val () = externs := SD.insert (!externs, str, (pname, fvar))
              val () = packages := VD.insert (!packages, fvar, pname)
            in
              (fvar, typ)
            end
      end

  (*
   * construct a global function with some code blocks
   *)
  fun mkFunction (cc, ccTyp) (state : state, name, args, argtyps, rettyp, blks, transfer, fx)
    = let
        val { im, cfg, globals, effects, ... } = state
        val entry = IM.labelFresh im 
        val blks = MS.finish (entry, Vector.new0 (), blks, transfer) (* close entry *)
        val code = M.GCode (M.F 
                 { fx        = fx
                 , escapes   = true
                 , recursive = true
                 , cc        = cc
                 , args      = Vector.fromList args
                 , rtyps     = rettyp
                 , body      = M.CB
                             { entry  = entry
                             , blocks = MF.toBlocksD blks
                             }
                  })
        val ftyp = M.TCode { cc = ccTyp, args = Vector.fromList argtyps , ress = rettyp } 
        val fvar = globalVariableFresh (im, name, ftyp)
        val _ = globals := VD.insert (!globals, fvar, code)
        val _ = effects := VD.insert (!effects, fvar, fx)
      in
        fvar
      end

  val mkCodeFunction = mkFunction (M.CcCode, M.CcCode)

  val mkThunkFunction 
    = fn (state, fvar, ftyp, args, rettyp, blks, transfer, fx) =>
      let
        val { im, ... } = state
        val name = IM.variableName (im, fvar)
        val typs = List.map (args, fn x => variableTyp (im, x))
        val ccCode = M.CcThunk { thunk = fvar, fvs = Vector.fromList args }
        val ccType = M.CcThunk { thunk = ftyp, fvs = Vector.fromList typs }
      in mkFunction (ccCode, ccType) (state, name, [], [], Vector.new1 rettyp, blks, transfer, fx)
      end

  val mkClosureFunction
    = fn (state, fvar, ftyp, n, args, rettyp, blks, transfer, fx) =>
      let
        val { im, ... } = state
        val name = IM.variableName (im, fvar)
        val typs = List.map (args, fn x => variableTyp (im, x))
        val (fvs, args) = List.splitAt (args, n)
        val (fts, typs) = List.splitAt (typs, n)
        val ccCode = M.CcClosure { cls = fvar, fvs = Vector.fromList fvs }
        val ccType = M.CcClosure { cls = ftyp, fvs = Vector.fromList fts }
      in mkFunction (ccCode, ccType) (state, name, args, typs, rettyp, blks, transfer, fx)
      end

  (* Make a non-recursive thunk that evaluates (fvar args), ignores the result(s),
   * and returns null.
   *)
  fun mkThunk (state, thk, fvar, args, rtyps, fx) 
    = let
        val { im, cfg, ... } = state
        val ftyp = variableTyp (im, fvar)
        val fvar' = IM.variableClone (im, fvar)
        val rVs = Vector.map (rtyps, fn t => localVariableFresh0 (im, t))
        val rOs = Vector.map (rVs, M.SVariable)
        (* gvar is self variable *)
        val gtyp = M.TThunk M.TRef
        val gvar = localVariableFresh0 (im, gtyp)
        val params = List.map (args, fn v => IM.variableClone (im, v))
        val blk = MU.call (im, cfg, M.CClosure { cls = fvar', code = noCode },
                            Vector.fromListMap (params, M.SVariable), exitCut, fx, rVs)
        val transfer = M.TReturn (Vector.new1 (M.SConstant (M.CRef 0)))
        val params = fvar' :: params
        val args = fvar :: args
        val afun = mkThunkFunction (state, gvar, gtyp, params, M.TRef, blk, transfer, fx)
        val typs = List.map (args, fn v => variableTyp (im, v))
        val fks = List.map (typs, fn t => FK.fromTyp (cfg, t))
      in
        MS.bindRhs (thk, M.RhsThunkInit 
          { typ = M.FkRef, thunk = NONE, fx = Effect.Total, code = SOME afun 
          , fvs = Vector.fromListMap (List.zip (fks, args), fn (fk, v) => (fk, M.SVariable v)) })
      end

  (* generate code block that either forces a thunk into a value, or just skip *)
  fun kmThunk (im, cfg, rvar, fvar, fx) 
    = let
        val ftyp = variableTyp (im, fvar)
      in
        case ftyp
          of M.TThunk typ => 
            let
              (* val _ = print ("kmThunk fvar = " ^ * Layout.toString(CF.layoutVariable(cf, fvar)) ^ " \n") *)
              val fk   = FK.fromTyp (cfg, ftyp)  
              val eval = M.EThunk { thunk = fvar, code = noCode }
              val blk  = MU.eval (im, cfg, fk, eval, exitCut, fx, Vector.sub (rvar, 0))
            in blk
            end
           | _ => Fail.fail ("ToMilUtil", "kmThunk", "impossible: " 
                                                     ^ IM.variableString (im, Vector.sub (rvar, 0))
                                                     ^ " = eval "
                                                     ^ IM.variableString (im, fvar))
      end

  fun stripLN s = String.dropTrailing (s, #"\n")
  fun stripCR s = String.dropTrailing (s, #"\r")
  val ghcLibRoot = ref NONE
  fun getGhcLibRoot cfg = 
      case !ghcLibRoot
        of SOME d => d
         | NONE => 
          let
            fun run (cmd, args) = Pass.runCmd (cmd, args, [], true)
            val d = stripCR (stripLN (run ("ghc", ["--print-libdir"])))
            val () = ghcLibRoot := SOME d
          in
            d
          end

  fun lookupEffect (effects, v, isStateful, isIO) = 
      case VD.lookup (!effects, v) 
        of SOME fx => fx
         | NONE => if isIO then Effect.Any 
                     else if isStateful then Effect.union (Effect.HeapInit, Effect.Control)
                       else Effect.Control
                                     
  fun constInt (i, arbTyp) 
    = case arbTyp
        of SOME arbTyp => M.CIntegral (IntArb.fromInt (arbTyp, i))
         | NONE => M.CInteger (IntInf.fromInt i)
  val simpleInt = M.SConstant o constInt


  fun variablesClone (im, vs) = Vector.map (vs, fn v => IM.variableClone (im, v))
end

