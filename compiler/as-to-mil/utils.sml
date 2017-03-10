(*
 * Redistribution and use in source and binary forms, with or without modification, are permitted 
 * provided that the following conditions are met:
 * 1.   Redistributions of source code must retain the above copyright notice, this list of 
 * conditions and the following disclaimer.
 * 2.   Redistributions in binary form must reproduce the above copyright notice, this list of
 * conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 * BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

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
               , aliases    : VS.t VD.t
               , codePtrs   : M.variable VD.t ref
               , globals    : M.globals ref
               , prelude    : MS.t ref
               , stableRoot : I.variable
               , externs    : (string * I.variable) SD.t ref
               , effects    : Effect.set VD.t ref
               }

  val noCode  = { possible = VS.empty, exhaustive = false }
  val noCut = M.C { exits = false, targets = LS.empty }
  val exitCut = M.C { exits = true, targets = LS.empty }

  val stateAddCodePtr = 
   fn (state, fvar, cptr) => 
      let
        val { codePtrs, ...} = state
        val () = codePtrs := VD.insert (!codePtrs, fvar, cptr)
      in ()
      end

  val stateGetCodePtr = 
   fn (state, fvar) => 
      let
        val { codePtrs, ...} = state
      in VD.lookup (!codePtrs, fvar)
      end

  val stateGetCodesForFunction = 
   fn (state, fvar) =>
      (case stateGetCodePtr (state, fvar)
        of SOME cptr => { possible = VS.singleton cptr, exhaustive = true}
         | NONE      => noCode)

  fun targetCut target = M.C { exits = false, targets = LS.singleton target }

  fun localVariableFresh   (im, name, typ) = IM.variableFresh (im, name, M.VI { typ = typ, kind = M.VkLocal })
  fun localVariableFresh0  (im, typ)       = localVariableFresh (im, "", typ)
  fun globalVariableFresh  (im, name, typ) = IM.variableFresh (im, name, M.VI { typ = typ, kind = M.VkGlobal })
  fun globalVariableFresh0 (im, typ)       = globalVariableFresh (im, "", typ)
  fun globalVariableRelated  (im, v, s, typ) = IM.variableRelated (im, v, s, M.VI { typ = typ, kind = M.VkGlobal })
  fun variableTyp (im, v) = let val M.VI { typ, ... } = IM.variableInfo (im, v) in typ end
  fun variableKind (im, v) = let val M.VI { kind, ... } = IM.variableInfo (im, v) in kind end
  fun setVariableKind (im, v, kind) = 
      let 
        val M.VI { typ, ... } = IM.variableInfo (im, v) 
      in 
        IM.variableSetInfo (im, v, M.VI { typ = typ, kind = kind }) 
      end

  fun newState (im, cfg, aliases, codePtrs, globals, prelude, stableRoot, externs, effects) : state = 
      { im = im, cfg = cfg, aliases = aliases, codePtrs = ref codePtrs, globals = ref globals, prelude = ref prelude, 
        stableRoot = stableRoot, externs = ref externs, 
        effects = ref effects }

  (* either lookup an existing extern variable, or make a new one *)
  fun externVariable (state : state, pname, str, mkTyp) = 
      let
        val { im, externs, ... } = state
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
            in
              (fvar, typ)
            end
      end

  fun mkFunctionPtr (state : state, fvar : M.variable, 
                     ccTyp : M.typ M.callConv, argtyps : M.typ Vector.t, rettyp : M.typ Vector.t) = 
      let
        val { im, ... } = state
        val ftyp = M.TCode { cc = ccTyp, args = argtyps , ress = rettyp } 
        val cptr = globalVariableRelated (im, fvar, "code", ftyp)
        val () = stateAddCodePtr (state, fvar, cptr)
      in cptr
      end
  (*
   * construct a global function with some code blocks
   *)
  fun mkFunction cc (state : state, cptr, escapes, recursive, args, rettyp, blks, transfer, fx) =
      let
        val { im, cfg, globals, effects, ... } = state
        val entry = IM.labelFresh im 
        val blks = MS.finish (entry, Vector.new0 (), blks, transfer) (* close entry *)
        val code = M.GCode (M.F 
                 { fx        = fx
                 , escapes   = escapes
                 , recursive = recursive
                 , cc        = cc
                 , args      = args
                 , rtyps     = rettyp
                 , body      = M.CB
                             { entry  = entry
                             , blocks = MF.toBlocksD blks
                             }
                  })
        val _ = globals := VD.insert (!globals, cptr, code)
        val _ = effects := VD.insert (!effects, cptr, fx)
      in ()
      end

  val mkNamedFunction =
   fn (state, name, ccCode, ccType, escapes, recursive, args, argtyps, rettyps, blks, transfer, fx) =>
      let
        val { im, ... } = state
        val ftyp = M.TCode { cc = ccType, args = argtyps , ress = rettyps } 
        val cptr = globalVariableFresh (im, name, ftyp)
        val () = mkFunction ccCode (state, cptr, escapes, recursive, args, rettyps, blks, transfer, fx)
      in cptr
      end

  val mkMainFunction = 
   fn (state, name, blks, transfer, fx) => 
      mkNamedFunction (state, name, M.CcCode, M.CcCode, true, false,
                       Vector.new0 (), Vector.new0 (), Vector.new0 (), blks, transfer, fx)

  val mkThunkFunction0 =
   fn (state, fvar, ftyp, fvtyps, rettyp) =>
      let
        val ccType = M.CcThunk { thunk = ftyp, fvs = fvtyps }
        val rettyp = Vector.new1 rettyp
        val cptr = mkFunctionPtr (state, fvar, ccType, Vector.new0 (), rettyp)
      in cptr
      end

  val mkThunkFunction1 =
   fn (state, fvar, cptr, escapes, recursive, fvs, rettyp, blks, transfer, fx) =>
      let
        val ccCode = M.CcThunk { thunk = fvar, fvs = fvs }
        val rettyp = Vector.new1 rettyp
        val () = mkFunction ccCode (state, cptr, escapes, recursive, Vector.new0 (), rettyp, blks, transfer, fx)
      in ()
      end

  val mkThunkFunction =
   fn (state, fvar, ftyp, escapes, recursive, fvs, rettyp, blks, transfer, fx) =>
      let
        val { im, ... } = state
        val fvtyps = Vector.map (fvs, fn x => variableTyp (im, x))
        val cptr = mkThunkFunction0 (state, fvar, ftyp, fvtyps, rettyp)
        val () = mkThunkFunction1 (state, fvar, cptr, escapes, recursive, fvs, rettyp, blks, transfer, fx)
      in cptr
      end

  val mkClosureFunction0 = 
   fn (state, fvar, ftyp, fvtyps, argtyps, rettyp) =>
      let
        val ccType = M.CcClosure { cls = ftyp, fvs = fvtyps }
        val cptr = mkFunctionPtr (state, fvar, ccType, argtyps, rettyp)
      in cptr
      end

  val mkClosureFunction1 = 
   fn (state, fvar, cptr, escapes, recursive, fvs, args, rettyp, blks, transfer, fx) =>
      let
        val ccCode = M.CcClosure { cls = fvar, fvs = fvs }
        val () = mkFunction ccCode (state, cptr, escapes, recursive, args, rettyp, blks, transfer, fx)
      in () 
      end

  val mkClosureFunction =
   fn (state, fvar, ftyp, escapes, recursive, fvs, args, rettyp, blks, transfer, fx) =>
      let
        val { im, ... } = state
        val fvtyps = Vector.map (fvs, fn x => variableTyp (im, x))
        val argtyps = Vector.map (args, fn x => variableTyp (im, x))
        val cptr = mkClosureFunction0 (state, fvar, ftyp, fvtyps, argtyps, rettyp)
        val () = mkClosureFunction1 (state, fvar, cptr, escapes, recursive, fvs, args, rettyp, blks, transfer, fx)
      in cptr
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
        val code = stateGetCodesForFunction (state, fvar)
        val blk = MU.call (im, cfg, M.CClosure { cls = fvar', code = code },
                            Vector.fromListMap (params, M.SVariable), exitCut, fx, rVs)
        val transfer = M.TReturn (Vector.new1 (M.SConstant (M.CRef 0)))
        val params = fvar' :: params
        val args = fvar :: args
        val afun = mkThunkFunction (state, gvar, gtyp, true, false, Vector.fromList params, M.TRef, blk, transfer, fx)
        val typs = List.map (args, fn v => variableTyp (im, v))
        val fks = List.map (typs, fn t => FK.fromTyp (cfg, t))
      in
        MS.bindRhs (thk, M.RhsThunkInit 
          { typ = M.FkRef, thunk = NONE, fx = Effect.Total, code = SOME afun 
          , fvs = Vector.fromListMap (List.zip (fks, args), fn (fk, v) => (fk, M.SVariable v)) })
      end

  (* generate code block that either forces a thunk into a value, or just skip *)
  fun kmThunk (state, rvar, fvar, fx) =
      let
        val { im, cfg, ...} = state
        val ftyp = variableTyp (im, fvar)
      in
        case ftyp
          of M.TThunk typ => 
            let
              (* val _ = print ("kmThunk fvar = " ^ * Layout.toString(CF.layoutVariable(cf, fvar)) ^ " \n") *)
              val fk   = FK.fromTyp (cfg, ftyp)  
              val code = stateGetCodesForFunction (state, fvar)
              val eval = M.EThunk { thunk = fvar, 
                                    value = not (MilUtils.Codes.exhaustive code), 
                                    code = code }
              val blk  = MU.eval (im, cfg, fk, eval, exitCut, fx, Vector.sub (rvar, 0))
            in blk
            end
           | _ => Fail.fail ("ToMilUtil", "kmThunk", "impossible: " 
                                                     ^ IM.variableString (im, Vector.sub (rvar, 0))
                                                     ^ " = eval "
                                                     ^ IM.variableString (im, fvar))
      end

  fun lookupEffect (effects, v, default) = 
      case VD.lookup (!effects, v) 
        of SOME fx => fx
         | NONE => default
                                     
  fun constInt (i, arbTyp) 
    = case arbTyp
        of SOME arbTyp => M.CIntegral (IntArb.fromInt (arbTyp, i))
         | NONE => M.CInteger (IntInf.fromInt i)
  val simpleInt = M.SConstant o constInt


  fun variablesClone (im, vs) = Vector.map (vs, fn v => IM.variableClone (im, v))
end

