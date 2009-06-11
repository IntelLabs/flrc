(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

(* Full/Functional Mil *)

signature FMIL =
sig

  type blockId = Mil.label
  type instructionId = int
  type transferId = blockId
  type globalId = Mil.variable

  datatype paramId = PiArg of int | PiCls | PiThunk | PiFreeVar of int

  datatype varDef =
      VdGlobal of Mil.global
    | VdFunParam of globalId * paramId
    | VdLabParam of blockId * int
    | VdInstr of instructionId * Mil.rhs
    | VdRetVar of transferId * int

  datatype labDef = Ld of {inFun : globalId, block : Mil.block}

  type t

  val codeBody : Config.t * Mil.symbolInfo * globalId * Mil.codeBody -> Mil.codeBody * t
  val code     : Config.t * Mil.symbolInfo * globalId * Mil.code -> Mil.code * t
  val program  : Config.t * Mil.t -> Mil.t * t

  (* These versions assume that instructions are already labelled. *)
  val codeBody' : Config.t * Mil.symbolInfo * globalId * Mil.codeBody -> t
  val code'     : Config.t * Mil.symbolInfo * globalId * Mil.code -> t
  val program'  : Config.t * Mil.t -> t

  val getVariable    : t * Mil.variable  -> varDef
  val getLabel       : t * Mil.label     -> labDef
  val getLabelFun    : t * Mil.label     -> globalId
  val getInstruction : t * instructionId -> Mil.instruction
  val getTransfer    : t * transferId    -> Mil.transfer
  val getBlock       : t * blockId       -> Mil.block
  val getGlobal      : t * globalId      -> Mil.global
  val getCode        : t * globalId      -> Mil.code

end;

structure FMil :> FMIL =
struct

  val passname = "FMil"

  fun fail (f, m) = Fail.fail ("FMil", f, m)
  fun assert (f, m, b) = if b then fail (f, m) else ()

  structure I = Identifier
  structure VD = I.VariableDict
  structure LD = I.LabelDict
  structure M = Mil
  structure MU = MilUtils
  structure ID = IntDict
  structure MNI = MilNumberInstructions

  type blockId = Mil.label
  type instructionId = int
  type transferId = blockId
  type globalId = Mil.variable

  datatype paramId = PiArg of int | PiCls | PiThunk | PiFreeVar of int

  datatype varDef =
      VdGlobal of Mil.global
    | VdFunParam of globalId * paramId
    | VdLabParam of blockId * int
    | VdInstr of instructionId * Mil.rhs
    | VdRetVar of transferId * int

  datatype labDef = Ld of {inFun : globalId, block : Mil.block}

  datatype state = S of {vars : varDef VD.t ref, 
                         instrs : Mil.instruction ID.t ref,
                         labs : labDef LD.t ref}

  fun stateMk () = S {vars = ref VD.empty, instrs = ref ID.empty, labs = ref LD.empty}

  fun addInstr (S {instrs, ...}, num, i) = instrs := ID.insert (!instrs, num, i)
  fun addVarDef (S {vars, ...}, v, d) = vars := VD.insert (!vars, v, d)
  fun addVarDefs (state, vv, d) = Vector.foreach (vv, fn v => addVarDef (state, v, d))
  fun addLabDef (S {labs, ...}, l, d) = labs := LD.insert (!labs, l, d)


  datatype env = E of {config : Config.t, si : M.symbolInfo}

  fun envMk (c, si) = E {config = c, si = si}

  fun analyseInstruction (s, e, f, i) =
      let
        val M.I {dests, n, rhs} = i
        val () = addVarDefs (s, dests, VdInstr (n, rhs))
        val () = addInstr (s, n, i)
      in ()
      end

  fun analyseReturn (s, e, f, l, r) =
      case r
       of M.RNormal {rets, ...} =>
          let
            fun doRetVar (i, v) = addVarDef (s, v, VdRetVar (l, i))
            val () = Vector.foreachi (rets, doRetVar)
          in ()
          end
        | M.RTail => ()

  fun analyseTransfer (s, e, f, l, t) =
      case t
       of M.TGoto t                      => ()
        | M.TCase s                      => ()
        | M.TInterProc {callee, ret, fx} => analyseReturn (s, e, f, l, ret)
        | M.TReturn os                   => ()
        | M.TCut {cont, args, cuts}      => ()
        | M.TPSumCase s                  => ()

  fun analyseBlock (s, e, f, l, b) =
      let
        val M.B {parameters, instructions, transfer} = b
        val () = addLabDef (s, l, Ld {inFun = f, block = b})
        fun doParam (i, v) = addVarDef (s, v, VdLabParam (l, i))
        val () = Vector.foreachi (parameters, doParam)
        val () = Vector.foreach (instructions, fn i => analyseInstruction (s, e, f, i))
        val () = analyseTransfer (s, e, f, l, transfer)
      in ()
      end

  fun analyseCodeBody (s, e, (f, M.CB {blocks, ...})) =
      LD.foreach (blocks, fn (l, b) => analyseBlock (s, e, f, l, b))

  fun analyseCode (s, e, (f, code)) =
      let
        (* We add the global in even if this was done by analyseGlobal so that code includes it *)
        val () = addVarDef (s, f, VdGlobal (M.GCode code))
        val M.F {cc, args, body, ...} = code
        fun doFv (i, v) = addVarDef (s, v, VdFunParam (f, PiFreeVar i))
        val () =
            case cc
             of M.CcCode => ()
              | M.CcClosure {cls, fvs} =>
                let
                  val () = addVarDef (s, cls, VdFunParam (f, PiCls))
                  val () = Vector.foreachi (fvs, doFv)
                in ()
                end
              | M.CcThunk {thunk, fvs} =>
                let
                  val () = addVarDef (s, thunk, VdFunParam (f, PiThunk))
                  val () = Vector.foreachi (fvs, doFv)
                in ()
                end
        fun doArg (i, v) = addVarDef (s, v, VdFunParam (f, PiArg i))
        val () = Vector.foreachi (args, doArg)
        val () = analyseCodeBody (s, e, (f, body))
      in ()
      end

  fun analyseGlobal (s, e, v, g) =
      let
        val () = addVarDef (s, v, VdGlobal g)
      in
        case g
         of M.GCode f                  => analyseCode (s, e, (v, f))
          | M.GErrorVal t              => ()
          | M.GIdx nis                 => ()
          | M.GTuple {vtDesc, inits}   => ()
          | M.GRat r                   => ()
          | M.GInteger i               => ()
          | M.GThunkValue {typ, ofVal} => ()
          | M.GSimple s                => ()
          | M.GPFunction vo            => ()
          | M.GPSum {tag, typ, ofVal}  => ()
          | M.GPSet s                  => ()
      end

  fun analyseGlobals (s, e, globals) =
      VD.foreach (globals, fn (x, g) => analyseGlobal (s, e, x, g))

  datatype t = P of {vars : varDef VD.t, instrs : Mil.instruction ID.t, labs : labDef LD.t}

  fun mkFMil (s, e) =
      let
        val S {vars, labs, instrs} = s
        val p = P {vars = !vars, instrs = !instrs, labs = !labs}
      in p
      end

  fun mk (c, si, f, x) =
      let
        val state = stateMk ()
        val env = envMk (c, si)
        val () = f (state, env, x)
        val p = mkFMil (state, env)
      in p
      end

  fun codeBody' (c, si, f, cb) = mk (c, si, analyseCodeBody, (f, cb))

  fun code' (c, si, f, code) = mk (c, si, analyseCode, (f, code))

  fun program' (c, M.P {symbolTable, globals, entry}) = 
      mk (c, I.SymbolInfo.SiTable symbolTable, analyseGlobals, globals)

  fun codeBody (c, si, f, cb) =
      let
        val (cb, _) = MNI.codeBody (c, cb)
      in (cb, codeBody' (c, si, f, cb))
      end

  fun code (c, si, f, cd) =
      let
        val (cd, _) = MNI.code (c, cd)
      in (cd, code' (c, si, f, cd))
      end

  fun program (c, p) = 
      let
        val (p, _) = MNI.program (c, p)
      in (p, program' (c, p))
      end

  fun getVariable (P {vars, ...}, v) =
      case VD.lookup (vars, v)
       of NONE   => fail ("getVariable", "variable not in FMil")
        | SOME d => d

  fun getLabel (P {labs, ...}, l) =
      case LD.lookup (labs, l)
       of NONE   => fail ("getLabel", "label not in FMil")
        | SOME d => d

  fun getInstruction (P {instrs, ...}, i) = 
      case ID.lookup (instrs, i)
       of NONE   => fail ("getInstruction", "instruction not in FMil")
        | SOME i => i

  fun getLabelFun (p, l) = case getLabel (p, l) of Ld {inFun, ...} => inFun

  fun getBlock (p, l) = case getLabel (p, l) of Ld {block, ...} => block

  fun getTransfer (p, l) = MU.Block.transfer (getBlock (p, l))

  fun getGlobal (p, v) =
      case getVariable (p, v)
       of VdGlobal g => g
        | _          => fail ("getGlobal", "variable is not global")

  fun getCode (p, v) =
      case getGlobal (p, v)
       of M.GCode code => code
        | _            => fail ("getCode", "variable is not code")

end;
