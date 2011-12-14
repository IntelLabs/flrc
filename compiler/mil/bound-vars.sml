(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* Provides utilities for extracting the set of bound variables from
   some mil AST subtree *)

signature MIL_BOUND =
sig
  type t 
  val rhs : Config.t * Mil.rhs -> t
  val instruction : Config.t * Mil.instruction -> t
  val call : Config.t * Mil.call -> t
  val eval : Config.t * Mil.eval -> t
  val transfer : Config.t * Mil.transfer -> t
  val block : Config.t * Mil.label * Mil.block -> t
  val blocks : Config.t * Mil.block Mil.LD.t -> t
  val codeBody : Config.t * Mil.codeBody -> t
  val code : Config.t * Mil.code -> t
  val global : Config.t * Mil.variable * Mil.global -> t
  val program : Config.t * Mil.t -> t
end;

structure MilBoundVarsLabels :> MIL_BOUND where type t = Mil.VS.t * Mil.LS.t =
struct
  structure VS = Identifier.VariableSet
  structure LS = Identifier.LabelSet

  type t = VS.t * LS.t

  datatype state = S of (VS.t ref * LS.t ref)

  fun mkState () = S (ref VS.empty, ref LS.empty)
  fun finish (S (vr, lr)) = (!vr, !lr)

  fun varBind (s as S (vs, ls), e, v) =
      let
        val () = vs := VS.insert (!vs, v)
      in e
      end

  fun labelBind (s as S (vs, ls), e, l) =
      let
        val () = ls := LS.insert (!ls, l)
      in e
      end

  structure MA = MilAnalyseF (
                 struct 
                   type state = state
                   type env = Config.t
                   fun config c = c
                   val indent = 2
                   val externBind = SOME varBind
                   val variableBind = SOME varBind
                   val labelBind = SOME labelBind
                   val variableUse = NONE
                   val analyseJump = NONE
                   val analyseCut = NONE
                   val analyseConstant = NONE
                   val analyseInstruction = NONE
                   val analyseTransfer = NONE
                   val analyseBlock = NONE
                   val analyseGlobal = NONE
                 end)

  fun mk1 af =
      fn (config, x) =>
         let
           val state = mkState ()
           val _ = af (state, config, x)
         in 
           finish state
         end

  fun mk2 af =
      fn (config, x, y) =>
         let
           val state = mkState ()
           val _ = af (state, config, x, y)
         in 
           finish state
         end

  val rhs = mk1 MA.analyseRhs
  val instruction = mk1 MA.analyseInstruction
  val call = mk1 MA.analyseCall
  val eval = mk1 MA.analyseEval
  val transfer = mk1 (fn (state, config, y) => MA.analyseTransfer (state, config, NONE, y))
  val block = mk2 MA.analyseBlock
  val blocks = mk1 MA.analyseBlocks
  val codeBody = mk1 MA.analyseCodeBody
  val code = mk1 MA.analyseCode
  val global = mk2 MA.analyseGlobal
  val program = mk1 MA.analyseProgram

end;

structure MilBoundVars :> MIL_BOUND where type t = Mil.VS.t =
struct
  type t = Mil.VS.t
  structure MBVL = MilBoundVarsLabels

  val rhs = #1 o MBVL.rhs
  val instruction = #1 o MBVL.instruction
  val call = #1 o MBVL.call
  val eval = #1 o MBVL.eval
  val transfer = #1 o MBVL.transfer
  val block = #1 o MBVL.block
  val blocks = #1 o MBVL.blocks
  val codeBody = #1 o MBVL.codeBody
  val code = #1 o MBVL.code
  val global = #1 o MBVL.global
  val program = #1 o MBVL.program

end;

structure MilBoundLabels :> MIL_BOUND where type t = Mil.LS.t =
struct
  type t = Mil.LS.t
  structure MBVL = MilBoundVarsLabels

  val rhs = #2 o MBVL.rhs
  val instruction = #2 o MBVL.instruction
  val call = #2 o MBVL.call
  val eval = #2 o MBVL.eval
  val transfer = #2 o MBVL.transfer
  val block = #2 o MBVL.block
  val blocks = #2 o MBVL.blocks
  val codeBody = #2 o MBVL.codeBody
  val code = #2 o MBVL.code
  val global = #2 o MBVL.global
  val program = #2 o MBVL.program

end;
