(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* Provides utilities for extracting the set of bound variables from
   some mil AST subtree *)

signature MIL_BOUND_VARS =
sig
  type t
  val rhs : t * Mil.rhs -> Identifier.VariableSet.t
  val instruction : t * Mil.instruction -> Identifier.VariableSet.t
  val call : t * Mil.call -> Identifier.VariableSet.t
  val eval : t * Mil.eval -> Identifier.VariableSet.t
  val transfer : t * Mil.transfer -> Identifier.VariableSet.t
  val block : t * Mil.label * Mil.block -> Identifier.VariableSet.t
  val blocks : t * Mil.block Mil.LD.t -> Identifier.VariableSet.t
  val global : t * Mil.variable * Mil.global -> Identifier.VariableSet.t
  val program : t * Mil.t -> Identifier.VariableSet.t
end;

structure MilBoundVars :> MIL_BOUND_VARS where type t = Config.t =
struct
  type t = Config.t

  structure VS = Identifier.VariableSet

  datatype state = S of VS.t ref

  fun mkState () = S (ref VS.empty)
  fun finish (S state) = !state

  fun varBind (s as S bound, e, v) =
      let
        val () = bound := VS.insert (!bound, v)
      in e
      end

  structure MA = MilAnalyseF (
                 struct 
                   type state = state
                   type env = Config.t
                   fun config c = c
                   val indent = 2
                   val variableBind = SOME varBind
                   val labelBind = NONE
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
  val transfer = mk1 MA.analyseTransfer
  val block = mk2 MA.analyseBlock
  val blocks = mk1 MA.analyseBlocks
  val global = mk2 MA.analyseGlobal
  val program = mk1 MA.analyseProgram

end;
