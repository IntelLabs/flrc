(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_FREE_VARS =
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

structure MilFreeVars :> MIL_FREE_VARS where type t = Config.t =
struct
  type t = Config.t

  structure VS = Identifier.VariableSet

  datatype state = S of {frees : VS.t ref, bound : VS.t ref}

  fun mkState () = S {frees = ref VS.empty, bound = ref VS.empty}
  fun finish (S {frees, bound, ...}) = VS.difference (!frees, !bound)

  fun varUse (s as S {frees, ...}, e, v) =
      let
        val () = frees := VS.insert (!frees, v)
      in ()
      end

  fun varBind (s as S {bound, ...}, e, v) =
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
                   val externBind = SOME varBind
                   val variableBind = SOME varBind
                   val labelBind = NONE
                   val variableUse = SOME varUse
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
         in finish state
         end

  fun mk2 af =
      fn (config, x, y) =>
         let
           val state = mkState ()
           val _ = af (state, config, x, y)
         in finish state
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
