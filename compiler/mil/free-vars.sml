(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

signature MIL_FREE_VARS =
sig
  type t
  val instruction : t * Mil.instruction -> Identifier.VariableSet.t
  val transfer : t * Mil.transfer -> Identifier.VariableSet.t
  val block : t * Mil.label * Mil.block -> Identifier.VariableSet.t
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

  val instruction = mk1 MA.analyseInstruction
  val transfer = mk1 MA.analyseTransfer
  val block = mk2 MA.analyseBlock
  val global = mk2 MA.analyseGlobal
  val program = mk1 MA.analyseProgram

end;
