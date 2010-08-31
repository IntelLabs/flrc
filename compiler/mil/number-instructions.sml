(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation *)

signature MIL_NUMBER_INSTRUCTIONS = 
sig
  (* Label every instruction in the program with a unique integer id.
   * Returns the numbered mil, and an integer guaranteed to be
   * greater than the largest instruction id.
   *)
  val program : Config.t * Mil.t -> Mil.t * int
  val globals : Config.t * Mil.globals -> Mil.globals * int
  val code : Config.t * Mil.code -> Mil.code * int
  val codeBody : Config.t * Mil.codeBody -> Mil.codeBody * int
end (* MIL_NUMBER_INSTRUCTIONS *)

structure MilNumberInstructions :> MIL_NUMBER_INSTRUCTIONS =
struct
  structure MU = MilUtils
  structure MRC = MilRewriterClient 
  structure M = Mil

  datatype state = S of {next : int ref}

  datatype env = E of {config : Config.t}

  val id = fn S {next} => Utils.Ref.inc next

  val instr = 
   fn (state, env, M.I {dests, n, rhs}) => MRC.StopWith (env, M.I {dests = dests, n = id state, rhs = rhs})

  structure R = 
  MilRewriterF (struct
                  type env   = env
                  type state = state
                  val config      = fn (E {config}) => config
                  val label       = fn _ => MRC.Stop
                  val variable    = fn _ => MRC.Stop
                  val operand     = fn _ => MRC.Stop
                  val instruction = instr
                  val transfer    = fn _ => MRC.Stop
                  val block       = fn _ => MRC.Continue
                  val global      = fn _ => MRC.Continue
                  val bind        = fn (_, env, _) => (env, NONE)
                  val bindLabel   = fn (_, env, _) => (env, NONE)
                  val indent      = 2
                  val cfgEnum     = fn (_, _, t) => MilUtils.CodeBody.dfsTrees t
                end)

  val number = 
   fn f => 
   fn (config, obj) => 
      let
        val next = ref 0
        val state = S {next = next}
        val env = E {config = config}
        val obj = f (state, env, obj)
      in (obj, !next)
      end

  val code = number R.code
  val codeBody = number R.codeBody
  val globals = (fn ((env, gs), i) => (gs, i)) o (number R.globals)
  val program = number R.program

end (* MilNumberInstructions *)
