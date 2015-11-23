(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_UTILS2 =
sig
  val irHelpers : Mil.t Pass.irHelpers
end;

structure MilUtils2 :> MIL_UTILS2 =
struct

  val statOptions = MilStats.O {id = NONE}

  val irHelpers = {printer = fn (p, c) => MilLayout.layout (c, p),
                   stater = MilStats.layout statOptions}

end;

