(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_INLINE_SMALL =
sig
  val pass : (BothMil.t, BothMil.t) Pass.t
end
                             
structure MilInlineSmall :> MIL_INLINE_SMALL = 
struct

  val passname = "MilInlineSmall"

  val stats = [("InlineSmall", "Small functions inlined")]

  structure M = Mil
  structure PD = PassData
  structure Use = IMil.Use
  structure IFunc = IMil.IFunc
  structure MU = MilUtils
  structure WS = IMil.WorkSet
  structure MS = MilSimplify

  structure Chat = ChatF(struct type env = PD.t
                         val extract = PD.getConfig
                         val name = passname
                         val indent = 0
                         end)
                   
  val <@ = Try.<@

  val inlineSmallLimit = 5

  val rounds = 5

  val (debugPassD, debugPass) =
      Config.Debug.mk (passname, "debug the Mil inline small pass")

  val inlineOne = 
   fn (d, imil, f) =>
      let
        val fname = IFunc.getFName (imil, f)
        fun getCandidateCall u = 
            Try.try
              (fn () => 
                  let
                    val t = <@ Use.toTransfer u
                    val {callee, ...} = <@ MU.Transfer.Dec.tInterProc t
                    val {call, ...} = <@ MU.InterProc.Dec.ipCall callee
                    val () = 
                        (case call
                          of M.CCode {ptr, code} => Try.require (ptr = fname)
                           | M.CDirectClosure {cls, code} => Try.require (code = fname)
                           | _ => Try.fail())
                    val () = PD.click (d, "InlineSmall")
                  in <@ Use.toIInstr u
                  end)
        val uses = IFunc.getUses (imil, f)
        val calls = Vector.keepAllMap (uses, getCandidateCall)
        val calls = Vector.toList calls
      in calls
      end

  val inlineAll =
   fn (d, imil) => 
      let
        val funcs = IMil.Enumerate.T.funcs imil
        val small = List.keepAll (funcs, fn f => IFunc.getSize (imil, f) < inlineSmallLimit)
        val calls = List.concatMap (small, fn c => inlineOne (d, imil, c))
      in calls
      end

  val chooseCalls : unit * PassData.t * IMil.t -> IMil.iInstr list = 
   fn ((), pd, imil) => inlineAll (pd, imil)

  val optimize = 
   fn ((), pd, imil, il) => 
      let
        val ws = WS.new ()
        val () = List.foreach (il, fn i => WS.addInstr (ws, i))
        val () = MS.simplify (pd, imil, ws)
      in ()
      end

  structure RewriterClient =
  struct
    type policyInfo = unit
    val analyze = fn _ => ()
    type callId = IMil.iInstr
    val callIdToCall =  fn (info, imil, i) => i
    val associateCallToCallId = fn _ => ()
    val rewriteOperation = fn _ => InlineFunctionCopy
    val policy = chooseCalls
    val optimizer = SOME optimize
  end
    
  structure Inliner = MilInlineRewriterF(RewriterClient)

  fun program (imil, d) = 
      let
        val () = Inliner.program (d, imil, SOME rounds)
        val () = PD.report (d, passname)
      in ()
      end

  val description = {name        = passname,
                     description = "Inline small functions",
                     inIr        = BothMil.irHelpers,
                     outIr       = BothMil.irHelpers,
                     mustBeAfter = [],
                     stats       = stats}

  val associates = {controls  = [],
                    debugs    = [debugPassD],
                    features  = [],
                    subPasses = []}

  val pass =
      Pass.mkOptPass (description, associates, BothMil.mkIMilPass program)

end
