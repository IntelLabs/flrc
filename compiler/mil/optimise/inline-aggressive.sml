(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, May 2008 *)
(* Description: Inline functions aggressively. *)

(* 
 * Inline Aggressive Policy:
 * 
 * Given a size budget
 *
 * for each function f in prog
 *   cost[f] = size (f) * # of uses (f)
 * subset = select (cost[f], budget)
 * callSites = nil
 * for each f in subset
 *   callSites = uses (f) @ callSites
 *
 * select (set, budget)
 *   sorted_set = sort set by cost
 *   selected_set = nil
 *   acc = 0;
 *   while (acc < budget andalso sorted_set has function)
 *     f = pop sorted_set
 *     selected_set = f :: selected_set
 *   return selected_set
 *)

signature MIL_INLINE_AGGRESSIVE = 
sig
  val pass : (BothMil.t, BothMil.t) Pass.t
end

structure MilInlineAggressive :> MIL_INLINE_AGGRESSIVE =
struct

  val passname = "MilInlineAggressive"

  val stats =
      [("AggressiveFuncInlined", 
        "functions inlined (Aggressive inliner)"),
       ("AggressiveCallSitesInlined", 
        "call sites inlined (Aggressive inliner)")]

  (* Budget ratio is used to calculate the budgetSize limit. *)
  val budgetRatio = 0.20 (* 20% *)
  (* Extra size limit. *)
  val budgetSize = ref 0

  (* Aliases *)
  structure PD = PassData
(*  structure MOU = MilOptUtils*)
  structure M   = Mil
  structure L   = Layout
  structure ID  = Identifier
  structure IM  = ID.Manager
  structure ACGP = AnnotatedCGPrinter

  val (prnCallGraphEndD, prnCallGraphEnd) =
      Config.Debug.mk (passname ^ ":print-call-graph-at-end", 
                       "print call graph at end of inline aggressive")

  val (prnCallGraphOptD, prnCallGraphOpt) =
      Config.Debug.mk (passname ^ ":print-call-graph-after-opt", 
                       "print call graph after each iter")

  (* Number of times the module was called. *)
  val nExec = ref 0

  (* Number of times the inliner iterated (optimizer was called) . *)
  val nOptExec = ref 0

  val (debugPassD, debugPass) =
      Config.Debug.mk (passname, "debug the Mil inline aggressive pass")

  (* Print a message if debug mode is on (String version). *)
  fun dbgString (d : PD.t, m : string) =
      if Config.debug andalso debugPass (PD.getConfig d) then
        print (passname ^ ": " ^ m )
      else ()

  (* Print a message if debug mode is on (Layout version). *)
  fun dbgLayout (d : PD.t, l : L.t) =
      if Config.debug andalso debugPass (PD.getConfig d) then
        LayoutUtils.printLayout (L.seq [L.str (passname ^ ": "), l])
      else ()
           
  (* Policy functions and types. *)
  type policyInfo = unit
  fun analyze (imil) = nOptExec := 0
  type callId = IMil.iInstr
  fun callIdToCall (info: policyInfo, imil: IMil.t, call: callId) = call
  fun associateCallToCallId (info: policyInfo, 
                             imil: IMil.t, 
                             cp: IMil.iInstr,
                             origBlock: IMil.iBlock, 
                             newBlock: IMil.iBlock) = ()
  fun rewriteOperation (c: callId) = InlineFunctionCopy

  val (noOptimizerF, noOptimizer) =
      Config.Feature.mk (passname^":noOptimizer", 
                         "Do not call the optimizer " ^
                         "after inlining the call site list")

  fun optimizer (info, d, imil, ils) =
      let
        val () = nOptExec := !nOptExec + 1
        val () = if ( noOptimizer (PD.getConfig d) ) then ()
                 else MilSimplify.program (d, imil)
      in
        (* Print the call graph into a file. *)
        if Config.debug andalso 
           prnCallGraphOpt (PD.getConfig d) then 
          let
            val nOptExec' = Int.toString (!nOptExec)
            val nExec' = Int.toString (!nExec)
            val graphLabel = "Call graph during inline aggressive "^
                             "- Exec: "^nExec'^
                             ". After optimizing iteration "^nOptExec'
          in
            ACGP.printCallGraph (d, imil, graphLabel)
          end
        else ()
      end

  (* Collect call sites that call function (fname, cfg). *)
  fun getInlineableCalls (d: PD.t, 
                          fname: Mil.variable, 
                          cfg: IMil.iFunc,
                          imil: IMil.t) : callId list * bool =
      let
        fun getCandidateCall (u: IMil.use) = 
            Try.try
              (fn () => 
                  let
                    val i = Try.<- (IMil.Use.toIInstr u)
(*                    val t = Try.<- (IMil.Use.toTransfer (imil, i))*)
                    val t = Try.<- (IMil.Use.toTransfer u)
                    fun warn f = 
                        (* XXX EB: Why is it necessary to check fname? *)
                        if f = fname then ()
                        else 
                          let 
                            val () = dbgString (d, "Fun code used in call "^
                                                   "but not callee!\n")
                          in Try.fail ()
                          end
                    (* XXX EB: What does doConv mean? *)
                    fun doConv conv = 
                        (case conv
                          of M.CCode f => warn f
                           | M.CDirectClosure {cls, code} => warn cls
                           | _ => Try.fail ())
                    (* Check transfer. Only TCall and TTailCall are valid. *)
                    (* XXX EB: Is there any other transfer that may
                     * use a function? *)
                    val () = 
                        case t
(*                         of M.TCall (conv, _, _, _, _) => doConv conv
                          | M.TTailCall (conv, _, _) => doConv conv*)
                         of M.TInterProc {callee, ret, fx} => 
                            (case callee 
                              of M.IpCall {call, args} => doConv call
                               | M.IpEval {typ, eval} => Try.fail())
                          | _ => Try.fail ()
                  in i
                  end)
        val uses = IMil.IFunc.getUses (imil, cfg)
        val calls = Vector.keepAllMap (uses, getCandidateCall)
        val allCallsAreInlineable : bool = (Vector.length (calls) = Vector.length (uses))
        (* Keep original function after inlining? *)
        val keepOriginal : bool = IMil.IFunc.getEscapes (imil, cfg) orelse 
                                  not allCallsAreInlineable
        (* Debug message. XXX EB: Delete this*)
        val () = if Vector.isEmpty (calls) then
                   ()
                 else 
                   (dbgLayout (d, L.seq [L.str "Function \"",
                                         ID.layoutVariable' (fname),
(*                                                            IMil.getST (imil)),*)
                                         L.str "\" selected for inlining in ",
                                         Int.layout (Vector.length (calls)),
                                         L.str " call sites."]))
      in
        (Vector.toList (calls), keepOriginal)
      end

  (* Select the best candidate to inline based on the size cost. *)
  fun selectByBudget (funs : (Mil.variable * IMil.iFunc * int) list, 
                      budget: int) =
      let
        val func = ref NONE
        val size = ref 0
        fun selectCheapest (f, c, sz) = 
            if Option.isNone (!func) orelse sz < !size then
              (func := SOME (f, c); size := sz)
            else
              ()
        val () = List.foreach (funs, selectCheapest)
      in
        (!func, !size)
      end

  (* If the function is an inlineable function, returns it and 
   * its cost in size to inline. Otherwise, return NONE. *)
  fun selectInlineable (d: PD.t, 
                        f: Mil.variable, 
                        cfg : IMil.iFunc, 
                        imil: IMil.t) : 
      (Mil.variable * IMil.iFunc * int) option = 
      Try.try
        (fn () =>
            let
              val () = Try.require (not (IMil.IFunc.getRecursive (imil, cfg)))
              val (calls, keepOriginal) = getInlineableCalls (d, f, cfg, imil)
              val nUses = List.length (calls)
              val () = Try.require (nUses > 1)
              val extraCopies = if keepOriginal then
                                  nUses
                                else
                                  nUses - 1
              val sizeCost = extraCopies * IMil.IFunc.getSize (imil, cfg)
            in
              (f, cfg, sizeCost)
            end)

  (* Select the call sites to inline. 
   * For each function, call callectCallSites and append the list of
   * call sites. *)
  fun policy (info: policyInfo, d: PD.t, imil: IMil.t) =
      let
        fun doOne (f, cfg) = selectInlineable (d, f, cfg, imil)
        val candidateFuns = List.keepAllMap (IMil.IFunc.getIFuncs (imil), doOne)
        val (func, codeSize) = selectByBudget (candidateFuns, !budgetSize)
        val calls = case func
                     of SOME (f, cfg) => 
                        #1 (getInlineableCalls (d, f, cfg, imil))
                      | NONE => nil
        val () = budgetSize := !budgetSize - codeSize
        val () = dbgLayout (d, L.seq [L.str "Policy selected ",
                                      Int.layout (List.length (calls)),
                                      L.str " call sites to inline."])
        (* Update statistics. *)
        val () = PD.clickN (d, "AggressiveCallSitesInlined", 
                            List.length (calls))
      in
        calls
      end
      
  structure Inliner = MilInlineRewriterF (
                        type policyInfo = policyInfo
                        val analyze = analyze
                        type callId = callId
                        val callIdToCall = callIdToCall
                        val associateCallToCallId = associateCallToCallId
                        val rewriteOperation = rewriteOperation
                        val policy = policy
                        val optimizer = SOME optimizer)
                               
  fun getProgSize (imil) = 
      let
        val cfgs = IMil.IFunc.getIFuncs (imil)
      in
        List.fold (cfgs, 0, 
                fn ((f, cfg), sz) => sz + IMil.IFunc.getSize (imil, cfg))
      end

  fun program (imil : IMil.t, d : PD.t) : unit = 
      let
        val () = budgetSize := round (real (getProgSize (imil)) * budgetRatio)
        (* Update the number of times the module was executed. *)
        val () = nExec := !nExec + 1
        val nExec' = Int.toString (!nExec)
        val () = dbgString (d, " - Starting the aggressive inliner"^
                               " (Iteration # "^nExec'^")...\n")
                 
        val () = Inliner.program (d, imil)
        val () = PD.report (d, passname)
        val () = dbgString (d, " - Finishing the aggressive inliner...\n")
      in
        (* Print the call graph into a file. *)
        if Config.debug andalso 
           prnCallGraphEnd (PD.getConfig d) then 
          let
            val graphLabel = "Call graph at the end of inline aggressive"^
                             " - Exec: "^nExec'^
                             ". After all optimizing iterations."
          in
            ACGP.printCallGraph (d, imil, graphLabel)
          end
        else ()
      end

  val description = {name        = passname,
                     description = "Aggressive Inliner",
                     inIr        = BothMil.irHelpers,
                     outIr       = BothMil.irHelpers,
                     mustBeAfter = [],
                     stats       = stats}

  val associates =
      {controls  = [],
       debugs    = [debugPassD, prnCallGraphEndD, prnCallGraphOptD],
       features  = [noOptimizerF],
       subPasses = []}

  val pass =
      Pass.mkOptPass (description, associates, BothMil.mkIMilPass program)

end (* end of structure MilInlineAggressive *)
