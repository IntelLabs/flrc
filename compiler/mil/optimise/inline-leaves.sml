(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* Description: Inline small leaf functions. *)

(* Small leaves policy:
 *
 * do {
 *   funList = select small leaf functions
 *   callList = calls to funList
 *   inline (callList)
 * } while (callList)
 *)

signature MIL_INLINE_LEAVES = 
sig
  val pass : (BothMil.t, BothMil.t) Pass.t
end

structure MilInlineLeaves :> MIL_INLINE_LEAVES =
struct

  val passname = "MilInlineLeaves"

  (* Aliases *)
  structure PD   = PassData
  structure M    = Mil
  structure MU   = MilUtils
  structure L    = Layout
  structure ID   = Identifier
  structure IM   = ID.Manager
  structure WS   = IMil.WorkSet
  structure ACGP = AnnotatedCGPrinter

  (* Module controls. *)
  structure Control = 
  struct 
  
    (* Relative Budge Size Control *)
    val defaultSmallLimit = 50

    fun default (_) = defaultSmallLimit

    fun parser (s : string) =
        case Int.fromString (s)
         of NONE => NONE
          | SOME n => 
            if (n >= 0) then SOME n else NONE

    fun description () =
        L.str (passname ^ " the size limit to consider a function small." ^
               "Must be a non negative number. Default = " ^ 
               Int.toString (defaultSmallLimit) ^ ".")
        
    val name = passname ^ ":small-limit"
               
    val (smallLimit, getSmallLimit) =
        Config.Control.mk (name, description, parser, default) 
        
    val all = [smallLimit]

  end
  
  structure Debug = 
  struct

    (* Number of times the module was called. *)
    val nExec = ref 0

    val incExec    : unit -> unit   = fn () => nExec := !nExec + 1
    val getExec    : unit -> int    = fn () => !nExec
    val getExecStr : unit -> string = fn () => Int.toString (getExec ())

    (* Number of times the inliner iterated (optimizer was called) . *)
    val nOptExec = ref 0

    val resetOptExec  : unit -> unit   = fn () => nOptExec := 0
    val incOptExec    : unit -> unit   = fn () => nOptExec := !nOptExec + 1
    val getOptExec    : unit -> int    = fn () => !nOptExec
    val getOptExecStr : unit -> string = fn () => Int.toString (getOptExec ())
                        
    (* Debug switches *)
    val (prnCallGraphEndD, prnCallGraphEnd) =
        Config.Debug.mk (passname ^ ":print-call-graph-at-end", 
                         "print call graph at end of inline leaves")
        
    val (prnCallGraphOptD, prnCallGraphOpt) =
        Config.Debug.mk (passname ^ ":print-call-graph-after-opt", 
                         "print call graph after each iter")

    val (debugPassD, debugPass) =
        Config.Debug.mk (passname, "debug the Mil inline leaves pass")

    val all = [debugPassD, prnCallGraphEndD, prnCallGraphOptD]

    (* Print a message if debug mode is on (String version). *)
    val print : PD.t * string -> unit = 
     fn (d, m) =>
        if Config.debug andalso debugPass (PD.getConfig d) then
          print (passname ^ ": " ^ m)
        else ()
             
    (* Print a message if debug mode is on (Layout version). *)
    val printLayout : PD.t * Layout.t -> unit = 
     fn (d, l) =>
        if Config.debug andalso debugPass (PD.getConfig d) then
          LayoutUtils.printLayout (L.seq [L.str (passname ^ ": "), l])
        else ()

    val printStartMsg : PD.t -> unit = 
     fn (d) => print (d, " - Starting the small/leaf functions inlining " ^
                         " (Iteration # " ^ getExecStr () ^ ")...\n")
                      
    val printEndMsg : PD.t -> unit = 
     fn (d) => print (d, " - Finishing the small/leaf functions inlining...\n")
                         
    (* Print the call graph into a file. *)
    val printCallGraph : IMil.t * PD.t * string * (Config.t -> bool) -> unit = 
     fn (imil, d, label, debugOn) =>
        if Config.debug andalso debugOn (PD.getConfig d) then 
          ACGP.printCallGraph (d, imil, label)
        else 
          ()

    (* Print the call graph at the end of the module. *)
    val printCallGraphEnd : IMil.t * PD.t -> unit =
     fn (imil, d) =>
        let
          val label = "Call graph at the end of inline leaves" ^
                      " - Exec: " ^ getExecStr () ^ 
                      ". After all optimizing iterations." 
        in
          printCallGraph (imil, d, label, prnCallGraphEnd)
        end

    (* Print the call graph after each iteration. *)
    val printCallGraphOpt : IMil.t * PD.t -> unit =
     fn (imil, d) =>
        let
          val label = "Call graph during inline leaves" ^
                      " - Exec: " ^ getExecStr () ^ 
                      ". After optimizing iteration " ^ 
                      getOptExecStr ()
        in
          printCallGraph (imil, d, label, prnCallGraphOpt)
        end

  end
           
  (* Policy functions and types. *)
  type policyInfo = unit
  fun analyze (imil) = Debug.resetOptExec ()
  type callId = IMil.iInstr
  fun callIdToCall (info : policyInfo, imil : IMil.t, call : callId) = call
  fun associateCallToCallId (info      : policyInfo, 
                             imil      : IMil.t, 
                             cp        : IMil.iInstr,
                             origBlock : IMil.iBlock, 
                             newBlock  : IMil.iBlock) = ()
  fun rewriteOperation (c: callId) = InlineFunctionCopy

  val (noOptimizerF, noOptimizer) =
      Config.Feature.mk (passname ^ ":no-optimizer", 
                         "do not call the simplifier after inlining")

  (* EB: Calling the simplifier at every iteration increases the
   * ammount of functions inlined at in the first inliner pass. *)
  fun optimizer (info, d, imil, il) =
      let
        val () = Debug.incOptExec ()
        fun simplifyModifiedCode () =
            let
              val w  = WS.new ()
              val () = List.foreach (il, fn i => WS.addInstr (w, i))
            in
              MilSimplify.simplify (d, imil, w)              
            end
        val () = if ( noOptimizer (PD.getConfig d) ) then ()
                 else simplifyModifiedCode ()
      in
        Debug.printCallGraphOpt (imil, d)
      end

  (* Collect call sites that call function (fname, ifunc). *)
  fun collectCallSitesForFunc (d     : PD.t, 
                               fname : Mil.variable, 
                               iFunc : IMil.iFunc,
                               imil  : IMil.t) : callId list =
      let
        fun getCandidateCall (u : IMil.use) = 
            Try.try
              (fn () => 
                  let
                    val i = Try.<- (IMil.Use.toIInstr u)
                    val t = Try.<- (IMil.IInstr.toTransfer i)
                    fun warn f = 
                        (* XXX EB: Why is it necessary to check fname? *)
                        if f = fname then
                          ()
                        else 
                          let 
                            val () = Debug.print (d, "Fun code used in call "^
                                                     "but not callee!\n")
                          in 
                            Try.fail ()
                          end
                    val {callee, ...} = Try.<- (MU.Transfer.Dec.tInterProc t)
                    val {call, ...} = Try.<- (MU.InterProc.Dec.ipCall callee)
                    val () =                     
                        case call
                         of M.CCode {ptr, ...} => warn ptr
                          | M.CDirectClosure {code, ...} => warn code
                          | _ => Try.fail ()

                    val () = PD.click (d, "LeafCallSitesInlined")
                  in i
                  end)
        val uses  = IMil.IFunc.getUses (imil, iFunc)
        val calls = Vector.keepAllMap (uses, getCandidateCall)
        val () = if not (Vector.isEmpty (calls)) then
                   let
                     (* EB: Debug message. *)
                     val l = [L.str "Function \"", 
                              IMil.Layout.var (imil, fname),
                              L.str "\" selected for inlining in ",
                              Int.layout (Vector.length (calls)),
                              L.str " call sites."]
                     val () = Debug.printLayout (d, L.seq l)
                   in 
                     PD.click (d, "LeafFuncInlined")
                   end
                 else
                   ()
      in
        Vector.toList (calls)
      end

  fun isSmallFunction (d : PD.t, imil: IMil.t, iFunc : IMil.iFunc) : bool =
      (IMil.IFunc.getSize (imil, iFunc) < Control.getSmallLimit (PD.getConfig d))

  (* Check  if the mil transfer  is a call. Do not consider EvalThunk
   * and BulkSpawn calls. *)
  fun isMilCall (tr: Mil.transfer) : bool = 
      case tr
       of Mil.TInterProc {callee = M.IpCall _, ...} => true
        | _               => false

  fun isCallInstr (imil : IMil.t, i : IMil.iInstr) : bool = 
      case IMil.IInstr.toTransfer i
       of NONE    => false
        | SOME tr => isMilCall tr

  fun isLeafFunction (imil : IMil.t, iFunc : IMil.iFunc) : bool =
      let
        val transfers = IMil.Enumerate.IFunc.transfers (imil, iFunc)
      in
        (not (List.exists (transfers, fn (i) => isCallInstr (imil, i))))
      end

  (* Collect only call to small leaf functions. *)
  fun collectCallSites (d : PD.t, (fname : Mil.variable, iFunc : IMil.iFunc),
                        imil : IMil.t) : callId list =
      if (isSmallFunction (d, imil, iFunc) andalso 
          isLeafFunction  (imil, iFunc) andalso
          not (IMil.IFunc.getRecursive (imil, iFunc)))
      then
        collectCallSitesForFunc (d, fname, iFunc, imil)
      else 
        nil

  (* Select the call sites to inline.
   * For each function, use selectCallSites to collect the list of
   * call sites. *)
  fun policy (info : policyInfo, d : PD.t, imil : IMil.t) =
      let
        fun collect (cfg) = collectCallSites (d, cfg, imil)
        val calls = List.concat (List.map (IMil.IFunc.getIFuncs (imil), collect))
        val () = Debug.print (d, "Policy selected " ^
                                 Int.toString (List.length (calls)) ^
                                 " call sites to inline.")
      in
        calls
      end
      
  structure Inliner = MilInlineRewriterF (
                        type policyInfo           = policyInfo
                        val analyze               = analyze
                        type callId               = callId
                        val callIdToCall          = callIdToCall
                        val associateCallToCallId = associateCallToCallId
                        val rewriteOperation      = rewriteOperation
                        val policy                = policy
                        val optimizer             = SOME optimizer)
                                  
  fun program (imil : IMil.t, d : PD.t) : unit = 
      let
        val () = Debug.incExec ()
        val () = Debug.printStartMsg (d)
        val () = Inliner.program (d, imil, NONE)
        val () = PD.report (d, passname)
        val () = Debug.printCallGraphEnd (imil, d)
        val () = Debug.printEndMsg (d)
      in ()
      end

  val stats = [("LeafFuncInlined",      "Small leaf functions inlined"),
               ("LeafCallSitesInlined", "Leaf call sites inlined"     )]

  val description = {name        = passname,
                     description = "Leaf functions inliner",
                     inIr        = BothMil.irHelpers,
                     outIr       = BothMil.irHelpers,
                     mustBeAfter = [],
                     stats       = stats}

  val associates =
      {controls  = Control.all,
       debugs    = Debug.all,
       features  = [noOptimizerF],
       subPasses = []}

  val pass =
      Pass.mkOptPass (description, associates, BothMil.mkIMilPass program)

end (* end of structure MilInlineLeaves *)
