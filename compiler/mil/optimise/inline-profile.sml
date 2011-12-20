(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* Description: Inline functions based on static profile information. *)

(* Profile Driven Inliner:
 *
 * Problem
 *  Given the constraints
 *     - size budget in: absolute # of Mil instructions or 
 *                       percentage of the current program size.
 *     - call site minimum execution frequency
 *  And the objective function
 *     - F = Sum of execution frequency of selected call sites
 *  Select call sites to be inlined so that the function F is maximized
 *  without violating any of the constraints.
 *
 * Maximizing F under size budget constraints it is NP-Hard (Knapsack problem).
 * This module uses the following naive heuristic:
 *
 * while (true) {
 *   cs = pick the best call site (highest execution frequency).
 *   if no cs then break
 *   else inline cs
 * }
 *
 * analyze:
 *  - Initialize the inliner
 *    - Build the "call sites info" (only inlineable calls)
 *    - Initialize the "new blocks mapping" as empty
 *
 * policy:
 *    - Use the "new blocks mapping" to update the "call sites info".
 *      (including frequencies).
 *    - Update the budget
 *    - cs = Select the best call site.
 *      if !cs then optimize and try to select again.
 *      return cs
 *
 * optimize:
 *    - Do nothing. The "policy" function will optimize at its discretion.
 *
 * The select best call site may return NONE because of
 * inlining constraints. One of the constraints is the code growth budget.
 * In this sense,  if the Select best call site  function cannot find a
 * call site to inline, we optimize the program in the hope that it
 * will reduce the code size and some of the functions will fit into the
 * growth budget.
 *)

signature MIL_INLINE_PROFILE = 
sig
  val pass : (BothMil.t, BothMil.t) Pass.t
end

structure MilInlineProfile :> MIL_INLINE_PROFILE =
struct

  val passname = "MilInlineProfile"

  (* Aliases *)
  structure PD   = PassData
  structure M    = Mil
  structure L    = Layout
  structure LU   = LayoutUtils
  structure ML   = MilLayout
  structure ID   = Identifier
  structure VD   = ID.VariableDict
  structure LD   = Mil.LD
  structure IM   = ID.Manager
  structure WS   = IMil.WorkSet
  structure ACGP = AnnotatedCGPrinter

  (* Reports a fail message and exit the program. *)
  fun fail (f, m) = Fail.fail ("inline-profile.sml ", f, m)

  type callId = IMil.iInstr

  (* Module controls. *)
  structure Control =
  struct 

    (* Relative Budget Size Control *)
    fun default (_) = 0.2

    fun parser (s : string) =
        case Real.fromString (s)
         of NONE => NONE
          | SOME n => 
            if (n >= 0.0) then SOME n else NONE

    fun description () =
        L.str (passname ^ " relative budget size if the maximum percentage" ^
               "of code growth allowed during inlining. 0.2 (20%) is the " ^
               "default value. The number must be non negative.")
        
    val name = passname ^ ":rel-bdg-size"
               
    val (relBdgSize, getRelBdgSize) =
        Config.Control.mk (name, description, parser, default) 

    (* Absolute Budget Size Control *)
    fun default (_) = NONE

    fun parser (s : string) =
        case Int.fromString (s)
         of NONE => NONE
          | SOME n => 
            if (n >= 0) then SOME (SOME n) else NONE

    fun description () =
        L.str (passname^" absolute budget size is the optional maximum " ^
               "number of mil instructions increase allowed during "^
               "inlining. The default value is no maximum. The number "^
               "must be non negative.")
        
    val name = passname ^ ":abs-bdg-size"
               
    val (absBdgSize, getAbsBdgSize) =
        Config.Control.mk (name, description, parser, default) 

    (* Minimum call site execution frequency *)
    val defaultValue = IntInf.fromInt 1
    fun default (_) = defaultValue
                      
    fun parser (s : string) =
        case IntInf.fromString (s)
         of NONE => NONE
          | SOME n => 
            if IntInf.isNegative n then
              NONE
            else 
              SOME n
              
    fun description () =
        L.str (passname ^ " minimum execution frequency is a limit used to " ^
               "avoid inlining infrequently executed call sites. The " ^
               "default value is " ^ IntInf.toString (defaultValue) ^ 
               ". The number must be non negative.")

    val name = passname ^ ":min-exec-freq"

    val (minExecFreq, getMinExecFreq) =
        Config.Control.mk (name, description, parser, default) 

    val maxRounds = 4

    (* Recursive call inlining limit. *)
    val defaultValue = 3

    fun default (_) = defaultValue

    fun parser (s : string) =
        case Int.fromString (s)
         of NONE => NONE
          | SOME n => 
            if (n >= 0) then SOME n else NONE

    fun description () =
        L.str (passname ^ " recursive call inlining limit limits the ammount" ^
               " of recursive call inlining for each function. " ^ 
               Int.toString defaultValue ^ " inlining is the default limit." ^
               " The number must be non negative.")
        
    val name = passname ^ ":rec-call-limit"
               
    val (recCallLimit, getRecCallLimit) =
        Config.Control.mk (name, description, parser, default) 

    val all = [relBdgSize, absBdgSize, minExecFreq, recCallLimit]

  end

  (* Module feature knowbs. *)
  structure Feature = 
  struct
  
    val (noOptimizerF, noOptimizer) =
        Config.Feature.mk (passname ^ ":never-optimize", 
                           "Do not call optimizer even when call site " ^
                           "selection cannot find call site candidates")
        
    val (noRecFuncsF, noRecFuncs) =
        Config.Feature.mk (passname ^ ":no-rec-funcs", 
                           "Do not inline recursive functions")
        
    val all = [noOptimizerF, noRecFuncsF]
              
  end

  structure Debug = 
  struct 

    (* Debug knobs *)
    val (prnMilFreqD, prnMilFreq) =
        Config.Debug.mk (passname ^ ":print-mil-freq", 
                         "print the mil IR annotated with execution " ^
                         "frequency information.")

    val (prnCallGraphD, prnCallGraph) =
        Config.Debug.mk (passname ^ ":print-call-graph", 
                         "print call graph after each inline iteration")

    val (debugPassD, debugPass) =
        Config.Debug.mk (passname, "debug the Mil inline profile pass")

    val all = [prnMilFreqD, prnCallGraphD, debugPassD]

    (* Number of times the module was called. *)
    val nExec = ref 0

    val printMilFreq : PD.t -> bool = fn d => prnMilFreq (PD.getConfig d)

    (* Helper functions *)
    val incExec : unit -> unit = fn () => nExec := !nExec + 1
    val getExec : unit -> int  = fn () => !nExec

    val print : PD.t * string -> unit = 
     fn (d, msg) =>
        if Config.debug andalso debugPass (PD.getConfig d) then
          print (passname ^ ": " ^ msg )
        else ()

    val printLayout : PD.t * Layout.t -> unit = 
     fn (d, l) =>
        if Config.debug andalso debugPass (PD.getConfig d) then
          LU.printLayout (L.seq [L.str (passname ^ ": "), l])
        else ()
             
    val printStartMsg : PD.t -> unit = 
     fn (d) => print (d, " - Starting the profile inliner (Iteration" ^
                         " # " ^ Int.toString (getExec ()) ^ ")...\n")
                      
    val printEndMsg : PD.t -> unit = 
     fn (d) => print (d, " - Finishing the profile inliner...\n")
               
    val printCallGraph : IMil.t * PD.t * int * Layout.t *
                         (Mil.label -> IntInf.t option) -> unit = 
     fn  (imil, d, nIter, comment, prof) =>
         let
           val gLabel = 
               "Call graph before profile module iteration # " ^
               Int.toString (getExec ()) ^ "-" ^ Int.toString (nIter) ^ "." ^
               Layout.toString (comment)
         in
           if prnCallGraph (PD.getConfig d) then
             ACGP.printCallGraph' (d, imil, gLabel, SOME prof)
           else
             ()
         end

    (* Check the imil if debugging. *)
    val checkIMil : PD.t * IMil.t -> unit =
     fn (d, imil) =>
        if Config.debug andalso debugPass (PD.getConfig d) then 
          IMil.T.check imil
        else 
          ()
  end

  structure Chat = ChatF (struct 
                            type env = PD.t
                            val extract = PD.getConfig
                            val name = passname
                            val indent = 2
                          end)

  structure Time = 
  struct
    val start  : unit -> Time.t = Time.now 
    val report1 : PassData.t * string * Time.t -> unit = 
     fn (d, subpass, t1) =>
        let
          val t2 = Time.now ()
        in 
          Chat.log1 (d, subpass ^ "\" executed in " ^ 
                        Time.toString (Time.- (t2, t1))^ "s.")
        end
    val report2 : PassData.t * string * Time.t -> unit = 
     fn (d, subpass, t1) =>
        let
          val t2 = Time.now ()
        in 
          Chat.log2 (d, subpass ^ "\" executed in " ^ 
                        Time.toString (Time.- (t2, t1))^ "s.")
        end
  end

  structure Constraints = 
  struct

    datatype t = C of {
             relBdgSize   : Real.t,
             absBdgSize   : int option,
             minExecFreq  : IntInf.t,
             recCallLimit : int,
             noRecFuncs   : bool}
                      
    fun getMinExecFreq      (C {minExecFreq, ...})  = minExecFreq
    fun getRecCallLimit     (C {recCallLimit, ...}) = recCallLimit
    fun getNoRecursiveFuncs (C {noRecFuncs, ...})   = noRecFuncs
    fun getRelBdgSize       (C {relBdgSize, ...})   = relBdgSize
    fun getAbsBdgSize       (C {absBdgSize, ...})   = absBdgSize

    val build: PD.t -> t =
     fn (d) => 
        C {relBdgSize   = Control.getRelBdgSize   (PD.getConfig d), 
           absBdgSize   = Control.getAbsBdgSize   (PD.getConfig d), 
           minExecFreq  = Control.getMinExecFreq  (PD.getConfig d),
           recCallLimit = Control.getRecCallLimit (PD.getConfig d),
           noRecFuncs   = Feature.noRecFuncs      (PD.getConfig d)}

    val computeSizeBudget : t * int -> int = 
     fn (constraints, initPrgSize) =>
        let
          val relBudgetSize = Real.floor (Real.fromInt (initPrgSize) * 
                                          getRelBdgSize (constraints))
          val absBudgetSize = case getAbsBdgSize (constraints)
                               of SOME sz => SOME sz
                                | NONE => NONE
        in
          case absBudgetSize
           of SOME absSz => Int.min (relBudgetSize, absSz)
            | NONE       => relBudgetSize
        end
        
  end

  (* IMil util stuff. *)
  structure Util =
  struct

    val getCallTarget : IMil.t * IMil.iInstr -> Mil.variable option =
     fn (imil, i) =>
        let
          fun callConv conv = 
              case conv
               of M.CCode {ptr, ...} => 
                  (case IMil.IFunc.getIFuncByName' (imil, ptr)
                    of SOME _ => SOME ptr
                     | NONE => NONE)
                | M.CClosure {cls, code} => NONE
                | M.CDirectClosure {cls, code} => SOME code
        in
          case IMil.IInstr.toTransfer i
           of SOME (M.TInterProc {callee, ret, fx}) =>
              (case callee 
                of M.IpCall {call, args} => callConv call
                 | M.IpEval {typ, eval} => NONE)
            | SOME (M.TGoto t) => fail ("getCallTarget", "goto target?")
            | SOME (M.TCase cs) => fail ("getCallTarget", "constant switch target?")
            | SOME (M.TReturn r) => fail ("getCallTarget", "return target?")
            | SOME (M.TCut c) => fail ("getCallTarget", "Cut target?")
            | _ => fail ("getCallTarget", "no target?")
        end

        
    val getFunSize : IMil.t * Mil.variable -> int = 
     fn (imil, f) => IMil.IFunc.getSize (imil, IMil.IFunc.getIFuncByName (imil, f))
                                             
    val getProgSize : IMil.t -> int =
     fn (imil) => 
        List.fold (IMil.IFunc.getIFuncs (imil), 0, 
                fn ((f, cfg), sz) => sz + IMil.IFunc.getSize (imil, cfg))

    val getInstrFun : IMil.t * IMil.iInstr -> Mil.variable =
     fn (imil, i) => IMil.IFunc.getFName (imil, IMil.IInstr.getIFunc (imil, i))

    val getInstrBlockLabel : IMil.t * IMil.iInstr -> Mil.label =
     fn (imil, i) => 
        #1 (IMil.IBlock.getLabel' (imil, IMil.IInstr.getIBlock (imil, i)))

  end
  
  (* Profiling information. *)
  structure ProfInfo =
  struct

    structure Profiler = MilProfilerF (type env         = PD.t
                                       val getConfig    = PD.getConfig
                                       val passname     = passname
                                       structure MilCfg = MilCfg
                                       structure MilCG  = MilCallGraph)
    
    datatype t = T of {
             milAbsFreqs : Profiler.CFG.cfgAbsFreq,
             milEdgProbs : Profiler.CFG.edgeProb
    }

    val build : PD.t * IMil.t  -> t =
     fn (d, imil) =>
        let
          val mil = IMil.T.unBuild (imil)
          val (milEdgProbs, _, milAbsFreqs) = Profiler.computeProfilingInfo (d, mil)
        in
          T {milAbsFreqs = milAbsFreqs,
             milEdgProbs = milEdgProbs}
        end

    val getExecFreq : t * Mil.label -> Profiler.absFrequency option = 
     fn (T {milAbsFreqs = (blkFreqs, edgFreqs), ...}, blk) =>
        LD.lookup (blkFreqs, blk)
        
    val zero = IntInf.fromInt 0

    (* Prints the CFG annotated with block frequencies and edge 
     * probabilities. *)
    val printFunctions : PD.t * IMil.t * t -> unit = 
     fn (d, imil, T {milEdgProbs = edgProb, 
                     milAbsFreqs = (blkAbsFreq, edgAbsFreq)}) =>
        let
          val env = PD.getConfig d
          val mil as Mil.P {globals, symbolTable=smt, ...} = IMil.T.unBuild imil
          val globalsList = VD.toList globals
          fun layoutBBFreq (b) = 
              case LD.lookup (blkAbsFreq, b)
               of SOME f => L.str (IntInf.toString f)
                | NONE   => L.empty
          fun layoutEdge (src : Mil.label, tgt: Mil.label) =
              case LD.lookup (edgProb, src)
               of NONE     => L.empty
                | SOME dic => 
                  (case LD.lookup (dic, tgt)
                    of NONE      => L.empty
                     | SOME prob => L.str ("P " ^ Real.toString (prob)))
          fun layoutFunc (f : Mil.variable, code) =
              let
                val si = Identifier.SymbolInfo.SiTable smt
                val header = L.seq [L.str "G ", ML.layoutVariable (env, si, f),
                                    L.str " = "]
                val body = ML.General.layoutCode (env, si, {varBind = NONE, 
                                                            block = SOME layoutBBFreq, 
                                                            edge = SOME layoutEdge,
                                                            cb = NONE}, code)
              in
                L.mayAlign [header, LU.indent body]
              end
          fun layoutGlobal (f, g) = 
              case g
               of Mil.GCode code => SOME (layoutFunc (f, code))
                | _ => NONE
          val l = L.align (List.keepAllMap (globalsList, layoutGlobal))
        in
          LU.printLayout l
        end
               
  end
  
  structure CallSitesInfo = 
  struct
  
    datatype csInfo = I of {
             freq    : ProfInfo.Profiler.absFrequency ref,
             srcFun  : Mil.variable,
             tgtFun  : Mil.variable,
             inlined : bool ref,
             callId  : callId
    }

    val inlined    : csInfo -> bool = fn (I {inlined, ...}) => !inlined
    val setInlined : csInfo -> unit = 
     fn (I {inlined, ...}) => inlined := true
    val getFreq    : csInfo -> ProfInfo.Profiler.absFrequency = 
     fn (I i) => !(#freq i)
    val isRecCall  : csInfo -> bool = 
     fn (I {srcFun, tgtFun, ...}) => MilUtils.Compare.variable (srcFun, tgtFun) = EQUAL
    val getTgtFun  : csInfo -> Mil.variable = fn (I i) => #tgtFun i
    val getSrcFun  : csInfo -> Mil.variable = fn (I i) => #srcFun i
    val getCallId  : csInfo -> callId = fn (I i) => #callId i

    val layoutCSInfo : IMil.t  * csInfo -> Layout.t =
     fn (imil, I {freq, srcFun, tgtFun, inlined, callId}) =>
        let
          val si         = IMil.T.getSi imil
          val srcFunLay  = ID.layoutVariable' srcFun
          val blk        = Util.getInstrBlockLabel (imil, callId)
          val blkLay     = Identifier.layoutLabel blk
          val tgtFunLay  = ID.layoutVariable' tgtFun
          val freqLay    = IntInf.layout (!freq)
          val inlinedLay = if !inlined then L.str " inlined"
                           else L.str ""
        in
          L.seq [srcFunLay, L.str "::", blkLay, L.str " -> ", 
                 tgtFunLay, L.str " [", freqLay, L.str "]", inlinedLay]
        end  

    datatype funInfo = FI of {
             freq      : ProfInfo.Profiler.absFrequency ref,
             size      : int ref,
             recursive : bool,
             callSites : csInfo LD.t ref
    }

    datatype t = T of {
             funInfoDict : funInfo VD.t,
             callSites   : csInfo LD.t ref
    }

    val getFunInfoDict : t -> funInfo VD.t =
     fn (T {funInfoDict, ...}) => funInfoDict

    val layout : t * IMil.t -> Layout.t =
     fn (T {funInfoDict, ...}, imil) => 
        let
          fun layoutFunInfo (f, FI {freq, size, recursive, callSites}) = 
              let
                val si      = IMil.T.getSi imil
                val funLay  = ID.layoutVariable' f
                val freqLay = IntInf.layout (!freq)
                val sizeLay = Int.layout (!size)
                val csList  = LD.toList (!callSites)
                val recursiveLay = if recursive then L.str ", recursive"
                                   else L.str ""
                val callSitesLay = 
                    L.align (List.map (csList, fn (_, csInfo) => 
                                                  layoutCSInfo (imil, csInfo)))
              in
                L.align [L.seq [L.str "Func: ", funLay, L.str ", sz: ", 
                                sizeLay, L.str ", freq: ", 
                                freqLay, recursiveLay],
                          L.indent (callSitesLay, 3)]
              end
        in
          L.align (List.map (VD.toList funInfoDict, layoutFunInfo))
        end

    fun getFunInfo (T {funInfoDict, ...}, f : Mil.variable) =
        case VD.lookup (funInfoDict, f)
         of SOME funInfo => funInfo
          | NONE => fail ("getFunInfo", "Could not find info for function.")

    fun isFunExist (T {funInfoDict, ...}, f : Mil.variable) = 
        case VD.lookup (funInfoDict, f)
         of SOME funInfo => true
          | NONE => false
                    
    val addCallSites : PD.t * t * Mil.variable * (Mil.label * csInfo) list -> unit =
     fn (d, callSitesInfo as T {callSites=allCallSites, ...}, f, csList) =>
        let
          val FI {callSites=funCallSites, ...} = getFunInfo (callSitesInfo, f)
          val () = funCallSites := LD.insertAll (!funCallSites, csList) 
          val () = allCallSites := LD.insertAll (!allCallSites, csList) 
        in ()
        end

    val isRecursive : IMil.t * PD.t * t * Mil.variable -> bool = 
     fn (imil, d, callSitesInfo, f) =>
        let
          val FI {recursive, ...} = getFunInfo (callSitesInfo, f)
        in
          recursive
        end

    val getFunFreq : PD.t * t * Mil.variable -> ProfInfo.Profiler.absFrequency ref =
     fn (d, callSitesInfo, f) => 
        let
          val FI {freq, ...} = getFunInfo (callSitesInfo, f)
        in
          freq
        end          

    val getFunCallSites : PD.t * t * Mil.variable -> csInfo LD.t =
     fn (d, callSitesInfo, f) => 
        let
          val FI {callSites, ...} = getFunInfo (callSitesInfo, f)
        in
          !callSites
        end          

    val getFunSize : PD.t * t * Mil.variable -> int =
     fn (d, callSitesInfo, f) => 
        let
          val FI {size, ...} = getFunInfo (callSitesInfo, f)
        in
          !size
        end          
        
    val incFunSize : PD.t * t * Mil.variable * int -> unit =
     fn (d, callSitesInfo, f, sz) =>
        let
          val FI {size, ...} = getFunInfo (callSitesInfo, f)
          in
          size := !size + sz
        end
        
    val getCSInfo : t * Mil.label -> csInfo option =
     fn (T {callSites, ...}, l) => LD.lookup (!callSites, l)

    fun inlineableCS (d, imil, i) =
        let
          (* Check call convention. *)
          fun chkCallConv conv = 
              case conv
               of M.CCode {ptr, ...}           => isSome (IMil.IFunc.getIFuncByName' (imil, ptr))
                | M.CDirectClosure {cls, code} => true
                | _ => false
        in
          (* Check transfer. Only TCall and TTailCall are inlineable. *)
          case IMil.IInstr.toTransfer (i)
           of SOME (M.TInterProc {callee, ret, fx}) => 
              (case callee 
                of M.IpCall {call, args} => chkCallConv call
                 | _ => false)
(*              SOME (M.TCall (conv, _, _, _, _)) => chkCallConv conv
            | SOME (M.TTailCall (conv, _, _))   => chkCallConv conv*)
            | _ => false
        end

    fun buildFunInfoDict (d : PD.t, imil : IMil.t, profInfo : ProfInfo.t) : 
        funInfo VD.t =
        let
          fun doCallSite (i) = 
              let
                val csBlk  = Util.getInstrBlockLabel (imil, i)
                val tgtFun = valOf (Util.getCallTarget (imil, i))
                val srcFun = Util.getInstrFun (imil, i)
                (* XXX EB: If it do not find the frequency for some reason and 
                 * fail, it would be safe to use the default value as 0. *)
                val freq   = valOf (ProfInfo.getExecFreq (profInfo, csBlk))
                val csInfo = I {freq    = ref freq,
                                srcFun  = srcFun,
                                tgtFun  = tgtFun,
                                inlined = ref false,
                                callId  = i}
              in
                (csBlk, csInfo)
              end
          fun doCfg (cfg) = 
              let
                val f : Mil.variable = IMil.IFunc.getFName (imil, cfg)
                val entryBlk = IMil.IFunc.getStart (imil, cfg)
                val entryLabel = #1 (IMil.IBlock.getLabel' (imil, entryBlk))
                (* XXX EB: If it do not find the frequency for some reason and 
                 * fail, it would be safe to use the default value as 0. *)
                val freq = valOf (ProfInfo.getExecFreq (profInfo, entryLabel))
                val size = Util.getFunSize (imil, f)
                val tfs = IMil.Enumerate.IFunc.transfers (imil, cfg)
                val cs  = List.keepAll (tfs, fn i => inlineableCS (d, imil, i))
                val callSites = LD.fromList (List.map (cs, doCallSite))
                val fInfo = FI {freq      = ref freq,
                                size      = ref size,
                                recursive = IMil.IFunc.getRecursive (imil, cfg),
                                callSites = ref callSites}
              in
                (f, fInfo)
              end

          val cfgs = IMil.Enumerate.T.funcs (imil)
          val funInfoList = List.map (cfgs, doCfg)
        in
          VD.fromList (funInfoList)
        end

    fun groupAllCallSitesDict (funInfoDict : funInfo VD.t) : csInfo LD.t = 
        let
          fun collectCallSites (f, FI {callSites, ...}, callSitesDict) = 
              LD.union (callSitesDict, !callSites, 
                  fn _ => fail ("collectCallSites", "duplicated block label."))
        in
          VD.fold (funInfoDict, LD.empty, collectCallSites)
        end
        
    val new : PD.t * IMil.t * ProfInfo.t -> t =
     fn (d, imil, profInfo) =>
        let
          val funInfoDict  = buildFunInfoDict (d, imil, profInfo)
          val allCallSites = groupAllCallSitesDict (funInfoDict)
        in
          T {funInfoDict = funInfoDict,
             callSites   = ref allCallSites}
        end
          
        
    (* Update  execution frequency  after inlining. Arguments  are the
     * inlined  edge  and  a  dictionary that maps the  original  call
     * transfer blocks to the newly duplicated  ones. 
     *
     *  Example:
     *
     *  Program    |    Call Graph    |    Call Graph    
     *             |   Before Inline  |   After Inline C at cs1
     *             |                  |                  
     *  Func A     |                  |                  
     *    cs1 (C)  |    (A)   (B)     |    (AC)    (B)     
     *             |  cs1 \   / cs2   | cs3'||cs4'  | cs2   
     *  Func B     |       v v        |     ||      v        
     *    cs2 (C)  |       (C)        |     ||     (C)        
     *             |   cs3 / \ cs4    |     ||  cs3//cs4
     *  Func C     |       | |        |      \\   //
     *    cs3 (D)  |       v v        |       vv vv         
     *    cs4 (D)  |       (D)        |        (D)        
     *             |                  |                  
     *  Func D     |                  |
     *             |                  |    
     *  let:
     *    - cs1->C be the call site;
     *    - C the inlined function;
     *    - cs3 and cs4 the call sites in C; and
     *    - cs3' and cs4' the duplicated (after inline) call sites
     *
     *  we update the frequencies using the following heuristic:
     *
     *  oldFreq (C) = Freq (C)
     *  newFreq (C) = oldFreq (C) - Freq (cs1->C)
     *  Freq (C)    = newFreq (C)
     *  redFactor   = newFreq (C) / oldFreq (C)
     *
     *  For every call site cs' duplicated form cs
     *    Freq cs' = Freq cs * (1 - redFactor)
     *
     *  For every call site cs in C:
     *    Freq cs = Freq cs * redFactor 
     *)
    val inlineUpdate : PD.t * t * csInfo * (Mil.label * IMil.iInstr) LD.t -> unit =
     fn (d, callSitesInfo, inlinedCS, blkMapping) =>
        let
          val srcFun = getSrcFun (inlinedCS)
          val tgtFun = getTgtFun (inlinedCS)
          fun duplicateCallSites (orgCallSites : csInfo LD.t) =
              let
                fun mapBlk (orgBlk) = 
                    case LD.lookup (blkMapping, orgBlk)
                     of SOME newBlk => newBlk
                      | NONE => fail ("mapBlk", "Could not map block.")
                (* Duplicate the call sites in the original function
                 * only if they were not inlined. *)
                fun buildNewCallSite (orgBlk, I {freq, tgtFun, 
                                                 inlined, ...}) = 
                    if (!inlined) then 
                      NONE
                    else
                      let
                        val (newBlk, newInstr) = mapBlk (orgBlk)
                        val newCSInfo = I {freq    = ref (!freq),
                                           srcFun  = srcFun,
                                           tgtFun  = tgtFun,
                                           inlined = ref false,
                                           callId  = newInstr}
                        (* XXX EB: Debug code. Keep it for a while
                        val () = print ("InlineProfile: buildNewCallSite : " ^
                                        "Copy from " ^ 
                                        Identifier.labelString (orgBlk) ^
                                        " to " ^ 
                                        Identifier.labelString (newBlk) ^ "\n")
                        --- *)
                      in
                        SOME (newBlk, newCSInfo)
                      end
                val orgCSList = LD.toList (getFunCallSites (d,
                                                            callSitesInfo, 
                                                            tgtFun))
                val newCSList = List.keepAllMap (orgCSList, buildNewCallSite)
              in
                LD.fromList (newCSList)
              end
          (* Duplicate the call sites in the inlined function *)
          val orgCallSites = getFunCallSites (d, callSitesInfo, tgtFun)
          val newCallSites = duplicateCallSites (orgCallSites)

          (* Update the execution frequencies: original and new ones.
           * newFreq tgtFun = oldFreq tgtFun - inlinedCS freq
           * redFactor = newFreq tgtFun / oldFreq tgtFun *)
          val tgtFunFreq = getFunFreq (d, callSitesInfo, tgtFun)
          val oldFreq = !tgtFunFreq
          val inlinedCSFreq = getFreq (inlinedCS)
          val newFreq = oldFreq - inlinedCSFreq
          (* freq * newFreq/oldFreq *)
          fun updateCSFreq1 (blk, I {freq as ref f, ...}) = 
              freq := (if oldFreq > 0 then (f * newFreq) div oldFreq else 0)
          (* freq * (1 - newFreq/oldFreq *)
          fun updateCSFreq2 (blk, I {freq as ref f, ...}) = 
              freq := (if oldFreq > 0 then f - ((f * newFreq) div oldFreq) else f)
          (* - For every call site in tgtFun
           *    callSite Freq = callSite Freq * redFactor *)
          val () = LD.foreach (orgCallSites, updateCSFreq1)
          (* - For every call site in newCallSites
           *    callSite Freq = callSite Freq * (1 - redFactor) *)
          val () = LD.foreach (newCallSites, updateCSFreq2)
          (* Insert the new call sites into the source function. *)
          val () = addCallSites (d, callSitesInfo, srcFun, 
                                 LD.toList newCallSites)
        in
          ()
        end
        
    val foreach : t * (Mil.label * csInfo -> unit) -> unit =
     fn (T {callSites, ...}, f) => LD.foreach (!callSites, f)

    val fold : t * 'a * (Mil.label * csInfo * 'a -> 'a) -> 'a =
     fn (T {callSites, ...}, acc, f) => LD.fold (!callSites, acc, f)

  end
  
  datatype policyInfo = PI of {
           constraints  : Constraints.t,
           callSites    : CallSitesInfo.t ref,
           recInlining  : int ref VD.t ref,
           inlinedCS    : CallSitesInfo.csInfo option ref,
           newBlocksMap : (Mil.label * (Mil.label * IMil.iInstr)) list ref,
           initPrgSize  : int,
           initBudget   : int,
           currBudget   : int ref,
           nIterations  : int ref,
           rounds       : int ref
  }
                  
  fun getRecInliningCountRef (PI {recInlining as ref d, ...}, f) =
      case VD.lookup (d, f)
       of SOME c => c
        | NONE   => 
          let
            val r = ref 0
            val () = recInlining := VD.insert (d, f, r)
          in r
          end
  (* recInlining  keeps track of how  many times a  given function was
   * recursively inlined. *)
  fun getRecInliningCount (info, f) = !(getRecInliningCountRef (info, f))

  fun incRecInliningCount (info, f) =
      let
        val r = getRecInliningCountRef (info, f)
      in r := !r + 1
      end

  fun setInlinedCS (PI {inlinedCS, ...}, csInfo) = inlinedCS := csInfo
  fun getInlinedCS (PI {inlinedCS, ...} ) = !inlinedCS
  fun incIterations (PI {nIterations, ...}) = nIterations := !nIterations + 1
  fun incRounds (PI {rounds, ...}) = rounds := !rounds + 1
  fun getCallSitesInfo (PI {callSites, ...}) = callSites 
  fun getInitPrgSz (PI {initPrgSize, ...}) = initPrgSize
  fun getCurrBudget (PI {currBudget , ...}) = !currBudget 
  fun getIterations (PI {nIterations, ...}) = !nIterations
  fun getRounds (PI {rounds, ...}) = !rounds
  fun getMinExecFreq (PI {constraints, ...}) = 
      Constraints.getMinExecFreq (constraints)
  fun recCallLimit (PI {constraints, ...}) = 
      Constraints.getRecCallLimit (constraints)
  fun noRecursiveFuncs (PI {constraints, ...}) = 
      Constraints.getNoRecursiveFuncs (constraints)
      
  fun getDbgProf (PI {callSites, ...}) = 
   fn blk => Option.map (CallSitesInfo.getCSInfo (!callSites, blk), 
                         CallSitesInfo.getFreq)
                                    
  fun callIdToCall (info : policyInfo, imil : IMil.t, call : callId) = call
  fun rewriteOperation (c : callId) = InlineFunctionCopy

  fun buildCallSitesInfo (d, imil) = 
      let
        val startTime = Time.start ()
        val profiling = ProfInfo.build (d, imil)
        val () = if Debug.printMilFreq (d) then
                   ProfInfo.printFunctions (d, imil, profiling)
                 else
                   ()
        val () = Time.report1 (d, "Profiling analysis", startTime)
        val startTime = Time.start ()
        val callSites = CallSitesInfo.new (d, imil, profiling)
        val () = Time.report1 (d, "Call sites information analysis", startTime)
      in
        callSites
      end

  (*  Recompute the policy infom Called after optiming the program. *)
  fun recomputePolicyInfo (d, imil, info as PI {initPrgSize, initBudget, 
                                                currBudget, callSites,
                                                newBlocksMap, ...}) =
      let
        val currPrgSize = Util.getProgSize (imil)
        val codeGrowth  = currPrgSize - initPrgSize
        val () = currBudget := initBudget - codeGrowth
        val () = callSites  := buildCallSitesInfo (d, imil)
        val () = newBlocksMap := nil
      in ()
      end

  (* Initialize the policyInfo information.
   *    - Build the "call sites info" (only inlineable calls)
   *    - Initialize the "new blocks mapping" as empty  *)
  fun analyze (d : PD.t, imil : IMil.t) = 
      let
        val constraints = Constraints.build (d)
        val initPrgSize = Util.getProgSize (imil)
        val budgetSize  = Constraints.computeSizeBudget (constraints,
                                                         initPrgSize)
        val callSites = buildCallSitesInfo (d, imil) 
        val recInlining = VD.map (CallSitesInfo.getFunInfoDict (callSites), 
                               fn f => ref 0)
        val info = PI {constraints  = constraints,
                       callSites    = ref callSites,
                       recInlining  = ref recInlining,
                       inlinedCS    = ref NONE,
                       newBlocksMap = ref nil,
                       initPrgSize  = initPrgSize,
                       initBudget   = budgetSize,
                       currBudget   = ref budgetSize,
                       nIterations  = ref 0,
                       rounds       = ref 0}
      in
        info
      end

  (* Save the instr mapping during inlining to update the call sites info. *)
  val associateCallToCallId : policyInfo * (* The inline information. *)
                              IMil.t     * (* The imil program. *)
                              callId     * (* Call site being inlined. *)
                              IMil.iBlock * (* Block being copied. *)
                              IMil.iBlock   (* The copy. *) -> unit =
   fn (info as PI {newBlocksMap, ...}, imil, call, orgBlk', newBlk') =>
      let
        val orgBlk = #1 (IMil.IBlock.getLabel' (imil, orgBlk'))
        val newBlk = #1 (IMil.IBlock.getLabel' (imil, newBlk'))
        val callSites = getCallSitesInfo (info)
        val csInfo = CallSitesInfo.getCSInfo (!callSites, orgBlk)
        (* XXX EB: Debug code. Keep it for a while
        fun getBlkStr (blk) = Identifier.labelString blk
        val () = print ("InlineProfile: inlineMap : Map from " ^ 
                        getBlkStr (orgBlk) ^ " to " ^ 
                        getBlkStr (newBlk) ^ ".\n")
        --- *)
      in
        case csInfo
         of SOME cs => (* Only add relevant mapping: call sites *)
            let
              val callInstr = IMil.IBlock.getTransfer (imil, newBlk')
            in
              newBlocksMap := (orgBlk, (newBlk, callInstr ))::(!newBlocksMap)
            end
          | NONE => ()
      end
      
  fun selectBestCallSite (d, imil, info) = 
      let
        val startTime = Time.start ()
        val ref callSitesInfo = getCallSitesInfo (info)
        val currBudget = getCurrBudget (info)
        val minExecFreq = getMinExecFreq (info)
        (* XXX EB: Debug code. Keep it for a while *)
        val () = Debug.print (d, "selectBestCallSite\n")
        val () = Debug.print (d, "Size budget = " ^ 
                                 Int.toString (currBudget) ^ "\n");
        val () = Debug.print (d, "Min exec freq = " ^ 
                                 IntInf.toString minExecFreq ^
                                 "\n")
        (* Print the call sites information. *)
        val () = Debug.printLayout (d, CallSitesInfo.layout (callSitesInfo, 
                                                             imil))

        (* A valid call site is one that comply with the constraints. *)
        fun validCS csInfo = 
            let
              val execFreq  = CallSitesInfo.getFreq   (csInfo)
              val tgtFun    = CallSitesInfo.getTgtFun (csInfo)
              val recursive = CallSitesInfo.isRecursive (imil, d, callSitesInfo, tgtFun)
              fun noRecInlining (f) = getRecInliningCount (info, f) >=
                                      recCallLimit (info)
            in
              CallSitesInfo.isFunExist (callSitesInfo, tgtFun)
              andalso not (CallSitesInfo.inlined csInfo)
              andalso IntInf.>= (execFreq, minExecFreq) 
              andalso CallSitesInfo.getFunSize (d, callSitesInfo, tgtFun) <= currBudget 
              andalso not (recursive andalso noRecInlining tgtFun)
              andalso not (recursive andalso noRecursiveFuncs info)
            end

        val choose =
         fn (l, cs1, acc) =>
            if validCS cs1 then
              case acc
               of NONE     => SOME cs1
                | SOME cs2 => 
                  if CallSitesInfo.getFreq cs2 < CallSitesInfo.getFreq cs1 then
                    SOME cs1
                  else
                    acc
            else
              acc

        val csi as CallSitesInfo.T {funInfoDict, callSites} = callSitesInfo

        val bestCS = CallSitesInfo.fold (callSitesInfo, NONE, choose)
        val () = Time.report2 (d, "Select best call site", startTime)
      in bestCS
      end
      
  (* XXX EB: Debug function. Remove it latter. *)
  fun printIMilDbg (d, imil) = 
      let
        val mil = IMil.T.unBuild imil
        val l = MilLayout.layout (PD.getConfig d, mil)
        val () = Debug.print (d, "Print IMIL at inline-profile\n")
        val () = Debug.printLayout (d, l)
      in ()
      end

  (* Optimize the program, rebuild the call sites info, and try to
   * select the best call site. *)
  fun optimizeAndSelect (d, imil, info) = 
      if getRounds info < Control.maxRounds then 
        let
          val () = incRounds info
          val startTime = Time.start ()
          val () = MilSimplify.program (d, imil)
          val () = Time.report1 (d, "Optimize (before select best cs)", startTime)
          val () = recomputePolicyInfo (d, imil, info)
        in
          selectBestCallSite (d, imil, info)
        end
      else
        NONE

  fun updateCallSitesInfo (d, 
                           PI {callSites, newBlocksMap, inlinedCS, ...}, 
                           imil) =
      case !inlinedCS
       of SOME cs => 
          let
            val blkMapping = LD.fromList (!newBlocksMap)
            (* Clean the inlined call site info. *)
            val () = newBlocksMap := nil
            val () = inlinedCS := NONE
          in
            CallSitesInfo.inlineUpdate (d, !callSites, cs, blkMapping)
          end
        | NONE => ()
      
  (* Update the current budget and function sizes assuming we are
   * inlining cs. *)
  fun updateSizes (d, info as PI {currBudget, callSites, ...}, csInfo) = 
      let
        (* Update source function size *)
        val srcFun = CallSitesInfo.getSrcFun  (csInfo)
        val tgtFun = CallSitesInfo.getTgtFun  (csInfo)
        val tgtSz  = CallSitesInfo.getFunSize (d, !callSites, tgtFun)
        val ()     = CallSitesInfo.incFunSize (d, !callSites, srcFun, tgtSz)
      in
        (* Update the current budget size *)
        currBudget := !currBudget - tgtSz
      end

  fun printCallGraph (info, d, imil, lastInlined) = 
      let
        val it = getIterations info
        val prof = getDbgProf info
        val comment = 
            case lastInlined
             of SOME csi => L.seq [L.str " (After inlining: ",
                                   CallSitesInfo.layoutCSInfo (imil, csi),
                                   L.str ")"]
              | NONE => L.str "(No inlining in the previous iteration)"
      in
        Debug.printCallGraph (imil, d, it, comment, prof)
      end

  (* Select the call sites to inline.
   * policy:
   *   - Use the "new blocks mapping" to update the "call sites info".
   *     (including frequencies).
   *   - Update the budget
   *   - cs = Select the best call site.
   *     if !cs and allowOpt then optimize and try to select again.
   *     else return cs
   *)
  fun policy (info: policyInfo, d: PD.t, imil: IMil.t) =
      let
        val () = Chat.log1 (d, "Starting inline profile round "^ Int.toString (getRounds info))
        val () = incIterations (info)
        val lastInlined = getInlinedCS (info)
        val () = updateCallSitesInfo (d, info, imil)
        val () = printCallGraph (info, d, imil, lastInlined)
        val bestCS = selectBestCallSite (d, imil, info)
        val allowOpt = not (Feature.noOptimizer (PD.getConfig d))
        val csInfo = case (bestCS, allowOpt)
                      of (NONE, true)     => optimizeAndSelect (d, imil, info)
                       | (SOME csInfo, _) => SOME csInfo
                       | (NONE, _)        => NONE
        val () = setInlinedCS (info, csInfo)
        (* XXX EB: Debug code. Keep it for a while.
        val () = printIMilDbg (d, imil)
         -- *)
      in
        case csInfo
         of NONE => nil (* No call sites selected. *)
          | SOME csInfo =>
            let
              val tgtFun = CallSitesInfo.getTgtFun csInfo
              val () = if CallSitesInfo.isRecCall (csInfo) 
                       then incRecInliningCount (info, tgtFun)
                       else ()
              val () = updateSizes (d, info, csInfo)
              val () = PD.click (d, "ProfileCallSitesInlined")
              val () = Debug.printLayout (d, 
                             L.seq [L.str "Inlining: ",
                                    CallSitesInfo.layoutCSInfo (imil, csInfo)])
              val () = CallSitesInfo.setInlined (csInfo)
            in
              [CallSitesInfo.getCallId (csInfo)]
            end
      end

  structure Inliner = MilInlineRewriterF (
                        type policyInfo            = policyInfo
                        val  analyze               = analyze
                        type callId                = callId
                        val  callIdToCall          = callIdToCall
                        val  associateCallToCallId = associateCallToCallId
                        val  rewriteOperation      = rewriteOperation
                        val  policy                = policy
                        (* optimize: Do not optimize after inlining. The 
                         * policy function will optimize at its discretion. *)
                        val  optimizer             = NONE)

  fun program (imil : IMil.t, d : PD.t) : unit = 
      let
        val () = Debug.incExec ()
        val () = Debug.printStartMsg (d)
        val () = Inliner.program (d, imil)
        val () = MilSimplify.program (d, imil)
        val () = PD.report (d, passname)
        val () = Debug.printEndMsg (d)
      in ()
      end

  val stats = [("ProfileCallSitesInlined", 
                "call sites inlined (Profile based inliner)")]
              
  val description = {name        = passname,
                     description = "Static profile based inliner",
                     inIr        = BothMil.irHelpers,
                     outIr       = BothMil.irHelpers,
                     mustBeAfter = [],
                     stats       = stats}

  val associates  = {controls  = Control.all,
                     debugs    = Debug.all @ ProfInfo.Profiler.debugs,
                     features  = Feature.all,
                     subPasses = []}

  val pass = Pass.mkOptPass (description, associates, 
                             BothMil.mkIMilPass program)

end (* end of structure MilInlineProfile *)
