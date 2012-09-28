(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_FX_ANALYSIS =
sig
  val pass : (BothMil.t, BothMil.t) Pass.t
end;

structure MilFxAnalysis :> MIL_FX_ANALYSIS =
struct

  structure PD = PassData
  structure M = Mil
  structure MU = MilUtils
  structure ML = MilLayout
  structure MCG = MilCallGraph
  structure MCFG = MilCfg
  structure PLG = PolyLabeledGraph
  structure LS = M.LS
  structure LD = M.LD
  structure VS = M.VS
  structure VD = M.VD
  structure L = Layout
  structure LU = LayoutUtils
  structure FX = Effect
  structure I = Identifier
  structure MRC = MilRewriterClient

  val passname = "MilFxAnalysis"

  val fail = 
   fn (f, m) => Fail.fail ("fx-analysis.sml", f, m)

  val mkDebug = 
   fn (tag, description) => PD.mkDebug (passname^":"^tag, description)

  val (debugPassD, debugPass) =
      mkDebug ("debug", "Debug fx analysis pass according to debug level")

  val (showWorksetD, showWorkset) =
      mkDebug ("show-workset", "Show the workset after each iteration")

  val (showDataflowD, showDataflow) =
      mkDebug ("show-dataflow", "Show changes in the dataflow information")

  structure Chat = ChatF(type env = PD.t
                         val extract = PD.getConfig
                         val name = passname
                         val indent = 2)

  structure Click = 
  struct
    val stats = []
  end (* structure Click *)

  val stats = Click.stats
  val debugs = [debugPassD, showDataflowD, showWorksetD]
    
  datatype fnInfo = FI of {fxMax : M.effects,     (* Annotation gives limit to max effects *)
                           fx : M.effects,        (* Inferred effect *)
                           canExit : bool,        (* Does annotation allow exit *)
                           exits : bool ,         (* Infered exit status *)
                           conts : LS.t}          (* Internal cut targets *)

  val ((fiSetFxMax, fiGetFxMax)
      ,(fiSetFx, fiGetFx)
      ,(fiSetCanExit, fiGetCanExit)
      ,(fiSetExits, fiGetExits)
      ,(fiSetConts, fiGetConts)) =
      let
        val r2t = fn (FI {fxMax, fx, canExit, exits, conts}) => (fxMax, fx, canExit, exits, conts)
        val t2r = fn (fxMax, fx, canExit, exits, conts) => FI {fxMax = fxMax, fx = fx, canExit = canExit, 
                                                               exits = exits, conts = conts}
      in 
        FunctionalUpdate.mk5 (r2t, t2r)
      end

  val layoutFnInfoDataflow = 
   fn (FI {fx, exits, ...}) => L.seq [FX.layout fx, if exits then L.str ", EXITS" else L.empty]

  structure State =
  struct
    datatype t = S of {functions : fnInfo ref VD.t}

    val mk : unit -> t = fn () => S {functions = VD.empty}

    val ((setFunctions, getFunctions)) =
        let
          val r2t = fn (S {functions}) => functions
          val t2r = fn (functions) => S {functions = functions}
        in 
          FunctionalUpdate.mk1 (r2t, t2r)
        end

  end

  structure Env = 
  struct
    datatype t = E of {current : M.variable, pd : PassData.t, st : M.symbolTable}

    val mk : M.variable * PassData.t * M.symbolTable -> t = 
     fn (current, pd, symbolTable) => E {current = current, pd = pd, st = symbolTable}
                                                                         
    val ((setCurrent, getCurrent), 
         (_, getPd), 
         (_, getSt)) =
        FunctionalUpdate.mk3 (fn (E {current, pd, st}) => (current, pd, st),
                              fn (current, pd, st) => E {current = current, pd = pd, st = st})
                             
    val getConfig = PD.getConfig o getPd
    val getSi = I.SymbolInfo.SiTable o getSt
  end

  val layoutVariable : State.t * Env.t * M.variable -> Layout.t = 
   fn (state, env, v) => ML.layoutVariable (Env.getConfig env, Env.getSi env, v)

  val stringOfVariable : State.t * Env.t * M.variable -> string = Layout.toString o layoutVariable

  val setCurrentFunction : State.t * Env.t * M.variable -> Env.t = 
   fn (state, env, f) => Env.setCurrent (env, f)

  val getCurrentFunction : State.t * Env.t -> M.variable =
   fn (state, env) => Env.getCurrent env

  val hasFnInfo : State.t * Env.t * M.variable -> bool = 
   fn (state, env, f) => isSome (VD.lookup (State.getFunctions state, f))

  val getFnInfoRef : State.t * Env.t * M.variable -> fnInfo ref = 
   fn (state, env, f) => 
      (case VD.lookup (State.getFunctions state, f)
        of SOME fir => fir
         | NONE     => fail ("getFnInfoRef", "No info for function: " ^ stringOfVariable (state, env, f)))

  val getFnInfo : State.t * Env.t * M.variable -> fnInfo = ! o getFnInfoRef

  val getFnInfoRef' : State.t * Env.t * M.variable -> fnInfo ref Option.t = 
   fn (state, env, f) => VD.lookup (State.getFunctions state, f)

  val getFnInfo' : State.t * Env.t * M.variable -> fnInfo Option.t = 
   fn (state, env, v) => Option.map (getFnInfoRef' (state, env, v), !)

  val getCurrentFnInfo : State.t * Env.t -> fnInfo = 
   fn (state, env) => 
      let
        val f = getCurrentFunction (state, env)
        val fi = getFnInfo (state, env, f)
      in fi
      end

  val updateCurrentFnInfo : State.t * Env.t * fnInfo -> unit = 
   fn (state, env, fi) => 
      let
        val f = getCurrentFunction (state, env)
        val r = getFnInfoRef (state, env, f)
        val () = r := fi
      in ()
      end

  local
    val getCurrentFnInfoRecord = 
     fn (state, env) => 
        let
          val FI r = getCurrentFnInfo (state, env)
        in r
        end
  in
    val getCurrentConts : State.t * Env.t -> LS.t = #conts o getCurrentFnInfoRecord
    val getCurrentExits : State.t * Env.t -> bool = #exits o getCurrentFnInfoRecord
  end

  (* For every function, we start with the assumption that it has no
   * effects, can not exit, and has no internal cut targets.  On each 
   * iteration, we update this information based on the previously
   * computed information, and repeat until we reach a fixed point.
   * *)
    
  val addExitsToCurrent : State.t * Env.t * bool -> unit = 
   fn (state, env, callExits) => 
      if callExits then 
        let
          val fi = getCurrentFnInfo (state, env)
          val canExit = fiGetCanExit fi
          val exits = canExit 
          val fi = fiSetExits (fi, exits)
          val () = updateCurrentFnInfo (state, env, fi)
        in ()
        end
      else
        ()

  val addEffectsToCurrent : State.t * Env.t * M.effects -> unit = 
   fn (state, env, callFx) => 
      let
        val fi = getCurrentFnInfo (state, env)
        val fxMax = fiGetFxMax fi
        val fx = fiGetFx fi
        val fx = FX.union (FX.intersection (fxMax, callFx), fx)
        val fi = fiSetFx (fi, fx)
        val () = updateCurrentFnInfo (state, env, fi)
      in ()
      end

  val addContToCurrent : State.t * Env.t * M.label -> unit = 
   fn (state, env, l) => 
      let
        val fi  = getCurrentFnInfo (state, env)
        val conts = fiGetConts fi
        val conts = LS.insert (conts, l)
        val fi = fiSetConts (fi, conts)
        val () = updateCurrentFnInfo (state, env, fi)
      in ()
      end

  (* Instructions have effects.  They also may add to the set of internal
   * labels to which a cut may happen
   *)
  val analyzeInstruction : State.t * Env.t * Mil.instruction -> Env.t = 
   fn (state, env, i as M.I {rhs, ...}) => 
      let
        val () = addEffectsToCurrent (state, env, MU.Instruction.fx (Env.getConfig env, i))
        val () = 
            case rhs
             of M.RhsCont l => addContToCurrent (state, env, l)
              | _           => ()
      in env
      end

  (* For a direct call (or one with fully known targets):
   *  1) The effects of the callee are added to the effects of the caller.  
   *     The effects of the callee are the meet of the annotated and computed
   *     effects.
   *  2) If the callee can exit, and the annotation says that the call can exit,
   *     then the caller can exit.
   *)
  val analyzeDirectCallEval : State.t * Env.t * M.variable * M.effects * bool -> unit = 
   fn (state, env, f, callFx, callExits) => 
      let
        val FI {fx = calleeFx, exits = calleeExits, ...} = getFnInfo (state, env, f)
        val fx = FX.intersection (calleeFx, callFx)
        val exits = callExits andalso calleeExits
        val () = addExitsToCurrent (state, env, exits)
        val () = addEffectsToCurrent (state, env, fx)
      in ()
      end

  (* For an indirect call with unkown targets:
   *  1) The effects of the callee are added to the effects of the caller.  
   *     The effects of the callee are the annotated effects.
   *  2) If the annotation says that the call can exit,  then the caller can exit.
   *)
  val analyzeIndirectCallEval : State.t * Env.t * M.codes * M.effects * bool -> unit = 
   fn (state, env, codes, callFx, callExits) => 
      if MU.Codes.exhaustive codes then
        VS.foreach (MU.Codes.possible codes, fn f => analyzeDirectCallEval (state, env, f, callFx, callExits))
      else
        let
          val () = addExitsToCurrent (state, env, callExits)
          val () = addEffectsToCurrent (state, env, callFx)
        in ()
        end

  val analyzeCallEval : State.t * Env.t * M.variable * M.codes * M.effects * bool -> unit = 
   fn (state, env, f, codes, fx, exits) => 
      if hasFnInfo (state, env, f) then
        analyzeDirectCallEval (state, env, f, fx, exits)
      else
        analyzeIndirectCallEval (state, env, codes, fx, exits)

  val analyzeInterProc : State.t * Env.t * {callee : M.interProc, ret : M.return, fx : M.effects} -> unit = 
   fn (state, env, {callee, ret, fx}) =>
      let
        val exits = MU.Cuts.exits (MU.Return.cuts ret)

        val () = 
            case callee 
             of M.IpCall {call, ...} => 
                (case call
                  of M.CCode {ptr, code}          => analyzeCallEval (state, env, ptr, code, fx, exits)
                   | M.CClosure {cls, code}       => analyzeIndirectCallEval (state, env, code, fx, exits)
                   | M.CDirectClosure {cls, code} => analyzeDirectCallEval (state, env, code, fx, exits))
              | M.IpEval {eval, ...} => 
                (case eval
                  of M.EThunk {code, ...}       => analyzeIndirectCallEval (state, env, code, fx, exits)
                   | M.EDirectThunk {code, ...} => analyzeDirectCallEval (state, env, code, fx, exits))
      in ()
      end

  val analyzeCuts : State.t * Env.t * M.cuts -> unit = 
   fn (state, env, cuts) => 
      let
        val exits = MU.Cuts.exits cuts
        val () = addExitsToCurrent (state, env, exits)
        val () = if exits then addEffectsToCurrent (state, env, FX.FailsS) else ()
      in ()
      end

  val analyzeTransfer : State.t * Env.t * Mil.label option * Mil.transfer -> Env.t =
   fn (state, env, lo, t) => 
      let
        val () = 
            case t
             of M.TGoto _       => ()
              | M.TCase _       => ()
              | M.TInterProc ip => analyzeInterProc (state, env, ip)
              | M.TReturn _     => ()
              | M.TCut r        => analyzeCuts (state, env, #cuts r)
              | M.THalt _       => ()
      in env
      end

  val analyzeGlobal : State.t * Env.t * M.variable * M.global -> Env.t = 
   fn (state, env, v, g) =>
      (case g
        of M.GCode _ => setCurrentFunction (state, env, v)
         | _         => env)

  structure AnalyzeParam = 
  struct
    type state = State.t
    type env = Env.t
    val config : env -> Config.t = Env.getConfig
    val indent : int = 1
    val externBind         : (state * env * Mil.variable -> env) option = NONE
    val variableBind       : (state * env * Mil.variable -> env) option = NONE
    val labelBind          : (state * env * Mil.label -> env) option = NONE
    val variableUse        : (state * env * Mil.variable -> unit) option = NONE
    val analyseJump        : (state * env * Mil.label -> unit) option = NONE
    val analyseCut         : (state * env * Mil.label -> unit) option = NONE
    val analyseConstant    : (state * env * Mil.constant -> unit) option = NONE
    val analyseInstruction : (state * env * Mil.instruction -> env) option = SOME analyzeInstruction
    val analyseTransfer    : (state * env * Mil.label option * Mil.transfer -> env) option = SOME analyzeTransfer
    val analyseBlock       : (state * env * Mil.label * Mil.block -> env) option  = NONE
    val analyseGlobal      : (state * env * Mil.variable * Mil.global -> env) option = SOME analyzeGlobal
  end

  structure Analyze = MilAnalyseF(AnalyzeParam)

  val initFunctionInfo : State.t * Env.t * VS.t * M.variable * M.code -> State.t = 
   fn (state, env, recursive, f, c) => 
      let
        val M.F {fx, body, ...} = c
        val partial =
            VS.member (recursive, f) orelse
            let
              val cfg = MCFG.build (Env.getConfig env, Env.getSi env, body)
              val scc = MCFG.scc cfg
              val loops = 
               fn cc => 
                  (case cc
                    of [a] => List.contains (MCFG.succ (cfg, a), a, fn (a, b) => MCFG.compareNode (a, b) = EQUAL)
                     | _   => true)
              val partial = List.exists (scc, loops)
            in partial
            end
        (* All effects except partiality are witnessed by some instruction.  For partiality,
         * we mark each function which either is directly contained in a call graph cycle, 
         * or which contains a cycle in its control-flow-graph as partial.  Dataflow propagation
         * then does the rest.
         *)
        val fi = FI {fxMax = fx, 
                     fx = if partial then FX.intersection (fx, FX.PartialS) else FX.Total,
                     canExit = FX.contains (fx, FX.Fails), 
                     exits = false, 
                     conts = LS.empty}
        val functions = State.getFunctions state
        val functions = VD.insert (functions, f, ref fi)
        val state = State.setFunctions (state, functions)
      in state
      end


  val findRecursive = 
   fn (cg, globals) => 
      let
        val MCG.Graph.G {known, unknown, graph} = cg
        val scc = PLG.scc graph
        val hasCycle = 
         fn cc => 
            case cc
             of [a] => List.contains (PLG.Node.succs a, a, op =)
              | _   => true
        val addNode = 
         fn (n, r) =>
            (case PLG.Node.getLabel n
              of MCG.Graph.NUnknown => r
               | MCG.Graph.NFun f   => VS.insert (r, f))
        val addCC = 
         fn (r, cc) => List.fold (cc, r, addNode)
        val recursive = List.fold (scc, VS.empty, fn (cc, r) => if hasCycle cc then addCC (r, cc) else r)
      in recursive 
      end

  val init = 
   fn (state, env, cg, globals) => 
      let
        val recursive = findRecursive (cg, globals)
        val init = 
         fn (v, g, state) => 
            (case g 
              of M.GCode code => initFunctionInfo (state, env, recursive, v, code)
              | _             => state)
        val state = VD.fold (globals, state, init)
      in state
      end

  (* Has the dataflow information in the function info changed? 
   *)
  val fnInfoDataflowChanged = 
   fn (FI {fx = fx0, exits = exits0, ...}, FI {fx = fx1, exits = exits1, ...}) =>
      (exits0 <> exits1) orelse (fx0 <> fx1)

  (* Order the call graph in post order (callees before callers), 
   * and then repeatedly walk the list of functions keeping an active
   * workset intitialized to all functions.  When a functions dataflow
   * information changes, all known callers are added to the workset.
   * We iterate until the workset is empty (that is, we have reached
   * a fixed point)
   *)
  val analyzeGlobals = 
   fn (state, env, mcg, cg, entry, globals) => 
      let
        val MCG.Graph.G {known, unknown, graph} = cg
        val entryNode = valOf (VD.lookup (known, entry))
        val entries = PLG.postOrderDfs (graph, entryNode)
        val entries = 
            let
              val filter = 
               fn node =>
                  (case PLG.Node.getLabel node 
                    of MCG.Graph.NUnknown => NONE
                     | MCG.Graph.NFun f   => SOME f)
            in List.keepAllMap (entries, filter)
            end
        val getCallers = 
         fn f => 
            let
              val MCG.CG {funs, calls, callMap} = mcg
              val MCG.FI {knownCallers, ...} = valOf (VD.lookup (funs, f))
              val calls = LS.toList knownCallers
              val callers = List.map (calls, fn l => valOf (LD.lookup (callMap, l)))
              val callers = VS.fromList callers
            in callers
            end
        val lvar = fn v => layoutVariable (state, env, v)
        val step = 
         fn (ws, f) => 
            let
              val ws = VS.remove (ws, f)
              val g = MU.Globals.get (globals, f)
              val fi0 = getFnInfo (state, env, f)
              val () = Analyze.analyseGlobal (state, env, f, g)
              val fi1 = getFnInfo (state, env, f)
              val changed = fnInfoDataflowChanged (fi0, fi1)
              val ws = if changed then 
                         let
                           val () = if showDataflow (Env.getPd env) then 
                                      LU.printLayout (L.seq [lvar f, L.str " : ", layoutFnInfoDataflow fi0,
                                                             L.str " -> ", layoutFnInfoDataflow fi1])
                                    else
                                      ()
                         in VS.union (getCallers f, ws)
                         end
                       else
                         ws
            in ws
            end
        val all = entries
        val reachable = List.fold (all, VS.empty, fn (v, vs) => VS.insert (vs, v))
        val rec iterate = 
         fn (ws, fns) => 
            if VS.isEmpty ws then ()
            else
              case fns 
               of []     => 
                  let
                    val () = if debugPass (Env.getPd env) then
                               LU.printLayout (L.seq[L.str " Iterating with workset -> ", VS.layout (ws, lvar)])
                             else ()
                  (* There may be unreachable functions that are in the workset but not in 
                   * the ordering, so be careful to trim out here *)
                  in iterate (VS.intersection (reachable, ws), all)
                  end
                | f::fns => 
                  let
                    val ws = if VS.member (ws, f) then step (ws, f) else ws
                    val () = 
                        if showWorkset (Env.getPd env) then
                          LU.printLayout (L.seq[lvar f, L.str " -> ", VS.layout (ws, lvar)])
                        else ()
                  in iterate (ws, fns)
                  end
      in iterate (reachable, all)
      end

  val getInterProcDataflowInfo = 
   fn (state, env, callee) => 
      let
        val (calleeFx, calleeExits) =
            let
              val getDirectInfo = 
               fn f => 
                  let
                    val (FI {fx, exits, ...}) = getFnInfo (state, env, f)
                  in (fx, exits)
                  end

              val getIndirectInfo = 
                  fn codes => 
                     let
                       val add = 
                        fn (f, (fx0, exits0)) => 
                           let
                             val (fx1, exits1) = getDirectInfo f
                           in (FX.union (fx0, fx1), exits0 orelse exits1)
                           end
                     in 
                       if MU.Codes.exhaustive codes then 
                         VS.fold (MU.Codes.possible codes, (FX.Total, false), add)
                       else
                         (FX.Any, true)
                     end

              val getInfo = 
               fn (f, codes) => 
                  if hasFnInfo (state, env, f) then 
                    getDirectInfo f
                  else 
                    getIndirectInfo codes
            in 
              case callee
               of M.IpCall {call, args} => 
                  (case call
                    of M.CCode {ptr, code}          => getInfo (ptr, code)
                     | M.CClosure {cls, code}       => getIndirectInfo code
                     | M.CDirectClosure {cls, code} => getDirectInfo code)
                | M.IpEval {eval, typ} => 
                  (case eval
                    of M.EThunk {code, ...}       => getIndirectInfo code
                     | M.EDirectThunk {code, ...} => getDirectInfo code)
            end
      in (calleeFx, calleeExits)
      end

  val rewriteInterProc = 
   fn (state, env, {callee, ret, fx}) => 
      let
        val (calleeFx, calleeExits) = getInterProcDataflowInfo (state, env, callee)
        val fx = FX.intersection (calleeFx, fx)
        val callerExits = getCurrentExits (state, env)
        (* The call can only exit if all of the below are true:
         *  1) The caller can exit
         *  2) The callee can exit (inferred)
         *  3) The callee can exit (annotated)
         * *)
        val ret = 
            case ret
             of M.RNormal {rets, block, cuts} => 
                let
                  val exits = callerExits andalso calleeExits andalso (MU.Cuts.exits cuts)
                  val localConts = getCurrentConts (state, env)
                  val targets = if calleeExits then 
                                  LS.intersection (MU.Cuts.targets cuts, localConts)
                                else
                                  LS.empty
                in M.RNormal {rets = rets, block = block, cuts = M.C {exits = exits, targets = targets}}
                end
              | M.RTail {exits} => 
                let
                  val exits = callerExits andalso calleeExits andalso exits
                in M.RTail {exits = exits}
                end
        val ans = {callee = callee, ret = ret, fx = fx}
      in ans
      end

  val rewriteCuts = 
   fn (state, env, {cont, args, cuts}) =>
      let
        val exits = getCurrentExits (state, env) andalso MU.Cuts.exits cuts
        val targets = LS.intersection (getCurrentConts (state, env), MU.Cuts.targets cuts)
        val cuts = M.C {exits = exits, targets = targets}
        val r = {cont = cont, args = args, cuts = cuts}
      in r
      end

  val rewriteTransfer = 
   fn (state, env, (lo, t)) => 
      let
        val ans = 
            case t
             of M.TGoto _       => MRC.Stop
              | M.TCase _       => MRC.Stop
              | M.TInterProc ip => MRC.StopWith (env, (NONE, M.TInterProc (rewriteInterProc (state, env, ip))))
              | M.TReturn _     => MRC.Stop
              | M.TCut r        => MRC.StopWith (env, (NONE, M.TCut (rewriteCuts (state, env, r))))
              | M.THalt _       => MRC.Stop
      in ans
      end

  val rewriteGlobal = 
   fn (state, env, (v, g)) => 
      let
        val ans = 
            case g
             of M.GCode code => 
                let
                  val M.F {fx, escapes, recursive, cc, args, rtyps, body} = code
                  val FI {fx = fxI, exits = exitsI, ...} = getFnInfo (state, env, v)
                  val fx = FX.intersection (fx, fxI)
                  val fx = if exitsI then fx else FX.remove (fx, FX.Fails)
                  val code = M.F {fx = fx, escapes = escapes, recursive = recursive, 
                                  cc = cc, args = args, rtyps = rtyps, body = body}
                  val g = M.GCode code
                in MRC.ContinueWith (setCurrentFunction (state, env, v), (v, g))
                end
              | _            => MRC.Stop
      in ans
      end

  structure Rewrite = MilRewriterF (struct
                                     type env   = Env.t
                                     type state = State.t
                                     val config      = Env.getConfig
                                     val label       = fn _ => MRC.Stop
                                     val variable    = fn _ => MRC.Stop
                                     val operand     = fn _ => MRC.Stop
                                     val instruction = fn _ => MRC.Stop
                                     val transfer    = rewriteTransfer
                                     val block       = fn _ => MRC.Continue
                                     val global      = rewriteGlobal
                                     val bind        = fn (_, env, _) => (env, NONE)
                                     val bindLabel   = fn (_, env, _) => (env, NONE)
                                     val indent      = 2
                                     val cfgEnum     = fn (_, _, t) => MilUtils.CodeBody.dfsTrees t
                                    end)
               
  val analyze = 
   fn (pd, m) => 
      let
        val M.P {includes, externs, globals, symbolTable, entry} = m
        val () = Chat.log1 (pd, "Doing analysis")
        val s = Time.now ()
        val env = Env.mk (entry, pd, symbolTable)
        val mcg = MCG.program (Env.getConfig env, Env.getSi env, m)
        val cg = MCG.Graph.make mcg
        val state = State.mk ()
        val state = init (state, env, cg, globals)
        val () = analyzeGlobals (state, env, mcg, cg, entry, globals)
        val e = Time.toString (Time.- (Time.now (), s))
        val () = Chat.log1 (pd, "Done with analysis in "^e^"s")
      in (state, env)
      end

  val optimize = 
   fn (state, env, m) => 
      let
          val () = Chat.log1 (Env.getPd env, "Rewriting program")
          val s = Time.now ()
          val m = Rewrite.program (state, env, m)
          val e = Time.toString (Time.- (Time.now (), s))
          val () = Chat.log1 (Env.getPd env, "Done with rewriting in "^e^"s")
      in m
      end

  fun program (m, pd) = 
      let
        val (state, env) = analyze (pd, m)
        val m = optimize (state, env, m)
        val () = PD.report (pd, passname)
      in m
      end

  val description = {name        = passname,
                     description = "Effect Analysis",
                     inIr        = BothMil.irHelpers,
                     outIr       = BothMil.irHelpers,
                     mustBeAfter = [],
                     stats       = stats}

  val associates = {controls  = [],
                    debugs    = debugs,
                    features  = [],
                    subPasses = []}

  val pass =
      Pass.mkOptPass (description, associates, BothMil.mkMilPass program)

end;
