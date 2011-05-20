(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* Compute the call graph for Mil.  Namely:
 *   * For each code global, identified by their binding variable, compute:
 *     * Does it have unknown callers
 *     * What callers are known to call it
 *   * For each call, tailcall, or eval, identified by the label of the block
 *     for which they are the transfer, compute:
 *     * Does it have unknown callees
 *     * What callees are known to be called from it
 *)

signature MIL_CALL_GRAPH =
sig

  datatype funInfo = FI of {
           knownCallers   : Identifier.LabelSet.t,
           unknownCallers : bool
  }

  datatype callInfo = CI of {
           knownCallees   : Identifier.VariableSet.t,
           unknownCallees : bool
  }

  datatype callGraph = CG of {
    funs  : funInfo Identifier.VariableDict.t,
    calls : callInfo Identifier.LabelDict.t,
    callMap : Mil.variable Identifier.LabelDict.t
  }

  val program : Config.t * Mil.symbolInfo * Mil.t -> callGraph

  val layout : Config.t * Mil.symbolInfo * callGraph -> Layout.t
  val layoutDot : callGraph *
                  {nodeOptions : Mil.variable * bool (* unknown callers *) -> Layout.t list,
                   edgeOptions : Mil.label (* call *) *
                                 Mil.variable (*src *) *
                                 Mil.variable option (* target *) * 
                                 bool (* virtual *) -> Layout.t list,
                   graphTitle : string}
                  -> Layout.t

  structure Graph :
  sig

    datatype node = NUnknown | NFun of Mil.variable

    datatype t = G of {
      unknown : (node, unit) PolyLabeledGraph.node,
      graph   : (node, unit) PolyLabeledGraph.t
    }

    (* Directed multi-graph.  Edges point from callers to callees *)
    val make : callGraph -> t
    val layoutDot : Config.t * Mil.symbolInfo * t -> Layout.t

  end

end;

structure MilCallGraph :> MIL_CALL_GRAPH =
struct

  structure I = Identifier
  structure LD = I.LabelDict
  structure LS = I.LabelSet
  structure VD = I.VariableDict
  structure VS = I.VariableSet
  structure M = Mil
  structure MU = MilUtils

  val passname = "MilCallGraph"
  val fail = 
   fn (f, msg) => Fail.fail (passname, f, msg)

  datatype funInfo = FI of {
           knownCallers   : Identifier.LabelSet.t,
           unknownCallers : bool
  }

  datatype callInfo = CI of {
           knownCallees   : Identifier.VariableSet.t,
           unknownCallees : bool
  }

  datatype callGraph = CG of {
    funs    : funInfo Identifier.VariableDict.t,
    calls   : callInfo Identifier.LabelDict.t, 
    callMap : Mil.variable Identifier.LabelDict.t
  }

  local
    open Layout
    structure LU = LayoutUtils
    val layoutVariable = MilLayout.layoutVariable
    val layoutLabel = MilLayout.layoutLabel
  in

  fun layoutCallName (config, si, callMap, l) = 
      (case LD.lookup (callMap, l)
        of SOME f => seq [layoutVariable (config, si, f), str ".", layoutLabel (config, si, l)]
         | NONE => seq [str "BAD FUNCTION NAME", str ".", layoutLabel (config, si, l)])

  fun layoutFunEntry (config, si, callMap, (f, FI {knownCallers, unknownCallers})) =
      let
        val caller = fn l => layoutCallName (config, si, callMap, l)
        val callee = layoutVariable (config, si, f)
        val known = List.map (LS.toList knownCallers, caller)
        val all = if unknownCallers then str "unknown"::known else known
        val calls = sequence ("{", "}", ",") all
        val l = mayAlign [seq [callee, str " is called by "], LU.indent calls]
      in l
      end

  fun layoutCallEntry (config, si, callMap, (l, CI {knownCallees, unknownCallees})) =
      let
        val caller = layoutCallName (config, si, callMap, l)
        val callee = fn v => layoutVariable (config, si, v)
        val known = List.map (VS.toList knownCallees, callee)
        val all = if unknownCallees then str "unknown"::known else known
        val calls = sequence ("{", "}", ",") all
        val l = mayAlign [seq [caller, str " calls "], LU.indent calls]
      in l
      end

  fun layout (config, si, CG {funs, calls, callMap}) =
      let
        val var = fn v => Layout.toString (layoutVariable (config, si, v))
        fun doFun entry = layoutFunEntry (config, si, callMap, entry)
        fun doCall entry = layoutCallEntry (config, si, callMap, entry)
      in
        align [str "Funs:",
               LU.indent (align (List.map (VD.toList funs, doFun))),
               str "Calls:",
               LU.indent (align (List.map (LD.toList calls, doCall)))]
      end

  (* Layout the call graph in the dot format. 
   * EB XXX: I'm still changing this function. Please, do not rely on it. *)
  fun layoutDot (CG {funs, calls, callMap}, {nodeOptions, edgeOptions, graphTitle}) =
      let
        (* Convert calls to edges.  An edge consistes of callsite, target funtion option, 
         * and virtual call - true if call more than one function.
         *)
        fun callToEdges (c, CI {knownCallees, unknownCallees, ...}) =  
            let
              val known = VS.toList knownCallees
              val virtualCall = List.length known > 1 orelse unknownCallees
              val srcFun = Option.valOf (LD.lookup (callMap, c))
              fun doOne tgtFun = (c, srcFun, SOME tgtFun, virtualCall)
              val known = List.map (known, doOne)
            in
              if unknownCallees then
                (c, srcFun, NONE, virtualCall)::known
              else
                known
            end
        val edges = List.concatMap (LD.toList calls, callToEdges)
        (* Layout the Edges *)
        fun layoutAnnotatedEdge (call, srcFun, tgtFun, virtualCall) = 
            let
              val srcFun' = (case LD.lookup (callMap, call)
                             of SOME f => I.variableString' f
                              | NONE => "BAD FUNCTION NAME")
              val tgtFun' = case tgtFun
                             of SOME f => I.variableString' f
                              | NODE => "unknown"
              val edgeAttributes = edgeOptions (call, srcFun, tgtFun, virtualCall)
            in
              seq [str (srcFun'), str (" -> "), 
                   str (tgtFun'), str " [", 
                   seq (separate (edgeAttributes, ", ")),
                   str ("];") ]
            end
        val edges = align ((str "/* Edges */")::(List.map (edges, layoutAnnotatedEdge)))
        (* Layout the nodes *)
        fun layoutAnnotatedNode (f, FI {unknownCallers, ...}) = 
            let 
              val id = I.variableString' f
              val nodeOptions = nodeOptions (f, unknownCallers)
            in
              seq [str (id), str "[", 
                   seq (separate (nodeOptions, ", ")), 
                   str "];"]
            end
        val nodes = align ((str "/* Nodes */")::(List.map (VD.toList funs, layoutAnnotatedNode)))
        (* The result *)
        val graphOptions =
            align [str ("/* Options */"), 
                   str ("size=\"8.5, 11\""),
                   str ("orientation=landscape"), 
                   str ("label=\"" ^ graphTitle ^ "\"")]
        val body = align [graphOptions, nodes, edges]
        val l = align [str ("digraph \"" ^ graphTitle ^ "\" {"), LU.indent body, str ("}\n")]
      in l
      end

  end


  datatype env = E of {
    config   : Config.t,
    si       : M.symbolInfo,
    code     : VS.t,
    curFun   : M.variable option,
    curBlock : M.label option
  }

  fun envInit (config, si, code) =
      E {config = config, si = si, code = code, curFun = NONE, curBlock = NONE}

  fun getConfig (E {config, ...}) = config

  fun isCode (E {code, ...}, v) = VS.member (code, v)

  fun getFun (E {curFun, ...}) =
      case curFun
       of NONE => fail ("getFun", "not in a function")
        | SOME f => f

  fun getBlock (E {curBlock, ...}) =
      case curBlock
       of NONE => fail ("getBlock", "not in a block")
        | SOME l => l

  fun setFun (E {config, si, code, curFun = _, curBlock}, f) =
      E {config = config, si = si, code = code, curFun = SOME f,
         curBlock = curBlock}

  fun setBlock (E {config, si, code, curFun, curBlock = _}, l) =
      E {config = config, si = si, code = code, curFun = curFun,
         curBlock = SOME l}


  datatype state = S of {
    funs  : (LS.t ref * bool) VD.t ref,  (* (callers, escapes) *)
    calls : callInfo LD.t ref,
    callMap : M.variable LD.t ref
  }

  fun mkState () = S {funs = ref VD.empty, calls = ref LD.empty, callMap = ref LD.empty}

  fun addCallToCallMap (S {callMap, ...}, e, cl) = 
      let
        val fv = getFun e
        val () = callMap := LD.insert (!callMap, cl, fv)
      in ()
      end

  fun addFun (S {funs, ...}, f, escapes) =
      case VD.lookup (!funs, f)
       of NONE =>
          let
            val fi = (ref LS.empty, escapes)
            val () = funs := VD.insert (!funs, f, fi)
          in ()
          end
        | SOME (callers, escapes') =>
          let
            val escapes = escapes' orelse escapes
            val () = funs := VD.insert (!funs, f, (callers, escapes))
          in ()
          end

  fun addCallerCalleeInfo (S {funs, calls, ...}, label, knownCallees, unknownCallees) = 
      let
        val ci = CI {knownCallees = knownCallees, unknownCallees = unknownCallees}
        val () = calls := LD.insert (!calls, label, ci)
        val addCaller = 
         fn f => 
            (case VD.lookup (!funs, f)
              of NONE =>
                 let
                   val fi = (ref (LS.singleton label), false)
                   val () = funs := VD.insert (!funs, f, fi)
                 in ()
                 end
               | SOME (callers, _) => callers := LS.insert (!callers, label))
        val () = VS.foreach (knownCallees, addCaller)
      in ()
      end

  fun addKnownCall (s, c, f) = addCallerCalleeInfo (s, c, VS.singleton f, false)
  fun addUnknownCall (s, c) = addCallerCalleeInfo (s, c, VS.empty, false)
  fun addCodes (s, c, {possible, exhaustive}) = addCallerCalleeInfo (s, c, possible, not exhaustive)

  fun analyseCodeCall (s, e, cl, codeVar) =
      if isCode (e, codeVar) then
        addKnownCall (s, cl, codeVar)
      else
        addUnknownCall (s, cl)

  fun analyseCall (s, e, cl, call) = 
      (case call
        of M.CCode {ptr, ...} => analyseCodeCall (s, e, cl, ptr)
         | M.CClosure {code, ...} => addCodes (s, cl, code)
         | M.CDirectClosure {code, ...} => addKnownCall (s, cl, code))

  fun analyseEval (s, e, cl, eval) = 
      (case eval
        of M.EThunk {code, ...} => addCodes (s, cl, code)
         | M.EDirectThunk {code, ...} => addKnownCall (s, cl, code))

  fun analyseInterProc (s, e, cl, callee) = 
      (case callee
        of M.IpCall {call, ...} => analyseCall (s, e, cl, call)
         | M.IpEval {typ, eval} => analyseEval (s, e, cl, eval))

  fun analyseTransfer (s, e, t) =
      case t
       of M.TInterProc {callee, ret, fx} =>
          let
            val cl = getBlock e
            val () = addCallToCallMap (s, e, cl)
            val () = analyseInterProc (s, e, cl, callee)
          in e
          end
        | _ => e

  fun analyseBlock (s, e, l, _) = setBlock (e, l)

  fun analyseGlobal (s, e, v, g) =
      case g
       of M.GCode (M.F {escapes, ...}) =>
          let
            val () = addFun (s, v, escapes)
          in setFun (e, v)
          end
        | _ => e

  structure MA = MilAnalyseF (type env = env
                              type state = state
                              val config = getConfig
                              val indent = 2
                              val externBind = NONE
                              val variableBind = NONE
                              val labelBind = NONE
                              val variableUse = NONE
                              val analyseJump = NONE
                              val analyseCut = NONE
                              val analyseConstant = NONE
                              val analyseInstruction = NONE
                              val analyseTransfer = SOME analyseTransfer
                              val analyseBlock = SOME analyseBlock
                              val analyseGlobal = SOME analyseGlobal)

  fun finish (S {funs, calls, callMap}) =
      let
        fun doOne (_, (cs, e)) =
            FI {knownCallers = !cs, unknownCallers = e}
        val funs = VD.map (!funs, doOne)
      in
        CG {funs = funs, calls = !calls, callMap = !callMap}
      end

  fun program (config, si, p as M.P {globals, ...}) =
      let
        fun doOne (v, g, code) =
            case g
             of M.GCode _ => VS.insert (code, v)
              | _ => code
        val code = VD.fold (globals, VS.empty, doOne)
        val env = envInit (config, si, code)
        val state = mkState ()
        val () = MA.analyseProgram (state, env, p)
      in
        finish state
      end

  structure Graph =
  struct

    structure PLG = PolyLabeledGraph

    datatype node = NUnknown | NFun of Mil.variable

    datatype t = G of {
      unknown : (node, unit) PLG.node,
      graph   : (node, unit) PLG.t
    }

    fun make (CG {funs, calls, callMap}) =
        let
          val nodes = NUnknown::(List.map (VD.toList funs, fn (f, _) => NFun f))
          fun node (l, n, (unknown, funMap)) =
              case l
               of NUnknown => (SOME n, funMap)
                | NFun f => (unknown, VD.insert (funMap, f, n))
          fun edges (unknown, funMap) =
              let
                val unknown = Option.valOf unknown
                fun funNode f = Option.valOf (VD.lookup (funMap, f))
                fun doFun (f, FI {unknownCallers, ...}) =
                    if unknownCallers then
                      SOME (unknown, funNode f, ())
                    else
                      NONE
                val es1 = List.keepAllMap (VD.toList funs, doFun)
                fun doCall (c, CI {knownCallees, unknownCallees}) =
                    let
                      val inFun = Option.valOf (LD.lookup (callMap, c))
                      val inFun = funNode inFun
                      fun doOne f = (inFun, funNode f, ())
                      val es = List.map (VS.toList knownCallees, doOne)
                      val es = if unknownCallees then (inFun, unknown, ())::es else es
                    in es
                    end
                val es2 = List.concat (List.map (LD.toList calls, doCall))
              in es1 @ es2
              end
          val (g, (unknown, _)) = PLG.new {nodes = nodes, init = (NONE, VD.empty), node = node, edges = edges}
          val g = G {unknown = Option.valOf unknown, graph = g}
        in g
        end

    fun layoutDot (c, si, G {unknown, graph}) =
        let
          fun nodeName n =
              case PLG.Node.getLabel n
               of NUnknown => Layout.str "The Unknown Function"
                | NFun f   => MilLayout.layoutVariable (c, si, f)
          val l = PLG.layoutDot (graph, {nodeName = nodeName, graphTitle = Layout.str "Mil Call Graph"})
        in l
        end

  end

end;
