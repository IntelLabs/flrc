(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, July 2007 *)

(* Compute the call graph for Mil.  Namely:
 *   * For each code global, identified by their binding variable, compute:
 *     * Does it have unknown callers
 *     * What callers are known to call it
 *   * For each call, tailcall, or eval, identified by the label of the block
 *     for which they are the transfer, compute:
 *     * Does it have unknown callees
 *     * What callees are known to be called from it
 *)

signature MilCallGraph =
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
    calls : callInfo Identifier.LabelDict.t
  }

  val program : Config.t * Mil.symbolInfo * Mil.t -> callGraph

  val layout : Config.t * Mil.symbolInfo * callGraph -> Layout.t
  val layoutDot : callGraph *
                  FMil.t *
                  {nodeOptions : Mil.variable * bool (* unknown callers *) -> Layout.t,
                   edgeOptions : Mil.label * Mil.variable option (* target *) * bool (* virtual *) -> Layout.t,
                   graphTitle : Layout.t}
                  -> Layout.t

  structure Graph :
  sig

    datatype node = NUnknown | NFun of Mil.variable

    datatype t = G of {
      unknown : (node, unit) PolyLabeledGraph.node,
      graph   : (node, unit) PolyLabeledGraph.t
    }

    val make : callGraph * FMil.t -> t
    val layoutDot : Config.t * Mil.symbolInfo * t -> Layout.t

  end

end;

structure MilCallGraph =
struct

  structure I = Identifier
  structure LD = I.LabelDict
  structure LS = I.LabelSet
  structure VD = I.VariableDict
  structure VS = I.VariableSet
  structure M = Mil
  structure MU = MilUtils

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
       of NONE => Fail.fail ("MilCallGraph", "getFun", "not in a function")
        | SOME f => f

  fun getBlock (E {curBlock, ...}) =
      case curBlock
       of NONE => Fail.fail ("MilCallGraph", "getBlock", "not in a block")
        | SOME l => l

  fun setFun (E {config, si, code, curFun = _, curBlock}, f) =
      E {config = config, si = si, code = code, curFun = SOME f,
         curBlock = curBlock}

  fun setBlock (E {config, si, code, curFun, curBlock = _}, l) =
      E {config = config, si = si, code = code, curFun = curFun,
         curBlock = SOME l}

  structure Chat = ChatF (type env = env
                          val extract = getConfig
                          val name = "MilCallGraph"
                          val indent = 0)

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
    calls : callInfo Identifier.LabelDict.t
  }

  local
    open Layout
    structure LU = LayoutUtils
    val layoutVariable = MilLayout.layoutVariable
  in

  fun layoutFunInfo (FI {knownCallers, unknownCallers, ...}) =
      let
        val known = List.map (LS.toList knownCallers, I.labelString)
        val all = if unknownCallers then "unknown"::known else known
        val l = sequence ("{", "}", ",") (List.map (all, str))
      in l
      end

  fun layoutCallInfo (c, si, CI {knownCallees, unknownCallees, ...}) =
      let
        val var = fn v => Layout.toString (layoutVariable (c, si, v))
        val known = List.map (VS.toList knownCallees, var)
        val all = if unknownCallees then "unknown"::known else known
        val l = sequence ("{", "}", ",") (List.map (all, str))
      in l
      end

  fun layout (c, si, CG {funs, calls, ...}) =
      let
        val var = fn v => Layout.toString (layoutVariable (c, si, v))
        fun doFun (f, fi) = mayAlign [seq [str (var f), str ":"], LU.indent (layoutFunInfo fi)]
        fun doCall (cl, ci) = mayAlign [seq [str (I.labelString cl), str ":"], LU.indent (layoutCallInfo (c, si, ci))]
      in
        align [str "Funs:",
               LU.indent (align (List.map (VD.toList funs, doFun))),
               str "Calls:",
               LU.indent (align (List.map (LD.toList calls, doCall)))]
      end

  (* Layout the call graph in the dot format. 
   * EB XXX: I'm still changing this function. Please, do not rely on it. *)
  fun layoutAnnotatedDot (CG {funs, calls, ...}, fmil, {nodeOptions, edgeOptions, graphTitle}) =
      let
        (* Convert calls to edges.  An edge consistes of callsite, target funtion option, 
         * and virtual call - true if call more than one function.
         *)
        fun callToEdges (c, CI {knownCallees, unknownCallees, ...}) =  
            let
              val known = VS.toList knownCallees
              val virtualCall = List.length known > 1 orelse unknownCallees
              fun doOne tgtFun = (c, SOME tgtFun, virtualCall)
              val known = List.map (known, doOne)
            in
              if unknownCallees then
                (c, NONE, virtualCall)::known
              else
                known
            end
        val edges = List.concatMap (LD.toList calls, callToEdges)
        (* Layout the Edges *)
        fun layoutAnnotatedEdge (call, tgtFun, virtualCall) = 
            let
              val srcFun = FMil.getLabelFun (fmil, call)
              val srcFun' = I.variableString' srcFun
              val tgtFun' = case tgtFun
                             of SOME f => I.variableString' f
                              | NODE => "unknown"
              val edgeAttributes = edgeOptions (call, tgtFun, virtualCall)
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

  datatype state = S of {
    funs  : (LS.t ref * bool) VD.t ref,
    calls : callInfo LD.t ref
  }

  fun mkState () = S {funs = ref VD.empty, calls = ref LD.empty}

  fun addFun (S {funs, ...}, f, escapes) =
      case VD.lookup (!funs, f)
       of NONE =>
          let
            val fi = (ref LS.empty, escapes)
            val () = funs := VD.insert (!funs, f, fi)
          in ()
          end
        | SOME (callees, escapes') =>
          let
            val escapes = escapes' orelse escapes
            val () = funs := VD.insert (!funs, f, (callees, escapes))
          in ()
          end

  fun addKnownCall (S {funs, calls, ...}, c, f) =
      let
        val ci = CI {knownCallees = VS.singleton f, unknownCallees = false}
        val () = calls := LD.insert (!calls, c, ci)
        val () =
            case VD.lookup (!funs, f)
             of NONE =>
                let
                  val fi = (ref (LS.singleton c), false)
                  val () = funs := VD.insert (!funs, f, fi)
                in ()
                end
              | SOME (callers, _) => callers := LS.insert (!callers, c)
      in ()
      end

  fun addUnknownCall (S {calls, ...}, c) =
      let
        val ci = CI {knownCallees = VS.empty, unknownCallees = true}
        val () = calls := LD.insert (!calls, c, ci)
      in ()
      end

  fun analyseCodeCall (s, e, call, codeVar) =
      if isCode (e, codeVar) then
        addKnownCall (s, call, codeVar)
      else
        addUnknownCall (s, call)

  fun analyseTransfer (s, e, t) =
      case t
       of M.TInterProc {callee, ret, fx} =>
          let
            val cl = getBlock e
            val () =
                case callee
                 of M.IpCall {call, ...} =>
                    (case call
                      of M.CCode cv => analyseCodeCall (s, e, cl, cv)
                       | M.CClosure _ => addUnknownCall (s, cl)
                       | M.CDirectClosure {code, ...} => analyseCodeCall (s, e, cl, code))
                  | M.IpEval {typ, eval} =>
                    (case eval
                      of M.EThunk _ => addUnknownCall (s, cl)
                       | M.EDirectThunk {code, ...} => analyseCodeCall (s, e, cl, code))
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
                              val variableBind = NONE
                              val variableUse = NONE
                              val analyseJump = NONE
                              val analyseCut = NONE
                              val analyseConstant = NONE
                              val analyseInstruction = NONE
                              val analyseTransfer = SOME analyseTransfer
                              val analyseBlock = SOME analyseBlock
                              val analyseGlobal = SOME analyseGlobal)

  fun finish (S {funs, calls, ...}) =
      let
        fun doOne (_, (cs, e)) =
            FI {knownCallers = !cs, unknownCallers = e}
        val funs = VD.map (!funs, doOne)
      in
        CG {funs = funs, calls = !calls}
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

    fun make (CG {funs, calls}, fmil) =
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
                fun doCall (c, CI {knownCallees, unknownCallees, ...}) =
                    let
                      val inFun = FMil.getLabelFun (fmil, c)
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
