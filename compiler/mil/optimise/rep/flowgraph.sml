(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_REP_FLOW_GRAPH = 
sig
  type 'data t
  type node = MilRepNode.node
  val add : 'data t * node * 'data -> unit
  val propagate : 'data t -> unit
  val query : 'data t * node -> 'data
  (* 'data must be a lattice with only finite upward chains.
   * forward: forward flow or backward?
   * merge:
   *   must return NONE if no change (required for termination)
   *   should be associative and commutative 
   *)
  val build : {pd : PassData.t,
               forward : bool,
               summary : MilRepSummary.summary,
               uDefInit : 'data,
               uUseInit : 'data,
               initialize : node -> 'data,
               merge : 'data * 'data -> 'data,
               equal : 'data * 'data -> bool} -> 'data t
end (* signature MIL_REP_FLOW_GRAPH *)

structure MilRepFlowGraph :> MIL_REP_FLOW_GRAPH = 
struct

  val fail = fn (f, m) => Fail.fail ("flowgraph.sml", f, m)

  structure ID = IntDict
  structure MRS = MilRepSummary
  structure MRN = MilRepNode

  structure IPLG = ImpPolyLabeledGraph

  type node = MilRepNode.node

  datatype 'data nodeLabel = NL of {data : 'data}
       and 'data t = FG of {pd : PassData.t,
                            graph : 'data graph,
                            classNodes : 'data graphNode ID.t,
                            merge : 'data * 'data -> 'data,
                            equal : 'data * 'data -> bool}
  withtype 'data graph = ('data nodeLabel, unit) IPLG.t
       and 'data graphNode = ('data nodeLabel, unit) IPLG.node

  structure NodeLabel = 
  struct

    val r2t = 
     fn (NL {data}) => data
    val t2r = 
     fn data => NL {data = data}

    val setData = fn nl => #1 (FunctionalUpdate.mk1 (r2t, t2r)) nl
    val data = fn nl => #2 (FunctionalUpdate.mk1 (r2t, t2r)) nl

    val new = 
     fn data => NL {data = data}
  end
  
  structure FlowGraph = 
  struct
    val r2t = 
     fn (FG {pd, graph, classNodes, merge, equal}) => (pd, graph, classNodes, merge, equal)
    val t2r = 
     fn  (pd, graph, classNodes, merge, equal) => FG {pd = pd, graph = graph, classNodes = classNodes, 
                                                      merge = merge, equal =equal}

    val pd = fn fg => (#2 o #1) (FunctionalUpdate.mk5 (r2t, t2r)) fg
    val graph = fn fg => (#2 o #2) (FunctionalUpdate.mk5 (r2t, t2r)) fg
    val classNodes = fn fg => (#2 o #3) (FunctionalUpdate.mk5 (r2t, t2r)) fg
    val merge = fn fg => (#2 o #4) (FunctionalUpdate.mk5 (r2t, t2r)) fg
    val equal = fn fg => (#2 o #5) (FunctionalUpdate.mk5 (r2t, t2r)) fg
  end (* structure FlowGraph *)

  val getDataForClassNode = 
   fn classNode => 
      let
        val label = IPLG.Node.getLabel classNode
        val cd = NodeLabel.data label
      in cd
      end

  val setDataForClassNode = 
   fn (classNode, cd) => 
      let
        val label = IPLG.Node.getLabel classNode
        val label = NodeLabel.setData (label, cd)
        val () = IPLG.Node.setLabel (classNode, label)
      in ()
      end

  val build =
   fn {pd, forward, summary, uDefInit, uUseInit, initialize, merge, equal} =>
      let
        val g = IPLG.new ()
        val uDef = IPLG.newNode (g, NodeLabel.new uDefInit)
        val uUse = IPLG.newNode (g, NodeLabel.new uUseInit)
        val addEdge = 
         fn (from, to) => 
            let
              val (from, to) = if forward then 
                                 (from, to)
                               else 
                                 (to, from)
              val edge = IPLG.addEdge (g, from, to, ())
            in ()
            end

        val help = 
         fn (id, n, classNodes) => 
            let
              val classId = MRN.classId n
              val data = initialize n
              val classNodes = 
                  (case ID.lookup (classNodes, classId)
                    of SOME classNode => 
                       let
                         val cd = getDataForClassNode classNode
                         val cd = merge (data, cd)
                         val () = setDataForClassNode (classNode, cd)
                       in classNodes
                       end
                     | NONE => 
                       let
                         val classNode = IPLG.newNode (g, NodeLabel.new data)
                         val classNodes = ID.insert (classNodes, classId, classNode)
                         val () = if MRN.defsKnown n then () else addEdge (uDef, classNode)
                         val () = if MRN.usesKnown n then () else addEdge (classNode, uUse)
                       in classNodes
                       end)
            in classNodes
            end
        val classNodes = ID.fold (MRS.nodes summary, ID.empty, help)
        val edges = MRS.edges summary
        val () = 
            let
              val cn = fn a => valOf (ID.lookup (classNodes, MRN.classId a))
            in List.foreach (edges, (fn (a, b) => addEdge (cn a, cn b)))
            end
        val fg = FG {pd = pd, graph = g, classNodes = classNodes, merge = merge, equal = equal}
      in fg
      end

  val classNodeForNode = 
   fn (fg, n) => 
      (case ID.lookup (FlowGraph.classNodes fg, MRN.classId n)
        of SOME classNode => classNode
         | NONE => fail ("classNodeForNode", "Node has no class node in graph"))

  val add : 'data t * node * 'data -> unit =
   fn (fg, n, d) => 
      let
        val classId = MRN.classId n
        val classNode = classNodeForNode (fg, n)
        val cd = getDataForClassNode classNode
        val cd = FlowGraph.merge fg (d, cd)
        val () = setDataForClassNode (classNode, cd)
      in ()
      end

  val propagate0 = 
   fn (fg, classNode) => 
      let
        val graph = FlowGraph.graph fg
        val merge = FlowGraph.merge fg
        val equal = FlowGraph.equal fg
        val cd = getDataForClassNode classNode
        val preds = IPLG.Node.preds classNode
        val datum = List.map (preds, getDataForClassNode)
        val cd' = List.fold (datum, cd, merge)
        val changed = not (equal (cd, cd'))
        val () = if changed then
                   setDataForClassNode (classNode, cd')
                 else
                   ()
      in changed
      end
  val propagate1 = 
   fn (fg, component) =>
      let
        val help = fn n => propagate0 (fg, n)
        val rec loop =  
         fn () => if List.exists (component, help) then
                    loop ()
                  else
                    ()
      in loop ()
      end
  val propagate2 = fn (fg, components) => List.foreach (components, fn component => propagate1 (fg, component))

  val propagate = 
   fn fg => 
      let
        val graph = FlowGraph.graph fg
        val components = IPLG.scc graph
        val () = propagate2 (fg, components)
      in ()
      end

  val query = 
   fn (fg, n) => 
      let
        val classId = MRN.classId n
        val classNode = classNodeForNode (fg, n)
        val label = IPLG.Node.getLabel classNode
        val cd = NodeLabel.data label
      in cd
      end


end (* structure MilRepFlowGraph *)
