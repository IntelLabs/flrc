(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, July 2008 *)

(* Graph implementations *)

(* Imperative polymorphic graphs *)
signature IMP_POLY_LABELED_GRAPH = 
sig 

  eqtype ('a, 'b) node
  eqtype ('a, 'b) edge

  type ('a, 'b) t
     
  structure Node : sig
    val compare   : ('a, 'b) node * ('a, 'b) node -> order
    val equal     : ('a, 'b) node * ('a, 'b) node -> bool
    (* Set of sucessor nodes. No duplicates. *) 
    val succs     : ('a, 'b) node -> ('a, 'b) node List.t
    (* Set of predecessor nodes. No duplicates. *) 
    val preds     : ('a, 'b) node -> ('a, 'b) node List.t
    (* List of input edges. *)
    val inEdges   : ('a, 'b) node -> ('a, 'b) edge List.t
    (* List of output edges. *)
    val outEdges  : ('a, 'b) node -> ('a, 'b) edge List.t
    (* Get list of edges from a to b. *)
    val getEdges  : ('a, 'b) node * ('a, 'b) node -> ('a, 'b) edge List.t
    val setLabel  : ('a, 'b) node * 'a -> unit
    val getLabel  : ('a, 'b) node -> 'a
  end
                   
  structure Edge : sig
    val compare  : ('a, 'b) edge * ('a, 'b) edge -> order
    val equal    : ('a, 'b) edge * ('a, 'b) edge -> bool
    val to       : ('a, 'b) edge -> ('a, 'b) node
    val from     : ('a, 'b) edge -> ('a, 'b) node
    val setLabel : ('a, 'b) edge * 'b -> unit
    val getLabel : ('a, 'b) edge -> 'b
  end
                   
  (* Graph: Nodes operations. *)
  val newNode        : ('a, 'b) t * 'a -> ('a, 'b) node
  val deleteNode     : ('a, 'b) t * ('a, 'b) node -> unit
  val nodes          : ('a, 'b) t -> ('a, 'b) node List.t

  (* Graph: Edges operations. *)
  val addEdge        : ('a, 'b) t * ('a, 'b) node * ('a, 'b) node * 'b
                       -> ('a, 'b) edge
  val deleteEdge     : ('a, 'b) t * ('a, 'b) edge -> unit
  val edges          : ('a, 'b) t -> ('a, 'b) edge List.t

  (* Graph operations. *)
  val new           : unit -> ('a, 'b) t
  val copy          : ('a, 'b) t
                      -> ('a, 'b) t * 
                         (('a, 'b) node -> ('a, 'b) node) *
                         (('a, 'b) edge -> ('a, 'b) edge)
  val map           : ('a, 'b) t * ('a -> 'c) * ('b -> 'd)
                      -> ('c, 'd) t * 
                         (('a, 'b) node -> ('c, 'd) node) *
                         (('a, 'b) edge -> ('c, 'd) edge)
         
  (* List of strongly connected components, topologically sorted.  No 
   * edges in the graph point backward in the list *)
  val scc          : ('a, 'b) t -> ('a, 'b) node list list
  val dfsTree      : ('a, 'b) t * ('a, 'b) node -> ('a, 'b) node Tree.t
  val postOrderDfs : ('a, 'b) t * ('a, 'b) node -> ('a, 'b) node List.t
  val domTree      : ('a, 'b) t * ('a, 'b) node -> ('a, 'b) node Tree.t
  val pdomTree     : ('a, 'b) t * ('a, 'b) node -> ('a, 'b) node Tree.t
  val reachable    : ('a, 'b) t * ('a, 'b) node -> ('a, 'b) node List.t
  val unreachable  : ('a, 'b) t * ('a, 'b) node -> ('a, 'b) node List.t

  val layoutDot' : ('a, 'b) t *
                   {edgeOptions  : ('a, 'b) edge -> Dot.EdgeOption.t list,
                    nodeOptions  : ('a, 'b) node -> Dot.NodeOption.t list,
                    graphOptions : Dot.GraphOption.t list,
                    graphTitle   : Layout.t}
                   -> Layout.t
  val layoutDot  : ('a, 'b) t * 
                   {nodeName   : ('a, 'b) node -> Layout.t, 
                    graphTitle : Layout.t}
                   -> Layout.t
end;

structure ImpPolyLabeledGraph :> IMP_POLY_LABELED_GRAPH = 
struct

  fun fail (f, m) = Fail.fail ("ImpPolyLabeledGraph", f, m)
                  
  datatype  ('a, 'b) t = G of {nodes : ('a, 'b) node List.t ref,
                               edges : ('a, 'b) edge List.t ref,
                               idGen : int ref}
                              
       and ('a, 'b) node = N of {uniqueId : int,
                                 outEdges : ('a, 'b) edge List.t ref,
                                 inEdges  : ('a, 'b) edge List.t ref,
                                 label    : 'a ref}
                                
       and ('a, 'b) edge = E of {uniqueId : int,
                                 to       : ('a, 'b) node,
                                 from     : ('a, 'b) node,
                                 label    : 'b ref}
                                
  fun compareNode (N n1, N n2) = Int.compare (#uniqueId n1, #uniqueId n2)
                                 
  fun compareEdge (E e1, E e2) = Int.compare (#uniqueId e1, #uniqueId e2)

  structure Edge = 
  struct
    val compare  = compareEdge
    val equal    = fn (e1, e2) => (compare (e1, e2) = EQUAL)
    val to       = fn (E e) => #to e
    val from     = fn (E e) => #from e
    val getLabel = fn (E e) => !(#label e)
    val setLabel = fn (E e, v) => (#label e) := v
  end
  
  structure Node = 
  struct
    val compare  = compareNode
    val equal    = fn (n1, n2) => (compare (n1, n2) = EQUAL)
    val inEdges  = fn (N n) => !(#inEdges n)
    val outEdges = fn (N n) => !(#outEdges n)
    val getEdges : ('a, 'b) node * ('a, 'b) node -> ('a, 'b) edge List.t =
     fn (a, b) =>
        List.keepAll (outEdges (a), fn (e) => equal (Edge.to (e), b))

    (* XXX EB: We may need a more efficient implementation. *)
    val succs = 
     fn (n) => 
        Utils.uniqueList (List.map (outEdges (n), Edge.to), equal)

    (* XXX EB: We may need a more efficient implementation. *)
    val preds = 
     fn (n) => 
        Utils.uniqueList (List.map (inEdges (n), Edge.from), equal)

    val setLabel = fn (N n, v) => (#label n) := v
    val getLabel = fn (N n) => !(#label n)

  end
  
  fun generateId (G g) = 
      let
        val idGen = #idGen g
        val newId = !idGen
        val () = idGen := !idGen + 1
      in
        newId
      end
      
  fun filterOutEdge (list, e) = 
      List.keepAll (list, fn e' => not (Edge.equal (e, e')))

  fun filterOutNode (list, n) = 
      List.keepAll (list, fn n' => not (Node.equal (n, n')))

  fun getNodeId (N n) = #uniqueId n

  fun getEdgeId (E e) = #uniqueId e

  fun addNodeInEdge (N n, edge) = (#inEdges n) := edge::(!(#inEdges n))
                                  
  fun addNodeOutEdge (N n, edge) = (#outEdges n) := edge::(!(#outEdges n))
                                   
  (* Graph: Edges operations. *)
  val addEdge : ('a, 'b) t * ('a, 'b) node * ('a, 'b) node * 'b -> ('a, 'b) edge =
   fn (G g, src, dst, label) =>
      let
        val edge = E {uniqueId = generateId (G g),
                      to       = dst,
                      from     = src,
                      label    = ref label}
        val edges = #edges g
        val () = edges := edge::(!edges)
        val () = addNodeInEdge (dst, edge)
        val () = addNodeOutEdge (src, edge)
      in
        edge
      end

  val deleteEdge =
   fn (G g, E e) =>
      let
        val N src = #from e
        val outEdges = #outEdges src
        val () = outEdges := filterOutEdge (!outEdges, E e)
        val N dst = #to e
        val inEdges = #inEdges dst
        val () = inEdges := filterOutEdge (!inEdges, E e)
        val edges = #edges g
        val () = edges := filterOutEdge (!edges, E e)
      in ()
      end

  val edges = fn (G g) => !(#edges g)

  (* Graph: Nodes operations. *)
  val newNode : ('a, 'b) t * 'a -> ('a, 'b) node =
   fn (G g, label) =>
      let
        val node = N {uniqueId = generateId (G g),
                      outEdges = ref nil,
                      inEdges  = ref nil,
                      label    = ref label}
        val nodes = #nodes g
        val () = nodes := node::(!nodes)
      in
        node
      end

  val deleteNode = 
   fn (G g, N n) =>
      let
        (* Delete adjacent edges. XXX EB: Could be improved for performance. *)
        val adjEdges = (Node.inEdges (N n)) @ (Node.outEdges (N n))
        val () = List.foreach (adjEdges, fn e => deleteEdge (G g, e))
        (* Remove the node from the graph. *)
        val nodes = #nodes g
        val () = nodes := filterOutNode (!nodes, N n)
      in ()
      end

  val nodes = fn (G g) => !(#nodes g)

  val new = 
   fn () => G {nodes = ref nil,
               edges = ref nil,
               idGen = ref 0}
            
  fun mapRev (G g, mapNodeData, mapEdgeData, rev) =
      let
        (* Map nodes. *)
        fun mapNode (N n) = 
            let
              val uniqueId = #uniqueId n
              val ref label = #label n
              val newLabel = mapNodeData label
            in
              N {uniqueId = uniqueId,
                 outEdges = ref nil,
                 inEdges  = ref nil,
                 label    = ref newLabel}
            end
        val newNodes = List.map (nodes (G g), mapNode)
        val idToNewNodeDict = 
            IntDict.fromList (List.map (newNodes, fn n => (getNodeId (n), n)))
        fun nodeToNewNode (node) = 
            case IntDict.lookup (idToNewNodeDict, getNodeId node)
             of SOME n => n
              | NONE => fail ("nodeToNewNode", 
                              "Could not map node to new node")
        (* Map nodes. *)
        fun mapEdge (E e) = 
            let
              val uniqueId = #uniqueId e
              val ref label = #label e
              val newLabel = mapEdgeData label
              val dst = Edge.to (E e)
              val src = Edge.from (E e)
              val (newSrc, newDst) = 
                  if rev then 
                    (nodeToNewNode (dst), nodeToNewNode (src))
                  else
                    (nodeToNewNode (src), nodeToNewNode (dst))
              val newEdge = E {uniqueId = uniqueId,
                               to       = newDst,
                               from     = newSrc,
                               label    = ref newLabel}
              val () = addNodeOutEdge (newSrc, newEdge)
              val () = addNodeInEdge (newDst, newEdge)
            in
              newEdge
            end
        val newEdges = List.map (edges (G g), mapEdge)
        val idToNewEdgeDict = 
            IntDict.fromList (List.map (newEdges, fn n => (getEdgeId (n), n)))
        fun edgeToNewEdge (edge) = 
            case IntDict.lookup (idToNewEdgeDict, getEdgeId edge)
             of SOME n => n
              | NONE => fail ("edgeToNewEdge", 
                              "Could not map edge to new edge")
        val ref idGen = #idGen g
      in
        (G {nodes = ref newNodes, 
            edges = ref newEdges, 
            idGen = ref idGen},
         nodeToNewNode, 
         edgeToNewEdge)
      end
      
  val map : ('a, 'b) t * ('a -> 'c) * ('b -> 'd)
            -> ('c, 'd) t * 
               (('a, 'b) node -> ('c, 'd) node) *
               (('a, 'b) edge -> ('c, 'd) edge) = 
   fn (g, mapNodeData, mapEdgeData) => 
      mapRev (g, mapNodeData, mapEdgeData, false)
      
  val copy : ('a, 'b) t -> ('a, 'b) t * 
                           (('a, 'b) node -> ('a, 'b) node) *
                           (('a, 'b) edge -> ('a, 'b) edge) =
   fn (g) => mapRev (g, fn n => n, fn e => e, false)
      
  (* EB: Slow (n^2) function, use it carefully. *)
  fun checkForDuplicates (nil, m) = ()
    | checkForDuplicates ([x], m) = ()
    | checkForDuplicates (x::xs, m) =
      if List.exists (xs, fn x' => (x=x')) then
        fail ("checkForDuplicates", m ^ " list has duplicated elements.")
      else
        checkForDuplicates (xs, m)

  fun checkNodesUniqueIds (g) = 
      let
        val idList = List.map (nodes (g), getNodeId)
      in
        checkForDuplicates (idList, "graph node unique id")
      end

  structure DG = DirectedGraph

  (* Build DG to reuse MLTON graph algorithms. 
   * Returns
   *  DG graph    : dg
   *  getDGNode   : IPLG.node -> DG.node
   *  getIPLGNode : DG.node -> IPLG.node
   *)
  fun buildDG' (G g, rev) =
      let
        (* XXX EB: Debug only (slow function). Ensure nodes have unique ids. *)
        (* val () = checkNodesUniqueIds (G g) *)
        val dg = DG.new ()
        val prop = Property.initRaise ("node data", DG.Node.layout)
        val {set, get=getIPLGNode, ...} = 
            Property.getSetOnce (DG.Node.plist, prop)
        val idToDGNodeDict = ref IntDict.empty
        fun getDGNode n = 
            case IntDict.lookup (!idToDGNodeDict, getNodeId (n))
             of SOME n => n
              | NONE => fail ("getDGNode", "Could not map id to DG node.")
        fun createDGNode (n) = 
            let
              val dgNode = DG.newNode dg
              val id = getNodeId (n)
            in
              (idToDGNodeDict := IntDict.insert (!idToDGNodeDict, id, dgNode);
               set (dgNode, n))
            end
        val () = List.foreach (nodes (G g), createDGNode)
        fun createDGEdge (e) = 
            let
              val src = getDGNode (Edge.from (e))
              val dst = getDGNode (Edge.to (e))
            in
              if rev then
                ignore (DG.addEdge (dg, {from = dst, to = src}))
              else
                ignore (DG.addEdge (dg, {from = src, to = dst}))
            end
        val () = List.foreach (edges (G g), createDGEdge)
      in
        {dg          = dg, 
         getIPLGNode = getIPLGNode,
         getDGNode   = getDGNode}
      end

  fun buildDG (g)        = buildDG' (g, false)
  fun buildReverseDG (g) = buildDG' (g, true)

  val scc =
   fn (G g) =>
      let
        val {dg, getIPLGNode, ...} = buildDG (G g)
        val dgSccList = DG.stronglyConnectedComponents (dg)
      in
        List.map (dgSccList, fn l => List.map (l, getIPLGNode))
      end

  val dfsTree =
   fn (G g, start) =>
      let
        val {dg, getIPLGNode, getDGNode} = buildDG (G g)
        val dgStart = getDGNode (start)
      in
        DG.dfsTree (dg, {root = dgStart, nodeValue = getIPLGNode})
      end

  val domTree =
   fn (G g, start) =>
      let
        val {dg, getIPLGNode, getDGNode} = buildDG (G g)
        val dgStart = getDGNode (start)
      in
        DG.dominatorTree (dg, {root = dgStart, nodeValue = getIPLGNode})
      end

  val pdomTree =
   fn (G g, start) =>
      let
        val {dg, getIPLGNode, getDGNode} = buildReverseDG (G g)
        val dgStart = getDGNode (start)
      in
        DG.dominatorTree (dg, {root = dgStart, nodeValue = getIPLGNode})
      end

  fun treeToList (t) = Tree.foldPost (t, nil, fn (a, l) => a::l)

  val reachable = 
   fn (g, start) => treeToList (dfsTree (g, start))

  val unreachable =
   fn (g, start) =>
      let
        val tree = dfsTree (g, start)
        val visited = ref IntDict.empty
        fun visitedNode n = IntDict.contains (!visited, getNodeId n)
        fun visit (n) = visited := IntDict.insert (!visited, getNodeId (n), ())
        val () = Tree.foreachPre (tree, visit)
      in
        List.keepAll (nodes (g), not o visitedNode)
      end

  val postOrderDfs : ('a, 'b) t * ('a, 'b) node -> ('a, 'b) node List.t =
   fn (g, start) =>
      let
        val visited = ref IntDict.empty
        fun visitedNode n = IntDict.contains (!visited, getNodeId n)
        fun visit (n) = visited := IntDict.insert (!visited, getNodeId (n), ())
        fun postOrderVisit (n) = 
            let
              val () = visit (n)
            in
              List.concatMap (Node.outEdges (n), visitSucc o Edge.to) @ [n]
            end
        and visitSucc (dst) = 
            if (visitedNode (dst)) then nil
            else postOrderVisit (dst)
      in
        postOrderVisit (start)
      end

  val layoutDot' = 
   fn (G g, {edgeOptions, nodeOptions, graphOptions, graphTitle}) =>
      let
        fun nameNode (n) = Int.toString (getNodeId (n))
        fun doNode (n) = 
            let
              fun doEdge (e) = 
                  {name    = nameNode (Edge.to e),
                   options = edgeOptions e}
            in 
              {name       = nameNode n,
               options    = nodeOptions n,
               successors = List.map (Node.outEdges (n), doEdge)}
            end
      in
        Dot.layout {nodes   = List.map (nodes (G g), doNode),
                    options = graphOptions,
                    title   = Layout.toString graphTitle}
      end

  val layoutDot = 
   fn (G g, {nodeName, graphTitle}) =>
      let
        fun edgeOptions e = []
        val graphOptions  = []
        fun nodeOptions n = 
            [Dot.NodeOption.label (Layout.toString (nodeName n))]
      in
        layoutDot' (G g, {edgeOptions  = edgeOptions,
                          nodeOptions  = nodeOptions,
                          graphOptions = graphOptions,
                          graphTitle   = graphTitle})
      end
end;

(* Applicative polymorphic graphs *)
signature POLY_LABELED_GRAPH =
sig

  (* An ('a, 'b) graph has nodes labelled by 'a and edges labelled by 'b;
   * edges are directed and there can be multiple edges between two nodes
   *)
  eqtype ('a, 'b) node
  eqtype ('a, 'b) edge
  type ('a, 'b) t

  structure Node : sig
    val compare  : ('a, 'b) node Compare.t
    val equal    : ('a, 'b) node * ('a, 'b) node -> bool
    val succs    : ('a, 'b) node -> ('a, 'b) node list
    val preds    : ('a, 'b) node -> ('a, 'b) node list
    val inEdges  : ('a, 'b) node -> ('a, 'b) edge list
    val outEdges : ('a, 'b) node -> ('a, 'b) edge list
    val getEdges : ('a, 'b) node * ('a, 'b) node -> ('a, 'b) edge list
    val getLabel : ('a, 'b) node -> 'a
  end

  structure Edge : sig
    val compare  : ('a, 'b) edge Compare.t
    val equal    : ('a, 'b) edge * ('a, 'b) edge -> bool
    val to       : ('a, 'b) edge -> ('a, 'b) node
    val from     : ('a, 'b) edge -> ('a, 'b) node
    val getLabel : ('a, 'b) edge -> 'b
  end

  (* For each entry in nodes create a new node labeled by that entry and call node threading an accumlator (left to
   * right through the list) whose initial value is init.  Pass the final accumulator to edges to get the edges.
   * Return the resulting graph and final accumulator.
   * Note that the nodes as passed to node have no edges, and are mutated after calling edges but before returning to
   * add the edges.
   *)
  val new : {nodes : 'a list,
             init  : 'c,
             node  : 'a * ('a, 'b) node * 'c -> 'c,
             edges : 'c -> (('a, 'b) node * ('a, 'b) node * 'b) list}
            -> ('a, 'b) t * 'c

  val nodes : ('a, 'b) t -> ('a, 'b) node list
  val edges : ('a, 'b) t -> ('a, 'b) edge list

  val map : ('a, 'b) t * ('a -> 'c) * ('b -> 'd)
            -> ('c, 'd) t * (('a, 'b) node -> ('c, 'd) node) * (('a, 'b) edge -> ('c, 'd) edge)

  (* List of strongly connected components, topologically sorted.  No 
   * edges in the graph point backward in the list *)
  val scc          : ('a, 'b) t -> ('a, 'b) node list list
  val dfsTree      : ('a, 'b) t * ('a, 'b) node -> ('a, 'b) node Tree.t
  val postOrderDfs : ('a, 'b) t * ('a, 'b) node -> ('a, 'b) node list
  val domTree      : ('a, 'b) t * ('a, 'b) node -> ('a, 'b) node Tree.t
  val pdomTree     : ('a, 'b) t * ('a, 'b) node -> ('a, 'b) node Tree.t
  val reachable    : ('a, 'b) t * ('a, 'b) node -> ('a, 'b) node list
  val unreachable  : ('a, 'b) t * ('a, 'b) node -> ('a, 'b) node list

  val layoutDot' : ('a, 'b) t *
                   {edgeOptions  : ('a, 'b) edge -> Dot.EdgeOption.t list,
                    nodeOptions  : ('a, 'b) node -> Dot.NodeOption.t list,
                    graphOptions : Dot.GraphOption.t list,
                    graphTitle   : Layout.t}
                   -> Layout.t
  val layoutDot  : ('a, 'b) t * {nodeName : ('a, 'b) node -> Layout.t, graphTitle : Layout.t} -> Layout.t

end;

structure PolyLabeledGraph :> POLY_LABELED_GRAPH =
struct

  structure G = ImpPolyLabeledGraph
  open G

  val new = 
   fn {nodes = nodeLabels, init, node = folder, edges = edgeGen} =>
      let
        val g = G.new ()
        val nodes = List.map (nodeLabels, fn l => G.newNode (g, l))
        val d = List.fold2 (nodeLabels, nodes, init, folder)
        val edges = edgeGen d
        val () = List.foreach (edges, fn (e1, e2, l) => ignore (G.addEdge (g, e1, e2, l)))
      in (g, d)
      end

end;

