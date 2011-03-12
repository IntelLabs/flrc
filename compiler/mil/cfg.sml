(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_CFG =
sig

    type t
    type node
    type label = Mil.label
    type block = Mil.block

    val tGetSi : t -> Mil.symbolInfo

    val compareNode : node * node -> order
    structure NodeDict : DICT where type key = node

    val build   : Config.t * Mil.symbolInfo * Mil.codeBody -> t
    val unBuild : t -> Mil.codeBody

    (* Pseudo nodes*)
    val entry : t -> node
    val exit  : t -> node

    val nodeGetLabel      : t * node -> label option
    val nodeGetBlock      : t * node -> block option
    val nodeGetLabelBlock : t * node -> (label * block) option

    val labelGetNode  : t * label -> node
    val labelGetBlock : t * label -> block

    val succ : t * node -> node List.t
    val pred : t * node -> node List.t

    val numInEdges : t * node -> int

    val nodes : t -> node list

    val scc         : t -> node list list
    val unreachable : t * node -> node list

    val startLabel : t -> label

    val getNodeDomTree       : t * node -> node Tree.t
    val getNodePDomTree      : t * node -> node Tree.t
    val getLabelDomTree      : t -> label Tree.t
    val getLabelBlockDomTree : t -> (label * block) Tree.t
    val getExtBBlockTree     : t -> ((label * block) List.t) Tree.t

    structure NodeDominance : DOMINANCE where type node = node
    structure LabelDominance : DOMINANCE where type node = label

    (* Layout the cfg in dot  format. The second argument is the graph
     * label. *)
    val layoutDot : t * Layout.t option -> Layout.t

end;

structure MilCfg :> MIL_CFG =
struct

  structure L = Layout
  structure LU = LayoutUtils
  structure G = ImpPolyLabeledGraph
  structure I = Identifier
  structure VS = I.VariableSet
  structure VD = I.VariableDict
  structure LS = I.LabelSet
  structure LD = I.LabelDict
  structure M = Mil
  structure MU = MilUtils
  structure OE = MU.OutEdge

  val passname = "MilCfg"

  fun fail (f, m) = Fail.fail ("MilCfg", f, m)

  type label = M.label
  type block = M.block
  type labelBlock = label * block
  type graph = (labelBlock option, unit) G.t
  type node  = (labelBlock option, unit) G.node

  datatype t = C of {
    config : Config.t,
    si     : M.symbolInfo,
    graph  : graph,
    entry  : node,
    exit   : node,
    blocks : block LD.t,
    nodes  : node LD.t
  }

  fun tGet sel (C t) = sel t
  val tGetConfig = tGet #config
  val tGetSi     = tGet #si
  val tGetGraph  = tGet #graph
  val tGetEntry  = tGet #entry
  val tGetExit   = tGet #exit
  val tGetBlocks = tGet #blocks
  val tGetNodes  = tGet #nodes

  structure Chat = ChatF (type env = t
                          val extract = tGetConfig
                          val name = passname
                          val indent = 0)

  fun tSet (C t, config, si, graph, entry, exit, blocks, nodes) = 
      {config = config t,
       si     = si t,
       graph  = graph t, 
       entry  = entry t, 
       exit   = exit t, 
       blocks = blocks t, 
       nodes  = nodes t}

  fun tSetGraph (t, graph)   = 
      tSet (t, #config, #si, fn _ => graph, #entry, #exit, #blocks, #nodes)
  fun tSetEntry (t, entry)   = 
      tSet (t, #config, #si, #graph, fn _ => entry, #exit, #blocks, #nodes)
  fun tSetExit (t, exit)     = 
      tSet (t, #config, #si, #graph, #entry, fn _ => exit, #blocks, #nodes)
  fun tSetBlocks (t, blocks) = 
      tSet (t, #config, #si, #graph, #entry, #exit, fn _ => blocks, #nodes)
  fun tSetNodes (t, nodes)   = 
      tSet (t, #config, #si, #graph, #entry, #exit, #blocks, fn _ => nodes)

  val compareNode : node * node -> order = G.Node.compare

  structure NodeDict = DictF (struct 
                                type t = node
                                val compare = compareNode
                              end)

  fun build (config, si, M.CB {entry = entryL, blocks, ...}) =
      let
        val graph = G.new ()
        val entry = G.newNode (graph, NONE)
        val exit  = G.newNode (graph, NONE)
        val nodes = LD.map (blocks, fn lb => G.newNode (graph, SOME lb))
        fun addEdges (l, b) = 
            let
              val node  = valOf (LD.lookup (nodes, l))
              val es = MU.Block.outEdges b
              fun addOne (OE.OE {dest, ...}) =
                  case dest
                   of OE.OedBlock l =>
                      (case LD.lookup (nodes, l)
                        of SOME n => ignore (G.addEdge (graph, node, n, ()))
                         | NONE => fail ("build", "Unbound label"))
                    | OE.OedExit =>
                      ignore (G.addEdge (graph, node, exit, ()))
              val () = Vector.foreach (es, addOne)
            in ()
            end
        val () = LD.foreach (blocks, addEdges)
        val startNode =
            case LD.lookup (nodes, entryL)
             of SOME n => n
              | NONE => fail ("build", "Bad entry label")
        val _ = G.addEdge (graph, entry, startNode, ())
        val cfg = 
            C {config = config,
               si     = si,
               graph  = graph,
               entry  = entry,
               exit   = exit,
               blocks = blocks,
               nodes  = nodes}
      in cfg
      end

  val entry = tGetEntry
  val exit  = tGetExit

  fun nodeGetLabelBlock (cfg, n) : labelBlock option = G.Node.getLabel n
  fun nodeGetLabel (cfg, n) = Option.map (nodeGetLabelBlock (cfg, n), #1)
  fun nodeGetBlock (cfg, n) = Option.map (nodeGetLabelBlock (cfg, n), #2)

  fun labelGetNode (cfg, l) =
      case LD.lookup (tGetNodes cfg, l)
       of SOME n => n
        | NONE => fail ("labelGetNode", "Bad label")

  fun labelGetBlock (cfg, l) =
      case LD.lookup (tGetBlocks cfg, l)
       of SOME n => n
        | NONE => fail ("labelGetBlock", "Bad label")

  fun succ (cfg, n) = G.Node.succs n
  fun pred (cfg, n) = G.Node.preds n

  fun numInEdges (cfg, n) = List.length (G.Node.inEdges n)

  fun startNode cfg =
      let
        val n =
            case succ (cfg, entry cfg)
             of [] => fail ("startNode", "Empty Cfg")
              | [n] => n
              | _ =>  fail ("startNode", "Multiple entry Cfg")
      in
        if G.Node.equal (n, (entry cfg)) orelse 
           G.Node.equal (n, (exit cfg)) then
          fail ("startNode", "Empty/Weird Cfg")
        else 
          n
      end

  fun startLabel cfg =
      case nodeGetLabel (cfg, startNode cfg)
       of SOME l => l
        | NONE => fail ("startLabel", "Start block is unlabelled")

  fun unBuild cfg =
      let
        val entry = startLabel cfg
        val blocks = tGetBlocks cfg
        val cb = M.CB {entry = entry, blocks = blocks}
      in cb
      end

  fun nodes cfg = G.nodes (tGetGraph cfg)

  fun scc cfg = G.scc (tGetGraph cfg)

  fun unreachable (cfg, start) = G.unreachable (tGetGraph cfg, start)

  fun getNodeDomTree (cfg, start) = G.domTree (tGetGraph cfg, start)

  fun getNodePDomTree (cfg, start) = G.pdomTree (tGetGraph cfg, start)

  fun labeledTree (cfg, buildTree, getLabel) =
      let
        val t = buildTree (cfg, startNode cfg)
        fun doOptTree (Tree.T (n, nv)) =
            case getLabel (cfg, n) 
             of SOME l => SOME (Tree.T (l, Vector.keepAllMap (nv, doOptTree)))
              | NONE => NONE
      in
        case doOptTree t
         of SOME t => t
          | NONE => fail ("labeledTree", "no tree left")
      end

  fun getLabelDomTree cfg =
      labeledTree (cfg, getNodeDomTree, nodeGetLabel)
  fun getLabelBlockDomTree cfg =
      labeledTree (cfg, getNodeDomTree, nodeGetLabelBlock)

  fun treeToBBlockTree (cfg, Tree.T (lb as (l, _), trees)) =
      let
        val n = labelGetNode (cfg, l)
        val exitN = exit cfg
        val entryN = exit cfg
        val bbTrees = Vector.map (trees, fn t => treeToBBlockTree (cfg, t))
        val t = 
            case succ (cfg, n)
             of [m] => 
                (case (G.Node.equal (m, entryN), 
                       G.Node.equal (m, exitN), 
                       pred (cfg, m))
                  of (false, false, [_]) => 
                     let
                       val Tree.T (ms, children) = Vector.sub (bbTrees, 0)
                       val t = Tree.T (lb::ms, children)
                     in t
                     end
                   | _ => Tree.T ([lb], bbTrees))
              | _ => Tree.T ([lb], bbTrees)
      in t
      end

  fun getExtBBlockTree cfg = treeToBBlockTree (cfg, getLabelBlockDomTree cfg)

  structure NodeDominance = DominanceF (type node = node
                                        val compare = compareNode)

  structure LabelDominance = DominanceF (type node = label
                                         val compare = I.labelCompare)

  fun layoutDot (cfg, label) =
      let
        val C {graph, entry, exit, ...} = cfg
        fun nodeName (n) =
            if G.Node.equal (n, entry) then
              L.str "Entry"
            else if G.Node.equal (n, exit) then
              L.str "Exit"
            else
              case nodeGetLabel (cfg, n)
               of SOME l => Identifier.layoutLabel l
                | NONE =>
                  fail ("layoutDot.nodeName",
                        "CFG node for block without label")
        val graphTitle =
            case label
             of SOME l => l 
              | NONE   => L.str "CFG" 
      in
        G.layoutDot (graph, {graphTitle = graphTitle, nodeName = nodeName})
      end

end;
