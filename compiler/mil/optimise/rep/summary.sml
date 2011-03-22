(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_REP_SUMMARY = 
sig
  type summary
  val summarize : PassData.t * Mil.symbolTable 
                  * {varNodes : MilRepNode.node Mil.VD.t,
                     iInfo : MilRepNode.node MilRepBase.iInfo MilUtils.Id.ImpDict.t,
                     nodes : MilRepNode.node list,
                     labelNodes : MilRepNode.node Mil.LD.t,
                     names : Mil.variable IntDict.t} -> summary
                                                        
  val layout : summary * Mil.symbolInfo -> Layout.t

  val iInfo' : summary * MilUtils.Id.t -> MilRepNode.node MilRepBase.iInfo option
  val iInfo : summary * MilUtils.Id.t -> MilRepNode.node MilRepBase.iInfo
  val variableClassId : summary * Mil.variable -> int
  val variableUsesKnown : summary * Mil.variable -> bool
  val variableDefsKnown : summary * Mil.variable -> bool
  val variableTyp : summary * Mil.variable -> Mil.typ
  val variableNode : summary * Mil.variable -> MilRepNode.node
  val nodeTyp : summary * MilRepNode.node -> Mil.typ
  val listVariables : summary -> Mil.variable list
  val resetTyps : summary -> unit
  val nodes : summary -> MilRepNode.node IntDict.t
  val addEdge : summary * (MilRepNode.node * MilRepNode.node) -> unit
  val edges : summary -> (MilRepNode.node * MilRepNode.node) list
  val debugs : Config.Debug.debug list
end (* signature MIL_REP_SUMMARY *)

structure MilRepSummary :> MIL_REP_SUMMARY = 
struct

  val passname = "MilRepSummary"

  val fail = 
   fn (f, m) => Fail.fail ("rep-summary.sml", f, m)

  structure PD = PassData
  structure M = Mil
  structure MU = MilUtils

  structure MRB = MilRepBase
  structure Node = MilRepNode
  structure Object = MilRepObject.Object
  structure Shape = MilRepObject.Shape

  structure ID = IntDict
  structure VD = Mil.VD
  structure LD = Mil.LD
  structure ND = Mil.ND
  structure VS = Mil.VS
  structure LS = Mil.LS
  structure I = Identifier
  structure IIdD = MU.Id.ImpDict
  structure IID = ImpIntDict
  structure L = Layout
  structure LU = LayoutUtils

  val (showNodeTypsD, showNodeTyps) =
      Config.Debug.mk (passname ^ ":show-node-types", "Show node types")

  val (showEscapesD, showEscapes) =
      Config.Debug.mk (passname ^ ":show-escaping", "Suppress non-escaping nodes in printout")

  val (showIntrudesD, showIntrudes) =
      Config.Debug.mk (passname ^ ":show-intruding", "Suppress non-intruding nodes in printout")

  val debugs = [showNodeTypsD, showEscapesD, showIntrudesD]

  structure ES = SetF(struct
                        type t = Node.node * Node.node
                        val nc = Int.compare o (Utils.Function.apply2 Node.classId)
                        val compare = Compare.pair (nc, nc)
                      end)

  datatype summary = S of {pd : PassData.t,
                           st : Mil.symbolTable,
                           typs : Mil.typ IID.t,
                           iInfo : Node.node MRB.iInfo IIdD.t,
                           nodes : Node.node ID.t,
                           edges : ES.t ref,
                           varNodes : Node.node VD.t,
                           labelNodes : Node.node LD.t,
                           names : M.variable ID.t}
  val ((setPd, getPd),
       (setSt, getSt),
       (setTyps, getTyps),
       (setNodes, getNodes),
       (setEdges, getEdges),
       (setVarNodes, getVarNodes),
       (setIInfo, getIInfo),
       (setLabelNode, getLabelNodes),
       (setNames, getNames)) = 
      let
        val r2t = 
         fn (S {pd, st, typs, nodes, edges, varNodes, iInfo, labelNodes, names}) => 
            (pd, st, typs, nodes, edges, varNodes, iInfo, labelNodes, names)
        val t2r = 
         fn (pd, st, typs, nodes, edges, varNodes, iInfo, labelNodes, names) => 
            S {pd = pd, st = st, typs = typs, nodes = nodes, edges = edges, varNodes = varNodes, 
               iInfo = iInfo, labelNodes = labelNodes, names = names}
      in
        FunctionalUpdate.mk9 (r2t, t2r)
      end
      
  val summarize = 
   fn (pd : PassData.t, 
       symbolTable : Mil.symbolTable,
       {varNodes : Node.node VD.t,
        nodes : Node.node list,
        iInfo : Node.node MRB.iInfo IIdD.t,
        labelNodes : Node.node LD.t,
        names : M.variable ID.t}) => 
      let
        val summary =
            S {pd = pd, 
               st = symbolTable,
               typs = IID.empty (),
               nodes = ID.fromList (List.map (nodes, fn n => (Node.id n, n))),
               edges = ref ES.empty,
               varNodes = varNodes, 
               iInfo = iInfo, 
               labelNodes = labelNodes, 
               names = names}
      in summary
      end

  val getConfig = 
   fn summary => PD.getConfig (getPd summary)

  val getSi = 
   fn summary => Identifier.SymbolInfo.SiTable (getSt summary)

  val iInfo' = 
   fn (summary, id) => 
      let
        val iInfo = getIInfo summary
        val info = IIdD.lookup (iInfo, id)
      in info
      end

  val iInfo = 
   fn (summary, id) => 
      let
        val info =
            (case iInfo' (summary, id)
              of SOME info => info
               | NONE =>
                 let
                   val si = getSi summary
                   val s = Layout.toString (MU.Id.layout (si, id))
                 in  fail ("iInfo", "Unknown identifier: "^ s)
                 end)
      in info
      end

  val variableNode = 
   fn (summary, v) => 
      let
        val varNodes = getVarNodes summary
        val n = 
            (case VD.lookup (varNodes, v) 
              of SOME n => n
               | NONE => fail ("variableNode", "Unknown variable"))
      in n
      end

  val variableClassId = 
   fn (summary, v) => 
      let
        val node = variableNode (summary, v)
        val id = Node.classId node
      in id
      end

  val variableUsesKnown =
   fn (summary, v) => 
      let
        val node = variableNode (summary, v)
        val b = Node.usesKnown node
      in b
      end

  val variableDefsKnown =
   fn (summary, v) => 
      let
        val node = variableNode (summary, v)
        val b = Node.defsKnown node
      in b
      end

  val rec nodeTyp = 
   fn (summary, n) => 
      (case IID.lookup (getTyps summary, Node.id n)
        of SOME t => t
         | NONE => 
           let
             val config = getConfig summary
             val id = Node.id n
             val typs = getTyps summary
             val shape = Node.shape n
             val flatTyp = Shape.flatTypOf (config, shape)
             val () = IID.insert (typs, id, flatTyp)

             val typ = 
                 let
                   val node = fn n => nodeTyp (summary, n)
                   val variance = fn n => Node.fieldVariance n
                 in Shape.typOf (config, shape, node, variance)
                 end

             val () =
                 if showNodeTyps config then
                   let
                     val si = getSi summary
                     val l = Layout.align [Layout.seq [Layout.str "Type for node ",
                                                       Int.layout id, 
                                                       Layout.str " = "],
                                           MilLayout.layoutTyp (config, si, typ)]
                     val () = LayoutUtils.printLayout l
                   in ()
                   end
                 else
                   ()

             val () = IID.insert (typs, id, typ)
           in typ
           end)

  val variableTyp = 
   fn (summary, v) => nodeTyp (summary, variableNode (summary, v))

  val resetTyps = 
   fn summary => IID.clear (getTyps summary)

  val listVariables = fn summary => List.map (VD.toList (getVarNodes summary), #1)

  val nodes = getNodes 

  val edges = ES.toList o (op !) o getEdges

  val addEdge = 
   fn (summary, e) => 
      let
        val edges as (ref s) = getEdges summary
        val s = ES.insert(s, e)
        val () = edges := s
      in ()
      end

  val layout = 
   fn (summary, si) => 
      let
        val config = getConfig summary
        val showFilter = 
            fn n => 
               (case (showEscapes config, Node.usesKnown n, showIntrudes config, Node.defsKnown n)
                 of (true, false, _, _) => SOME n
                  | (_, _, true, false) => SOME n
                  | (true, true, _, _) => NONE
                  | (_, _, true, true) => NONE
                  | _ => SOME n)
        val names = getNames summary
        val nameNode = 
         fn id => 
            Option.map (ID.lookup (names, id), fn v => MU.SymbolInfo.layoutVariable (si, v))
        val layoutNode = fn n => Node.layout (config, si, n, SOME nameNode)
        val nodeShallow = 
            fn n => 
               let
                 val id = Node.id n
               in case nameNode id
                   of SOME l => l
                    | NONE => Int.layout id
               end
        val layoutIInfo = fn i => MRB.layoutIInfo (config, getSi summary, i, nodeShallow)
        val edges = 
            let
              val le = fn (n1, n2) => L.seq[nodeShallow n1, L.str " => ", nodeShallow n2]
              val s = ES.layout (!(getEdges summary), le)
            in s
            end
        val varNodes = 
            let
              val filter = fn (id, n) => showFilter n
              val nodes = VD.keepAllMap (getVarNodes summary, filter)
              val l = VD.layout (nodes, fn (v, n) => layoutNode n)
            in l
            end
        val internalNodes = 
            let
              val filter = fn (id, n) => if ID.contains (names, id) then NONE else showFilter n
              val nodes = ID.keepAllMap (getNodes summary, filter)
              val l = ID.layout (nodes, fn (i, n) => layoutNode n)
            in l
            end
        val labelNodes = 
            let
              val filter = fn (id, n) => showFilter n
              val nodes = LD.keepAllMap (getLabelNodes summary, filter)
              val l = LD.layout (nodes, fn (l, n) => layoutNode n)
            in l
            end
        val layoutIInfoEntry = 
         fn (id, i) => L.mayAlign [L.seq [MU.Id.layout (si, id), L.str " => "], 
                                   LU.indent (layoutIInfo i)]
        val iInfo = IIdD.layout (getIInfo summary, layoutIInfoEntry)
      in L.align [L.str "VARIABLE NODES",
                  LU.indent varNodes,
                  L.str "ANONYMOUS NODES",
                  LU.indent internalNodes,
                  L.str "LABEL NODES",
                  LU.indent labelNodes,
                  L.str "EDGES",
                  LU.indent edges,
                  L.str "INSTRUCTION INFO",
                  LU.indent iInfo]
      end

end (* structure MilRepSummary *)