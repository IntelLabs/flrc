(* The Haskell Research Compiler *)
(*
 * Redistribution and use in source and binary forms, with or without modification, are permitted 
 * provided that the following conditions are met:
 * 1.   Redistributions of source code must retain the above copyright notice, this list of 
 * conditions and the following disclaimer.
 * 2.   Redistributions in binary form must reproduce the above copyright notice, this list of
 * conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 * BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)


signature MIL_REP_FLOW_GRAPH = 
sig
  type 'data t
  type node = MilRepNode.node
  val add : 'data t * node * 'data -> unit
  val addEdge : 'data t * node * node -> unit
  val propagate : 'data t -> unit
  val query : 'data t * node -> 'data
  val cc : MilRepSummary.summary * 'data t -> int MilRepNode.Dict.t
  (* 'data must be a lattice with only finite upward chains.
   * forward: forward flow or backward?
   * merge:
   *   must return NONE if no change (required for termination)
   *   should be associative and commutative 
   *)
  val build : {pd : PassData.t,
               forward : bool,
               summary : MilRepSummary.summary,
               uDefInit : 'data option,
               uUseInit : 'data option,
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
       and 'data t = FG of {forward : bool,
                            pd : PassData.t,
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
     fn (FG {forward, pd, graph, classNodes, merge, equal}) => (forward, pd, graph, classNodes, merge, equal)
    val t2r = 
     fn  (forward, pd, graph, classNodes, merge, equal) => 
         FG {forward = forward, pd = pd, graph = graph, classNodes = classNodes, merge = merge, equal =equal}
    val forward    = fn fg => (#2 o #1) (FunctionalUpdate.mk6 (r2t, t2r)) fg
    val pd         = fn fg => (#2 o #2) (FunctionalUpdate.mk6 (r2t, t2r)) fg
    val graph      = fn fg => (#2 o #3) (FunctionalUpdate.mk6 (r2t, t2r)) fg
    val classNodes = fn fg => (#2 o #4) (FunctionalUpdate.mk6 (r2t, t2r)) fg
    val merge      = fn fg => (#2 o #5) (FunctionalUpdate.mk6 (r2t, t2r)) fg
    val equal      = fn fg => (#2 o #6) (FunctionalUpdate.mk6 (r2t, t2r)) fg
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
        val uDefO = Option.map (uDefInit, fn uDefInit => IPLG.newNode (g, NodeLabel.new uDefInit))
        val uUseO = Option.map (uUseInit, fn uUseInit => IPLG.newNode (g, NodeLabel.new uUseInit))
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
        val addUDefEdge = 
         fn cn => case uDefO of SOME uDef => addEdge (uDef, cn) | NONE      => ()
        val addUUseEdge = 
         fn cn => case uUseO of SOME uUse => addEdge (cn, uUse) | NONE      => ()
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
                         val () = if MRN.defsKnown n then () else addUDefEdge classNode
                         val () = if MRN.usesKnown n then () else addUUseEdge classNode
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
        val fg = FG {forward = forward, pd = pd, graph = g, classNodes = classNodes, merge = merge, equal = equal}
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
        val classNode = classNodeForNode (fg, n)
        val cd = getDataForClassNode classNode
        val cd = FlowGraph.merge fg (d, cd)
        val () = setDataForClassNode (classNode, cd)
      in ()
      end

  val addEdge : 'data t * node * node -> unit =
   fn (fg, from, to) => 
      let
        val forward = FlowGraph.forward fg
        val (from, to) = if forward then 
                           (from, to)
                         else 
                           (to, from)
        val from = classNodeForNode (fg, from)
        val to = classNodeForNode (fg, to)
        val edge = IPLG.addEdge (FlowGraph.graph fg, from, to, ())
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
        val help = fn (n, changed) => propagate0 (fg, n) orelse changed
        val rec loop =  
         fn () => if List.fold (component, false, help) then
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
        val classNode = classNodeForNode (fg, n)
        val label = IPLG.Node.getLabel classNode
        val cd = NodeLabel.data label
      in cd
      end

  val cc : MilRepSummary.summary * 'data t -> int MilRepNode.Dict.t = 
   fn (summary, fg as FG {classNodes, graph, ...}) => 
      let
        val nodeComponentNumber = 
            let
              val components = IPLG.cc graph
              val help2 = fn i => fn (cn, d) => IntDict.insert (d, IPLG.Node.id cn, i)
              val help = fn (i, component, d) => List.fold (component, d, help2 i)
              (* Map graph node ids to equivalence class # *)
              val d : int ID.t = List.foldi (components, IntDict.empty, help)
              val map = 
               fn n => case IntDict.lookup (d, IPLG.Node.id (classNodeForNode (fg, n)))
                        of SOME i => i
                         | NONE   => fail ("cc", "Bad node")
            in map
            end
        val nodes = MRS.nodes summary
        val add = fn (n, d) => MRN.Dict.insert (d, n, nodeComponentNumber n)
        val d = IntDict.fold (nodes, MRN.Dict.empty, fn (_, n, d) => add (n, d))
      in d
      end

end (* structure MilRepFlowGraph *)
