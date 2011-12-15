(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* Redundant Conditional Branch Removal 
 *)

signature MIL_REMOVE_BRANCH =
sig
  val pass : (BothMil.t, BothMil.t) Pass.t
end
                             
structure MilRemoveBranch :> MIL_REMOVE_BRANCH = 
struct

  val passname = "MilRemoveBranch"

  fun fail (f, m) = Fail.fail (passname, f, m)
  fun assert (f, m, b) = if b then fail (f, m) else ()

  structure M   = Mil
  structure PD  = PassData
  structure L   = Layout
  structure LU  = LayoutUtils
  structure ID  = Identifier
  structure LD  = ID.LabelDict
  structure LDOM = MilCfg.LabelDominance
  structure I   = Identifier
  structure MU  = MilUtils
  structure ML  = MilLayout

  structure Chat = ChatF (struct 
                            type env = PD.t
                            val extract = PD.getConfig
                            val name = passname
                            val indent = 2
                          end)

  structure Click = 
  struct
    val stats = []
    val {stats, click = rcbr} = PD.clicker {stats = stats, passname = passname, 
                                            name = "ImpossibleEdges", desc = "Impossible branches removed"}
    val {stats, click = prunePath} = PD.clicker {stats = stats, passname = passname, 
                                                 name = "PrunedPaths", desc = "Short correlated branches skipped"}
  end   (*  structure Click *)

  val stats = Click.stats

  datatype const =
           RCons of M.constant
         | RSel  of M.constant

  (* EP (v, c, true)  => v = c
   * EP (v, c, false) => v <> c 
   *)
  datatype edgePredicate = EP of M.variable * const * bool

  val getLabel = fn (imil, b) => #1 (IMil.IBlock.getLabel' (imil, b))
  val labelString = fn (imil, b) => ID.labelString(getLabel(imil, b))

  val constCompare = 
   fn (c1, c2) => 
      case (c1, c2) 
       of (RSel cc1,  RSel cc2)  => MU.Compare.constant (cc1, cc2)
        | (RCons cc1, RCons cc2) => MU.Compare.constant (cc1, cc2)
        | (RSel _,   _)          => LESS
        | (_,         RSel _)    => GREATER

  val constEquals = 
   fn (c1, c2) => 
      case (c1, c2) 
       of (RSel cc1,  RSel cc2)  => MU.Eq.constant (cc1, cc2)
        | (RCons cc1, RCons cc2) => MU.Eq.constant (cc1, cc2)
        | (_,   _)               => false

  val predicateCompare =
   fn (EP x1, EP x2) =>
      Compare.triple (Identifier.variableCompare, constCompare, Bool.compare) (x1, x2)

  val predicateEquals =
   fn (EP x1, EP x2) =>
      Equality.triple (Identifier.variableEqual, constEquals, Bool.equals) (x1, x2)

                              
  structure PS = SetF (struct 
                        type t = edgePredicate
                        val compare = predicateCompare
                       end)

  structure Debug :>
  sig
    val debugPass : Config.t -> bool
    val debugPassD : Config.Debug.debug
     (* Everything exported should be check debugPass *)
    val debugDo : PD.t * (unit -> unit) -> unit
    val prints : PD.t * string -> unit
    val printLayout : PD.t * Layout.t -> unit
    val printPredicateSet : PD.t * IMil.t * string * PS.t -> unit
    val printEdgeState : PD.t * IMil.t * (IMil.iBlock * IMil.iBlock) * PS.t * bool -> unit
    val printOrigInstr : PD.t * IMil.t * (IMil.iBlock * IMil.iBlock) * IMil.iInstr -> unit
    val printNewInstr  : PD.t * IMil.t * IMil.iInstr -> unit
    val layoutCfg : PD.t * IMil.t * IMil.iFunc * string -> unit
    val layoutTreeDot : PD.t * IMil.t * IMil.iFunc * string * IMil.iBlock Tree.t -> unit
  end =
  struct

    val (debugPassD, debugPass) = Config.Debug.mk (passname, "debug the Mil branch removal pass")

    val debugDo =
     fn (d, f) => if debugPass (PD.getConfig d) then f () else ()

    val prints =
     fn (d, s) => debugDo (d, fn () => print s)

    val printLayout =
     fn (d, l) => debugDo (d, fn () => LU.printLayout l)

    val debugShowPre =
     fn (d, imil, fname)  =>
        debugDo (d, fn () => 
                       if (Config.debugLevel (PD.getConfig d, passname)) > 0 then 
                         let
                           val () = prints (d, "before branch removal:\n")
                           val () = printLayout (d, IMil.IFunc.layout (imil, IMil.IFunc.getIFuncByName (imil, fname)))
                           val () = prints (d, "\n")
                         in ()
                         end
                       else 
                         let
                           val () = prints (d, "Branch removal: ")
                           val () = printLayout (d, IMil.Layout.var (imil, fname))
                           val () = prints (d, "\n")
                         in ()
                         end)
        
    val printVariable = 
     fn (d, imil, v) => 
        prints (d, Layout.toString (IMil.Layout.var (imil, v)))

    val printEdge = 
     fn (d, imil, (a, b)) =>
        prints (d, "[" ^ labelString (imil, a) ^ "->" ^ labelString (imil, b) ^ "] ")

    val printConst = 
     fn (d, imil, const) =>
        let
          val f = 
           fn (t, c) => 
            let
              val l = MilLayout.layoutConstant (PD.getConfig d, IMil.T.getSi imil, c)
              val s = Layout.toString l
            in prints (d, t^s)
            end
        in
          case const
           of RSel c  => f ("S.", c)
            | RCons c => f ("C.", c)
        end
  
    val printPredicate =
     fn (d, imil, EP (v, const, b)) =>
        let
          val () = prints (d, "(")
          val () = printVariable (d, imil, v)
          val () = if b then prints (d, " = ") else prints (d, " <> ")
          val () = printConst (d, imil, const)
          val () = prints (d, ")")
        in ()
        end

    val printPredicateSet =
     fn (d, imil, s, ps) =>
        debugDo (d, fn () => 
                       let
                         val () = prints (d, "PSSet:[" ^ s ^ "]")
                         val () = PS.foreach (ps, fn x => printPredicate (d, imil, x))
                         val () = prints (d, "\n")
                       in ()
                       end)

    val printEdgeState =
     fn (d, imil, e as (a, b), ps, impossible) =>
        debugDo (d, fn () => 
                       let
                         val () = if impossible then 
                                    prints (d, "impossible edge: ")
                                  else
                                    prints (d, "unknown edge: ")
                         val () = printEdge (d, imil, e)
                         val () = printPredicateSet (d, imil, labelString(imil, a) ^ "->" ^ labelString (imil, b), ps)
                         val instr = IMil.IBlock.getTransfer(imil, a)
                         val () = printLayout (d, IMil.IInstr.layout (imil, instr))
                         val () = prints (d, "\n")
                       in ()
                       end)

    val printOrigInstr = 
     fn (d, imil, e as (a, b), instr) =>
        debugDo (d, fn () => 
                       printLayout (d, L.seq [L.str "remove ", 
                                              L.str (labelString(imil, b)), 
                                              L.str " in ", 
                                              L.str (labelString(imil, a)),
                                              L.str "\n", 
                                              IMil.IInstr.layout (imil, instr)]))

    val printNewInstr =
     fn (d, imil, newinstr) =>
        debugDo (d, fn () => 
                       printLayout (d, L.seq [L.str("=>\n"), IMil.IInstr.layout (imil, newinstr), L.str("\n")]))

    val layoutCfg =
     fn (d, imil, ifunc, s) =>
        debugDo (d, fn () => 
                       let
                         val cn = ID.variableString'(IMil.IFunc.getFName (imil, ifunc))
                         val () = LU.writeLayout (IMil.IFunc.layout (imil, ifunc), cn ^ "_" ^ s ^ ".fun" )
                         val ()  = LU.writeLayout (IMil.IFunc.layoutDot (imil, ifunc), cn ^ "_" ^ s ^ ".dot" )
                       in ()
                       end)

    val layoutTreeDot =
     fn (d, imil, ifunc, s, t) =>
        let
          val f = 
           fn () => 
              let
                val cfgname = "dom" ^ ID.variableString'(IMil.IFunc.getFName (imil, ifunc)) ^ "_" ^ s ^ ".dot" 
                fun labelNode n = [Dot.NodeOption.Label[(labelString(imil, n), Dot.Center)], 
                                   Dot.NodeOption.Shape Dot.Ellipse]
                val graphOptions = [Dot.GraphOption.Size {width=8.5, height=10.0},
                                    Dot.GraphOption.Page {width=8.5, height=11.0},
                                    Dot.GraphOption.Orientation Dot.Landscape]
                val () = LU.writeLayout (Tree.layoutDot (t, {nodeOptions = labelNode, 
                                                             options = graphOptions, 
                                                             title = cfgname}), 
                                         cfgname)
              in ()
              end
        in  debugDo (d, f)
        end

  end (* structure Debug *)

  (*
   *	Split critical edges.  
   *    This guarantees that every branch with more than one out edge 
   *    immediately dominates all of its successors 
   *    (and hence each successor is in a 1:1 correspondence with the outedges).
   *)

  val splitCriticalEdges = IMil.IFunc.splitCriticalEdges

  (* Given a potential edge from a to b, extract predicates
   * from the edge.  Assumes that the CFG has had critical edges split.
   *
   * If a->b is a CFG edge then the edge (not just the source node)
   * must dominate the target (since otherwise there must be a path from
   * a to b which does not include a->b, and hence a must have an additional 
   * outedge and b must have an additional edge, which contradicts).
   *
   * If we have such an edge and the transfer is a case, we can extract
   * an equality predicate if the edge is a normal branch, or a set of
   * of inequality predicates if the edge is a default.
   *)
  val getEdgePredicates =
   fn (d, imil, e as (a, b)) =>
      let
        val bl = getLabel (imil, b)

        val doCase = 
         fn ({select, on, cases, default}) =>
            Try.try 
              (fn () => 
                  let
                    val v = Try.<@ MU.Operand.Dec.sVariable on
                    val construct = 
                        case select
                         of M.SeSum _    => RSel
                          | M.SeConstant => RCons
                    val doIt = 
                     fn ((n, t as M.T {block, ...}), (ps, found)) =>
                        (case (found, block = bl)
                          of (true, true) => 
                             let
                               val () = Chat.warn0 (d, "Multiple targets to same block!")
                             in Try.fail ()
                             end
                           | (true, false)  => (ps,                                         found)
                           | (false, true)  => (PS.insert (ps, EP (v, construct n, true)),  true)
                           | (false, false) => (PS.insert (ps, EP (v, construct n, false)), false))

                    val (ps, found) = Vector.fold (cases, (PS.empty, false), doIt)

                    val ps = 
                        (case (found, default)
                          of (true,  NONE)                          => ps
                           | (false, NONE)                          => Try.fail ()
                           | (false, SOME (M.T {block, arguments})) =>
                             if block = bl then ps else Try.fail ()
                           | (true,  SOME (M.T {block, arguments})) =>
                             if block = bl then
                               let
                                 val () = Chat.warn0 (d, "Multiple targets to same block (default)!")
                               in Try.fail ()
                               end
                             else
                               ps)
                  in ps
                  end)
            
        val ps = 
            let
              val psO =
                  case IMil.IBlock.getTransfer' (imil, a)
                   of M.TCase sw     => doCase sw
                    | _ => NONE
              val ps = Utils.Option.get (psO, PS.empty)
            in ps
            end
      in ps
      end

  (* Given a dominator tree on an edge split CFG, propagate predicates down 
   * the tree. The root node starts with no predicates.  Each child 
   * gets its parents predicates plus any predicates that can be 
   * inferred from its inedges.  *)
  val propagatePredicates =
   fn (d, imil, tree) =>
      let
        val gDict = ref LD.empty
        val rec doChild =
         fn (a, child as Tree.T (b, _), aSet) =>
            let
              val edgePs = getEdgePredicates (d, imil, (a, b))
              val ps = PS.union (aSet, edgePs)
              val l = getLabel (imil, b)
              val () = gDict := LD.insert (!gDict, l, ps)
            in doTree (child, ps)
            end
        and rec doTree =
         fn (Tree.T (a, children), mySet) =>
            Vector.foreach (children, fn child => doChild (a, child, mySet))
        val () = doTree (tree, PS.empty)
      in !gDict
      end

  (* Given a set of true predicates ps and a predicate p guarding
   * an edge, check whether p could possibly ever be true given ps.  *)
  val impossiblePredicate : PD.t * PS.t * edgePredicate -> bool = 
   fn (d, psset, EP (v1, c1, eq1)) =>
      PS.exists (psset, fn EP (v2, c2, eq2) => 
                           (case (v1 = v2, constEquals (c1, c2), eq1, eq2)
                             of (true, false, true, true) => true  (* x =  c1, x =  c2, c1 <> c2*)
                              | (true, true, false, true) => true  (* x <> c1, x =  c1 *)
                              | (true, true, true, false) => true  (* x =  c1, x <> c1 *)
                              | _                         => false))


  val impossibleEdge =
   fn (d, imil, sourcePredicates, e as (a, b)) =>
      let
        (* These predicates must all be true in order for the
         *  edge to be taken
         *)
        val edgePredicates = getEdgePredicates (d, imil, e)

        (* If any of the predicates that must be true in order
         * for the edge to be taken are constradicted by the predicates
         * known to be true in the source block, then the edge is impossible
         *)
        val impossible = PS.exists (edgePredicates, fn p => impossiblePredicate (d, sourcePredicates, p)) 
               
        val () = Debug.debugDo (d, fn () => Debug.printPredicateSet (d, imil, labelString(imil, a), sourcePredicates))
        val () = Debug.debugDo (d, fn () => Debug.printEdgeState(d, imil, e, edgePredicates, impossible))
                 
      in impossible
      end

  val removeImpossible =
   fn (d, imil, e as (a, b), sw, predicates) =>
      Try.exec
        (fn () => 
            let
              val () = Try.require (impossibleEdge (d, imil, predicates, e))

              val () = Debug.prints (d, "find impossible switch case instruction\n")
              val instr = IMil.IBlock.getTransfer (imil, a)
              val () = Debug.debugDo (d, fn () => Debug.printOrigInstr (d, imil, e, instr))
              val {select, on, cases, default} = sw
              val bl = getLabel (imil, b)
              val cases = 
                  let
                    val noteq = 
                     fn (_, M.T {block, arguments}) => block <> bl
                  in Vector.keepAll (cases, noteq)
                  end
              val default = 
                  case default
                   of SOME (M.T {block, arguments}) => 
                      if block = bl then NONE else default
                    | _                             => NONE

              val () = Click.rcbr d
              val t = 
                  if Vector.length cases > 0 orelse isSome default then
                    let
                      val sw = {select = select, on = on, cases = cases, default = default}
                      val t = M.TCase sw
                    in t
                    end
                  else
                    M.THalt (M.SConstant (MU.Sintp.int (PD.getConfig d, ~1)))
              val nmt = IMil.MTransfer t
              val () = IMil.IInstr.replaceMil (imil, instr, nmt)
              val () = Debug.debugDo (d, fn () => Debug.printNewInstr (d, imil, instr))
            in ()
            end)

  val trimEdge =
   fn (d, imil, dict, e as (a, b)) =>
      case LD.lookup (dict, getLabel (imil, a))
       of SOME psset => 
          (case IMil.IBlock.getTransfer' (imil, a)
            of M.TCase cs     => removeImpossible (d, imil, e, cs, psset)
             | _ => ())
        | _ => ()
      
  val checkBlockPS = 
   fn (d, imil, dict, a) =>
      let
        val edges = IMil.IBlock.outEdges (imil, a)
        val () = List.foreach (edges, fn e => trimEdge (d, imil, dict, e))
      in ()
      end

  (* Redundant conditional branch elimination.  The algorithm is as follows.
   * 1) Split critical edges. 
   * 2) Walk the dominator tree top down, building a global dictionary 
   *    G mapping labels to sets of predicates which are true on entry 
   *    to that label.  For each node B in the tree with immediate dominator
   *    A, G(B) contains the set of predicates which are true in A, plus 
   *    any predicates induced by the edge from A->B, if A->B is an edge
   *    in the cfg.
   * 3) Walk the dominator tree bottom up.  For every node A: 
   *    For every successor B of A:
   *      If the edge A->B is impossible, delete it.  An edge is impossible if
   *        a) x = c1 is in G(A), and x = c2 guards the edge, or
   *        b) x <> c1 is in G(A) and x = c1 guards the edge, or
   *        b) x = c1 is in G(A) and x <> c1 guards teh edge
   *)
  val rcbrFunction = 
   fn (d, imil, ifunc) => 
      let
        val () = Debug.debugDo (d, fn () => Debug.layoutCfg (d, imil, ifunc, "rcbr"))
        val dom = IMil.IFunc.getDomTree (imil, ifunc)
        val () = Debug.debugDo (d, fn () => Debug.layoutTreeDot (d, imil, ifunc, "rcbr", dom))
        val psDict = propagatePredicates (d, imil, dom)
        val () = Tree.foreachPost (dom, fn b => checkBlockPS (d, imil, psDict, b))
      in ()
      end

  val checkBlockPSExt : PD.t * IMil.t * (PS.t LD.t) * IMil.iBlock -> unit =
   fn (d, imil, psDict, a) =>
      let
        val config = PD.getConfig d

        val replaceCase =
         fn (a, b, c, t as {select, on, cases, default}) =>
            let
              val instr = IMil.IBlock.getTransfer (imil, a)
              val cl = getLabel (imil, c)

              val doTarget =
               fn (t as M.T {block, arguments}) => 
                  if block = cl then
                    M.T {block = cl, arguments = Vector.new0 ()}
                  else 
                    t

              val newcases = 
                  Utils.Vector.mapSecond (cases, doTarget)
              val newdefault = Option.map (default, doTarget)
              val instr = IMil.IBlock.getTransfer (imil, a)
              val oldInstrLayout = IMil.IInstr.layoutMil (imil, IMil.IInstr.getMil (imil, instr))
              val newinstr = IMil.MTransfer (M.TCase {select = select, on=on, cases=newcases, default=newdefault})
              val newInstrLayout = IMil.IInstr.layoutMil (imil, newinstr)
              val () = IMil.IInstr.replaceMil (imil, instr, newinstr)
              val () = Debug.debugDo (d, fn () => Debug.printLayout(d, L.seq[L.str "extension replace case:\n", 
                                                                             oldInstrLayout,
                                                                             L.str " =>\n",
                                                                             newInstrLayout]))
              val () = Click.prunePath d
            in ()
            end

        val replaceGoto =
         fn (a, b, c, t as M.T {block, arguments}) =>
            let
              val instr = IMil.IBlock.getTransfer (imil, a)
              val oldInstrLayout = IMil.IInstr.layoutMil (imil, IMil.IInstr.getMil (imil, instr))
              val newtarget = M.T {block=getLabel (imil, c), arguments=Vector.new0()}
              val newinstr = IMil.MTransfer (M.TGoto newtarget)
              val newInstrLayout = IMil.IInstr.layoutMil (imil, newinstr)
              val () = IMil.IInstr.replaceMil (imil, instr, newinstr)
              val () = Debug.debugDo (d, fn () => Debug.printLayout(d, L.seq[L.str "extension replace goto:\n", 
                                                                             oldInstrLayout,
                                                                             L.str " =>\n",
                                                                             newInstrLayout]))
              val () = Click.prunePath d
            in ()
            end

        (* Is (a, b) an acceptable candidate starting edge? *)
        val candidate =
            Try.lift
              (fn (a, b) => 
                  let
                    val bpara = IMil.IBlock.getParameters (imil, b)
                    val () = Try.require (IMil.IBlock.isEmpty (imil, b))
                    val () = Try.require (Vector.size bpara = 0)
                    (* Guard against degenerate cases *)
                    val caseOk = 
                     fn {select, on, cases, default} => 
                        (Vector.length cases > 1) orelse
                        ((Vector.length cases = 1) andalso
                         isSome default)
                    val () = 
                        (case IMil.IBlock.getTransfer'(imil, b)
                          of Mil.TGoto _      => Try.fail ()
                           | Mil.TCase sw     => Try.require (caseOk sw)
                           | Mil.TInterProc _ => Try.fail ()
                           | Mil.TReturn _    => Try.fail ()
                           | Mil.TCut _       => Try.fail ()
                           | Mil.THalt _      => Try.fail ())
                    val () = 
                        case IMil.IBlock.getTransfer' (imil, a)
                         of M.TGoto t      => ()
                          | M.TCase t      => ()
                          | M.TInterProc t => Try.fail ()
                          | M.TReturn t    => Try.fail ()
                          | M.TCut t       => Try.fail ()
                          | M.THalt _      => Try.fail ()
                  in ()
                  end)

        val compressPath = 
         fn (a, b, c) =>
            Try.exec 
              (fn () =>
                  let
                    val cpara = IMil.IBlock.getParameters (imil, c)
                    val () = Try.require (Vector.size cpara = 0)
                    val () = 
                        case IMil.IBlock.getTransfer' (imil, a)
                         of M.TGoto t      => replaceGoto (a, b, c, t)
                          | M.TCase t      => replaceCase (a, b, c, t)
                          | M.TInterProc t => ()
                          | M.TReturn t    => ()
                          | M.TCut t       => ()
                          | M.THalt _      => ()
                  in ()
                  end)

        val impossiblePath =
         fn (ps, a, b, c) =>
            let
              val bc_ps = getEdgePredicates (d, imil, (b, c))
            in impossibleEdge (d, imil, ps, (b, c))
            end

        val checkEdge =
         fn (a, b) => 
            Try.exec
              (fn () => 
                  let
                    val () = Try.<@ candidate (a, b)
                    val ps = 
                        let
                          val ab_ps = getEdgePredicates (d, imil, (a, b))
                        in case LD.lookup (psDict, getLabel (imil, a))
                            of SOME a_ps => PS.union(a_ps, ab_ps)
                             | NONE      => ab_ps
                        end
                    val cs = IMil.IBlock.succs (imil, b)
                    val cs = List.keepAll (cs, fn c => not (impossiblePath (ps, a, b, c)))
                    val () = 
                        (case cs
                          of [c] => compressPath (a, b, c)
                           | _   => ())
                  in ()
                  end)
            
        val () = List.foreach (IMil.IBlock.succs (imil, a), fn b => checkEdge (a, b))
      in ()
      end

  (* RCBR extension
   *
   * This optimization considers all triples of paths of the form a->b->c.
   * It considers the predicates that are known to be true in b if the edge
   * a->b is taken.  If only one outedge from b is possible given these facts,
   * and if b is a skippable block, then a is re-targeted directly to c.
   *
   * This optimization preserves the edge-split property.  It only fires if
   * b ends in a non-degenerative case.  Since b has multiple out edges,
   * c must therefore have a single in-edge, so targetting a to c cannot 
   * induce a critical edge.
   * example:
   * L1:
   *   ...
   *   goto L2;
   * L2:
   *   case v {10 => goto L3; ...}
   *
   * If v=10 in L1, then we could make L1 go straight to L3 instead of L2.
   *
   * simple algorithm:
   * Consider edge (a->b->c), if PS(b->c) is redundant in (PS(a) union PS(a->b)), 
   * and b is empty block only with transfer instruction, 
   * then we can make (a->c) directly.
   *)
  val prunePaths =
   fn (d, imil, ifunc) =>
      let
        val () = Debug.debugDo (d, fn () => Debug.layoutCfg (d, imil, ifunc, "ext"))
        val dom = IMil.IFunc.getDomTree (imil, ifunc)
        val () = Debug.debugDo (d, fn () => Debug.layoutTreeDot (d, imil, ifunc, "ext", dom))
        val psDict = propagatePredicates (d, imil, dom)
        val () = Tree.foreachPost (dom, fn b => checkBlockPSExt (d, imil, psDict, b))
      in ()
      end

  val optimize = 
   fn (d, m, imil, ifunc) =>
      let
        val () = splitCriticalEdges (imil, ifunc)
        val () = prunePaths (d, imil, ifunc)
        val () = rcbrFunction (d, imil, ifunc)
      in ()
      end

  fun program (imil, d) = 
      let
        val m  = IMil.T.unBuild imil
        val () = List.foreach (IMil.Enumerate.T.funcs imil, fn ifunc => optimize (d, m, imil, ifunc))
        val () = PD.report (d, passname)
      in ()
      end

  val description = {name        = passname,
                     description = "redudant condition branch removal",
                     inIr        = BothMil.irHelpers,
                     outIr       = BothMil.irHelpers,
                     mustBeAfter = [],
                     stats       = stats}

  val associates = {controls  = [],
                    debugs    = [Debug.debugPassD],
                    features  = [],
                    subPasses = []}

  val pass = Pass.mkOptPass (description, associates, BothMil.mkIMilPass program)

end
