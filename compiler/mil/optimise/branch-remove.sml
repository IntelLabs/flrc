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
  structure P   = M.Prims
  structure PD  = PassData
  structure L   = Layout
  structure LU  = LayoutUtils
  structure ID  = Identifier
  structure LD  = ID.LabelDict
  structure LDOM = MilCfg.LabelDominance
  structure I   = Identifier
  structure MU  = MilUtils
  structure ML  = MilLayout
  structure MP  = Mil.Prims
  structure MPU = MU.Prims.Utils
  structure VD  = Identifier.VariableDict
  structure UO  = Utils.Option
  structure IBlock = IMil.IBlock
  structure IFunc  = IMil.IFunc

  structure Chat = ChatF (struct
                            type env = PD.t
                            val extract = PD.getConfig
                            val name = passname
                            val indent = 2
                          end)

  structure Click =
  struct
    val stats = []
    val {stats, click = rcbr}
      = PD.clicker {stats = stats, passname = passname,
                    name = "ImpossibleEdges", desc = "Impossible branches removed"}
    val {stats, click = prunePath}
      = PD.clicker {stats = stats, passname = passname,
                    name = "PrunedPaths", desc = "Short correlated branches skipped"}
    val {stats, click = mergeBranches}
      = PD.clicker {stats = stats, passname = passname,
                    name = "MergeBranch", desc = "Merge similar branches of a boolean switch"}
  end   (*  structure Click *)

  val stats = Click.stats

  datatype const =
      RCons of M.constant
    | RSel  of M.constant

  (* EP (v, c, true)  => v = c
   * EP (v, c, false) => v <> c
   * EpCmp (nt, v1, c, v2) => v1 c (at type nt) v2
   *)
  datatype edgePredicate =
      EP of M.variable * const * bool
    | EpCmp of MP.numericTyp * MP.compareOp * M.variable * M.variable

  val getLabel = fn (imil, b) => #1 (IBlock.getLabel' (imil, b))
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
   fn (ep1, ep2) =>
      case (ep1, ep2)
       of (EP x1, EP x2) =>
          Compare.triple (Identifier.variableCompare, constCompare, Bool.compare) (x1, x2)
        | (EP _, EpCmp _) => LESS
        | (EpCmp _, EP _) => GREATER
        | (EpCmp x1, EpCmp x2) =>
          Compare.quad
            (MPU.Compare.numericTyp, MPU.Compare.compareOp, Identifier.variableCompare, Identifier.variableCompare)
            (x1, x2)

  val predicateEquals =
   fn (ep1, ep2) =>
      case (ep1, ep2)
       of (EP x1, EP x2) =>
          Equality.triple (Identifier.variableEqual, constEquals, Bool.equals) (x1, x2)
        | (EP _, EpCmp _) => false
        | (EpCmp _, EP _) => false
        | (EpCmp x1, EpCmp x2) =>
          Equality.quad (MPU.Eq.numericTyp, MPU.Eq.compareOp, Identifier.variableEqual, Identifier.variableEqual)
                        (x1, x2)

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
                           val () = printLayout (d, IFunc.layout (imil, IFunc.getIFuncByName (imil, fname)))
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
     fn (d, imil, ep) =>
        case ep
         of EP (v, const, b) =>
            let
              val () = prints (d, "(")
              val () = printVariable (d, imil, v)
              val () = if b then prints (d, " = ") else prints (d, " <> ")
              val () = printConst (d, imil, const)
              val () = prints (d, ")")
            in ()
            end
          | EpCmp (nt, cmp, v1, v2) =>
            let
              val () = prints (d, "(")
              val () = printVariable (d, imil, v1)
              val c = PD.getConfig d
              val () = prints (d, " " ^ MPU.ToString.numericTyp (c, nt) ^ MPU.ToString.compareOp (c, cmp) ^ " ")
              val () = printVariable (d, imil, v2)
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
                         val instr = IBlock.getTransfer(imil, a)
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
                         val cn = ID.variableString'(IFunc.getFName (imil, ifunc))
                         val () = LU.writeLayout (IFunc.layout (imil, ifunc), cn ^ "_" ^ s ^ ".fun" )
                         val ()  = LU.writeLayout (IFunc.layoutDot (imil, ifunc), cn ^ "_" ^ s ^ ".dot" )
                       in ()
                       end)

    val layoutTreeDot =
     fn (d, imil, ifunc, s, t) =>
        let
          val f =
           fn () =>
              let
                val cfgname = "dom" ^ ID.variableString'(IFunc.getFName (imil, ifunc)) ^ "_" ^ s ^ ".dot"
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

  val splitCriticalEdges = IFunc.splitCriticalEdges

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
                    val extra =
                        Try.try
                          (fn () =>
                              let
                                val i = Try.<@ IMil.Def.toIInstr (IMil.Def.get (imil, v))
                                val r = Try.<@ IMil.IInstr.toRhs i
                                val {prim, args, ...} = Try.<@ MU.Rhs.Dec.rhsPrim r
                                val p = Try.<@ MPU.Dec.T.prim prim
                                val {typ, operator} = Try.<@ MPU.Dec.Prim.pNumCompare p
                                val (o1, o2) = Try.V.doubleton args
                                val v1 = Try.<@ MU.Operand.Dec.sVariable o1
                                val v2 = Try.<@ MU.Operand.Dec.sVariable o2
                                val ept = EpCmp (typ, operator, v1, v2)
                                val epf = EpCmp (typ, MPU.CompareOp.invert operator, v2, v1)
                              in {t = ept, f = epf}
                              end)
                    val addOne =
                     fn (ps, v, n, tf) =>
                        let
                          val ps = PS.insert (ps, EP (v, construct n, tf))
                          val ps =
                              case (extra, n, tf)
                               of (SOME {t, f}, M.CBoolean true,  true ) => PS.insert (ps, t)
                                | (SOME {t, f}, M.CBoolean true,  false) => PS.insert (ps, f)
                                | (SOME {t, f}, M.CBoolean false, true ) => PS.insert (ps, f)
                                | (SOME {t, f}, M.CBoolean false, false) => PS.insert (ps, t)
                                | _                                      => ps
                        in ps
                        end
                    val doIt =
                     fn ((n, t as M.T {block, ...}), (ps, found)) =>
                        (case (found, block = bl)
                          of (true, true) =>
                             let
                               val () = Chat.warn0 (d, "Multiple targets to same block!")
                             in Try.fail ()
                             end
                           | (true, false)  => (ps,                       found)
                           | (false, true)  => (addOne (ps, v, n, true ), true )
                           | (false, false) => (addOne (ps, v, n, false), false))
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
              val pso =
                  case IBlock.getTransfer' (imil, a)
                   of M.TCase sw     => doCase sw
                    | _ => NONE
              val ps = Utils.Option.get (pso, PS.empty)
            in ps
            end
      in ps
      end

  (* Compute the transitive closure of comparison predicats.
   * That is, if (nt, op1, v1, v2) and (nt, op2, v2, v3) are in the set,
   * add (nt, op1;op2, v1, v3) to the set until the set no longer changes.
   *)

  val transitiveClosure =
   fn (d, ps) =>
      let
        val out = ref ps
        val combineOne =
         fn (ep1, ep2) =>
            Try.try
              (fn () =>
                  let
                    val (nt1, c1, v1, v2) = case ep1 of EP _ => Try.fail () | EpCmp x => x
                    val (nt2, c2, v3, v4) = case ep2 of EP _ => Try.fail () | EpCmp x => x
                    val () = Try.require (I.variableEqual (v2, v3) andalso MPU.Eq.numericTyp (nt1, nt2))
                    val () = Try.require (not (predicateEquals (ep1, ep2)))
                    val c =
                        case (c1, c2)
                         of (MP.CEq, MP.CEq) => MP.CEq
                          | (MP.CEq, MP.CNe) => MP.CNe
                          | (MP.CEq, MP.CLt) => MP.CLt
                          | (MP.CEq, MP.CLe) => MP.CLe
                          | (MP.CNe, MP.CEq) => MP.CNe
                          | (MP.CNe, MP.CNe) => Try.fail ()
                          | (MP.CNe, MP.CLt) => Try.fail ()
                          | (MP.CNe, MP.CLe) => Try.fail ()
                          | (MP.CLt, MP.CEq) => MP.CLt
                          | (MP.CLt, MP.CNe) => Try.fail ()
                          | (MP.CLt, MP.CLt) => MP.CLt
                          | (MP.CLt, MP.CLe) => MP.CLt
                          | (MP.CLe, MP.CEq) => MP.CLe
                          | (MP.CLe, MP.CNe) => Try.fail ()
                          | (MP.CLe, MP.CLt) => MP.CLt
                          | (MP.CLe, MP.CLe) => MP.CLe
                    val ep = EpCmp (nt1, c, v1, v4)
                  in ep
                  end)
        val rec loop1 =
         fn eps =>
            case eps
             of [] => ()
              | ep::eps => loop2 (ep, eps, PS.toList (!out))
        and loop2 =
         fn (ep1, eps1, eps2) =>
            case eps2
             of [] => loop1 eps1
              | ep2::eps2 =>
                (case combineOne (ep1, ep2)
                  of NONE => loop2 (ep1, eps1, eps2)
                   | SOME ep =>
                     if PS.member (!out, ep)
                     then loop2 (ep1, eps1, eps2)
                     else let val () = out := PS.insert (!out, ep) in loop2 (ep1, ep::eps1, eps2) end)
        val () = loop1 (PS.toList ps)
      in !out
      end

  (* Given a dominator tree on an edge split CFG, propagate predicates down
   * the tree. The root node starts with no predicates.  Each child
   * gets its parents predicates plus any predicates that can be
   * inferred from its inedges, and the transitive closure of comparison predicates.
   *)
  val propagatePredicates =
   fn (d, imil, tree) =>
      let
        val gDict = ref LD.empty
        val rec doChild =
         fn (a, child as Tree.T (b, _), aSet) =>
            let
              val edgePs = getEdgePredicates (d, imil, (a, b))
              val ps = transitiveClosure (d, PS.union (aSet, edgePs))
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
   fn (d, psset, ep1) =>
      let
        val chkOne =
         fn ep2 =>
            (case (ep1, ep2)
              of (EP (v1, c1, eq1), EP (v2, c2, eq2)) =>
                 (case (v1 = v2, constEquals (c1, c2), eq1, eq2)
                   of (true, false, true, true) => true  (* x =  c1, x =  c2, c1 <> c2*)
                    | (true, true, false, true) => true  (* x <> c1, x =  c1 *)
                    | (true, true, true, false) => true  (* x =  c1, x <> c1 *)
                    | _                         => false)
               | (EP _, EpCmp _) => false
               | (EpCmp _, EP _) => false
               | (EpCmp (nt1, c1, v1, v2), EpCmp (nt2, c2, v3, v4)) =>
                 MPU.Eq.numericTyp (nt1, nt2) andalso I.variableEqual (v1, v4) andalso
                 (case (c1, c2)
                   of (MP.CEq, MP.CNe) => (I.variableEqual (v1, v3) andalso I.variableEqual (v2, v4)) orelse
                                          (I.variableEqual (v1, v4) andalso I.variableEqual (v2, v3))
                    | (MP.CEq, MP.CLt) => (I.variableEqual (v1, v3) andalso I.variableEqual (v2, v4)) orelse
                                          (I.variableEqual (v1, v4) andalso I.variableEqual (v2, v3))
                    | (MP.CNe, MP.CEq) => (I.variableEqual (v1, v3) andalso I.variableEqual (v2, v4)) orelse
                                          (I.variableEqual (v1, v4) andalso I.variableEqual (v2, v3))
                    | (MP.CLt, MP.CLt) => I.variableEqual (v1, v4) andalso I.variableEqual (v2, v3)
                    | (MP.CLt, MP.CLe) => I.variableEqual (v1, v4) andalso I.variableEqual (v2, v3)
                    | (MP.CLt, MP.CEq) => (I.variableEqual (v1, v3) andalso I.variableEqual (v2, v4)) orelse
                                          (I.variableEqual (v1, v4) andalso I.variableEqual (v2, v3))
                    | (MP.CLe, MP.CLt) => I.variableEqual (v1, v4) andalso I.variableEqual (v2, v3)
                    | _                => false))
      in
        PS.exists (psset, chkOne)
      end


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
              val instr = IBlock.getTransfer (imil, a)
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
          (case IBlock.getTransfer' (imil, a)
            of M.TCase cs     => removeImpossible (d, imil, e, cs, psset)
             | _ => ())
        | _ => ()

  val checkBlockPS =
   fn (d, imil, dict, a) =>
      let
        val edges = IBlock.outEdges (imil, a)
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
        val dom = IFunc.getDomTree (imil, ifunc)
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
              val instr = IBlock.getTransfer (imil, a)
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
              val instr = IBlock.getTransfer (imil, a)
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
              val instr = IBlock.getTransfer (imil, a)
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
                    val bpara = IBlock.getParameters (imil, b)
                    val () = Try.require (IBlock.isEmpty (imil, b))
                    val () = Try.require (Vector.size bpara = 0)
                    (* Guard against degenerate cases *)
                    val caseOk =
                     fn {select, on, cases, default} =>
                        (Vector.length cases > 1) orelse
                        ((Vector.length cases = 1) andalso
                         isSome default)
                    val () =
                        (case IBlock.getTransfer'(imil, b)
                          of Mil.TGoto _      => Try.fail ()
                           | Mil.TCase sw     => Try.require (caseOk sw)
                           | Mil.TInterProc _ => Try.fail ()
                           | Mil.TReturn _    => Try.fail ()
                           | Mil.TCut _       => Try.fail ()
                           | Mil.THalt _      => Try.fail ())
                    val () =
                        case IBlock.getTransfer' (imil, a)
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
                    val cpara = IBlock.getParameters (imil, c)
                    val () = Try.require (Vector.size cpara = 0)
                    val () =
                        case IBlock.getTransfer' (imil, a)
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
                    val cs = IBlock.succs (imil, b)
                    val cs = List.keepAll (cs, fn c => not (impossiblePath (ps, a, b, c)))
                    val () =
                        (case cs
                          of [c] => compressPath (a, b, c)
                           | _   => ())
                  in ()
                  end)

        val () = List.foreach (IBlock.succs (imil, a), fn b => checkEdge (a, b))
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
        val dom = IFunc.getDomTree (imil, ifunc)
        val () = Debug.debugDo (d, fn () => Debug.layoutTreeDot (d, imil, ifunc, "ext", dom))
        val psDict = propagatePredicates (d, imil, dom)
        val () = Tree.foreachPost (dom, fn b => checkBlockPSExt (d, imil, psDict, b))
      in ()
      end

  (* Merge both branches of a boolean switch:
   *
   *  case b of true  => goto L1;
   *          | false => goto L2;
   *  L1: B1; goto L3(X);
   *  L2: B2; goto L3(Y);
   *
   *  == goto L1;
   *     B;
   *     case b of true  => goto L3(X);
   *             | false => goto L3(Y);
   *
   * where B is the result of merging identical instructions in B1 and B2.
   *
   * Under the following conditions:
   * 1. the case block is the only predecessor of L1 and L2.
   * 2. B1 and B2 are similar enough (with at most 3 mismatches, modulo alpha renaming).
   *)
  val mergeBranches =
    fn (d, imil, ifunc) =>
      let
        fun doCase (i, r) =
            let
              val config = PD.getConfig d
              val func = IMil.IInstr.getIFunc (imil, i)
              val b0   = IMil.IInstr.getIBlock (imil, i)
              val {on, trueBranch, falseBranch} = Try.<@ MU.Transfer.isBoolIf (M.TCase r)
              val M.T {block = l1, arguments = args1} = trueBranch
              val M.T {block = l2, arguments = args2} = falseBranch

              val () = Try.require (Vector.length args1 = 0)
              val () = Try.require (Vector.length args2 = 0)
              val () = Try.require (l1 <> l2)
              val b1 = IFunc.getBlockByLabel (imil, func, l1)
              val b1pd = IBlock.preds (imil, b1)
              val () = Try.require ((List.length b1pd = 1) andalso (hd b1pd = b0))
              val b2 = IFunc.getBlockByLabel (imil, func, l2)
              val b2pd = IBlock.preds (imil, b2)
              val () = Try.require ((List.length b2pd = 1) andalso (hd b2pd = b0))

              fun checkGoto (b1, b2) =
                  let
                    val M.T {block = l3, arguments = args1} = Try.<@ MU.Transfer.Dec.tGoto (IBlock.getTransfer' (imil, b1))
                    val M.T {block = l3', arguments = args2} = Try.<@ MU.Transfer.Dec.tGoto (IBlock.getTransfer' (imil, b2))
                    val () = Try.require (l3 = l3')
                  in
                    fn () =>
                      let
                        val t1 = M.T {block = l3, arguments = args1 }
                        val t2 = M.T {block = l3, arguments = args2 }
                        val tr = MU.Bool.ifT (config, on, { trueT = t1, falseT = t2 })
                      in tr
                      end
                  end

              fun checkReturn (b1, b2) =
                  let
                    val args1 = Try.<@ MU.Transfer.Dec.tReturn (IBlock.getTransfer' (imil, b1))
                    val args2 = Try.<@ MU.Transfer.Dec.tReturn (IBlock.getTransfer' (imil, b2))
                    val () = Try.require (Vector.length args1 = Vector.length args2)
                  in
                    fn () =>
                      let
                        val l3 = IMil.Var.labelFresh imil
                        val typs = Vector.map (args1, fn x => MilType.Typer.operand (config, IMil.T.getSi imil, x))
                        val args = Vector.map (typs, fn t => IMil.Var.new (imil, "rv_#", t, M.VkLocal))
                        val b3 = M.B { parameters = args,
                                       instructions = Vector.new0 (),
                                       transfer = M.TReturn (Vector.map (args, M.SVariable)) }
                        val _ = IBlock.build (imil, func, (l3, b3))
                        val t1 = M.T {block = l3, arguments = args1 }
                        val t2 = M.T {block = l3, arguments = args2 }
                        val tr = MU.Bool.ifT (config, on, { trueT = t1, falseT = t2 })
                      in
                        tr
                      end
                  end

              val trF = Try.<@ (Try.|| (checkGoto, checkReturn)) (b1, b2)

              fun checkBlock (b1, b2) =
                  let
                    val effects = Effect.fromList [Effect.InitRead, Effect.HeapRead]

                    fun checkEffect i = if Effect.subset (MU.Instruction.fx (config, i), effects)
                                          then SOME i else NONE

                    fun emptyBlk () = []
                    fun pushInstr (blk, instr) = instr :: blk

                    fun checkInstr state =
                      fn (SOME (M.I {dests = d1, rhs = rhs1, ... }),
                          SOME (M.I {dests = d2, rhs = rhs2, ... })) =>
                        let
                          val (vMap, aMap, blk) = state
                          val vMap = ref vMap (* vMap keeps track of variable equivalance *)
                          val aMap = ref aMap (* aMap keeps track of assumptions *)
                          val blk  = ref blk

                          fun newCondMov (u, v1, v2) = MU.Instruction.new (u, M.RhsPrim {
                              prim = P.Prim P.PCondMov, createThunks = false, typs = Vector.new0 (),
                              args = Vector.new3 (on, v1, v2) })

                          type 'a opt = (unit -> 'a) option

                          val when : (bool * ('a -> 'b) * 'a opt) -> 'b opt =
                            fn (b, f, x) => if b then Option.map (x, fn g => f o g) else NONE

                          val both : ('a opt * 'b opt) -> ('a * 'b) opt =
                            fn (x, y) => UO.map2 (x, y, fn (x, y) => fn () => (x (), y ()))

                          val checkVariable =
                            fn (v1, v2) => if v1 = v2 then SOME (fn () => v1) else
                              (case (VD.lookup (!vMap, v1), VD.lookup (!vMap, v2))
                                of (SOME i, SOME j) => if i = j then SOME (fn () => i) else NONE
                                 | (vi, vj) =>
                                  let
                                    fun f () =
                                      let
                                        val i = UO.out (vi, fn () => v1)
                                        val j = UO.out (vj, fn () => v2)
                                        val t = IMil.Var.typ (imil, i)
                                        val u = IMil.Var.new (imil, "cm_#", t, M.VkLocal)
                                        val () = aMap := VD.insert (!aMap, u, (M.SVariable i, M.SVariable j))
                                        val () = vMap := VD.insert (VD.insert (!vMap, i, u), j, u)
                                        val () = blk  := pushInstr (!blk, newCondMov (u, M.SVariable i, M.SVariable j))
                                      in
                                        u
                                      end
                                  in SOME f
                                  end)

                          val checkVariable' =
                            fn (NONE, NONE) => SOME (fn () => NONE)
                             | (SOME v1, SOME v2) => Option.map (checkVariable (v1, v2), fn f => fn () => SOME (f ()))
                             | _ => NONE

                          val checkOperand =
                            fn (o1, o2) =>
                              (case (o1, o2)
                                of (M.SVariable v1, M.SVariable v2) =>
                                  Option.map (checkVariable (v1, v2), fn f => M.SVariable o f)
                                 | (M.SConstant c1, M.SConstant c2) =>
                                  if MU.Constant.eq (c1, c2) then SOME (fn () => M.SConstant c1) else
                                    let
                                      fun f () =
                                        let
                                          val t = MU.Constant.typOf (config, c1)
                                          val u = IMil.Var.new (imil, "cm_#", t, M.VkLocal)
                                          val () = aMap := VD.insert (!aMap, u, (o1, o2))
                                          val () = blk  := pushInstr (!blk, newCondMov (u, o1, o2))
                                        in
                                          M.SVariable u
                                        end
                                    in SOME f
                                    end
                                | _ => NONE)

                          val checkOperands = fn (a1, a2) =>
                              if Vector.length a1 = Vector.length a2
                                then Option.map (UO.distributeV (Vector.map2 (a1, a2, checkOperand)),
                                                 fn ops => fn () => Vector.map (ops, fn f => f ()))
                                else NONE

                          val checkField =
                            fn (M.FiVariable o1, M.FiVariable o2) =>
                              Option.map (checkOperand (o1, o2), fn f => M.FiVariable o f)
                             | (f1, f2) =>
                              if MU.FieldIdentifier.eq (f1, f2) then SOME (fn () => f1) else NONE

                          val rhs : M.rhs opt =
                              (case (rhs1, rhs2)
                                of (M.RhsSimple o1, M.RhsSimple o2) =>
                                  when (true, M.RhsSimple, checkOperand (o1, o2))
                                 | (M.RhsPrim { prim = p1, args = a1, createThunks = c1, typs = t1 },
                                    M.RhsPrim { prim = p2, args = a2, ... }) =>
                                  let
                                    fun rhs args = M.RhsPrim
                                        { prim = p1, args = args, createThunks = c1, typs = t1 }
                                  in
                                    when (p1 = p2, rhs, checkOperands (a1, a2))
                                  end
                                 | (M.RhsTupleSub (M.TF { tup = v1, field = f1, tupDesc = d1 }),
                                    M.RhsTupleSub (M.TF { tup = v2, field = f2, tupDesc = d2 })) =>
                                  let
                                    val eqD = MU.TupleDescriptor.eq (d1, d2)
                                    fun rhs (tup, field) = M.RhsTupleSub (M.TF
                                        { tup = tup, field = field, tupDesc = d1 })
                                  in
                                    when (eqD, rhs, both (checkVariable (v1, v2), checkField (f1, f2)))
                                  end
                                 | (M.RhsTupleSet { tupField = M.TF { tup = v1, field = f1, tupDesc = d1 },
                                                    ofVal = o1 },
                                    M.RhsTupleSet { tupField = M.TF { tup = v2, field = f2, tupDesc = d2 },
                                                    ofVal = o2 }) =>
                                  let
                                    val eqD = MU.TupleDescriptor.eq (d1, d2)
                                    fun rhs (tup, (field, ofVal)) = M.RhsTupleSet
                                        { tupField = M.TF { tup = tup, field = field, tupDesc = d1 },
                                          ofVal = ofVal }
                                  in
                                    when (eqD, rhs, both (checkVariable (v1, v2),
                                          both (checkField (f1, f2), checkOperand (o1, o2))))
                                  end
                                 | (M.RhsIdxGet { idx = v1, ofVal = o1 },
                                    M.RhsIdxGet { idx = v2, ofVal = o2 }) =>
                                  let
                                    fun rhs (idx, ofVal) = M.RhsIdxGet { idx = idx, ofVal = ofVal }
                                  in
                                    when (true, rhs, both (checkVariable (v1, v2), checkOperand (o1, o2)))
                                  end
                                 | (M.RhsCont l1, M.RhsCont l2) =>
                                  when (l1 = l2, M.RhsCont, SOME (fn () => l1))
                                 | (M.RhsThunkGetFv { thunk = v1, idx = i1, typ = t1, fvs = fk1 },
                                    M.RhsThunkGetFv { thunk = v2, idx = i2, typ = t2, fvs = fk2 }) =>
                                  let
                                    val eqFK = Vector.forall2 (fk1, fk2, MU.FieldKind.eq)
                                    fun rhs v = M.RhsThunkGetFv { thunk = v, idx = i1, typ = t1, fvs = fk1 }
                                  in
                                    when (i1 = i2 andalso eqFK, rhs, checkVariable (v1, v2))
                                  end
                                 | (M.RhsThunkGetValue { thunk = v1, typ = fk1},
                                    M.RhsThunkGetValue { thunk = v2, typ = fk2}) =>
                                  let
                                    val eqFK = MU.FieldKind.eq(fk1, fk2)
                                    fun rhs v = M.RhsThunkGetValue { thunk = v, typ = fk1 }
                                  in
                                    when (eqFK, rhs, checkVariable (v1, v2))
                                  end
                                 | (M.RhsThunkValue { thunk = v1, typ = fk1, ofVal = o1},
                                    M.RhsThunkValue { thunk = v2, typ = fk2, ofVal = o2}) =>
                                  let
                                    val eqFK = MU.FieldKind.eq(fk1, fk2)
                                    fun rhs (v, ofVal) = M.RhsThunkValue { thunk = v, typ = fk1, ofVal = ofVal }
                                  in
                                    when (eqFK, rhs, both (checkVariable' (v1, v2), checkOperand (o1, o2)))
                                  end
                                 | (M.RhsClosureMk { fvs = fk1 },
                                    M.RhsClosureMk { fvs = fk2 }) =>
                                  let
                                    val eqFK = Vector.length fk1 = Vector.length fk2 andalso
                                               Vector.forall2 (fk1, fk2, MU.FieldKind.eq)
                                    fun rhs fk = M.RhsClosureMk { fvs = fk }
                                  in
                                    when (eqFK, rhs, SOME (fn () => fk1))
                                  end
                                 | (M.RhsClosureInit { cls = v1, code = c1, fvs = fv1 },
                                    M.RhsClosureInit { cls = v2, code = c2, fvs = fv2 }) =>
                                  let
                                    val (fk1, o1) = Vector.unzip fv1
                                    val (fk2, o2) = Vector.unzip fv2
                                    val eqCFK = c1 = c2 andalso Vector.length fk1 = Vector.length fk2 andalso
                                                Vector.forall2 (fk1, fk2, MU.FieldKind.eq)
                                    fun rhs (v, os) = M.RhsClosureInit { cls = v, code = c1, fvs = Vector.zip (fk1, os) }
                                  in
                                    when (eqCFK, rhs, both (checkVariable' (v1, v2), checkOperands (o1, o2)))
                                  end
                                 | (M.RhsClosureGetFv { cls = v1, idx = i1, fvs = fk1 },
                                    M.RhsClosureGetFv { cls = v2, idx = i2, fvs = fk2 }) =>
                                  let
                                    val eqFK = Vector.length fk1 = Vector.length fk2 andalso
                                               Vector.forall2 (fk1, fk2, MU.FieldKind.eq)
                                    fun rhs v = M.RhsClosureGetFv { cls = v, idx = i1, fvs = fk1 }
                                  in
                                    when (i1 = i2 andalso eqFK, rhs, checkVariable (v1, v2))
                                  end
                                 | (M.RhsEnum { tag = o1, typ = fk1 }, M.RhsEnum { tag = o2, typ = fk2 }) =>
                                  let
                                    fun rhs tag = M.RhsEnum { tag = tag, typ = fk1 }
                                  in
                                    when (MU.FieldKind.eq (fk1, fk2), rhs, checkOperand (o1, o2))
                                  end
                                 | (M.RhsSumProj { sum = v1, tag = t1, idx = i1, typs = fk1 },
                                    M.RhsSumProj { sum = v2, tag = t2, idx = i2, typs = fk2 }) =>
                                  let
                                    val eqC = MU.Constant.eq (t1, t2)
                                    val eqFK = Vector.forall2 (fk1, fk2, MU.FieldKind.eq)
                                    fun rhs v = M.RhsSumProj { sum = v, tag = t1, idx = i1, typs = fk1 }
                                  in
                                    when (i1 = i2 andalso eqC andalso eqFK, rhs, checkVariable (v1, v2))
                                  end
                                 | (M.RhsSumGetTag { sum = v1, typ = fk1 },
                                    M.RhsSumGetTag { sum = v2, typ = fk2 }) =>
                                  let
                                    fun rhs v = M.RhsSumGetTag { sum = v, typ = fk1 }
                                  in
                                    when (MU.FieldKind.eq (fk1, fk2), rhs, checkVariable (v1, v2))
                                  end
                                 | _ => NONE)

                          fun match rhs =
                              let
                                fun updateMap (v1, v2) =
                                    let
                                      val u  = IMil.Var.clone (imil, v1)
                                      val () = vMap := VD.insert (VD.insert (!vMap, v1, u), v2, u)
                                    in
                                      u
                                    end
                                val dest = Vector.map2 (d1, d2, updateMap)
                                val () = blk := pushInstr (!blk, MU.Instruction.new' (dest, rhs))
                                fun reassign (u, v) =
                                    blk := pushInstr (!blk, MU.Instruction.new (u, M.RhsSimple (M.SVariable v)))
                                val () = Vector.foreach2 (d1, dest, reassign)
                                val () = Vector.foreach2 (d2, dest, reassign)
                              in
                                (rhs, (!vMap, !aMap, !blk))
                              end
                        in
                          Option.map (when (Vector.length d1 = Vector.length d2, match, rhs), fn f => f ())
                        end
                       | _ => NONE

                    fun checkIInstrs (state, mismatched, i1, i2) =
                        let
                          val (vMap, aMap, blk) = state
                        in
                          if mismatched > 3 then NONE
                          else case (i1, i2)
                            of (NONE, NONE) => SOME (mismatched, blk)
                             | _ =>
                              let
                                val mi1 = UO.bind (i1, IMil.IInstr.toInstruction)
                                val mi2 = UO.bind (i2, IMil.IInstr.toInstruction)
                                val i1' = UO.bind (i1, fn i => IMil.IInstr.next (imil, i))
                                val i2' = UO.bind (i2, fn i => IMil.IInstr.next (imil, i))
                                val ins = checkInstr state (mi1, mi2)
                              in
                                case ins
                                  of SOME (x, state) => checkIInstrs (state, mismatched, i1', i2')
                                   | NONE =>
                                    let
                                      val mismatched = mismatched + 1
                                      fun nextInstrs (i1, i2) = fn instr =>
                                          checkIInstrs ((vMap, aMap, pushInstr (blk, instr)), mismatched, i1, i2)
                                      val l = UO.bind (UO.bind (mi1, checkEffect), nextInstrs (i1', i2))
                                      val r = UO.bind (UO.bind (mi2, checkEffect), nextInstrs (i1, i2'))
                                    in
                                      case (l, r)
                                        of (SOME (m, _), SOME (n, _)) => if m < n then l else r
                                         | (SOME _, _) => l
                                         | (_, SOME _) => r
                                         | _ => NONE
                                    end
                              end
                        end

                    val initState = (VD.empty, VD.empty, [])
                  in
                    checkIInstrs (initState, 0, IBlock.getFirst (imil, b1), IBlock.getFirst (imil, b2))
                  end

              val (_, blk) = Try.<@ checkBlock (b1, b2)
              val _ = List.fold (blk, i, fn (j, i) => IMil.IInstr.insertBefore (imil, j, i))
              val tr = trF ()
              val () = IBlock.replaceTransfer (imil, b0, tr)
              val () = IBlock.delete (imil, b1)
              val () = IBlock.delete (imil, b2)
            in
              Click.mergeBranches d
            end

        fun doBlock b =
            case IBlock.getTransfer' (imil, b)
              of M.TCase sw => doCase (IBlock.getTransfer (imil, b), sw)
               | _ => ()

        val dom = IFunc.getDomTree (imil, ifunc)
        val () = Tree.foreachPost (dom, fn b => Try.exec (fn () => doBlock b))
      in
        ()
      end

  val optimize =
   fn (d, m, imil, ifunc) =>
      let
        val () = splitCriticalEdges (imil, ifunc)
        val () = prunePaths (d, imil, ifunc)
        val () = rcbrFunction (d, imil, ifunc)
        val () = mergeBranches (d, imil, ifunc)
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
