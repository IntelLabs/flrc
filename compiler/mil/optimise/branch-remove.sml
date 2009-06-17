
(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, January 2009 *)

(* Redudant Conditional Branch Removal 
 * TODO:
 *   1) add support for TCase 
 *)

signature MIL_REMOVE_BRANCH =
sig
  val pass : (BothMil.t, BothMil.t) Pass.t
end
                             
structure MilRemoveBranch :> MIL_REMOVE_BRANCH = 
struct

  val passname = "MilRemoveBranch"

  val stats = [("RemoveBranch", "Redudant Conditional Branch Removal")]

  fun fail (f, m) = Fail.fail ("branch-remove.sml", f, m)
  fun assert (f, m, b) = if b then fail (f, m) else ()

  structure M = Mil
  structure PD = PassData
  structure LU  = LayoutUtils
  structure ID  = Identifier
  structure LD = ID.LabelDict

  datatype psCond =
           rcons of M.constant
         | rname of ID.name

  fun printOpnd opnd =
      (case opnd
        of M.SVariable v => print (ID.variableString' v)
         | M.SConstant c => 
           (case c 
             of M.CName n => print ("SConstant " ^ ID.nameString' n)
              | M.CRat n => fail ("SConstant ", "rat" )
              | M.CInteger n => fail ("SConstant ", "integer" )
              | M.CIntegral n => print ("SConstant " ^ IntArb.stringOf n )
              | M.CFloat n => fail ("SConstant ", "float" )
              | M.CDouble n => fail ("SConstant ", "double" )
              | M.CViVector n => fail ("SConstant ", "vivector" )
              | M.CViMask n => fail ("SConstant ", "vimask" )
              | M.CPok n => fail ("SConstant ", "pok" )
              | M.COptionSetEmpty => fail ("SConstant ", "optionsetempty" )
              | CTypePH => fail ("SConstant ", "typeph" )))
      
  fun printOpndOp opndo =
      case opndo
       of SOME opnd => printOpnd opnd
        | NONE => print ("others ")

  fun printOpndOpPair (opndo1, opndo2, eq) =
      let
        val () = printOpndOp opndo1
        val () = if eq = true then print " == " else print " <> "
        val () = printOpndOp opndo2
        val () = print "\n"
      in ()
      end

  fun printCond cond = 
      case cond
       of rname n => print (ID.nameString' n)
        | rcons c => (case c
                       of M.CName n => print (ID.nameString' n)
                        | M.CIntegral i => print(IntArb.stringOf i)
                        | M.CFloat f => print(Real32.toString f)
                        | M.CDouble d => print (Real64.toString d)
                        | _ => fail ("rcons", "unknown"))
  
  fun printCondOp condo =
      case condo
       of SOME cond => printCond cond
        | NONE => print ("unknown")

  fun printCondOpPair (condo1, condo2, eq) =
      let
        val () = printCondOp condo1
        val () = if eq = true then print " == " else print " <> "
        val () = printCondOp condo2
        val () = print "\n"
      in ()
      end

  fun eqConstant (c1, c2) =
      case c1 
       of M.CName n1 => (case c2
                         of M.CName n2 => n1 = n2
                          | _ => false)
        | M.CIntegral i1 => (case c2
                             of M.CIntegral i2 => IntArb.equals(i1, i2)
                              | _ => false)
        | M.CFloat f1 => (case c2
                           of M.CFloat f2 => Real32.compare(f1, f2) = EQUAL
                            | _ => false)
        | M.CDouble d1 => (case c2
                            of M.CDouble d2 => Real64.compare(d1, d2) = EQUAL
                             | _ => false)
        | _ => false

  fun eqOpnd (o1, o2) =
      case o1
       of M.SVariable v1 => (case o2
                              of M.SVariable v2 => v1 = v2
                               | _ => false)
        | M.SConstant c1 => (case o2
                              of M.SConstant c2 => eqConstant(c1, c2)
                              | _ => false)

  fun eqOpndOp (o1o, o2o) =
      case o1o
       of SOME o1 => (case o2o 
                       of SOME o2 => eqOpnd (o1, o2)
                        | NONE => false)
        | NONE => (case o2o 
                    of SOME o2 => false
                     | NONE => true)

  fun eqCond (c1, c2) =
      case c1
       of rname n1 => (case c2
                       of rname n2 => n1 = n2
                        | _ => false)
        | rcons cc1 => (case c2
                        of rcons cc2 => eqConstant(cc1, cc2)
                         | _ => false)

  fun eqCondOp (c1o, c2o) =
      case c1o 
       of SOME c1 => (case c2o
                       of SOME c2 => eqCond (c1, c2)
                        | NONE => false)
        | NONE => (case c2o
                    of SOME c2 => false
                     | NONE => true)

  fun PSCompare ((o1, n1, b1, s1), (o2, n2, b2, s2)) = 
      if s1 = s2 
         andalso eqOpndOp(o1, o2) 
         andalso eqCondOp(n1, n2) 
         andalso b1 = b2 
      then EQUAL else LESS
                              
  structure PSSet = SetF (struct 
                            (* variable, name, equal or not , isSwitch*)
                            type t = M.operand option * psCond option * bool * bool
                            val compare = PSCompare 
                          end)

  structure Chat = ChatF (struct 
                            type env = PD.t
                            val extract = PD.getConfig
                            val name = passname
                            val indent = 0
                          end)

  type edge = IMil.iBlock * IMil.iBlock

  datatype psState =
           Redundant  (* a->b while c=x and ps (c=x, ...)*)
         | Impossible (* a->b while c=x and ps (c=y, ...*)
         | Unknown    (* a->b while c=x and ps (no c )*)

  val (debugPassD, debugPass) =
      Config.Debug.mk (passname, "debug the Mil branch removal pass")

  val gDict = ref LD.empty

  fun debugPrint () = false
  fun dbgPrint s = if debugPrint() then print s else ()

  fun debugShowPre (d, imil, fname)  = 
      if Config.debug andalso debugPass (PD.getConfig d) then
        if (Config.debugLevel (PD.getConfig d, passname)) > 0 then 
          let
            val () = print ("before branch removal:\n")
            val l = IMil.IFunc.layout (imil, IMil.IFunc.getIFuncByName (imil, fname))
            val () = LU.printLayout l
            val () = print "\n"
          in ()
          end
        else 
          let
            val () = dbgPrint ("Branch removal: ")
            val l = IMil.Layout.var (imil, fname)
            val () = LU.printLayout l
            val () = print "\n"
          in ()
          end
        else ()

  fun debugShowPost (d, imil)  = 
      if Config.debug andalso 
         debugPass (PD.getConfig d) andalso
         (Config.debugLevel (PD.getConfig d, passname)) > 1 then 
        let
          val () = print ("after branch removal:\n")
(*          val mil = IMil.unBuild imil
          val () = MilLayout.printGlobalsOnly (PD.getConfig d, mil)*)
          val () = print "\n"
        in ()
        end
      else ()

  fun getLabel (imil, b) = #1 (IMil.IBlock.getLabel' (imil, b))

  fun printEdge (imil, (a, b)) =
      if debugPrint() then
        let
          val e = IMil.IBlock.getTransfer (imil, a)
          val la = getLabel (imil, a)
          val lb = getLabel (imil, b)
          val sa = ID.labelString la
          val sb = ID.labelString lb
          val () = print ("[" ^ sa ^ "->" ^ sb ^ "] ")
        in ()
        end
      else ()

  fun printBlockPS (imil, (a, es)) = 
      if debugPrint() then
        let
          val la = getLabel (imil, a)
          val sa = ID.labelString la
          val () = print ("[" ^ sa ^ "] ")
          fun printEdge' (e) = printEdge (imil, e)
          val () = List.foreach (es, printEdge')
          val () = print ("\n")
        in ()
        end
      else ()

  fun printPS (opndo, condo, b, s) =
      if debugPrint() then
        let
          val () = print ("(")
          val () = printOpndOp opndo                     
          val () = if b then print (" = ") else print (" <> ")
          val () = printCondOp condo
          val () = print (")")
        in ()
        end
      else ()

  fun printPSSet (k, ps) = 
      if debugPrint() then
        let
          val () = print ("Block Predict Set Dict:")
          val () = print ("[" ^ (ID.labelString k) ^ "]")
          val () = PSSet.foreach (ps, printPS)
          val () = print ("\n")
        in ()
        end
      else ()

  fun printEdgePSState (imil, e as (a, b), ps, state) =
      if debugPrint() then
        let
          val () = case state 
                    of Redundant => print ("redundant edge: ")
                     | Unknown => print ("unknown edge: ")
                     | Impossible => print ("impossible edge: ")
                                     
          val () = printEdge (imil, e)
          val () = printPSSet (getLabel(imil, a), ps)
                   
          val instr = IMil.IBlock.getTransfer(imil, a)
          val () = LU.printLayout (IMil.IInstr.layout (imil, instr))
          val () = print("\n")
        in ()
        end
      else ()

  fun printOrigInstr (imil, e as (a, b), instr) =
      if debugPrint() then
        let
          val () = print ("\n")
          val tl = ID.labelString(getLabel(imil, b))
          val sl = ID.labelString(getLabel(imil, a))
          val () = print ("remove target " ^ tl ^ " in " ^ sl ^ "\n")
          val () = LU.printLayout (IMil.IInstr.layout (imil, instr))
        in ()
        end
      else ()

  fun printNewInstr (imil, newinstr) =
      if debugPrint() then
        let
          val () = print ("replace with new PSumCase instruction\n")
          val () = LU.printLayout (IMil.IInstr.layoutMil (imil, newinstr))
          val () = print ("\n")
        in ()
        end
      else ()

  fun layoutCfg (imil, cfg) =
      if debugPrint() then
        let
          val cn = ID.variableString'(IMil.IFunc.getFName (imil, cfg))
          val l = IMil.IFunc.layout (imil, cfg)
          val cfgname = "cfg" ^ cn ^ ".cfg" 
          val () = LU.writeLayout (l, cfgname)
                   
          val ld = IMil.IFunc.layoutDot (imil, cfg)
          val dotname = "cfg" ^ cn ^ ".dot" 
          val ()  = LU.writeLayout (ld, dotname)

        in ()
        end
      else ()

  (* critical edge split *)
(*
1)	Split critical edges.  
    This guarantees that every branch with more than one out edge 
    immediately dominates all of its successors 
    (and hence each successor is in a 1:1 correspondence with the outedges).
*)

  fun splitCriticalEdge (imil, cfg) =
      let
        fun splitBlockCE b =
            let
              fun isCritical (e as (a, b)) =
                  ((List.length(IMil.IBlock.outEdges (imil, a)) > 1)
                   andalso (List.length(IMil.IBlock.inEdges (imil, b)) > 1))
                  
              fun findInCE b =
                  List.keepAll (IMil.IBlock.inEdges(imil, b), isCritical)
                  
              fun splitEdge e = 
                  let
                    val bo = IMil.IBlock.splitEdge (imil, e)
                  in ()
                  end
            in
              List.foreach (findInCE b, splitEdge)
            end
      in 
        List.foreach(IMil.IFunc.getBlocks(imil, cfg), splitBlockCE)
      end

  (* redundant conditional branch removal *)
  fun getTransMil (imil, b) 
    = IMil.IInstr.getMil (imil, IMil.IBlock.getTransfer (imil, b))
              
  fun getPSumCase (imil, e as (a, b)) =
      case getTransMil(imil, a)
       of IMil.MTransfer t =>
          (case t
            of M.TPSumCase ns => SOME ns
             | _ => NONE)
        | _ => NONE 

  fun getTCase (imil, e as (a, b)) =
      case getTransMil(imil, a)
       of IMil.MTransfer t =>
          (case t
            of M.TCase cs => SOME cs
             | _ => NONE)
        | _ => NONE

  fun isPSumCase (imil, a, b) = isSome (getPSumCase (imil, (a, b)))

  fun isTCase (imil, a, b) = isSome (getTCase (imil, (a, b)))

  fun getTransOpnd (imil, a, b) = 
      let
        fun getSwitchOpnd ({on, cases, default}) = 
            case on
             of M.SVariable v => SOME on
              | M.SConstant c => SOME on

      in case getTransMil(imil, a)
          of IMil.MTransfer t =>
             (case t
               of M.TPSumCase ns => getSwitchOpnd ns
                | M.TCase sw => getSwitchOpnd sw
                | _ => NONE)
           | _ => NONE
      end

  (* create edge PS set, variable set
   * (variable, (equal? (true or false), condition))
   *)
  fun getEdgePS (imil, e as (a, b)) =
      let
        (* PSumCase or TCase *)
        fun getPCasePS (imil, a, b, {on, cases, default}) =
            let
              val opnd = getTransOpnd (imil, a, b)
              val bl = getLabel (imil, b)
              fun eq (n, M.T {block, arguments}) = block = bl
              val item = Vector.peek (cases, eq)
            in case item
                of SOME (n, t) => SOME (SOME on, SOME (rname n), true, true)
                 | _ => NONE
            end

        fun getTCasePS (imil, a, b, {on, cases, default}) =
            let
              val opnd = getTransOpnd (imil, a, b)
              val bl = getLabel (imil, b)
              fun eq (n, M.T {block, arguments}) = block = bl
              val item = Vector.peek (cases, eq)
            in case item
                of SOME (n, t) => SOME (SOME on, SOME (rcons n), true, true)
                 | _ => NONE
            end

        val pso = case getTransMil (imil, a)
                  of IMil.MTransfer t =>
                     (case t
                       of M.TPSumCase ns => getPCasePS (imil, a, b, ns)
                        | M.TCase sw => getTCasePS (imil, a, b, sw)
                        | _ => SOME (NONE, NONE, false, false) )
                   | _ => NONE
(*
        val () = printEdge (imil, e)
        val () = case pso
                  of SOME ps => printPS ps
                   | NONE => print "none"
        val () = print "\n"
*)
      in pso
      end

  fun getEdgePSSet (imil, e) : PSSet.t =
      case getEdgePS (imil, e)
       of SOME it => PSSet.insert (PSSet.empty, it)
        | _ => PSSet.empty

  fun getDomTreeEdges (Tree.T (a, v)) = 
      let
        fun getDomEdges (b, []) = []
          | getDomEdges (b, (Tree.T (a, v))::xs) = 
            (b, a)::getDomEdges(a, Vector.toList v)@getDomEdges(b, xs)
      in 
        getDomEdges (a, Vector.toList v)
      end

  (* propagate PS on dom tree *)
  fun propagatePS (imil, tree) =
      let
        val domEdges = getDomTreeEdges tree
        fun isDomEdge e = List.exists (domEdges, fn x => x = e)

        fun travNode (a) =
            let
              val label = getLabel(imil, a)
              val inEdges = List.keepAll(IMil.IBlock.inEdges(imil, a), isDomEdge)
(*              val inEdges = IMil.IBlock.inEdges(imil, a)*)

              fun foldf (e as (p, a), ps) =
                  let
                    val parentSet = LD.lookup(!gDict, getLabel(imil, p))
                    val thisSet = getEdgePSSet(imil, (p, a))
                                  
                    val curPS = if isSome (parentSet)
                                then PSSet.union(valOf parentSet, thisSet)
                                else thisSet
                  in PSSet.union(ps, curPS)
                  end

              val ps = List.fold(inEdges,
                                 PSSet.empty,
                                 foldf)

              val () = gDict := LD.insert (!gDict, label, ps)

            in fn () => ()
            end
      in
        Tree.traverse(tree, travNode)
      end

  fun getEdgePSState (imil, e as (a, b), ps : PSSet.t) =
      let
        val epso = getEdgePS (imil, e)

(*        fun isRedundant (item, ps) = PSSet.member (ps, item)*)
   
        fun maybeRedundant ((eopnd, en, eb, es), ps) =
            let
              fun hasSameOpnd (item as (opnd, n, b, s)) = eqOpndOp(opnd, eopnd)
              fun hasSameCond (item as (v, n, b, s)) = eqCondOp (n, en)
              fun maybeRedundant' x = (hasSameOpnd x) andalso (hasSameCond x)
              val rtn = PSSet.exists (ps, maybeRedundant')

              val () = if rtn then dbgPrint "maybeRedundant\n" else ()
            in 
              rtn
            end

        fun maybeImp ((eopnd, en, eb, es), ps) = (* impossible *)
            let
              fun hasSameOpnd (item as (opnd, n, b, s)) = eqOpndOp(opnd, eopnd)
              fun hasSameCond (item as (v, n, b, s)) = eqCondOp (n, en)
              fun maybeImp' x = (hasSameOpnd x) andalso (not (hasSameCond x))

              val rtn = PSSet.exists (ps, maybeImp')
              val () = if rtn then dbgPrint "maybeImp\n" else ()
            in 
              rtn
            end

        fun isImpossible (item, ps) = if (not (maybeRedundant (item, ps))) andalso maybeImp (item, ps) 
                                      then true 
                                      else false

        fun isUnknown ((eopnd, en, eb, es), ps) =
            let
              fun hasSameOpnd (item as (opnd, n, b, s)) = eqOpndOp (opnd, eopnd)
              fun isUnknown' x = (es = false) orelse (not (hasSameOpnd x))
            in
              PSSet.exists (ps, isUnknown')
            end
   
        val state = 
            if isSome(epso) then
              let
                val eps = valOf epso
              in 
                if isImpossible (eps, ps) then Impossible
                else if maybeRedundant (eps, ps) then Redundant
                else Unknown
              end
            else Unknown
               
        val () = printPSSet(getLabel(imil, a), getEdgePSSet(imil, e))
        val () = printEdgePSState(imil, e, ps, state)
                 
      in state
      end

  fun removeImp (d, imil, e as (a, b)) =
      let
        fun removeTarget (imil, ns as {on, cases, default}, e as (a, b)) =
            let
              fun noteq (arm as (_, M.T {block, arguments})) = 
                  not (block = getLabel (imil, b) )

(*              val newns = {on=on, cases=Vector.keepAll (cases, noteq), default=default}*)
              val () = PD.click (d, "RemoveBranch")
            in 
              {on=on, cases=Vector.keepAll (cases, noteq), default=default}
            end

        fun replacePSumCaseInstr (imil, e as (a, b), ns as {on, cases, default}) =
            let
              val instr = IMil.IBlock.getTransfer (imil, a)             

              val () = printOrigInstr (imil, e, instr)

              val newns = removeTarget (imil, ns, e)
              val nt = M.TPSumCase (newns)

              val nmt = IMil.MTransfer nt
              val () = IMil.IInstr.replaceMil (imil, instr, nmt)

              val () = printNewInstr (imil, nmt)
            in ()
            end

        fun replaceTCaseInstr (imil, e as (a, b), cs as {on, cases, default}) =
            let
              val () = dbgPrint ("find impossible tcase instruction\n")
              val instr = IMil.IBlock.getTransfer (imil, a)             

              val () = printOrigInstr (imil, e, instr)

              val newns = removeTarget (imil, cs, e)
              val nt = M.TCase (newns)
             
              val nmt = IMil.MTransfer nt
              val () = IMil.IInstr.replaceMil (imil, instr, nmt)

              val () = printNewInstr (imil, nmt)
            in ()
            end

      in case getPSumCase (imil, e)
          of SOME ns => replacePSumCaseInstr(imil, e, ns)
           | _ => (case getTCase (imil, e)
                    of SOME cs => replaceTCaseInstr(imil, e, cs)
                     | _ => ())
      end

  fun checkRedundant (d, imil, e as (a, b), ps) =
      if getEdgePSState (imil, e, ps) = Impossible 
      then removeImp (d, imil, e)
      else ()

  fun checkEdgePS (d, imil, dict, e as (a, b)) =
      if isPSumCase (imil, a, b) orelse isTCase (imil, a, b) then
        case LD.lookup (dict, getLabel (imil, a))
         of SOME ps => checkRedundant (d, imil, e, ps)
          | _ => ()
      else ()

  fun checkBlockPS (d, imil, dict, a) =
      let
        fun checkEdgePS' e = checkEdgePS (d, imil, dict, e)

        val outEdges = List.map(IMil.IBlock.succs (imil, a), fn b => (a, b))
      in 
        List.foreach (outEdges, checkEdgePS')
      end

  fun layoutTreeDot (imil, cfg, t) = 
      if debugPrint() then
        let
          val cn = ID.variableString'(IMil.IFunc.getFName (imil, cfg))
          val cfgname = "dom" ^ cn ^ ".dot" 
          fun labelNode n = [Dot.NodeOption.Label[(ID.labelString(getLabel(imil, n)), Dot.Center)], 
                             Dot.NodeOption.Shape Dot.Ellipse]
                            
          val graphOptions = [Dot.GraphOption.Size {width=8.5, height=10.0},
                              Dot.GraphOption.Page {width=8.5, height=11.0},
                              Dot.GraphOption.Orientation Dot.Landscape]
                             
          val l = Tree.layoutDot (t, {nodeOptions = labelNode, options = graphOptions, title = cfgname})
          val () = LU.writeLayout (l, cfgname)
        in ()
        end
      else ()

  fun rcbrCfg (d, imil, cfg) =
      let
        val cn = IMil.IFunc.getFName(imil, cfg)
        val () = dbgPrint("cfg:" ^ ID.variableString' (cn) ^ "\n")

        val () = splitCriticalEdge (imil, cfg)

        val () = layoutCfg (imil, cfg)

        val dom = IMil.IFunc.getDomTree (imil, cfg)

        val () = layoutTreeDot (imil, cfg, dom)

        val () = propagatePS (imil, dom)

        val () = dbgPrint ("finish propagatePS\n")

        fun checkBlockPS' b = checkBlockPS (d, imil, !gDict, b)
      in 
        Tree.foreachPre(dom, checkBlockPS')
      end

  fun program (imil, d) = 
      let
        fun rcbrCfg' (cfg) = rcbrCfg (d, imil, cfg)
        val () = List.foreach (IMil.Enumerate.T.funcs imil, rcbrCfg')

        val () = debugShowPost (d, imil)
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
                    debugs    = [debugPassD],
                    features  = [],
                    subPasses = []}

  val pass =
      Pass.mkOptPass (description, associates, BothMil.mkIMilPass program)

end
