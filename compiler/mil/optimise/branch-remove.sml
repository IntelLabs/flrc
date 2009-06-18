
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

  fun fail (f, m) = Fail.fail (passname, f, m)
  fun assert (f, m, b) = if b then fail (f, m) else ()

  structure M = Mil
  structure PD = PassData
  structure LU  = LayoutUtils
  structure ID  = Identifier
  structure LD = ID.LabelDict

  datatype psCond =
           RCons of M.constant
         | RName of ID.name

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
       of RName n => print (ID.nameString' n)
        | RCons c => (case c
                       of M.CName n => print (ID.nameString' n)
                        | M.CIntegral i => print(IntArb.stringOf i)
                        | M.CFloat f => print(Real32.toString f)
                        | M.CDouble d => print (Real64.toString d)
                        | _ => fail ("RCons", "unknown"))
  
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

  fun psOpndCompare (o1, o2) =
      case (o1, o2)
       of (M.SVariable v1, M.SVariable v2) => MilUtils.Compare.variable (v1, v2)
        | (M.SConstant c1, M.SConstant c2) => MilUtils.Compare.constant (c1, c2)
        | (M.SVariable _,  _)              => LESS
        | (_,              M.SVariable _)  => GREATER
  
  fun psOpndCompareOp (o1o, o2o) = Compare.option psOpndCompare (o1o, o2o)      

  fun eqOpndOp (o1o, o2o) = psOpndCompareOp (o1o, o2o) = EQUAL
       
  fun psCondCompare (c1, c2) =
      case (c1, c2) 
       of (RName n1,  RName n2)  => MilUtils.Compare.name (n1, n2)
        | (RCons cc1, RCons cc2) => MilUtils.Compare.constant (cc1, cc2)
        | (RName _,   _)         => LESS
        | (_,         RName _)   => GREATER

  fun psCondCompareOp (c1o, c2o) = Compare.option psCondCompare (c1o, c2o)

  fun eqCondOp (c1o, c2o) = Compare.option psCondCompare (c1o, c2o) = EQUAL

  fun psCompare (x1, x2) =
      Compare.quad (psOpndCompareOp, psCondCompareOp, Bool.compare, Bool.compare) (x1, x2)
                              
  structure PSSet = SetF (struct 
                            (* variable, name, equal or not , isSwitch*)
                            type t = M.operand option * psCond option * bool * bool
                            val compare = psCompare 
                          end)

  type edge = IMil.iBlock * IMil.iBlock

  datatype psState =
           Redundant  (* a->b while c=x and ps (c=x, ...)*)
         | Impossible (* a->b while c=x and ps (c=y, ...*)
         | Unknown    (* a->b while c=x and ps (no c )*)

  val (debugPassD, debugPass) = Config.Debug.mk (passname, "debug the Mil branch removal pass")

  fun debugPrint () = false
  fun dbgPrint s = if debugPrint() then print s else ()

  fun debugShowPre (d, imil, fname)  = 
      if Config.debug andalso debugPass (PD.getConfig d) then
        if (Config.debugLevel (PD.getConfig d, passname)) > 0 then 
          let
            val () = print ("before branch removal:\n")
            val () = LU.printLayout (IMil.IFunc.layout (imil, IMil.IFunc.getIFuncByName (imil, fname)))
            val () = print "\n"
          in ()
          end
        else 
          let
            val () = dbgPrint ("Branch removal: ")
            val () = LU.printLayout (IMil.Layout.var (imil, fname))
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
        print ("[" ^ ID.labelString (getLabel (imil, a)) ^ "->" ^ ID.labelString (getLabel (imil, b)) ^ "] ")
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
          val () = print ("\nremove " ^ ID.labelString(getLabel(imil, b)) ^ " in " ^ ID.labelString(getLabel(imil, a)) ^ "\n")
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
          val () = LU.writeLayout (IMil.IFunc.layout (imil, cfg), "cfg" ^ cn ^ ".cfg" )
          val ()  = LU.writeLayout (IMil.IFunc.layoutDot (imil, cfg), "cfg" ^ cn ^ ".dot" )
        in ()
        end
      else ()

  (*
   *	Split critical edges.  
   *    This guarantees that every branch with more than one out edge 
   *    immediately dominates all of its successors 
   *    (and hence each successor is in a 1:1 correspondence with the outedges).
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
        fun eq (n, M.T {block, arguments}) = block = getLabel (imil, b)

        (* PSumCase or TCase *)
        fun getPCasePS (imil, a, b, {on, cases, default}) =
            case Vector.peek (cases, eq)
             of SOME (n, t) => SOME (SOME on, SOME (RName n), true, true)
              | _ => NONE

        fun getTCasePS (imil, a, b, {on, cases, default}) =
            case Vector.peek (cases, eq)
             of SOME (n, t) => SOME (SOME on, SOME (RCons n), true, true)
              | _ => NONE

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
  fun propagatePS (gDict, imil, tree) =
      let
        val domEdges = getDomTreeEdges tree
        fun isDomEdge e = List.exists (domEdges, fn x => x = e)

        fun travNode (a) =
            let
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

              val ps = List.fold(inEdges, PSSet.empty, foldf)
              val () = gDict := LD.insert (!gDict, getLabel(imil, a), ps)
            in fn () => ()
            end
      in
        Tree.traverse(tree, travNode)
      end

  fun getEdgePSState (imil, e as (a, b), ps : PSSet.t) =
      let
        val epso = getEdgePS (imil, e)

        fun maybeRedundant ((eopnd, en, eb, es), ps) =
            let
              fun hasSameOpnd (item as (opnd, n, b, s)) = eqOpndOp(opnd, eopnd)
              fun hasSameCond (item as (v, n, b, s)) = eqCondOp (n, en)
              fun maybeRedundant' x = (hasSameOpnd x) andalso (hasSameCond x)

              val () = if PSSet.exists (ps, maybeRedundant') then dbgPrint "maybeRedundant\n" else ()
            in 
              PSSet.exists (ps, maybeRedundant')
            end

        fun maybeImp ((eopnd, en, eb, es), ps) = (* impossible *)
            let
              fun hasSameOpnd (item as (opnd, n, b, s)) = eqOpndOp(opnd, eopnd)
              fun hasSameCond (item as (v, n, b, s)) = eqCondOp (n, en)
              fun maybeImp' x = (hasSameOpnd x) andalso (not (hasSameCond x))

              val () = if PSSet.exists (ps, maybeImp') then dbgPrint "maybeImp\n" else ()
            in 
              PSSet.exists (ps, maybeImp')
            end

        fun isImpossible (item, ps) = if (not (maybeRedundant (item, ps))) andalso maybeImp (item, ps) 
                                      then true 
                                      else false

        fun isUnknown ((eopnd, en, eb, es), ps) =
            let
              fun hasSameOpnd (item as (opnd, n, b, s)) = eqOpndOp (opnd, eopnd)
            in
              PSSet.exists (ps, fn x => (es = false) orelse (not (hasSameOpnd x)))
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
              fun noteq (arm as (_, M.T {block, arguments})) = not (block = getLabel (imil, b) )
              val () = PD.click (d, "RemoveBranch")
            in 
              {on=on, cases=Vector.keepAll (cases, noteq), default=default}
            end

        fun replacePSumCaseInstr (imil, e as (a, b), ns as {on, cases, default}) =
            let
              val () = dbgPrint ("find impossible psumcase instruction\n")
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
              val nmt = IMil.MTransfer (M.TCase newns)
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
      List.foreach (List.map(IMil.IBlock.succs (imil, a), fn b => (a, b)), 
                    fn e => checkEdgePS (d, imil, dict, e))

  fun layoutTreeDot (imil, cfg, t) = 
      if debugPrint() then
        let
          val cfgname = "dom" ^ ID.variableString'(IMil.IFunc.getFName (imil, cfg)) ^ ".dot" 
          fun labelNode n = [Dot.NodeOption.Label[(ID.labelString(getLabel(imil, n)), Dot.Center)], 
                             Dot.NodeOption.Shape Dot.Ellipse]
          val graphOptions = [Dot.GraphOption.Size {width=8.5, height=10.0},
                              Dot.GraphOption.Page {width=8.5, height=11.0},
                              Dot.GraphOption.Orientation Dot.Landscape]
        in 
          LU.writeLayout (Tree.layoutDot (t, {nodeOptions = labelNode, options = graphOptions, title = cfgname}), 
                          cfgname)
        end
      else ()

  fun rcbrCfg (d, imil, cfg) =
      let
        val gDict = ref LD.empty
        val () = dbgPrint("cfg:" ^ ID.variableString' (IMil.IFunc.getFName(imil, cfg)) ^ "\n")
        val () = splitCriticalEdge (imil, cfg)
        val () = layoutCfg (imil, cfg)
        val dom = IMil.IFunc.getDomTree (imil, cfg)
        val () = layoutTreeDot (imil, cfg, dom)
        val () = propagatePS (gDict, imil, dom)
      in 
        Tree.foreachPre(dom, fn b => checkBlockPS (d, imil, !gDict, b))
      end

  fun program (imil, d) = 
      let
        val () = List.foreach (IMil.Enumerate.T.funcs imil, fn cfg => rcbrCfg (d, imil, cfg))
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

  val pass = Pass.mkOptPass (description, associates, BothMil.mkIMilPass program)

end
