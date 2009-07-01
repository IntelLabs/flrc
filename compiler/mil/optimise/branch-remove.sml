
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

  val stats = [(passname, "Redudant Conditional Branch Removal")]

  fun fail (f, m) = Fail.fail (passname, f, m)
  fun assert (f, m, b) = if b then fail (f, m) else ()

  structure M   = Mil
  structure PD  = PassData
  structure L   = Layout
  structure LU  = LayoutUtils
  structure ID  = Identifier
  structure LD  = ID.LabelDict

  datatype psCond =
           RCons of M.constant
         | RName of ID.name

  fun getLabel (imil, b) = #1 (IMil.IBlock.getLabel' (imil, b))

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

  datatype psState =
           Redundant  (* a->b while c=x and ps (c=x, ...)*)
         | Impossible (* a->b while c=x and ps (c=y, ...*)
         | Unknown    (* a->b while c=x and ps (no c )*)

  structure Debug =
  struct
    val (debugPassD, debugPass) = Config.Debug.mk (passname, "debug the Mil branch removal pass")

    fun prints (d, s) = if Config.debug andalso debugPass (PD.getConfig d) then print s else ()

    fun printLayout (d, l) = if Config.debug andalso debugPass (PD.getConfig d) then
                               LU.printLayout (L.seq [L.str (passname ^ ": "), l])
                             else ()

    fun debugShowPre (d, imil, fname)  = 
        if Config.debug andalso debugPass (PD.getConfig d) then
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
            end
        else ()
             
    fun debugShowPost (d, imil)  = 
        if Config.debug andalso 
           debugPass (PD.getConfig d) andalso
           (Config.debugLevel (PD.getConfig d, passname)) > 1 then 
          let
            val () = prints (d, ("after branch removal:\n"))
            (*          val mil = IMil.unBuild imil
             val () = MilLayout.printGlobalsOnly (PD.getConfig d, mil)*)
            val () = prints (d, "\n")
          in ()
          end
        else ()

    fun printEdge (d, imil, (a, b)) =
        prints (d, "[" ^ ID.labelString (getLabel (imil, a)) ^ "->" ^ ID.labelString (getLabel (imil, b)) ^ "] ")

    fun printOpnd (d, opnd) =
        (case opnd
          of M.SVariable v => prints (d, ID.variableString' v)
           | M.SConstant c => 
             (case c 
               of M.CName n         => prints (d, "SConstant " ^ ID.nameString' n)
                | M.CRat n          => fail ("SConstant ", "rat" )
                | M.CInteger n      => fail ("SConstant ", "integer" )
                | M.CIntegral n     => prints (d, "SConstant " ^ IntArb.stringOf n )
                | M.CFloat n        => fail ("SConstant ", "float" )
                | M.CDouble n       => fail ("SConstant ", "double" )
                | M.CViVector n     => fail ("SConstant ", "vivector" )
                | M.CViMask n       => fail ("SConstant ", "vimask" )
                | M.CPok n          => fail ("SConstant ", "pok" )
                | M.COptionSetEmpty => fail ("SConstant ", "optionsetempty" )
                | CTypePH           => fail ("SConstant ", "typeph" )))

    fun printOpndOp (d, opndo) = case opndo
                                  of SOME opnd => printOpnd (d, opnd)
                                   | NONE => prints (d, "others ")

    fun printOpndOpPair (d, opndo1, opndo2, eq) =
        let
          val () = printOpndOp (d, opndo1)
          val () = if eq = true then prints (d, " == ") else prints (d, " <> ")
          val () = printOpndOp (d, opndo2)
          val () = prints (d, "\n")
        in ()
        end

    fun printCond (d, cond) = 
        case cond
         of RName n => prints (d, ID.nameString' n)
          | RCons c => (case c
                         of M.CName n => prints (d, ID.nameString' n)
                          | M.CIntegral i => prints (d, IntArb.stringOf i)
                          | M.CFloat f => prints (d, Real32.toString f)
                          | M.CDouble dou => prints (d, Real64.toString dou)
                          | _ => fail ("RCons", "unknown"))
  
    fun printCondOp (d, condo) =
        case condo
         of SOME cond => printCond (d, cond)
          | NONE => prints (d, "unknown")
                    
    fun printCondOpPair (d, condo1, condo2, eq) =
        let
          val () = printCondOp (d, condo1)
          val () = if eq = true then prints (d, " == ") else prints (d, " <> ")
          val () = printCondOp (d, condo2)
          val () = prints (d, "\n")
        in ()
        end
        
    fun printBlockPS (d, imil, (a, es)) = 
        let
          val () = prints (d, "[" ^ ID.labelString (getLabel (imil, a)) ^ "] ")
          val () = List.foreach (es, fn e => printEdge (d, imil, e))
          val () = prints (d, "\n")
        in ()
        end

    fun printPS (d, (opndo, condo, b, s)) =
        let
          val () = prints (d, "(")
          val () = printOpndOp (d, opndo                     )
          val () = if b then prints (d, " = ") else prints (d, " <> ")
          val () = printCondOp (d, condo)
          val () = prints (d, ")")
        in ()
        end

    fun printPSSet (d, k, ps) = 
        let
          val () = prints (d, "Block Predict Set Dict:[" ^ (ID.labelString k) ^ "]")
          val () = PSSet.foreach (ps, fn x => printPS (d, x))
          val () = prints (d, "\n")
        in ()
        end

    fun printEdgePSState (d, imil, e as (a, b), ps, state) =
        let
          val () = case state 
                    of Redundant => prints (d, "redundant edge: ")
                     | Unknown => prints (d, "unknown edge: ")
                     | Impossible => prints (d, "impossible edge: ")
                                     
          val () = printEdge (d, imil, e)
          val () = printPSSet (d, getLabel(imil, a), ps)
          val instr = IMil.IBlock.getTransfer(imil, a)
          val () = printLayout (d, IMil.IInstr.layout (imil, instr))
          val () = prints (d, "\n")
        in ()
        end

    fun printOrigInstr (d, imil, e as (a, b), instr) =
        let
          val () = prints (d, "\nremove " ^ ID.labelString(getLabel(imil, b)) ^ " in " ^ ID.labelString(getLabel(imil, a)) ^ "\n")
          val () = printLayout (d, IMil.IInstr.layout (imil, instr))
        in ()
        end

    fun printNewInstr (d, imil, newinstr) =
        let
          val () = prints (d, "replace with new PSumCase instruction\n")
          val () = printLayout (d, IMil.IInstr.layoutMil (imil, newinstr))
          val () = prints (d, "\n")
        in ()
        end

    fun layoutCfg (d, imil, cfg) =
        if Config.debug andalso debugPass (PD.getConfig d) then
          let
            val cn = ID.variableString'(IMil.IFunc.getFName (imil, cfg))
            val () = LU.writeLayout (IMil.IFunc.layout (imil, cfg), cn ^ ".cfg" )
            val ()  = LU.writeLayout (IMil.IFunc.layoutDot (imil, cfg), cn ^ ".dot" )
          in ()
          end
        else ()

    fun layoutTreeDot (d, imil, cfg, t) = 
        if Config.debug andalso debugPass (PD.getConfig d) then
          let
            val cfgname = "dom" ^ ID.variableString'(IMil.IFunc.getFName (imil, cfg)) ^ ".dot" 
            fun labelNode n = [Dot.NodeOption.Label[(ID.labelString(getLabel(imil, n)), Dot.Center)], 
                               Dot.NodeOption.Shape Dot.Ellipse]
            val graphOptions = [Dot.GraphOption.Size {width=8.5, height=10.0},
                                Dot.GraphOption.Page {width=8.5, height=11.0},
                                Dot.GraphOption.Orientation Dot.Landscape]
            val () = LU.writeLayout (Tree.layoutDot (t, {nodeOptions = labelNode, options = graphOptions, title = cfgname}), 
                                     cfgname)
          in ()
          end
        else ()

  end

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

  fun getEdgePSSet (d, imil, e) : PSSet.t =
      let
        fun getEdgePS (d, imil, e as (a, b)) =
            let
              fun eq (n, M.T {block, arguments}) = block = getLabel (imil, b)
                                                   
              (* PSumCase or TCase *)
              fun getPCasePS (imil, a, b, {on, cases, default}) =
                  case Vector.peek (cases, eq)
                   of SOME (n, t) => [(SOME on, SOME (RName n), true, true)]
                    | _ => (case default
                             of SOME (dt as M.T {block, arguments}) => 
                                (if block = getLabel (imil, b) then
                                   List.map (Vector.toList cases, fn (n, t) => (SOME on, SOME (RName n), false, true))
                                 else [])
                              | _ => [])
               
              fun getTCasePS (imil, a, b, {on, cases, default}) =
                  case Vector.peek (cases, eq)
                   of SOME (n, t) => [(SOME on, SOME (RCons n), true, true)]
                    | _ => (case default
                             of SOME (dt as M.T {block, arguments}) =>
                                (if block = getLabel (imil, b) then
                                   List.map (Vector.toList cases, fn (n, t) => (SOME on, SOME (RCons n), false, true))
                                 else [])
                              | _ => [])

              val pso = case getTransMil (imil, a)
                         of IMil.MTransfer t =>
                            (case t
                              of M.TPSumCase ns => getPCasePS (imil, a, b, ns)
                               | M.TCase sw => getTCasePS (imil, a, b, sw)
                               | _ => [(NONE, NONE, false, false)])
                          | _ => []
            (*
             val () = printEdge (imil, e)
             val () = case pso
                       of SOME ps => printPS ps
                        | NONE => print "none"
             val () = print "\n"
             *)
            in pso
            end
            
      in
        PSSet.union (PSSet.empty, PSSet.fromList(getEdgePS (d, imil, e)))
      end

  fun getDomTreeEdges (Tree.T (a, v)) = 
      let
        fun getDomEdges (b, []) = []
          | getDomEdges (b, (Tree.T (a, v))::xs) = 
            (b, a)::getDomEdges(a, Vector.toList v)@getDomEdges(b, xs)
      in 
        getDomEdges (a, Vector.toList v)
      end

  (* propagate PS on dom tree *)
  fun propagatePS (d, gDict, imil, tree) =
      let
        val domEdges = getDomTreeEdges tree
        fun getInDomEdge tgtBlock = List.keepAll (domEdges, fn (a, b) => b = tgtBlock)

        fun travNode (a) =
            let
              val inEdges = getInDomEdge a

              fun foldf (e as (p, a), ps) =
                  let
                    val parentSet = LD.lookup(!gDict, getLabel(imil, p))
                    val thisSet = getEdgePSSet(d, imil, (p, a))
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

  fun getEdgePSState (d, imil, e as (a, b), ps : PSSet.t) =
      let
        val eps : PSSet.t = getEdgePSSet (d, imil, e)

        fun maybeRedundant ((eopnd, en, eb, es), ps) =
            let
              fun hasSameOpnd (item as (opnd, n, b, s)) = eqOpndOp(opnd, eopnd)
              fun hasSameCond (item as (v, n, b, s)) = eqCondOp (n, en)
              fun isEq (item as (v, n, b, s)) = b = eb
              fun maybeRedundant' x = (hasSameOpnd x) andalso (hasSameCond x) andalso (isEq x)

              val () = if PSSet.exists (ps, maybeRedundant') then Debug.prints (d, "maybeRedundant\n") else ()
            in 
              PSSet.exists (ps, maybeRedundant')
            end

        fun maybeImp ((eopnd, en, eb, es), ps) = (* impossible *)
            let
              fun hasSameOpnd (item as (opnd, n, b, s)) = eqOpndOp(opnd, eopnd)
              fun hasSameCond (item as (v, n, b, s)) = eqCondOp (n, en)
              fun isEq (item as (v, n, b, s)) = b = eb
              fun maybeImp' x = ((hasSameOpnd x) andalso (not (hasSameCond x)) andalso (isEq x)) 
                                orelse (((hasSameOpnd x) andalso (hasSameCond x) andalso (not (isEq x))))

              val () = if PSSet.exists (ps, maybeImp') then Debug.prints (d, "maybeImp\n") else ()
            in 
              PSSet.exists (ps, maybeImp')
            end

        fun isImpossible' (item, ps) = if (not (maybeRedundant (item, ps))) andalso maybeImp (item, ps) 
                                      then true 
                                      else false

        fun isDefaultEdgePS (item : PSSet.t) = PSSet.exists (item, fn (opnd, n, b, s) => b = false)

        fun isDefaultEdgeImpossible (item, ps) =
            let
              fun hasSameOpnd (item, ps) =
                  let
                    val firsteps as (eopnd, _, _, _) = List.first (PSSet.toList (item))
                    val sameOpndPS = PSSet.keepAll (ps, fn (opnd, _, _, _) => eqOpndOp(opnd, eopnd))
                  in
                    PSSet.size (sameOpndPS) > 0
                  end

              val epslist = PSSet.toList item
              val revepslist = List.map (epslist, fn (opnd, n, b, s) => (opnd, n, not b, s))
              val intersection = PSSet.intersection (PSSet.fromList epslist, ps)
            in
              PSSet.isEmpty (intersection) andalso hasSameOpnd (item, ps)
            end

        fun isImpossible (item : PSSet.t, ps) = 
            if PSSet.isEmpty item then false
            else if isDefaultEdgePS item then isDefaultEdgeImpossible (item, ps)
            else isImpossible' (List.first(PSSet.toList(item)), ps)

        val state = if isImpossible (eps, ps) then Impossible else Unknown
               
        val () = Debug.printPSSet(d, getLabel(imil, a), getEdgePSSet(d, imil, e))
        val () = Debug.printEdgePSState(d, imil, e, ps, state)
                 
      in state
      end

  fun removeImp (d, imil, e as (a, b)) =
      let
        fun removeTarget (imil, ns as {on, cases, default}, e as (a, b)) =
            let
              fun noteq (arm as (_, M.T {block, arguments})) = not (block = getLabel (imil, b) )
              val () = PD.click (d, passname)
              val newdefault = case default
                                of SOME (arm as M.T {block, arguments}) => if block = getLabel (imil, b) 
                                                                           then NONE
                                                                           else default
                                 | _ => NONE
            in 
              {on=on, cases=Vector.keepAll (cases, noteq), default=default}
            end

        fun replacePSumCaseInstr (imil, e as (a, b), ns as {on, cases, default}) =
            let
              val () = Debug.prints (d, "find impossible psumcase instruction\n")
              val instr = IMil.IBlock.getTransfer (imil, a)             
              val () = Debug.printOrigInstr (d, imil, e, instr)
              val newns = removeTarget (imil, ns, e)
              val nt = M.TPSumCase (newns)
              val nmt = IMil.MTransfer nt
              val () = IMil.IInstr.replaceMil (imil, instr, nmt)
              val () = Debug.printNewInstr (d, imil, nmt)
            in ()
            end

        fun replaceTCaseInstr (imil, e as (a, b), cs as {on, cases, default}) =
            let
              val () = Debug.prints (d, "find impossible tcase instruction\n")
              val instr = IMil.IBlock.getTransfer (imil, a)             
              val () = Debug.printOrigInstr (d, imil, e, instr)
              val newns = removeTarget (imil, cs, e)
              val nmt = IMil.MTransfer (M.TCase newns)
              val () = IMil.IInstr.replaceMil (imil, instr, nmt)
              val () = Debug.printNewInstr (d, imil, nmt)
            in ()
            end

      in case getPSumCase (imil, e)
          of SOME ns => replacePSumCaseInstr(imil, e, ns)
           | _ => (case getTCase (imil, e)
                    of SOME cs => replaceTCaseInstr(imil, e, cs)
                     | _ => ())
      end

  fun checkRedundant (d, imil, e as (a, b), ps) =
      if getEdgePSState (d, imil, e, ps) = Impossible 
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

  fun rcbrCfg (d, imil, cfg) =
      let
        val gDict = ref LD.empty
        val () = splitCriticalEdge (imil, cfg)
        val () = Debug.layoutCfg (d, imil, cfg)
        val dom = IMil.IFunc.getDomTree (imil, cfg)
        val () = Debug.layoutTreeDot (d, imil, cfg, dom)
        val () = propagatePS (d, gDict, imil, dom)
      in 
        Tree.foreachPre(dom, fn b => checkBlockPS (d, imil, !gDict, b))
      end

  fun program (imil, d) = 
      let
        val () = List.foreach (IMil.Enumerate.T.funcs imil, fn cfg => rcbrCfg (d, imil, cfg))
        val () = Debug.debugShowPost (d, imil)
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
