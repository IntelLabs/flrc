
(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, January 2009 *)

(* Redudant Conditional Branch Removal 
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
  structure LDOM = MilCfg.LabelDominance
  structure I   = Identifier
  structure MU  = MilUtils
  structure ML  = MilLayout

  datatype psCond =
           RCons of M.constant
         | RName of ID.name

  fun getLabel (imil, b) = #1 (IMil.IBlock.getLabel' (imil, b))
  fun labelString (imil, b) = ID.labelString(getLabel(imil, b))

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
                               LU.printLayout l
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
        prints (d, "[" ^ labelString (imil, a) ^ "->" ^ labelString (imil, b) ^ "] ")

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
          val () = prints (d, "[" ^ labelString (imil, a) ^ "] ")
          val () = List.foreach (es, fn e => printEdge (d, imil, e))
          val () = prints (d, "\n")
        in ()
        end

    fun printPS (d, (opndo, condo, b, s)) =
        let
          val () = prints (d, "(")
          val () = printOpndOp (d, opndo)
          val () = if b then prints (d, " = ") else prints (d, " <> ")
          val () = printCondOp (d, condo)
          val () = prints (d, ")")
        in ()
        end

    fun printPSSet (d, s, ps) = 
        let
          val () = prints (d, "PSSet:[" ^ s ^ "]")
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
          val () = printPSSet (d, labelString(imil, a), ps)
          val instr = IMil.IBlock.getTransfer(imil, a)
          val () = printLayout (d, IMil.IInstr.layout (imil, instr))
          val () = prints (d, "\n")
        in ()
        end

    fun printOrigInstr (d, imil, e as (a, b), instr) =
        printLayout (d, L.seq [L.str "remove ", 
                               L.str (labelString(imil, b)), 
                               L.str " in ", 
                               L.str (labelString(imil, a)),
                               L.str "\n", 
                               IMil.IInstr.layout (imil, instr)])

    fun printNewInstr (d, imil, newinstr) =
        printLayout (d, L.seq [L.str("=>\n"), IMil.IInstr.layoutMil (imil, newinstr), L.str("\n")])

    fun layoutCfg (d, imil, ifunc) =
        if Config.debug andalso debugPass (PD.getConfig d) then
          let
            val cn = ID.variableString'(IMil.IFunc.getFName (imil, ifunc))
            val () = LU.writeLayout (IMil.IFunc.layout (imil, ifunc), cn ^ ".fun" )
            val ()  = LU.writeLayout (IMil.IFunc.layoutDot (imil, ifunc), cn ^ ".dot" )
          in ()
          end
        else ()

    fun layoutTreeDot (d, imil, ifunc, t) = 
        if Config.debug andalso debugPass (PD.getConfig d) then
          let
            val cfgname = "dom" ^ ID.variableString'(IMil.IFunc.getFName (imil, ifunc)) ^ ".dot" 
            fun labelNode n = [Dot.NodeOption.Label[(labelString(imil, n), Dot.Center)], 
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

  fun splitCriticalEdge (imil, ifunc) =
      let
        fun splitBlockCE b =
            let
              fun isCritical (e as (a, b)) =
                  ((List.length(IMil.IBlock.outEdges (imil, a)) > 1)
                   andalso (List.length(IMil.IBlock.inEdges (imil, b)) > 1))
                  
              fun findInCE b = List.keepAll (IMil.IBlock.inEdges(imil, b), isCritical)
                  
              fun splitEdge e = 
                  let
                    val bo = IMil.IBlock.splitEdge (imil, e)
                  in ()
                  end
            in
              List.foreach (findInCE b, splitEdge)
            end
      in 
        List.foreach(IMil.IFunc.getBlocks(imil, ifunc), splitBlockCE)
      end

  (* create edge PS set, variable set
   * (variable, (equal? (true or false), condition))
   *)
  fun getEdgePSSet (d, imil, e as (a, b)) : PSSet.t =
      let
        fun eqTarget (_, M.T {block, arguments}) = block = getLabel (imil, b)
                                                   
        fun getCasePS (imil, a, b, tCond, {on, cases, default}) =
            case Vector.peek (cases, eqTarget)
             of SOME (n, t) => [(SOME on, SOME (tCond n), true, true)]
              | _ => (case default
                       of SOME (dt as M.T {block, arguments}) =>
                          (if block = getLabel (imil, b) then
                             List.map (Vector.toList cases, fn (n, t) => (SOME on, SOME (tCond n), false, true))
                           else [])
                        | _ => [])

        val pss = case IMil.IBlock.getTransfer' (imil, a)
                   of M.TPSumCase ns => getCasePS (imil, a, b, RName, ns)
                    | M.TCase sw => getCasePS (imil, a, b, RCons, sw)
                    | _ => [(NONE, NONE, false, false)]
      in
        PSSet.fromList pss
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
  fun propagatePS' (d, gDict, imil, tree) =
      let
        fun doTree (Tree.T (a, children), ancestors) =
            let
              fun doOne ((ancestorL, ancestor), ps) =
                  let
                    val ancestorSet = LD.lookup (!gDict, ancestorL)
                    val thisSet = getEdgePSSet (d, imil, (ancestor, a))
                    val curPS =
                        case ancestorSet
                         of NONE             => thisSet
                          | SOME ancestorSet => PSSet.union (ancestorSet, thisSet)
                    val ps = PSSet.union (ps, curPS)
                  in ps
                  end
              val ps = List.fold (ancestors, PSSet.empty, doOne)
              val al = getLabel (imil, a)
              val () = gDict := LD.insert (!gDict, al, ps)
              val ancestors' = (al, a)::ancestors
              val () = Vector.foreach (children, fn c => doTree (c, ancestors'))
            in ()
            end
        val () = doTree (tree, [])
      in ()
      end

  fun hasRealOpnd (eps as (opnd, n, b, s)) = isSome opnd
  fun hasSameOpnd (eps1 as (opnd1, n1, b1, s1), eps2 as (opnd2, n2, b2, s2)) = eqOpndOp (opnd1, opnd2)
  fun hasSameCond (eps1 as (opnd1, n1, b1, s1), eps2 as (opnd2, n2, b2, s2)) = eqCondOp (n1, n2)
  fun isEq        (eps1 as (opnd1, n1, b1, s1), eps2 as (opnd2, n2, b2, s2)) = b1 = b2
  fun samePS   (eps1, eps2) = (hasSameOpnd (eps1, eps2)) andalso (     hasSameCond (eps1, eps2))  andalso (     isEq (eps1, eps2))
  fun diffCond (eps1, eps2) = (hasSameOpnd (eps1, eps2)) andalso (not (hasSameCond (eps1, eps2))) andalso (     isEq(eps1, eps2))
  fun diffEq   (eps1, eps2) = (hasSameOpnd (eps1, eps2)) andalso (     hasSameCond (eps1, eps2))  andalso (not (isEq (eps1, eps2)))

  fun isRedundant (d, eps, psset) =
      PSSet.exists (psset, fn x => samePS (x, eps)) 
      andalso (not (PSSet.exists (psset, fn x => diffCond (x, eps)))) 
      andalso (not (PSSet.exists (psset, fn x => diffEq (x, eps))))

  fun maybeImp (d, eps, psset) = PSSet.exists (psset, fn x => (diffCond (x, eps)) orelse (diffEq(x, eps)))

  fun getEdgePSState (d, imil, e as (a, b), psset) =
      let
        val epset = getEdgePSSet (d, imil, e)

        fun isDefaultEdgePS (item : PSSet.t) = PSSet.exists (item, fn (opnd, n, b, s) => b = false)

        fun isDefaultEdgeImpossible (item, psset) =
            let
              fun hasSameOpnd (item, psset) =
                  let
                    val firsteps as (eopnd, _, _, _) = List.first (PSSet.toList (item))
                    val sameOpndPS = PSSet.keepAll (psset, fn (opnd, _, _, _) => eqOpndOp(opnd, eopnd))
                  in
                    PSSet.size (sameOpndPS) > 0
                  end

              val epslist = PSSet.toList item
              val revepslist = List.map (epslist, fn (opnd, n, b, s) => (opnd, n, not b, s))
              val intersection = PSSet.intersection (PSSet.fromList epslist, psset)
            in
              PSSet.isEmpty (intersection) andalso hasSameOpnd (item, psset)
            end

        fun isImpossible (item : PSSet.t, psset) = 
            if PSSet.isEmpty item then false
            else if isDefaultEdgePS item then isDefaultEdgeImpossible (item, psset)
            else maybeImp (d, List.first(PSSet.toList(item)), psset)

        val state = if isImpossible (epset, psset) then Impossible else Unknown
               
        val () = Debug.printPSSet(d, labelString(imil, a), getEdgePSSet(d, imil, e))
        val () = Debug.printEdgePSState(d, imil, e, psset, state)
                 
      in state
      end

  fun removeImp (d, imil, e as (a, b), tCase, sw, psset) =
      let
        fun removeTarget (imil, ns as {on, cases, default}, e as (a, b)) =
            let
              fun noteq (arm as (_, M.T {block, arguments})) = not (block = getLabel (imil, b) )
              val newdefault = case default
                                of SOME (arm as M.T {block, arguments}) => if block = getLabel (imil, b) 
                                                                           then NONE
                                                                           else default
                                 | _ => NONE
              val () = PD.click (d, passname)
            in 
              {on=on, cases=Vector.keepAll (cases, noteq), default=newdefault}
            end

        fun replaceInstr (imil, e as (a, b), tCase, ns) =
            let
              val () = Debug.prints (d, "find impossible switch case instruction\n")
              val instr = IMil.IBlock.getTransfer (imil, a)             
              val () = Debug.printOrigInstr (d, imil, e, instr)
              val nmt = IMil.MTransfer (tCase (removeTarget (imil, ns, e)))
              val () = IMil.IInstr.replaceMil (imil, instr, nmt)
              val () = Debug.printNewInstr (d, imil, nmt)
            in ()
            end

      in 
        if getEdgePSState (d, imil, e, psset) = Impossible then 
          replaceInstr (imil, e, tCase, sw)
        else () 
      end

  fun checkEdgePS (d, imil, dict, e as (a, b)) =
      case LD.lookup (dict, getLabel (imil, a))
       of SOME psset => (case IMil.IBlock.getTransfer' (imil, a)
                          of M.TPSumCase ns => removeImp (d, imil, e, M.TPSumCase, ns, psset)
                           | M.TCase cs => removeImp (d, imil, e, M.TCase, cs, psset)
                           | _ => ())
        | _ => ()
      
  fun checkBlockPS (d, imil, dict, a) =
      List.foreach (List.map(IMil.IBlock.succs (imil, a), fn b => (a, b)), 
                    fn e => checkEdgePS (d, imil, dict, e))

  (* RCBR extension
   *
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
  fun checkBlockPSExt (d, m as Mil.P {globals, symbolTable, entry}, imil, ifunc, psDict, a) = 
      let
        val si = I.SymbolInfo.SiTable symbolTable
        val config = PD.getConfig d
                                                               
        fun layoutConsequentBlocks (a, b, c) =
            let
              val () = Debug.prints (d, "rcbr extension " 
                                        ^ labelString(imil, a) ^ " => "
                                        ^ labelString(imil, b) ^ " => "
                                        ^ labelString(imil, c) ^ "\n")
              val bc_ps = getEdgePSSet (d, imil, (b, c))
              val ab_ps = getEdgePSSet (d, imil, (a, b))
              val a_pso = LD.lookup(!psDict, getLabel(imil, a))
              val () = Debug.printPSSet (d, labelString(imil, b) ^ "=>" ^ labelString(imil, c), bc_ps)
              val () = case a_pso 
                        of SOME a_ps => Debug.printPSSet (d, "a_ps u ab_ps", PSSet.union(a_ps, ab_ps))
                         | NONE => ()
            in ()
            end

        fun replaceCase (a, b, c, instr, tCase, t as {on, cases, default}) =
            let
              val cpara = IMil.IBlock.getParameters (imil, c)
              val newcases = 
                  Vector.map (cases, 
                           fn (ct, M.T {block, arguments}) => if block = getLabel (imil, b) 
                                                              then (ct, MilUtils.Target.fromVars (getLabel(imil, c), cpara))
                                                              else (ct, M.T {block=block, arguments=arguments}))
              val newdefault = 
                  (case default
                    of NONE => default
                     | SOME (M.T {block, arguments}) => if block = getLabel (imil, b)
                                                        then SOME (MilUtils.Target.fromVars (getLabel(imil, c), cpara))
                                                        else default)
              val instr = IMil.IBlock.getTransfer (imil, a)
              val oldInstrLayout = IMil.IInstr.layoutMil (imil, IMil.IInstr.getMil (imil, instr))
              val newinstr = IMil.MTransfer (tCase {on=on, cases=newcases, default=newdefault})
              val newInstrLayout = IMil.IInstr.layoutMil (imil, newinstr)
              val () = IMil.IInstr.replaceMil (imil, instr, newinstr)
              val () = Debug.printLayout(d, L.seq[L.str "extension replace case:\n", 
                                                  oldInstrLayout,
                                                  L.str " =>\n",
                                                  newInstrLayout])
              val () = PD.click (d, passname)
            in ()
            end

        fun replaceGoto (a, b, c, instr, t as M.T {block, arguments}) =
            let
              val para = IMil.IBlock.getParameters (imil, c)
            in
              if Vector.size (para) = 0 then
                let
                  val newtarget = if block = getLabel (imil, b) then M.T {block=getLabel (imil, c), arguments=Vector.new0()}
                                  else t
                  val instr = IMil.IBlock.getTransfer (imil, a)
                  val oldInstrLayout = IMil.IInstr.layoutMil (imil, IMil.IInstr.getMil (imil, instr))
                  val newinstr = IMil.MTransfer (M.TGoto(newtarget))
                  val () = IMil.IInstr.replaceMil (imil, instr, newinstr)
                  val () = Debug.printLayout(d, L.seq[L.str "extension replace goto:\n", 
                                                      oldInstrLayout,
                                                      L.str " =>\n",
                                                      IMil.IInstr.layoutMil (imil, newinstr)])
                  val () = PD.click (d, passname)
                in ()
                end
              else ()
            end

        fun replaceTarget (a, b, c) =
            let
              val instr = IMil.IBlock.getTransfer' (imil, a)
            in
              case instr
               of M.TGoto t => replaceGoto (a, b, c, instr, t)
                | M.TCase t => replaceCase (a, b, c, instr, M.TCase, t)
                | M.TInterProc t => ()
                | M.TReturn t => ()
                | M.TCut t => ()
                | M.TPSumCase t => replaceCase (a, b, c, instr, M.TPSumCase, t)
            end 

        fun checkConsequentEdges (a, b, c) =
            let
              val bc_ps = getEdgePSSet (d, imil, (b, c))
              val ab_ps = getEdgePSSet (d, imil, (a, b))
              val a_pso = LD.lookup(!psDict, getLabel(imil, a))
              val bpara = IMil.IBlock.getParameters (imil, b)
              val cpara = IMil.IBlock.getParameters (imil, c)
            in 
              (case a_pso
                of SOME a_ps =>
                   if (List.fold (PSSet.toList(bc_ps), 
                                  true, 
                               fn (eps, r) => (isRedundant(d, eps, PSSet.union(a_ps, ab_ps)) andalso r))
                       andalso IMil.IBlock.isEmpty (imil, b)
                       andalso Vector.size (bpara) = 0
                       andalso Vector.size (cpara) = 0
                       andalso (case IMil.IBlock.getTransfer'(imil, b)
                                 of Mil.TGoto _ => false
                                  | Mil.TCase _ => true
                                  | Mil.TInterProc _ => false
                                  | Mil.TReturn _ => false
                                  | Mil.TCut _ => false
                                  | Mil.TPSumCase _ => true))
                   then replaceTarget (a, b, c)
                   else ()
                 | _ => ())
            end

        fun checkEdge (a, b) = List.foreach (IMil.IBlock.succs (imil, b), fn c => checkConsequentEdges (a, b, c))

        val () = List.foreach (IMil.IBlock.succs (imil, a), fn b => checkEdge (a, b))
      in ()
      end
  
  fun rcbrCfg (d, m, imil, ifunc) =
      let
        val () = splitCriticalEdge (imil, ifunc)
        val () = Debug.layoutCfg (d, imil, ifunc)
        val dom = IMil.IFunc.getDomTree (imil, ifunc)
        val () = Debug.layoutTreeDot (d, imil, ifunc, dom)
        val psDict = ref LD.empty
        val () = propagatePS' (d, psDict, imil, dom)
        (* rcbr extension *)
        val () = Tree.foreachPre (dom, fn b => checkBlockPSExt (d, m, imil, ifunc, psDict, b))
        (* rcbr *)
        val () = Tree.foreachPre (dom, fn b => checkBlockPS (d, imil, !psDict, b))
      in ()
      end

  fun program (imil, d) = 
      let
        val m  = IMil.T.unBuild imil
        val () = List.foreach (IMil.Enumerate.T.funcs imil, fn ifunc => rcbrCfg (d, m, imil, ifunc))
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
