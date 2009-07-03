
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
        List.foreach(IMil.IFunc.getBlocks(imil, cfg), splitBlockCE)
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
(*
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
*)
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

  fun hasSameOpnd (eps1 as (opnd1, n1, b1, s1), eps2 as (opnd2, n2, b2, s2)) = eqOpndOp (opnd1, opnd2)
  fun hasSameCond (eps1 as (opnd1, n1, b1, s1), eps2 as (opnd2, n2, b2, s2)) = eqCondOp (n1, n2)
  fun isEq (eps1 as (opnd1, n1, b1, s1), eps2 as (opnd2, n2, b2, s2)) = b1 = b2

  fun maybeRedundant (d, eps, psset) =
      let
        fun maybeRedundant' x = (hasSameOpnd (x, eps)) andalso (hasSameCond (x, eps)) andalso (isEq (x, eps))
      in 
        PSSet.exists (psset, maybeRedundant')
      end

  fun isRedundant (d, eps as (eopnd, en, eb, es), psset) =
      let
        val reveps = (eopnd, en, not eb, es)
        val r = (maybeRedundant (d, eps, psset)) andalso (not (maybeRedundant(d, reveps, psset)))
      in r
      end

  fun maybeImp (d, eps, psset) = (* impossible *)
      let
        fun maybeImp' x = ((hasSameOpnd (x, eps) andalso (not (hasSameCond (x, eps))) andalso (isEq (x, eps))) 
                           orelse (((hasSameOpnd (x, eps)) andalso (hasSameCond (x, eps)) andalso (not (isEq (x, eps))))))
                          
        val () = if PSSet.exists (psset, maybeImp') then Debug.prints (d, "maybeImp\n") else ()
      in 
        PSSet.exists (psset, maybeImp')
      end

  fun getEdgePSState (d, imil, e as (a, b), psset) =
      let
        val epset = getEdgePSSet (d, imil, e)

        fun isImpossible' (item, psset) = (not (maybeRedundant (d, item, psset))) andalso maybeImp (d, item, psset)

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
            else isImpossible' (List.first(PSSet.toList(item)), psset)

        val state = if isImpossible (epset, psset) then Impossible else Unknown
               
        val () = Debug.printPSSet(d, getLabel(imil, a), getEdgePSSet(d, imil, e))
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
  fun checkBlockPSExt (d, imil, psDict, a) = 
      let
        fun replaceCase (a : IMil.iBlock, b: IMil.iBlock, c: IMil.iBlock, instr, tCase, t as {on, cases, default}) =
            let
              val newcases = 
                  Vector.map (cases, 
                           fn (ct, M.T {block, arguments}) => if block = getLabel (imil, b) 
                                                             then 
                                                               let
                                                                 val () = Debug.prints (d, "extension replace case target\n")
                                                               in (ct, M.T {block=getLabel(imil, c), arguments=arguments})
                                                               end
                                                             else (ct, M.T {block=block, arguments=arguments}))
              val newdefault = 
                  (case default
                    of NONE => default
                     | SOME (M.T {block, arguments}) => if block = getLabel (imil, b)
                                                        then
                                                          let
                                                            val () = Debug.prints (d, "extension replace default target\n")
                                                          in SOME (M.T {block=getLabel (imil, c), arguments = arguments})
                                                          end
                                                        else default)
              val instr = IMil.IBlock.getTransfer (imil, a)
              val () = Debug.printOrigInstr (d, imil, (a, b), instr)
              val newinstr = IMil.MTransfer (tCase {on=on, cases=newcases, default=default})
              val () = IMil.IInstr.replaceMil (imil, instr, newinstr)
              val () = Debug.printNewInstr (d, imil, newinstr)
            in ()
            end

        fun replaceTarget (a : IMil.iBlock, b : IMil.iBlock, c : IMil.iBlock) =
            let
              val instr = IMil.IBlock.getTransfer' (imil, a)
              val () = case instr
                        of M.TGoto t => ()
                         | M.TCase t => replaceCase (a, b, c, instr, M.TCase, t)
                         | M.TInterProc t => ()
                         | M.TReturn t => ()
                         | M.TCut t => ()
                         | M.TPSumCase t => replaceCase (a, b, c, instr, M.TPSumCase, t)
            in ()
            end

        fun checkConsequentEdges (a : IMil.iBlock, b : IMil.iBlock, c : IMil.iBlock) =
            let
              val bc_ps = getEdgePSSet (d, imil, (b, c))
              val ab_ps = getEdgePSSet (d, imil, (a, b))
              val a_pso = LD.lookup(!psDict, getLabel(imil, a))
            in 
              (case a_pso
                of SOME a_ps =>
                   if (List.fold (PSSet.toList(bc_ps), 
                                  true, 
                               fn (eps, r) => (isRedundant(d, eps, PSSet.union(a_ps, ab_ps)) andalso r))
                       andalso IMil.IBlock.isEmpty (imil, b)
                       andalso (case IMil.IBlock.getTransfer'(imil, b)
                                 of Mil.TGoto _ => true
                                  | Mil.TCase _ => true
                                  | Mil.TInterProc _ => false
                                  | Mil.TReturn _ => false
                                  | Mil.TCut _ => false
                                  | Mil.TPSumCase _ => true))
                   then
                     let
(*
                       val () = Debug.prints (d, "maybe redundant extension\n")
                       val al = IMil.IBlock.layout (imil, a)
                       val bl = IMil.IBlock.layout (imil, b)
                       val cl = IMil.IBlock.layout (imil, c)
                       val () = Debug.printLayout (d, L.seq [L.str "\na:", al])
                       val () = Debug.printLayout (d, L.seq [L.str "\nb:", bl])
                       val () = Debug.printLayout (d, L.seq [L.str "\nc:", cl])
*)
                     in replaceTarget (a, b, c)
                     end
                   else ()
                 | _ => ())
            end

        fun checkEdge (a, b) =
            let
              val clist = IMil.IBlock.succs (imil, b)
            in 
              List.foreach (clist, fn c => checkConsequentEdges (a, b, c))
            end

        val blist = IMil.IBlock.succs (imil, a)
        val () = List.foreach (blist, fn b => checkEdge (a, b))
      in ()
      end
  
  fun rcbrCfg (d, imil, cfg) =
      let
        val () = splitCriticalEdge (imil, cfg)
        val () = Debug.layoutCfg (d, imil, cfg)
        val dom = IMil.IFunc.getDomTree (imil, cfg)
        val () = Debug.layoutTreeDot (d, imil, cfg, dom)
        val psDict = ref LD.empty
        val () = propagatePS' (d, psDict, imil, dom)

        val () = Tree.foreachPre (dom, fn b => checkBlockPSExt (d, imil, psDict, b))
      in 
        Tree.foreachPre(dom, fn b => checkBlockPS (d, imil, !psDict, b))
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
