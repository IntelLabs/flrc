
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
  structure IMT = IMilTypes

  datatype psCond =
           rcons of M.constant
         | rname of ID.name

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
        | _ => false

  fun eqCond (c1, c2) =
      case c1
       of rname n1 => (case c2
                       of rname n2 => n1 = n2
                        | _ => false)
        | rcons cc1 => (case c2
                        of rcons cc2 => eqConstant(cc1, cc2)
                         | _ => false)

  fun PSCompare ((o1, n1, b1), (o2, n2, b2)) = 
      if eqOpnd(o1, o2) andalso eqCond(n1, n2) andalso b1 = b2 
      then EQUAL else LESS
                              
  structure PSSet = SetF (struct 
                            (* variable, name, equal or not *)
                            type t = M.operand * psCond * bool
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
         | Unhandled  (* other cases *)

  val (debugPassD, debugPass) =
      Config.Debug.mk (passname, "debug the Mil branch removal pass")

  val gDict = ref LD.empty

  fun debugShowPre (d, imil, fname)  = 
      if Config.debug andalso debugPass (PD.getConfig d) then
        if (Config.debugLevel (PD.getConfig d, passname)) > 0 then 
          let
            val () = print ("before branch removal:\n")
(*            val () = IMil.printCfg (imil, IMil.Cfg.getCfgByName (imil, fname))*)
            val () = print "\n"
          in ()
          end
        else 
          let
            val () = print ("Branch removal: ")
(*            val () = IMil.printVar (imil, fname)*)
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

  fun debugPrint () = true

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

  fun printPS (opnd, cond, b) =
      if debugPrint() then
        let
          val () = print ("(")
          val () = case opnd
                    of M.SVariable v => print (ID.variableString' v)
                     | M.SConstant c => (case c 
                                          of M.CName n => 
                                             print ("SConstant " ^ID.nameString' n)
                                           | _ => fail ("SConstant", "unknown"))
                     | _ => fail ("SCoerce", "unsupported")
                     
          val () = if b then print (" = ") else print (" <> ")
          val () = case cond
                    of rname n => print (ID.nameString' n)
                     | rcons c => (case c
                                    of M.CName n => print (ID.nameString' n)
                                     | M.CIntegral i => print(IntArb.stringOf i)
                                     | M.CFloat f => print(Real32.toString f)
                                     | M.CDouble d => print (Real64.toString d)
                                     | _ => fail ("rcons", "unknown"))

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
      let
        val () = if state = Redundant then print ("redundant edge: ")
                 else if state = Unknown then print ("unknown edge: ")
                 else if state = Impossible then print ("impossible edge: ")
                 else print ("unhandled edge: ")
        val () = printEdge (imil, e)
        val () = printPSSet (getLabel(imil, a), ps)
                 
        val instr = IMil.IBlock.getTransfer(imil, a)
        val () = LU.printLayout (IMil.IInstr.layout (imil, instr))
        val () = print("\n")
      in ()
      end

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
          val () = LU.printLayout (IMil.IInstr.layout (imil, newinstr))
          val () = print ("\n")
        in ()
        end
      else ()

  fun layoutCfg (imil, cfg) =
      if debugPrint() then
        let
(*
          val cfgId = IMil.Cfg.getId (imil, cfg)
          val () = print ("Cfg ID:" ^ Int.toString(cfgId) ^ "\n")
          val () = IMil.printCfg (imil, cfg)
                   
          val ld = IMil.Cfg.layoutDot (imil, cfg)
          val fname = "cfg" ^ Int.toString(cfgId) ^ ".dot" 
          val ()  = LU.writeLayout (valOf(ld), fname)
*)
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
  fun findCriticalEdges (imil, cfg) =
      let
        fun isCritical (imil, e as (a, b)) =
            let 
              val oe = IMil.IBlock.outEdges (imil, a)
              val ie = IMil.IBlock.inEdges (imil, b)
            in (List.length(oe) > 1) andalso (List.length(ie) > 1)
            end

        fun findCE (imil, b) =
            let
              val es = IMil.IBlock.outEdges (imil, b)
            in 
              List.keepAll (es, fn e => isCritical (imil, e))
            end
(*        val bl = IMil.Cfg.getBlockList (imil, cfg)*)
        val bl = ILD.fold (IMT.iFuncGetIBlocks c, [], fn (l, _, ll) => l::ll)
      in 
        List.fold (bl, [], fn (b, l) => findCE (imil, b) @ l)
      end

  fun splitCriticalEdge (imil, cfg) =
      let
        val ces = findCriticalEdges (imil, cfg)
        fun splitEdge e = IMil.IBlock.splitEdge (imil, e)
      in 
        List.foreach(ces, splitEdge)
      end

  (* WL: CODE REVIE HERE *)
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

  fun isTCase (imil, a, b) = isSome(getTCase(imil, (a, b)))

  fun getTransOpnd (imil, a, b) = 
      let
        fun getSwitchOpnd (opnd, arms, defo) = case opnd
                                             of M.SVariable v => SOME opnd
                                              | M.SConstant c => SOME opnd
                                              | _ => NONE 

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
        fun getPCasePS (imil, a, b, (opnd, arms, defo)) =
            let
              val opnd = getTransOpnd (imil, a, b)
              val bl = getLabel (imil, b)
              fun eq (n, M.T {block, arguments}) = block = bl
              val item = Vector.peek (arms, eq)
            in case item
                of SOME (n, t) => SOME (valOf opnd, rname n, true)
                 | _ => NONE
            end

        fun getTCasePS (imil, a, b, (opnd, arms, defo)) =
            let
              val opnd = getTransOpnd (imil, a, b)
              val bl = getLabel (imil, b)
              fun eq (n, M.T {block, arguments}) = block = bl
              val item = Vector.peek (arms, eq)
            in case item
                of SOME (n, t) => SOME (valOf opnd, rcons n, true)
                 | _ => NONE
            end

      in case getTransMil (imil, a)
          of IMil.MTransfer t =>
             (case t
               of M.TPSumCase ns => getPCasePS (imil, a, b, ns)
                | M.TCase sw => getTCasePS (imil, a, b, sw)
                | _ => NONE )
           | _ => NONE
      end

  fun getEdgePSSet (imil, e) : PSSet.t =
      case getEdgePS (imil, e)
       of SOME it => PSSet.insert (PSSet.empty, it)
        | _ => PSSet.empty

(*
  fun getEdgesPSSet (imil, es : edge List.t) : PSSet.t =
      let
        fun getEdgePSSet' e = getEdgePSSet (imil, e)
      in List.fold (es, 
                    PSSet.empty, 
                    fn (d, set) => PSSet.union (set, getEdgePSSet' d))
      end
*)

  (* propagate PS on dom tree *)
  fun propagatePS (imil, tree) =
      let
        fun travNode (a) =
            let
              val label = getLabel(imil, a)
              val inEdges = IMil.IBlock.inEdges(imil, a)
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

  fun getEdgePSState (imil, e as (a, b), ps) =
      let
        val epso = getEdgePS (imil, e)

        fun isRedundant (item, ps) = PSSet.member (ps, item)

        fun isImp ((eopnd, en, eb), ps) = (* impossible *)
            let
              fun hasSameOpnd (item as (opnd, n, b)) = eqOpnd(opnd, eopnd)
              fun hasSameCond (item as (v, n, b)) = eqCond (n, en)
              fun isImp' x = (hasSameOpnd x) andalso (not (hasSameCond x))
            in 
              PSSet.exists (ps, isImp')
            end

        fun isUnknown ((eopnd, en, eb), ps) =
            let
              fun hasSameOpnd (item as (opnd, n, b)) = eqOpnd (opnd, eopnd)
              fun isUnknown' x = not (hasSameOpnd x)
            in
              PSSet.exists (ps, isUnknown')
            end

      val state = 
          if isSome(epso) then
            let
              val eps = valOf epso
            in 
              if isRedundant(eps, ps) andalso (not (isImp(eps, ps))) then Redundant
              else if isImp(eps, ps) then Impossible
              else if isUnknown(eps, ps) then Unknown
              else Unhandled
            end
          else Unhandled

      val () = printPSSet(getLabel(imil, a), getEdgePSSet(imil, e))
      val () = printEdgePSState(imil, e, ps, state)
      
      in state
      end

  fun removeRedundant (imil, e as (a, b)) =
      let
        fun removeTarget (imil, ns as (opnd, arms, defo), e as (a, b)) =
            let
              fun eq (arm as (_, M.T {block, arguments})) = 
                  block = getLabel (imil, b) 

            in (opnd, Vector.keepAll (arms, eq), NONE)
            end

        fun replacePSumCaseInstr (imil, e as (a, b), ns as (opnd, arms, defo)) =
            let
              val instr = IMil.IBlock.getTransfer (imil, a)             

              val () = printOrigInstr (imil, e, instr)

              val newns = removeTarget (imil, ns, e)
              val nt = M.TPSumCase (newns)
             
              val nmt = IMil.MTransfer nt
              val ni = IMil.IInstr.new' (imil, nmt)
              val nmi = IMil.IInstr.getMil (imil, ni)
              val newinstr = IMil.IInstr.replaceMil' (imil, instr, nmi)
        
              val () = printNewInstr (imil, newinstr)
            in ()
            end

        fun replaceTCaseInstr (imil, e as (a, b), cs as (opnd, arms, defo)) =
            let
              val () = print ("find redundant tcase instruction\n")
              val instr = IMil.IBlock.getTransfer (imil, a)             

              val () = printOrigInstr (imil, e, instr)

              val newns = removeTarget (imil, cs, e)
              val nt = M.TCase (newns)
             
              val nmt = IMil.MTransfer nt
              val ni = IMil.IInstr.new' (imil, nmt)
              val nmi = IMil.IInstr.getMil (imil, ni)
              val newinstr = IMil.IInstr.replaceMil' (imil, instr, nmi)
        
              val () = printNewInstr (imil, newinstr)
            in ()
            end

      in case getPSumCase (imil, e)
          of SOME ns => replacePSumCaseInstr(imil, e, ns)
           | _ => (case getTCase (imil, e)
                    of SOME cs => replaceTCaseInstr(imil, e, cs)
                     | _ => ())
      end

  fun checkRedundant (imil, e as (a, b), ps) =
      if getEdgePSState (imil, e, ps) = Redundant 
      then removeRedundant (imil, e)
      else ()

  fun checkEdgePS (imil, dict, e as (a, b)) =
      if isPSumCase (imil, a, b) orelse isTCase (imil, a, b) then
        case LD.lookup (dict, getLabel (imil, a))
         of SOME ps => checkRedundant (imil, e, ps)
          | _ => ()
      else ()

  fun checkBlockPS (imil, dict, a) =
      let
        fun checkEdgePS' e = checkEdgePS (imil, dict, e)

        val outEdges = List.map(IMil.IBlock.succs (imil, a), fn b => (a, b))
      in 
        List.foreach (outEdges, checkEdgePS')
      end

  fun getDomTreeEdges (Tree.T (a, v)) = 
      let
        fun getDomEdges (b, []) = []
          | getDomEdges (b, (Tree.T (a, v))::xs) = 
            (b, a)::getDomEdges(a, Vector.toList v)@getDomEdges(b, xs)
      in 
        getDomEdges (a, Vector.toList v)
      end

  fun rcbrCfg (imil, cfg) =
      let
        val cn = IMil.Cfg.getFName(imil, cfg)
        val () = print("cfg:" ^ ID.variableString' (cn) ^ "\n")

        val () = splitCriticalEdge (imil, cfg)

        val dom = IMil.Cfg.getDomTree (imil, cfg)

        val () = propagatePS (imil, dom)
        val () = print ("finish propagatePS\n")

        fun checkBlockPS' b = checkBlockPS (imil, !gDict, b)
      in 
        Tree.foreachPre(dom, checkBlockPS')
      end

  fun program (imil, d) = 
      let
        fun rcbrCfg' (cfg) = rcbrCfg (imil, cfg)
        val () = List.foreach (IMil.Enumerate.T.cfgs imil, rcbrCfg')

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
