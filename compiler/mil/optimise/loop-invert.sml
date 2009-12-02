
(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, January 2009 *)

(*
 * loop inversion, transform while loop to do while loop
 *)

signature MIL_LOOP_INVERT =
sig
  val pass : (BothMil.t, BothMil.t) Pass.t
end

structure MilLoopInvert :> MIL_LOOP_INVERT =
struct

  val passname = "MilLoopInvert"
  val stats = [(passname, "Loop Invert")]

  fun fail (f, m) = Fail.fail (passname, f, m)
  fun assert (f, m, b) = if b then fail (f, m) else ()

  structure M    = Mil
  structure PD   = PassData
  structure L    = Layout
  structure LU   = LayoutUtils
  structure ID   = Identifier
  structure LD   = ID.LabelDict
  structure VD   = ID.VariableDict
  structure LS   = ID.LabelSet
  structure VS   = ID.VariableSet
  structure LDOM = MilCfg.LabelDominance
  structure MU   = MilUtils
  structure ML   = MilLayout
  structure I    = Identifier
  structure TextIO = Pervasive.TextIO
  structure PD    = PassData

  structure Debug =
  struct
    val (debugPassD, debugPass) = Config.Debug.mk (passname, "debug the Mil loop invert")

    fun prints (d, s) = if Config.debug andalso debugPass (PD.getConfig d) then print s else ()

    fun printLayout (config, l) = if Config.debug andalso debugPass (config) then LU.printLayout l
                                  else ()

    fun layoutLoop (config, si, miloop, loop as MilLoop.L {header, blocks, ...}) =
        let
          fun layoutBlocks blks = L.sequence ("{", "}", ",") (List.map (LD.toList blks, fn (l, _) => ID.layoutLabel l))
        in
          L.seq [L.str "Header: ", 
                 ID.layoutLabel header,
                 L.str " Blocks: ",
                 layoutBlocks blocks,
                 L.str " Exits: ", 
                 LS.layout(MilLoop.getExits(miloop, header), fn x => ID.layoutLabel x)]
        end

    fun layoutWhileLoop (config, si, miloop, loop, r1, blocksNotInR1, isWhileLoop) =
        L.seq [L.str "isWhileLoop: ", 
               layoutLoop (config, si, miloop, loop),
               L.str " R1:",
               LS.layout (r1, fn x => ML.layoutLabel (config, si, x)),
               L.str " blocksNotInR1:",
               LS.layout (blocksNotInR1, fn x => ML.layoutLabel (config, si, x)),
               (if isWhileLoop then L.str " is while loop" else L.str " is not while loop")]

    fun layoutMap (config, si, map) =
        LD.layout (map, fn (ol, nl) => Layout.seq [ ML.layoutLabel (config, si, ol),
                                                    Layout.str " -> ",
                                                    ML.layoutLabel (config, si, nl)])
  end

  fun getLabel (p, b) = #1 (IMil.IBlock.getLabel' (p, b))

  fun getMappedVar (vvMap, var) =
      (case VD.lookup (!vvMap, var) 
        of SOME newv => newv 
         | _ => var)

  fun getMappedOpnd (vvMap, opnd) =
      (case opnd 
        of Mil.SVariable var => Mil.SVariable (getMappedVar (vvMap, var)) 
         | Mil.SConstant c => opnd)

  (* clone a variable, insert it in the dict, and return the new variable *)
  fun newVar (p, vvMap, oldv) =
      let  
        val newv = IMil.Var.clone (p, oldv)
        val () = vvMap := VD.insert (!vvMap, oldv, newv)
      in newv
      end

  (* get out edges' target blocks' label *)
  fun getOutBlocksLabel (p, ifunc, blockLabel) =
      let
        val b = IMil.IFunc.getBlockByLabel (p, ifunc, blockLabel)
        val outEdges = IMil.IBlock.outEdges (p, b)
        val outBlocks = List.map (outEdges, fn (_, l) => l)
        val outLabel = List.map (outBlocks, fn x => getLabel (p, x))
      in outLabel
      end

  fun appendOpndVector (vec, v) =
      Vector.fromList (List.append(Vector.toList(vec), [Mil.SVariable v]))

  fun isInRegion (l, r) = LS.exists (r, fn x => MU.Compare.label (x, l) = EQUAL)

  fun notR1 (l, r1) = not (isInRegion (l, r1))

  fun isCut (p, func, l) =
      let
        val b = IMil.IFunc.getBlockByLabel (p, func, l)
      in case IMil.IBlock.getTransfer' (p, b)
          of Mil.TCut _ => true
           | _ => false
      end

  (*
   * get duplicate block if it exists, otherwise return the original
   *)
  fun getR1' (x, r1Map) = (case LD.lookup (!r1Map, x)
                            of SOME y => y
                             | _ => x)

  fun toNewBlock (config, si, p, ifunc, bl, ol, nl) =
      let
        fun getNew block =
            if (MU.Compare.label (block, ol) = EQUAL) 
            then nl
            else block

        fun newCase (on, cases, default, tCase) =
            let
              fun doOne (c, Mil.T {block, arguments}) = (c, Mil.T {block = getNew block, arguments=arguments})
              val newbranches = Vector.map(cases, doOne)
              val newdefault =
                  case default
                   of SOME (Mil.T {block, arguments}) => SOME (Mil.T {block = getNew block, arguments=arguments})
                    | NONE => NONE
            in
              tCase {on = on, cases = newbranches, default = newdefault}
            end
            
        fun newTInterProc (callee, ret, fx) =
            let
              val nret = case ret
                          of Mil.RNormal {rets, block, cuts as Mil.C {exits, targets}} => 
                             let
                               val nblock = getNew block
                               val ncuts = Mil.C {exits = exits, 
                                                  targets = LS.fromList(List.map(LS.toList(targets), 
                                                                              fn x => getNew x ))}
                             in Mil.RNormal {rets = rets, block = nblock, cuts = ncuts}
                             end
                           | Mil.RTail => ret
            in Mil.TInterProc {callee = callee, ret = nret, fx = fx}
            end

        val b = IMil.IFunc.getBlockByLabel (p, ifunc, bl)
        val transfer = IMil.IBlock.getTransfer' (p, b)
        val nt =
            case transfer
             of Mil.TGoto (Mil.T {block, arguments}) => Mil.TGoto (Mil.T {block = getNew block, 
                                                                          arguments = arguments})
              | Mil.TCase {on, cases, default} => newCase (on, cases, default, Mil.TCase)
              | Mil.TReturn ret => Mil.TReturn (ret)
              | Mil.TInterProc {callee, ret, fx} => newTInterProc (callee, ret, fx)
              | Mil.TCut _ => fail ("toNewLoopHeader", "TCut")
              | Mil.TPSumCase {on, cases, default} => newCase (on, cases, default, Mil.TPSumCase)
        val () = IMil.IBlock.replaceTransfer (p, b, nt)
      in ()
      end

  (*
   * redirect the block's transfer taget to new loop header
   *)
  fun toNewLoopHeader (config, si, p, ifunc, miloop, loop as MilLoop.L {header, blocks, ...}, r1Map, bl) =
      toNewBlock (config, si, p, ifunc, bl, header, getR1' (header, r1Map))

  (*
   * Have all the edges into the loop (that is edges to H that are not from loop blocks) 
   *       goto the duplicated H instead of the original.
   *)
  fun linkToNewLoopHeader (config, si, p, ifunc, miloop, loop as MilLoop.L {header, blocks, ...}, r1Map) =
      let
        val preds = IMil.IBlock.preds (p, IMil.IFunc.getBlockByLabel(p, ifunc, header))
        val predsLabel = List.map (preds, fn x => getLabel(p, x))

        val loopBlocks = List.map (LD.toList(blocks), fn (l, _) => l)
        fun isLoopBlocks x = List.exists (loopBlocks, fn y => (MU.Compare.label(x, y) = EQUAL))
        val nonLoopBlocks = List.removeAll (predsLabel, isLoopBlocks)

        val duplicatedBlocks = List.map(LD.toList (!r1Map), fn (_, l) => l)
        fun isDuplicatedBlock x = List.exists (duplicatedBlocks, fn y => (MU.Compare.label(x, y) = EQUAL))
        val nonDuplicatedBlock = List.removeAll (nonLoopBlocks, isDuplicatedBlock)

        val () = List.foreach (nonDuplicatedBlock, fn l => toNewLoopHeader (config, si, p, ifunc, miloop, loop, r1Map, l))
      in ()
      end

  (* 
   * duplicate all the blocks in R1 
   *)
  fun dupR1 (config, si, p, ifunc, r1Map, r1, header, exit) =
      let
        fun mapBlks (b, nb) = r1Map := LD.insert (!r1Map, getLabel (p, b), getLabel (p, nb))
        val blks = List.map (LS.toList r1, fn l => (l, IMil.IFunc.getBlockByLabel (p, ifunc, l)))
        val newBlks = IMil.IFunc.duplicateBlocks (p, ifunc, blks, SOME mapBlks)
        val header' = getR1' (header, r1Map)
        val exit' = getR1' (exit, r1Map)
        val () = toNewBlock (config, si, p, ifunc, exit', header', header)
      in ()
      end

  fun replaceVarsInRegion (config, si, p, ifunc, vars, region, vvMap) =
      let
        fun replaceVar (config, si, p, ifunc, v, newv, l) =
            IMil.IFunc.renameBlock (p, ifunc, IMil.IFunc.getBlockByLabel (p, ifunc, l), v, newv)

        fun replaceVarInRegion (config, si, p, ifunc, v, newv, region) =
            LS.foreach (region, fn l => replaceVar (config, si, p, ifunc, v, newv, l))

      in VS.foreach (vars, fn v => replaceVarInRegion (config, si, p, ifunc, v, newVar (p, vvMap, v), region))
      end

  (* find those edges out r1 region *)
  fun findR1OutEdges (config, si, p, ifunc, r1, exit) =
      let
        val fname = "findR1OutEdges"
        val b = IMil.IFunc.getBlockByLabel (p, ifunc, exit)
        val outEdges = List.map (IMil.IBlock.outEdges (p, b), fn (bx, by) => (getLabel(p, bx), getLabel(p, by)))
      in List.keepAll (outEdges, fn (x, y) => notR1(y, r1))
      end

  (*
   * Given a region of code, find all variables (maybe just the ones live out variables) defined in that region.
   * 1. On all edges out of the region, add those variables as arguments to the gotos on those edges (could fail for cut edges).
   * 2. On all the other end of those edges, add parameters for those variables 
   *    and then rename all uses of those variables appropriately.
   * 3. Then the region can be duplicated without concern 
   *    (must clone all the binders and rename the uses of those, but those uses are all in that region).
   * 4. Other optimisations will clean this up.
   *)
  fun findLiveOutVar (config, si, p, ifunc, r1) =
      let
        fun findDefinedVar (config, si, p, ifunc, r1) =
            let
              (* itereate all instrs to get the dest variables *)
              fun findDefVarInstrs (io, vars) =
                  (case io
                    of SOME i => 
                       let
                         val nvars = VS.fromVector (IMil.IInstr.variablesDefined (p, i))
                       in
                         findDefVarInstrs (IMil.IInstr.next (p, i), VS.union (vars, nvars))
                       end  
                     | NONE => vars)

              (* find defined vars in block *)
              fun findDefinedVarBlock l =
                  let
                    val b = IMil.IFunc.getBlockByLabel (p, ifunc, l)
                    (* take parameters as defined variables *)
                    val pvars = VS.fromVector (IMil.IInstr.variablesDefined (p, IMil.IBlock.getLabel(p, b)))
                    (* get defined vars from instruction *)
                    val ivars = findDefVarInstrs (IMil.IBlock.getFirst (p, b), VS.empty)
                    (* get defined vars from transfer *)
                    val tvars = VS.fromVector (IMil.IInstr.variablesDefined (p, IMil.IBlock.getTransfer (p, b)))
                  in VS.union (VS.union (pvars, ivars), tvars)
                  end
            in LS.fold (r1, VS.empty, fn (l, vs) => VS.union (vs, findDefinedVarBlock l))
            end

        (*
         * get rest blocks, which doesn't belong to r1
         *)
        fun getR1Rest (config, si, p, ifunc, r1) =
            let
              val blocks = IMil.IFunc.getBlocks (p, ifunc)
              val r1Blocks = List.map (LS.toList(r1), fn x => IMil.IFunc.getBlockByLabel (p, ifunc, x))
              val rest = List.removeAll (blocks, fn x => List.exists(r1Blocks, fn y => x = y))
              val rest = List.map (rest, fn x => getLabel (p, x))
            in rest
            end

        (*
         * get the used variables in the block
         *)
        fun getUsedVar (p, ifunc, l) =
            IMil.IBlock.freeVars (p, IMil.IFunc.getBlockByLabel (p, ifunc, l))

        val rest = getR1Rest (config, si, p, ifunc, r1)
        val r1Def = findDefinedVar (config, si, p, ifunc, r1)         (* defined variables in r1 blocks*)
        val restUse = List.fold (rest, VS.empty, fn (l, vs) => VS.union (getUsedVar(p, ifunc, l), vs))
        val vars = VS.intersection (r1Def, restUse)                   (* defined in r1 and used in rest *)
        val () = Debug.printLayout (config, L.seq [L.str "vars to be parameterized: ", 
                                                   VS.layout (vars, fn x => ML.layoutVariable (config, si, x))])
      in vars
      end

  (* REMOVE THIS FUNCTION, DEBUG ONLY *)
  fun debugLoops (config, si, header) =
      let
(*
        val ins = TextIO.openIn "c:\\p\\ppiler-test\\debug.loop.txt"
        fun readLoopList (loopList) =
            (case TextIO.inputLine ins
              of SOME line => 
                 let
                   val newLoopList = List.insert (loopList, line, fn (x, y) => x = y)
                 in readLoopList newLoopList
                 end
               | _ => loopList)

        fun sameLabel x = (L.toString(ML.layoutLabel (config, si, header)) ^ "\n") = x
            
        val loopList = readLoopList ([])
        val () = TextIO.closeIn ins
        val dbg = List.exists (loopList, sameLabel)
*)
      in true
      end

  (*
   * get all blocks, which do not belong to the loop, define it as r0
   *)
  fun getR0 (config, si, p, ifunc, loop as MilLoop.L {header, blocks, ...}) =
      let
        val allLabels = List.map (IMil.IFunc.getBlocks (p, ifunc), fn b => getLabel (p, b))
        val loopBlocks = List.map (LD.toList blocks, fn (l, _) => l)
      in LS.fromList (List.removeAll (allLabels, fn x => List.exists(loopBlocks, fn y => x = y)))
      end

  (*
   * add a new variable to transfer instruction's parameter
   *)
  fun paramOutEdgeTrans (config, si, p, ifunc, predl, bl, v) =
      let
        val fname = "paramOutEdgeTrans"
        val b = IMil.IFunc.getBlockByLabel (p, ifunc, bl)
        val pred = IMil.IFunc.getBlockByLabel (p, ifunc, predl)
        val predt = IMil.IBlock.getTransfer' (p, pred)
                    
        fun newCase (on, cases, default, tCase) =
            let
              fun newTarget (Mil.T {block, arguments}) =
                  if (MU.Compare.label (block, bl) = EQUAL) 
                  then Mil.T {block = block, arguments = appendOpndVector (arguments, v)}
                  else Mil.T {block = block, arguments = arguments}
                       
              val newbranches = Vector.map(cases, fn (c, tgt) => (c, newTarget tgt))
              val newdefault =
                  case default
                   of SOME tgt => SOME (newTarget tgt)
                    | NONE => NONE
            in
              tCase {on = on, cases = newbranches, default = newdefault}
            end
            
        val nt = case predt
                  of Mil.TGoto (Mil.T {block, arguments}) =>
                     Mil.TGoto (Mil.T {block = block, arguments = appendOpndVector (arguments, v)})
                   | Mil.TCase {on, cases, default} => newCase (on, cases, default, Mil.TCase)
                   | Mil.TInterProc {callee, ret, fx} => 
                     let
                       val newCallee = 
                           (case callee 
                             of Mil.IpCall {call, args} => Mil.IpCall {call = call, args = appendOpndVector (args, v)}
                              | Mil.IpEval {typ, eval} => Mil.IpEval {typ = typ, eval = eval})
                     in Mil.TInterProc {callee = newCallee, ret = ret, fx = fx}
                     end
                   | Mil.TReturn opnd => fail (fname, "TReturn")
                   | Mil.TCut {cont, args, cuts} => fail (fname, "TCut") 
                   | Mil.TPSumCase {on, cases, default} => newCase (on, cases, default, Mil.TPSumCase)
      in IMil.IBlock.replaceTransfer (p, pred, nt)
      end

  (*
   * a function parameterizeRegion which given a region R, an edge E into that region, and a list of variables V:
   * 1)	splits the edge if needed (possibly twice)
   *    a.  If the source of E is a call, then we need to add a landing pad block to pass the parameters out.  
   *        This block is outside of R.
   *    b.  If the target of E has multiple predecessors then we need to add an entry block to receive the parameters.  
   *        This block is inside of R.
   *    c.  if E is a cut edge, fail.
   * 2)	replaces V with new variables V’ in R
   * 3)	passes V as additional parameters from the source of E (or the landing pad block)
   * 4)	modifies the target of E (or the entry block) to bind V’
   *)
  fun paramRegion (config, si, p, ifunc, region, srcl, tgtl, vars) =
      let
        val fname = "paramRegion"
        (*
         * vvMap is used to record cloned variable
         * the cloned variable will be used to replace the orignal variable in r2 and non loop blocks
         *)
        val vvMap = ref VD.empty
        val srcb = IMil.IFunc.getBlockByLabel (p, ifunc, srcl)
        val tgtb = IMil.IFunc.getBlockByLabel (p, ifunc, tgtl)
        (* step 1*)
        val nbo = IMil.IBlock.splitEdge (p, (srcb, tgtb)) (* always split edge to simplify *)
        val () = if isCut(p, ifunc, srcl) 
                 then fail (fname, "cannot be cut transfer here!")
                 else ()
        (* step 2 *)
        val () = replaceVarsInRegion (config, si, p, ifunc, vars, region, vvMap)
        val () = case nbo
                  of SOME nb => 
                     let
                       val nbl = getLabel (p, nb)
                       (* step 3 *)
                       val () = VS.foreach (vars, fn v => paramOutEdgeTrans (config, si, p, ifunc, srcl, nbl, v))
                       (* step 4 *)
                       val nbParam = IMil.IBlock.getParameters (p, nb)
                       val newVars = List.map (VS.toList vars, fn x => getMappedVar(vvMap, x))
                       val newParamList = List.append (Vector.toList nbParam, newVars)
                       val newParam = Vector.fromList newParamList
                     in IMil.IBlock.replaceLabel (p, nb, (nbl, newParam))
                     end
                   | NONE => fail (fname, "cannot split the edge!")
      in ()
      end

  fun paramOutEdges (config, si, p, ifunc, outEdges, vars, r1, r2, loop as MilLoop.L {header, blocks, ...}) =
      let
        val r0 = getR0 (config, si, p, ifunc, loop)
        fun paramOneEdge (srcl, tgtl) =
            if (isInRegion (tgtl, r2)) 
            then paramRegion (config, si, p, ifunc, r2, srcl, tgtl, vars)
            else paramRegion (config, si, p, ifunc, r0, srcl, tgtl, vars)
      in List.foreach (outEdges, fn (sl, el) => paramOneEdge (sl, el))
      end
  (*
   * convert simple while loop (contains only one exit block)
   *)
  fun convertSimpleWhileLoop (d, config, si, p, ifunc, miloop, loop as MilLoop.L {header, blocks, ...}, r1, r2, exit) =
      if debugLoops (config, si, header) then 
        let
          (* 
           * r1Map is used to recored block duplication in r1 
           * (original block, duplicated block)
           *)
          val r1Map = ref LD.empty
          val vars = findLiveOutVar (config, si, p, ifunc, r1)
          val outEdges = findR1OutEdges (config, si, p, ifunc, r1, exit)
          val () = paramOutEdges (config, si, p, ifunc, outEdges, vars,  r1, r2, loop)
          val () = dupR1 (config, si, p, ifunc, r1Map, r1, header, exit)
          val () = linkToNewLoopHeader (config, si, p, ifunc, miloop, loop, r1Map)
          val () = PD.click (d, passname)
          val () = Debug.printLayout (config, Debug.layoutMap (config, si, !r1Map))
        in ()
        end
      else ()

  (*
   * convert while loop to do while 
   *)           
  fun convertWhileLoop (d, config, si, p, ifunc, miloop, loop as MilLoop.L {header, blocks, ...}, children, r1, r2) =
      let
        (*
         * only convert while loop with one exit block, and dont have children loop!
         *)
        val exits = MilLoop.getExits (miloop, header)
      in
        if (LS.size exits = 1 
            andalso  Vector.length (children) = 0)
        then convertSimpleWhileLoop (d, config, si, p, ifunc, miloop, loop, r1, r2, List.first (LS.toList (exits)))
        else ()
      end

  (*
   * H is the loop header.
   * H and E are in R1.
   * E has two outgoing edges, one to a block outside the loop and one to a block in R1.
   * Blocks in R1 other than E only go to blocks in R1.
   * edges in R1 not to H
   *)
  fun getR1 (config, si, p, ifunc, loop as MilLoop.L {header, blocks, ...}, loopExits) =
      let
        fun notToH (H, l) =
            let
              val outLabel = getOutBlocksLabel (p, ifunc, l)
              val ret = List.exists (outLabel, fn x => (MU.Compare.label (x, H) = EQUAL))
            in not ret
            end

        fun isToR1Only (r1, l) =
            let
              val outLabel = getOutBlocksLabel (p, ifunc, l)
              val ret = List.fold(outLabel, true, fn (x, r) => LS.exists (r1, fn y => (MU.Compare.label (x, y) = EQUAL)))
            in ret
            end

        fun mergeR1 (bls, header, r1) =
            let
              val ns = List.keepAll(bls, fn x => isToR1Only(r1, x) andalso notToH (header, x))
              val nbls = List.removeAll(bls, fn x => List.exists (ns, fn y => (MU.Compare.label (x, y) = EQUAL)))
            in 
              (* if there are new blocks merged into r1, then repeat the merging till it becomes stable *)
              if List.length (ns) > 0 
              then mergeR1 (nbls, header, LS.union (r1, LS.fromList ns)) 
              else r1
            end

        (* header and exits *)
        val HE = LS.insert (loopExits, header)
        val bls = List.map (LD.toList(blocks), fn (l, _) => l)
      in mergeR1 (bls, header, HE)
      end

  (*
   * get the blocks label set in the loop, which does not belong to r1
   *)
  fun getBlocksNotInR1 (loop as MilLoop.L {header, blocks, ...}, R1) =
      let
        (* the label list includes all blocks *)
        val bls = List.map (LD.toList(blocks), fn (l, _) => l)
        (* blocks not in R1 region *)
        val blocksNotInR1 = List.removeAll (bls, fn x => LS.exists (R1, fn y => (MU.Compare.label (x, y) = EQUAL)))
      in LS.fromList blocksNotInR1
      end

  (*
   * check those non r1 blocks
   * whether all of them meet the requirement of r2
   * if yes, then it forms a while loop
   *)
  fun isR2Region (config, si, p, ifunc, loop, R1, blocksNotInR1) =
      let
        (*
         * Blocks in R2 go only to other blocks in R2 or to H.
         *)
        fun isBlockInR2 (config, si, p, ifunc, blockLabel, loop as MilLoop.L {header, ...}, R2) =
            let 
              val R2H = LS.insert (R2, header) (* R2 and H *)
              val outBlocksLabel = getOutBlocksLabel (p, ifunc, blockLabel)
            in List.fold (outBlocksLabel, true, fn (x, r) => LS.exists (R2H, fn y => (MU.Compare.label (x, y) = EQUAL)))
            end

      in LS.fold (blocksNotInR1, true, fn (x, y) => (isBlockInR2 (config, si, p, ifunc, x, loop, blocksNotInR1) andalso y))
      end

  (*
   * find the while loop, and then conert it
   *)
  fun tryConvertWhileLoop (d, config, si, m, p, ifunc, miloop, loop as MilLoop.L {header, blocks, ...}, children) =
      let
        fun hasCutTransfer (config, si, p, func, r (*region*)) =
            let
            in LS.fold (r, false, fn (l, ret) => (ret orelse (isCut (p, func, l))))
            end

        val r1 = getR1 (config, si, p, ifunc, loop, MilLoop.getExits (miloop, header))
        val blocksNotInR1 = getBlocksNotInR1 (loop, r1) 
        val isWhileLoop = isR2Region (config, si, p, ifunc, loop, r1, blocksNotInR1)
        val isWhileLoop = (not (hasCutTransfer (config, si, p, ifunc, r1))) andalso isWhileLoop
        val () = Debug.printLayout (config, Debug.layoutWhileLoop (config, si, miloop, loop, r1, blocksNotInR1, isWhileLoop))
      in
        if isWhileLoop 
        then convertWhileLoop (d, config, si, p, ifunc, miloop, loop, children, r1, blocksNotInR1) 
        else ()
      end

  (*
   * traverse the loop tree to find while loop
   *)
  fun tryConvertWhileLoops (d, config, si, m, p, ifunc, miloop, loops) =
      let
        val Tree.T (parent as MilLoop.L {header, blocks, ...}, children) = loops
        val () = tryConvertWhileLoop (d, config, si, m, p, ifunc, miloop, parent, children)
        val () = Vector.foreach (children, fn x => tryConvertWhileLoops (d, config, si, m, p, ifunc, miloop, x))
      in ()
      end

  (*
   * transfrom every loop in the function
   *)
  fun loopInvert (d, config, m as Mil.P {globals, symbolTable, ...}, imil, ifunc) =
      let
        val (gv, global) = IMil.IFunc.unBuild ifunc
        val si = ID.SymbolInfo.SiTable symbolTable
      in (case global
           of Mil.GCode (c as Mil.F {body, ...}) =>
              let
                val cfg = MilCfg.build(config, si, body)
                val lbdomtree = MilCfg.getLabelBlockDomTree cfg
                val linfo = MilLoop.build (config, si, cfg, lbdomtree)
                val linfo = MilLoop.genAllNodes linfo
                val linfo = MilLoop.genExits linfo
                val loops = MilLoop.getLoops linfo
                val () = Debug.printLayout (config, L.seq [L.str "MilLoop: ", MilLoop.layout linfo])
                val () = Vector.foreach(loops, fn x => tryConvertWhileLoops (d, config, si, m, imil, ifunc, linfo, x))
              in ()
              end
            | _ => ())
      end

  fun program (imil, d) = 
      let
        val config = PD.getConfig d
        val m as Mil.P {globals, symbolTable, ...} = IMil.T.unBuild imil
        val () = List.foreach (IMil.Enumerate.T.funcs imil, fn ifunc => loopInvert (d, config, m, imil, ifunc))
        val () = PD.report (d, passname)
      in ()
      end

  val description = {name        = passname,
                     description = "Loop Invert",
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
