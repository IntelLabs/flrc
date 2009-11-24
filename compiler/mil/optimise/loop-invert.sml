
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

    fun layoutEdge (config, si, (l1, l2)) =
        L.seq [L.str "(", 
               ML.layoutLabel (config, si, l1),
               L.str ",",
               ML.layoutLabel(config, si, l2),
               L.str ")"]

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

    fun layoutConvertLoop (config, si, H', r1Map) =
        L.seq [L.str "convertSimpleWhileLoop: ",
               L.str " getH' = ",
               ML.layoutLabel (config, si, H'),
               L.str " r1Map = ",
               layoutMap (config, si, !r1Map)]
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
        val outLabel' = List.map (outBlocks, fn x => IMil.IBlock.getLabel' (p, x))
        val outLabel = List.map (outLabel', fn (l, _) => l)
      in outLabel
      end

  fun appendVarVector (vec, v) =
      Vector.fromList (List.append(Vector.toList(vec), [v]))

  fun appendOpndVector (vec, v) =
      Vector.fromList (List.append(Vector.toList(vec), [Mil.SVariable v]))

  fun isInRegion (l, r) = LS.exists (r, fn x => MU.Compare.label (x, l) = EQUAL)

  fun notR1 (l, r1) = not (isInRegion (l, r1))

 (*
  * Let H' be the block that E goes to in the loop.
  * Duplicate the blocks R1.
  * Have all the edges into the loop (that is edges to H that are not from loop blocks) 
  *       goto the duplicated H instead of the original.
  * Now H' should be a loop header instead of either the original or duplicated H.
  * H' and the original E should form a do-while loop.
  *)
  fun getH' (config, si, p, ifunc, miloop, loop as MilLoop.L {header, blocks, ...}) =
      let
        fun isBlockInsideLoop (loop as MilLoop.L {header, blocks, ...}, bl) =
            let
              val ls = List.map (LD.toList blocks, fn (l, _) => l)
            in List.exists (ls, fn x => MU.Compare.label (x, bl) = EQUAL)
            end

        val exits = MilLoop.getExits (miloop, header)
        val () = if LS.size(exits) > 1 
                 then fail ("getH'", "dont handle multiple exits loop now")
                 else ()
        val exitLabelO = LS.getAny exits
        val outLabel = case exitLabelO
                        of SOME l => getOutBlocksLabel (p, ifunc, l)
                         | _ => fail ("getH'", "exits is empty")
        val outLabelInsideLoop = List.keepAll (outLabel, fn x => isBlockInsideLoop (loop, x))
        val len = List.length (outLabelInsideLoop)
        val H' = if len = 1 
                 then SOME (List.first outLabelInsideLoop)
                 else 
                   let
                     val () = Debug.printLayout (config, 
                                                 L.seq [L.str "exits: ", 
                                                        LS.layout (exits, fn x => ML.layoutLabel (config, si, x)),
                                                        L.str " outLabel: ",
                                                        List.layout (fn x => ML.layoutLabel (config, si, x)) outLabel,
                                                        L.str " cannot get H'"])
                   in NONE
                   end
      in H'
      end

  (*
   * redirect the block's transfer taget to new loop header
   *)
  fun toNewLoopHeader (config, si, p, ifunc, miloop, loop as MilLoop.L {header, blocks, ...}, r1Map, bl) =
      let
        fun getHeader' (loop as MilLoop.L {header, blocks, ...}, bl, r1Map) =
            if (MU.Compare.label (bl, header) = EQUAL) then
              (case LD.lookup (!r1Map, header)
                of SOME y => y
                 | _ => fail ("getHeader'", "header is not duplicated"))
            else bl

        fun newCase (on, cases, default, loop, r1Map, tCase) =
            let
              fun doOne (c, Mil.T {block, arguments}) = (c, Mil.T {block=getHeader' (loop, block, r1Map), arguments=arguments})
              val newbranches = Vector.map(cases, doOne)
              val newdefault =
                  case default
                   of SOME (Mil.T {block, arguments}) => SOME (Mil.T {block=getHeader'(loop, block, r1Map), arguments=arguments})
                    | NONE => NONE
            in
              tCase {on = on, cases = newbranches, default = newdefault}
            end
            
        fun newTInterProc (callee, ret, fx, r1Map, loop) =
            let
              val nret = case ret
                          of Mil.RNormal {rets, block, cuts as Mil.C {exits, targets}} => 
                             let
                               val nblock = getHeader' (loop, block, r1Map)
                               val ncuts = Mil.C {exits = exits, targets = LS.fromList(List.map(LS.toList(targets), 
                                                                                             fn x => getHeader' (loop, x, r1Map)))}
                             in Mil.RNormal {rets = rets, block = nblock, cuts = ncuts}
                             end
                           | Mil.RTail => ret
            in Mil.TInterProc {callee = callee, ret = nret, fx = fx}
            end

        val b = IMil.IFunc.getBlockByLabel (p, ifunc, bl)
        val transfer = IMil.IBlock.getTransfer' (p, b)
        val nt =
            case transfer
             of Mil.TGoto (Mil.T {block, arguments}) => Mil.TGoto (Mil.T {block=getHeader' (loop, block, r1Map), 
                                                                          arguments=arguments})
              | Mil.TCase {on, cases, default} => newCase (on, cases, default, loop, r1Map, Mil.TCase)
              | Mil.TReturn ret => Mil.TReturn (ret)
              | Mil.TInterProc {callee, ret, fx} => newTInterProc (callee, ret, fx, r1Map, loop)
              | Mil.TCut _ => fail ("toNewLoopHeader", "TCut")
              | Mil.TPSumCase {on, cases, default} => newCase (on, cases, default, loop, r1Map, Mil.TPSumCase)
      in IMil.IBlock.replaceTransfer (p, b, nt)
      end

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

  fun dupBlockInstrs (config, si, p, ifunc, r1Map, vvMap, nb, bl, i) =
      let
        val fname = "dupBlockInstrs"

        fun getDests (dests) =
            if Vector.length (dests) = 1 
            then Vector.new1(newVar(p, vvMap, Vector.sub (dests, 0)))
            else dests

        fun getRhs rhs =
            let
              val nrhs = case rhs 
                          of Mil.RhsSimple s => fail(fname ^ "RhsSimple ", "not handled")
                           | Mil.RhsPrim {prim, createThunks, args} =>
                             let
                               val nrhs = Mil.RhsPrim {prim = prim, 
                                                       createThunks = createThunks, 
                                                       args = Vector.map(args, fn x => getMappedOpnd (vvMap, x))} 
                             in nrhs
                             end
                           | Mil.RhsTuple {vtDesc, inits} => 
                             let
                               val newInits = Vector.map(inits, fn x => getMappedOpnd (vvMap, x))
                             in Mil.RhsTuple {vtDesc = vtDesc, inits = newInits}
                             end
                           | Mil.RhsTupleSub (ts as Mil.TF {tupDesc, tup, field}) => 
                             let
                               val newTup = getMappedVar (vvMap, tup)
                               val newField = case field
                                               of Mil.FiFixed i => field
                                                | Mil.FiVariable opnd => Mil.FiVariable (getMappedOpnd (vvMap, opnd))
                                                | Mil.FiViFixed _ => field
                                                | Mil.FiViVariable {typ, idx} => Mil.FiViVariable {typ = typ, idx = getMappedOpnd (vvMap, idx)}
                                                | Mil.FiViIndexed {typ, idx} => Mil.FiViIndexed {typ = typ, idx = getMappedOpnd (vvMap, idx)}
                             in Mil.RhsTupleSub (Mil.TF {tupDesc = tupDesc, tup = newTup, field = newField})
                             end
                           | Mil.RhsTupleSet {tupField as Mil.TF {tupDesc, tup, field}, ofVal} => 
                             let
                               val newTup = getMappedVar (vvMap, tup)
                               val newField = case field
                                               of Mil.FiFixed _ => field
                                                | Mil.FiVariable opnd => Mil.FiVariable (getMappedOpnd (vvMap, opnd))
                                                                | _ => fail (fname ^ "RhsTupleSet ", "not handled")
                               val newTupField = Mil.TF {tupDesc = tupDesc, tup = newTup, field = newField}
                               val newOfVal = getMappedOpnd (vvMap, ofVal)
                               val nrhs = Mil.RhsTupleSet {tupField = newTupField, ofVal = newOfVal}
                             in nrhs
                             end
                           | Mil.RhsTupleInited _ => fail (fname ^ "RhsTupleInited ", "not handled")
                           | Mil.RhsIdxGet _ => fail (fname ^ "RhsIdxGet ", "not handled")
                           | Mil.RhsCont _ => fail (fname ^ "RhsCont ", "not handled")
                           | Mil.RhsObjectGetKind _ => fail (fname ^ "RhsObjectGetKind ", "not handled")
                           | Mil.RhsThunkMk tm => Mil.RhsThunkMk tm
                           | Mil.RhsThunkInit {typ, thunk, fx, code, fvs} => 
                             let
                               val newThunk = case thunk 
                                               of SOME t => SOME (getMappedVar (vvMap, t))
                                                | _ => thunk
                               val newCode = case code 
                                              of SOME c => SOME (getMappedVar (vvMap, c))
                                               | _ => code
                               val newFvs = Vector.map (fvs, fn (fk, opnd) => (fk, getMappedOpnd(vvMap, opnd)))
                                            
                             in Mil.RhsThunkInit {typ = typ,
                                                  thunk = newThunk,
                                                  fx = fx,
                                                  code = newCode,
                                                  fvs = newFvs}
                             end
                           | Mil.RhsThunkGetFv _ => fail (fname ^ "RhsThunkGetFv ", "not handled")
                           | Mil.RhsThunkValue {typ, thunk, ofVal} => 
                             let
                               val newThunk = case thunk 
                                               of SOME t => SOME (getMappedVar (vvMap, t))
                                                | _ => thunk
                               val nrhs = Mil.RhsThunkValue {typ = typ,
                                                             thunk = newThunk,
                                                             ofVal = getMappedOpnd(vvMap, ofVal)}
                             in nrhs
                             end
                           | Mil.RhsThunkGetValue _ => fail (fname ^ "RhsThunkGetValue ", "not handled")
                           | Mil.RhsThunkSpawn {typ, thunk, fx} => 
                             Mil.RhsThunkSpawn {typ = typ, thunk = getMappedVar (vvMap, thunk), fx = fx}
                           | Mil.RhsPFunctionMk _ => fail (fname ^ "RhsPFunctionMk ", "not handled")
                           | Mil.RhsPFunctionInit _ => fail (fname ^ "RhsPFunctionInit ", "not handled")
                           | Mil.RhsPFunctionGetFv _ => fail (fname ^ "RhsPFunctionGetFv ", "not handled")
                           | Mil.RhsPSetNew _ => fail (fname ^ "RhsPSetNew ", "not handled")
                           | Mil.RhsPSetGet _ => fail (fname ^ "RhsPSetGet ", "not handled")
                           | Mil.RhsPSetCond {bool, ofVal} => 
                             Mil.RhsPSetCond {bool = getMappedOpnd (vvMap, bool), ofVal = getMappedOpnd (vvMap, ofVal)}
                           | Mil.RhsPSetQuery opnd => Mil.RhsPSetQuery (getMappedOpnd (vvMap, opnd))
                           | Mil.RhsPSum _ => fail (fname ^ "RhsPSum ", "not handled")
                           | Mil.RhsPSumProj _ => fail (fname ^ "RhsSumProj ", "not handled")
            in nrhs
            end
      in
        (case i
          of SOME i => 
             let
               val () = (case IMil.IInstr.toInstruction(i)
                          of SOME (Mil.I {dests, n, rhs}) => 
                             let
                               val newdest = getDests dests
                               val nrhs = getRhs rhs
                               val nmi = MU.Instruction.new' (newdest, nrhs)
                               val nni = IMil.IBlock.append (p, nb, nmi)
                             in ()
                             end  
                           | NONE => ())
             in
               dupBlockInstrs (config, si, p, ifunc, r1Map, vvMap, nb, bl, IMil.IInstr.next (p, i))
             end
           | NONE => ())
      end

  (*
   * duplicate a block
   * record the block duplication in r1Map
   * record the variable duplication in vvMap
   *)
  fun dupBlock (config, si, p, ifunc, bl, r1Map, vvMap) =
      let
        fun dupBlockTransfer b =
            let
              val trans = IMil.IBlock.getTransfer' (p, b)
              val newTrans = case trans
                              of Mil.TInterProc {callee, ret, fx} =>
                                 let
                                   val newRet = (case ret
                                                  of Mil.RNormal {rets, block, cuts} => 
                                                     let
                                                       val newRets = Vector.map (rets, fn x => newVar (p, vvMap, x))
                                                     in Mil.RNormal {rets = newRets, block = block, cuts = cuts}
                                                     end
                                                   | Mil.RTail => ret)
                                 in Mil.TInterProc {callee = callee, ret = newRet, fx = fx}
                                 end
                               | _ => trans
            in newTrans
            end

        val b = IMil.IFunc.getBlockByLabel (p, ifunc, bl)
        val param = IMil.IBlock.getParameters (p, b)
        val nbl = IMil.Var.labelFresh p
        val nb = IMil.IBlock.build (p, 
                                    ifunc, 
                                    (nbl, Mil.B {parameters = Vector.map(param, fn x => newVar(p, vvMap, x)), 
                                                 instructions = Vector.new0(),
                                                 transfer = dupBlockTransfer b}))
        val () = dupBlockInstrs (config, si, p, ifunc, r1Map, vvMap, nb, bl, IMil.IBlock.getFirst (p, b))
        val () = r1Map := LD.insert (!r1Map, bl, nbl)
      in ()
      end

  (*
   * get duplicate block if it exists, otherwise return the original
   *)
  fun getR1' (x, r1Map) = (case LD.lookup (!r1Map, x)
                            of SOME y => y
                             | _ => x)


  (* 
   * if a block is duplicated, 
   * then it's transfer instruction should
   * 1. point to new duplicated block, if it's taget is duplicated too, excep the exit block
   * 2. replace the parameters if they are cloned
   *)
  fun dupTransfer (config, si, p, ifunc, bl, r1Map, vvMap, exit) =
      let
        val fname = "dupTransfer"
        val b = IMil.IFunc.getBlockByLabel (p, ifunc, bl)
        val transfer = IMil.IBlock.getTransfer' (p, b)
        (* if this block is duplicated exit block, then we dont change the target of transfer *)
        val isExit = MU.Compare.label (bl, getR1' (exit, r1Map)) = EQUAL
        fun getR1'' (x, r1Map) = if isExit then x
                                 else getR1' (x, r1Map)

        fun newCase (on, cases, default, tCase) =
            let
              fun doOneTarget (c, Mil.T {block, arguments}) 
                = (c, Mil.T {block = getR1'' (block, r1Map), 
                             arguments = Vector.map(arguments, fn x => getMappedOpnd(vvMap, x))})
              val newbranches = Vector.map(cases, doOneTarget)
              val newdefault =
                  case default
                   of SOME (Mil.T {block, arguments}) => 
                      SOME (Mil.T {block = getR1'' (block, r1Map), 
                                   arguments = Vector.map(arguments, fn x => getMappedOpnd(vvMap, x))})
                    | NONE => NONE
            in
              tCase {on = getMappedOpnd (vvMap, on), cases = newbranches, default = newdefault}
            end

        fun newInterProc (callee, ret, fx) =
            let
              val newCallee = 
                  let
                    fun newCode ({possible, exhaustive}) =
                        {possible = VS.fromList (List.map(VS.toList possible, fn x => getMappedVar (vvMap, x))),
                         exhaustive = exhaustive}
                  in
                    (case callee
                      of Mil.IpEval {typ, eval} =>
                         let
                           val newEval = (case eval 
                                           of Mil.EThunk {thunk, code} => 
                                              Mil.EThunk {thunk = getMappedVar(vvMap, thunk), code = newCode code}
                                            | Mil.EDirectThunk {thunk, code} => 
                                              Mil.EDirectThunk {thunk = getMappedVar (vvMap, thunk), code = getMappedVar (vvMap, code)})
                         in Mil.IpEval {typ = typ, eval = newEval}
                         end
                       | Mil.IpCall {call, args} => 
                         let
                           val newCall = (case call
                                           of Mil.CCode c => Mil.CCode (getMappedVar (vvMap, c))
                                            | Mil.CClosure {cls, code} => Mil.CClosure {cls = getMappedVar (vvMap, cls), 
                                                                                        code = newCode code}
                                            | Mil.CDirectClosure {cls, code} => Mil.CDirectClosure {cls = getMappedVar (vvMap, cls), 
                                                                                                    code = getMappedVar (vvMap, code)})
                         in Mil.IpCall {call = newCall, args = Vector.map (args, fn x => getMappedOpnd (vvMap, x))}
                         end)
                  end

              val newRet = (case ret
                             of Mil.RNormal {rets, block, cuts} => 
                                Mil.RNormal {rets = rets, block = getR1'' (block, r1Map), cuts = cuts}
                              | Mil.RTail => ret)

            in Mil.TInterProc {callee = newCallee, ret = newRet, fx = fx}
            end

        val nt =
            case transfer
             of Mil.TGoto (Mil.T {block, arguments}) => 
                Mil.TGoto (Mil.T {block = getR1'' (block, r1Map), 
                                  arguments = Vector.map(arguments, fn x => getMappedOpnd(vvMap, x))})
              | Mil.TCase {on, cases, default} => newCase (on, cases, default, Mil.TCase)
              | Mil.TReturn _ => transfer
              | Mil.TPSumCase {on, cases, default} => newCase (on, cases, default, Mil.TPSumCase)
              | Mil.TInterProc {callee, ret, fx} => newInterProc (callee, ret, fx)
              | _ => fail (fname, "unexpected transfer " ^ L.toString (ML.layoutTransfer (config, si, transfer)))
      in IMil.IBlock.replaceTransfer (p, b, nt)
      end

  (*
   * dup blocks on dominator tree
   *)
  fun dupR1' (config, si, p, ifunc, r1Map, r1, vvMap, header, exit) =
      let
        (* dup block only in r1 region *)
        fun dupR1Block (config, si, p, ifunc, bl, r1, r1Map, vvMap) =
            if isInRegion(bl, r1) then
              dupBlock (config, si, p, ifunc, bl, r1Map, vvMap)
            else ()
                 
        (* dup block tree in r1 region *)
        fun dupR1Blocks (config, si, p, ifunc, tree, r1, r1Map, vvMap) =
            let
              val Tree.T (parent, children) = tree
              val () = dupR1Block (config, si, p, ifunc, getLabel (p, parent), r1, r1Map, vvMap)
              val () = Vector.foreach (children, fn child => dupR1Blocks (config, si, p, ifunc, child, r1, r1Map, vvMap))
            in ()
            end

        val domTree = IMil.IFunc.getDomTree' (p, IMil.IFunc.getBlockByLabel (p, ifunc, header))
        val () = dupR1Blocks (config, si, p, ifunc, domTree, r1, r1Map, vvMap)
      in ()
      end
  (* 
   * duplicate all the blocks in R1 
   * also change the transfer targets to the duplicated blocks  
   *)
  fun dupR1 (config, si, p, ifunc, r1Map, r1, vvMap, header, exit) =
      let
        val () = dupR1' (config, si, p, ifunc, r1Map, r1, vvMap, header, exit)
        val r1' = LS.fromList (List.map (LS.toList r1, fn x => getR1'(x, r1Map)))
        val () = LS.foreach (r1', fn x => dupTransfer (config, si, p, ifunc, x, r1Map, vvMap, exit))
      in ()
      end

  (*
   * get var vector from opnd
   *)
  fun getVarFromOpnd (config, si, opnd) =
      case opnd
       of Mil.SVariable v => Vector.new1 v
        | _ => Vector.new0 ()

  (*
   * get var set from opnd vector
   *)
  fun getVarFromOpndVector (config, si, opnds) = 
      let
        (*
         * get var vector from opnd vector
         *)
        fun getVarFromOpnds (config, si, opnds) =
            let
              val opnds' = Vector.keepAll (opnds, fn x => case x 
                                                           of Mil.SVariable v => true
                                                            | _ => false)
              val vars = Vector.map (opnds', 
                                  fn x => case x
                                           of Mil.SVariable v => v
                                            | _ => fail ("getVarFromOpnds: ", L.toString (ML.layoutOperand (config, si, x))))
            in vars
            end
            
      in VS.fromVector (getVarFromOpnds (config, si, opnds))
      end

  (*
   * extract variable usage information from the transfer instruction
   *)
  fun getUsedVarsFromTransfer (config, si, p, ifunc, l) =
      let
        val b = IMil.IFunc.getBlockByLabel (p, ifunc, l)
        val fname = "getUsedVarsFromTransfer"
        val trans = IMil.IBlock.getTransfer' (p, b)
      in             
        case trans
         of Mil.TGoto (Mil.T {block, arguments}) => getVarFromOpndVector (config, si, arguments)
          | Mil.TCase {on, cases, default} =>
            let
              fun doOneTarget (c, Mil.T {block, arguments}) = getVarFromOpndVector (config, si, arguments)
              val vars1 = Vector.fold(cases, VS.empty, fn (tgt, vs) => VS.union(vs, doOneTarget tgt))
              val vars2 =
                  case default
                   of SOME (Mil.T {block, arguments}) => getVarFromOpndVector (config, si, arguments)
                    | NONE => VS.empty
            in
              VS.union (vars1, vars2)
            end
          | Mil.TReturn opnds => getVarFromOpndVector (config, si, opnds)
          | Mil.TInterProc {callee, ret, fx} => 
            let
              val vars = case callee 
                          of Mil.IpCall {call, args} => getVarFromOpndVector (config, si, args)
                           | Mil.IpEval {typ, eval} => VS.empty
            in vars
            end
          | Mil.TPSumCase {on, cases, default} =>
            let
              fun doOneTarget (c, Mil.T {block, arguments}) = getVarFromOpndVector (config, si, arguments)
              val vars1 = Vector.fold(cases, VS.empty, fn (tgt, vs) => VS.union(vs, doOneTarget tgt))
              val vars2 =
                  case default
                   of SOME (Mil.T {block, arguments}) => getVarFromOpndVector (config, si, arguments)
                    | NONE => VS.empty
            in VS.union (vars1, vars2)
            end
          | _ => fail (fname, "unexpected transfer " ^ L.toString (ML.layoutTransfer (config, si, trans)))
      end

  (*
   * get the used variables in the block
   *)
  fun getUsedVar (config, si, p, ifunc, l) =
      let
        val b = IMil.IFunc.getBlockByLabel (p, ifunc, l)
        val fname = "getUsedVar"

        fun getUsedVarFromInstr (rhs) =
            let
              val fname = "getUsedVarFromInstr "
              val vars = (case rhs
                           of Mil.RhsSimple s => fail(fname, L.toString (ML.layoutRhs (config, si, rhs)))
                            | Mil.RhsPrim {prim, createThunks, args} => 
                              let
                              in getVarFromOpndVector (config, si, args)
                              end
                            | Mil.RhsTuple {vtDesc, inits} => getVarFromOpndVector (config, si, inits)
                            | Mil.RhsTupleSub (ts as Mil.TF {tupDesc, tup, field}) => VS.fromVector(Vector.new1 tup)
                            | Mil.RhsTupleSet {tupField as Mil.TF {tupDesc, tup, field}, ofVal} => 
                              VS.fromVector(getVarFromOpnd (config, si, ofVal))
                            | Mil.RhsThunkValue {typ, thunk, ofVal} => VS.fromVector(getVarFromOpnd (config, si, ofVal))
                            | Mil.RhsThunkMk tm => VS.fromVector (Vector.new0 ())
                            | Mil.RhsThunkInit {typ, thunk, fx, code, fvs} =>
                              let
                                val v1 = case thunk 
                                          of SOME t => VS.fromVector (Vector.new1 t)
                                           | _ => VS.fromVector (Vector.new0 ())
                                val v2 = case code 
                                          of SOME c => VS.fromVector (Vector.new1 c)
                                           | _ => VS.fromVector (Vector.new0 ())
                                val v3 = getVarFromOpndVector (config, si, Vector.map (fvs, fn (x, y) => y))
                              in VS.union (v3, VS.union (v1, v2))
                              end
                            | Mil.RhsTupleInited _ => fail (fname ^ "RhsTupleInited ", "not handled")
                            | Mil.RhsIdxGet _ => fail (fname ^ "RhsIdxGet ", "not handled")
                            | Mil.RhsCont _ => fail (fname ^ "RhsCont ", "not handled")
                            | Mil.RhsObjectGetKind _ => fail (fname ^ "RhsObjectGetKind ", "not handled")
                            | Mil.RhsThunkGetFv _ => fail (fname ^ "RhsThunkGetFv ", "not handled")
                            | Mil.RhsThunkGetValue _ => fail (fname ^ "RhsThunkGetValue ", "not handled")
                            | Mil.RhsThunkSpawn {typ, thunk, fx} => VS.fromVector (Vector.new1 thunk)
                            | Mil.RhsPFunctionMk pfmk => VS.empty
                            | Mil.RhsPFunctionInit {cls, code, fvs} => 
                              let
                                val v1 = case cls of SOME clsv => VS.fromVector (Vector.new1 clsv) | _ => VS.empty
                                val v2 = case code of SOME codev => VS.fromVector (Vector.new1 codev) | _ => VS.empty
                                val v3 = getVarFromOpndVector (config, si, Vector.map (fvs, fn (x, y) => y))
                              in VS.union (v3, VS.union (v1, v2))
                              end
                            | Mil.RhsPFunctionGetFv {fvs, cls, idx} => VS.fromVector (Vector.new1 cls)
                            | Mil.RhsPSetNew _ => fail (fname ^ "RhsPSetNew ", "not handled")
                            | Mil.RhsPSetGet _ => fail (fname ^ "RhsPSetGet ", "not handled")
                            | Mil.RhsPSetCond {bool, ofVal} => 
                              let
                                val var1 = VS.fromVector (getVarFromOpnd (config, si, bool))
                                val var2 = VS.fromVector (getVarFromOpnd (config, si, ofVal))
                              in VS.union (var1, var2)
                              end 
                            | Mil.RhsPSetQuery opnd => VS.fromVector(getVarFromOpnd (config, si, opnd))
                            | Mil.RhsPSum {tag, typ, ofVal} => VS.fromVector(getVarFromOpnd (config, si, ofVal))
                            | Mil.RhsPSumProj {typ, sum, tag} => VS.fromVector(Vector.new1 sum))
            in vars
            end
                    
        (* itereate all instrs to get the used variables *)
        fun iterInstrs (io, vars) =
            (case io
              of SOME i => 
                 let
                   val nvars = case IMil.IInstr.toInstruction i
                                of SOME (Mil.I {dests, n, rhs}) => getUsedVarFromInstr (rhs)
                                 | NONE => VS.empty
                   val vars = VS.union (vars, nvars)
                 in
                   iterInstrs (IMil.IInstr.next (p, i), vars)
                 end  
               | NONE => vars)
            
        (* used vars from instructions *)
        val varsFromInsts = iterInstrs (IMil.IBlock.getFirst (p, b), VS.empty)
        (* used vars from transfer *)
        val varsFromTrans = getUsedVarsFromTransfer (config, si, p, ifunc, l)
        val vars = VS.union (varsFromInsts, varsFromTrans)
      in vars
      end

  (*
   * replace transfer instruction's parameter
   * if the parameters are cloned
   * return the new transfer instruction
   *)
  fun replaceTransferParam (config, si, p, ifunc, b, trans, v, newv) =
      let
        val fname = "replaceTransferParam"
        val l = #1 (IMil.IBlock.getLabel' (p, b))
        fun newArgs (args) = Vector.map (args, fn x => (case x 
                                                         of Mil.SVariable xv => if MU.Compare.variable (xv, v) = EQUAL 
                                                                                then Mil.SVariable newv 
                                                                                else x
                                                          | _ => x))

        fun newCase (on, cases, default, tCase) = 
            let
              fun newTarget (Mil.T {block, arguments}) = Mil.T {block = block, arguments = newArgs arguments}
                                                         
              val newBranches = Vector.map(cases, fn (c, tgt) => (c, newTarget tgt))
              val newDefault =
                  case default
                   of SOME tgt => SOME (newTarget tgt)
                    | NONE => NONE
            in
              tCase {on = on, cases = newBranches, default = newDefault}
            end

        val nt = case trans
                  of Mil.TGoto (Mil.T {block, arguments}) =>
                     let
                     in Mil.TGoto (Mil.T {block = block, arguments = newArgs arguments})
                     end
                   | Mil.TCase {on, cases, default} =>  newCase (on, cases, default, Mil.TCase)
                   | Mil.TInterProc {callee, ret, fx} => 
                     let
                       val newCallee = case callee 
                                        of Mil.IpCall {call, args} =>
                                           let
                                           in Mil.IpCall {call = call, args = newArgs args}
                                           end
                                         | Mil.IpEval {typ, eval} => Mil.IpEval {typ = typ,
                                                                                 eval = eval}
                     in Mil.TInterProc {callee = newCallee, ret = ret, fx = fx}
                     end
                   | Mil.TReturn opnds => Mil.TReturn (newArgs opnds)
                   | Mil.TCut {cont, args, cuts} => Mil.TCut {cont = cont, args = newArgs args, cuts = cuts}
                   | Mil.TPSumCase {on, cases, default} => newCase (on, cases, default, Mil.TPSumCase)

      in nt
      end

  (*
   * replace a block's transfer instruction's parameter
   *)
  fun replaceBlockTransferParam (config, si, p, ifunc, b, v, newv) =
      let
        val fname = "replaceBlockTransferParam"
        val trans = IMil.IBlock.getTransfer' (p, b)
        val nt = replaceTransferParam (config, si, p, ifunc, b, trans, v, newv)
      in IMil.IBlock.replaceTransfer (p, b, nt)
      end

  (* change var in block to new var *)
  fun replaceVar (config, si, p, ifunc, v, newv, l) =
      let
        val b = IMil.IFunc.getBlockByLabel (p, ifunc, l)

        (* if opnd contains oldv, then return newv*)
        fun getNewOpnd (opnd, oldv, newv) =
              (case opnd 
                of Mil.SVariable v => if MU.Compare.variable (v, oldv) = EQUAL then Mil.SVariable newv
                                      else opnd
                 | _ => opnd)

        fun getNewVar (v, oldv, newv) =
            if MU.Compare.variable (v, oldv) = EQUAL then newv
            else v

        fun replaceVarInInstr (rhs) =
            let
              val fname = "replaceVar.replaceVarInInstr."
              val nrhs = case rhs 
                          of Mil.RhsSimple s => fail(fname ^ "RhsSimple ", "not handled")
                           | Mil.RhsPrim {prim, createThunks, args} =>
                             let
                               val nrhs = Mil.RhsPrim {prim = prim, 
                                                       createThunks = createThunks, 
                                                       args = Vector.map(args, fn x => getNewOpnd (x, v, newv))} 
                             in nrhs
                             end
                           | Mil.RhsTuple {vtDesc, inits} => 
                             let
                               val newInits = Vector.map(inits, fn x => getNewOpnd (x, v, newv))
                             in Mil.RhsTuple {vtDesc = vtDesc, inits = newInits}
                             end
                           | Mil.RhsTupleSub (ts as Mil.TF {tupDesc, tup, field}) => 
                             let
                               val newTup = getNewVar (tup, v, newv)
                             in Mil.RhsTupleSub (Mil.TF {tupDesc = tupDesc,
                                                         tup = newTup,
                                                         field = field})
                             end
                           | Mil.RhsTupleSet {tupField as Mil.TF {tupDesc, tup, field}, ofVal} => 
                             let
                               val newTup = getNewVar (tup, v, newv)
                               val newField = case field
                                               of Mil.FiFixed _ => field
                                                | Mil.FiVariable opnd => Mil.FiVariable (getNewOpnd (opnd, v, newv))
                                                | _ => fail (fname ^ "RhsTupleSet ", "not handled")
                               val newTupField = Mil.TF {tupDesc = tupDesc, tup = newTup, field = newField}
                               val newOfVal = getNewOpnd (ofVal, v, newv)
                               val nrhs = Mil.RhsTupleSet {tupField = newTupField, ofVal = newOfVal}
                             in nrhs
                             end
                           | Mil.RhsTupleInited _ => fail (fname ^ "RhsTupleInited ", "not handled")
                           | Mil.RhsIdxGet _ => fail (fname ^ "RhsIdxGet ", "not handled")
                           | Mil.RhsCont _ => fail (fname ^ "RhsCont ", "not handled")
                           | Mil.RhsObjectGetKind _ => fail (fname ^ "RhsObjectGetKind ", "not handled")
                           | Mil.RhsThunkMk tm => Mil.RhsThunkMk tm
                           | Mil.RhsThunkInit {typ, thunk, fx, code, fvs} => 
                             let
                               val newThunk = case thunk 
                                               of SOME realThunk => SOME (getNewVar (realThunk, v, newv) )
                                                | NONE => thunk
                               val newCode = case code
                                              of SOME realCode => SOME (getNewVar (realCode, v, newv))
                                               | _ => code
                               val newFvs = Vector.map (fvs, fn (x, y) => (x, getNewOpnd (y, v, newv)))
                             in Mil.RhsThunkInit {typ = typ, thunk = newThunk, fx = fx, code = newCode, fvs = newFvs}
                             end
                           | Mil.RhsThunkGetFv _ => fail (fname ^ "RhsThunkGetFv ", "not handled")
                           | Mil.RhsThunkValue {typ, thunk, ofVal} => 
                             let
                               val nrhs = Mil.RhsThunkValue {typ = typ,
                                                             thunk = (case thunk of SOME thunkv => SOME (getNewVar (thunkv, v, newv)) | _ => thunk),
                                                             ofVal = getNewOpnd(ofVal, v, newv)}
                             in nrhs
                             end
                           | Mil.RhsThunkGetValue _ => fail (fname ^ "RhsThunkGetValue ", "not handled")
                           | Mil.RhsThunkSpawn {typ, thunk, fx} => 
                             Mil.RhsThunkSpawn { typ = typ, thunk = getNewVar (thunk, v, newv), fx = fx}
                           | Mil.RhsPFunctionMk {fvs} => Mil.RhsPFunctionMk {fvs = fvs}
                           | Mil.RhsPFunctionInit {cls, code, fvs} => 
                             let
                               val newCls = (case cls of SOME clsv => SOME (getNewVar (clsv, v, newv)) | _ => cls)
                               val newCode = (case code of SOME codev => SOME (getNewVar (codev, v, newv)) | _ => code)
                               val newFvs = Vector.map (fvs, fn (fk, opnd) => (fk, getNewOpnd (opnd, v, newv)))
                             in Mil.RhsPFunctionInit {cls = newCls, code = newCode, fvs = newFvs}
                             end
                           | Mil.RhsPFunctionGetFv {fvs, cls, idx} => 
                             let
                               val newCls = getNewVar (cls, v, newv)
                             in Mil.RhsPFunctionGetFv {fvs = fvs, cls = newCls, idx = idx}
                             end
                           | Mil.RhsPSetNew _ => fail (fname ^ "RhsPSetNew ", "not handled")
                           | Mil.RhsPSetGet _ => fail (fname ^ "RhsPSetGet ", "not handled")
                           | Mil.RhsPSetCond {bool, ofVal} => 
                             let
                               val newBool = getNewOpnd (bool, v, newv)
                               val newOfVal = getNewOpnd (ofVal, v, newv)
                             in Mil.RhsPSetCond {bool = newBool, ofVal = newOfVal}
                             end
                           | Mil.RhsPSetQuery opnd => Mil.RhsPSetQuery (getNewOpnd (opnd, v, newv))
                           | Mil.RhsPSum {tag, typ, ofVal} => 
                             let
                               val newOfVal = getNewOpnd (ofVal, v, newv)
                             in Mil.RhsPSum {tag = tag, typ = typ, ofVal = newOfVal}
                             end
                           | Mil.RhsPSumProj {typ, sum, tag} =>
                             let
                             in
                               Mil.RhsPSumProj {typ = typ,
                                                sum = getNewVar (sum, v, newv),
                                                tag = tag}
                             end
                             
            in nrhs
            end
        (* itereate all instrs to replace the used variables *)
        fun iterInstrs (io, v, newv) =
            (case io
              of SOME i => 
                 let
                   val fname = "replaceVar.interInstrs"
                   val () = case IMil.IInstr.toInstruction (i) 
                             of SOME (Mil.I {dests, n, rhs}) =>
                                 let
                                   val nrhs = replaceVarInInstr (rhs)
                                   val nmi = MU.Instruction.new' (dests, nrhs)
                                   val nni = IMil.IInstr.replaceInstruction (p, i, nmi)
                                 in ()
                                 end
                              | _ => ()
                 in
                   iterInstrs (IMil.IInstr.next (p, i), v, newv)
                 end  
               | NONE => ())

        val () = iterInstrs (IMil.IBlock.getFirst(p, b), v, newv)
        val () = replaceBlockTransferParam (config, si, p, ifunc, b, v, newv)
      in ()
      end

  (*
   * A()
   *  x = ...
   *  y = call f(3) ->  B
   *
   * B()
   *  ...
   *
   *
   * A()
   *  x = ...
   *  y = call f(3) ->  A’
   *
   * A’()
   *  goto B(x, y)
   *
   * B(x1, y1)
   *
   *)
  fun addCallLandingPad (config, si, p, ifunc, al, bl, var) =
      let
        val fname = "addCallLandingPad"
        (* create A' *)
        val atrans' = Mil.TGoto (Mil.T {block = bl, arguments = Vector.new1 (Mil.SVariable var)})
        val al' = IMil.Var.labelFresh p
        val a' = IMil.IBlock.build (p, 
                                    ifunc,
                                    (al', Mil.B {parameters = Vector.new0(), 
                                                 instructions = Vector.new0(),
                                                 transfer = atrans'}))
        (* point A to A' *)
        val a = IMil.IFunc.getBlockByLabel (p, ifunc, al)
        val atrans = IMil.IBlock.getTransfer' (p, a)
        val newAtrans = (case atrans
                          of Mil.TInterProc {callee, ret, fx} => 
                             let
                               val newRet = (case ret
                                              of Mil.RNormal {rets, block, cuts} => 
                                                 Mil.RNormal {rets = rets, block = al', cuts = cuts}
                                               | Mil.RTail => ret)
                             in Mil.TInterProc {callee = callee, ret = newRet, fx = fx}
                             end
                           | _ => fail (fname, "must be call transfer"))
      in newAtrans
      end

  (*
   * after adding a parameter to a block
   * we need to add a parameter to it's pred block's transfer instruction
   *)
  fun paramPredTrans (config, si, p, ifunc, pred, b, v) =
      let
        val fname = "paramPredTrans"
        val l = #1 (IMil.IBlock.getLabel' (p, b))
        val predl = #1 (IMil.IBlock.getLabel' (p, pred))
        val predt = IMil.IBlock.getTransfer' (p, pred)

        fun newTCase (on, cases, default) =
            let
              fun newTarget (Mil.T {block, arguments}) =
                  if (MU.Compare.label (block, l) = EQUAL) 
                  then Mil.T {block = block, arguments = appendOpndVector (arguments, v)}
                  else Mil.T {block = block, arguments = arguments}

              val newbranches = Vector.map(cases, fn (c, tgt) => (c, newTarget tgt))
              val newdefault =
                  case default
                   of SOME tgt => SOME (newTarget tgt)
                    | NONE => NONE
            in
              Mil.TCase {on = on, cases = newbranches, default = newdefault}
            end

        val nt = case predt
                  of Mil.TGoto (Mil.T {block, arguments}) =>
                     Mil.TGoto (Mil.T {block = block, arguments = appendOpndVector (arguments, v)})
                   | Mil.TCase {on, cases, default} => newTCase (on, cases, default)
                   | Mil.TInterProc {callee, ret, fx} => addCallLandingPad (config, si, p, ifunc, predl, l, v)
                   | Mil.TReturn opnd => fail (fname, "TReturn")
                   | Mil.TCut {cont, args, cuts} => fail (fname, "TCut")
                   | Mil.TPSumCase ns => fail (fname, "TPSumCase")
      in IMil.IBlock.replaceTransfer (p, pred, nt)
      end

  (* find those edges out r1 region *) (* fixme too heavy *)
  fun findR1OutEdges (config, si, p, ifunc, r1) =
      let
        val fname = "findR1OutEdges"
        fun findBlockOutEdges (l, r1) =
            let
              val b = IMil.IFunc.getBlockByLabel (p, ifunc, l)
              val outEdges = IMil.IBlock.outEdges (p, b)
              val outEdges' = List.map (outEdges, fn (bx, by) => (getLabel(p, bx), getLabel(p, by)))
            in List.keepAll (outEdges', fn (x, y) => notR1(y, r1))
            end
        val oes = LS.fold (r1, [], fn (l, es) => findBlockOutEdges(l, r1)@es)
        val () = Debug.printLayout (config, L.seq [L.str fname,
                                                   List.layout (fn x => Debug.layoutEdge(config, si, x)) oes])
      in oes
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
                  let
                    val fname = "findDefVarInstrs"
                  in
                    (case io
                      of SOME i => 
                         let
                           val nvars = case IMil.IInstr.toInstruction(i)
                                        of SOME (Mil.I {dests, n, rhs}) => VS.fromVector dests
                                         | NONE => VS.empty
                         in
                           findDefVarInstrs (IMil.IInstr.next (p, i), VS.union (vars, nvars))
                         end  
                       | NONE => vars)
                  end

              (* find defined vars in transfer instruction *)
              fun findDefVarTransfer (l, b) =
                  let
                    val fname = "findDefVarTransfer "
                    val transfer = IMil.IBlock.getTransfer' (p, b)
                    val vars = case transfer
                                of Mil.TInterProc {callee, ret, fx} =>
                                   (case ret
                                     of Mil.RNormal {rets, block, cuts} => VS.fromVector rets
                                      | _ => VS.empty)
                                 | _ => VS.empty
                  in vars
                  end

              (* find defined vars in block *)
              fun findDefinedVarBlock l =
                  let
                    val fname = "findDefinedVarBlock"
                    val b = IMil.IFunc.getBlockByLabel (p, ifunc, l)
                    (* take parameters as defined variables *)
                    val param = IMil.IBlock.getParameters (p, b)
                    val pvars = VS.fromVector (param)
                    (* get defined vars from instruction *)
                    val ivars = findDefVarInstrs (IMil.IBlock.getFirst (p, b), VS.empty)
                    (* get defined vars from transfer *)
                    val tvars = findDefVarTransfer (l, b)
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
              val rest = List.map (rest, fn x => #1 (IMil.IBlock.getLabel' (p, x)))
            in rest
            end

        val rest = getR1Rest (config, si, p, ifunc, r1)
        val r1Def = findDefinedVar (config, si, p, ifunc, r1)         (* defined variables in r1 blocks*)
        val restUse = List.fold (rest, VS.empty, fn (l, vs) => VS.union (getUsedVar(config, si, p, ifunc, l), vs))
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
   * get all blocks, which do not belong to the loop
   *)
  fun getNonLoopBlocks (config, si, p, ifunc, loop as MilLoop.L {header, blocks, ...}) =
      let
        val allLabels = List.map (IMil.IFunc.getBlocks (p, ifunc), fn b => getLabel (p, b))
        val loopBlocks = List.map (LD.toList blocks, fn (l, _) => l)
      in LS.fromList (List.removeAll (allLabels, fn x => List.exists(loopBlocks, fn y => x = y)))
      end

  (*
   * outEdges are those edges out of the region
   * vars are those variable live out of the region
   * 1. add the vars to out edges' parameter
   * 2. clone the vars in the out region, and replace the variable use in the region respectively
   * 3. change the preds's transfer if the block's parameter is changed.
   *)
  fun paramOutEdges (config, si, p, ifunc, outEdges, vars, vvMap, r1, r2, loop as MilLoop.L {header, blocks, ...}) =
      let
        val nonLoopBlocks = getNonLoopBlocks (config, si, p, ifunc, loop)

        (*
         * add a new variable to transfer instruction's parameter
         * if it's not there
         *)
        fun paramOutEdgeTrans (predl, bl, v) =
            let
              val fname = "paramOutEdgeTrans"
              val b = IMil.IFunc.getBlockByLabel (p, ifunc, bl)
              val pred = IMil.IFunc.getBlockByLabel (p, ifunc, predl)
              val predt = IMil.IBlock.getTransfer' (p, pred)
                          
              fun newCase (on, cases, default, tCase) =
                  let
                    fun newTarget (Mil.T {block, arguments}) =
                        if (MU.Compare.label (block, bl) = EQUAL) 
                        then 
                          let
                          in Mil.T {block = block, arguments = appendOpndVector (arguments, v)}
                          end
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
         * 
         *)
        fun paramPredsTrans (el, v, newv) =
            let
              val eb = IMil.IFunc.getBlockByLabel (p, ifunc, el)
              val preds = IMil.IBlock.preds (p, eb)

              fun paramPredTrans' (pred) =
                  let
                    val predl = getLabel (p, pred)
                  in
                    if notR1 (predl, r1)
                    then paramPredTrans (config, si, p, ifunc, pred, eb, v) 
                    else ()                  
                  end

            in List.foreach (preds, fn pred => paramPredTrans' pred)
            end

        (*
         * add var as parameter to a block
         *)
        fun paramVarBlock (v, sl, el) =
            let
              val eb = IMil.IFunc.getBlockByLabel (p, ifunc, el)
              val param = IMil.IBlock.getParameters (p, eb)
              val newv = newVar (p, vvMap, v)
              val newparam = appendVarVector (param, newv)
              val () = IMil.IBlock.replaceLabel (p, eb, (el, newparam))
              val () = paramOutEdgeTrans (sl, el, v)
              val () = paramPredsTrans (el, v, newv)
              val () = if (isInRegion (el, r2)) 
                       then LS.foreach (r2, fn l => replaceVar (config, si, p, ifunc, v, newv, l))
                       else LS.foreach (nonLoopBlocks, fn l => replaceVar (config, si, p, ifunc, v, newv, l))
            in ()
            end

        val () = List.foreach (outEdges, fn (sl, el) => VS.foreach (vars, fn v => paramVarBlock (v, sl, el)))
      in ()
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
          (*
           * vvMap is used to record cloned variable
           * the cloned variable will be used to replace the orignal variable in r2 and non loop blocks
           *)
          val vvMap = ref VD.empty
          val HO' = getH' (config, si, p, ifunc, miloop, loop)
        in (case HO' 
             of SOME H' => 
                let
                  val vars = findLiveOutVar (config, si, p, ifunc, r1)
                  val outEdges = findR1OutEdges (config, si, p, ifunc, r1)
                  val () = paramOutEdges (config, si, p, ifunc, outEdges, vars, vvMap, r1, r2, loop)
                  val () = dupR1 (config, si, p, ifunc, r1Map, r1, vvMap, header, exit)
                  val () = linkToNewLoopHeader (config, si, p, ifunc, miloop, loop, r1Map)
                  val () = PD.click (d, passname)
                in          
                  Debug.printLayout (config, Debug.layoutConvertLoop (config, si, H', r1Map))
                end
              | _ => ())
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
  (* rename function *)
  fun tryConvertWhileLoop (d, config, si, m, p, ifunc, miloop, loop as MilLoop.L {header, blocks, ...}, children) =
      let
        fun hasCutTransfer (config, si, p, func, r (*region*)) =
            let
              fun isCutTransfer (p, func, l) =
                  let
                    val b = IMil.IFunc.getBlockByLabel (p, func, l)
                  in case IMil.IBlock.getTransfer' (p, b)
                      of Mil.TCut _ => true
                       | _ => false
                  end
            in LS.fold (r, false, fn (l, ret) => (ret orelse (isCutTransfer (p, func, l))))
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
