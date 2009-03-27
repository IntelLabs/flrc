(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, March 2008 *)

signature MIL_LICM = 
sig
  val pass : (BothMil.t, BothMil.t) Pass.t
end

structure MilLicm :> MIL_LICM =
struct

  val passname = "MilLicm"

  val stats = []

  structure L = Layout
  structure LU = LayoutUtils
  structure I = Identifier
  structure IM = Identifier.Manager
  structure VD = Identifier.VariableDict
  structure IVD = Identifier.ImpVariableDict
  structure VS = Identifier.VariableSet
  structure DG = Identifier.VariableDirectedGraph
  structure LD = Identifier.LabelDict
  structure LS = Identifier.LabelSet
  structure M = Mil
  structure PD = PassData
  structure MOU = MilOptUtils
  structure GD = MilCmp.GlobalDict
  structure ED = MilCmp.EvalDict

  type envT = 
       {data    : PD.t}

  datatype env = E  of envT

  datatype state = S of {stM   : M.symbolTableManager}

  fun envGet sel (E t) = sel t 

  val envGetData      = envGet #data
                        

  fun stateGet sel (S t) = sel t
  val stateGetStm       = stateGet #stM

  fun getConfig env = PD.getConfig (envGetData env)

  structure Chat = ChatF(struct 
                         type env = env
                         val extract = getConfig
                         val name = passname
                         val indent = 2
                         end)

  structure FV = MilFreeVarsF(type env = env
  val config = getConfig
  val indent = 2
                             )

  structure RN = MilRenameF(type env = env
  val config = getConfig
  val indent = 2
                           )

  structure RNL = RN.Label

  structure MilCFG = MilCfgF(type env = env
  val getConfig = getConfig
  val passname = passname
  val indent = 2)

  structure CFG = MilCFG.Cfg
  structure DomInfo = MilCFG.LabelDominance
  structure CFGL = MilCFG.Loop

  val (debugPassD, debugPass) =
      Config.Debug.mk (passname,
                       "debug the Mil loop-invariant code motion pass")

  fun debugDo (env, f) = 
      if Config.debug andalso debugPass (getConfig env)
      then f()
      else ()

  (* note - replicated from cfg.sml *)
  fun getLabelL blks = LD.fold (blks, [], fn (l, _, ls) => l::ls)
      
  fun getBlockL blks = LD.fold (blks, [], fn (_, b, bs) => b::bs)
              
  (* debug *)
  fun printVS (banner, s:VS.t ) =
      let
        val() = print ( banner )
        val() = LU.printLayout(
                VS.layout(
                s,
                Identifier.layoutVariable') )
      in  ()
      end
      
  fun varDefinedInBlockS (env, 
                          M.B{parameters, instructions, transfer}:M.block)
      : VS.t = 
      let
        val res = Vector.keepAllMap(instructions,
                                    (fn(M.I {dest, rhs}) => dest))
                  
        val res = VS.fromVector res
        (* add 'parameters' to vars defined in block *)
        val res = Vector.fold(
                  parameters,
                  res,
                  (fn (p,a) => VS.insert(a,p)))
                  
        val () = debugDo 
                   (env, 
                    (fn () => 
                        let
                          val()=printVS("VadDefinedInBlockS", res)
                        in ()
                        end))
      in
        res
      end

  (* returns set of vars defined in ANY block in the loop         *)
  (* including inner loops                                        *)
  fun varDefinedInBlockListS (env, blocks : M.block list ) : VS.t = 
      let
        val res = List.fold(
                  blocks,
                  VS.empty,
                  (fn(b,s) => VS.union(
                              varDefinedInBlockS(env, b),
                              s)))
      in
        res
      end

  (* returns set of vars defined in ANY block in the loop     *)
  (* including inner loops                                    *)
  fun varDefinedInLoopTreeS (env, lt : CFGL.loop Tree.t) : VS.t = 
      let
        val help = 
            (fn (l, s) => 
                let
                  val CFGL.L {header, blocks, ...} = l
                  val resR =
                      varDefinedInBlockListS (env, getBlockL blocks)
                  val s = VS.union(s, resR)
                in s
                end)
        val s = Tree.foldPre (lt, VS.empty, help)
        val () = debugDo 
                   (env, 
                    (fn () => 
                        let
                          val()=printVS("VarDefinedInLopTreeS", s)
                        in ()
                        end))
      in s
      end


(*
  fun preheaderRenameDictTree (tr : CFGL.loop Tree.t)
      : M.label LD.t =
      (* compute rename dictionary - preHeaders associated with headers *)
        let   
          val help = 
              (fn (l, dict) => 
                  let
                    val CFGL.L {h
                                 preHeader, 
                                 preHeaderB, 
                                 blocks, 
                                 exits} = l
                    val dict = LD.insert( dict, header, preHeader )
                  in dict
                  end)
          val res = Tree.foldPre (tr, LD.empty, help)
        in res
        end

  fun retargetToPreheaderTree(env, 
                              tr : CFGL.LoopTree,
                              rename: M.label LD.t )
      : CFGL.LoopTree =
      (* redirect loop's predecessors to pre-header *)
        let   
          val Tree.T(root, children) = tr
          val CFGL.LUP{header, preHeader, preHeaderB, blocks, exits} = root
          val () = debugDo 
                     (env, 
                   fn () => 
                      let
                        val()=LU.printLayout(
                           L.seq[
                           L.str("Retargeting header "),
                           Identifier.layoutLabel( header ),
                           L.str("preHeader "),
                           Identifier.layoutLabel( preHeader )
                        ])
                      in ()
                      end)
          (* no redirection of blocks inside loop *)
          val rename = LD.remove(rename, header) 
                       
          fun retargetBlock( b : M.block, 
                             l : M.label, 
                             rename : M.label LD.t) : M.block =
              let

                val () = debugDo 
                           (env, 
                         fn () => 
                            let                    
                              val()=print("retargetBlock ")
                              val()=LU.printLayout(
                                 Identifier.layoutLabel( l) )
                            in ()
                            end)

                val M.B{parameters, instructions, transfer} = b
                val ninstructions = Vector.map(
                                    instructions,
                                    (fn(i) => RNL.instruction(env,
                                                              rename, i)))
                val ntransfer = RNL.transfer(env, rename, transfer)
              in
                M.B{parameters=parameters, 
                    instructions=ninstructions, 
                    transfer=ntransfer}
              end

          val nblocks    = List.map(
                           blocks,
                           (fn  (l,b) => (l, retargetBlock(b,l,rename))))
          val nroot     = CFGL.LUP{header     = header, 
                                   preHeader  = preHeader, 
                                   preHeaderB = preHeaderB, 
                                   blocks     = nblocks, 
                                   exits      = exits}
          val nchildren = retargetToPreheaderForest(env, children, rename)
        in
          Tree.T(nroot,nchildren)
        end

  and retargetToPreheaderForest(env, 
                                trf: CFGL.LoopForest, 
                                rename: M.label LD.t)
      : CFGL.LoopForest =
      let
        val res = Vector.map(trf, fn(c) => 
                                    retargetToPreheaderTree(env,
                                                            c, 
                                                            rename))
      in
        res
      end
*)
  (* fixme -replicated from cfg.sml *)
  (* finds  of blocks (nodes) in this dominator tree *)
  fun blocksInTree (tree : ((M.label * M.block) Tree.t)) : LS.t =
      let
        val help = 
            (fn ((bLabel, _), s) => LS.insert(s, bLabel))
        val s = Tree.foldPre (tree, LS.empty, help)
      in s
      end

  fun dominates (l: M.label, 
                 xs : LS.t, 
                 dominfo : MilCFG.LabelDominance.t,
                 iphs : M.label LD.t)
      : bool =
      let
        (* The preheaders are not in the dominator tree, so map them to the
         * corresponding header, iphs maps preheaders to headers (the inverse
         * of the header to preheader map)
         *)
        val l = Utils.optDefault (LD.lookup (iphs, l), l)
        fun checkOne l' = MilCFG.LabelDominance.dominates (dominfo, l, l')
        val chk = LS.forall (xs, checkOne)
      in chk
      end
(*      let

        val Tree.T( ((RootLabel, theBlock ):M.label * M.block),
                    Rchildren) = domtree
        val res =
            if RootLabel = l then
              LS.isSubset (xs,
                           blocksInTree(domtree))
            else
              Vector.exists(
              Rchildren,
              (fn( c ) => dominates(l, xs, c)))
      in
        res
      end*)
        
   fun TopoSortInstructions(env, invarList: M.instruction list)
       : M.instruction list =
       let
         val nodes = List.map(invarList,
                                     (fn (i as (M.I {dest, rhs})) => 
                                          case dest
                                            of SOME d => (d, i)
                                             | NONE   => Fail.fail(
                                                            "licm.sml",
                                                            "TopoSortInstructions",
                                                            "invalid Invar instructions")))
         val nodesS = VS.fromList(
                         List.map(nodes, #1))
         val freeList = List.map(nodes,
                                 fn(x, i) =>(x,VS.toList(
                                                  VS.intersection(
                                                     FV.instruction(env, i),
                                                     nodesS))))
         val freeDict = VD.fromList(freeList)
         fun successor( x:M.variable)
             : M.variable List.t =
               valOf(VD.lookup(freeDict, x))
         val graph = DG.fromList(
                        nodes,
                        successor)
         val components = DG.scc( graph )
         val invarListInTopoSortedOrder =
                List.map(
                   components,
                   fn c => case c
                             of [(_,i)] => i
                              |  _  => Fail.fail(
                                       "licm.sml",
                                       "TopoSortInstructions",
                                       "should not happen-recursive instruction"))
       in
         invarListInTopoSortedOrder
       end

(* LICM Algorithm:                                                            *)
(*    For each loop L (recursively visit Loop Forest , bottom-up )            *)
(*       1- build set S: All var defini in loop (NOTE: consider inner loops)  *)
(*       2- find loop's Invariant computations (Inv)                          *)
(*           Initially: Inv = empty                                           *)
(*                I(var, rhs) is added to INVARIANT if                        *)
(*                1- ALL free variables v in I's rhs are either               *)
(*                        a. CONSTANT , OR                                    *)
(*                        b. defined OUTSIDE loop i.e, NOT defined in the loop*)
(*                               i.e not member of set S),  AND               *)
(*                        c. have NO effects                                  *)
(*                2- if 'v' is defined in loop                                *)
(*                       a. it is defined by an Invariant instruction         *)
(*                         (SSA property guarantees single definition) OR     *)
(*                       b. it is a Block argument coming from an             *)
(*                            Invariant instruction and                       *)
(*                            that block has a single-predecessor             *)
(*        3 - revisit loop tree and                                           *)
(*              Move each instr I' in Inv to loop pre-header                  *)
(*              IN THE ORDER TRAVERSED iff I's block dominates loop's exits   *)

  fun licmTree (state, env, cfg, dominfo, pblks, lt, exitsMap, phs, iphs) =
      let   
        val Tree.T (root, children) = lt
        val CFGL.L {header, blocks, ...} = root
        val (nchildren, blocks) =
            licmForest (state, env, cfg, dominfo, blocks, children,
                        exitsMap, phs, iphs)

        (* find loop invariantes in this node,                                *)
        (* including those in the pre-header of children nodes                *)
        val preHeader =
            case LD.lookup (phs, header)
             of NONE => Fail.unimplemented ("MilLicm", "licmTree", "no preheader")
              | SOME phl => phl
        val preHeaderB = Option.valOf (LD.lookup (pblks, preHeader))
        val exits = Option.valOf (LD.lookup (exitsMap, header))

        val blockLabels = getLabelL blocks
        val blockBlocks = getBlockL blocks
(*        val childrenPreHeadersB = 
            Vector.toList(
            Vector.map(
            nchildren,
            (fn (Tree.T 
                   (CFGL.LUP{header, 
                             preHeader, 
                             preHeaderB, 
                             blocks, 
                             exits},
                    nchildren)) => preHeaderB)))*)


        fun doOne (l, b, blks) =
            if dominates (l, exits, dominfo, iphs) then (l, b)::blks else blks
        val blocksDominatingExits = LD.fold (blocks, [], doOne)
(*            List.keepAll(
            blocks,
            (fn( (l,b) ) => dominates(l,
                                      LS.fromList(exits),
                                      domtree)))*)


(*        val childrenDominatingExits =
            List.keepAll(
               childrenPreHeadersB,
            fn( M.B{parameters,
                    instructions,
                    transfer})
                 => (case transfer 
                       of M.TGoto (M.T{block, arguments}) => dominates(block,
                                                                       LS.fromList(exits),
                                                                       domtree)
                        |  _   => Fail.fail("licm.sml",
                                            "childrenDominatingExits",
                                            "bad transfer in pre-header")))*)

        val blocksToConsiderLICM:M.block list = 
            (*childrenDominatingExits @ getBlockL(blocksDominatingExits)*)
            List.map (blocksDominatingExits, #2)

        val vDefinedInLoopS = varDefinedInLoopTreeS(env, lt )

        val () = debugDo 
                   (env, 
                 fn () => 
                    let
                      val()=printVS("vDefinedInLoopS", vDefinedInLoopS)
                    in ()
                    end)


        fun findInvarInst (i : M.instruction,
                           accum : (VS.t * VS.t))
            : (VS.t * VS.t) =
            let
              val (s, Invar) = accum
              val M.I{dest, rhs} = i
              val rhsVarS = FV.instruction(env, i)
              val res = case dest
                         of SOME v => 
                            let
                              val usesFromLoop = VS.intersection(
                                                 rhsVarS,
                                                 vDefinedInLoopS)
                              val res  = if (VS.isSubset(usesFromLoop, Invar) 
                                             andalso MOU.canLICMInstr(i))
                                         then VS.insert(s, v)
                                         else s
                            in
                              res
                            end
                          | NONE   => s
            in
              (res, Invar)
            end

        fun processInvarBlock(b:M.block,
                              accum : (VS.t * VS.t))
            : (VS.t * VS.t) =
            let
              val M.B{parameters, instructions, transfer} = b
              val res = Vector.fold(
                        instructions,
                        accum,
                        findInvarInst)
            in
              res
            end

        fun calcInvariants( InvAccum : VS.t )
            : VS.t =
            let
              val (res,_) = List.fold(
                          blocksToConsiderLICM,
                          (VS.empty, InvAccum),
                          (fn( b:M.block,
                               s:(VS.t * VS.t) )
                                 => processInvarBlock(b,s) ))
              in
                res
              end

        fun InstructionsInBlock( b : M.block )
            : M.instruction List.t =
            let
              val M.B{parameters, instructions, transfer} = b
            in
              Vector.toList(instructions)
            end

        val InstructionsInLoop = 
            List.fold(
            blocksToConsiderLICM,
            [],
            (fn (b:M.block, res) => InstructionsInBlock(b) @ res  ))

        val () = debugDo 
                  (env, 
                  fn () => 
                    let
                      val data = envGetData env
                      val config = PD.getConfig data
                      val()=print("LICM: instructions in loop:")
                      val st= IM.finish(stateGetStm state)
                      val foo = List.map(
                                  InstructionsInLoop,
                                  fn(i) => LU.printLayout(
                                           MilLayout.layoutInstr(
                                           config, 
                                           IM.finish(stateGetStm state), i)))
                    in ()
                    end)

        fun calcLoopInvar( ):VS.t =
            let
              fun loop( inv ) =
                  let
                    val res = calcInvariants(inv) 
                    val res = if (VS.equal(res, inv))
                              then res
                              else loop ( res )
                  in
                    res
                  end

              val invloop = loop ( VS.empty )

              val () = debugDo 
                         (env, 
                       fn () => 
                          let                    
                            val ()=printVS("calcLoopInvar - invloop (final)",
                                           invloop)
                          in ()
                          end)
            in
              invloop
            end

        val LupInvar = calcLoopInvar()

        val () = debugDo 
                   (env, 
                 fn () => 
                    let                    
                      val ()=printVS("Loop invariants:", LupInvar)
                    in ()
                    end)

        fun isInvarInstruction(i:M.instruction):bool =                             
            let
              val M.I {dest, rhs} = i
            in
              case dest
               of  SOME d =>  VS.member(LupInvar, d)
                 |  NONE   =>  false
            end

        val invarList = List.removeAll(
                        InstructionsInLoop,
                     fn(i) => ( not( isInvarInstruction(i))))
        val invarList = TopoSortInstructions(env, invarList)

        val () = debugDo 
                   (env, 
                 fn () => 
                    let
                      val data = envGetData env
                      val config = PD.getConfig data
                      val()=print("LICM: inv instructions being moved:(")
                      val()=print(Int.toString(List.length invarList))
                      val()=print(")\n") 
                      val st= IM.finish(stateGetStm state)
                      val foo = List.map(
                                invarList,
                             fn(i) => LU.printLayout(
                                      MilLayout.layoutInstr(
                                      config, 
                                      IM.finish(stateGetStm state), i)))
                    in ()
                    end)

        (* add invariant instr to pre-header block *)  
        val M.B{ parameters, instructions, transfer} = preHeaderB
        val InvInstructions = Vector.fromList(
                              Vector.toList(instructions) @ invarList )
        val nPreHeaderB = M.B{ parameters   = parameters,
                               instructions = InvInstructions,
                               transfer     = transfer }
        val npblks = LD.insert (pblks, preHeader, nPreHeaderB)

        (* remove invariant instr from loop blocks *)
        fun removeInvarFromBlock(cfg, p) =
            let
              val (bl, b) = p
              val M.B{parameters, instructions, transfer} = b
              val ninstructions = Vector.keepAll(
                                  instructions,
                               fn(i) => (not(isInvarInstruction(i))))
              val newB = M.B{parameters   = parameters,
                             instructions = ninstructions,
                             transfer     = transfer}
            in newB
            end

        val nblocks =
            LD.map (blocks, fn lb => removeInvarFromBlock (cfg, lb))
(*
        fun removeInvarFromPreheader( lt: CFGL.LoopTree ) : CFGL.LoopTree =
            let
              val Tree.T(root, children) = lt
              val  CFGL.LUP{header, 
                            preHeader, 
                            preHeaderB, 
                            blocks, 
                            exits} = root
              val (_,newPreHeaderB) =  removeInvarFromBlock(cfg,
                                                            (preHeader,
                                                             preHeaderB))
             val newroot =                                                          
                          CFGL.LUP{header     = header, 
                                   preHeader  = preHeader, 
                                   preHeaderB = newPreHeaderB, 
                                   blocks     = blocks, 
                                   exits      = exits}
             in
              Tree.T(newroot, children)
            end
*)
        fun bl( (l,b):(M.label * M.block)):M.block = b
            

  (*      val nchildren =
               Vector.map(
                   nchildren,
                   (fn (Tree.T 
                          (CFGL.LUP{header, 
                                    preHeader, 
                                    preHeaderB, 
                                    blocks, 
                                    exits},
                           Cchildren)) => 
                                    Tree.T(CFGL.LUP{header=header, 
                                              preHeader=preHeader, 
                                              preHeaderB=bl(removeInvarFromBlock(cfg, (preHeader, preHeaderB))),
                                              blocks=blocks, 
                                              exits=exits},Cchildren)))*)


        val nroot = CFGL.L {header = header, blocks = nblocks}
      in
        (Tree.T (nroot, nchildren), npblks)
      end

  and licmForest (state, env, cfg, dominfo, pblks, trf, exitsMap, phs, iphs) =
      let
        fun doOne (c, pblks) =
            licmTree (state, env, cfg, dominfo, pblks, c, exitsMap, phs, iphs)
        val res = Vector.mapAndFold (trf, pblks, doOne)
      in res
      end

  fun licmLoopStructure (state, env, cfg, dominfo, ls, exits) =
      let
        val (ls, phs) = CFGL.mkPreheaders (stateGetStm state, env, ls)
        fun doOne (hl, phl, iphs) = LD.insert (iphs, phl, hl)
        val iphs = LD.fold (phs, LD.empty, doOne)
        val CFGL.LS {entry, loops, blocksNotInLoops = blks} = ls
        val (loops, blks) = licmForest (state, env, cfg, dominfo, blks, loops,
                                        exits, phs, iphs)
        val ls =
            CFGL.LS {entry = entry, loops = loops, blocksNotInLoops = blks}
      in ls
      end

(*  fun layoutLoops (state, env, fname, code, f) = 
      let
        val config = getConfig env
        val st = Identifier.Manager.finish (stateGetStm state)
        fun layoutLoopTree t = 
            let
              val layoutLB = 
               fn (bid, b) => MilLayout.layoutLabelBlock(config, st, bid, b)
              val layoutLoop = 
               fn (CFGL.LUP {header, blocks, exits, ...}) => 
                  let
                    val l = L.seq [L.str "Header is ", 
                                   Identifier.layoutLabel header]
                    val bs = List.map (blocks, layoutLB)
                    val le' = L.sequence ("{", "}", ",")
                                         (List.map (exits, I.layoutLabel))
                    val le = L.mayAlign [L.str "Exits are", le']
                    val l = L.align (l::bs @ [le])
                  in l
                  end
              val rec layoutT = 
               fn (Tree.T (l, children)) => 
                  let
                    val l = layoutLoop l
                    val ls = layoutLoopForest children
                    val l = L.align [l, L.str "Subloops are:", LU.indent ls]
                  in l
                  end
            in layoutT t
            end
        and layoutLoopForest f = L.align (Vector.toListMap (f, layoutLoopTree))

        val cl = MilLayout.layoutCode (config, st, code)
        val l = L.align [L.seq [L.str "Function ", 
                                MilLayout.layoutVar (config, st, fname)],
                         L.str "Code is:", 
                         LU.indent cl,
                         L.str "Loops are:", 
                         LU.indent (layoutLoopForest f)]
       in l
       end


  val printLoops = LU.printLayout o layoutLoops*)

  fun printFun (state, env, fname, code) =
      let
        val config = getConfig env
        val st = Identifier.Manager.finish (stateGetStm state)
        val cl = MilLayout.layoutCode (config, st, code)
        val l = L.align [L.seq [L.str "Function ", 
                                MilLayout.layoutVar (config, st, fname)],
                         L.str "Code is:", 
                         LU.indent cl]
      in LU.printLayout l
      end

  val (showLoopsD, showLoops) =
      Config.Debug.mk (passname ^ ":showLoops",
                       "show loops in loop-invariant code motion")

  fun doGlobals (state, env, stm, l) = 
   let
(*      fun foldLoopTree( tr : CFGL.LoopTree, dict : M.block LD.t)
          : M.block LD.t =
          let   
            val Tree.T(root, children) = tr
            val CFGL.LUP{header, preHeader, preHeaderB, blocks, exits} = root
            val dict = LD.insertAll( dict, blocks )
            val dict = LD.insert( dict, preHeader, preHeaderB )
            val res  = foldLoopForest( children, dict )
          in
            res
          end

      and foldLoopForest( trf : CFGL.LoopForest, dict : M.block LD.t)
          : M.block LD.t =
          Vector.fold(trf, 
                      dict,
                      foldLoopTree)*)



      (* process body of a global function *)
      fun processcode (
          env:env, 
          stm:M.symbolTableManager,
          fname : M.variable,
          code as M.F {effects, escapes, recursive, conv, args, rtyps, body}:M.code)
          : M.code =
          let
            val cfg = CFG.build body
            val lbdomtree = CFG.getLabelBlockDomTree cfg
            val ls = CFGL.build (env, cfg, lbdomtree)
            val ans = CFGL.allNodes (env, ls)
            val exits = CFGL.exits (env, ls, ans)
(*            val TopLoopsLT = CFGL.loops(env, stm, cfg, domtree)*)

            val () =
                if showLoops (getConfig env) then
                  let
                    (*val () = printLoops (state, env, fname, code, TopLoopsLT)
                    val ls = CFGL.build (env, cfg)*)
                    val () = printFun (state, env, fname, code)
                    val () = LU.printLayout (CFGL.layout ls)
                    fun doOne (l, ls) =
                        L.mayAlign [L.seq [I.layoutLabel l, L.str ":"],
                                    L.sequence ("{", "}", ",")
                                               (List.map (LS.toList ls,
                                                          I.layoutLabel))]
                    val le' = List.map (LD.toList exits, doOne)
                    val le = L.align [L.str "Exits:", LU.indent (L.align le')]
                    val () = LU.printLayout le
                    val ivs = CFGL.inductionVariables (env, l, cfg, ls, ans)
                    val cnf = getConfig env
                    val st = IM.finish stm
                    fun doIv (CFGL.IV {variable, init = (m,i,c), step, ...}) =
                        L.tuple [I.layoutVariable' variable,
                                 Rat.layout m,
                                 MilLayout.layoutOperand (cnf, st, i),
                                 Rat.layout c,
                                 Rat.layout step]
                    fun doOne (h, ivs) =
                        L.mayAlign [L.seq [I.layoutLabel h, L.str ":"],
                                    L.sequence ("{", "}", ",")
                                               (List.map (ivs, doIv))]
                    val livs' = List.map (LD.toList ivs, doOne)
                    val livs = L.align [L.str "Induction variables:",
                                        LU.indent (L.align livs')]
                    val () = LU.printLayout livs
                    val tcs = CFGL.tripCounts (env, cfg, lbdomtree, ls, ans,
                                               exits, ivs)
                    fun doOne (h, tc) =
                        let
                          val CFGL.TC {block, cond, flip1, comparison, flip2,
                                       init = (m, i, c), step, bound} = tc
                          
                          val li = L.seq [Rat.layout m,
                                          L.str "*",
                                          MilLayout.layoutOperand (cnf, st, i),
                                          L.str "+",
                                          Rat.layout c]
                          val l1 = L.seq [Rat.layout step, L.str "*",
                                          L.str "#", L.str "+", li]
                          val l2 = MilLayout.layoutOperand (cnf, st, bound)
                          val (l1, l2) = if flip2 then (l2, l1) else (l1, l2)
                          val l =
                              L.seq [L.str (Prims.stringOfCompare comparison),
                                     L.tuple [l1, l2]]
                          val l = if flip1 then L.seq [L.str "not ", l] else l
                          val l = L.mayAlign [L.seq [I.layoutLabel h,
                                                     L.str ":"],
                                              LU.indent l]
                        in l
                        end
                    val ltcs' = List.map (LD.toList tcs, doOne)
                    val ltcs = L.align [L.str "Trip counts:",
                                        LU.indent (L.align ltcs')]
                    val () = LU.printLayout ltcs
                  in ()
                  end
                else ()

            (* perform LICM *)
            val ldomtree = Tree.map (lbdomtree, fn (l, b) => l)
            val dominfo = MilCFG.LabelDominance.new ldomtree
            val ls = licmLoopStructure (state, env, cfg, dominfo, ls, exits)
(*
            val MovedInvar = licmForest (state, env, cfg, domtree, TopLoopsLT)

            (* create 'Root' of loop tree *)
            val Tree.T( ((rootLabel, rootBlock ):M.label * M.block), 
                          rootChildren) = DomTree.getTree domtree

            fun allBlocksInDominatorForest( d:(M.label * M.block) Tree.t vector, 
                                            accum:LS.t)
                : LS.t =
                Vector.fold(d, accum, allBlocksInDominatorTree)

            and allBlocksInDominatorTree( d:(M.label * M.block) Tree.t,
                                          accum:LS.t )
                : LS.t =
                let
                  val Tree.T( ((Blabel, theBlock ):M.label * M.block),
                              children) = d
                  val res = LS.insert( allBlocksInDominatorForest(children,accum),
                                       Blabel)
                in
                  res
                end

            (* blocks dominated by entry not in any loop *)
            val topBlocksL = LS.toList(
                               LS.difference( 
                                            allBlocksInDominatorForest(
                                                rootChildren,
                                                LS.empty),
                                            CFGL.allBlocksInSomeLoopForest(
                                                MovedInvar,
                                                LS.empty)))
            val topBlocks = List.map(
                                topBlocksL,
                                fn(l) => (l, CFG.labelGetBlock(cfg,l)))


            val () = debugDo 
                       (env, 
                     fn () => 
                        let
                          val()=print("topBlocksL\n")
                          val foo=List.map(
                              topBlocksL,
                           fn(l) => LU.printLayout(
                                       Identifier.layoutLabel(l)))
                        in ()
                        end)

            val preHeadersOfTopLoops = 
                        Vector.toList(Vector.map(
                                MovedInvar,
                                fn (l) => let
                                            val Tree.T(root, children) = l
                                            val CFGL.LUP{header,
                                                         preHeader,
                                                         preHeaderB,
                                                         blocks,
                                                         exits} = root
                                          in
                                            (preHeader, preHeaderB)
                                          end))
            val pHlabel = IM.labelFresh(stm)
            val phTarget = M.T{block = rootLabel, arguments = Vector.new0()}
            val pHtransfer = M.TGoto phTarget
            val EmptyBlock = M.B{ parameters   = Vector.new0(), 
                                  instructions = Vector.new0(),
                                  transfer     = pHtransfer   }

            (* implicit 'top' loop *)
            val RootLoopT : CFGL.loop  = 
                  CFGL.LUP{ 
                      header = rootLabel,  
                      preHeader = pHlabel, 
                      preHeaderB = EmptyBlock,
                      blocks = [(rootLabel, rootBlock),(pHlabel, EmptyBlock)]
                               @ topBlocks
                               @ preHeadersOfTopLoops,
                      exits = [] }

            val TheLoopTree   : CFGL.LoopTree = Tree.T (RootLoopT, MovedInvar )
            val preHeaderDict : M.label LD.t = 
                preheaderRenameDictTree TheLoopTree

            (* rename all transfers to loop preheaders *)
            val linkedPreHeaderT : CFGL.LoopTree = retargetToPreheaderTree(
                                                      env,
                                                      TheLoopTree,
                                                      preHeaderDict ) 

            (* recreate dictionary *)
            val dict2: M.block LD.t = foldLoopTree( linkedPreHeaderT, LD.empty )
            val body = M.CFG(pHlabel ,dict2)
*)
            val body = CFGL.unbuild (env, ls)
            val res = M.F{effects   = effects, 
                          escapes   = escapes, 
                          recursive = recursive,
                          conv      = conv,
                          args      = args, 
                          rtyps     = rtyps, 
                          body      = body}
          in
            res
          end

      fun LICMglobal (env:env, stm, v:M.variable, g:M.global)
          : (M.variable * M.global) = 
          (case g
            of M.GCode code =>  (*  LICM of code  *)
                                  let
                                    val ncode = processcode(env, stm, v, code )
                                    val res2 = M.GCode(ncode)
                                  in (v, res2)
                                  end
             | foo =>  (* global *)
                       (v, g))

      fun processGlobal (env, v, g) = 
          env

      fun doComponent (v, g) =
          let
            val (vr,gr) = LICMglobal (env, stm,  v, g)
          in gr
          end
      val gs = VD.map (l, doComponent)
    in gs
    end

fun globals (state, env, stm, gs) = 
    let
      val gs = doGlobals (state, env, stm, gs)
    in gs
    end

fun rewrite (d, mil) = 
    let
      val M.P {globals = gs, symbolTable, entry} = mil

      val stM = IM.fromExistingAll symbolTable

      val env = E {data   = d}

      val state = S { stM = stM }

      val gs = globals (state, env, stM, gs)

      val st = IM.finish stM

      val mil = M.P {globals = gs, symbolTable = st, entry = entry}

      val () = debugDo
         (env,
          fn () =>
             let
               val()=print(">printing FINAL mil\n")
               val()=LU.printLayout(
                  MilLayout.layout((getConfig env),mil))
               val()=print("<printing FINAL mil\n")
             in
               ()
             end)

    in mil
    end

fun program (mil, d) = 
    let
      val mil = rewrite (d, mil)
      val () = PD.report (d, passname)
    in mil
    end

val description = {name        = passname,
                   description = "Loop-invariant code motion",
                   inIr        = BothMil.irHelpers,
                   outIr       = BothMil.irHelpers,
                   mustBeAfter = [],
                   stats       = stats}

val associates = {controls  = [],
                  debugs    = [debugPassD, showLoopsD],
                  features  = [],
                  subPasses = []}

val pass = Pass.mkOptPass (description, associates, BothMil.mkMilPass program)

end
