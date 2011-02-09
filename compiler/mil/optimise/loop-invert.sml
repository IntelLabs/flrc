(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, January 2009 *)

(*
 * loop inversion, transform while-do loop to do-while loop.
 *
 * The control flow graph of a while loop looks like this,
 * where R1, R2, RC are code regions.
 *
 *         |
 *         v
 *     --> R1
 *     |   | \
 *     |   |  \
 *     |   v   v
 *     ---R2   RC

 * The goal is to duplicate R1 to R1', and re-wire the program
 * as follows:
 *
 *         |
 *         v
 *         R1-
 *         |  \
 *         |   \
 *         v    v
 *     --->R2   RC
 *     |   |    ^
 *     |   |   /
 *     ---R1'-/
 *
 * The advantage is that RC can now be arranged to immediately
 * follow the code block of R1' so that there is no more jump
 * when the runtime execution exit the loop body (compared
 * to the original diagram, where there are two jumps when
 * we exit the loop: R2 -> R1 and R1 -> RC).
 *
 * Furthermore, to maintain SSA property, we must ensure that
 * variables defined in R1 are renamed in R1', and R2 and RC
 * must receive these variables as block parameters (and hence
 * also renamed accordingly).
 *
 * Our actual algorithm works on the dominator tree structure.
 * At the moment, it only handles while loops that
 *
 *   1. exits to only one RC block;
 *   2. both R2 and RC have only one entry (so that they can be
 *      properly closed).
 *   4. contains no nested loop in R1.
 *
 * We make use of an important property of dominator tree, i.e.,
 * that all leaves either
 *
 *   1. return, or
 *   2. exit to an ancestor (thus forming a loop), or
 *   3. exit to a child of an ancestor.
 *
 * We first annotate all nodes in the dominator tree with a set
 * of its dominance frontiers, S(L), by the following formula:
 *
 *  S(L) = successors_in_CFG(L) + union of S(children(L)) - children(L)
 *
 * If an L itself is in S(L), then it's an entry point to a loop
 * (loop header).
 *
 * The we do a post-order traversal of the annotated tree, and
 * for each loop we must check whether it's a while loop with
 * single RC by locating its RC region.
 *
 * We start from the loop-entry, and check its children:
 *
 *   1. if S(L) for a child L has something not in the sub-tree of
        the loop-entry (i.e., either returns or exits to something
        outside the loop tree), then it becomes a RC candidate.
 *
 *   2. if there are more than one RC candidates, we fail.
 *
 *   3. if there is only one RC candidate, and:
 *      a. if it only returns or exits to outside, we've found RC.
 *      b. otherwise, the RC candidate is an ancestor to the actual
 *         RC, and we iterate down its children from Step 1.
 *
 * After we find a while loop with a single RC, we must identify
 * its R1 and R2:
 *   R2 = the only subtree that exits to loop-entry
 *   R1 = (loop body - R2) with at least one exit to RC
 *
 * We need a sanity check before duplicating R1, e.g., R1 shall not
 * contain nested loops.
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
  structure I    = Identifier
  structure IM   = Identifier.Manager
  structure LD   = I.LabelDict
  structure VD   = I.VariableDict
  structure LS   = I.LabelSet
  structure VS   = I.VariableSet
  structure LDOM = MilCfg.LabelDominance
  structure MU   = MilUtils
  structure ML   = MilLayout
  structure MBV  = MilBoundVars
  structure MFV  = MilFreeVars
  structure MR   = MilRename
  structure TextIO = Pervasive.TextIO
  structure PD    = PassData

  val <- = Try.<-
  val try = Try.try
  fun otherwise v g = case v of SOME x => x | NONE => g ()
  fun fmap f v = case v of SOME x => SOME (f x) | NONE => NONE

  (*
   * The domTree type represents the main DOM tree of interest.
   * It differs from the DOM type in CFG in its annotated frontier
   * and block representation.
   *
   * Whenever we finish inverting a while loop, we collapse its
   * entire sub-tree into a single leaf node. So a leaf node has
   * to keep track a set of blocks instead of just one.
   *)
  type frontier = { blocks : LS.t, exits : bool }
  type domTree  = (M.label * M.block LD.t * frontier) Tree.t

  (*
   * A few helper functions.
   *)
  val vempty = Vector.new0
  val vmap  = Tree.Seq.map
  val vfold = Tree.Seq.fold
  fun keepAll (v, f) = vfold (v, [], fn (x, l) => if f x then x :: l else l)

  fun unionFrontiers (m : frontier, n : frontier) =
      { blocks = LS.union (#blocks m, #blocks n)
      , exits  = (#exits m) orelse (#exits n) }
  fun treeChildren (Tree.T (_,       c) : domTree) = c
  fun treeLabel    (Tree.T ((l,_,_), _) : domTree) = l
  fun treeBlocks   (Tree.T ((_,b,_), _) : domTree) = b
  fun treeFrontier (Tree.T ((_,_,f), _) : domTree) = f
  fun treeChildrenLabels t = vmap (treeChildren t, treeLabel)

  fun rename func (config, blks, dict) =
      LD.map (blks, fn (l, blk) => #2 (func (config, dict, l, blk)))
  val renameLabels = rename MR.Label.block
  val renameVars   = rename MR.Var.block
  val renameBoth   = rename MR.VarLabel.block

  structure Debug =
  struct
    val (debugPassD, debugPass) = Config.Debug.mk (passname, "debug the Mil loop invert")

    fun prints (config, s) = if Config.debug andalso debugPass config then print s else ()

    fun printLayout (config, l) = if Config.debug andalso debugPass config then LU.printLayout l
                                  else ()

    fun layoutNode (label,blks,{blocks,exits}) =
        L.seq [ L.str "(",
                I.layoutLabel label,
                L.str " => ",
                L.sequence ("{","}",",") (map I.layoutLabel (LS.toList blocks)),
                LU.layoutBool exits,
                L.str ")" ]

    fun layoutDom (node) = Tree.layout (node, layoutNode)

    fun layoutBlocks (config, sm, blks) = 
      let 
        val si = I.SymbolInfo.SiManager sm
        fun layoutBlock (l, b) = ML.layoutBlock (config, si, (l, b))
      in 
        L.sequence ("==BEGIN==\n","==END==\n","\n") (List.map (LD.toList blks, layoutBlock))
      end

  end

  (*
   * Collapse a tree into a leaf
   *)
  fun collapse (Tree.T ((label, blks, frontier), children)) : domTree =
      Tree.T ((label,
               vfold (vmap (children, treeBlocks o collapse), blks,
                      fn (b,d) => LD.union (b,d,#3)),
               frontier),
              vempty ())

  (*
   * Close a region (set of blocks) with respect to a given
   * set of parameters. This is done in the following steps:
   *
   * 1. Add a new entry block that takes the given set of
   *    parameters in addition to those found in the old
   *    entry block.
   *
   * 2. Rename afresh the given set of paramemter names in
   *    this region.
   *
   * 3. Replace all block transfer to this region found
   *    in the affected region.
   *)
  fun closeRegion (config, sm, entrylabels, blks, params) =
    case entrylabels
      of [entrylabel] =>
        let
          val M.B {parameters, ...} = <- (LD.lookup (blks, entrylabel)) (* never fail *)
          val parameters    = Vector.map (parameters, fn v => IM.variableClone (sm, v))
          val newparams     = Vector.map (params, fn v => IM.variableClone (sm, v))
          val newparameters = Vector.concat [parameters, newparams]
          val renamedict    = Vector.fold (Vector.zip (params, newparams),
                                Rename.none, fn ((m, n), d) => Rename.renameTo (d, m, n))
          val blks = renameVars (config, blks, renamedict)
          val trans = M.TGoto (M.T { block = entrylabel
                                   , arguments = Vector.map (parameters, M.SVariable) })
          val newentry = IM.labelFresh sm
          val blks = LD.insert (blks, newentry,
                       M.B { parameters = newparameters
                           , instructions = vempty ()
                           , transfer = trans })
          fun replaceT (target as M.T { block, arguments }) =
              if block = entrylabel
                then M.T { block = newentry
                         , arguments = Vector.concat [arguments,
                                       Vector.map (params, M.SVariable)]}
                else target
          fun replaceS ({ on, cases, default }) =
                        { on = on
                        , cases = Vector.map (cases, fn (x, t) => (x, replaceT t))
                        , default = fmap replaceT default }
          fun replaceTr (M.TGoto t) = M.TGoto (replaceT t)
            | replaceTr (M.TCase s) = M.TCase (replaceS s)
            | replaceTr (M.TPSumCase s) = M.TPSumCase (replaceS s)
            | replaceTr x = x
          fun replaceB (M.B { parameters, instructions, transfer }) =
                        M.B { parameters = parameters,
                              instructions = instructions,
                              transfer = replaceTr transfer }
          fun adjust blks = LD.map (blks, fn (l, b) => replaceB b)
        in (blks, adjust)
        end
       | _ => (blks, fn x => x)

  (*
   * Clone a region by refresh all its bounded labels and
   * variables, return a new entry label together with the
   * blocks.
   *)
  fun cloneRegion (config, sm, entrylabel, blks) =
      let
        val labels = LD.domain blks
        val vars   = MBV.blocks (config, blks)
        val vdict  = VS.fold (vars, Rename.none,
                          fn (v, d) => Rename.renameTo (d, v, IM.variableClone (sm, v)))
        val ldict  = LD.fromList (map (fn l => (l, IM.labelFresh sm)) labels)
        val newentry = <- (LD.lookup (ldict, entrylabel)) (* never fail *)
        val newblks = renameBoth (config, blks, (vdict, ldict))
        val newblks = LD.fold (newblks, blks,
                         fn (l, b, m) =>
                            let val l' = <- (LD.lookup (ldict, l))
                            in LD.insert (m, l', b)
                            end)
      in
        (newentry, newblks)
      end

  (*
   * Map a dominator tree to a new tree by annotating each node
   * with the set of exits it dominates, excluding its children.
   *
   * annotateFrontier : (label * block) Tree.t -> domTree
   *)
  fun annotateFrontier (Tree.T ((label, block), children)) : domTree =
      let
        val children = vmap (children, annotateFrontier)
        val labels = LS.fromVector (vmap (children, treeLabel))
        val successors = MU.Block.successors block
        fun gather (t, e) = unionFrontiers (treeFrontier t, e)
        val childrenFrontiers = vfold (children, successors, gather)
        val blocks = LS.difference (#blocks childrenFrontiers, labels)
      in
        Tree.T ((label, LD.singleton (label, block),
                 { blocks = blocks, exits  = #exits childrenFrontiers}),
                children)
      end

  (*
   * Verify if a loop is indeed a while loop that exits to a
   * single RC region. If so, invert this loop.
   *  tryInvertLoop :: Config.t * symbolTableManager * block LD.t * domTree -> block LD.t * domTree
   *)
  fun tryInvertLoop (config, sm, entry) =
      let
        val entrylabel = treeLabel entry
        (*
         * RC is the only (and max) child that only returns or exits to
         * external.  We use "internal" to track possible exits within a
         * tree by keep all ancecstors (from the entry) and their children.
         * We reject invalide cases (multiple RC candidates) by returning
         * NONE, or return SOME [rc] when a single RC is found, or return
         * SOME [] when there is no RC to be found.
         *)
        fun findRC () : (domTree list) option =
            let
              fun find (internal, self) =
                  let
                    val children = treeChildren self
                    val labels   = LS.fromVector (treeChildrenLabels self)
                    val internal = LS.union (internal, labels)
                    fun verify n =
                        let val f = treeFrontier n
                        in  #exits f orelse
                            (not o LS.isEmpty o LS.difference) (#blocks f, internal)
                        end
                    fun found n  = (LS.isEmpty o LS.intersection)
                                   (#blocks (treeFrontier n), internal)
                  in
                    case keepAll (children, verify)
                      of [rc] => if found rc then SOME [rc] else find (internal, rc)
                       | []   => SOME []
                       | _    => NONE
                  end
            in find (LS.singleton entrylabel, entry)
            end
        (*
         * R2 is the only (and max) child that exits to loop entry but
         * not anything internal to the loop tree except RC. The parameter
         * max shall always be the set of loop entry and RC labels.
         *)
        fun findR2 (max) : domTree option =
            let
              fun find (internal, self) =
                  let
                    val children = treeChildren self
                    val labels   = LS.fromVector (treeChildrenLabels self)
                    val internal = LS.union (internal, labels)
                    fun verify n = LS.member (#blocks (treeFrontier n), entrylabel)
                    fun found  n = LS.isSubset (LS.intersection 
                                     (#blocks (treeFrontier n), internal), max)
                  in
                    case keepAll (children, verify)
                       of [r2] => if found r2 then SOME r2 else find (internal, r2)
                       |  _    => NONE
                  end
            in find (LS.singleton entrylabel, entry)
            end
        (*
         * Adjust for the situation where the root of R2 itself exits
         * the loop and it has only one child that qualify as R2.
         * In this case, we can choose this child to be R2.
         *)
        fun adjustR2 (r2) : domTree option =
            let
              fun adjust (internal, r2) : domTree option =
                let
                  val children   = treeChildren r2
                  val labels     = LS.fromVector (treeChildrenLabels r2)
                  val internal   = LS.union (internal, labels)
                  val blk        = hd (LD.range (treeBlocks r2)) (* must never fail *)
                  val successors = MU.Block.successors blk
                  val _ = Debug.prints (config, "successors of " ^ I.labelString (treeLabel r2) 
                                 ^ " is " ^ concat (map I.labelString
                                 (LS.toList (#blocks successors))))
                  fun verify  n  = LS.member (#blocks (treeFrontier n), entrylabel)
                in
                  if (#exits successors orelse
                     (not o LS.isEmpty o LS.difference) (#blocks successors, internal))
                    then if Vector.size children = 0
                           then NONE (* no more R2 when it both exits loop and is a leaf *)
                           else case keepAll (children, verify)
                                  of [newR2] => adjust (internal, newR2)
                                   | _       => SOME r2
                    else SOME r2
                end
            in adjust (LS.fromList [entrylabel, treeLabel r2], r2)
            end
        (*
         * R1 region is the set of nodes not in RC and R2.
         *)
        fun findR1 (exclude) : (domTree list) option =
            let
              fun find (node, r1) =
                  let val l = treeLabel node
                      val children = treeChildren node
                  in if LS.member (exclude, l)
                       then r1
                       else vfold (children, node :: r1, find)
                  end
            in case find (entry, [])
                 of [] => NONE
                  | x  => SOME x
            end
        (*
         * Check if R1 has no node that is a loop.
         * Invariant: any sub-loops would have already been collapsed
         * into a single leaf node that contains more than 1 block.
         *)
         fun sanityCheck (r1) : bool =
            List.forall (r1, fn n => LD.size (treeBlocks n) = 1)
        (*
         * Return the spine from loop entry to a certain label
         * as a list of nodes.
         *)
        fun spineTo label : domTree list =
            let
              fun find spine (node, found) =
                  case (null found, treeLabel node <> label)
                    of (true,  true) => vfold (treeChildren node, [], find (node :: spine))
                     | (false, _   ) => found
                     | (_,    false) => spine
            in find [] (entry, [])
            end
        (*
         * Invert a while loop by duplicating R1 region, and collapse
         * the entire loop tree into a leaf node. This is done in the
         * following steps:
         *   1. We first get the set variables defined on the spine
         *      to R2 (or RC) and close up R2 (or RC) with respect to
         *      the set.
         *   2. Then we create a copy of R1, call it R1', and change
         *      every JMP in R2 to R1'.
         *)
        fun invertLoop (r1, r2, rc) =
           let
             val r2label = treeLabel r2
             val rclabels = map treeLabel rc
             fun concatBlks t = List.fold (t, LD.empty,
                                           fn (x,y) => LD.union (x,y,#3))
             val concatTreeBlks = concatBlks o map treeBlocks
             val toR2 = concatTreeBlks (spineTo r2label)
             val toRC = concatTreeBlks (List.concat (map spineTo rclabels))
             val r1 = concatTreeBlks r1
             val r2 = treeBlocks (collapse r2)
             val rc = concatTreeBlks (map collapse rc)
             fun intersect (s, t) = (VS.toVector o VS.intersection)
                                    (MBV.blocks (config, s), MFV.blocks (config, t))
             val r2v = intersect (toR2, r2)
             val rcv = intersect (toRC, rc)
             val _ = Debug.prints (config, "R1 is " ^ concat (map I.labelString (LD.domain r1)) ^ "\n")
             val _ = Debug.prints (config, "r2v is " ^ concat (map
                     I.variableString' (Vector.toList r2v)) ^ "\n")
             val _ = Debug.prints (config, "rcv is " ^ concat (map
                     I.variableString' (Vector.toList rcv)) ^ "\n")
             val (r2, adjust) = closeRegion (config, sm, [r2label], r2, r2v)
             val _ = Debug.printLayout (config, L.seq [L.str "after closing R2 = ", 
                        Debug.layoutBlocks (config, sm, r2)])
             val r1 = adjust r1
             val _ = Debug.printLayout (config, L.seq [L.str "after adjusting R1 = ", 
                        Debug.layoutBlocks (config, sm, r1)])
             val (rc, adjust) = closeRegion (config, sm, rclabels, rc, rcv)
             val _ = Debug.printLayout (config, L.seq [L.str "after closing RC = ", 
                        Debug.layoutBlocks (config, sm, rc)])
             val r1 = adjust r1
             val _ = Debug.printLayout (config, L.seq [L.str "after adjusting R1 = ", 
                        Debug.layoutBlocks (config, sm, r1)])
             val r2 = adjust r2
             val _ = Debug.printLayout (config, L.seq [L.str "after adjusting R2 = ", 
                        Debug.layoutBlocks (config, sm, r2)])
             val (newlabel, r1) = cloneRegion (config, sm, entrylabel, r1)
             val _ = Debug.printLayout (config, L.seq [L.str "after cloning R1 = ", 
                        Debug.layoutBlocks (config, sm, r1)])
             val r2 = renameLabels (config, r2, LD.fromList [(entrylabel, newlabel)])
             val _ = Debug.printLayout (config, L.seq [L.str "after renaming R2 = ", 
                        Debug.layoutBlocks (config, sm, r2)])
           in Tree.T ((entrylabel, concatBlks [r1,r2,rc], treeFrontier entry), vempty ())
           end
      in otherwise
           (try (fn () =>
                let
                  val rc = <- (findRC ())
                  val rclabels = map treeLabel rc
                  val _ = Debug.prints (config, "RC is " ^ 
                            (String.concatWith (map I.labelString rclabels, ",")) ^ "\n")
                  val r2 = <- (findR2 (LS.fromList (entrylabel :: rclabels)))
                  val _ = Debug.prints (config, "R2 is " ^ I.labelString (treeLabel r2) ^ "\n")
                  val r2 = <- (adjustR2 r2)
                  val r2label = treeLabel r2
                  val _ = Debug.prints (config, "R2 is " ^ I.labelString r2label ^ "\n")
                  val r1 = <- (findR1 (LS.fromList (r2label :: rclabels)))
                in invertLoop (r1, r2, rc)
                end))
           (fn () => collapse entry)
      end

  (*
   * traverse the annotated dominator tree, locate loops from
   * inner to outer, and invert while-loops with one RC.
   *
   *  doDom : Config.t * symbolTableManager * domTree -> domTree
   *)
  fun doDom (config, sm, self) : domTree =
      let
        val Tree.T ((label, blk, frontier), children) = self
        val children = Vector.map (children, fn c => doDom (config, sm, c))
        val self = Tree.T ((label, blk, frontier), children)
      in
        if LS.member (#blocks frontier, label)
          then tryInvertLoop (config, sm, self)
          else self
      end

  fun doBody (config, sm, body) =
      let
        val si = I.SymbolInfo.SiManager sm
        val M.CB {entry, blocks} = body
        val cfg = MilCfg.build(config, si, body)
        val dom = annotateFrontier (MilCfg.getLabelBlockDomTree cfg)
        val _ = Debug.printLayout(config, Debug.layoutDom dom)
        val l = MilCfg.layoutDot (cfg, NONE)
        val _ = LayoutUtils.writeLayout' (l, "loop-cfg.dot", true)
        val dom = doDom (config, sm, dom)
        val blocks = treeBlocks (collapse dom)
      in
        M.CB { entry = entry, blocks = blocks }
      end

  fun doGlobal (config, M.P {includes, externs, globals = gs, symbolTable, entry}) =
      let
        val sm = I.Manager.fromExistingAll symbolTable

        fun doCode (M.F { fx, escapes, recursive, cc, args, rtyps, body }) =
            M.F { fx        = fx,
                  escapes   = escapes,
                  recursive = recursive,
                  cc        = cc,
                  args      = args,
                  rtyps     = rtyps,
                  body      = doBody (config, sm, body) }


        val gs = VD.map (gs, fn (_, glob) => case glob
                   of M.GCode code => M.GCode (doCode code)
                    | _            => glob)
      in
        M.P { includes    = includes,
              externs     = externs,
              globals     = gs,
              symbolTable = I.Manager.finish sm,
              entry       = entry }
      end

  fun program (mil, d) =
      let
        val config = PD.getConfig d
        val mil = doGlobal (config, mil)
        val ()  = PD.report (d, passname)
      in mil
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

  val pass = Pass.mkOptPass (description, associates, BothMil.mkMilPass program)

end
