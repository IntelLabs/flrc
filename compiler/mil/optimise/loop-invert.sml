(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, January 2009 *)

(*
 * Loop inversion: transform while-do loop to do-while loop.
 *
 * The control flow graph of a while-do loop looks like this,
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
 * This is a a pre-step for loop invariant code motion, where
 * invariants can be safely moved out of a do-while loop.
 *
 * Another advantage is that RC can now be arranged to immediately
 * follow the code block of R1' so that there is no more jump
 * when the runtime execution exit the loop body (compared
 * to the original diagram, where there are two jumps when
 * we exit the loop, namely R2 -> R1 and R1 -> RC).
 *
 * To maintain SSA property, we must ensure that variables defined
 * in R1 are renamed in R1', and R2 and RC must receive these
 * variables as block parameters (and hence also renamed accordingly).
 *
 * Our actual algorithm works on the dominator tree structure.
 * At the moment, it only handles while loops where:
 *
 *   1. there is exactly one R2 (s.t. it is a while-do loop);
 *   2. there is one or no RC;
 *   2. R2 and RC have only one entry;
 *   4. R1 contains no loop.
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
 * If an L itself is in S(L), then it's loop header.
 *
 * The we do a post-order traversal of the annotated tree, and
 * for each loop we must check whether it's a qualifying while-do
 * loop by first locating its RC region.
 *
 * We start from the loop header, and check its children:
 *
 *   1. if S(L) for a child L has something not in the sub-tree of
        the loop header (i.e., either returns or exits to something
        outside the loop tree), then it becomes a RC candidate.
 *
 *   2. if there are more than one RC candidates, we fail.
 *
 *   3. if there is only one RC candidate, and:
 *      a. if it only returns or exits to outside, we've found RC.
 *      b. otherwise, the RC candidate is an ancestor to the actual
 *         RC, and we iterate down its children from Step 1.
 *
 * After we find RC, we then identify R1 and R2:
 *   R2 = the only subtree that exits to loop header
 *   R1 = loop body - R2
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
  structure V    = Vector
  structure O    = Option
  structure UO   = Utils.Option
  structure PD   = PassData
  structure L    = Layout
  structure LU   = LayoutUtils
  structure I    = Identifier
  structure IM   = Identifier.Manager
  structure LD   = I.LabelDict
  structure VD   = I.VariableDict
  structure LS   = I.LabelSet
  structure VS   = I.VariableSet
  structure MU   = MilUtils
  structure ML   = MilLayout
  structure MBV  = MilBoundVars
  structure MFV  = MilFreeVars
  structure MR   = MilRename

  val <- = Try.<-
  val try = Try.try
  val require = Try.require
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
  type blocks   = M.block LD.t
  type frontier = { blocks : LS.t, exits : bool }
  type domTree  = (M.label * blocks * frontier) Tree.t

  (*
   * A few helper functions.
   *)
  val vempty = V.new0
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

    fun debug config = Config.debug andalso debugPass config

    fun prints (config, s) = if debug config then print s else ()

    fun printLayout (config, l) = if debug config then LU.printLayout l else ()

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
        L.sequence ("","","\n") (List.map (LD.toList blks, layoutBlock))
      end
  end

  (*
   * Collapse a tree into a leaf
   *)
  fun collapse (Tree.T ((label, blks, frontier), children)) : domTree =
      Tree.T ((label, vfold (vmap (children, treeBlocks o collapse), blks,
                             fn (b,d) => LD.union (b,d,#3)), frontier), vempty ())

  (*
   * Close a region (set of blocks) with respect to a given
   * set of arguments. This is done in the following steps:
   *
   * 1. Add a new entry block that takes the given set of
   *    arguments in addition to those found in the old
   *    entry block.
   *
   * 2. Rename afresh the given set of argument names in
   *    this region.
   *
   * 3. Replace all block transfer to the entry to this
   *    region found in the affected region.
   *
   * Step 3 is done by returning an adjustment function.
   *)
  val closeRegion : Config.t * M.symbolTableManager * M.label * blocks * I.variable V.t
                    -> blocks * (blocks -> blocks option) =
    fn (config, sm, entrylabel, blks, arguments) =>
       let
         (* Get the original parameters *)
         val M.B {parameters, ...} = <- (LD.lookup (blks, entrylabel)) (* never fail *)
         (* Map them into fresh names *)
         val parameters    = V.map (parameters, fn v => IM.variableClone (sm, v))
         (* Map the additional arguments into fresh names *)
         val newarguments  = V.map (arguments, fn v => IM.variableClone (sm, v))
         (* Make a new parameter list for the new entry block *)
         val newparameters = V.concat [parameters, newarguments]
         (* Rename all occurrances of the additional arguments *)
         val renamedict    = V.fold (V.zip (arguments, newarguments),
                               Rename.none, fn ((m, n), d) => Rename.renameTo (d, m, n))
         val blks = renameVars (config, blks, renamedict)
         (* Add an entry block that jumps to the old entry block *)
         val trans = M.TGoto (M.T { block = entrylabel
                                  , arguments = V.map (parameters, M.SVariable) })
         val newentry = IM.labelFresh sm
         val blks = LD.insert (blks, newentry,
                      M.B { parameters = newparameters
                          , instructions = vempty ()
                          , transfer = trans })
         (*
          * Prepare for the adjust function that replaces all jumps to
          * the old entry with the new one.
          *)
         fun replaceT (target as M.T { block = label, arguments = args }) =
             if label = entrylabel
               then M.T { block = newentry
                        , arguments = V.concat [args,
                                      V.map (arguments, M.SVariable)]}
               else target
         fun replaceS ({ on, cases, default }) =
                       { on = on
                       , cases = V.map (cases, fn (x, t) => (x, replaceT t))
                       , default = fmap replaceT default }
         fun replaceTr (M.TGoto t) = SOME (M.TGoto (replaceT t))
           | replaceTr (M.TCase s) = SOME (M.TCase (replaceS s))
           | replaceTr (M.TPSumCase s) = SOME (M.TPSumCase (replaceS s))
           (* if there are calls returning to the entrylabel, we fail! *)
           | replaceTr (x as M.TInterProc { ret = M.RNormal { block, ... }, ...  }) =
             if block = entrylabel then NONE else SOME x
           (* if there are cuts to the entrylabel, we fail! *)
           | replaceTr (x as M.TCut { cuts = M.C { targets, ... }, ... }) =
             if LS.member (targets, entrylabel) then NONE else SOME x
           | replaceTr x = SOME x
         fun adjust blks = try (fn () =>
             LD.map (blks, fn (_, M.B { parameters, instructions, transfer }) =>
                              M.B { parameters = parameters,
                                    instructions = instructions,
                                    transfer = <- (replaceTr transfer)}))
       in
          (blks, adjust)
       end

  (*
   * Clone a region by refresh all its bounded labels and
   * variables, return a new entry label together with the
   * blocks including both old and new.
   *)
   val cloneRegion : Config.t * M.symbolTableManager * M.label * blocks
                     -> M.label * blocks =
     fn (config, sm, entrylabel, blks) =>
       let
         val vars   = MBV.blocks (config, blks)
         val vdict  = VS.fold (vars, Rename.none,
                           fn (v, d) => Rename.renameTo (d, v, IM.variableClone (sm, v)))
         val ldict  = LD.map (blks, fn _ => IM.labelFresh sm)
         val newentry = <- (LD.lookup (ldict, entrylabel)) (* never fail *)
         val newblks = renameBoth (config, blks, (vdict, ldict))
         (* the following is not the same as union, since we need
          * to assign new labels to newblks *)
         val newblks = LD.fold (newblks, blks,
                         fn (l, b, m) =>
                            let val l' = <- (LD.lookup (ldict, l)) (* never fail *)
                            in LD.insert (m, l', b)
                            end)
       in
        (newentry, newblks)
       end

  (*
   * Map a dominator tree to a new tree by annotating each node
   * with its dominance frontier.
   *
   *  annotateFrontier : (label * block) Tree.t -> domTree
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
   *
   * Always return a collapsed domTree.
   *)
  val tryInvertLoop : Config.t * M.symbolTableManager * domTree -> domTree =
    fn (config, sm, header) =>
      let
        fun printLayout (m, l) = Debug.printLayout (config, L.seq [L.str m, l])
        fun printList   (m, l) = printLayout (m, L.sequence ("{","}",",") l)
        fun printRegion (m, r) = printLayout (m, Debug.layoutBlocks (config, sm, r))

        val headerlabel = treeLabel header
        (*
         * RC is the only (and max) child that only returns or exits to
         * external.  We use "internal" to track possible exits within a
         * tree using a set of all ancecstors (from the header) and their
         * children.
         * We reject invalide cases (multiple RC candidates) by failing,
         * or return SOME rc when a single RC is found, or return NONE
         * when there is no RC to be found.
         *)
        fun findRC () : domTree option =
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
                      of [rc] => if found rc then SOME rc else find (internal, rc)
                       | []   => NONE
                       | _    => Try.fail ()
                  end
            in find (LS.singleton headerlabel, header)
            end
        (*
         * R2 is the only (and max) child that exits to loop header
         * but not to anything internal (except RC). The parameter
         * max shall always be the set consisting of only loop
         * header and RC label.
         *)
        fun findR2 (max) : domTree option =
            let
              fun find (internal, self) =
                  let
                    val children = treeChildren self
                    val labels   = LS.fromVector (treeChildrenLabels self)
                    val internal = LS.union (internal, labels)
                    fun verify n = LS.member (#blocks (treeFrontier n), headerlabel)
                    fun found  n = LS.isSubset (LS.intersection
                                      (#blocks (treeFrontier n), internal), max)
                  in
                    case keepAll (children, verify)
                       of [r2] => if found r2 then SOME r2 else find (internal, r2)
                       |  _    => NONE
                  end
            in find (LS.singleton headerlabel, header)
            end
        (*
         * Adjust for the situation where the root of R2 itself exits
         * the loop and it has only one child that qualify as R2.
         * In this case, we choose its child to be R2.
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
                  fun verify  n  = LS.member (#blocks (treeFrontier n), headerlabel)
                in
                  if (#exits successors orelse
                     (not o LS.isEmpty o LS.difference) (#blocks successors, internal))
                    then if V.size children = 0
                           then NONE (* no more R2 when it both exits loop and is a leaf *)
                           else case keepAll (children, verify)
                                  of [newR2] => adjust (internal, newR2)
                                   | _       => SOME r2
                    else SOME r2
                end
            in adjust (LS.fromList [headerlabel, treeLabel r2], r2)
            end
        (*
         * R1 region is the set of nodes not in RC and R2.
         * Return NONE when the region is empty.
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
            in case find (header, [])
                 of [] => NONE
                  | x  => SOME x
            end
        (*
         * Check if R1 has no node that is a loop.
         * Invariant: any sub-loops would have already been collapsed
         * into a single leaf node that contains more than 1 block.
         *
         * TODO:
         *   shall we check if R1 actually returns or exits to RC?
         *)
         fun sanityCheck (r1) : bool =
             not (null r1) andalso List.forall (r1, fn n => LD.size (treeBlocks n) = 1)
        (*
         * Return the spine from loop header to a certain label
         * as a list of nodes.
         *)
        fun spineTo label : domTree list =
            let
              fun find spine (node, found) =
                  case (null found, treeLabel node <> label)
                    of (true,  true) => vfold (treeChildren node, [], find (node :: spine))
                     | (false, _   ) => found
                     | (_,    false) => spine
            in find [] (header, [])
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
         *
         * Note that we still may fail if the closing a region involves
         * adjusting inter-proc call or cut transfers.
         *)
        fun invertLoop (r1, r2, rc) : domTree option = try (fn () => 
           let
             fun concatBlks t = List.fold (t, LD.empty,
                                           fn (x,y) => LD.union (x,y,#3))
             val concatTreeBlks = concatBlks o map treeBlocks

             fun process r =
                 let
                   val label = treeLabel r
                   (* Find spine blocks to node r *)
                   val toR = concatTreeBlks (spineTo label)
                   (* Turn r into blocks *)
                   val r = treeBlocks (collapse r)
                   (* Find variables bound on the spine, and free in r *)
                   fun intersect (s, t) = (VS.toVector o VS.intersection)
                                          (MBV.blocks (config, s), MFV.blocks (config, t))
                   val rv = intersect (toR, r)
                   val _ = printList ("Set of var: ", map I.layoutVariable' (V.toList rv))
                   val (r, adjust) = closeRegion (config, sm, label, r, rv)
                   val _ = printRegion ("After closing: ", r)
               in (r, adjust)
               end

             (* Turn R1 into blocks *)
             val r1 = concatTreeBlks r1

             (* Process R2 *)
             val _ = Debug.prints (config, "Closing R2\n")
             val (r2, adjust) = process r2
             val r1 = <- (adjust r1)
             val _ = printRegion ("Adjusted R1: ", r1)

             (* Process RC *)
             val (r1, r2, rc) = UO.dispatch (rc,
                         fn rc => let
                                    val _ = Debug.prints (config, "Closing RC\n")
                                    val (rc, adjust) = process rc
                                    val r1 = <- (adjust r1)
                                    val _ = printRegion ("Adjusted R1: ", r1)
                                    val r2 = <- (adjust r2)
                                    val _ = printRegion ("Adjusted R2: ", r2)
                                  in (r1, r2, rc)
                                  end,
                         fn () => (r1, r2, LD.empty))

             (* Clone R1 *)
             val (newlabel, r1) = cloneRegion (config, sm, headerlabel, r1)
             val _ = printRegion ("Cloned R1: ", r1)
             (* Let R2 jump to the cloned R1 *)
             val r2 = renameLabels (config, r2, LD.fromList [(headerlabel, newlabel)])
             val _ = printRegion ("Adjusted R2: ", r2)
           in Tree.T ((headerlabel, concatBlks [r1,r2,rc], treeFrontier header), vempty ())
           end)
      in UO.out
           (try (fn () =>
                let
                  val rc = findRC ()
                  val rclabels = map treeLabel (UO.toList rc)
                  val _ = printList ("Found RC: ", map I.layoutLabel rclabels)
                  val r2 = <- (findR2 (LS.fromList (headerlabel :: rclabels)))
                  val _ = printLayout ("Found R2: ", I.layoutLabel (treeLabel r2))
                  val r2 = <- (adjustR2 r2)
                  val r2label = treeLabel r2
                  val _ = printLayout ("Adjusted R2: ", I.layoutLabel r2label)
                  val r1 = <- (findR1 (LS.fromList (r2label :: rclabels)))
                  val _ = require (sanityCheck r1)
                  val _ = printList ("Found R1: ", map (I.layoutLabel o treeLabel) r1)
                  val header = <- (invertLoop (r1, r2, rc))
                in
                  header
                end),
           fn () => collapse header)
      end

  (*
   * Traverse an annotated dominator tree, locate loops from
   * inner to outer, and invert while-do loops that exits to
   * only one RC.
   *
   *  doDom : Config.t * M.symbolTableManager * domTree -> domTree
   *)
  fun doDom (config, sm, self) : domTree =
      let
        val Tree.T ((label, blk, frontier), children) = self
        val children = V.map (children, fn c => doDom (config, sm, c))
        val self = Tree.T ((label, blk, frontier), children)
      in
        if LS.member (#blocks frontier, label)
          then let val _ = Debug.prints (config, "Loop header: " ^ I.labelString label ^ "\n")
               in tryInvertLoop (config, sm, self)
               end
          else self
      end

  fun doBody (config, sm, body) =
      let
        val si = I.SymbolInfo.SiManager sm
        val M.CB {entry, blocks} = body
        val cfg = MilCfg.build(config, si, body)
        val dom = annotateFrontier (MilCfg.getLabelBlockDomTree cfg)
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
