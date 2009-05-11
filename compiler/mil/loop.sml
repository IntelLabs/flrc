(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

signature MIL_LOOP =
sig

  type blocks = Mil.block Identifier.LabelDict.t

  datatype loop = L of {
    header : Mil.label,
    blocks : blocks
  }

  type loopTree = loop Tree.t
  type loopForest = loopTree Vector.t

  type t

  (* Build the loop information for the given Mil CFG and Dom Tree.
   * Does not compute all nodes, exits, preheaders, induction variables, or trip counts
   *   - these must be added with other calls.
   *)
  val build : Config.t * Mil.symbolInfo * MilCfg.t * (Mil.label * Mil.block) Tree.t -> t

  (* Build the loop information from existing loop structure.
   * Does not compute all nodes, exits, preheaders, induction variables, or trip counts
   *   - these must be added with other calls.
   *)
  val fromLoops : Config.t * Mil.symbolInfo * {entry : Mil.label, loops : loopForest, blocksNotInLoops : blocks} -> t

  val unbuild : t -> Mil.codeBody

  val layout : t -> Layout.t

  val getEntry            : t -> Mil.label
  val getLoops            : t -> loopForest
  val getBlocksNotInLoops : t -> blocks

  val genAllNodes : t -> t
  val getAllNodes : t * Mil.label -> blocks
  val allNodes : t -> blocks Identifier.LabelDict.t

  val genExits : t -> t (* pre: requires all nodes *)
  val getExits : t * Mil.label -> Identifier.LabelSet.t
  val allExits : t -> Identifier.LabelSet.t Identifier.LabelDict.t

  (* Make preheaders for the loops and return the new loop information.
   * If a header is the target of a cut edge then a preheader cannot be
   * made for it, in that case the mapping will be undefined for that header.
   * It removes the all nodes information if that was present.
   *)
  val addPreheaders : t * Mil.symbolTableManager -> t
  val getPreheader : t * Mil.label -> Mil.label option
  val getPreheaders : t -> Mil.label Identifier.LabelDict.t

  (* Let # = 0 on entry to loop and incremented on each iteration.
   * Then IV {variable, init = (r1, opnd, r2), step} means that:
   *   variable is always r1*opnd+r2+step*# in the arithmetic of variable's type
   *)
  datatype inductionVariable = IV of {
    variable : Mil.variable,
    init     : Rat.t * Mil.operand * Rat.t,
    step     : Rat.t
  }

  val genInductionVariables : t * FMil.t * MilCfg.t -> t (* pre: requires all nodes *)
  val getInductionVariables : t * Mil.label -> inductionVariable list
  val inductionVars : t -> inductionVariable list Identifier.LabelDict.t

  (* For trip counts we only deal with loops that have a certain form:
   *   There is a block that has the only exit edges for the loop and that
   *   dominates all the back edges of the loop.
   * If a loop has such a form then a trip count
   *   {block, cond, flip1, comparison, flip2, init=(m,i,c), step, bound}
   * indicates that the loop exits from block the first time that cmp(o1, o2)
   * is true where:
   *   # = number of times around the loop starting at 0
   *   cmp = if flip1 then not comparison else comparison
   *   o1, o2 = if flip2 then bound, iv else iv, bound
   *   iv = step*# + m*i+c
   *)
  datatype tripCount = TC of {
    block      : Mil.label,
    cond       : Mil.variable,
    flip1      : bool,
    comparison : Prims.compare,
    flip2      : bool,
    init       : Rat.t * Mil.operand * Rat.t,
    step       : Rat.t,
    bound      : Mil.operand
  }

  (* Compute trip counts for loops.  Only loops of the above form will have
   * trip counts, but only if a trip count exists for them.
   * pre: requires all nodes, exits, and induction variables
   *)
  val genTripCounts : t * FMil.t * MilCfg.t * (Mil.label * Mil.block) Tree.t -> t
  val getTripCount : t * Mil.label -> tripCount option
  val allTripCounts : t -> tripCount Identifier.LabelDict.t
end;

structure MilLoop :> MIL_LOOP =
struct

  val moduleName = "MilLoop"

  fun fail (f, m) = Fail.fail (moduleName, f, m)

  structure L = Layout
  structure LU = LayoutUtils

  structure I = Identifier
  structure VD = I.VariableDict
  structure VS = I.VariableSet
  structure LD = I.LabelDict
  structure LS = I.LabelSet
  structure P = Prims
  structure M = Mil
  structure MU = MilUtils
  structure MSTM = MU.SymbolTableManager
  structure Cfg = MilCfg

  type blocks = M.block LD.t

  datatype loop = L of {
    header : M.label,
    blocks : blocks
  }

  type loopTree = loop Tree.t
  type loopForest = loopTree Vector.t

  datatype inductionVariable = IV of {
    variable : M.variable,
    init     : Rat.t * M.operand * Rat.t,
    step     : Rat.t
  }

  datatype tripCount = TC of {
    block      : M.label,
    cond       : M.variable,
    flip1      : bool,
    comparison : P.compare,
    flip2      : bool,
    init       : Rat.t * M.operand * Rat.t,
    step       : Rat.t,
    bound      : M.operand
  }

  datatype t = LS of {
    config           : Config.t,
    si               : M.symbolInfo,
    entry            : M.label,
    loops            : loopForest,
    blocksNotInLoops : blocks,
    allNodes         : blocks LD.t,
    exits            : LS.t LD.t,
    preheaders       : M.label LD.t,
    inductionVars    : inductionVariable list LD.t,
    tripCounts       : tripCount LD.t
  }

  (* inLoops tracks which loops a block is in, nested or otherwise
   * top tracks which blocks are outside all loops
   * loops tracks which blocks are immediately within each loop
   * topLoops is all the top level loops
   * subloops gives the loops immediately nested within a given loop
   *)
  datatype buildState = S of {
    inLoops  : LS.t LD.t ref,
    top      : blocks ref,
    loops    : blocks LD.t ref,
    topLoops : loopTree list ref,
    subloops : loopTree list LD.t ref
  }

  fun buildStateMk () =
      S {inLoops = ref LD.empty, top = ref LD.empty, loops = ref LD.empty, topLoops = ref [], subloops = ref LD.empty}

  fun getInLoops (S {inLoops, ...}, l) = Utils.Option.get (LD.lookup (!inLoops, l), LS.empty)

  fun addInLoop (s as S {inLoops, ...}, l, h) =
      inLoops := LD.insert (!inLoops, l, LS.insert (getInLoops (s, l), h))

  fun getTop (S {top, ...}) = !top

  fun addTop (S {top, ...}, l, b) = top := LD.insert (!top, l, b)

  fun getLoop (S {loops, ...}, h) = LD.lookup (!loops, h)

  fun getLoop' (s, h) = Utils.Option.get (getLoop (s, h), LD.empty)

  fun addLoop (s as S {loops, ...}, l, b, h) = loops := LD.insert (!loops, h, LD.insert (getLoop' (s, h), l, b))

  fun getTopLoops (S {topLoops, ...}) = Vector.fromList (List.rev (!topLoops))

  fun addTopLoop (S {topLoops, ...}, l) = topLoops := l::(!topLoops)

  fun getSubloops (S {subloops, ...}, h) =
      let
        val ls = Utils.Option.get (LD.lookup (!subloops, h), [])
        val ls = Vector.fromList (List.rev ls)
      in ls
      end

  fun addSubloop (S {subloops, ...}, h, l) =
      let
        val ls = Utils.Option.get (LD.lookup (!subloops, h), [])
        val ls = l::ls
        val () = subloops := LD.insert (!subloops, h, ls)
      in ()
      end

  datatype buildEnv = E of {config : Config.t, si : M.symbolInfo}

  fun buildEnvMk (c, si) = E {config = c, si = si}

  (* Natural loops:
   *   A back edge in the dominator tree from l to h identifies a natural
   *   loop.  The header of this loop is h.  The nodes of the loop are all
   *   nodes reachable from l going backwards over edges in the CFG until h
   *   is reached.
   *)

  (* Given a back edge from l to h, for each of the nodes in the natural loop
   * add h to the set of loops that node is in.
   *)
  fun addNaturalLoop (state, env, cfg, l, h) =
      let
        fun loop (visited, stk) =
            case stk
             of [] => ()
              | (l, n)::stk =>
                let
                  val () = addInLoop (state, l, h)
                  val visited = LS.insert (visited, l)
                  fun doOne (n, stk) =
                      case Cfg.nodeGetLabel (cfg, n)
                       of NONE => stk
                        | SOME l => if LS.member (visited, l) then stk else (l, n)::stk
                  val stk =
                      if l <> h
                      then List.fold (Cfg.pred (cfg, n), stk, doOne)
                      else stk
                in loop (visited, stk)
                end
        val n = Cfg.labelGetNode (cfg, l)
        val () = loop (LS.empty, [(l, n)])
      in ()
      end

  (* Determine which loops each block in dt is in.
   *   dt is some subtree of the dominator tree
   *   ds is the set of parents of dt in the dominator tree
   *)
  fun determineInLoops (state, env, cfg, dt, ds) =
      let
        val Tree.T ((l, b), children) = dt
        val ds = LS.insert (ds, l)
        fun doTarget t =
            if LS.member (ds, t) then
              addNaturalLoop (state, env, cfg, l, t)
            else
              ()
        val () = LS.foreach (#blocks (MU.Block.successors b), doTarget)
        fun doChild c = determineInLoops (state, env, cfg, c, ds)
        val () = Vector.foreach (children, doChild)
      in ()
      end

  (* Build the loop forrest for dt
   *   dt is some subtree of the dominator tree
   *   ds is the list of parents of dt in the dominator tree ordered from
   *     immediate parent to root
   *)
  fun buildLoops (cfg, state, env, dt, ds) =
      let
        val Tree.T ((l, b), children) = dt
        val ds' = l::ds
        (* Build children first - l's blocks/subloops are in the children *)
        fun doChild c = buildLoops (cfg, state, env, c, ds')
        val () = Vector.foreach (children, doChild)
        val myLoops = getInLoops (state, l)
        fun detLoop ds = List.peek (ds, fn h => LS.member (myLoops, h))
        val () =
            case detLoop ds'
             of NONE   => addTop (state, l, b)
              | SOME h => addLoop (state, l, b, h)
        val () =
            case getLoop (state, l)
             of NONE => ()
              | SOME blks =>
                let
                  val loop = L {header = l, blocks = blks}
                  val sl = getSubloops (state, l)
                  val lt = Tree.T (loop, sl)
                in
                  case detLoop ds
                   of NONE   => addTopLoop (state, lt)
                    | SOME h => addSubloop (state, h, lt)
                end
      in ()
      end

  fun fromLoops (c, si, {entry, loops, blocksNotInLoops}) = 
      LS {config = c, si = si, entry = entry, loops = loops, blocksNotInLoops = blocksNotInLoops, allNodes = LD.empty,
          exits = LD.empty, preheaders = LD.empty, inductionVars = LD.empty, tripCounts = LD.empty}

  fun build (c, si, cfg, dt) =
      let
        val entry = Cfg.startLabel cfg
        val state = buildStateMk ()
        val env = buildEnvMk (c, si)
        val () = determineInLoops (state, env, cfg, dt, LS.empty)
        val () = buildLoops (cfg, state, env, dt, [])
        val loops = getTopLoops state
        val top = getTop state
        val r = fromLoops (c, si, {entry = entry, loops = loops, blocksNotInLoops = top})
      in r
      end

  fun getConfig           (LS {config,           ...}) = config
  fun getSi               (LS {si,               ...}) = si
  fun getEntry            (LS {entry,            ...}) = entry
  fun getLoops            (LS {loops,            ...}) = loops
  fun getBlocksNotInLoops (LS {blocksNotInLoops, ...}) = blocksNotInLoops

  fun unbuild ls =
      let
        fun doLoop (L {header, blocks}, nblks) = LD.insertAll (nblks, LD.toList blocks)
        fun doLoopTree (lt, nblks) = Tree.foldPre (lt, nblks, doLoop)
        val allBlks = Vector.fold (getLoops ls, getBlocksNotInLoops ls, doLoopTree)
      in M.CB {entry = getEntry ls, blocks = allBlks}
      end

  (*** Layout ***)

  fun layoutBlocks blks =
      let
        val blks = LD.toList blks
        fun doOne (l, _) = I.layoutLabel l
        val l = L.sequence ("{", "}", ",") (List.map (blks, doOne))
      in l
      end

  fun layoutLoops ls =
      let
        val Tree.T (L {header, blocks, ...}, children) = ls
        val l1 = L.seq [L.str "Header: ", I.layoutLabel header]
        val l2' = layoutBlocks blocks
        val l2 = L.mayAlign [L.str "Blocks:", LU.indent l2']
        val l3' = Vector.toListMap (children, layoutLoops)
        val l3 = L.align [L.str "Loops:", LU.indent (L.align l3')]
        val l = L.align [l1, l2, l3]
      in l
      end

  fun layout (LS {entry, loops, blocksNotInLoops, ...}) =
      let
        val l1 = L.seq [L.str "Entry: ", I.layoutLabel entry]
        val l2' = layoutBlocks blocksNotInLoops
        val l2 = L.mayAlign [L.str "Blocks:", LU.indent l2']
        val l3' = Vector.toListMap (loops, layoutLoops)
        val l3 = L.align [L.str "Loops:", LU.indent (L.align l3')]
        val l = L.align [l1, l2, l3]
      in l
      end

  (*** All nodes ***)

  fun genAllNodes ls =
      let
        val LS {config, si, entry, loops, blocksNotInLoops, allNodes, exits, preheaders, inductionVars, tripCounts} =
            ls
        val allNodes = ref LD.empty
        fun addNodes (h, ns) = allNodes := LD.insert (!allNodes, h, ns)
        fun doLoop l =
            let
              val Tree.T (L {header, blocks, ...}, children) = l
              fun doChild (c, nodes) =
                  let
                    val childNodes = doLoop c
                    fun doOne (l, b, nodes) = LD.insert (nodes, l, b)
                    val nodes = LD.fold (childNodes, nodes, doOne)
                  in nodes
                  end
              val nodes = Vector.fold (children, blocks, doChild)
              val () = addNodes (header, nodes)
            in nodes
            end
        fun doOne l = let val _ = doLoop l in () end
        val () = Vector.foreach (loops, doOne)
        val r =
            LS {config = config, si = si, entry = entry, loops = loops, blocksNotInLoops = blocksNotInLoops,
                allNodes = !allNodes, exits = exits, preheaders = preheaders, inductionVars = inductionVars,
                tripCounts = tripCounts}
      in r
      end

  fun getAllNodes (LS {allNodes, ...}, h) =
      case LD.lookup (allNodes, h)
       of NONE    => fail ("getAllNodes", "no all nodes for header: " ^ I.labelString h)
        | SOME ns => ns

  fun allNodes (LS {allNodes, ...}) = allNodes

  (*** Exits ***)

  fun genExits ls =
      let
        val LS {config, si, entry, loops, blocksNotInLoops, allNodes, exits, preheaders, inductionVars, tripCounts} =
            ls
        val exits = ref LD.empty
        fun addExits (h, es) = exits := LD.insert (!exits, h, es)
        fun doLoop l =
            let
              val Tree.T (L {header, ...}, children) = l
              val myNodes = getAllNodes (ls, header)
              fun doOne (l, b, es) =
                  let
                    val {blocks, exits} = MU.Block.successors b
                    fun checkOne l = not (LD.contains (myNodes, l))
                    val es = if exits orelse LS.exists (blocks, checkOne) then LS.insert (es, l) else es
                  in es
                  end
              val es = LD.fold (myNodes, LS.empty, doOne)
              val () = addExits (header, es)
              val () = Vector.foreach (children, doLoop)
            in ()
            end
        val () = Vector.foreach (loops, doLoop)
        val r =
            LS {config = config, si = si, entry = entry, loops = loops, blocksNotInLoops = blocksNotInLoops,
                allNodes = allNodes, exits = !exits, preheaders = preheaders, inductionVars = inductionVars,
                tripCounts = tripCounts}
      in r
      end

  fun getExits (LS {exits, ...}, h) =
      case LD.lookup (exits, h)
       of NONE    => fail ("getExits", "no exits for header: " ^ I.labelString h)
        | SOME ns => ns

  fun allExits (LS {exits, ...}) = exits

  (*** Generate and link in preheaders for a loop structure ***)

  fun cutsInBlock (ls, b, cs) = LS.union (cs, MU.Cuts.targets (MU.Block.cuts b))

  fun cutsInLoop (ls, l, cs) =
      let
        val L {blocks, ...} = l
        fun doOne (_, b, cs) = cutsInBlock (ls, b, cs)
        val cs = LD.fold (blocks, cs, doOne)
      in cs
      end

  fun cutsInLoops (ls, cs) =
      let
        val Tree.T (l, children) = ls
        val cs = cutsInLoop (ls, l, cs)
        fun doOne (ls, cs) = cutsInLoops (ls, cs)
        val cs = Vector.fold (children, cs, doOne)
      in cs
      end

  (* Compute all blocks that are the target of a cut edge in the loops *)
  fun cuts ls =
      let
        fun doOne (_, b, cs) = cutsInBlock (ls, b, cs)
        val cs = LD.fold (getBlocksNotInLoops ls, LS.empty, doOne)
        fun doOne (ls, cs) = cutsInLoops (ls, cs)
        val cs = Vector.fold (getLoops ls, cs, doOne)
      in cs
      end

  (* Make the preheader labels and mapping of header to preheader *)
  fun genPreheaders (ls, stm, cuts) =
      let
        fun doOne (Tree.T (L {header, blocks, ...}, children), phs) =
            let
              val phs =
                  if LS.member (cuts, header) then
                    phs
                  else
                    let
                      val ph = MSTM.labelFresh stm
                    in
                      LD.insert (phs, header, ph)
                    end
              val phs = Vector.fold (children, phs, doOne)
            in phs
            end
        val phs = Vector.fold (getLoops ls, LD.empty, doOne)
      in phs
      end

  (* Link in the preheaders by retargeting entry edges to preheaders
   * Make the actual preheader blocks
   *)

  fun retargetBlock (ls, phs, b) =
      let
        val M.B {parameters = ps, instructions = is, transfer = t} = b
        val t = MilRename.Label.transfer (getConfig ls, phs, t)
        val b = M.B {parameters = ps, instructions = is, transfer = t}
      in b
      end

  fun genPreheaderBlock (ls, stm, ph, h, blocks) =
      let
        val hb = Option.valOf (LD.lookup (blocks, h))
        val M.B {parameters = ps, ...} = hb
        val nps = Vector.map (ps, fn p => MSTM.variableClone (stm, p))
        val gas = Vector.map (nps, M.SVariable)
        val t = M.TGoto (M.T {block = h, arguments = gas})
        val phb = M.B {parameters = nps, instructions = Vector.new0 (), transfer = t}
      in phb
      end

  fun addPreheadersA (ls, stm, phs) =
      let
        val LS {config, si, entry, loops, blocksNotInLoops, allNodes, exits, preheaders, inductionVars, tripCounts} =
            ls
        val entry = Utils.Option.get (LD.lookup (phs, entry), entry)
        val blks = LD.map (blocksNotInLoops, fn (_, b) => retargetBlock (ls, phs, b))
        fun doLoop phs (Tree.T (L {header, blocks}, children), blks) =
            let
              val blks =
                  case LD.lookup (phs, header)
                   of NONE => blks
                    | SOME ph =>
                      let
                        val phb = genPreheaderBlock (ls, stm, ph, header, blocks)
                      in
                        LD.insert (blks, ph, phb)
                      end
              (* Do not retarget blocks in loop to preheader *)
              val phs = LD.remove (phs, header)
              fun doBlk (_, b) = retargetBlock (ls, phs, b)
              val blocks = LD.map (blocks, doBlk)
              val (children, blocks) = Vector.mapAndFold (children, blocks, doLoop phs)
              val ls = Tree.T (L {header = header, blocks = blocks}, children)
            in (ls, blks)
            end
        val (loops, blks) = Vector.mapAndFold (loops, blks, doLoop phs)
        val r =
            LS {config = config, si = si, entry = entry, loops = loops, blocksNotInLoops = blks,
                allNodes = LD.empty, exits = exits, preheaders = phs, inductionVars = inductionVars,
                tripCounts = tripCounts}
      in r
      end

  (* The actual preheader generation and link in *)
  fun addPreheaders (ls, stm) =
      let
        val cuts = cuts ls
        val phs = genPreheaders (ls, stm, cuts)
        val ls = addPreheadersA (ls, stm, phs)
      in ls
      end

  fun getPreheader (LS {preheaders, ...}, h) = LD.lookup (preheaders, h)

  fun getPreheaders (LS {preheaders, ...}) = preheaders

  (*** Induction Variables ***)

  (* Strategy:
   *   1. Figure out which variables are linear functions of other variables.
   *   2. If a parameter to a loop header has itself plus a constant coming
   *      in on all loop edges, ie, if the argument is a linear function of
   *      the loop parameter with a multiplier of 1, and the same operand
   *      comes in on all entry edges then we've found a basic induction
   *      variable.
   *   3. Any linear function of a basic induction variable is an induction
   *      variable.
   *
   * We only deal with rats and machine integers, and to keep things simple
   * represent all constants as rats.
   *)

  (* The linear function m * var + c in the arithmetic of var's type *)
  type linearFunction = {var : M.variable, m : Rat.t, c : Rat.t}

  fun layoutLinearFunction ({var, m, c}) =
      L.seq [Rat.layout m, L.str "*", I.layoutVariable' var, L.str "+", Rat.layout c]

  (* We want to figure out that a variable is linear function of another
   * variable.  To do this we want to find out that a variable is ultimately
   * either a constant or a linear function.
   * Note that if the variable in the linear function is also known to be
   * a constant or linear function of another variable, then we want to 
   * collapse that into the value of the outer variable.
   *)
  datatype value =
      VConstant of Rat.t
    | VLf of linearFunction

  fun layoutValue v =
      case v
       of VConstant r => Rat.layout r
        | VLf lf => layoutLinearFunction lf

  (* The state for determining induction variables:
   *   vals: is the variable ultimately a constant or linear function
   *   ivs:  the induction variables of each loop
   *   bivs: a variable is a basic induction variable and the header, init
   *         value, and step constant
   *)
  datatype ivState = S of {
    vals : value VD.t ref,
    ivs  : inductionVariable List.t LD.t ref,
    bivs : {hdr : M.label, init : M.operand, step : Rat.t} VD.t ref
  }

  fun ivStateMk () = S {vals = ref VD.empty, ivs = ref LD.empty, bivs = ref VD.empty}

  fun getValue (S {vals, ...}, v) = VD.lookup (!vals, v)
  fun getValues (S {vals, ...}) = !vals

  fun addValue (S {vals, ...}, vr, vl) = vals := VD.insert (!vals, vr, vl)

  fun addInductionVariable (S {ivs, ...}, h, v, m, init, c, step) =
      let
        val ivl = Utils.Option.get (LD.lookup (!ivs, h), [])
        val ivl = (IV {variable = v, init = (m, init, c), step = step})::ivl
        val () = ivs := LD.insert (!ivs, h, ivl)
      in ()
      end

  fun getBasicInductionVariable (S {bivs, ...}, v) = VD.lookup (!bivs, v)

  fun addBasicInductionVariable (s as S {bivs, ...}, h, v, init, step) =
      let
        val () = addInductionVariable (s, h, v, Rat.one, init, Rat.zero, step)
        val () = bivs := VD.insert (!bivs, v, {hdr = h, init = init, step = step})
      in ()
      end

  fun ivStateFinish (S {ivs, ...}) = !ivs

  fun layoutValues (S {vals, ...}) =
      let
        fun doOne (v, vl) = L.seq [I.layoutVariable' v, L.str ": ", layoutValue vl]
        val l = L.align (List.map (VD.toList (!vals), doOne))
        val l = L.align [L.str "Values:", LU.indent l]
      in l
      end

  (* Figure out what a variable ultimately is.
   * For each variable:
   *   Figure out what the operands on its rhs ultimate are.
   *   Collapse that according to the operation.
   *)
  local
    open Rat
  in
  fun genValues (state, env, fmil, ls) =
      let
        val visited = ref VS.empty
        fun isVisited v = VS.member (!visited, v)
        fun addVisited v = visited := VS.insert (!visited, v)
        fun getVarValue v =
            if isVisited v then
              getValue (state, v)
            else
              let
                val v' = getVarValueA v
                val () = addVisited v
                val () =
                    case v'
                     of NONE => ()
                      | SOME v' => addValue (state, v, v')
              in v'
              end
        and getVarValueA v =
            case FMil.getVariable (fmil, v)
             of FMil.VdGlobal g       => getGlobalValue g
              | FMil.VdFunParam _     => NONE
              | FMil.VdLabParam _     => NONE
              | FMil.VdInstr (_, rhs) => getRhsValue rhs
              | FMil.VdRetVar _       => NONE
        and getGlobalValue g =
            case g
             of M.GRat r     => SOME (VConstant r)
              | M.GInteger i => SOME (VConstant (Rat.fromIntInf i))
              | M.GSimple s  => getOperandValue s
              | _            => NONE
        and getRhsValue rhs =
            case rhs
             of M.RhsSimple s => getOperandValue s
              | M.RhsPrim x   => getPrimValue x
              | _             => NONE
        and getPrimValue {prim, args, ...} =
            let
              fun unary f =
                  case getOperandValue (Vector.sub (args, 0))
                   of NONE   => NONE
                    | SOME v => f v
              fun binary f =
                  case (getOperandValue (Vector.sub (args, 0)), getOperandValue (Vector.sub (args, 1)))
                   of (SOME v1, SOME v2) => f (v1, v2)
                    | _                  => NONE
            in
              case prim
               of P.Prim (P.PNumArith (_, a)) =>
                  (case a
                    of P.APlus   => binary getPlusValue
                     | P.ANegate => unary  getNegateValue
                     | P.AMinus  => binary getMinusValue
                     | P.ATimes  => binary getTimesValue
                     | _ => NONE)
                | _ => NONE
            end
        and binaryLf (x1, x2, f) =
            if #var x1 = #var x2 then
              let
                val m = f (#m x1, #m x2)
                val c = f (#c x1, #c x2)
              in
                if equals (m, zero) then
                  SOME (VConstant c)
                else
                  SOME (VLf {var = #var x1, m = m, c = c})
              end
            else
              NONE
        and getPlusValue (v1, v2) =
            case (v1, v2)
             of (VConstant r1,    VConstant r2   ) => SOME (VConstant (r1 + r2))
              | (VConstant r1,    VLf {var, m, c}) => SOME (VLf {var = var, m = m, c = r1 + c})
              | (VLf {var, m, c}, VConstant r2   ) => SOME (VLf {var = var, m = m, c = c + r2})
              | (VLf  x1,         VLf x2         ) => binaryLf (x1, x2, Rat.+)
        and getNegateValue v =
            case v
             of VConstant r     => SOME (VConstant (~r))
              | VLf {var, m, c} => SOME (VLf {var = var, m = ~m, c = ~c})
        and getMinusValue (v1, v2) =
            case (v1, v2)
             of (VConstant r1,    VConstant r2   ) => SOME (VConstant (r1 - r2))
              | (VConstant r1,    VLf {var, m, c}) => SOME (VLf {var = var, m = ~m, c = r1 - c})
              | (VLf {var, m, c}, VConstant r2   ) => SOME (VLf {var = var, m = m,  c = c - r2})
              | (VLf x1,          VLf x2         ) => binaryLf (x1, x2, Rat.-)
        and getTimesValue (v1, v2) =
            case (v1, v2)
             of (VConstant r1,    VConstant r2   ) => SOME (VConstant (r1 * r2))
              | (VConstant r1,    VLf {var, m, c}) => SOME (VLf {var = var, m = r1 * m, c = r1 * c})
              | (VLf {var, m, c}, VConstant r2   ) => SOME (VLf {var = var, m = m * r2, c = c * r2})
              | (VLf _,           VLf _          ) => NONE
        and getOperandValue opnd =
            case opnd
             of M.SConstant c =>
                (case c
                  of M.CRat r      => SOME (VConstant (Rat.fromIntInf r))
                   | M.CInteger i  => SOME (VConstant (Rat.fromIntInf i))
                   | M.CIntegral i => SOME (VConstant (Rat.fromIntInf (IntArb.toIntInf i)))
                   | _             => NONE)
              | M.SVariable v =>
                (case getVarValue v
                  of NONE    => SOME (VLf {var = v, m = Rat.one, c = Rat.zero})
                   | SOME v' => SOME v')
        fun doInstruction i =
            case MU.Instruction.dest i
             of NONE => ()
              | SOME v => ignore (getVarValue v)
        fun doBlock (_, b) = Vector.foreach (MU.Block.instructions b, doInstruction)
        fun doLoop (L {blocks, ...}) = LD.foreach (blocks, doBlock)
        fun doLoopTree (Tree.T (l, ls)) =
            let
              val () = doLoop l
              val () = Vector.foreach (ls, doLoopTree)
            in ()
            end
        val () = LD.foreach (getBlocksNotInLoops ls, doBlock)
        val () = Vector.foreach (getLoops ls, doLoopTree)
      in ()
      end
  end

  (* Find basic induction variables.
   *   For each loop header and parameter of that header we analyse the
   *   incoming edges to see if:
   *     1) All entry edges have the same operand.
   *     2) All loop edges are the same lf of the parameter with multiplier 1
   *)

  (* IaUndertermined: we haven't seen an entry edge yet
   * IaOperand:       we've seen at least one entry edge and all of them have
   *                  the same operand passed in
   * IaUnknown:       there are at least two entry edges with different
   *                  operands
   * LaUndetermined:  we haven't seen a loop edge yet
   * LaIv:            we've seen at least one loop edge and all of them are
   *                  passing the parameter plus the constant in
   * LaUnknown:       the parameter is not an induction variable by the loop
   *                  edges
   *)
  datatype initAnalysis = IaUndetermined | IaOperand of M.operand | IaUnknown
  datatype loopAnalysis = LaUndetermined | LaIv of Rat.t | LaUnknown

  fun genBasicInductionVariables (state, env, cfg, ls) =
      let
        val LS {loops, ...} = ls
        fun doLoopTree lt =
            let
              val Tree.T (L {header, blocks, ...}, children) = lt
              val hb = Option.valOf (LD.lookup (blocks, header))
              val params = MU.Block.parameters hb
              val M.B {parameters, ...} = hb
              val allNodes = getAllNodes (ls, header)
              (* Initial analysis *)
              fun doOne p = (p, IaUndetermined, LaUndetermined)
              val a = Vector.map (params, doOne)
              (* Do analysis *)
              fun failed () = fail ("genBasicInductionVariables.doEdges", "bad transfer")
              fun doTarget (a, t, f1, f2) = Vector.map2 (a, MU.Target.arguments t, f1)
              fun doSwitch (a, s, f1, f2) =
                  let
                    val {cases, default, ...} = s
                    fun doOne ((_, t), a) = if MU.Target.block t = header then doTarget (a, t, f1, f2) else a
                    val a = Vector.fold (cases, a, doOne)
                    val a =
                        case default
                         of NONE => a
                          | SOME t =>
                            if MU.Target.block t = header
                            then doTarget (a, t, f1, f2)
                            else a
                  in a
                  end
              fun doEdges (a, b, f1, f2) =
                  case MU.Block.transfer b
                   of M.TGoto t      => doTarget (a, t, f1, f2)
                    | M.TCase s      => doSwitch (a, s, f1, f2)
                    | M.TInterProc _ => Vector.map (a, f2)
                    | M.TReturn _    => failed ()
                    | M.TPSumCase s  => doSwitch (a, s, f1, f2)
                    | M.TCut _       => Vector.map (a, f2)
              fun doInitUnknown (p, _, la) = (p, IaUnknown, la)
              fun doInitArg ((p, ia, la), arg) =
                  case ia
                   of IaUndetermined => (p, IaOperand arg, la)
                    | IaOperand opnd =>
                      if MU.Operand.compare (opnd, arg) = EQUAL
                      then (p, ia, la)
                      else (p, IaUnknown, la)
                    | IaUnknown => (p, ia, la)
              fun doLoopUnknown (p, ia, _) = (p, ia, LaUnknown)
              fun getValueO opnd =
                  case opnd
                   of M.SVariable v => getValue (state, v)
                    | _             => NONE
              fun doLoopArg ((p, ia, la), arg) =
                  case la
                   of LaUndetermined =>
                      (case getValueO arg
                        of NONE => (p, ia, LaUnknown)
                         | SOME (VConstant _) => (p, ia, LaUnknown)
                         | SOME (VLf {var, m, c}) =>
                           if Rat.equals (m, Rat.one) andalso var = p
                           then (p, ia, LaIv c)
                           else (p, ia, LaUnknown))
                    | LaIv step =>
                      (case getValueO arg
                        of NONE => (p, ia, LaUnknown)
                         | SOME (VConstant _) => (p, ia, LaUnknown)
                         | SOME (VLf {var, m, c}) =>
                           if Rat.equals (m, Rat.one) andalso var = p andalso Rat.equals (c, step)
                           then (p, ia, la)
                           else (p, ia, LaUnknown))
                    | LaUknown => (p, ia, la)
              fun unknown (p, _, _) = (p, IaUnknown, LaUnknown)
              fun doPred (pred, a) =
                  case Cfg.nodeGetLabelBlock (cfg, pred)
                   of NONE => Vector.map (a, unknown)
                    | SOME (pl, pb) =>
                      if LD.contains (allNodes, pl) then
                        doEdges (a, pb, doLoopArg, doLoopUnknown)
                      else
                        doEdges (a, pb, doInitArg, doInitUnknown)
              val preds = Cfg.pred (cfg, Cfg.labelGetNode (cfg, header))
              val a = List.fold (preds, a, doPred)
              (* Form basic induction variables based on analysis *)
              fun doOne (p, ia, la) =
                  case (ia, la)
                   of (IaOperand init, LaIv step) => addBasicInductionVariable (state, header, p, init, step)
                    | _ => ()
              val () = Vector.foreach (a, doOne)
              (* Do children *)
              val () = Vector.foreach (children, doLoopTree)
            in ()
            end
        val () = Vector.foreach (loops, doLoopTree)
      in ()
      end

  (* Compute induction variables that are linear functions of basic
   * induction variables.
   *)
  fun genDerivedInductionVariables (state, env) =
      let
        fun doOne (v, vl) =
            case vl
             of VLf {var, m, c} =>
                (case getBasicInductionVariable (state, var)
                  of NONE => ()
                   | SOME {hdr, init, step} => addInductionVariable (state, hdr, v, m, init, c, Rat.* (m, step)))
              | _ => ()
        val () = VD.foreach (getValues state, doOne)
      in ()
      end

  (* The actual induction variable determination *)
  fun genInductionVariables (ls, fmil, cfg) =
      let
        val state = ivStateMk ()
        val env = ls
        val () = genValues (state, env, fmil, ls)
        val () = genBasicInductionVariables (state, env, cfg, ls)
        val () = genDerivedInductionVariables (state, env)
        val ivs = ivStateFinish state
        val LS x = ls
        val r =
            LS {config = #config x, si = #si x, entry = #entry x, loops = #loops x,
                blocksNotInLoops = #blocksNotInLoops x, allNodes = #allNodes x, exits = #exits x,
                preheaders = #preheaders x, inductionVars = ivs, tripCounts = #tripCounts x}
      in r
      end

  fun getInductionVariables (LS {inductionVars, ...}, h) =
      case LD.lookup (inductionVars, h)
       of NONE     => fail ("getInductionVariables", "no induction variables for header: " ^ I.labelString h)
        | SOME ivs => ivs

  fun inductionVars (LS {inductionVars, ...})= inductionVars

  (*** Compute loop trip counts ***)

  (* allDefs: all variables defined in a loop
   * tcs: the trip counts
   *)
  datatype leState = S of {allDefs : VS.t LD.t ref, tcs : tripCount LD.t ref}

  fun tcStateMk () = S {allDefs = ref LD.empty, tcs = ref LD.empty}

  fun isDef (S {allDefs, ...}, h, v) = VS.member (Option.valOf (LD.lookup (!allDefs, h)), v)
  fun addDefs (S {allDefs, ...}, h, vs) = allDefs := LD.insert (!allDefs, h, vs)

  fun addTripCount (S {tcs, ...}, h, tc) = tcs := LD.insert (!tcs, h, tc)

  fun tcStateFinish (S {tcs, ...}) = !tcs

  fun genDefs (state, env, ls) =
      let
        fun doInstruction (M.I {dest, ...}, defs) = Option.fold (dest, defs, VS.insert o Utils.flip2)
        fun doBlock (l, b, defs) =
            let
              val M.B {parameters, instructions, ...} = b
              val defs = Vector.fold (parameters, defs, VS.insert o Utils.flip2)
              val defs = Vector.fold (instructions, defs, doInstruction)
            in defs
            end
        fun doLoop l =
            let
              val Tree.T (L {header, blocks, ...}, children) = l
              val myDefs = LD.fold (blocks, VS.empty, doBlock)
              val childDefs = Vector.map (children, doLoop)
              val defs = Vector.fold (childDefs, myDefs, VS.union)
              val () = addDefs (state, header, defs)
            in defs
            end
        fun doOne l = let val _ = doLoop l in () end
        val () = Vector.foreach (getLoops ls, doOne)
      in ()
      end

  (* An operand is:
   *   ALoopInvariant if it is loop invariant for the loop of interest
   *   AInductionVariable if it is an induction variable for the LOI
   *   AUnknown if one of the other two is not known to hold
   *)
  datatype leAnalysis =
      ALoopInvariant
    | AInductionVariable of (Rat.t * M.operand * Rat.t) * Rat.t
    | AUnknown

  fun analyseOperand (state, env, h, myIvs, opnd) =
      case opnd
       of M.SVariable v =>
          (case List.peek (myIvs, fn (IV {variable, ...}) => v = variable)
            of SOME (IV {init, step, ...}) => AInductionVariable (init, step)
             | NONE => if isDef (state, h, v) then AUnknown else ALoopInvariant)
        | M.SConstant _ => ALoopInvariant

  (* determine loop h's trip count if there is one *)
  fun loopTripCount (state, env, fmil, cfg, lDomInfo, h) =
      Try.exec
        (fn () =>
            let
              val myNodes = getAllNodes (env, h)
              (* Check for a single exit block *)
              val myExits = getExits (env, h)
              val myExit =
                  case LS.toList myExits
                   of [exit] => exit
                    | _ => Try.fail ()
              (* Check that exit block dominates back edges *)
              val myNode = Cfg.labelGetNode (cfg, h)
              val myPreds = Cfg.pred (cfg, myNode)
              fun prjOne n = Cfg.nodeGetLabel (cfg, n)
              val myPreds = List.keepAllMap (myPreds, prjOne)
              fun checkOne l =
                  not (LD.contains (myNodes, l)) orelse MilCfg.LabelDominance.dominates (lDomInfo, myExit, l)
              val () = Try.require (List.forall (myPreds, checkOne))
              (* Check for the transfer being an if and determine the true and
               * false branches
               *)
              val {on, trueBranch, falseBranch} =
                  Try.<- (MU.Transfer.isBoolIf (MU.Block.transfer (Option.valOf (LD.lookup (myNodes, myExit)))))
              val lt = MU.Target.block trueBranch
              val lf = MU.Target.block falseBranch
              (* Determine if the true or the false branch exits *)
              val tne = LD.contains (myNodes, lt)
              val fne = LD.contains (myNodes, lf)
              val flip1 =
                  if tne andalso not fne then true
                  else if not tne andalso fne then false
                  else fail ("loopTripCount", "bad exit")
              (* Figure out what comparison the branch is on *)
              val v =
                  case on
                   of M.SConstant _ => Try.fail ()
                    | M.SVariable v => v
              val (cmp, o1, o2) =
                  case FMil.getVariable (fmil, v)
                   of FMil.VdInstr (_, M.RhsPrim {prim = P.Prim (P.PNumCompare (_, cmp)), args, ...}) =>
                      (cmp, Vector.sub (args, 0), Vector.sub (args, 1))
                    | _ => Try.fail ()
              (* Determine if the comparison is between a loop invariant
               * operand and an induction variable and which is which
               *)
              val myIvs = getInductionVariables (env, h)
              val ((init, step), bnd, flip2) =
                  case (analyseOperand (state, env, h, myIvs, o1),
                        analyseOperand (state, env, h, myIvs, o2))
                   of (ALoopInvariant, AInductionVariable iv) =>
                      (iv, o1, true)
                    | (AInductionVariable iv, ALoopInvariant) =>
                      (iv, o2, false)
                    | _ => Try.fail ()
              (* Form and record trip count *)
              val tc = TC {block = myExit, cond = v, flip1 = flip1, comparison = cmp, flip2 = flip2, init = init,
                           step = step, bound = bnd}
              val () = addTripCount (state, h, tc)
            in ()
            end)

  fun genTripCounts (ls, fmil, cfg, dt) =
      let
        val labelDomTree = Tree.map (dt, fn (l,b) => l)
        val labelDominanceInfo = MilCfg.LabelDominance.new labelDomTree
        val state = tcStateMk ()
        val env = ls
        val () = genDefs (state, env, ls)
        fun doLoop l =
            let
              val Tree.T (L {header, ...}, children) = l
              val () = loopTripCount (state, env, fmil, cfg, labelDominanceInfo, header)
              val () = Vector.foreach (children, doLoop)
            in ()
            end
        val () = Vector.foreach (getLoops ls, doLoop)
        val tcs = tcStateFinish state
        val LS x = ls
        val r =
            LS {config = #config x, si = #si x, entry = #entry x, loops = #loops x,
                blocksNotInLoops = #blocksNotInLoops x, allNodes = #allNodes x, exits = #exits x,
                preheaders = #preheaders x, inductionVars = #inductionVars x, tripCounts = tcs}
      in r
      end

  fun getTripCount (LS {tripCounts, ...}, h) = LD.lookup (tripCounts, h)
  fun allTripCounts (LS {tripCounts, ...}) = tripCounts

end;
