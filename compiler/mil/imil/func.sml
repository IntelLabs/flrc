(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature IMIL_FUNC = 
sig
  include IMIL_PUBLIC_TYPES

  val getFName : t * iFunc -> Mil.variable 
  val getCallConv : t * iFunc -> Mil.variable Mil.callConv
  val getSize  : t * iFunc -> int  (* number of instructions *)
  val getEscapes : t * iFunc -> bool
  val getRecursive : t * iFunc -> bool
  val getEffects : t * iFunc -> Mil.effects
  val getArgs : t * iFunc -> Mil.variable Vector.t
  val getRtyps : t * iFunc -> Mil.typ Vector.t
  val getIFuncByName' : t * Mil.variable -> iFunc option
  val getIFuncByName : t * Mil.variable -> iFunc
  val getIFuncs  : t -> (Mil.variable * iFunc) list 
  val getUses  : t * iFunc -> use Vector.t
  val getUsedBy : t * iFunc -> item Vector.t
  val getDfsTree' : t * iBlock -> iBlock Tree.t
  val getDfsTree : t * iFunc -> iBlock Tree.t
  val getDomTree' : t * iBlock -> iBlock Tree.t
  val getDomTree  : t * iFunc -> iBlock Tree.t
  val getBlockByLabel : t * iFunc * Mil.label -> iBlock
  val getExits : t * iFunc -> iBlock list
  val getStart : t * iFunc -> iBlock
  val getBlocks : t * iFunc -> iBlock list
  val freeVars  : t * iFunc -> Identifier.VariableSet.t
  val freeVars' : t * iFunc -> Mil.variable list
  val unreachable' : t * iBlock -> iBlock list
  val unreachable  : t * iFunc -> iBlock list
  val setCallConv : t * iFunc * Mil.variable Mil.callConv -> unit
  val markNonEscaping : t * iFunc -> unit
  val markEscaping : t * iFunc -> unit
  val markNonRecursive : t * iFunc -> unit
  val setArgs : t * iFunc * Mil.variable Vector.t -> unit
  val setRtyps : t * iFunc * Mil.typ Vector.t -> unit
  val isProgramEntry : t * iFunc -> bool
  val splitCriticalEdges : t * iFunc -> unit
  (* inline (t, v, c) = ls
   * v is the function name,
   * c is the call
   * ls is the list of affected/new instructions (the label
   * on the entry block, and the labels of the return blocks)
   *)
  val inline     : t * Mil.variable * iInstr -> iInstr list
  val inlineCopy : t * Mil.variable * iInstr -> iInstr list
  (* inlineMap inlines a copy of the callee and call the mapping
   * functions on every pair of (original, copy) block/instruction. *)
  val inlineMap  : t * Mil.variable * iInstr *
                   (iBlock * iBlock -> unit) option *
                   (iInstr * iInstr -> unit) option -> iInstr list
  val copy : t * iFunc -> (Mil.variable * iFunc)
  val duplicateBlocks : t * iFunc * (label * iBlock) list *
                        (iBlock * iBlock -> unit) option -> unit
  val renameBlock : t * iFunc * iBlock * Mil.variable * Mil.variable -> unit
  val delete : t * iFunc -> unit
  val new  : t * Mil.variable * Mil.code -> iFunc
  val unBuild : iFunc -> Mil.variable * Mil.global
  val layout : t * iFunc -> Layout.t
  (* Layout in dot format. *)
  val layoutDot : t * iFunc -> Layout.t 
end

structure IMilFunc :
sig 
  include IMIL_FUNC 
  val checkAll : t -> unit
end
  = 
struct
  open IMilPublicTypes

  val fail = 
   fn (f, s) => Fail.fail ("func.sml", f, s)

  structure M = Mil
  structure MU = MilUtils
  structure L = Layout
  structure LU = LayoutUtils
  structure IMT = IMilTypes
  structure IVD = IMT.IVD
  structure ILD = IMT.ILD
  structure LD = IMT.LD
  structure VS = IMT.VS
  structure LS = IMT.LS
  structure FV = IMilCommon.FV
  structure VLRN = IMilCommon.VLRN
  structure Def = IMilDef
  structure Var = IMilVar
  structure Use = IMilUse
  structure IML = IMilLayout
  structure Graph = IMT.Graph
  structure Instr = IMilInstr
  structure Block = IMilBlock
  structure Enumerate = IMilEnumerate

  type t = IMT.t
  type iFunc = IMT.iFunc
  type iBlock = IMT.iBlock
  type iInstr = IMT.iInstr
  type use = IMT.use
  type item = IMT.item

  val getId =
   fn (p, c) => IMT.iFuncGetId c
  val getFName =
   fn (p, c) => IMT.iFuncGetFName c
  val getCallConv =
   fn (p, c) => IMT.iFuncGetConv c
  val getSize =
   fn (p, c) => !(IMT.iFuncGetSize c)
  val getEffects =
   fn (p, c) => IMT.iFuncGetEffects c
  val getEscapes =
   fn (p, c) => IMT.iFuncGetEscapes c
  val getRecursive =
   fn (p, c) => IMT.iFuncGetRecursive c
  val getConv =
   fn (p, c) => IMT.iFuncGetConv c
  val getArgs =
   fn (p, c) => IMT.iFuncGetArgs c
  val getRtyps =
   fn (p, c) => IMT.iFuncGetRtyps c
  val getEntry =
   fn (p, c) => IMT.iFuncGetEntry c
  val getExit =
   fn (p, c) => IMT.iFuncGetExit c
  val getIBlocks =
   fn (p, c) => IMT.iFuncGetIBlocks c
  val getCfg =
   fn (p, c) => IMT.iFuncGetCfg c

  val getExits =
   fn (p, c) => IMT.nodeGetPredIBlocks (getExit (p, c))

  val getIFuncs =
   fn p => IVD.toList (IMT.tGetIFuncs p)

  val getBlockByLabel =
   fn (p, iFunc, l) => IMT.iFuncGetIBlockByLabel (iFunc, l)

  val addArgDefs =
   fn (p, iFunc, c, args) => 
      let
        val def = IMT.DefParameter iFunc
        val () = MilUtils.CallConv.foreach (c, fn v => Def.add (p, v, def))
        val () = Vector.foreach (args, fn v => Def.add (p, v, def))
      in ()
      end


  val getUses =
   fn (p, c) => 
      Use.getUses (p, getFName (p, c))

  val freeVars =
   fn (p, c) => 
      let
        val instrs = Enumerate.IFunc.instructions (p, c)
        val dovars = fn (v, _, s) => if Var.kind (p, v) = M.VkLocal then s else VS.insert (s, v)
        val folder = fn (i, s) => IVD.fold (IMT.iInstrGetVars i, s, dovars)
        val fvs = List.fold (instrs, VS.empty, folder)
      in fvs
      end

  val freeVars' =
   fn (p, c) =>
      let
        val s = freeVars (p, c)
        val l = VS.toList s
      in l
      end

  val getUsedBy =
   fn (p, c) => 
      let
        val fvs = freeVars' (p, c)
        val defs = Vector.fromListMap (fvs, fn v => Def.get (p, v))
        val items = Def.defsToItems (p, defs)
      in items
      end

  val getStart =
   fn (p, c) => 
      let
        val entry = IMT.iFuncGetEntry c
        val succs = IMT.nodeGetSuccIBlocks entry
        val start = 
            (case succs
              of [start] => start
               | _ => fail ("getStart",
                            "Multiple or zero entry CFG!"))
      in start
      end

  val getBlocks =
   fn (p, c) => Enumerate.IFunc.blocks (p, c)

  val blockTree  = 
   fn (p, c, t) => 
      let
        val t = 
            let
              val entry = IMT.iFuncGetEntry c
              val Tree.T (first, succs) = t
              val t = 
                  if first = entry then
                    if Vector.length succs = 1 then
                      Vector.sub (succs, 0)
                    else
                      fail ("blockTree",
                            "Multiple or zero entry CFG!")
                  else 
                    t
            in t
            end

        val graph = IMT.iFuncGetCfg c
        val rec doOptTree = 
         fn Tree.T (n, nv) => 
            Option.map
              (Graph.Node.getLabel (n),
               (fn a => Tree.T (a, Vector.keepAllMap (nv, doOptTree))))

        val t = 
            Utils.Option.out
              (doOptTree t,
               (fn () => Fail.fail ("cfg.sml",
                                    "labeledTree",
                                    "Unexpected Exit/Entry node")))
      in t
      end

  val getDomTree'' =
   fn (p, 
       c, 
       n) => 
      let
        val graph = IMT.iFuncGetCfg c
        val t = Graph.domTree (graph, n)
      in t
      end
  val getDomTree' =
   fn (p, b) => 
      let
        val n = IMT.iBlockGetNode b
        val c = IMT.iBlockGetIFunc b
        val t = getDomTree'' (p, c, n)
        val t = blockTree (p, c, t)
      in t
      end
  val getDomTree =
   fn  (p, c ) => 
       let
         val s = getStart (p, c)
         val t = getDomTree' (p, s)
       in t
       end

  val getDfsTree'' =
   fn (p, c, n) => 
      let
        val graph = IMT.iFuncGetCfg c
        val t = Graph.dfsTree (graph, n)
      in t
      end
  val getDfsTree' =
   fn (p, b) => 
      let
        val n = IMT.iBlockGetNode b
        val c = IMT.iBlockGetIFunc b
        val t = getDfsTree'' (p, c, n)
        val t = blockTree (p, c, t)
      in t
      end
  val getDfsTree =
   fn  (p, c ) => 
       let
         val s = getStart (p, c)
         val t = getDfsTree' (p, s)
       in t
       end

  val blockList = 
   fn (p, c, l) => List.keepAllMap (l, Graph.Node.getLabel)

  val unreachable'' =
   fn (p, 
       c, 
       n) => 
      let
        val graph = IMT.iFuncGetCfg c
        val l = Graph.unreachable (graph, n)
      in l
      end
  val unreachable' =
   fn (p, b) => 
      let
        val n = IMT.iBlockGetNode b
        val c = IMT.iBlockGetIFunc b
        val l = unreachable'' (p, c, n)
        val l = blockList (p, c, l)
        val l = List.removeAll (l, fn b => Block.isDead (p, b))
      in l
      end
  val unreachable =
   fn  (p, c) => 
       let
         val s = getStart (p, c)
         val l = unreachable' (p, s)
       in l
       end

  val setCallConv =
   fn (p, c, cc) =>
      let

        val () = MilUtils.CallConv.foreach (IMT.iFuncGetConv c, fn v => Def.add (p, v, IMT.DefUnk))
        val () = MilUtils.CallConv.foreach (cc, fn v => Def.add (p, v, IMT.DefParameter c))
        val () = IMT.iFuncSetConv (c, cc)
      in ()
      end

  val markNonEscaping =
   fn (p, c) => IMT.iFuncSetEscapes (c, false)

  val markEscaping =
   fn (p, c) => IMT.iFuncSetEscapes (c, true)

  val markNonRecursive =
   fn (p, c) => IMT.iFuncSetRecursive (c, false)

  val setArgs =
   fn (p, c, nargs) =>
      let
        val oargs = IMT.iFuncGetArgs c
        val () = Vector.foreach (oargs, fn v => Def.add (p, v, IMT.DefUnk))
        val def = IMT.DefParameter c
        val () = Vector.foreach (nargs, fn v => Def.add (p, v, def))
        val () = IMT.iFuncSetArgs (c, nargs)
      in ()
      end

  val setRtyps =
   fn (p, c, rtyps) => IMT.iFuncSetRtyps (c, rtyps)

  val isProgramEntry =
   fn (p, c) => 
      (IMT.iFuncGetFName c) = (IMT.tGetEntry p)

  val delete =
   fn (p, c) => 
      let
        val fname = IMT.iFuncGetFName c
        val funs = IMT.tGetIFuncs p
        val () = 
            if IVD.contains (funs, fname) then 
              let
                val () = ILD.foreach (IMT.iFuncGetIBlocks c, 
                                   fn (l, b) => Block.delete (p, b))
                val () = IVD.remove (funs, fname)
              in ()
              end
            else ()
        val () = IVD.remove (IMT.tGetDefs p, fname)
      in ()
      end

  val new =
   fn  (p, v, m) => 
       let 
         val M.F {
             fx,
             escapes, 
             recursive,
             cc,
             args,
             rtyps,
             body = M.CB {entry = milEntry, blocks = milGraph}
         } = m

         val body = Graph.new ()
         val entry   = Graph.newNode (body, NONE)
         val exit    = Graph.newNode (body, NONE)
         val id = IMT.nextId p
         val c' = 
             IMT.F {id        = id,
                    fname     = v,
                    size      = ref 0,
                    effects   = fx,
                    escapes   = escapes,
                    recursive = recursive,
                    conv      = cc,
                    args      = args,
                    rtyps     = rtyps,
                    entry     = entry,
                    exit      = exit,
                    start     = milEntry,
                    iBlocks   = ILD.empty (),
                    cfg       = body}
         val c = IMT.iFuncNew c'

         val bbs = LD.map (milGraph, 
                        fn (l, mb) => (mb,
                                       Block.new (p, c, l)))
         val () = LD.foreach (bbs,
                           fn (l, (mb, b)) => 
                              Block.init (p, b, (l, mb)))
                             
                             (* XXX We should be defensive against unreachable code here - leaf *)
         val () = IVD.insert (IMT.tGetIFuncs p, v, c)
         val () = IVD.insert (IMT.tGetDefs p, v, IMT.DefFunc c)
         val () = addArgDefs (p, c, cc, args)
       in c
       end

  val unBuild =
   fn iFunc => 
      let
        val iFunc' = IMT.iFuncGetIFunc' iFunc
        val IMT.F {id, 
                   fname,
                   size,
                   effects,
                   escapes,
                   recursive,
                   conv,
                   args, 
                   rtyps,
                   entry,
                   exit,
                   start,
                   iBlocks,
                   cfg} = iFunc'

        val nodes = Graph.nodes cfg
        val help =
         fn n => 
            if n <> entry andalso n <> exit then
              Block.unBuild (valOf (Graph.Node.getLabel n))
            else 
              NONE

        val entries = List.keepAllMap (nodes, help)

        val milBlocks = LD.fromList entries
        val milGraph = M.CB {entry = start, blocks = milBlocks}
        val m = M.F {fx = effects,
                     escapes = escapes,
                     recursive = recursive,
                     cc = conv,
                     args = args,
                     rtyps = rtyps,
                     body = milGraph}
      in (fname, M.GCode m)
      end

  val splitCriticalEdges = 
   fn (imil, ifunc) =>
      let
        val splitBlockCE =
         fn b =>
            let
              val isCritical =
               fn (a, b) =>
                  (case (IMilBlock.outEdges (imil, a), IMilBlock.inEdges (imil, b))
                    of (_::_::_, _::_::_) => true (* length of both > 1*)
                     | _                  => false)
                        
              val findInCE = fn b => List.keepAll (IMilBlock.inEdges(imil, b), isCritical)

              val splitEdge = fn e => ignore(IMilBlock.splitEdge (imil, e))
                              
            in
              List.foreach (findInCE b, splitEdge)
            end
      in 
        List.foreach(getBlocks(imil, ifunc), splitBlockCE)
      end

  local

    val convBound =
     fn c => 
        (case c
          of M.CcCode => []
           | M.CcClosure {cls, fvs} => cls::(Vector.toList fvs)
           | M.CcThunk {thunk, fvs} =>  thunk::(Vector.toList fvs))

    val rename =
     fn (p, r, v) => 
        let
          val v' = Var.clone (p, v)
          val r = Rename.renameTo (r, v, v')
        in r
        end
        
    val renameAll =
     fn (p, r, vs) => 
        List.fold (vs, r, fn (v, r) => rename (p, r, v))

    val buildRename =
     fn (p, c) => 
        let
          val instrs = Enumerate.IFunc.instructions (p, c)
          val r = Rename.none
          val bound = fn i => Vector.toList (Instr.variablesDefined (p, i))
          val help = fn (i, r) => renameAll (p, r, bound i)
          val r = List.fold (instrs, r, help)
          val r = renameAll (p, r, convBound (IMT.iFuncGetConv c))
          val r = renameAll (p, r, Vector.toList (IMT.iFuncGetArgs c))
          val lbls = ILD.fold (IMT.iFuncGetIBlocks c, [], fn (l, _, ll) => l::ll)
          val lbls = List.map (lbls, fn l => (l, Var.labelFresh p))
          val b = LD.fromList lbls
        in (r, b)
        end

    val buildRenameForBlocks =
     fn (p, c, blocks) =>
        let
          val instrs = List.fold (blocks, [], fn ((l, b), ll) => Enumerate.IBlock.instructions (p, b)@ll) 
          val r = Rename.none
          val bound = fn i => Vector.toList (Instr.variablesDefined (p, i))
          val help = fn (i, r) => renameAll (p, r, bound i)
          val r = List.fold (instrs, r, help)
          val b = LD.fromList (List.map (blocks, fn (l, _) => (l, Var.labelFresh p)))
        in (r, b)
        end

    val renameVar =
     fn ((rn, _), v) => Rename.use (rn, v)

    val renameLabel =
     fn ((_, ld), l) => 
        (case LD.lookup (ld, l)
          of SOME l => l
           | NONE => fail ("renameLabel",
                           "Unknown label!"))

    val renameConv =
     fn (r, c) => MilUtils.CallConv.map (c, fn v => renameVar (r, v))

    val renameArgs =
     fn (r, vs) => Vector.map (vs, fn v => renameVar (r, v))

    val renameMilInstr =
     fn (p, r, mi) => 
        let
          val mi = 
              (case mi
                of IMT.MInstr mi => 
                   IMT.MInstr (VLRN.instruction (IMT.tGetConfig p, r, mi))
                 | IMT.MLabel (l, args) => 
                   let
                     val args = Vector.map (args,
                                         fn v => renameVar (r, v))
                     val l = renameLabel (r, l)
                     val lbl = IMT.MLabel (l, args)
                   in lbl
                   end
                 | IMT.MTransfer t => 
                   IMT.MTransfer (VLRN.transfer (IMT.tGetConfig p, r, t))
                 | IMT.MDead => IMT.MDead)
        in mi
        end

    val copyInstr =
     fn (p, r, i) => 
        let
          val mi = Instr.getMil (p, i)
          val mi = renameMilInstr (p, r, mi)
          val i = Instr.new' (p, mi)
        in i
        end

    val copyBlock =
     fn (p, cfg, r, oldb, newb, mapInstr') =>
        let
          val IMT.B {label, code, trans, ...} = IMT.iBlockGetIBlock' oldb
          val mapInstr =
           fn (org, new) => case mapInstr'
                             of SOME map => map (org, new)
                              | NONE => ()
          val tmi = renameMilInstr (p, r, Instr.getMil (p, trans))
          val lmi = renameMilInstr (p, r, Instr.getMil (p, label))
          val () = Block.replaceTransfer' (p, newb, tmi)
          val () = mapInstr (IMT.iBlockGetTrans oldb, 
                             IMT.iBlockGetTrans newb)
          val () = Block.replaceLabel' (p, newb, lmi)
          val move =
           fn orgi =>
              let
                val newi = copyInstr (p, r, orgi)
                val () = mapInstr (orgi, newi)
                val () = Block.append' (p, newb, newi)
              in ()
              end
          val () = DList.foreach (code, move)
        in ()
        end

    val copyBlocks =
     fn (p, 
         cfg, 
         r,
         blocks,
         mapBlk,
         mapInstr) =>
        let
          val lbs = ILD.toList blocks
          val createNewBlock =
           fn (l, b) =>
              let
                val l = renameLabel (r, l)
              in 
                (l, b, Block.new (p, cfg, l))
              end
          val copyOne =
           fn (l, ob, nb) =>
              let
                val () = copyBlock (p, cfg, r, ob, nb, mapInstr)
                (* Check if the block has a valid label. If so, map it *)
                val map =
                 fn (b1, b2) => case mapBlk
                                 of SOME map => map (b1, b2)
                                  | NONE => ()
                val () = case IMT.iInstrGetMil (IMT.iBlockGetLabel ob)
                          of IMT.MLabel _ => map (ob, nb)
                           | _ => ()
              in  ()
              end
          val lbbs = List.map (lbs, createNewBlock)
          val ()  = List.foreach (lbbs, copyOne)
        in ()
        end



  in
  val copy' =
   fn (p, c, mapBlk, mapInstr) =>
      let 
        val r = buildRename (p, c)

        val IMT.F {id   = id,
                   fname = fname1,
                   size = size1,
                   effects,
                   escapes,
                   recursive,
                   conv = conv1,
                   args = args1, 
                   rtyps,
                   entry,
                   exit,
                   start,
                   iBlocks,
                   cfg = body1} = IMT.iFuncGetIFunc' c

        val fname = Var.clone (p, fname1)
        val conv = renameConv (r, conv1)
        val args = renameArgs (r, args1)
        val cfg = new (p, fname, 
                       M.F {
                       fx        = effects,
                       escapes   = escapes,
                       recursive = recursive,
                       cc        = conv,
                       args      = args,
                       rtyps     = rtyps,
                       body      = M.CB {entry = renameLabel (r, start),
                                         blocks =  LD.empty}
                      })
                  
        val () = copyBlocks (p, cfg, r, iBlocks, mapBlk, mapInstr)

        val () = IVD.insert (IMT.tGetIFuncs p, fname, cfg)
        val () = IVD.insert (IMT.tGetDefs p, fname, IMT.DefFunc cfg)
      in (fname, cfg)
      end

  val duplicateBlocks =
   fn (p,
       cfg,
       blocks,
       mapBlks) =>
      let
        val r = buildRenameForBlocks (p, cfg, blocks)
        val () = copyBlocks (p, cfg, r, ILD.fromList blocks, mapBlks, NONE)
      in ()
      end

  end

  local
    val rename =
     fn (p, r, v, v') => 
        let
          val r = Rename.renameTo (r, v, v')
        in r
        end

    val renameVar =
     fn ((rn, _), v) => Rename.use (rn, v)

    val renameAll =
     fn (p, r, vs) => 
        List.fold (vs, r, fn ((v, v'), r) => rename (p, r, v, v'))

    val renameMilInstr =
     fn (p, r, mi) => 
        let
          val mi = 
              (case mi
                of IMT.MInstr mi => 
                   IMT.MInstr (VLRN.instruction (IMT.tGetConfig p, r, mi))
                 | IMT.MLabel (l, args) => 
                   let
                     val args = Vector.map (args,
                                         fn v => renameVar (r, v))
                     val lbl = IMT.MLabel (l, args)
                   in lbl
                   end
                 | IMT.MTransfer t => 
                   IMT.MTransfer (VLRN.transfer (IMT.tGetConfig p, r, t))
                 | IMT.MDead => IMT.MDead)
        in mi
        end

    val buildRenameForBlock =
     fn (p, c, b, v, v') =>
        let
          val (l, _) = Block.getLabel' (p, b)
          val r = rename (p, Rename.none, v, v')
        in (r, LD.fromList [])
        end
  in
    val renameBlock =
     fn (p, 
         c,
         b,
         v,
         v') =>
        let
          val r = buildRenameForBlock (p, c, b, v, v')
          val IMT.B {label, code, trans, ...} = IMT.iBlockGetIBlock' b 
          val tmi = renameMilInstr (p, r, Instr.getMil (p, trans))
          val lmi = renameMilInstr (p, r, Instr.getMil (p, label))
          val () = Block.replaceTransfer' (p, b, tmi)
          val () = Block.replaceLabel' (p, b, lmi)
          val move =
           fn i =>
              let
                val mi = Instr.getMil (p, i)
                val mi = renameMilInstr (p, r, mi)
                val () = Instr.replaceMil (p, i, mi)
              in ()
              end
          val () = DList.foreach (code, move)
        in ()
        end
  end

  local
    val moveBlocks =
     fn (p, 
         caller, 
         callee) => 
        let

          val callerG = IMT.iFuncGetCfg caller
          val calleeG = IMT.iFuncGetCfg callee
          val blocks = Enumerate.IFunc.blocks (p, callee)
          val help =
           fn b =>
              let
                val callerNode = Graph.newNode (callerG, SOME b)
                val () = IMT.iBlockSetNode (b, callerNode)
                val () = IMT.iBlockSetIFunc (b, caller)
              in ()
              end

          val () = List.foreach (blocks, help)
          val () = ILD.add (IMT.iFuncGetIBlocks caller, IMT.iFuncGetIBlocks callee)

        in blocks
        end

    val doTransfer =
     fn (p, caller, callRet, i) =>
        let
          val is = 
              case Instr.toTransfer i
               of SOME t => 
                  let
                    val (is, t) = 
                        (case (callRet, t)
                          of (M.RNormal {block, ...}, M.TReturn args) =>
                             let
                               val t = M.TGoto (M.T {block = block, arguments = args})
                             in ([], t)
                             end
                           | (M.RTail _, _)     => ([], t)
                           | (_, M.TGoto _)     => ([], t)
                           | (_, M.TCase _)     => ([], t)
                           | (_, M.THalt _)     => ([], t)
                           | (_, M.TPSumCase _) => ([], t)
                           | (M.RNormal {cuts = callCuts, ...}, 
                              M.TInterProc {callee, ret = M.RNormal {rets, block, cuts}, fx})  =>
                             let 
                               val ret = M.RNormal {rets = rets, 
                                                    block = block, 
                                                    cuts = MilUtils.Cuts.inlineCall (cuts, callCuts)}
                               val t =  M.TInterProc {callee = callee, ret = ret, fx = fx}
                             in ([], t)
                             end
                           | (M.RNormal {rets, block, cuts = callCuts}, 
                              M.TInterProc {callee, ret = M.RTail {exits}, fx})  =>
                             let
                               val rets = Vector.map (rets, fn v => Var.clone (p, v))
                               val args = Vector.map (rets, M.SVariable)
                               val l = Var.labelFresh p
                               val b = M.B {parameters = Vector.new0(),
                                            instructions = Vector.new0(),
                                            transfer = M.TGoto (M.T {block = block, arguments = args})}
                               val b = Block.build (p, caller, (l, b))
                               val cuts = if exits then callCuts else MU.Cuts.none
                               val ret = M.RNormal {rets = rets, block = l, cuts = cuts}
                               val t = M.TInterProc {callee = callee, ret = ret, fx = fx}
                               val is = Enumerate.IBlock.instructions (p, b)
                             in (is, t)
                             end
                           | (M.RNormal {cuts = callCuts, ...}, M.TCut {cont, args, cuts}) => 
                             let
                               val cuts = MilUtils.Cuts.inlineCall (cuts, callCuts)
                               val t = M.TCut {cont = cont, args = args, cuts = cuts}
                             in ([], t)
                             end)
                    val () = Instr.replaceMil (p, i, IMT.MTransfer t)
                  in i::is
                  end
                | NONE => []
        in is
        end

    val doTransfers =
     fn (p, caller, blocks, ret) => 
        let
          val is = List.concatMap (blocks, 
                                fn b => doTransfer (p,
                                                    caller,
                                                    ret,
                                                    IMT.iBlockGetTrans b))
        in is
        end

    val decomposeCall =
     fn (p, call, callee) => 
        let
          val calleeConv = IMT.iFuncGetConv callee
          val formals = IMT.iFuncGetArgs callee
          val mkMv = 
           fn (v, a) => MU.Instruction.new (v, M.RhsSimple a)
          val mkMvs = 
              fn (formals, actuals) => 
                 Vector.map2 (formals, actuals, mkMv)
          val t = case Instr.toTransfer call
                   of SOME t => t
                    | NONE => fail ("decomposeCall", "dead call")
          val (pis, ret) = 
              (case t
                of M.TInterProc {callee = M.IpCall {call, args}, ret, fx} => 
                   let
                     val mvs = mkMvs (formals, args)
                   in
                     case (MilUtils.Call.cls call, calleeConv)
                      of (NONE, M.CcCode) => (mvs, ret)
                       | (SOME clsA, M.CcClosure {cls = clsF, fvs}) => 
                         let
                           val mv = mkMv (clsF, M.SVariable clsA)
                           val fks = Vector.map (fvs, fn v => Var.fieldKind (p, v))
                           val proj = 
                            fn (i, a) => MU.Instruction.new (a, M.RhsClosureGetFv {fvs = fks, cls = clsA, idx = i})
                           val projs = Vector.mapi (fvs, proj)
                         in (Vector.concat [Vector.new1 mv, projs, mvs], ret)
                         end
                       | _ => fail ("decomposeCall", "Mismatched call calling convention")
                   end
                 | M.TInterProc {callee = M.IpEval {eval, typ}, ret, fx} => 
                   (case (MilUtils.Eval.thunk eval, calleeConv)
                     of (thunkA, M.CcThunk {thunk = thunkF, fvs}) => 
                        let
                          val mv = mkMv (thunkF, M.SVariable thunkA)
                          val fks = Vector.map (fvs, fn v => Var.fieldKind (p, v))
                          val fk = Var.fieldKind (p, thunkF)
                          val proj = 
                           fn (i, a) => 
                              MU.Instruction.new (a, 
                                                  M.RhsThunkGetFv {fvs = fks, thunk = thunkA, idx = i, typ = fk})
                          val projs = Vector.mapi (fvs, proj)
                        in (Vector.concat [Vector.new1 mv, projs], ret)
                        end
                       | _ => fail ("decomposeCall", "Mismatched eval calling convention"))
                 | _ => fail ("decomposeCall", "Not a call"))
          val pis = Vector.toListMap (pis, fn i => Instr.new' (p, IMT.MInstr i))
        in (pis, ret)
        end

    val doEntry =
     fn (p, call, callee, prelude) => 
        let
          val (startL, _) = Block.getLabel' (p, getStart (p, callee))
          val t = IMT.MTransfer (M.TGoto (M.T {block = startL,
                                               arguments = Vector.new0()}))
          val () = Instr.replaceMil (p, call, t)
          val () = List.foreach (prelude, fn i => Instr.insertBefore' (p, i, call))
        in ()
        end

    val doRet = 
     fn (p, caller, ret) =>
        (case ret
          of M.RTail _ => (ret, [])
           | M.RNormal {rets, block, cuts} => 
             let
               val l = Var.labelFresh p
               val b = M.B {parameters = rets,
                            instructions = Vector.new0(),
                            transfer = M.TGoto (M.T {block = block, arguments = Vector.new0()})}
               val b = Block.build (p, caller, (l, b))
               val ret = M.RNormal {rets = rets, 
                                    block = l,
                                    cuts = cuts}
             in (ret, Enumerate.IBlock.instructions (p, b))
             end)
        
    val inlineCFG =
     fn (p, callee, call) => 
        let
          val callB = Instr.getIBlock (p, call)
          val caller = IMT.iBlockGetIFunc callB
          val calleeSize = !(IMT.iFuncGetSize callee)
                            (* Get the affected blocks before rewriting! *)
          val ls = List.map (IMT.iBlockGetSuccIBlocks callB, IMT.iBlockGetLabel)
          val (preludeInstructions, ret) = 
              decomposeCall (p, call, callee)
          val blocks = moveBlocks (p, caller, callee)
          val () = doEntry (p, call, callee, preludeInstructions)
          val (ret, phiInstructions) = doRet (p, caller, ret)
          val newTransferInstructions = doTransfers (p, caller, blocks, ret)
          val () = IMT.iFuncAddToSize (caller, calleeSize)
        in call :: (List.concat [ls, preludeInstructions, phiInstructions, newTransferInstructions])
        end
        
    val inline' = 
     fn (p, fname, call, copy) =>
        let
          val callee = case IVD.lookup (IMT.tGetIFuncs p, fname)
                        of SOME cfg => cfg
                         | NONE     => fail ("inline'", "Bad function name:" ^ L.toString (IML.var (p, fname)))
          val (fname, callee) = copy (p, callee)
          val ls = inlineCFG (p, callee, call)
          val () = IVD.remove (IMT.tGetIFuncs p, fname)
          val () = IVD.remove (IMT.tGetDefs p, fname)
        in 
          ls
        end
        
  in
  
  val copy = 
   fn (p, cfg) => copy' (p, cfg, NONE, NONE)
                  
  val inline =
   fn (p, fname, call) => 
      inline' (p, fname, call, fn (p, cfg) => (fname, cfg))
      
  val inlineCopy =
   fn (p, fname, call) => inline' (p, fname, call, copy)
                          
  val inlineMap =
   fn (p, fname, call, mapBlk, mapInstr) => 
      let
        val copyAndMap =
         fn (imil, cfg) => copy' (p, cfg, mapBlk, mapInstr)
      in
        inline' (p, fname, call, copyAndMap)
      end
  end
      
  val layout =
   fn (p, c) => 
      let
        val l = 
            L.seq [L.str "CFG ", IML.var (p, getFName (p, c))]
        val vars = VS.layout (freeVars (p, c), fn v => IML.var (p, v))
        val uses = Vector.layout 
                     (fn u => IML.use (p, u)) 
                     (getUses (p, c))
        val res = 
            L.mayAlign [l,
                        LU.indent (
                        L.mayAlign [L.seq[L.str " <- ", vars],
                                    L.seq[L.str " -> ", uses]])]
      in res
      end
  val () = BackPatch.fill (IML.iFuncH, layout)

  val mkTitle =
   fn (p, fname, recursive, escapes, size) =>
      let
        val attrs = "size=" ^ (Int.toString (size))
        val attrs = if recursive 
                    then attrs ^ ", recursive" 
                    else attrs
        val attrs = if escapes 
                    then attrs ^ ", escapes" 
                    else attrs
      in
        L.seq [L.str "CFG - Function ", IML.var (p, fname), 
               L.str (" (" ^ attrs ^ ")")]
      end

        (* Layout cfg in dot format. *)
  val layoutDot' = 
   fn (p, IMT.F {cfg, entry, exit, recursive, 
                 escapes, size, fname, ...} ) =>
      let
        val title = mkTitle (p, fname, recursive, escapes, !size)
        val edgeOptions =
         fn (edg) => 
            let
            in []
            end
        val labelNode =
         fn (n) => 
            case (Graph.Node.getLabel (n))
             of NONE => "No label?"
              | SOME blk => 
                let
                  val (label, _) = Block.getLabel' (p, blk)
                in
                  Identifier.labelString label
                end
        val nodeOptions =
         fn (n) => 
            if (n = entry) then
              [Dot.NodeOption.Label [("Entry", Dot.Center)],
               Dot.NodeOption.Shape Dot.Box]
            else if (n = exit) then
              [Dot.NodeOption.Label [("Exit", Dot.Center)],
               Dot.NodeOption.Shape Dot.Box]
            else
              [Dot.NodeOption.Label [(labelNode n, Dot.Center)],
               Dot.NodeOption.Shape Dot.Ellipse]
        val graphOptions = [Dot.GraphOption.Page {width=8.5, height=11.0},
                            Dot.GraphOption.Orientation Dot.Landscape]
      in
        Graph.layoutDot' (cfg, {edgeOptions  = edgeOptions, 
                                nodeOptions  = nodeOptions, 
                                graphOptions = graphOptions,
                                graphTitle   = title})
      end

  val layoutDot = 
   fn (p, cfg) => layoutDot' (p, IMT.iFuncGetIFunc' cfg)

  val getIFuncByName' =
   fn (p, fname) => IVD.lookup (IMT.tGetIFuncs p, fname)

  val getIFuncByName =
   fn (p, fname) => 
      let
        val cfg = 
            case getIFuncByName' (p, fname)
             of SOME cfg => cfg
              | NONE => fail ("getIFuncByName",
                              "Bad function name: "^Layout.toString (IML.var (p, fname)))
      in cfg
      end


  local
    val checkBlocks =
     fn (p, iFunc) =>
        let
          val bs = Enumerate.IFunc.blocks (p, iFunc)
          val help = 
           fn b => 
              let
                val es = IMT.iBlockGetNodeOutEdges b
                val t = Block.getTransfer' (p, b)
                val {blocks, exits} = MilUtils.Transfer.targets t
                val n = (Vector.length blocks) + (if exits then 1 else 0)
                val m = List.length es
                val () = if m = n then ()
                         else
                           let
                           in
                             fail ("Cfg.check",
                                   "Bad block edges")
                           end
              in ()
              end
          val () = List.foreach (bs, help)
        in ()
        end

    val computeFunctionSize =
     fn (p, iFunc) => 
        let
          val is = Enumerate.IFunc.instructions (p, iFunc)
        in List.length is
        end
  in
  val check =
   fn (p, iFunc) => 
      let
        val () = 
            if IMT.iFuncIsInitialized iFunc then ()
            else
              fail ("iFunc.check",
                    "iFunc is not initialized")
        val cfg' = IMT.iFuncGetIFunc' iFunc
        val c = computeFunctionSize (p, iFunc)
        val () = checkBlocks (p, iFunc)
        val () = 
            if (c = getSize (p, iFunc)) then 
              ()
            else
              (print "IMil sizes are incorrect:\n";
               LU.printLayout 
                 (L.align [L.str "Function is:",
                           layout (p, iFunc),
                           L.seq[L.str "With recorded size ",
                                 Int.layout (getSize (p, iFunc))],
                           L.seq[L.str "With computed size ",
                                 Int.layout c]
                 ]);
               fail ("IFunc.check",
                     "Program size mismatch"))
              
      in ()
      end

  val checkAll =
   fn (p) => 
      let
        val iFuncs = getIFuncs p
        val () = List.foreach (iFuncs, fn (f, iFunc) => check (p, iFunc))
      in ()
      end
  end
                  
end
