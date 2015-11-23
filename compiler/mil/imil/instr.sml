(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature IMIL_INSTR = 
sig
  include IMIL_PUBLIC_TYPES

  (* Marks instruction as dead.  Instruction is still valid for use. *)
  val delete        : t * iInstr -> unit

  val getMil        : t * iInstr -> mInstr
  val getIBlock     : t * iInstr -> iBlock
  val getIFunc      : t * iInstr -> iFunc

  val freeVars  : t * iInstr -> Identifier.VariableSet.t
  val freeVars' : t * iInstr -> Mil.variable list

  val variablesDefined : t * iInstr -> Mil.variable vector

  (* replaceMil (t, i, mi).  Replace the contents of i with mi.  This should 
   * be used with caution.  In particular, the following are the only 
   * guaranteed valid replacements:
   * 1) Replacing any instruction with MDead.  This essentially kills the 
   * instruction.  Using delete is a better option.
   * 2) Replacing a MInstr with a new MInstr.  The use def info will be 
   * updated.
   * 3) Replacing a MTransfer with a new MTransfer.  The use def info and 
   * control flow graph info will be updated appropriately.
   * 4) Replacing a MLabel with a new MLabel, both of which are labeled with 
   * the same Mil.label.  Use def info will be updated appropriately.
   *)
  val replaceMil : t * iInstr * mInstr -> unit
  val replaceInstruction : t * iInstr * Mil.instruction -> unit
  val replaceTransfer : t * iInstr * Mil.transfer -> unit
  val replaceLabel : t * iInstr * (Mil.label * Mil.variable Vector.t) -> unit

  val isRec : t * iInstr -> bool

  (* Get the predecessor or successor blocks of the block
   * in which the given instruction resides.
   *)
  val preds : t * iInstr -> iBlock list
  val succs : t * iInstr -> iBlock list

  (* Get the items which are used by this instruction *)
  val getUsedBy : t * iInstr -> item Vector.t

  (* Get the uses of this instruction *)
  val getUses   : t * iInstr -> use Vector.t

  (* insertBefore (t, i1, i2) => Inserts i1 before i2 in
   * in the instruction list.  It is a fatal error if i2 
   * is a label or if i2 is not in a block.  *)
  val insertBefore  : t * Mil.instruction * iInstr -> iInstr
  val insertBefore' : t * iInstr * iInstr -> unit 
  (* insertAfter (t, i1, i2) => Inserts i2 after i1 in
   * in the instruction list.  It is a fatal error if i1 
   * is a transfer or if i1 is not in a block *)
  val insertAfter   : t * iInstr * Mil.instruction -> iInstr
  val insertAfter'  : t * iInstr * iInstr -> unit

  (* Get the next instruction within a block,
   * skipping dead instructions.
   * Throws an error if argument is transfer or label.
   *)
  val next : t * iInstr -> iInstr option
  val prev : t * iInstr -> iInstr option

  (* Get the next instruction in an extended basic block.
   * nextExt (t, i) => NONE only if
   *   1) i is not in a block or
   *   2) i is a transfer from block b and
   *      a) b has multiple successors or
   *      b) the successor of b has multiple predecessors
   *)      
  val nextExt : t * iInstr -> iInstr option
  (* Get the previous instruction in an extended basic block.
   * prevExt (t, i) => NONE only if
   *   1) i is not in a block or
   *   2) i is a label of block b and
   *      a) b has multiple predecessors or
   *      b) the predecessor of b has multiple successors
   *)      
  val prevExt : t * iInstr -> iInstr option

  val layout    : t * iInstr -> Layout.t
  val layoutMil : t * mInstr -> Layout.t

  val toInstruction : iInstr -> Mil.instruction option
  val toRhs         : iInstr -> Mil.rhs option
  val toTransfer    : iInstr -> Mil.transfer option
  val toLabel       : iInstr -> (Mil.label * Mil.variable Vector.t) option
                                    
  val splitUses' : t * iInstr * use Vector.t -> {inits : use Vector.t, others : use Vector.t}
  val splitUses  : t * iInstr  -> {inits : use Vector.t, others : use Vector.t}

  val fx : t * iInstr -> Effect.set
end

structure IMilInstr :
sig 
  include IMIL_INSTR

  (* new creates a new empty instruction with no location and no block
   * association. *)
  val new    : t -> iInstr
  (* new' creates a new instruction with no location and no block
   * association.*)
  val new'   : t * mInstr -> iInstr

  val setLoc    : t * iInstr * iBlock * iInstr DList.cursor option -> unit
end 
  = 
struct
  open IMilPublicTypes

  val fail = 
   fn (f, s) => Fail.fail ("instr.sml", f, s)

  structure L = Layout
  structure LU = LayoutUtils
  structure M = Mil
  structure MU = MilUtils
  structure IMT = IMilTypes
  structure Var = IMilVar
  structure Use = IMilUse
  structure Def = IMilDef
  structure FV = IMilCommon.FV
  structure VS = Identifier.VariableSet
  structure Graph = IMT.Graph
  structure IVD = IMT.IVD

  val getId = 
   fn (p, i) => IMT.iInstrGetId i
  val getMil = 
   fn (p, i) => IMT.iInstrGetMil i
  val getVars = 
   fn (p, i) => IMT.iInstrGetVars i
  val getIBlock = 
   fn (p, i)  => IMT.iInstrGetIBlock i
  val getLoc = 
   fn (p, i) => IMT.iInstrGetLoc i

  val getIFunc =
   fn (p, i) => IMT.iBlockGetIFunc (IMT.iInstrGetIBlock i)

  val toInstruction = IMT.iInstrToInstruction
  val toRhs = IMT.iInstrToRhs
  val toTransfer = IMT.iInstrToTransfer
  val toLabel = IMT.iInstrToLabel

  val preds =
   fn (p, i) => 
      IMT.iBlockGetPredIBlocks (getIBlock (p, i))

  val succs =
   fn (p, i) => 
      IMT.iBlockGetSuccIBlocks (getIBlock (p, i))

  val freeVarsInMilInstr = 
   fn (p, m) =>
      let
        val s = 
            case m
             of IMT.MInstr i    => FV.instruction (p, i)
              | IMT.MTransfer t => FV.transfer (p, t)
              | IMT.MLabel (l, vs) => VS.empty
              | IMT.MDead => VS.empty
      in s
      end

  val buildMilVars = 
   fn (p, i, m) =>
      let
        val vs = freeVarsInMilInstr (p, m)
        val vs = VS.toList vs
        val vars = List.map (vs, (fn v => (v, Use.addUse (p, v, IMT.UseInstr i))))
        val vars = IVD.fromList vars
      in vars
      end


  val updateGraph =
   fn (p, block, new, old) => 
      let
        val iFunc = IMT.iBlockGetIFunc block 
        val exit = IMT.iFuncGetExit iFunc
        val graph = IMT.iFuncGetCfg iFunc
        val node = IMT.iBlockGetNode block
        val getNodeForLabel = fn l => IMT.iFuncGetNodeByLabel (iFunc, l)
        val () = 
            case old
             of IMT.MTransfer told => 
                let
                  val deleteEdge =
                   fn e => Graph.deleteEdge (graph, e)
                  val edges = Graph.Node.outEdges (node)
                  val () = List.foreach (edges, deleteEdge)
                in ()
                end
              | _ => ()
        val () = 
            case new
             of IMT.MTransfer tnew => 
                let
                  val {blocks, exits} = MilUtils.Transfer.targets tnew
                  val addEdge =
                   fn label => 
                      ignore (Graph.addEdge (graph, node, getNodeForLabel label, ()))
                  val () = Vector.foreach (blocks, addEdge)
                  val () = if exits then
                             ignore (Graph.addEdge (graph, node, exit, ()))
                           else 
                             ()
                in ()
                end
              | _ => ()

      in ()
      end

  val variablesDefinedByMInstr = 
   fn (p, m) => 
      let
        val vv = 
            case m
             of IMT.MInstr (M.I {dests, ...}) => dests
              | IMT.MLabel (l, parms) => parms
              | IMT.MTransfer (M.TInterProc {ret = M.RNormal {rets, ...}, ...}) => rets
              | IMT.MTransfer _ => Vector.new0 ()
              | IMT.MDead => Vector.new0 ()
      in vv
      end

  val variablesDefined = 
   fn (p, i) => variablesDefinedByMInstr (p, getMil (p, i))

  val deleteDefs =
   fn (p, m) => 
      let
        val removeDef = fn v => Def.delete (p, v)
        val () = Vector.foreach (variablesDefinedByMInstr (p, m), removeDef)
      in ()
      end

  val addDefs =
   fn (p, i, m) => 
      let
        val addDef = fn v => Def.add (p, v, IMT.DefInstr i)
        val () = Vector.foreach (variablesDefinedByMInstr (p, m), addDef)
      in ()
      end

  val decIFuncSize =
   fn (p, i) => 
      (case getMil (p, i)
        of IMT.MDead => ()
         | _ => IMT.iFuncDecSize (getIFunc (p, i)))

  val incIFuncSize =
   fn (p, i) => 
      (case getMil (p, i)
        of IMT.MDead => ()
         | _ => IMT.iFuncIncSize (getIFunc (p, i)))

  val setLoc =
   fn (p, i, b, c) => 
      let
        val () = 
            if IMT.iBlockIsInitialized (IMT.iInstrGetIBlock i) then
              decIFuncSize (p, i)
            else
               ()
        val () = 
            (case IMT.iInstrGetLoc i
              of NONE => ()
               | SOME c' => DList.remove c')
        val () = IMT.iInstrSetLoc (i, c)
        val () = IMT.iInstrSetIBlock (i, b)
        val () = incIFuncSize (p, i)
      in ()
      end


  val replaceMil =
   fn (p, i, m) => 
      let
        val block = IMT.iInstrGetIBlock i
        val mil =  IMT.iInstrGetMil i
        val vars =  IMT.iInstrGetVars i
        val () = IVD.foreach (vars, fn (v, u) => Use.deleteUse (p, u))
        val () = deleteDefs (p, mil)
        val () = decIFuncSize (p, i)
        val () = addDefs (p, i, m)
        val vars = buildMilVars (p, i, m)
        val () = updateGraph (p, block, m, mil)
        val () = IMT.iInstrSetMil (i, m)
        val () = IMT.iInstrSetVars (i, vars)
        val () = incIFuncSize (p, i)
        val () = 
            case (m, mil)
             of (IMT.MTransfer _, IMT.MTransfer _) => ()
              | (IMT.MInstr _, IMT.MInstr _) => ()
              | (IMT.MLabel (l, _), IMT.MLabel (l', _)) => 
                if l = l' then ()
                else fail ("replaceMil",
                           "Replacing label with different label not allowed")
              | (IMT.MDead, _) => ()
              | (_, IMT.MDead) => ()
              | _ => 
                fail ("replaceMil",
                      "Illegal mil replacement in instr")
      in ()
      end

  val () = BackPatch.fill (Use.replaceMilIH, replaceMil)

  val replaceInstruction = 
   fn (p, i, m) => replaceMil (p, i, IMT.MInstr m)

  val replaceTransfer = 
   fn (p, i, t) => replaceMil (p, i, IMT.MTransfer t)

  val replaceLabel =
   fn (p, i, l) => replaceMil (p, i, IMT.MLabel l)

  val delete = 
   fn (p, i) => replaceMil (p, i, IMT.MDead)

  local
    val new'' = 
        fn (p, b, m) => 
        let
          val id = IMT.nextId p
          val i' = 
              IMT.I {id    = id,
                     vars  = IVD.empty (),
                     iBlock = b,
                     mil   = IMT.MDead,
                     loc   = NONE}
          val i = IMT.iInstrNew i'
          val () = addDefs (p, i, m)
          val vars = buildMilVars (p, i, m)
          val () = IMT.iInstrSetVars (i, vars)
          val () = IMT.iInstrSetMil (i, m)
        in i
        end
  in
  val new' = 
      fn (p, m) => new'' (p, IMT.iBlockNewUninitialized (), m) 
  val new = 
   fn (p) => new' (p, IMT.MDead)
  end

  val getUses = 
   fn (p, i) => 
      let
        val vars = variablesDefined (p, i)
        val uses = Vector.toListMap (vars, fn v => Use.getUses (p, v))
        val uses = 
            case getMil (p, i)
             of IMT.MInstr _ => uses
              | IMT.MLabel (l, args) => Vector.new1 IMT.Used :: uses
              | IMT.MTransfer t => 
                let
                  val succ = IMT.iBlockGetSuccIBlocks (IMT.iInstrGetIBlock i)
                  val labelUses = Vector.fromListMap (succ, IMT.UseInstr o IMT.iBlockGetLabel)
                  (* XXX Could only mark things connected to exit
                   * node as always used.  *)
                  val uses = Vector.new1 IMT.Used :: labelUses :: uses
                in uses
                end
              | IMT.MDead  => []
        val uses = Vector.concat uses
      in uses
      end

  val freeVars' = 
      fn (p, i) => 
      let
        val vars = getVars (p, i)
        val vars = IVD.domain vars
      in vars
      end

  val freeVars = VS.fromList o freeVars'

  val getUsedBy = 
   fn (p, i) => 
      let
        val dls = freeVars' (p, i)
        val defs = Vector.fromListMap (dls, 
                                    fn v => Def.get (p, v))
        val items = Def.defsToItems (p, defs)
      in items
      end

  val layout = 
   fn (p, i) => 
      let
        val IMT.I {id, mil, vars, iBlock, loc} = IMT.iInstrGetIInstr' i
        val l = IMilLayout.mInstr (p, mil)
        val vars = IVD.layout (vars, fn (v, _) => IMilLayout.var (p, v))
        val uses = Vector.layout 
                     (fn u => IMilLayout.use (p, u)) 
                     (getUses (p, i))
        val res = 
            L.mayAlign [L.seq [L.str "I ", l],
                        LU.indent (
                        L.mayAlign [L.seq[L.str " <- ", vars],
                                    L.seq[L.str " -> ", uses]])]
      in res
      end

  val () = BackPatch.fill(IMilLayout.iInstrH, layout)

  val layoutMil = 
   fn (p, mi) => IMilLayout.mInstr (p, mi)

  val isRec = 
   fn (p, i) => 
      let
        val b = getIBlock (p, i)
        val f = IMT.iFuncGetFName (IMT.iBlockGetIFunc b)
        val s = freeVarsInMilInstr (p, getMil (p, i))
        val recursive = VS.member (s, f)
      in recursive
      end


  local
    val rec move = 
     fn mv => fn (p, i) => 
        let
          val loc = IMT.iInstrGetLoc i
          val res = 
              case loc
               of SOME l => 
                  (case mv l
                    of SOME c => 
                       let
                         val i = DList.getVal c
                         val res = case IMT.iInstrGetMil i
                                    of IMT.MDead => move mv (p, i)
                                     | _ => SOME i
                       in res
                       end
                     | NONE => NONE)
                | _ => 
                  let
                    val () = LayoutUtils.printLayout 
                               (L.seq[IMilLayout.iInstr (p, i), L.str "\n"])
                  in
                    fail ("move",
                          "Instruction is transfer/label or not in a block")
                  end
        in res
        end
  in
  val next : IMT.t * IMT.iInstr -> IMT.iInstr option = move DList.next
  val prev : IMT.t * IMT.iInstr -> IMT.iInstr option = move DList.prev

  val nextExt = 
   fn (p, i) => 
      let
        val block = IMT.iInstrGetIBlock i
        val mil = IMT.iInstrGetMil i
        val res = 
            case mil
             of IMT.MLabel _ => 
                let
                  val code = IMT.iBlockGetCode block
                  val res = Option.map (DList.first code, DList.getVal)
                in res
                end
              | IMT.MTransfer _ => 
                (case IMT.iBlockGetSuccIBlocks block
                  of [b] => 
                     (case IMT.iBlockGetPredIBlocks b
                       of [_] => SOME (IMT.iBlockGetLabel b)
                        | _ => NONE)
                   | _ => NONE)
              | _ => (case next (p, i)
                       of NONE => SOME (IMT.iBlockGetTrans block)
                        | io => io)
      in res
      end

  val prevExt = 
   fn (p, i) => 
      let
        val block = IMT.iInstrGetIBlock i
        val mil = IMT.iInstrGetMil i
        val res = 
            case mil
             of IMT.MLabel _ => 
                (case IMT.iBlockGetPredIBlocks block
                  of [b] => 
                     (case IMT.iBlockGetSuccIBlocks b
                       of [_] => SOME (IMT.iBlockGetTrans b)
                        | _ => NONE)
                   | _ => NONE)
              | IMT.MTransfer _ => 
                let
                  val code = IMT.iBlockGetCode block
                  val res = Option.map (DList.last code, DList.getVal)
                in res
                end
              | _ => (case prev (p, i)
                       of NONE => SOME (IMT.iBlockGetLabel block)
                        | io => io)
      in res
      end
  end

  val assertInstruction = 
   fn mi => 
      (case mi
        of IMT.MDead => ()
         | IMT.MInstr _ => ()
         | _ => fail ("assertInstruction",
                      "Not a legal location for a transfer or label"))

  val insertBefore' = 
   fn (p, i1, i2) => 
      let
        val m1 = IMT.iInstrGetMil i1
        val m2 = IMT.iInstrGetMil i2
        val l2 = IMT.iInstrGetLoc i2
        val b2 = IMT.iInstrGetIBlock i2
        val () = assertInstruction m1
        val l1 = 
            (case (m2, l2) 
              of (IMT.MTransfer _, _) => 
                 DList.insertLast (IMT.iBlockGetCode b2, i1)
               | (_, SOME loc) => DList.insertL (loc, i1)
               | (_, NONE)     => fail ("insertBefore'",
                                        "Instr is not in block"))
        val () = setLoc (p, i1, b2, SOME l1)
      in
        ()
      end
  val insertBefore = 
   fn (p, m, i2) => 
      let
        val i1 = new' (p, IMT.MInstr m)
        val () = insertBefore' (p, i1, i2)
      in i1
      end

  val insertAfter' = 
   fn (p, i1, i2) => 
      let
        val m1 = IMT.iInstrGetMil i1
        val l1 = IMT.iInstrGetLoc i1
        val b1 = IMT.iInstrGetIBlock i1
        val m2 = IMT.iInstrGetMil i2
        val () = assertInstruction m2
        val l2 = case (m1, l1)
                  of (IMT.MLabel _, _) => 
                     DList.insert (IMT.iBlockGetCode b1, i2)
                   | (_, SOME loc) => DList.insertR (loc, i2)
                   | (_, NONE)     => fail ("insertAfter'",
                                            "Instr is not in block")
        val () = setLoc (p, i2, b1, SOME l2)
      in
        ()
      end
  val insertAfter = 
   fn (p, i1, m) => 
      let
        val i2 = new' (p, IMT.MInstr m)
        val () = insertAfter' (p, i1, i2)
      in i2
      end

  val splitUses' =
   fn (imil, i, uses) =>
      (case getMil (imil, i)
        of IMT.MInstr (M.I {dests, ...}) => 
           let
             val ps = Vector.map (dests, fn v => Use.splitUses' (imil, v, uses))
             val help = 
              fn ({others, inits}, (othersL, initsL)) => (others::othersL, inits::initsL)
             val (othersL, initsL) = Vector.fold (ps, ([],[]), help)
             val others = Vector.concat othersL
             val inits = Vector.concat initsL
           in {others = others, inits = inits}
           end
         | _ => {others = uses, inits = Vector.new0 ()})

  val splitUses = 
   fn (imil, i) => splitUses' (imil, i, getUses (imil, i))

  val fx = 
   fn (imil, i) =>
      let
        val fx = 
            case IMT.iInstrGetMil i
             of IMT.MInstr i         => MU.Instruction.fx (IMT.tGetConfig imil, i)
              | IMT.MLabel (l, args) => Effect.Total
              | IMT.MTransfer t      => MU.Transfer.fx (IMT.tGetConfig imil, t)
              | IMT.MDead            => Effect.Total
      in fx
      end


end
