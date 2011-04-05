(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_REP_OPTIMIZE = 
sig
  val stats : (string * string) list
  val debugs : Config.Debug.debug list
  val program : PassData.t * MilRepSummary.summary * Mil.t -> Mil.t
end

structure MilRepOptimize :> MIL_REP_OPTIMIZE = 
struct
  val passname = "MilRepOptimize"
  val fail = 
   fn (fname, msg) => Fail.fail ("optimize.sml", fname, msg)

  structure M = Mil
  structure MU = MilUtils
  structure MRS = MilRepSummary
  structure FG = MilRepFlowGraph
  structure PD = PassData
  structure SS = StringSet
  structure ID = IntDict

  structure Chat = ChatF (struct 
                            type env = PD.t
                            val extract = PD.getConfig
                            val name = passname
                            val indent = 4
                          end)


  structure Click = 
  struct
    val stats = []
    val {stats, click = constantProp} = PD.clicker {stats = stats, passname = passname, 
                                                    name = "ConstantProp", desc = "Constants globally propagated"}
    val {stats, click = unboxTuple} = PD.clicker {stats = stats, passname = passname, 
                                                  name = "UnboxTuple", desc = "Single element tuples unboxed"}
    val {stats, click = mkDirect} = PD.clicker {stats = stats, passname = passname, 
                                                name = "MkDirect", desc = "Calls/Evals resolved to direct"}
    val {stats, click = escapeAnalysis} = PD.clicker {stats = stats, passname = passname, 
                                                      name = "NonEscape", desc = "Codes marked non-escaping"}
                                               
  end   (*  structure Click *)

  val stats = Click.stats

  val (debugPassD, debugPass) =
      Config.Debug.mk (passname ^ ":debug", "Debug rep analysis according to debug level")

  val mkDebug : string * string * int -> (Config.Debug.debug * (PassData.t -> bool)) = 
   fn (tag, description, level) =>
      let
        val (debugD, debug) = 
            Config.Debug.mk (passname ^ ":" ^ tag, description)
        val debug = 
         fn d => 
            let
              val config = PD.getConfig d
            in debug config orelse 
               (debugPass config andalso Config.debugLevel (config, passname) >= level)
            end
      in (debugD, debug)
      end

  val debug = 
   fn (config, i) => debugPass config andalso (Config.debugLevel (config, passname) >= i)

  val (showUnboxingD, showUnboxing) = mkDebug ("show-unboxing", "Show unboxing analysis", 1)
  val (showConstantPropD, showConstantProp) = mkDebug ("show-constant-prop", "Show constant propagation analysis", 1)
  val (showCFAD, showCFA) = mkDebug ("show-cfa", "Show global control flow analysis", 1)
  val (showPhasesD, showPhases) = mkDebug ("show-phases", "Show IR between phases", 1)
      

  val debugs = [debugPassD, showUnboxingD, showCFAD, showConstantPropD, showPhasesD]

  structure Unbox = 
  struct
    val skip = MilRepBase.noTupleUnbox

    (* We could loosen this to subtyping, except that float < bits, and int < bits, 
     * but calling conventions and other issues could give us problems if
     * we unbox something which contains both.
     *)
    structure TLat = FlatLatticeFn (struct 
                                      type element = Mil.typ
                                      val equal = MilType.Type.equal 
                                    end)

    datatype 'data state = S of {summary : MRS.summary,
                                 flowgraph : 'data FG.t}

    datatype env = E of {pd : PD.t}

    val getSummary = fn (S {summary, ...}) => summary
    val getFlowgraph = fn (S {flowgraph, ...}) => flowgraph
    val getPd = fn (E {pd, ...}) => pd
    val getConfig = PD.getConfig o getPd

    val typOfVariable = 
     fn ((s, e), v) => 
        let
          val summary = getSummary s
          val t = MRS.variableTyp (summary, v)
        in t
        end

    val unboxTuple = 
     fn (s, e, noUnbox, dests, {mdDesc, inits}) => 
        let
          val summary = getSummary s
          val unboxed = 
              Try.try
                (fn () =>
                    let
                      val dest = Try.V.singleton dests
                      val () = Try.require (not (MU.MetaDataDescriptor.hasArray mdDesc))
                      val () = Try.require (MU.MetaDataDescriptor.numFixed mdDesc = 1)
                      val () = Try.require (MU.MetaDataDescriptor.immutable mdDesc)
                      val oper = Try.V.sub (inits, 0)
                      val v = Try.<@ MU.Simple.Dec.sVariable oper
                      val t = MRS.variableTyp (summary, v)
                      val node = MRS.variableNode (summary, dest)
                      val () = FG.add (getFlowgraph s, node, TLat.elt t)
                    in ()
                    end)
          val () = if isSome unboxed then ()
                   else noUnbox ()
        in ()
        end

    structure Analyze1 =
    MilAnalyseF(struct
                  type state = TLat.t state
                  type env = env
                  val config = getConfig
                  val indent = 2
                  val variableBind = NONE
                  val labelBind = NONE
                  val variableUse = NONE
                  val analyseJump = NONE
                  val analyseCut = NONE
                  val analyseConstant = NONE
                  val analyseInstruction' = 
                   fn (s, e, M.I {dests, n, rhs}) => 
                       let
                         val summary = getSummary s
                         val noUnbox = 
                          fn () =>
                             let
                               val ()= 
                                   if debug (getConfig e, 1) then
                                     LayoutUtils.printLayout 
                                       (Layout.seq [Layout.str "Variables cannot be unboxed: ",
                                                    Vector.layout (Identifier.layoutVariable') dests])
                                   else 
                                     ()
                               val mark = 
                                fn v => FG.add (getFlowgraph s, MRS.variableNode (summary, v), TLat.top)
                             in Vector.foreach (dests, mark)
                             end

                         val () = 
                             (case rhs
                               of M.RhsSimple s         => 
                                  (case s
                                    of M.SVariable _    => ()
                                     | _                => noUnbox ())
                                | M.RhsPrim _           => noUnbox ()
                                | M.RhsTuple r          => unboxTuple (s, e, noUnbox, dests, r)
                                | M.RhsTupleSub _       => ()
                                | M.RhsTupleSet _       => ()
                                | M.RhsTupleInited _    => ()
                                | M.RhsIdxGet _         => noUnbox ()
                                | M.RhsCont _           => noUnbox ()
                                | M.RhsObjectGetKind _  => noUnbox ()
                                | M.RhsThunkMk _        => noUnbox ()
                                | M.RhsThunkInit _      => noUnbox ()
                                | M.RhsThunkGetFv _     => ()
                                | M.RhsThunkValue _     => noUnbox ()
                                | M.RhsThunkGetValue _  => ()
                                | M.RhsThunkSpawn _     => ()
                                | M.RhsClosureMk _      => noUnbox ()
                                | M.RhsClosureInit _    => noUnbox ()
                                | M.RhsClosureGetFv _   => ()
                                | M.RhsPSetNew _        => noUnbox ()
                                | M.RhsPSetGet _        => noUnbox ()
                                | M.RhsPSetCond _       => noUnbox ()
                                | M.RhsPSetQuery _      => noUnbox ()
                                | M.RhsPSum _           => noUnbox ()
                                | M.RhsPSumProj _       => ())

                       in e
                       end
                  val analyseInstruction = SOME analyseInstruction'
                  val analyseTransfer = NONE
                  val analyseBlock = NONE
                  val analyseGlobal' = 
                   fn (s, e, v, g) => 
                       let
                         val summary = getSummary s
                         val noUnbox = 
                          fn () =>
                             let
                               val ()= 
                                   if debug (getConfig e, 1) then
                                     LayoutUtils.printLayout 
                                       (Layout.seq [Layout.str "Global cannot be unboxed: ",
                                                    Identifier.layoutVariable' v])
                                   else 
                                     ()
                                val () = FG.add (getFlowgraph s, MRS.variableNode (summary, v), TLat.top)
                             in ()
                             end

                         val () = 
                             (case g
                               of M.GTuple r                => unboxTuple (s, e, noUnbox, Vector.new1 v, r)
                                | M.GSimple (M.SVariable _) => ()
                                | M.GErrorVal t             => 
                                  let
                                    val unboxed = 
                                        Try.try
                                          (fn () =>
                                              let
                                                val {pok, fixed, array} = Try.<@ MU.Typ.Dec.tTuple t
                                                val (t, fv) = Try.V.singleton fixed
                                                val () = Try.require (MU.FieldVariance.immutable fv)
                                                val node = MRS.variableNode (summary, v)
                                                val () = FG.add (getFlowgraph s, node, TLat.elt t)
                                              in ()
                                              end)
                                    val () = if isSome unboxed then ()
                                             else noUnbox ()
                                  in ()
                                  end
                                | _                         => noUnbox ())
                       in e
                       end
                  val analyseGlobal = SOME analyseGlobal'
                end)

    val forward1 = 
     fn (pd, summary, p) => 
        let
          val fgF1 = FG.build {pd = pd,
                               forward = true,
                               summary = summary,
                               uDefInit = TLat.top,
                               uUseInit = TLat.top, 
                               initialize = fn n => TLat.bot,
                               merge = TLat.join,
                               equal = TLat.equal MilType.Type.equal
                              }
          val state = S {summary = summary, flowgraph = fgF1}
          val env = E {pd = pd}
          val () = Analyze1.analyseProgram (state, env, p)
          val () = FG.propagate fgF1
        in fgF1
        end

    structure Analyze2 =
    MilAnalyseF(struct
                  type state = bool state
                  type env = env
                  val config = getConfig
                  val indent = 2
                  val variableBind = NONE
                  val labelBind = NONE
                  val variableUse = NONE
                  val analyseJump = NONE
                  val analyseCut = NONE
                  val analyseConstant = NONE
                  val analyseInstruction' = 
                   fn (s, e, M.I {dests, n, rhs}) => 
                       let
                         val summary = getSummary s
                         val noUnbox = 
                          fn v => 
                             let
                               val () = 
                                   if debug (getConfig e, 1) then
                                     LayoutUtils.printLayout 
                                       (Layout.seq [Layout.str "Variable use cannot be unboxed: ",
                                                    Identifier.layoutVariable' v])
                                   else 
                                     ()
                               val () = FG.add (getFlowgraph s, MRS.variableNode (summary, v), false)
                             in ()
                             end

                         val noUnboxO = 
                          fn c =>
                             let
                               val v = 
                                   (case c
                                     of M.SVariable v => v
                                      | _ => fail ("analyseInstruction'", "Not in named form"))
                             in noUnbox v
                             end

                         val () = 
                             (case rhs
                               of M.RhsPrim {args, ...} => Vector.foreach (args, noUnboxO)
                                | M.RhsObjectGetKind v  => noUnbox v
                                | M.RhsPSetNew oper     => noUnboxO oper
                                | M.RhsPSetCond r       => noUnboxO (#ofVal r)
                                | _                     => ())
                       in e
                       end
                  val analyseInstruction = SOME analyseInstruction'
                  val analyseTransfer' = 
                   fn (s, e, t) => 
                      let 
                        val summary = getSummary s
                        val noUnboxO = 
                         fn oper => 
                             let
                               val v = 
                                   (case oper
                                     of M.SVariable v => v
                                      | _ => fail ("analyseTransfer'", "Not in named form"))

                               val () = 
                                   if debug (getConfig e, 1) then
                                     LayoutUtils.printLayout 
                                       (Layout.seq [Layout.str "Variable in comparison cannot be unboxed: ",
                                                    Identifier.layoutVariable' v])
                                   else 
                                     ()
                               val () = FG.add (getFlowgraph s, MRS.variableNode (summary, v), false)
                             in ()
                             end
                        (* Lowered option sets may compare pointers to zero *)
                        val () = 
                            (case t
                              of M.TCase {on , ...} => noUnboxO on
                               | _ => ())
                      in e
                      end
                  val analyseTransfer = SOME analyseTransfer'
                  val analyseBlock = NONE
                  val analyseGlobal' = 
                   fn (s, e, v, g) => 
                       let
                         val summary = getSummary s
                         val noUnbox = 
                          fn v => 
                             let
                               val () = 
                                   if debug (getConfig e, 1) then
                                     LayoutUtils.printLayout 
                                       (Layout.seq [Layout.str "Global use cannot be unboxed: ",
                                                    Identifier.layoutVariable' v])
                                   else 
                                     ()
                               val () = FG.add (getFlowgraph s, MRS.variableNode (summary, v), false)
                             in ()
                             end

                         val noUnboxO = 
                          fn c =>
                             let
                               val v = 
                                   (case c
                                     of M.SVariable v => v
                                      | _ => fail ("analyseGlobal'", "Not in named form"))
                             in noUnbox v
                             end

                         val () = 
                             (case g
                               of M.GPSet s       => noUnboxO s
                                | _ => ())
                       in e
                       end
                  val analyseGlobal = SOME analyseGlobal'

                end)

    (* XXX This is overly conservative.  Not all nodes are required to have a consistent view.
     * Doesn't matter for unification based though.  -leaf
     *)
    val backward = 
     fn (pd, summary, p, fgF1) => 
        let
          val fgB = 
              FG.build {pd = pd,
                        forward = false,
                        summary = summary,
                        uDefInit = false,
                        uUseInit = false, 
                        initialize = fn n => true,
                        merge = fn (a, b) => (a andalso b),
                        equal = op =
                       }

          val nodes = MRS.nodes summary
          val help = fn (i, n) => 
                        (case TLat.get (FG.query (fgF1, n))
                          of NONE => FG.add (fgB, n, false)
                           | _ => ())
          val () = ID.foreach (nodes, help)
          val state = S {summary = summary, flowgraph = fgB}
          val env = E {pd = pd}
          val () = Analyze2.analyseProgram (state, env, p)
          val () = FG.propagate fgB
        in fgB
        end

    val forward2 = 
     fn (pd, summary, fgB) => 
        let
          val fgF2 = 
              FG.build {pd = pd,
                        forward = true,
                        summary = summary,
                        uDefInit = false,
                        uUseInit = false, 
                        initialize = fn n => FG.query (fgB, n),
                        merge = fn (a, b) => (a andalso b),
                        equal = op =
                       }
          val () = FG.propagate fgF2
        in fgF2
        end


    val show = 
     fn (pd, summary, fg, p) => 
        if showUnboxing pd then
          let
            val si = Identifier.SymbolInfo.SiTable (MU.Program.symbolTable p)
            val vars = MRS.listVariables summary
            val unboxes = List.map (vars, fn v => (v, FG.query (fg, MRS.variableNode (summary, v))))
            val lv = fn v => MilLayout.layoutVariable (PD.getConfig pd, si, v)
            val ls = List.keepAllMap (unboxes, fn (v, unbox) => if unbox then SOME (lv v) else NONE)
            val l = Layout.align ls
            val l = Layout.align [Layout.str "Unboxing:", LayoutUtils.indent l]
            val () = LayoutUtils.printLayout l
          in ()
          end
        else
          ()

    structure Rewrite = 
    MilRewriterF (struct
                    structure MRC = MilRewriterClient
                    type env   = env
                    type state = bool state
                    val config = getConfig
                    val indent = 2
                    val label       = fn _ => MRC.Stop
                    val variable    = fn _ => MRC.Stop
                    val operand     = fn _ => MRC.Stop
                    val unboxTuple = 
                     fn (state, env, dests, {mdDesc, inits}) => 
                        Try.try 
                          (fn () => 
                              let
                                val summary = getSummary state
                                val fg = getFlowgraph state
                                val v = Try.V.singleton dests
                                val node = MRS.variableNode (summary, v)
                                val () = Try.require (FG.query (fg, node))
                                val oper = Vector.sub (inits, 0)
                                val v' = Try.<@ MU.Simple.Dec.sVariable oper
                                val node' = MRS.variableNode (summary, v')
                                val edge = (node', node)
                                val () = MRS.addEdge (summary, edge)
                                val () = Click.unboxTuple (getPd env)
                              in oper
                              end)

                    val instruction = 
                     fn (state, env, i as M.I {dests, n, rhs}) => 
                        let
                          val summary = getSummary state
                          val fg = getFlowgraph state
                          val res = 
                              (case rhs
                                of M.RhsTuple r => 
                                   let
                                     val operO = unboxTuple (state, env, dests, r)
                                     val c = (case operO
                                               of SOME oper => 
                                                  MRC.StopWith (env, M.I {dests = dests, n = n, rhs = M.RhsSimple oper})
                                                | NONE => MRC.Stop)
                                   in c
                                   end
                                 | M.RhsTupleSub tf => 
                                   let
                                     val v = MU.TupleField.tup tf
                                     val node = MRS.variableNode (summary, v)
                                     val res = 
                                         if FG.query (fg, node) then
                                           let
                                             val rhs = M.RhsSimple (M.SVariable v)
                                             val () = 
                                                 (case MU.Instruction.dest i
                                                   of SOME v' => 
                                                      let
                                                        val node' = MRS.variableNode (summary, v')
                                                        val edge = (node, node')
                                                        val () = MRS.addEdge (summary, edge)
                                                      in ()
                                                      end
                                                    | NONE => ())
                                             val i = M.I {dests = dests, n = n, rhs = rhs}
                                           in MRC.StopWith (env, i)
                                           end
                                         else
                                           MRC.Stop
                                           
                                   in res
                                   end
                                 | _ => MRC.Stop)
                        in res
                        end
                    val transfer    = fn _ => MRC.Stop
                    val block       = fn _ => MRC.Continue
                    val global      = 
                     fn (state, env, (v, g)) => 
                        let
                          val summary = getSummary state
                          val fg = getFlowgraph state
                          val res = 
                              (case g
                                of M.GTuple r => 
                                   let
                                     val operO = unboxTuple (state, env, Vector.new1 v, r)
                                     val c = (case operO
                                               of SOME oper => 
                                                  MRC.StopWith (env, (v, M.GSimple oper))
                                                | NONE => MRC.Stop)
                                   in c
                                   end
                                 | M.GErrorVal t => 
                                   let
                                     val node = MRS.variableNode (summary, v)
                                     val res = 
                                         if FG.query (fg, node) then
                                           let
                                             val t = 
                                                 case MU.Typ.Dec.tTuple t
                                                  of SOME {pok, fixed, array = (M.TNone, M.FvReadWrite)} =>
                                                     #1 (Vector.sub (fixed, 0))
                                                   | _ => fail ("global", "Not an unboxable error value")
                                           in MRC.StopWith (env, (v, M.GErrorVal t))
                                           end
                                         else
                                           MRC.Stop
                                   in res
                                   end
                                 | _ => MRC.Continue)
                        in res
                        end
                    val bind        = fn (_, env, _) => (env, NONE)
                    val bindLabel   = fn (_, env, _) => (env, NONE)
                    val cfgEnum     = fn (_, _, t) => MilUtils.CodeBody.dfsTrees t
                  end)

    val replaceNodeDataWithoutShape = 
     fn (pd, summary, fgF1, n) => 
        let        
          val t = 
              (case TLat.get (FG.query (fgF1, n))
                of SOME t => t
                 | NONE => fail ("rewrite", "Unbox with conflicting type!"))
          val fk = MU.FieldKind.fromTyp' (PD.getConfig pd, t)
          val fv = MilRepNode.fieldVariance' n
          val t = MU.FlatTyp.fromTyp (PD.getConfig pd, t)
          val shape = SOME (MilRepObject.Shape.Build.unknown t)
          val () = MilRepNode.setData (n, shape, fk, fv)
        in ()
        end

    val replaceNodeDataWithShape = 
     fn (pd, summary, fgF1, n, s) => 
        let   
          val fallback = 
           fn () => 
              let
                val () = Chat.warn1 (pd, "Approximating node type")
                val () = replaceNodeDataWithoutShape (pd, summary, fgF1, n)
              in ()
              end
          val replace = 
           fn inner => 
              let
                val fk = MilRepNode.fieldKind' inner
                val fv = MilRepNode.fieldVariance' n
                val shape = MilRepNode.shape' inner
                val () = MilRepNode.setData (n, shape, fk, fv)
              in ()
              end
          val () = 
              (case MilRepObject.Shape.Dec.tuple s
                of NONE => fallback ()
                 | SOME {pok, fields, array} => 
                   (case Utils.Vector.lookup (fields, 0)
                     of NONE => 
                        (case array 
                          of SOME inner => replace inner
                           | NONE => fallback ())
                      | SOME inner => replace inner))
        in ()
        end

    val replaceNodeData = 
     fn (pd, summary, fgF1, fgF2, n) => 
        let
          val () = 
              if FG.query (fgF2, n) then
                (case MilRepNode.shape' n
                  of SOME s => replaceNodeDataWithShape (pd, summary, fgF1, n, s)
                   | NONE => replaceNodeDataWithoutShape (pd, summary, fgF1, n))
              else
                ()
        in ()
        end

    val replaceAllNodeData = 
     fn (pd, summary, fgF1, fgF2) =>
        let
          val nodes = MRS.nodes summary
          val help = fn (i, n) => replaceNodeData (pd, summary, fgF1, fgF2, n)
          val () = ID.foreach (nodes, help)
        in ()
        end

    val rewrite = 
     fn (pd, summary, p, fgF1, fgF2) => 
        let
          val state = S {summary = summary, flowgraph = fgF2}
          val env = E {pd = pd}
          val p = Rewrite.program (state, env, p)
          val () = replaceAllNodeData (pd, summary, fgF1, fgF2)
        in p
        end

    val program = 
     fn (pd, summary, p) => 
        let
          val fgF1 = forward1 (pd, summary, p)
          val fgB = backward (pd, summary, p, fgF1)
          val fgF2 = forward2 (pd, summary, fgB)
          val () = show (pd, summary, fgF2, p)
          val p = rewrite (pd, summary, p, fgF1, fgF2)
          val () = MRS.resetTyps summary
        in p
        end

  end (* structure Unbox *)

  structure ConstantProp = 
  struct
    val skip = MilRepBase.noConstantProp

    (* We construct a lattice whose elements are drawn from (variable x (constant option)).
     * An element (v, co) consists of a global variable v.  If co = SOME c, then v is 
     * bound to c.  If co = NONE, then the definition of v is unknown.  Elements are equal
     * if the variables are equal, or if both variables have equal definitions.  This avoids 
     * relying on CSE of constants (which is generally violated by the name small values pass).
     *)
    val elementEq = 
     fn ((v1, co1), (v2, co2)) => 
        (case (v1 = v2, co1, co2)
          of (true, _, _) => true
           | (false, SOME c1, SOME c2) =>  MU.Constant.eq (c1, c2)
           | _ => false)
    structure CLat = FlatLatticeFn (struct 
                                      type element = (Mil.variable * (Mil.constant option))
                                      val equal = elementEq
                                    end)

    datatype state = S of {summary : MRS.summary,
                           flowgraph : CLat.t FG.t}

    datatype env = E of {pd : PD.t}

    val getSummary = fn (S {summary, ...}) => summary
    val getFlowgraph = fn (S {flowgraph, ...}) => flowgraph
    val getPd = fn (E {pd, ...}) => pd
    val getConfig = PD.getConfig o getPd

    structure Analyze =
    MilAnalyseF(struct
                  type state = state
                  type env = env
                  val config = getConfig
                  val indent = 2
                  val variableBind = NONE
                  val labelBind = NONE
                  val variableUse = NONE
                  val analyseJump = NONE
                  val analyseCut = NONE
                  val analyseConstant = NONE
                  val analyseInstruction' = 
                   fn (s, e, M.I {dests, n, rhs}) => 
                       let
                         val summary = getSummary s
                         val markV = 
                          fn v => FG.add (getFlowgraph s, MRS.variableNode (summary, v), CLat.top)
                         val mark = fn () => Vector.foreach (dests, markV)
                         val () = 
                             (case rhs
                               of M.RhsSimple s         => 
                                  (case s
                                    of M.SVariable _    => ()
                                     | M.SConstant _    => 
                                       fail ("ConstantProp::analyseInstruction'", "Un-named constant"))
                                | M.RhsPrim _           => mark ()
                                | M.RhsTuple r          => mark ()
                                | M.RhsTupleSub _       => ()
                                | M.RhsTupleSet _       => ()
                                | M.RhsTupleInited _    => ()
                                | M.RhsIdxGet _         => mark ()
                                | M.RhsCont _           => mark ()
                                | M.RhsObjectGetKind _  => mark ()
                                | M.RhsThunkMk _        => mark ()
                                | M.RhsThunkInit _      => mark ()
                                | M.RhsThunkGetFv _     => ()
                                | M.RhsThunkValue _     => mark ()
                                | M.RhsThunkGetValue _  => ()
                                | M.RhsThunkSpawn _     => ()
                                | M.RhsClosureMk _      => mark ()
                                | M.RhsClosureInit _    => mark ()
                                | M.RhsClosureGetFv _   => ()
                                | M.RhsPSetNew _        => mark ()
                                | M.RhsPSetGet _        => ()
                                | M.RhsPSetCond _       => mark ()
                                | M.RhsPSetQuery _      => mark ()
                                | M.RhsPSum _           => mark ()
                                | M.RhsPSumProj _       => ())

                       in e
                       end
                  val analyseInstruction = SOME analyseInstruction'
                  val analyseTransfer = NONE
                  val analyseBlock = NONE
                  val analyseGlobal' = 
                   fn (s, e, v, g) => 
                       let
                         val elt = 
                             (case g
                               of M.GSimple (M.SConstant c) => (v, SOME c)
                                | _                         => (v, NONE))
                         val summary = getSummary s
                         val node = MRS.variableNode (summary, v)
                         val () = FG.add (getFlowgraph s, node, CLat.elt elt)
                       in e
                       end
                  val analyseGlobal = SOME analyseGlobal'
                end)

    structure Rewrite = 
    MilRewriterF (struct
                    structure MRC = MilRewriterClient
                    type env   = env
                    type state = state
                    val config = getConfig
                    val indent = 2
                    val label       = fn _ => MRC.Stop
                    val variable    = 
                     fn (state, env, v) => 
                        let
                          val summary = getSummary state
                          val fg = getFlowgraph state
                          val node = MRS.variableNode (summary, v)
                          val vo = 
                              Try.try 
                                (fn () => 
                                    let
                                      val (v', _) = Try.<@ CLat.get (FG.query (fg, node))
                                      val () = Try.require (v <> v')
                                      val () = Click.constantProp (getPd env)
                                    in v'
                                    end)
                          val res = 
                              (case vo
                                of SOME v => MRC.StopWith (env, v)
                                 | NONE   => MRC.Stop)
                        in res
                        end
                    val operand     = fn _ => MRC.Continue
                    val instruction = fn _ => MRC.Continue
                    val transfer    = fn _ => MRC.Continue
                    val block       = fn _ => MRC.Continue
                    val global      = fn _ => MRC.Continue
                    val bind        = fn (_, env, _) => (env, NONE)
                    val bindLabel   = fn (_, env, _) => (env, NONE)
                    val cfgEnum     = fn (_, _, t) => MilUtils.CodeBody.dfsTrees t
                  end)

    val show = 
     fn (pd, summary, fg, p) => 
        if showConstantProp pd then
          let
            val si = Identifier.SymbolInfo.SiTable (MU.Program.symbolTable p)
            val vars = MRS.listVariables summary
            val props = List.map (vars, fn v => (v, FG.query (fg, MRS.variableNode (summary, v))))
            val config = PD.getConfig pd
            val le = 
             fn (v, p) => 
                (case CLat.get p
                  of SOME (v', _) => 
                     if v <> v' then
                       SOME (Layout.seq[MilLayout.layoutVariable (config, si, v), Layout.str " = ",
                                        MilLayout.layoutVariable (config, si, v')])
                     else
                       NONE
                   | NONE => NONE)
            val ls = List.keepAllMap (props, le)
            val l = Layout.align ls
            val l = Layout.align [Layout.str "Propagating:", LayoutUtils.indent l]
            val () = LayoutUtils.printLayout l
          in ()
          end
        else
          ()

    val program = 
     fn (pd, summary, p) => 
        let
          val fgF = FG.build {pd = pd,
                              forward = true,
                              summary = summary,
                              uDefInit = CLat.top,
                              uUseInit = CLat.bot, 
                              initialize = fn n => CLat.bot,
                              merge = CLat.join,
                              equal = CLat.equal elementEq
                             }
          val state = S {summary = summary, flowgraph = fgF}
          val env = E {pd = pd}
          val () = Analyze.analyseProgram (state, env, p)
          val () = FG.propagate fgF
          val () = show (pd, summary, fgF, p)
          val p = Rewrite.program (state, env, p)
        in p
        end

  end (* structure ConstantProp *)

  structure CFA = 
  struct
    val skip = MilRepBase.noCFA

    structure LS = Identifier.LabelSet
    structure VS = Identifier.VariableSet
    structure VD = Identifier.VariableDict

    structure Lat = 
    struct
      (* A set s represents the set of functions |s| where:
       *  | Empty |       = {}
       *  | Singleton v | = {v}
       *  | Set s       | = s
       *  | Any         | {f | f escapes} 
       *
       * By invariant, Set s => |s| > 1
       *)
      datatype ('a, 's) set = Empty | Singleton of 'a | Set of 's | Any of 's

      datatype t = L of (M.label, LS.t) set * (M.variable, VS.t) set

      val bot = L (Empty, Empty)

      val escaping = L (Any LS.empty, Any VS.empty)

      val label = fn l => L (Singleton l, Empty)

      val codePtr = fn v => L (Empty, Singleton v)

      val unknownCodePtr = L (Empty, Any VS.empty)

      val toSet = 
       fn (s, empty, singleton) => 
          (case s
            of Empty       => {possible = empty,       exhaustive = true}
             | Singleton v => {possible = singleton v, exhaustive = true}
             | Set s       => {possible = s,           exhaustive = true}
             | Any s       => {possible = s,           exhaustive = false})

      val toCodes = 
       fn (L (_, s)) => toSet (s, VS.empty, VS.singleton)

      (* By invariant, Set s => s not empty *)
      val empty =
       fn (L (ls, vs)) => 
          (case (ls, vs)
            of (Empty, Empty) => true
             | _              => false)

      val join' = 
       fn (s1, s2, precise, e, s, u) => 
          (case (s1, s2) 
            of (Empty, _)                 => s2
             | (_, Empty)                 => s1
             | (Any s1, s2)               => Any (u (s1, #possible (toSet (s2, e, s))))
             | (s1, Any s2)               => Any (u (#possible (toSet (s1, e, s)), s2))
             | (Singleton a, Singleton b) => if a = b then s1 else 
                                             if precise then Set (u (s a, s b)) else Any e
             | (Singleton a, Set b)       => Set (u (s a, b))
             | (Set a, Singleton b)       => Set (u (a, s b))
             | (Set a, Set b)             => Set (u (a, b)))

     (* if precise is false, then we keep track only of singleton sets *)
      val join = 
       fn precise => 
       fn (L (ls1, vs1), L (ls2, vs2)) => L (join' (ls1, ls2, precise, LS.empty, LS.singleton, LS.union),
                                             join' (vs1, vs2, precise, VS.empty, VS.singleton, VS.union))
      val equal' = 
       fn (s1, s2, se) => 
        (case (s1, s2)
          of (Empty, Empty)             => true
           | (Singleton a, Singleton b) => a = b 
           | (Set a, Set b)             => se (a, b)
           | (Any s1, Any s2)           => se (s1, s2) (*true -XXX *)
           | _                          => false)

      val equal = 
       fn (L (ls1, vs1), L (ls2, vs2)) => equal' (ls1, ls2, LS.equal) andalso
                                          equal' (vs1, vs2, VS.equal)

      val layout' = 
       fn ((s, b), sl, el) => Layout.seq [sl (s, el), if b then Layout.str "^" else Layout.str "!"]

      val layout = 
       fn (config, si, L (ls, vs)) => 
          let
            val ls = 
                let
                  val {possible, exhaustive} = toSet (ls, LS.empty, LS.singleton)
                in (possible, exhaustive)
                end
            val vs =
                let
                  val {possible, exhaustive} = toSet (vs, VS.empty, VS.singleton)
                in (possible, exhaustive)
                end
            val lbl = fn l => MilLayout.layoutLabel (config, si, l)
            val var = fn v => MilLayout.layoutVariable (config, si, v)
            val l = Layout.mayAlign [layout' (ls, LS.layout, lbl), 
                                     layout' (vs, VS.layout, var)]
          in l
          end

    end (* structure Lat *)

    datatype state = S of {summary : MRS.summary,
                           flowgraph : Lat.t FG.t}

    datatype env = E of {pd : PD.t,
                         signatures : {args : int, rets : int, thunk : bool} VD.t,
                         currentRetCount : int} 

    val getSummary = fn (S {summary, ...}) => summary
    val getFlowgraph = fn (S {flowgraph, ...}) => flowgraph
    val getPd = fn (E {pd, ...}) => pd
    val getConfig = PD.getConfig o getPd
    val getSignatures = fn (E {signatures, ...}) => signatures
    val getSignature = 
     fn (E {signatures, ...}, f) => 
        (case VD.lookup (signatures, f)
          of SOME s => s
           | NONE => fail ("CFA:getSignature", "No signature for variable"))
    val getRetCount = fn (E {currentRetCount, ...}) => currentRetCount
    val setRetCount = fn (E {pd, signatures, ...}, rc) => 
                         E {pd = pd, signatures = signatures, currentRetCount = rc}

    structure Analyze =
    MilAnalyseF(struct
                  type state = state
                  type env = env
                  val config = getConfig
                  val indent = 2
                  val variableBind = NONE
                  val labelBind = NONE
                  val variableUse = NONE
                  val analyseJump = NONE
                  val analyseCut = NONE
                  val analyseConstant = NONE
                  val analyseInstruction' = 
                   fn (s, e, M.I {dests, n, rhs}) => 
                       let
                         val summary = getSummary s
                         val addLabelV = 
                          fn l => fn v => FG.add (getFlowgraph s, MRS.variableNode (summary, v), Lat.label l)
                         val addLabel = fn l => Vector.foreach (dests, addLabelV l)
                         val addCodeV = 
                          fn f => fn v => FG.add (getFlowgraph s, MRS.variableNode (summary, v), Lat.codePtr f)
                         val addCode = fn f => Vector.foreach (dests, addCodeV f)
                         val () = 
                             (case rhs
                               of M.RhsCont l => addLabel l
                                | M.RhsThunkInit {thunk, code = SOME cptr, ...} => 
                                  let
                                    val () = Option.foreach (thunk, addCodeV cptr)
                                    val () = addCode cptr
                                  in ()
                                  end
                                | M.RhsThunkValue {thunk, ...} => 
                                  let
                                    val addCodeV = 
                                     fn v => FG.add (getFlowgraph s, MRS.variableNode (summary, v), Lat.unknownCodePtr)
                                    val () = Option.foreach (thunk, addCodeV)
                                    val () = Vector.foreach (dests, addCodeV)
                                  in ()
                                  end
                                | M.RhsClosureInit {cls, code = SOME cptr, ...} => 
                                  let
                                    val () = Option.foreach (cls, addCodeV cptr)
                                    val () = addCode cptr
                                  in ()
                                  end
                                | _           => ())
                       in e
                       end
                  val analyseInstruction = SOME analyseInstruction'
                  val analyseTransfer = NONE
                  val analyseBlock = NONE
                  val analyseGlobal' = 
                   fn (s, e, v, g) => 
                       let
                         val summary = getSummary s
                         val elt = 
                             (case g
                               of M.GClosure {code = SOME cptr, ...} => 
                                  FG.add (getFlowgraph s, MRS.variableNode (summary, v), Lat.codePtr cptr)
                                | M.GThunkValue _ => 
                                  FG.add (getFlowgraph s, MRS.variableNode (summary, v), Lat.unknownCodePtr)
                                | M.GCode _ => 
                                  FG.add (getFlowgraph s, MRS.variableNode (summary, v), Lat.codePtr v)
                                | _         => ())
                       in e
                       end
                  val analyseGlobal = SOME analyseGlobal'
                end)

    val show = 
     fn (pd, summary, fg, p) => 
        if showCFA pd then
          let
            val si = Identifier.SymbolInfo.SiTable (MU.Program.symbolTable p)
            val vars = MRS.listVariables summary
            val sets = List.map (vars, fn v => (v, FG.query (fg, MRS.variableNode (summary, v))))
            val config = PD.getConfig pd
            val le = 
             fn (v, set) => 
                if Lat.empty set then
                  NONE
                else
                  SOME (Layout.seq[MilLayout.layoutVariable (config, si, v), 
                                   Layout.str " = " ,
                                   Lat.layout (config, si, set)])
            val ls = List.keepAllMap (sets, le)
            val l = Layout.align ls
            val l = Layout.align [Layout.str "CFA:", 
                                  LayoutUtils.indent l]
            val () = LayoutUtils.printLayout l
          in ()
          end
        else
          ()

    structure Rewrite = 
    MilRewriterF (struct
                    structure MRC = MilRewriterClient
                    type env   = env
                    type state = state
                    val config = getConfig
                    val indent = 2
                    val label       = fn _ => MRC.Stop
                    val variable    = fn _ => MRC.Stop
                    val operand     = fn _ => MRC.Stop
                    val instruction = fn _ => MRC.Stop
                    val transfer    = 
                     fn (state, env, t) => 
                        let
                          val flowgraph = getFlowgraph state
                          val summary = getSummary state
                          val pd = getPd env
                          (* Both are conservative, so if either is exhaustive, we can use it,
                           * and if both are exhaustive we can take their intersection.  If
                           * neither are exhaustive, we must take care not to invalidate the
                           * "escapes" flag on something that relied on the syntactic use point,
                           * and so we take the union *)
                          val combine = 
                           fn ({possible = p1, exhaustive = e1},
                               {possible = p2, exhaustive = e2}) =>
                              (case (e1, e2) 
                                of (true, true)   => {possible = VS.intersection (p1, p2), exhaustive = true}
                                 | (false, false) => {possible = VS.union (p1, p2), exhaustive = false}
                                 | (true, false)  => {possible = p1, exhaustive = true}
                                 | (false, true)  => {possible = p2, exhaustive = true})
                          val filterCode = 
                           fn ({possible, exhaustive}, argC, retC, isThunk) => 
                              let
                                val pred = 
                                 fn v => 
                                    let
                                      val {args, rets, thunk, ...} = getSignature (env, v)
                                    in args = argC andalso rets = retC andalso thunk = isThunk 
                                    end
                                val possible = VS.keepAll (possible, pred)
                              in {possible = possible, exhaustive = exhaustive}
                              end
                          val mkCodes =
                           fn (v, oldCodes, argC, retC, isThunk) => 
                              let
                                val set = FG.query (flowgraph, MRS.variableNode (summary, v))
                                val newCodes = Lat.toCodes set
                                val newCodes = 
                                    if MilRepBase.cfaAnnotateFull pd orelse VS.size (#possible newCodes) <= 1 then
                                      newCodes
                                    else
                                      {possible = VS.empty, exhaustive = false}
                                val newCodes = combine (oldCodes, newCodes)
                                val newCodes = filterCode (newCodes, argC, retC, isThunk)
                              in newCodes
                              end
                          val getCallee =
                           fn {possible, exhaustive} => 
                              if exhaustive andalso VS.size possible = 1 then
                                VS.getAny possible
                              else
                                NONE

                          val getReturnCount =
                           fn ret => 
                              (case ret
                                of M.RTail _ => getRetCount env
                                 | M.RNormal {rets, ...} => Vector.length rets)
                          val r =  
                              (case t
                                of M.TInterProc {callee = M.IpCall {call = M.CClosure {cls, code}, args}, ret, fx} => 
                                   let
                                     val code = mkCodes (cls, code, Vector.length args, getReturnCount ret, false)
                                     val call = 
                                         (case getCallee code
                                           of SOME cptr => 
                                              let
                                                val () = Click.mkDirect (getPd env)
                                              in M.CDirectClosure {cls = cls, code = cptr}
                                              end
                                            | NONE => M.CClosure {cls = cls, code = code})
                                     val callee = M.IpCall {call = call, args = args}
                                     val t = M.TInterProc {callee = callee, ret = ret, fx = fx}
                                   in  MRC.StopWith (env, t)
                                   end
                                 | M.TInterProc {callee = M.IpCall {call = M.CCode {ptr, code}, args}, ret, fx} => 
                                   let
                                     val code = mkCodes (ptr, code, Vector.length args, getReturnCount ret, false)
                                     val call = M.CCode {ptr = ptr, code = code}
                                     val callee = M.IpCall {call = call, args = args}
                                     val t = M.TInterProc {callee = callee, ret = ret, fx = fx}
                                   in  MRC.StopWith (env, t)
                                   end
                                 | M.TInterProc {callee = M.IpEval {eval = M.EThunk {thunk, code}, typ}, ret, fx} => 
                                   let
                                     val code = mkCodes (thunk, code, 0, getReturnCount ret, true)
                                     val eval = 
                                         (case getCallee code
                                           of SOME cptr => 
                                              let
                                                val () = Click.mkDirect (getPd env)
                                              in M.EDirectThunk {thunk = thunk, code = cptr}
                                              end
                                            | NONE => M.EThunk {thunk = thunk, code = code})
                                     val callee = M.IpEval {eval = eval, typ = typ}
                                     val t = M.TInterProc {callee = callee, ret = ret, fx = fx}
                                   in  MRC.StopWith (env, t)
                                   end
                                 | _ => MRC.Stop)
                        in r
                        end
                    val block       = (fn _ => MRC.Continue)
                    val global      = (fn _ => MRC.Continue)
                    val bind        = fn (_, env, _) => (env, NONE)
                    val bindLabel   = fn (_, env, _) => (env, NONE)
                    val cfgEnum     = fn (_, _, t) => MilUtils.CodeBody.dfsTrees t
                  end)

    val getFunctionSignatures =
     fn (pd, summary, p) =>
        let
          val globals = MU.Program.globals p
          val mapper = 
           fn (v, g) => 
              (case g
                of M.GCode (M.F {args, rtyps, cc, ...}) => 
                   let
                     val args = Vector.length args
                     val rets = Vector.length rtyps
                     val thunk = case cc of M.CcThunk _ => true | _ => false
                     val si = {args = args, rets = rets, thunk = thunk}
                   in SOME si
                   end
                 | _ => NONE)
          val signatures = VD.keepAllMap (globals, mapper)
        in signatures
        end

    val program = 
     fn (pd, summary, p) => 
        let
          val precise = MilRepBase.cfaAnnotateFull pd
          val fgF = FG.build {pd = pd,
                              forward = true,
                              summary = summary,
                              uDefInit = Lat.escaping,
                              uUseInit = Lat.bot, 
                              initialize = fn n => Lat.bot,
                              merge = Lat.join precise,
                              equal = Lat.equal 
                             }
          val signatures = getFunctionSignatures (pd, summary, p) 
          val state = S {summary = summary, flowgraph = fgF}
          val env = E {pd = pd, signatures = signatures, currentRetCount = 0}
          val () = Analyze.analyseProgram (state, env, p) 
          val () = FG.propagate fgF 
          val () = show (pd, summary, fgF, p)
          val p = Rewrite.program (state, env, p)
        in p
        end

  end (* structure CFA *)


  structure EscapeAnalysis = 
  struct
    val skip = MilRepBase.noEscapeAnalysis

    structure LS = Identifier.LabelSet
    structure VS = Identifier.VariableSet
    structure VD = Identifier.VariableDict


    structure Lat = LatticeFn (struct
                                 type element = VS.t
                                 val lub = SOME o VS.intersection
                               end)

    datatype state = S of {summary : MRS.summary,
                           flowgraph : Lat.t FG.t}

    datatype env = E of {pd : PD.t} 

    val getSummary = fn (S {summary, ...}) => summary
    val getFlowgraph = fn (S {flowgraph, ...}) => flowgraph
    val getPd = fn (E {pd, ...}) => pd
    val getConfig = PD.getConfig o getPd

    structure Analyze1 =
    MilAnalyseF(struct
                  type state = state
                  type env = env
                  val config = getConfig
                  val indent = 2
                  val variableBind = NONE
                  val labelBind = NONE
                  val variableUse = NONE
                  val analyseJump = NONE
                  val analyseCut = NONE
                  val analyseConstant = NONE
                  val analyseInstruction = NONE
                  val analyseTransfer' = 
                   fn (s, e, t) => 
                      let
                        val summary = getSummary s
                        val add = 
                         fn (v, possible) => FG.add (getFlowgraph s, MRS.variableNode (summary, v), Lat.elt possible)
                        val () =
                            (case t
                              of M.TInterProc {callee, ...} => 
                                 (case callee
                                   of M.IpCall {call, ...} => 
                                      (case call
                                        of M.CCode {ptr, code}          => add (ptr, #possible code)
                                         | M.CClosure {cls, code}       => add (cls, #possible code)
                                         | M.CDirectClosure {cls, code} => (add (code, VS.singleton code);
                                                                            add (cls, VS.singleton code)))
                                    | M.IpEval {eval, ...} => 
                                      (case eval
                                        of M.EThunk {thunk, code}       => add (thunk, #possible code)
                                         | M.EDirectThunk {thunk, code} =>  (add (code, VS.singleton code);
                                                                             add (thunk, VS.singleton code))))
                               | _ => ())
                      in e
                      end
                  val analyseTransfer = SOME analyseTransfer'
                  val analyseBlock = NONE
                  val analyseGlobal = NONE
                end)

    (* Merge the closure information into the code pointer information *)
    structure Analyze2 =
    MilAnalyseF(struct
                  type state = state
                  type env = env
                  val config = getConfig
                  val indent = 2
                  val variableBind = NONE
                  val labelBind = NONE
                  val variableUse = NONE
                  val analyseJump = NONE
                  val analyseCut = NONE
                  val analyseConstant = NONE
                  val analyseInstruction' = 
                   fn (s, e, M.I {dests, n, rhs}) => 
                       let
                         val summary = getSummary s
                         val fg = getFlowgraph s
                         val addTo = 
                          fn cptr => 
                          fn v => 
                             FG.add (fg, MRS.variableNode (summary, cptr), FG.query (fg, MRS.variableNode (summary, v)))
                         val () = 
                             (case rhs
                               of M.RhsThunkInit {thunk, code = SOME cptr, ...} => 
                                  let
                                    val add = addTo cptr
                                    val () = Option.foreach (thunk, add)
                                    val () = Vector.foreach (dests, add)
                                  in ()
                                  end
                                | M.RhsClosureInit {cls, code = SOME cptr, ...} => 
                                  let
                                    val add = addTo cptr
                                    val () = Option.foreach (cls, add)
                                    val () = Vector.foreach (dests, add)
                                  in ()
                                  end
                                | _           => ())
                       in e
                       end
                  val analyseInstruction = SOME analyseInstruction'
                  val analyseTransfer = NONE
                  val analyseBlock = NONE
                  val analyseGlobal' = 
                   fn (s, e, v, g) => 
                       let
                         val summary = getSummary s
                         val fg = getFlowgraph s
                         val elt = 
                             (case g
                               of M.GClosure {code = SOME cptr, ...} => 
                                  FG.add (fg, MRS.variableNode (summary, cptr), 
                                          FG.query (fg, MRS.variableNode (summary, v)))
                                | _         => ())
                       in e
                       end
                  val analyseGlobal = SOME analyseGlobal'
                end)

    structure Rewrite = 
    MilRewriterF (struct
                    structure MRC = MilRewriterClient
                    type env   = env
                    type state = state
                    val config = getConfig
                    val indent = 2
                    val label       = fn _ => MRC.Stop
                    val variable    = fn _ => MRC.Stop
                    val operand     = fn _ => MRC.Stop
                    val instruction = fn _ => MRC.Stop
                    val transfer    = fn _ => MRC.Stop
                    val block       = fn _ => MRC.Stop
                    val global      = 
                     fn (state, env, (v, g)) => 
                        let
                         val res = 
                             (case g
                               of M.GCode (M.F {fx, escapes = true, recursive, cc, args, rtyps, body}) => 
                                  let
                                    val summary = getSummary state
                                    val fg = getFlowgraph state
                                    val pd = getPd env
                                    val info = FG.query (fg, MRS.variableNode (summary, v))
                                    val allKnown = 
                                        Lat.isBot info orelse (case Lat.get info
                                                                of SOME s => VS.member (s, v)
                                                                 | NONE   => false)
                                     val escapes = not allKnown
                                     val f = M.F {fx = fx, escapes = escapes, recursive = recursive, cc = cc, 
                                                  args = args, rtyps = rtyps, body = body}
                                     val () = if escapes then () else Click.escapeAnalysis pd
                                     val g = M.GCode f
                                   in MRC.ContinueWith (env, (v, g))
                                  end
                                | _ => MRC.Stop)
                        in res
                        end
                    val bind        = fn (_, env, _) => (env, NONE)
                    val bindLabel   = fn (_, env, _) => (env, NONE)
                    val cfgEnum     = fn (_, _, t)   => MilUtils.CodeBody.dfsTrees t
                  end)

    val program = 
     fn (pd, summary, p) => 
        let
          val fgB = FG.build {pd = pd,
                              forward = false,
                              summary = summary,
                              uDefInit = Lat.bot,
                              uUseInit = Lat.top,
                              initialize = fn n => Lat.bot,
                              merge = Lat.join,
                              equal = Lat.equal (VS.equal)
                             }
          val state = S {summary = summary, flowgraph = fgB}
          val env = E {pd = pd}
          val () = Analyze1.analyseProgram (state, env, p)
          val () = Analyze2.analyseProgram (state, env, p)
          val () = FG.propagate fgB
          val p = Rewrite.program (state, env, p)
        in p
        end

  end (* structure EscapeAnalysis *)

  val program = 
   fn (pd, summary, p) => 
      let
        val p = if Unbox.skip pd then p else Unbox.program (pd, summary, p)
        val () = if showPhases pd then MilLayout.print (PD.getConfig pd, p) else ()
        val p = if ConstantProp.skip pd then p else ConstantProp.program (pd, summary, p)
        val p = if CFA.skip pd then p else CFA.program (pd, summary, p)
        val p = if EscapeAnalysis.skip pd then p else EscapeAnalysis.program (pd, summary, p)
      in p
      end

end
