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

  structure I = Identifier
  structure M = Mil
  structure MU = MilUtils
  structure MRS = MilRepSummary
  structure MRN = MilRepNode
  structure MRB = MilRepBase
  structure FG = MilRepFlowGraph
  structure PD = PassData
  structure SS = StringSet
  structure ID = IntDict
  structure IS = IntSet
  structure EC = EquivalenceClass
  structure VS = M.VS
  structure L = Layout

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
      PD.mkDebug (passname ^ ":debug", "Debug rep analysis according to debug level")

  val mkDebug = 
   fn (tag, description, level) => PD.mkLevelDebug (passname, passname^":"^tag, description, level, debugPass)

  val debug = 
   fn (pd, i) => debugPass pd andalso (Config.debugLevel (PD.getConfig pd, passname) >= i)

  val (showUnboxingD, showUnboxing) = mkDebug ("show-unboxing", "Show unboxing analysis", 1)
  val (showConstantPropD, showConstantProp) = mkDebug ("show-constant-prop", "Show constant propagation analysis", 1)
  val (showCFAD, showCFA) = mkDebug ("show-cfa", "Show global control flow analysis", 1)
  val (showPhasesD, showPhases) = mkDebug ("show-phases", "Show IR between phases", 1)
      

  val debugs = [debugPassD, showUnboxingD, showCFAD, showConstantPropD, showPhasesD]

  val debugShow =
   fn (pd, f) => if debugPass pd then LayoutUtils.printLayout (f ()) else ()
      
  val opToVar = 
   fn c =>
      case c
       of M.SVariable v => v
        | _ => fail ("opToVar", "Not in named form")
      
  structure Unbox = 
  struct
    val skip = MilRepBase.noTupleUnbox

    structure TS = MU.TraceabilitySize

   (* Algorithm:
    *  1) Forward -> defs appropriate
    *  2) Backward -> uses appropriate
    *  3) Forward -> candidates
    *  4) For every potential unboxing a = {b}, add edge from b -> a in flow graph
    *     For every subscript from potential unboxing a = b[0] add edge from b -> a in flow graph
    *     For every def, add traceability
    *     Mark cycles Top
    *     Propagate forward
    *  5) Backward -> all defs appropriate
    *  6) Forward -> final unboxes
    *  7) Build same graph as in 4, propogate types forward
    *)
    (* Invariant: UsFix ts => TS.traceabilityKnown ts *)
    datatype unboxStat = UsTop | UsBox | UsFix of TS.t | UsBot

    val layoutUs = 
     fn (config, us) => 
        case us 
         of UsTop    => L.str "T"
          | UsBot    => L.str "B"
          | UsBox    => L.str "BX"
          | UsFix ts => L.seq [L.str "Fix", L.str (TS.toString (config, ts))]

    val usIsTop = fn us => (case us of UsTop => true 
                                     | _     => false)
    val joinUS = 
     fn (us1, us2) => 
        case (us1, us2)
         of (UsTop  , _      ) => UsTop
          | (_      , UsTop  ) => UsTop
          | (UsBot  , _      ) => us2
          | (_      , UsBot  ) => us1
          | (UsBox  , UsBox  ) => UsBox
          | (UsFix s, UsFix t) => if TS.eq (s, t) then UsFix s else UsTop
          | (UsBox  , UsFix t) => if TS.isRef t then UsBox else UsTop
          | (UsFix s, UsBox  ) => if TS.isRef s then UsBox else UsTop

    val typFromUs = 
     fn us => 
        case us
         of UsTop   => fail ("typFromUs", "Shouldn't have unboxed top node")
          | UsBot   => fail ("typFromUs", "Shouldn't have unboxed bottom node")
          | UsFix t => MU.Typ.fromTraceabilitySize t
          | UsBox   => MU.Typ.fromTraceabilitySize TS.TsRef

    val fkFromUs = 
     fn us => 
        case us
         of UsTop   => fail ("typFromUs", "Shouldn't have unboxed top node")
          | UsBot   => fail ("typFromUs", "Shouldn't have unboxed bottom node")
          | UsFix t => MU.FieldKind.fromTraceSize' t
          | UsBox   => SOME M.FkRef

    val mkUs = 
     fn (box, ts) => 
        if box andalso TS.isRef ts then
          UsBox
        else if TS.known ts then
          UsFix ts
        else
          UsTop

    structure SE1 = 
    struct

      datatype state = S of {summary : MilRepSummary.summary,
                             si : Mil.symbolInfo,
                             ccs : int MRN.Dict.t,
                             ecs : unboxStat EC.t ID.t,
                             unboxed : IS.t ref}

      datatype env = E of {pd : PD.t}

      val ((setSummary, getSummary),
           (setSi, getSi),
           (setCcs, getCcs),
           (setEcs, getEcs),
           (setUnboxed, getUnboxed)) = 
          let
            val r2t = fn S {summary, si, ccs, ecs, unboxed} => (summary, si, ccs, ecs, unboxed)
            val t2r = fn (summary, si, ccs, ecs, unboxed) => 
                         S {summary = summary, si = si, ccs = ccs, ecs = ecs, unboxed = unboxed} 
          in FunctionalUpdate.mk5 (r2t, t2r)
          end

      val getPd = fn (E {pd, ...}) => pd

      val getConfig = PD.getConfig o getPd

    end (* structure SE1 *) 

    val layoutVariable = 
     fn (s, e, v) => MilLayout.layoutVariable (SE1.getConfig e, SE1.getSi s, v)

    val nodeForVariable = 
     fn (s, e, v) => MRS.variableNode (SE1.getSummary s, v)

    val typForVariable = 
     fn (s, e, v) => MilType.Typer.variable (SE1.getConfig e, SE1.getSi s, v)

    val tsForVariable = 
     fn (s, e, v) => MU.Typ.traceabilitySize (SE1.getConfig e, typForVariable (s, e, v))

    val ccForNode = 
     fn (s, e, n) => 
        (case MRN.Dict.lookup (SE1.getCcs s, n)
          of SOME cc => cc
           | NONE    => fail ("ccForNode", "Bad node"))

    val ccForVar = 
     fn (s, e, v) => ccForNode (s, e, nodeForVariable (s, e, v))

    val ecForNode = 
     fn (s, e, n) => 
        (case ID.lookup (SE1.getEcs s, ccForNode (s, e, n))
          of SOME cc => cc
           | NONE    => fail ("ecForNode", "Bad node"))
 
    val ecForVar =
     fn (s, e, v) => ecForNode (s, e, nodeForVariable (s, e, v))

    val addToEcForNode = 
     fn (s, e, n, l) =>
        let
          val ec = ecForNode (s, e, n)
          val () = EC.set (ec, joinUS (l, EC.get ec))
        in ()
        end

    val addToEcForVar = 
     fn (s, e, v, l) => addToEcForNode (s, e, nodeForVariable (s, e, v), l)

    val addFixedNode =
     fn (s, e, n, ts) => addToEcForNode (s, e, n, mkUs (false, ts))

    val addFixed =
     fn (s, e, v) => 
        let
          val ts = tsForVariable (s, e, v)
          val () = addToEcForVar (s, e, v, mkUs (false, ts))
        in ()
        end

    val addBoxedNode =
     fn (s, e, n, ts) => addToEcForNode (s, e, n, mkUs (true, ts))

    val addBoxed =
     fn (s, e, v) => 
        let
          val ts = tsForVariable (s, e, v)
          val () = addToEcForVar (s, e, v, mkUs (true, ts))
        in ()
        end

    val addTopNode =
     fn (s, e, n) => addToEcForNode(s, e, n, UsTop)

    val addUnboxedCc =
     fn (s, e, i) => 
        let
          val unboxed = SE1.getUnboxed s
          val () = unboxed := IS.insert (!unboxed, i)
        in ()
        end

    val addUnboxed =
     fn (s, e, v) => addUnboxedCc (s, e, ccForVar (s, e, v))

    val addUnboxedNode = 
     fn (s, e, n) => addUnboxedCc (s, e, ccForNode (s, e, n))

    val ccIsUnboxed = 
     fn (s, e, i) => IS.member (!(SE1.getUnboxed s), i)

    val nodeIsUnboxed = 
     fn (s, e, n) => ccIsUnboxed (s, e, ccForNode (s, e, n))

    val varIsUnboxed = 
     fn (s, e, v) => ccIsUnboxed (s, e, ccForVar (s, e, v))

    structure Analyze1 =
    MilAnalyseF(struct
                  type state = SE1.state
                  type env = SE1.env
                  val config = SE1.getConfig
                  val indent = 2
                  val externBind = NONE
                  val variableBind = NONE
                  val labelBind = NONE
                  val variableUse = NONE
                  val analyseJump = NONE
                  val analyseCut = NONE
                  val analyseConstant = NONE
                  val analyseInstruction' = 
                   fn (s, e, M.I {dests, n, rhs}) => 
                       let
                         val fixed = fn () => Vector.foreach (dests, fn v => addFixed (s, e, v))
                         val boxed = fn v => addBoxed (s, e, v)
                         val boxedV = fn vv => Vector.foreach (vv, boxed)
                         val boxedO = boxed o opToVar
                         val boxedOV = fn vv => Vector.foreach (vv, boxedO)
                         val () = 
                             (case rhs
                               of M.RhsSimple s         => 
                                  (case s
                                    of M.SVariable _    => ()
                                     | _                => fixed ())
                                | M.RhsPrim r           => 
                                  let
                                    val () = fixed ()
                                    val () = boxedOV (#args r)
                                  in ()
                                  end
                                | M.RhsTuple r          => if Vector.length (#inits r) > 0 then () else boxedV dests
                                | M.RhsTupleSub tf       => 
                                  (case MU.FieldIdentifier.Dec.fiFixed (MU.TupleField.field tf)
                                    of SOME 0 => ()
                                     | _      => boxed (MU.TupleField.tup tf))
                                | M.RhsTupleSet r       => boxed (MU.TupleField.tup (#tupField r))
                                | M.RhsTupleInited _    => ()
                                | M.RhsIdxGet _         => fixed ()
                                | M.RhsCont _           => fixed ()
                                | M.RhsObjectGetKind v  => 
                                  let
                                    val () = fixed ()
                                    val () = boxed v
                                  in ()
                                  end
                                | M.RhsThunkMk _        => fixed ()
                                | M.RhsThunkInit _      => fixed ()
                                | M.RhsThunkGetFv _     => ()
                                | M.RhsThunkValue _     => fixed ()
                                | M.RhsThunkGetValue _  => ()
                                | M.RhsThunkSpawn _     => ()
                                | M.RhsClosureMk _      => fixed ()
                                | M.RhsClosureInit _    => fixed ()
                                | M.RhsClosureGetFv _   => ()
                                | M.RhsPSetNew p        => let val () = fixed () val () = boxedO p in () end
                                | M.RhsPSetGet _        => ()
                                | M.RhsPSetCond r       => let val () = fixed () val () = boxedO (#ofVal r) in () end
                                | M.RhsPSetQuery _      => fixed ()
                                | M.RhsPSum _           => fixed ()
                                | M.RhsPSumProj _       => ())

                       in e
                       end
                  val analyseInstruction = SOME analyseInstruction'
                  val analyseTransfer' = 
                   fn (s, e, t) => 
                      let 
                        val boxed = fn v => addBoxed (s, e, v)
                        val boxedO = boxed o opToVar
                        (* We may compare pointers to various cref constants *)
                        val () = 
                            (case t
                              of M.TCase {on , ...} => boxedO on
                               | _ => ())
                      in e
                      end

                  val analyseTransfer = SOME analyseTransfer'
                  val analyseBlock = NONE
                  val analyseGlobal' = 
                   fn (s, e, v, g) => 
                       let
                         val fixed = fn () => addFixed (s, e, v)
                         val boxed = fn v => addBoxed (s, e, v)
                         val () = 
                             (case g
                               of M.GTuple r                => 
                                  if Vector.length (#inits r) > 0 then () else boxed v
                                | M.GSimple (M.SVariable _) => ()
                                | M.GErrorVal t             => ()
                                | M.GPSet s                 => let val () = fixed () val () = boxed (opToVar s) 
                                                               in () end
                                | _                         => fixed ())
                       in e
                       end
                  val analyseGlobal = SOME analyseGlobal'
                end)

    val tryToUnboxTuple = 
     fn (s, e, v1, inits) =>
        Try.exec 
          (fn () => 
              let
                val because = 
                 fn st => 
                    let
                      val f = fn () => L.seq [L.str "Can't unbox ", layoutVariable (s, e, v1), L.str ": ", L.str st]
                    in debugShow (SE1.getPd e, f)
                    end
                val fail = fn s => let val () = because s in Try.fail () end
                val require = fn (b, s) => if b then () else fail s
                val () = require (Vector.length inits > 0, "not enough elements")
                val v2 = opToVar (Vector.sub (inits, 0))
                val ec1 = ecForVar (s, e, v1)
                val ec2 = ecForVar (s, e, v2)
                val () = require (not (EC.equal (ec1, ec2)), "same ec")
                val us1 = EC.get ec1
                val us2 = EC.get ec2
                val () = 
                    case us1
                     of UsTop => fail "top"
                      | UsBox => fail "box"
                      | _     => 
                        let
                          val us = joinUS (us1, us2)
                          val () = require (not (usIsTop us), "incompatible")
                          val _  = EC.join (ec1, ec2)
                          val () = EC.set (ec1, us)
                          val () = addUnboxed (s, e, v1)
                        in ()
                        end
              in ()
              end)

    structure Analyze2 =
    MilAnalyseF(struct
                  type state = SE1.state
                  type env = SE1.env
                  val config = SE1.getConfig
                  val indent = 2
                  val externBind = NONE
                  val variableBind = NONE
                  val labelBind = NONE
                  val variableUse = NONE
                  val analyseJump = NONE
                  val analyseCut = NONE
                  val analyseConstant = NONE
                  val analyseInstruction' = 
                   fn (s, e, M.I {dests, n, rhs}) => 
                       let
                         val () = 
                             case rhs
                              of M.RhsTuple r => tryToUnboxTuple (s, e, Vector.sub (dests, 0), #inits r)
                               | _            => ()
                       in e
                       end
                  val analyseInstruction = SOME analyseInstruction'
                  val analyseTransfer = NONE
                  val analyseBlock = NONE
                  val analyseGlobal' = 
                   fn (s, e, v, g) => 
                       let
                         val () = 
                             case g
                              of M.GTuple r => tryToUnboxTuple (s, e, v, #inits r)
                               | _          => ()
                       in e
                       end
                  val analyseGlobal = SOME analyseGlobal'
                end)

    val unboxTuple = 
     fn (s, e, v1, inits) => 
        let
          val summary = SE1.getSummary s
          val oper = Vector.sub (inits, 0)
          val v2 = opToVar oper
          val n1 = nodeForVariable (s, e, v1)
          val n2 = nodeForVariable (s, e, v2)
          val edge = (n2, n1)
          val () = MRS.addEdge (summary, edge)
          val () = Click.unboxTuple (SE1.getPd e)
        in oper
        end


    structure Rewrite = 
    MilTransformF (struct
                    structure MS = MilStream
                    type env   = SE1.env
                    type state = SE1.state
                    val config = SE1.getConfig
                    val indent = 2
                    val label       = fn (_, env, _, _) => (env, NONE)
                    val instr = 
                     fn (s, e, i as M.I {dests, n, rhs}) => 
                        let
                          val summary = SE1.getSummary s
                          val so = 
                              (case rhs
                                of M.RhsTuple r => 
                                   if varIsUnboxed (s, e, Vector.sub (dests, 0)) then
                                     let
                                       val oper = unboxTuple (s, e, Vector.sub (dests, 0), #inits r)
                                       val s = MS.instruction (M.I {dests = dests, n = n, rhs = M.RhsSimple oper})
                                     in SOME s
                                     end
                                   else 
                                     NONE
                                 | M.RhsTupleSub tf => 
                                   let
                                     val v1 = MU.TupleField.tup tf
                                     val res = 
                                         if varIsUnboxed (s, e, v1) then
                                           let
                                             val n1 = MRS.variableNode (summary, v1)
                                             val rhs = M.RhsSimple (M.SVariable v1)
                                             val v2 = Vector.sub (dests, 0)
                                             val n2 = MRS.variableNode (summary, v2)
                                             val edge = (n1, n2)
                                             val () = MRS.addEdge (summary, edge)
                                             val i = M.I {dests = dests, n = n, rhs = rhs}
                                             val s = MS.instruction i
                                           in SOME s
                                           end
                                         else
                                           NONE
                                   in res
                                   end
                                 | M.RhsTupleInited {tup, ...} => 
                                   if varIsUnboxed (s, e, tup) then
                                     SOME MS.empty
                                   else 
                                     NONE
                                 | _ => NONE)
                        in (e, so)
                        end
                    val transfer    = fn (_, env, _) => (env, NONE)
                    val global      = 
                     fn (s, e, v, g) => 
                        let
                          val summary = SE1.getSummary s
                          val so = 
                              case g
                               of M.GTuple r => 
                                  if varIsUnboxed (s, e, v) then 
                                    let
                                      val oper = unboxTuple (s, e, v, #inits r)
                                      val l = [(v, M.GSimple oper)]
                                    in SOME l
                                    end
                                  else
                                    NONE
                                | M.GErrorVal t => 
                                  if varIsUnboxed (s, e, v) then
                                    let
                                      val t = typFromUs (EC.get (ecForVar (s, e, v)))
                                      val l = [(v, M.GErrorVal t)]
                                    in SOME l
                                    end
                                  else
                                    NONE
                                | _ => NONE
                        in (e, so)
                        end
                   end)

    val doUnknowns = 
     fn (s, e) => 
        let
          val summary = SE1.getSummary s
          val doNode = 
           fn (n, t) =>  
              if MRN.usesKnown n andalso MRN.defsKnown n then ()
              else
                let
                  val ts = MU.Typ.traceabilitySize (SE1.getConfig e, t)
                  val () = addBoxedNode (s, e, n, ts)
                in ()
                end
          val doReturns =
           fn (ns, ts) => Vector.foreach2 (ns, ts, doNode)
          val doIt = 
           fn v => 
              let
                val () =
                    case MRS.iInfo' (summary, MU.Id.G v)
                     of SOME (MRB.IiCode r) => 
                        (case typForVariable (s, e, v) 
                          of M.TCode {cc, args, ress} => doReturns (#returns r, ress)
                           | _                        => Vector.foreach (#returns r, fn n => addTopNode (s, e, n)))
                      | _                   =>  ()
                val n = nodeForVariable (s, e, v)
                val t = typForVariable (s, e, v)
                val () = doNode (n, t)
              in ()
              end
          val variables = MRS.listVariables summary
          val () = List.foreach (variables, doIt)
        in ()
        end
    
    val analyze = 
     fn (s, e, p) => 
        let
          val () = doUnknowns (s, e)
          val () = Analyze1.analyseProgram (s, e, p)
          val () = Analyze2.analyseProgram (s, e, p)
        in ()
        end

    val show = 
     fn (s, e, p) => 
        if showUnboxing (SE1.getPd e) then
          let
            val si = Identifier.SymbolInfo.SiTable (MU.Program.symbolTable p)
            val lv = fn v => MilLayout.layoutVariable (SE1.getConfig e, si, v)
            val vars = MRS.listVariables (SE1.getSummary s)
            val components = 
                let
                  val add = fn (v, components) => 
                               case ID.lookup (components, ccForVar (s, e, v))
                                of SOME component => ID.insert (components, ccForVar (s, e, v), v::component)
                                 | NONE           => ID.insert (components, ccForVar (s, e, v), [v])
                  val components = List.fold (vars, ID.empty, add)
                  val lc = fn (i, component) => L.seq [Int.layout i, L.str " => ", List.layout lv component]
                in List.map (ID.toList components, lc)
                end
            val info = 
                let
                  val p = 
                   fn v => 
                      let
                        val cc = Int.layout (ccForVar (s, e, v))
                        val ec = layoutUs (SE1.getConfig e, EC.get (ecForVar (s, e, v)))
                      in
                        L.seq [lv v, L.str " is ", cc, L.str " with status ", ec]
                      end
                in List.map (vars, p)
                end
            val unboxes = 
                List.keepAllMap (vars, fn v => if varIsUnboxed (s, e, v) then SOME (lv v) else NONE)
            val l = Layout.align [Layout.str "Components:", 
                                  LayoutUtils.indent (Layout.align components),
                                  Layout.str "Variables:", 
                                  LayoutUtils.indent (Layout.align info),
                                  Layout.str "Unboxing:", 
                                  LayoutUtils.indent (Layout.align unboxes)]
            val () = LayoutUtils.printLayout l
          in ()
          end
        else
          ()


    val replaceNodeDataWithoutShape = 
     fn (s, e, done, n) => 
        let
          val us = EC.get (ecForNode (s, e, n))
          val t = typFromUs us
          val fk = fkFromUs us
          val fv = MilRepNode.fieldVariance' n
          val shape = SOME (MilRepObject.Shape.Build.unknown t)
          val () = MilRepNode.setData (n, shape, fk, fv)
          val done = MRN.Set.insert (done, n)
        in done
        end

    val rec replaceNodeDataWithShape = 
     fn (s, e, done, n, shp) => 
        let   
          val fallback = 
           fn () => replaceNodeDataWithoutShape (s, e, done, n)

          val replace = 
           fn inner => 
              let
                val done = replaceNodeData (s, e, done, inner)
                val fk = MilRepNode.fieldKind' inner
                val fv = MilRepNode.fieldVariance' n
                val shape = MilRepNode.shape' inner
                val () = MilRepNode.setData (n, shape, fk, fv)
                val done = MRN.Set.insert (done, n)
              in done
              end
          val done = 
              case MilRepObject.Shape.Dec.tuple shp
               of NONE => fallback ()
                | SOME {pok, fields, array} => 
                  (case Utils.Vector.lookup (fields, 0)
                    of NONE => 
                       (case array 
                         of SOME inner => replace inner
                          | NONE => fallback ())
                     | SOME inner => replace inner)
        in done
        end

    and rec replaceNodeData = 
     fn (s, e, done, n) => 
        if MRN.Set.member (done, n) then done else
        let
          val done = 
              if nodeIsUnboxed (s, e, n) then
                case MilRepNode.shape' n
                 of SOME sh => replaceNodeDataWithShape (s, e, done, n, sh)
                  | NONE    => replaceNodeDataWithoutShape (s, e, done, n)
              else
                done
        in done
        end

    val replaceAllNodeData = 
     fn (s, e) =>
        let
          val summary = SE1.getSummary s
          val nodes = MRS.nodes summary
          val help = fn (i, n, done) => replaceNodeData (s, e, done, n)
          val _ = ID.fold (nodes, MRN.Set.empty, help)
        in ()
        end

    val rewrite = 
     fn (s, e, p) => 
        let
          val p = Rewrite.program (s, e, Rewrite.OAny, p)
          val () = replaceAllNodeData (s, e)
        in p
        end

    val program = 
     fn (pd, summary, p) => 
        let
          val M.P {symbolTable, ...} = p
          val fg = FG.build {pd = pd,
                             forward = true,
                             summary = summary,
                             uDefInit = NONE,
                             uUseInit = NONE, 
                             initialize = fn n => (),
                             merge = fn _ => (),
                             equal = fn _ => true
                            }

          val ccs = FG.cc (summary, fg)
          val ecs = 
              let
                val addCC = 
                 fn (n, i, ecs) => if ID.contains (ecs, i) then ecs else ID.insert (ecs, i, EC.new UsBot)
              in MRN.Dict.fold (ccs, ID.empty, addCC)
              end
          val s = SE1.S {summary = summary, si = I.SymbolInfo.SiTable symbolTable,
                         ccs = ccs, ecs = ecs, unboxed = ref IS.empty}
          val e = SE1.E {pd = pd}
          val () = analyze (s, e, p)
          val () = show (s, e, p)
          val p = rewrite (s, e, p)
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
                  val externBind = NONE
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
                              uDefInit = SOME CLat.top,
                              uUseInit = SOME CLat.bot, 
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
       fn ((s, b), sl, el) => Layout.seq [sl (s, el), if b then Layout.str "!" else Layout.str "^"]

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
                  val externBind = NONE
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
                         val () = 
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
                    val global      = 
                     fn (state, env, (v, g)) => 
                        (case g
                          of M.GCode (M.F {rtyps, ...}) => MRC.ContinueWith (setRetCount (env, Vector.length rtyps), (v, g))
                           | _                          => MRC.Continue)
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
                              uDefInit = SOME Lat.escaping,
                              uUseInit = SOME Lat.bot, 
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
                  val externBind = NONE
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
                  val externBind = NONE
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
                              uDefInit = SOME Lat.bot,
                              uUseInit = SOME Lat.top,
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
