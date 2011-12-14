(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_THUNK_OPTIMIZE =
sig
  val debugs : Config.Debug.debug list
  val stats : (string * string) list
  val pass : (BothMil.t, BothMil.t) Pass.t
end;

structure MilThunkOptimize :> MIL_THUNK_OPTIMIZE =
struct

  val passname = "MilThunkOptimize"
  val fail = 
   fn (fname, msg) => Fail.fail ("thunks.sml", fname, msg)

  structure M = Mil
  structure MU = MilUtils
  structure PD = PassData
  structure L = Layout
  structure LU = LayoutUtils
  structure VD = M.VD
  structure VS = M.VS
  structure I = Identifier
  structure MSTM = MU.SymbolTableManager
  structure MS = MilStream
  structure MF = MilFragment
  structure LD = Mil.LD
  structure MCC = MilCodeCopy
  structure MFV = MilFreeVars

  structure Chat = ChatF (struct 
                            type env = PD.t
                            val extract = PD.getConfig
                            val name = passname
                            val indent = 2
                          end)

  structure Click = 
  struct
    val stats = []
    val {stats, click = thunkVal} = PD.clicker {stats = stats, passname = passname,
                                                name = "ThunkVal", desc = "ThunkVals created"}
  end (* structure Click *)

  val mkDebug = 
   fn (tag, description) => PD.mkDebug (passname^":"^tag, description)

  val (debugPassD, debugPass) =
      mkDebug ("debug", "Debug thunk optimize according to debug level")

  val mkLevelDebug = 
   fn (tag, description, level) => PD.mkLevelDebug (passname, passname^":"^tag, description, level, debugPass)

  val (checkPhasesD, checkPhases) =
      mkLevelDebug ("check-phases", "Check IR between each phase", 0)

  val (showPhasesD, showPhases) =
      mkLevelDebug ("show-phases", "Show IR between each phase", 0)

  val (skipThunkValD, skipThunkVal) =
      mkDebug ("skip-thunk-val", "Don't optimize")

  val (showThunkValD, showThunkVal) = 
      mkLevelDebug ("show-thunk-val", "Show thunk val analysis", 0)

  val debugs = [debugPassD, checkPhasesD, showThunkValD, showPhasesD, skipThunkValD] 

  val mkLogFeature = 
   fn (tag, description, level) => PD.mkLogFeature (passname, passname^":"^tag, description, level)

  val (statPhasesF, statPhases) = 
      mkLogFeature ("stat-phases", "Show stats between each phase", 2)

  val features = [statPhasesF] 

  val stats = Click.stats

  structure ThunkVal =
  struct
    structure DG = DepGraph

    datatype state = S of {done : M.code VD.t ref,              (* rewritten code blocks*)
                           nodes : unit DepGraph.t VD.t ref,    (* dependency graph nodes, 1 per code pointer *)
                           stm   : M.symbolTableManager,
                           thunks : M.variable List.t VD.t ref, (* mapping from code pointers to thunks they implement*)
                           todo : M.code VD.t ref,              (* code blocks left to be rewritten*)
                           values : VS.t ref                    (* thunks which are valuable *)
                          }

    datatype env = E of {curFun : M.variable, pd : PD.t}

    val ((stateSetDone, stateGetDone),
         (stateSetNodes, stateGetNodes),
         (stateSetStm, stateGetStm),
         (stateSetThunks, stateGetThunks),
         (stateSetTodo, stateGetTodo),
         (stateSetValues, stateGetValues)) = 
        let
          val r2t = fn S {done, nodes, stm, thunks, todo, values} => (done, nodes, stm, thunks, todo, values)
          val t2r = fn (done, nodes, stm, thunks, todo, values) => 
                       S {done = done, nodes = nodes, stm = stm, thunks = thunks, todo = todo, values = values}
        in FunctionalUpdate.mk6 (r2t, t2r)
        end

    val ((envSetCurFun, envGetCurFun),
         (envSetPd, envGetPd)
        ) = 
        let
          val r2t = fn E {curFun, pd} => (curFun, pd)
          val t2r = fn (curFun, pd) => E {curFun = curFun, pd = pd}
        in FunctionalUpdate.mk2 (r2t, t2r)
        end

    val envGetConfig = PD.getConfig o envGetPd

    val getPd = 
     fn (state, env) => envGetPd env

    val getConfig = 
     fn (state, env) => envGetConfig env

    val getStm = 
     fn (state, env) => stateGetStm state

    val getSi = 
     fn (state, env) => I.SymbolInfo.SiManager (getStm (state, env))


    val getNodeForVariable = 
     fn (state, env, v) => 
        let
          val nodes as ref nd = stateGetNodes state
        in
          case VD.lookup (nd, v)
           of SOME n => n
            | NONE   => 
              let
                val n = ref (DG.DgTop NONE)
                val () = nodes := VD.insert (nd, v, n)
              in n
              end
        end

    val getCurrentFunction =
     fn (state, env) => envGetCurFun env

    val setCurrentFunction =
     fn (state, env, v) => envSetCurFun (env, v)

    val getCurrentFunctionNode = 
     fn (state, env) => getNodeForVariable (state, env, getCurrentFunction (state, env))

    val chooseCodeTodo = 
     fn (state, env) =>
        let
          val todo as ref cd = stateGetTodo state
        in case VD.choose cd
            of SOME (cd, v, c) => 
               let
                 val () = todo := cd
               in SOME (v, c)
               end
             | NONE => NONE
        end

    val takeCodeFromTodo = 
     fn (state, env, v) =>
        let
          val todo as ref cd = stateGetTodo state
        in case VD.lookup (cd, v)
            of SOME c => 
               let
                 val () = todo := VD.remove (cd, v)
               in SOME c
               end
             | NONE => NONE
        end

    val addCodeToTodo = 
     fn (state, env, v, c) =>
        let
          val todo as ref cd = stateGetTodo state
          val () = todo := VD.insert (cd, v, c)
        in ()
        end

    val getCodeFromDone = 
     fn (state, env, v) =>
        let
          val done as ref cd = stateGetDone state
        in VD.lookup (cd, v)
        end

    val addCodeToDone = 
     fn (state, env, v, c) =>
        let
          val done as ref cd = stateGetDone state
          val () = done := VD.insert (cd, v, c)
        in ()
        end

    val codePtrIsValuable = 
     fn (state, env, v) => 
        let
          val nodes as ref nd = stateGetNodes state
        in
          case VD.lookup (nd, v)
           of SOME (ref (DG.DgTop _)) => true
            | _                       => false
        end

    val thunkIsValuable = 
     fn (state, env, v) => 
        let
          val values as ref vs = stateGetValues state
        in VS.member (vs, v)
        end

    val addThunkForCodePtr = 
     fn (state, env, ptr, thunk) => 
        let
          val thunks as ref td = stateGetThunks state
          val l = case VD.lookup (td, ptr)
                   of SOME l => l
                    | NONE   => [thunk]
          val () = thunks := VD.insert (td, ptr, l)
        in ()
        end

    val init = 
     fn (pd, p) =>
        let
          val M.P {symbolTable, entry, ...} = p
          val stm = Identifier.Manager.fromExistingAll symbolTable
          val env = E {curFun = entry, pd = pd}
          val state = S {done = ref VD.empty, nodes = ref VD.empty, stm = stm, 
                         thunks = ref VD.empty, todo = ref VD.empty, values = ref VS.empty}
        in (state, env)
        end

    structure Analyze = 
    MilAnalyseF(struct
                  type state = state
                  type env = env
                  val config = envGetConfig
                  val indent = 2
                  val externBind = NONE
                  val variableBind = NONE
                  val labelBind = NONE
                  val variableUse = NONE
                  val analyseJump = NONE
                  val analyseCut = NONE
                  val analyseConstant = NONE
                  val currentFunctionIsComputation = 
                   fn (s, e) => 
                      let
                        val r = getCurrentFunctionNode (s, e)
                        val () = r := DG.DgBot NONE
                      in ()
                      end
                  val currentFunctionDependsOn = 
                   fn (s, e, v) => 
                      let
                        val r1 = getCurrentFunctionNode (s, e)
                        val r2 = getNodeForVariable (s, e, v)
                        val () = DG.both (r1, r2)
                      in ()
                      end
                  val analyseInstruction' = 
                   fn (s, e, M.I {dests, n, rhs}) => 
                       let
                         val computation = fn () => currentFunctionIsComputation (s, e)
                         val depends = fn v => currentFunctionDependsOn (s, e, v)
                         val () = 
                             case rhs
                              of M.RhsSimple s         => ()
                               | M.RhsPrim _           => computation ()
                               | M.RhsTuple r          => ()
                               | M.RhsTupleSub tf      => computation ()
                               | M.RhsTupleSet r       => computation ()
                               | M.RhsTupleInited r    => computation ()
                               | M.RhsIdxGet _         => computation ()
                               | M.RhsCont _           => ()
                               | M.RhsObjectGetKind _  => computation ()
                               | M.RhsThunkMk _        => ()
                               | M.RhsThunkInit r      => 
                                 (case #code r
                                   of NONE     => ()
                                    | SOME ptr => 
                                      let
                                        val ()= 
                                            case #thunk r 
                                             of SOME t => addThunkForCodePtr (s, e, ptr, t)
                                              | NONE   => addThunkForCodePtr (s, e, ptr, Vector.sub (dests, 0))
                                        val () = depends ptr
                                      in ()
                                      end)
                               | M.RhsThunkGetFv r     => computation ()
                               | M.RhsThunkValue r     => ()
                               | M.RhsThunkGetValue _  => computation ()
                               | M.RhsThunkSpawn _     => computation ()
                               | M.RhsClosureMk _      => ()
                               | M.RhsClosureInit r    => ()
                               | M.RhsClosureGetFv r   => computation ()
                               | M.RhsPSetNew _        => ()
                               | M.RhsPSetGet _        => computation ()
                               | M.RhsPSetCond r       => ()
                               | M.RhsPSetQuery _      => computation ()
                               | M.RhsEnum _           => ()
                               | M.RhsSum _            => ()
                               | M.RhsSumProj _        => computation ()
                               | M.RhsSumGetTag _      => computation ()

                       in e
                       end
                  val analyseInstruction = SOME analyseInstruction'
                  val analyseTransfer' = 
                   fn (s, e, lo, t) => 
                      let 
                        val computation = fn () => currentFunctionIsComputation (s, e)
                        (* Could allow some control flow, but need to account for loops.
                         * Could consider allowing calls/evals of valuable functions. *)
                        val () = 
                            (case t
                              of M.TGoto _      => computation ()
                               | M.TCase r      => computation () 
                               | M.TInterProc r => computation ()
                               | M.TReturn _    => ()
                               | M.TCut r       => computation ()
                               | M.THalt p      => computation ())
                      in e
                      end
                  val analyseTransfer = SOME analyseTransfer'
                  val analyseBlock = NONE
                  val analyseGlobal' = 
                   fn (s, e, v, g) => 
                       let
                         val e = 
                             (case g
                               of M.GCode c => 
                                  let
                                    val e = setCurrentFunction (s, e, v)
                                    val () = addCodeToTodo (s, e, v, c)
                                  in e
                                  end
                                | _         => e)
                       in e
                       end
                  val analyseGlobal = SOME analyseGlobal'
                end)

    (* Decide each node of the dependency graph, 
     * and add all of the thunks associated with any valuable code
     * pointers to the value set 
     *)
    val finalize =
     fn (state, env) =>
        let
          val nodes as ref nd = stateGetNodes state
          val values = stateGetValues state
          val thunks as ref td = stateGetThunks state
          val decide = 
           fn (v, n, vs) => 
              let
                val () = DG.decide n
                val vs = 
                    case !n
                     of DG.DgTop _ => 
                        (case VD.lookup (td, v)
                          of SOME ts => VS.insertList (vs, ts)
                           | NONE    => vs)
                      | _          => vs
              in vs
              end
          val vs = VD.fold (nd, VS.empty, decide)
          val () = values := vs
        in ()
        end

    val analyze = 
     fn (pd, p) => 
        let
          val (state, env) = init (pd, p)
          val () = Analyze.analyseProgram (state, env, p)
          val () = finalize (state, env)
        in (state, env)
        end

    val show =
     fn (state, env, p) => 
        if showThunkVal (getPd (state, env)) then
          let
            val config = getConfig (state, env)
            val si = getSi (state, env)
            val nodes as ref nd = stateGetNodes state
            val nd = VD.keepAllMap (nd, fn (v, nd) => case !nd
                                                       of DG.DgTop _ => SOME ()
                                                        | _          => NONE)
            val l = VD.layout (nd, fn (v, _) => MilLayout.layoutVariable (config, si, v))
            val () = LU.printLayout l
          in ()
          end
        else
          ()

    structure Rewrite = 
    struct

      val inlineTransfer = 
       fn (state, env, mergeL, t) => 
          (case t
            of M.TGoto _       => t
             | M.TCase _       => t
             | M.TInterProc _  => fail ("inlineTransfer", "Not a value")
             | M.TReturn opers => M.TGoto (M.T {block = mergeL, arguments = opers})
             | M.TCut _        => fail ("inlineTransfer", "Not a value")
             | M.THalt _       => t)

      val inlineCodeBlock =
       fn (state, env, mergeL, M.B {parameters, instructions, transfer}) => 
          M.B {parameters = parameters, instructions = instructions, 
               transfer = inlineTransfer (state, env, mergeL, transfer)}

      val inlineCodeBody = 
       fn (state, env, formals, actuals, M.CB {entry, blocks}, resV) => 
          let
            val config = getConfig (state, env)
            val stm = getStm (state, env)
            val preEntryL = MSTM.labelFresh stm
            val mergeL = MSTM.labelFresh stm
            val s = 
                let
                  val t1 = M.TGoto (M.T {block = preEntryL, arguments = actuals})
                  val s1 = MS.transfer (t1, preEntryL, formals)
                  val t2 = M.TGoto (M.T {block = entry, arguments = Vector.new0 ()})
                  val s2 = MS.appendTL (s1, t2, mergeL, Vector.new1 resV)
                in s2
                end
            val blocks = LD.map (blocks, fn (l, b) => inlineCodeBlock (state, env, mergeL, b))
            val s = MS.merge (s, MF.fromBlocks (LD.toList blocks))
          in s
          end

      val rec inlineValuable = 
       fn (state, env, dests, typ, thunkO, ptr, fvsA) =>
          let
            val config = getConfig (state, env)
            val stm = getStm (state, env)
            val code = doCode (state, env, ptr)
            val (M.F {body, cc, ...}, _) = MCC.code (config, stm, code)
            val thunkV = 
                case thunkO
                 of SOME thunkV => thunkV
                  | NONE        => Vector.sub (dests, 0)
            val (recursive, formals, actuals) = 
                case cc
                 of M.CcThunk {thunk, fvs} => 
                    let
                      val trueFvs = MFV.codeBody (config, body)
                      val recursive = VS.member (trueFvs, thunk)
                      val formals = 
                          if recursive then
                            Utils.Vector.cons (thunk, fvs)
                          else
                            fvs
                      val fvsA = Vector.map (fvsA, #2)
                      val actuals = 
                          if recursive then
                            Utils.Vector.cons (M.SVariable thunkV, fvsA)
                          else
                            fvsA
                    in (recursive, formals, actuals)
                    end
                  | _ => fail ("inlineValuable", "Not a thunk code ptr")
            val v = 
                let
                  val vT = MU.FieldKind.toTyp typ
                in MSTM.variableFresh (stm, "merge", vT, M.VkLocal)
                end
            val s = inlineCodeBody (state, env, formals, actuals, body, v)
            val s = 
                case (recursive, thunkO)
                 of (true, NONE) => 
                    let
                      val mkRhs = M.RhsThunkMk {typ = typ, fvs = Vector.new0 ()}
                      val mk = MS.bindRhs (thunkV, mkRhs)
                      val rhs = M.RhsThunkValue {typ = typ, thunk = SOME thunkV, ofVal = M.SVariable v}
                      val i = MS.doRhs rhs
                    in MS.seq (mk, MS.seq (s, i))
                    end
                  | _            => 
                    let
                      val rhs = M.RhsThunkValue {typ = typ, thunk = thunkO, ofVal = M.SVariable v}
                      val i = MS.bindsRhs (dests, rhs)
                    in MS.seq (s, i)
                    end
            val () = Click.thunkVal (getPd (state, env))
          in s
          end

      and rec doInstruction = 
       fn (state, env, instruction as M.I {dests, n, rhs}) =>
          (case rhs
            of M.RhsThunkMk {typ, fvs} => 
               if thunkIsValuable (state, env, Vector.sub (dests, 0)) then
                 MS.instruction (M.I {dests = dests, n = n, rhs = M.RhsThunkMk {typ = typ, fvs = Vector.new0 ()}})
               else
                 MS.instruction instruction
             | M.RhsThunkInit {typ, thunk, fx, code = SOME ptr, fvs} => 
               if codePtrIsValuable (state, env, ptr) then
                 inlineValuable (state, env, dests, typ, thunk, ptr, fvs)
               else 
                 MS.instruction instruction
             | _ => MS.instruction instruction)

      and rec doInstructions = 
       fn (state, env, instructions) => 
          let
            val f = fn (instruction, s) => MS.seq (s, doInstruction (state, env, instruction))
          in Vector.fold (instructions, MS.empty, f)
          end

      and rec doBlock = 
       fn (state, env, l, b) =>
          let
            val M.B {parameters, instructions, transfer} = b
            val s = doInstructions (state, env, instructions)
          in MS.finish (l, parameters, s, transfer)
          end

      and rec doBlocks = 
       fn (state, env, blocks) => 
          let
            val f = fn (l, b, frag) => MF.merge (frag, doBlock (state, env, l, b))
            val frag = LD.fold (blocks, MF.empty, f)
          in MF.toBlocksD frag
          end

      and rec doCodeBody = 
       fn (state, env, M.CB {entry, blocks}) => M.CB {entry = entry, blocks = doBlocks (state, env, blocks)}

      and rec doCode = 
       fn (state, env, v) => 
          case getCodeFromDone (state, env, v)
           of SOME c => c
            | NONE => 
              (case takeCodeFromTodo (state, env, v) 
                of SOME c => 
                   let
                     val M.F {fx, escapes, recursive, cc, args, rtyps, body} = c
                     val body = doCodeBody (state, env, body)
                     val c = M.F {fx = fx, escapes = escapes, recursive = recursive, 
                                  cc = cc, args = args, rtyps = rtyps, 
                                  body = body} 
                     val () = addCodeToDone (state, env, v, c)
                   in c
                   end
                 | NONE   => fail ("global", "cycle, or unknown function!"))

      val doGlobal = 
       fn (state, env, v, g) => 
          case g
           of M.GCode _ => M.GCode (doCode (state, env, v)) 
            | _         => g

      val globals = 
       fn (state, env, globals) => VD.map (globals, fn (v, g) => doGlobal (state, env, v, g))
          
    end (* structure Rewrite *)

    val rewrite = 
     fn (state, env, p) => 
        let
          val M.P {includes, externs, globals, symbolTable, entry} = p
          val globals = Rewrite.globals (state, env, globals)
          val st = I.Manager.finish (getStm (state, env))
          val p = M.P {includes = includes, 
                       externs = externs, 
                       globals = globals, 
                       symbolTable = st, 
                       entry = entry}
        in p
        end

    val program = 
     fn (pd, p) => 
        let
          val (state, env) = analyze (pd, p)
          val () = show (state, env, p) 
          val p = rewrite (state, env, p)
        in p
        end

  end (* structure ThunkVal *)

  val postPhase = 
   fn (pd, p) => 
      let
        val config = PD.getConfig pd
        val () = if statPhases pd then Stats.report (PD.getStats pd) else ()
        val () = if checkPhases pd then MilCheck.program (config, p) else ()
        val () = if showPhases pd then MilLayout.print (config, p) else ()
      in ()
      end

  val doPhase = 
   fn (skip, f, name) =>
   fn (pd, p) => 
      if skip pd then
        let
          val () = Chat.log1 (pd, "Skipping "^name)
        in p
        end
      else
        let
          val pd = PD.push pd
          val () = Chat.log1 (pd, "Doing "^name)
          val s = Time.now ()
          val p = f (pd, p)
          val e = Time.toString (Time.- (Time.now (), s))
          val () = Chat.log1 (pd, "Done with "^name^" in "^e^"s")
          val () = postPhase (pd, p)
        in p
        end

  val thunkVal = fn (pd, p) => 
                    doPhase (skipThunkVal, 
                          fn (pd, p) => ThunkVal.program (pd, p),
                             "ThunkVal Optimization") (pd, p)

  val program = 
   fn (pd, p) =>
      let
        val p = thunkVal (pd, p)
        val () = PD.report (pd, passname)
      in p
      end

  val description = {name        = passname,
                     description = "Thunk optimizations",
                     inIr        = BothMil.irHelpers,
                     outIr       = BothMil.irHelpers,
                     mustBeAfter = [],
                     stats       = stats}

  val associates = {controls  = [],
                    debugs    = debugs,
                    features  = features,
                    subPasses = []}

  val pass =
      Pass.mkOptPass (description, associates, BothMil.mkMilPass (Utils.Function.flipIn program))


end;
