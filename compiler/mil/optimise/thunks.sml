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
  structure MCFG = MilCfg
  structure MPU = MU.Prims.Utils
  structure PD = PassData
  structure L = Layout
  structure LU = LayoutUtils
  structure ML = MilLayout
  structure VD = M.VD
  structure VS = M.VS
  structure LS = M.LS
  structure I = Identifier
  structure MSTM = MU.SymbolTableManager
  structure MS = MilStream
  structure MF = MilFragment
  structure LD = Mil.LD
  structure MCC = MilCodeCopy
  structure MFV = MilFreeVars
  structure MBV = MilBoundVars
  structure FX = Effect

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
    val {stats, click = thunkDefInline} = PD.clicker {stats = stats, passname = passname,
                                                      name = "ThunkDefInline", desc = "Thunks inlined at def site"}
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

  val (skipLocalStrictnessD, skipLocalStrictness) =
      mkDebug ("skip-local-strictness", "Skip local strictness")

  val (showLocalStrictnessD, showLocalStrictness) = 
      mkLevelDebug ("show-local-strictness", "Show local strictness", 1)

  val (skipThunkInlineD, skipThunkInline) =
      mkDebug ("skip-thunk-inline", "Skip thunk inlining")

  val (showThunkInlineD, showThunkInline) = 
      mkLevelDebug ("show-thunk-inline", "Show thunk inline analysis", 1)

  val debugs = [checkPhasesD, debugPassD, 
                showLocalStrictnessD, showThunkInlineD, showThunkValD, showPhasesD, 
                skipLocalStrictnessD, skipThunkInlineD, skipThunkValD] 

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
                               | M.RhsPrim r           => 
                                 if Effect.subset (MPU.Effects.t (#prim r), Effect.Total) then
                                   ()
                                 else
                                   computation ()
                               | M.RhsTuple r          => ()
                               | M.RhsTupleSub tf      => computation ()
                               | M.RhsTupleSet r       => computation ()
                               | M.RhsTupleCAS r       => computation ()
                               | M.RhsTupleWait r      => computation ()
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

      val doTransfer = 
       fn (state, env, t) => 
          (case t
            of M.TGoto _       => t
             | M.TCase _       => t
             | M.TInterProc r  => 
               let
                 val callee = 
                     case #callee r
                      of M.IpEval {eval, typ} => 
                         let
                           val isVal = fn v => codePtrIsValuable (state, env, v)
                           val eval =
                               case eval 
                                of M.EThunk {thunk, value, code = {exhaustive, possible}} => 
                                   if VS.exists (possible, isVal) then
                                     let
                                       val possible = VS.keepAll (possible, not o isVal)
                                       val code = {exhaustive = exhaustive, possible = possible}
                                     in M.EThunk {thunk = thunk, value = true, code = code}
                                     end
                                   else
                                     eval
                                 | M.EDirectThunk {thunk, value, code} => 
                                   if codePtrIsValuable (state, env, code) then
                                     M.EThunk {thunk = thunk, value = true, code = MU.Codes.none}
                                   else
                                     eval
                         in M.IpEval {eval = eval, typ = typ}
                         end
                       | callee => callee
               in M.TInterProc {callee = callee, ret = #ret r, fx = #fx r}
               end
             | M.TReturn opers => t
             | M.TCut _        => t
             | M.THalt _       => t)

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
            val transfer = doTransfer (state, env, transfer)
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

  structure ThunkInline =
  struct
 
    structure Analyze = 
    struct

      datatype analysis = A of {pd : PD.t, variables : VS.t, cfg : MCFG.t}

      val getVariables = fn (A {pd, cfg, variables}) => variables

      val getCfg = fn (A {pd, cfg, variables}) => cfg

      val getPd = fn (A {pd, cfg, variables}) => pd

      val getConfig = PD.getConfig o getPd

      type node = MCFG.node
                    
      structure NodeDict = MCFG.NodeDict

      structure Info = 
      struct
        type info = {com : VS.t, fx : VS.t}

        type infoRef = info ref

        val refMk = fn s => ref s

        val refGet = fn r => !r

        val empty = {com = VS.empty, fx = VS.empty}

        val isSubset = 
         fn ({com = com1, fx = fx1}, {com = com2, fx = fx2}) => VS.isSubset (com1, com2) andalso
                                                                VS.isSubset (fx1, fx2)

        val intersection = 
         fn ({com = com1, fx = fx1}, {com = com2, fx = fx2}) => {com = VS.intersection (com1, com2),
                                                                 fx = VS.intersection (fx1, fx2)}

        val union = 
         fn ({com = com1, fx = fx1}, {com = com2, fx = fx2}) => {com = VS.union (com1, com2),
                                                                 fx = VS.union (fx1, fx2)}

        val difference = 
         fn ({com = com1, fx = fx1}, {com = com2, fx = fx2}) => {com = VS.difference (com1, com2),
                                                                 fx = VS.difference (fx1, fx2)}

        val insertCommutative = 
         fn ({com, fx}, f) => {com = VS.insert (com, f), fx = fx}

        val insertFx = 
         fn ({com, fx}, f) => {com = com, fx = VS.insert (fx, f)}

        val clearFx = 
         fn ({com, fx}) => {com = com, fx = VS.empty}
      end
        
      val initial = fn (a, n) => MCFG.compareNode (n, MCFG.exit (getCfg a)) = EQUAL

      val initialVal = fn a => Info.empty

      val bottom = fn a => {com = getVariables a, fx = getVariables a}

      val components = fn a => List.rev (MCFG.scc (getCfg a))

      val successors = fn (a, n) => MCFG.pred (getCfg a, n)

      val fxCommutesWithAll = FX.union (FX.HeapGenS, FX.InitGenS)
      val fxCommutesWithPartial = FX.fromList [FX.Partial, FX.HeapGen, FX.HeapRead, FX.HeapWrite,
                                               FX.InitGen, FX.InitRead, FX.InitWrite]

      val buildBlockTransferFunction = 
       fn (a, M.B {parameters, instructions, transfer}) => 
          let
            val config = getConfig a
            val noChange = fn info => info
            val clearFx = fn info => Info.clearFx info
            val clearAll = fn into => Info.empty
            val fxChooseFn = 
             fn fx => if FX.subset (fx, fxCommutesWithAll) then noChange
                      else if FX.subset (fx, fxCommutesWithPartial) then clearFx
                      else clearAll
            val iFx = Vector.fold (instructions, FX.Total, fn (i, s) => FX.union (s, MU.Instruction.fx (config, i)))
            val iF = fxChooseFn iFx
            val tF = 
                (case transfer
                  of M.TInterProc {callee = M.IpEval {eval, ...}, fx, ...} => 
                     let
                       val f0 = fxChooseFn fx
                       val f1 = 
                           if FX.subset (fx, FX.PartialS) then 
                             fn info => Info.insertCommutative (info, MU.Eval.thunk eval)
                           else
                             fn info => Info.insertFx (info, MU.Eval.thunk eval)
                     in f1 o f0
                     end
                   | _                                                     => 
                     fxChooseFn (MU.Transfer.fx (config, transfer)))
          in iF o tF
          end

      val transfer = 
       fn (a, n) => 
          let
            val update = 
             fn (new, ior) => 
                let
                  val changed = not (Info.isSubset (!ior, new))
                  val () = if changed then ior := Info.intersection (new, !ior) else ()
                in changed
                end
            val t = 
                case MCFG.nodeGetBlock (getCfg a, n)
                 of NONE   => update
                  | SOME b => 
                    let
                      val f = buildBlockTransferFunction (a, b)
                    in 
                      fn (ii, ior) => update (f ii, ior)
                    end
          in t
          end
            
      structure Analysis = DataFlowF(struct
                                      type analysis = analysis
                                      type node = node
                                      structure NodeDict = NodeDict
                                      structure Info = Info
                                      val initial = initial
                                      val initialVal = initialVal
                                      val bottom = bottom
                                      val components = components
                                      val successors = successors
                                      val transfer = transfer
                                      end
                                    )

      val analyzeCode = 
       fn (pd, si, M.F {body, ...}) =>
          let
            val config = PD.getConfig pd
            val variables = MFV.codeBody (config, body)
            val variables = VS.union (variables, MBV.codeBody (config, body))
            val cfg = MCFG.build (config, si, body)
            val result = Analysis.analyze (A {pd = pd, cfg = cfg, variables = variables})
            val M.CB {blocks, ...} = body
            val getInfo = 
             fn (l, b) => 
                let
                  val node = MCFG.labelGetNode (cfg, l)
                  val {iInfo, oInfo} = result node
                  val {com, fx} = oInfo
                in VS.union (com, fx)
                end
            val results = LD.map (blocks, getInfo)
          in results
          end

      val analyzeGlobals = 
       fn (pd, si, globals) => 
          let
            val analyzeGlobal = 
             fn (v, g, d) => 
                (case g
                  of M.GCode code => LD.union (analyzeCode (pd, si, code), d, 
                                               fn _ => fail ("analyzeGlobals", "dup label"))
                   | _            => d)
          in VD.fold (globals, LD.empty, analyzeGlobal)
          end

      (* This is really too coarse - it only treats an init as strict if
       * the strictness reaches the top of the block.  *)
      val summarize = 
       fn (pd, si, globals, results) => 
          let
            val config = PD.getConfig pd
            val doGlobal = 
             fn (v, g, strictAtInit) => 
                (case g
                  of M.GCode (M.F {body, ...}) =>
                     let
                       val M.CB {blocks, ...} = body
                       val getInfo = 
                        fn (l, b, strictAtInit) => 
                           let
                             val strictAtBlock = valOf (LD.lookup (results, l))
                             val dfdInBlock = MBV.block (config, l, b)
                             val strictAtInit = VS.union (strictAtInit, VS.intersection (strictAtBlock, dfdInBlock))
                           in strictAtInit
                           end
                       val strictAtInit = LD.fold (blocks, strictAtInit, getInfo)
                     in strictAtInit
                     end
                   | _            => strictAtInit)
          in VD.fold (globals, VS.empty, doGlobal)
          end

      val show = 
       fn (pd, si, p, results, strictAtInit) =>
          if showThunkInline pd then
            let
              val config = PD.getConfig pd
              val var = fn v => L.seq[ML.layoutVariable (config, si, v),
                                      if VS.member (strictAtInit, v) then L.str "!" else L.empty]
              val block =
               fn l => VS.layout (valOf (LD.lookup (results, l)), var)
              val helpers = {varBind = NONE, block = SOME block, edge = NONE, cb = NONE}
              val l = ML.General.layout (config, helpers, p)
              val () = LU.printLayout l
            in ()
            end
          else
            ()

      val program = 
       fn (pd, p as M.P {includes, externs, globals, symbolTable, entry}) =>
          let
            val si = I.SymbolInfo.SiTable symbolTable
            val results = analyzeGlobals (pd, si, globals)
            val strictAtInit = summarize (pd, si, globals, results)
            val () = show (pd, si, p, results, strictAtInit)
          in strictAtInit
          end

    end (* structure Analyze *)

    type codeInfo = {exits : bool, conts : LS.t, rtyps : M.typ Vector.t, fx : M.effects}

    datatype state = S of {codeInfo : codeInfo VD.t ref,    (* Code information *)
                           codeMap  : M.variable VD.t ref,  (* mapping from old cptrs to new cptrs *)
                           stm      : M.symbolTableManager
                          }

    datatype env = E of {current : M.variable, pd : PD.t, strict : VS.t}

    val ((_, stateGetCodeInfo),
         (_, stateGetStm),
         (_, stateGetCodeMap)) = 
        let
          val r2t = fn S {codeInfo, stm, codeMap} => (codeInfo, stm, codeMap)
          val t2r = fn (codeInfo, stm, codeMap) => S {codeInfo = codeInfo, stm = stm, codeMap = codeMap}
        in FunctionalUpdate.mk3 (r2t, t2r)
        end

    val mkState = fn stm => S {codeInfo = ref VD.empty, stm = stm, codeMap = ref VD.empty}

    val ((envSetPd, envGetPd),
         (envSetCurrent, envGetCurrent),
         (envSetStrict, envGetStrict)
        ) = 
        let
          val r2t = fn E {pd, current, strict} => (pd, current, strict)
          val t2r = fn (pd, current, strict) => E {pd = pd, current = current, strict = strict}
        in FunctionalUpdate.mk3 (r2t, t2r)
        end

    val mkEnv = fn (pd, current, strict) => E {current = current, pd = pd, strict = strict}

    val envGetConfig = PD.getConfig o envGetPd

    structure MSU = MilStreamUtilsF(struct
                                     type state = state
                                     type env = env
                                     val getStm = stateGetStm
                                     val getConfig = envGetConfig
                                    end)

    val getPd = 
     fn (state, env) => envGetPd env

    val getConfig = 
     fn (state, env) => envGetConfig env

    val getStm = 
     fn (state, env) => stateGetStm state

    val getSi = 
     fn (state, env) => I.SymbolInfo.SiManager (getStm (state, env))

    val cloneVariable = 
     fn (state, env, v) => MSTM.variableClone (getStm (state, env), v)

    val localVariableFresh = 
     fn (state, env, s, t) => MSTM.variableFresh (getStm (state, env), s, t, M.VkLocal)

    val localVariableRelated = 
     fn (state, env, v, t) => MSTM.variableRelated (getStm (state, env), v, "stcl", t, M.VkLocal)

    val globalVariableRelated = 
     fn (state, env, v, t) => MSTM.variableRelated (getStm (state, env), v, "stcl", t, M.VkGlobal)

    val variableTyp = 
     fn (state, env, v) => MSTM.variableTyp (getStm (state, env), v)

    val addStencilVariable = 
     fn (state, env, f, fStencil) => 
        let
          val codeMapR = stateGetCodeMap state
          val () = codeMapR := VD.insert (!codeMapR, f, fStencil)
        in ()
        end

    val getStencilVariable = 
     fn (state, env, f) => 
        let
          val codeMapR = stateGetCodeMap state
        in case VD.lookup (!codeMapR, f)
            of SOME i => i
             | NONE   => fail ("getStencilVariable", "No stencil for code")
        end

    val hasStencil =
     fn (state, env, f) => 
        let
          val codeMapR = stateGetCodeMap state
        in isSome (VD.lookup (!codeMapR, f))
        end
        
    val getCodeInfo = 
     fn (state, env, f) =>
        let
          val codeInfoR = stateGetCodeInfo state
        in case VD.lookup (!codeInfoR, f)
            of SOME i => i
             | NONE   => fail ("getCodeInfo", "Bad function pointer")
        end

    val setCodeInfo = 
     fn (state, env, f, i) =>
        let
          val codeInfoR = stateGetCodeInfo state
          val () = codeInfoR := VD.insert (!codeInfoR, f, i)
        in ()
        end

    val getCurrentCodeInfo = 
     fn (state, env) => getCodeInfo (state, env, envGetCurrent env)

    (* The simple algorithm is as follows:
     *   Identify the thunks which are strict at their init site.
     *   Allocate new code pointers for each thunk code which was 
     *     identified in step 1 and which is a valid candidate
     *   Optimize the program
     *   Make new code copies of each candidate
     *   A valid candidate is a function which does not mention
     *    the thunk "self" variable in its body.
     *     
     *)

    val isCandidate =
     fn (state, env, code) =>
        (case code
          of M.F {fx, escapes, recursive, cc = M.CcThunk {thunk, fvs}, args, rtyps, body} =>
             not (VS.member(MFV.codeBody (getConfig (state, env), body), thunk))
           | _ => false)

    val stencil : state * env * M.variable * M.global -> (M.variable * M.global) List.t =
     fn (state, env, f, g) => 
        (case g
          of M.GCode code => 
             if isCandidate (state, env, code) then
               let
                 val clone = fn v => cloneVariable (state, env, v)
                 val M.F {fx, escapes, recursive, cc, args, rtyps, body} = code
                 val {thunk, fvs} = valOf (MU.CallConv.Dec.ccThunk cc)
                 val argTs = Vector.map (fvs, fn v => variableTyp (state, env, v))
                 val stencilT = M.TCode {cc = M.CcCode, args = argTs, ress = rtyps}
                 val fStencil = globalVariableRelated (state, env, f, stencilT)
                 val gStencil = 
                     let
                       val cc = M.CcCode
                       val args = fvs
                       val c = M.F {fx = fx, escapes = false, recursive = recursive, cc = cc,
                                    args = args, rtyps = rtyps, body = body}
                     in M.GCode c
                     end
                 val g = 
                     let
                       val fvs = Vector.map (fvs, clone)
                       val cc = M.CcThunk {thunk = thunk, fvs = fvs}
                       val body = 
                           let
                             val call = M.CCode {ptr = fStencil, code = {possible = VS.singleton fStencil, 
                                                                         exhaustive = true}}
                             val args = Vector.map (fvs, M.SVariable)
                             val callee = M.IpCall {call = call, args = args}
                             val ret = M.RTail {exits = FX.contains (fx, FX.Fails)}
                             val t = M.TInterProc {callee = callee, ret = ret, fx = fx}
                             val entry = MSTM.labelFresh (getStm (state, env))
                             val block = M.B {parameters = Vector.new0 (),
                                              instructions = Vector.new0 (), 
                                              transfer = t}
                           in M.CB {entry = entry, blocks = LD.singleton (entry, block)}
                           end
                       val c = M.F {fx = fx, escapes = escapes, recursive = recursive, 
                                    cc = cc, args = args, rtyps = rtyps, body = body}
                     in M.GCode c
                     end
                 val () = addStencilVariable (state, env, f, fStencil)
                 val ci as {exits, conts, rtyps, fx} = getCodeInfo (state, env, f)
                 val () = setCodeInfo (state, env, f, {exits = exits, conts = LS.empty, rtyps = rtyps, fx = fx})
                 val () = setCodeInfo (state, env, fStencil, ci)
               in [(f, g), (fStencil, gStencil)]
               end
             else
               [(f, g)]
           | _ => [(f, g)])

    val generateStencils = 
     fn (state, env, globals) => 
        let
          val globals = VD.toList globals
          val globals = List.concatMap (globals, fn (f, g) => stencil (state, env, f, g))
          val globals = VD.fromList globals
        in globals
        end

    val makeStrict = 
     fn (state, env, dests, typ, thunk, code, fvs) => 
        let
          val (rets, call) = 
              let
                val fStencil = getStencilVariable (state, env, code)
                val {exits = calleeExits, rtyps,     conts = _, fx} = getCodeInfo (state, env, fStencil)
                val {exits = callerExits, rtyps = _, conts = callerTargets, fx = _} = getCurrentCodeInfo (state, env)
                val call = M.CCode {ptr = fStencil, 
                                    code = {possible = VS.singleton fStencil, 
                                            exhaustive = true}}
                val args = Vector.map (fvs, #2)
                val rets = Vector.map (rtyps, fn t => localVariableFresh (state, env, "ret", t))
                val exits = calleeExits andalso callerExits
                val targets = if calleeExits then callerTargets else LS.empty
                val cuts = M.C {exits = exits, targets = targets}
                val s = MSU.call (state, env, call, args, cuts, fx, rets)
                val () = Click.thunkDefInline (getPd (state, env))
              in (rets, s)
              end
          val init = 
              let
                val rhs = M.RhsThunkValue {typ = typ, thunk = thunk, ofVal = M.SVariable (Vector.sub (rets, 0))}
                val s = MS.bindsRhs (dests, rhs)
              in s
              end
        in MS.seq (call, init)
        end

    val strictAtInit = 
     fn (state, env, v) => VS.member (envGetStrict env, v)

    val transformInstruction = 
     fn (state, env, i as M.I {dests, rhs, ...}) => 
        let
          val ans = 
              case rhs
               of M.RhsThunkInit {typ, thunk, fx, code = SOME codeF, fvs} => 
                  let
                    val thunkVar = 
                        case thunk 
                         of SOME t => t
                          | NONE   => Vector.sub (dests, 0)
                  in if strictAtInit (state, env, thunkVar) 
                        andalso hasStencil (state, env, codeF) 
                        andalso not (#exits (getCodeInfo (state, env, codeF))) then
                       SOME (makeStrict (state, env, dests, typ, thunk, codeF, fvs))
                     else
                       NONE
                  end
                | _ => NONE
        in (env, ans)
        end

    val transformGlobal = 
     fn (state, env, v, g) => 
        let
          val env = 
              case g
               of M.GCode _ => envSetCurrent (env, v)
                | _         => env
        in (env, NONE)
        end

    structure Transform = MilTransformF(struct
                                         type state = state
                                         type env = env
                                         val config = envGetConfig
                                         val indent = 2
                                         val label = fn (state, env, _, _) => (env, NONE)
                                         val instr = transformInstruction
                                         val transfer = fn (state, env, _) => (env, NONE)
                                         val global = transformGlobal
                                       end)

    val collectCodeInfo = 
     fn (state, env, globals) => 
        let
          val doGlobal = 
           fn (v, g) => 
              (case g
                of M.GCode code => 
                   let
                     val M.F {fx, escapes, recursive, cc, args, rtyps, body} = code
                     val exits = FX.contains (fx, FX.Fails)
                     val conts = MU.CodeBody.conts body
                     val ci = {exits = exits, conts = conts, rtyps = rtyps, fx = fx}
                   in setCodeInfo (state, env, v, ci)
                   end
                 | _ => ())
        in VD.foreach (globals, doGlobal)
        end

    (*  We rewrite the program by 
     *   1) Stenciling each candidate thunk code function.
     *   2) Rewriting all of the code blocks to turn strict 
     *      thunk inits into calls to the stenciled functions
     *)
    val rewrite = 
     fn (pd, p, strictAtInit) => 
        let
          val M.P {includes, externs, globals, symbolTable, entry} = p
          val stm = Identifier.Manager.fromExistingAll symbolTable
          val state = mkState stm
          val env = mkEnv (pd, entry, strictAtInit)
          val () = collectCodeInfo (state, env, globals)
          val globals = generateStencils (state, env, globals)
          val globals = Transform.globals (state, env, Transform.OAny, globals)
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
          val strictAtInit = Analyze.program (pd, p)
(*          val () = show (state, env, p)  *)
          val p = rewrite (pd, p, strictAtInit)
        in p
        end

  end (* structure ThunkInline *)

  structure LocalStrictness = 
  struct
    datatype analysis = A of {cfg : MCFG.t,
                              variables : VS.t}
    val getCfg = fn (A {cfg, variables}) => cfg
    val getVariables = fn (A {cfg, variables}) => variables
    type node = MCFG.node
    structure NodeDict = MCFG.NodeDict
    structure Info = 
    struct
      type info = VS.t
      type infoRef = VS.t ref
      val refMk = fn s => ref s
      val refGet = fn r => !r
    end
    val initial = fn (a, n) => MCFG.compareNode (n, MCFG.exit (getCfg a)) = EQUAL
    val initialVal = fn a => getVariables a
    val bottom = fn a => VS.empty
    val components = fn a => List.rev (MCFG.scc (getCfg a))
    val successors = fn (a, n) => MCFG.pred (getCfg a, n)
    val composeGenKill =
        fn ((gen0, kill0), (gen1, kill1)) => 
           (VS.union (VS.difference (gen0, kill1), gen1),
            VS.union (VS.difference (kill0, gen1), kill1))
    val instructionsMakeGenKill =
     fn (a, is) => (VS.empty, VS.empty)

    val transferMakeGenKill = 
     fn (a, t) => 
        (case t
          of M.TInterProc {callee = M.IpEval {eval, ...}, ...} => (VS.empty, VS.singleton (MU.Eval.thunk eval))
           | _                                                 => (VS.empty, VS.empty))
    val blockMakeGenKill = 
     fn (a, M.B {parameters, instructions, transfer}) => 
        composeGenKill (transferMakeGenKill (a, transfer), instructionsMakeGenKill (a, instructions))
    val transfer = 
     fn (a, n) => 
        let
          val update = 
           fn (new, ior) => 
              let
                val changed = not (VS.isSubset (new, !ior))
                val () = if changed then ior := VS.union (new, !ior) else ()
              in changed
              end
          val t = 
              case MCFG.nodeGetBlock (getCfg a, n)
               of NONE   => update
                | SOME b => 
                  let
                    val (gen, kill) = blockMakeGenKill (a, b)
                  in
                    fn (ii, ior) => 
                       let
                         val lv = L.str o I.variableString' 
                         val variables = getVariables a
(*                         val l = L.mayAlign [L.seq [L.str "In for block ", 
                                                   L.str (I.labelString (valOf (MCFG.nodeGetLabel (getCfg a, n))))],
                                             LU.indent (VS.layout (VS.difference (variables, ii), lv)),
                                             L.seq [L.str "Out for block ", 
                                                    L.str (I.labelString (valOf (MCFG.nodeGetLabel (getCfg a, n))))],
                                             LU.indent (VS.layout (VS.difference (variables, !ior), lv))]
                         val () = LU.printLayout l*)
                         val new = VS.union (gen, VS.difference (ii, kill))
                       in update (new, ior)
                       end
                  end
        in t
        end
          
    structure Analysis = DataFlowF(type analysis = analysis
                                   type node = node
                                   structure NodeDict = NodeDict
                                   structure Info = Info
                                   val initial = initial
                                   val initialVal = initialVal
                                   val bottom = bottom
                                   val components = components
                                   val successors = successors
                                   val transfer = transfer
                                  )

    val analyzeCode = 
     fn (config, si, M.F {body, ...}) =>
        let
          val variables = MFV.codeBody (config, body)
          val variables = VS.union (variables, MBV.codeBody (config, body))
          val cfg = MCFG.build (config, si, body)
          val result = Analysis.analyze (A {cfg = cfg, variables = variables})
          val M.CB {blocks, ...} = body
          val getInfo = 
           fn (l, b) => 
              let
                val node = MCFG.labelGetNode (cfg, l)
                val {iInfo, oInfo} = result node
              in VS.difference (variables, oInfo)
              end
          val results = LD.map (blocks, getInfo)
        in results
        end

    val analyzeGlobals = 
     fn (config, si, globals) => 
        let
          val analyzeGlobal = 
           fn (v, g, d) => 
              (case g
                of M.GCode code => LD.union (analyzeCode (config, si, code), d, 
                                             fn _ => fail ("analyzeGlobals", "dup label"))
                 | _            => d)
        in VD.fold (globals, LD.empty, analyzeGlobal)
        end

    val show = 
     fn (pd, si, p, results) =>
        if showLocalStrictness pd then
          let
            val config = PD.getConfig pd
            val var = fn v => ML.layoutVariable (config, si, v)
            val block =
             fn l => VS.layout (valOf (LD.lookup (results, l)), var)
            val helpers = {varBind = NONE, block = SOME block, edge = NONE, cb = NONE}
            val l = ML.General.layout (config, helpers, p)
            val () = LU.printLayout l
          in ()
          end
        else
          ()

    val program = 
     fn (pd, p as M.P {includes, externs, globals, symbolTable, entry}) =>
        let
          val config = PD.getConfig pd
          val si = I.SymbolInfo.SiTable symbolTable
          val results = analyzeGlobals (config, si, globals)
          val () = show (pd, si, p, results)
(*          val p = doProgram (pd, p, results)*)
        in p
        end
  end

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

  val thunkInline = fn (pd, p) => 
                       doPhase (skipThunkInline, 
                                fn (pd, p) => ThunkInline.program (pd, p),
                                "ThunkInline Optimization") (pd, p)

  val strictness = fn (pd, p) => 
                      doPhase (skipLocalStrictness, 
                               fn (pd, p) => LocalStrictness.program (pd, p),
                               "Local Strictness Optimization") (pd, p)

  val program = 
   fn (pd, p) =>
      let
        val p = thunkVal (pd, p)
        val p = strictness (pd, p)
        val p = thunkInline (pd, p)
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
