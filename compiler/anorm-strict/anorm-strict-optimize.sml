(* The Haskell Research Compiler *)
(*
 * Redistribution and use in source and binary forms, with or without modification, are permitted 
 * provided that the following conditions are met:
 * 1.   Redistributions of source code must retain the above copyright notice, this list of 
 * conditions and the following disclaimer.
 * 2.   Redistributions in binary form must reproduce the above copyright notice, this list of
 * conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 * BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

signature ANORM_STRICT_OPTIMIZE =
sig
  val pass : (ANormStrict.t, ANormStrict.t) Pass.t
end

structure ANormStrictOptimize :> ANORM_STRICT_OPTIMIZE =
struct
  structure AS = ANormStrict
  structure ANSS = ANormStrictStats
  structure ASFV = ANormStrictFreeVars
  structure GPT = GHCPrimType
  structure GPO = GHCPrimOp
  structure ASL = ANormStrictLayout
  structure ASU = ANormStrictUtils
  structure ASC = ANormStrictClone
  structure CHU = CoreHsUtils
  structure I = Identifier
  structure SI = Identifier.SymbolInfo
  structure VD = I.VariableDict
  structure ND = I.NameDict
  structure DG = DepGraph
  structure IM = Identifier.Manager
  structure PD = PassData
  structure L = Layout
  structure LU = LayoutUtils
  structure VS = I.VariableSet
  structure FVS = ANormStrictFreeVars
  structure C = Compare
  structure Parse = StringParser

  val passname = "ANormStrictOptimize"

  structure Click =
  struct
    val stats = []
    val {stats, click = caseBeta} = PD.clicker {stats = stats, passname = passname,
                                                name = "CaseBeta", desc = "Cases beta reduced"}
    val {stats, click = deadCode} = PD.clicker {stats = stats, passname = passname,
                                                name = "DeadCode", desc = "Bindings killed"}
    val {stats, click = deadField} = PD.clicker {stats = stats, passname = passname,
                                                name = "DeadField", desc = "Constructor fields killed"}
    val {stats, click = funcInOnce} = PD.clicker {stats = stats, passname = passname,
                                                  name = "InlineFunctionOnce", desc = "Functions used once inlined"}
    val {stats, click = funcInTotal} = PD.clicker {stats = stats, passname = passname,
                                                   name = "InlineFunctionTotal",
                                                   desc = "Total (non-duplicative) functions inlined"}
    val {stats, click = strictArg} = PD.clicker {stats = stats, passname = passname,
                                                 name = "StrictArg", desc = "Function arguments/frees made strict"}
    val {stats, click = strictField} = PD.clicker {stats = stats, passname = passname,
                                                   name = "StrictField", desc = "Constructor fields made strict"}
    val {stats, click = thunkEta} = PD.clicker {stats = stats, passname = passname,
                                                name = "ThunkEta", desc = "Thunk eta reduced"}
    val {stats, click = thunkInVal} = PD.clicker {stats = stats, passname = passname,
                                                  name = "InlineThunkVal", desc = "Thunk vals inlined"}
    val {stats, click = thunkInOnce} = PD.clicker {stats = stats, passname = passname,
                                                   name = "InlineThunkOnce", desc = "Thunks used once inlined"}
    val {stats, click = thunkReIntro} = PD.clicker {stats = stats, passname = passname,
                                                    name = "ThunkReIntro", desc = "Re-introduced thunks eliminated"}
    val {stats, click = toThunkVal} = PD.clicker {stats = stats, passname = passname,
                                                  name = "ToThunkVal", desc = "Thunks to thunk vals"}
    val {stats, click = uncurry} = PD.clicker {stats = stats, passname = passname,
                                               name = "Uncurry", desc = "Applications uncurried"}

  end (* structure Click *)

  structure Chat = ChatF (struct
                            type env = PD.t
                            val extract = PD.getConfig
                            val name = passname
                            val indent = 2
                          end)

  val fail =
   fn (f, m) => Fail.fail ("anorm-strict-optimize.sml", f, m)

  val mkDebug =
   fn (tag, description) => PD.mkDebug (passname^":"^tag, description)

  val (debugPassD, debugPass) =
      mkDebug ("debug", "Debug according to debug level")

  val mkLevelDebug =
   fn (tag, description, level) => PD.mkLevelDebug (passname, passname^":"^tag, description, level, debugPass)

  val (showCensusD, showCensus) =
      mkLevelDebug ("show-census", "Show census counts", 0)

  val (showDefsD, showDefs) =
      mkLevelDebug ("show-defs", "Show defs added to environment", 1)

  val (showOptimize1D, showOptimize1) =
      mkLevelDebug ("show-optimize1", "Show result of optimize1 phase", 0)

  val (showStrictnessD, showStrictness) =
      mkLevelDebug ("show-strictness", "Show result of strictness phase", 0)

  val (showStrictnessAnalysisD, showStrictnessAnalysis) =
      mkLevelDebug ("show-strictness-info", "Show result of strictness analysis", 0)

  val (showStrictnessStepsD, showStrictnessSteps) =
      mkLevelDebug ("show-strictness-steps", "Show steps of strictness phase", 2)

  val (showUncurryD, showUncurry) =
      mkLevelDebug ("show-uncurry", "Show result of uncurry phase", 0)

  val (showUncurryStepsD, showUncurrySteps) =
      mkLevelDebug ("show-uncurry-steps", "Show steps of uncurry phase", 2)

  val debugs = [debugPassD, showCensusD, showDefsD,
                showOptimize1D,
                showStrictnessD, showStrictnessAnalysisD, showStrictnessStepsD,
                showUncurryD, showUncurryStepsD]

  val mkFeature =
   fn (tag, description) => PD.mkFeature (passname^":"^tag, description)

  val mkLogFeature =
   fn (tag, description, level) => PD.mkLogFeature (passname, passname^":"^tag, description, level)

  val (statPhasesF, statPhases) =
      mkLogFeature ("stat-phases", "Show stats between phases", 2)

  val (measurePhasesF, measurePhases) =
      mkLogFeature ("measure-phases", "Measure IR between phases", 2)

  val (skipOptimize1F, skipOptimize1) =
      mkFeature ("skip-optimize1", "Skip the optimize1 phase")

  val (skipStrictnessF, skipStrictness) =
      mkFeature ("skip-strictness", "Skip the strictness phase")

  val (skipUncurryF, skipUncurry) =
      mkFeature ("skip-uncurry", "Skip the uncurry phase")

  val (evalFreesF, evalFrees) =
      mkFeature ("eval-free-variables", "Strictness evals strict free vars")

  val (noSinkMainF, noSinkMain) =
      mkFeature ("no-sink-main", "Don't sink into main")

  val features = [evalFreesF, measurePhasesF, noSinkMainF,
                  skipOptimize1F, skipStrictnessF, skipUncurryF,
                  statPhasesF]

  datatype liveStatusS = LsLive | LsUses of liveStatus List.t
  withtype liveStatus = liveStatusS ref

  datatype useCountS = UcUsed of int | UcUsedBy of liveStatus List.t
  withtype useCount = useCountS ref

  datatype effect = ETotal | EPartial | EThreaded | EImplicit

(*
  val cmpInstruction : AS.exp * AS.exp -> order =
   fn (e1, e2) =>
      let
        val cVar = Identifier.variableCompare
        val cVars = C.list cVar
        val cCon = C.pair (Identifier.nameCompare, Int.compare)
        val cast =
         fn (c1, c2) =>
            (case (c1, c2)
              of (AS.FromAddr v1, AS.FromAddr v2) => cVar (v1, v2)
               | (AS.FromAddr _ , _             ) => GREATER
               | (_             , AS.FromAddr _ ) => LESS
               | (AS.ToAddr v1  , AS.ToAddr v2  ) => cVar (v1, v2)
               | (AS.ToAddr _   , _             ) => GREATER
               | (_             , AS.ToAddr _   ) => LESS
               | (AS.NullRef    , AS.NullRef    ) => EQUAL
               | (AS.NullRef    , _             ) => GREATER
               | (_             , AS.NullRef    ) => LESS
               | (AS.Bottom     , AS.Bottom     ) => EQUAL)
        val lit = CHU.compareCoreLit
      in
        case (e1, e2)
         of (AS.Return vs1, AS.Return vs2) => cVars (vs1, vs2)
          | (AS.Return _  , _            ) => GREATER
          | (_            , AS.Return _  ) => LESS
          | (AS.PrimApp p1, AS.PrimApp p2) => C.pair (C.string, cVars) (p1, p2)
          | (AS.PrimApp _ , _            ) => GREATER
          | (_            , AS.PrimApp _ ) => LESS
          | (AS.ExtApp _  , _            ) => fail ("cmpInstruction", "Extern app not allowed")
          | (_            , ExtApp _     ) => fail ("cmpInstruction", "Extern app not allowed")
          | (AS.ConApp p1 , AS.ConApp p2 ) => C.pair (cCon, cVars) (p1, p2)
          | (AS.ConApp _  , _            ) => GREATER
          | (_            , AS.ConApp _  ) => LESS
          | (AS.App p1,   , AS.App p2    ) => C.pair (cVar, cVars) (p1, p2)
          | (AS.App _     , _            ) => GREATER
          | (_            , AS.App _     ) => LESS
          | (AS.Let _     , _            ) => fail ("cmpInstruction", "Let not in named form")
          | (_            , AS.Let _     ) => fail ("cmpInstruction", "Let not in named form")
          | (AS.Case _    , _            ) => fail ("cmpInstruction", "case not allowed")
          | (_            , AS.Case _    ) => fail ("cmpInstruction", "case not allowed")
          | (AS.Lit p1    , AS.Lit p2    ) => C.pair (lit, ty) (p1, p2)
          | (AS.Lit _     , _            ) => GREATER
          | (_            , AS.Lit _     ) => LESS
          | (AS.Cast c1   , AS.Cast c2   ) => cast (c1, c2)
          | (AS.Cast _    , _            ) => GREATER
          | (_            , AS.Cast _    ) => LESS
          | (AS.Eval v1   , AS.Eval v2   ) => cVar (v1, v2)
      end
*)
  val joinEffect : effect * effect -> effect =
   fn (e1, e2) =>
    (case (e1, e2)
      of (EImplicit, _) => EImplicit
       | (_, EImplicit) => EImplicit
       | (EThreaded, _) => EThreaded
       | (_, EThreaded) => EThreaded
       | (EPartial, _)  => EPartial
       | (_, EPartial)  => EPartial
       | (_,         _) => ETotal)

  val effectTotal : effect -> bool = fn e => e = ETotal
  val effectImplicit : effect -> bool = fn e => e = EImplicit

  val effectLayout : effect -> Layout.t =
   fn e =>
      (case e
        of ETotal    => L.str "!"
         | EPartial  => L.str "!!"
         | EThreaded => L.str "!!!"
         | EImplicit => L.str "!!!!")

  datatype def = DDef of AS.exp
               | DEta of {remaining : int,  (* Arguments left until saturation *)
                          uncurry : AS.var, (* uncurried version *)
                          args : AS.var List.t (* Arguments so far *)
                         }
               | DFunc of {i : int, r : bool, vs : AS.var List.t, e : AS.exp}
                            (* size, recursive, args, body *)
               | DNone
               | DSubst of AS.var
               | DThunk of {t : bool, e : AS.exp} (* value, body *)


  datatype control = C of {uncurry : bool}

  datatype fnInfo = FI of {size : int, fx : effect}

  datatype state = S of {fns  : fnInfo ref VD.t ref,
                         stm  : AS.symbolTableManager,
                         vars : (liveStatus * useCount) VD.t ref,
                         tm   : AS.typeManager}


  datatype env = E of {curFns  : AS.var List.t,
                       current : liveStatus,
                       control : control,
                       defs    : def VD.t,
                       pd      : PD.t}

  val ((stateSetFns, stateGetFns),
       (stateSetStm, stateGetStm),
       (stateSetVars, stateGetVars),
       (stateSetTm, stateGetTm)) =
        let
          val r2t = fn S {fns, stm, vars, tm} => (fns, stm, vars, tm)
          val t2r = fn (fns, stm, vars, tm) =>
                       S {fns = fns, stm = stm, vars = vars, tm = tm}
        in FunctionalUpdate.mk4 (r2t, t2r)
        end

  val ((envSetCurFns, envGetCurFns),
       (envSetCurrent, envGetCurrent),
       (envSetControl, envGetControl),
       (envSetDefs, envGetDefs),
       (envSetPd, envGetPd)
      ) =
      let
        val r2t = fn E {curFns, current, control, defs, pd} => (curFns, current, control, defs, pd)
        val t2r = fn (curFns, current, control, defs, pd) => E {curFns = curFns, current = current,
                                                               control = control, defs = defs, pd = pd}
        in FunctionalUpdate.mk5 (r2t, t2r)
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

  val getFnInfoRef =
   fn (state, env, f) => valOf (VD.lookup (!(stateGetFns state), f))

  val getFnInfoRefOption =
   fn (state, env, f) => VD.lookup (!(stateGetFns state), f)

  val getFnSize =
   fn (state, env, f) => case getFnInfoRef (state, env, f) of ref (FI {size, fx}) => size

  val getFnFx =
   fn (state, env, f) =>
      case VD.lookup (!(stateGetFns state), f)
       of SOME (ref (FI {size, fx})) => fx
        | NONE                       => EImplicit

  val newFnInfoForFn =
   fn (state, env, f, fx) =>
      let
        val fns as ref d = stateGetFns state
      in
        case VD.lookup (d, f)
         of SOME _ => fail ("newFnInfoForFn", "duplicate function name")
          | NONE   => fns := (VD.insert (d, f, ref (FI {size = 0, fx = fx})))
      end

  val addToFnSize =
   fn (state, env, v, i) =>
      let
        val r as ref (FI {size, fx}) = getFnInfoRef (state, env, v)
        val () = r := (FI {size = size + i, fx = fx})
      in ()
      end

  val setFnFx =
   fn (state, env, v, fx) =>
      let
        val r as ref (FI {size, fx = _}) = getFnInfoRef (state, env, v)
        val () = r := (FI {size = size, fx = fx})
      in ()
      end

  val addToCurFnsSize =
   fn (state, env, i) => List.foreach (envGetCurFns env, fn v => addToFnSize (state, env, v, i))

  val incCurFnsSize =
   fn (state, env) => addToCurFnsSize (state, env, 1)

  val pushCurFn =
   fn (state, env, f) =>
      let
        val curFns = envGetCurFns env
      in envSetCurFns (env, f::curFns)
      end

  val layoutDef =
   fn (state, env, d) =>
      let
        val c = getConfig (state, env)
        val si = getSi (state, env)
        val stm = getStm (state, env)
        val var = fn v => IM.layoutVariable (v, stm)
        val vars = fn vs => LayoutUtils.angleSeq (List.map (vs, var))
        val value = fn v => L.str (if v then "!" else "")
        val recursive = fn r => L.str (if r then "*" else "")
        val size = fn i => LU.paren(Int.layout i)
        val l =
            case d
             of DDef e                 => L.seq [L.str "Exp ", ASL.exp (c, si, e)]
              | DNone                  => L.str "No def"
              | DSubst v               => L.seq [L.str "-> ", var v]
              | DThunk {t, e}          => L.seq [L.str "Thunk", value t, L.str " {", ASL.exp (c, si, e), L.str "}"]
              | DFunc {i, r, vs, e}    => L.seq [L.str "\\", recursive r, size i, LU.paren (vars vs),
                                                 L.str "{", ASL.exp (c, si, e), L.str "}"]
              | DEta r                 =>
                let
                  val {remaining, uncurry, args} = r
                  val l = L.seq [L.str "Uncurries ", var uncurry, vars args, L.str " remaining ", Int.layout remaining]
                in l
                end
      in l
      end

  val addDef =
   fn (state, env, v, d) =>
      let
        val () =
            if showDefs (getPd (state, env)) then
              let
                val stm = getStm (state, env)
                val lv = IM.layoutVariable (v, stm)
                val l = L.mayAlign [L.seq [L.str "Adding def ", lv, L.str " = "],
                                    LU.indent (layoutDef (state, env, d))]
                val () = LayoutUtils.printLayout l
              in ()
              end
            else ()
        val env = envSetDefs (env, VD.insert (envGetDefs env, v, d))
      in env
      end

  val doUncurry =
   fn (state, env) =>
      let
        val C {uncurry} = envGetControl env
      in uncurry
      end

  val clearThunkDefs =
   fn (state, env) =>
      let
        val clear =
         fn (v, d) =>
            case d
             of DThunk {t, ...} => if t then SOME d else NONE
              | _               => SOME d
      in envSetDefs (env, VD.keepAllMap (envGetDefs env, clear))
      end

  val getDef =
   fn (state, env, v) =>
      case VD.lookup (envGetDefs env, v)
       of SOME d => d
        | NONE   => DNone

  val addVar =
   fn (state, env, v) =>
      let
        val vars as ref vd = stateGetVars state
        val () = vars := VD.insert (vd, v, (ref (LsUses []), ref (UcUsedBy [])))
      in ()
      end

  (* Add vars for a parallel binding.  For parallel binding, we share
   * the ref, so that all get kept or eliminated together.
   *)
  val addParVars =
   fn (state, env, vs) =>
      let
        val vars as ref vd = stateGetVars state
        val d = (ref (LsUses []), ref (UcUsedBy []))
        val () = vars := List.fold (vs, vd, fn (v, vd) => VD.insert (vd, v, d))
      in ()
      end

  val addVars =
   fn (state, env, vs) => List.foreach (vs, fn v => addVar (state, env, v))

  val addVars1 =
   fn (state, env, vs) => List.foreach (vs, fn (v, _) => addVar (state, env, v))

  val getVarInfo =
   fn (state, env, v) =>
      case VD.lookup (!(stateGetVars state), v)
       of SOME info => info
        | NONE      =>
          let
            val s = IM.variableString (getStm (state, env), v)
            val s = "No info for var: " ^ s
          in fail ("getVarInfo", s)
          end

  val getVarLiveStatus = #1 o getVarInfo
  val getVarUsedStatus = #2 o getVarInfo

  (* Par bindings share entries, so just use the first *)
  val enterParBinding =
   fn (state, env, vs) =>
      (case vs
        of []   => env
         | v::_ => envSetCurrent (env, getVarLiveStatus (state, env, v)))

  val enterBinding =
   fn (state, env, v) => envSetCurrent (env, getVarLiveStatus (state, env, v))

  val enterNewFunction =
   fn (state, env, f) =>
      let
        val () = newFnInfoForFn (state, env, f, EImplicit)
      in pushCurFn (state, env, f)
      end

  val enterNewThunk =
   fn (state, env, f) =>
      let
        val () = newFnInfoForFn (state, env, f, EPartial)
      in pushCurFn (state, env, f)
      end

  val enterFunction =
   fn (state, env, f) => pushCurFn (state, env, f)

  (* Var is unconditionally kept, use count not changed *)
  val rec keepLive =
   fn (state, env, live) =>
      let
        val liveS = !live
        val () = live := LsLive
        val () =
            case liveS
             of LsLive   => ()
              | LsUses l => List.foreach (l, fn live => keepLive (state, env, live))
      in ()
      end

  (* Use a var in the current context, adjust graphs accordingly *)
  val useVar =
   fn (state, env, v) =>
      let
        val (live, used) = getVarInfo (state, env, v)
        val cur = envGetCurrent env
        val () =
            case !used
             of UcUsed i   => used := UcUsed (i+1)
              | UcUsedBy l => used := UcUsedBy (cur::l)
        val () =
            case !cur
             of LsLive   => keepLive (state, env, live)
              | LsUses l => cur := LsUses (live::l)
      in ()
      end

  (* Var has effects - keep if the current context is kept (but do not change use count) *)
  val effectVar =
   fn (state, env, v) =>
      let
        val live = getVarLiveStatus (state, env, v)
        val cur = envGetCurrent env
        val () =
            case !cur
             of LsLive   => keepLive (state, env, live)
              | LsUses l => cur := LsUses (live::l)
      in ()
      end

  val useVars =
   fn (state, env, vs) => List.foreach (vs, fn v => useVar (state, env, v))

  (* Valid after a complete census *)
  val varIsLive =
   fn (state, env, v) =>
      case !(getVarLiveStatus (state, env, v))
       of LsLive => true
        | _      => false

  val varsAreLive =
   fn (state, env, vts) => List.isEmpty vts orelse (List.exists (vts, fn (v, _) => varIsLive (state, env, v)))

  (* Valid after a complete census *)
  val varUseCount =
   fn (state, env, v) =>
      let
        val ur = getVarUsedStatus (state, env, v)
        val count =
            case !ur
             of UcUsed   i => i
              | UcUsedBy l =>
                let
                  val add =
                   fn (ls, count) => case !ls of LsLive => count + 1 | LsUses _ => count
                  val count = List.fold (l, 0, add)
                  val () = ur := UcUsed count
                in count
                end
      in count
      end

  val setVarUseCount =
   fn (state, env, v, count) =>
      let
        val status = getVarUsedStatus (state, env, v)
        val () =
              status := UcUsed count
      in ()
      end

  val incVarUseCount =
   fn (state, env, v) =>
      let
        val count = varUseCount (state, env, v)
        val () = setVarUseCount (state, env, v, count + 1)
      in ()
      end

  val decVarUseCount =
   fn (state, env, v) =>
      let
        val count = varUseCount (state, env, v)
        val () = setVarUseCount (state, env, v, count - 1)
      in ()
      end

  val setVarLive =
   fn (state, env, v, live) =>
      let
        val r = getVarLiveStatus (state, env, v)
        val () = r := (if live then LsLive else LsUses [])
      in ()
      end

  val replaceVar =
   fn (state, env, v1, v2) =>
      let
        val env = addDef (state, env, v1, DSubst v2)
        val count1 = varUseCount (state, env, v1)
        val count2 = varUseCount (state, env, v2)
        val () = setVarUseCount (state, env, v2, count2 - 1 + count1)
      in env
      end

  val replaceVars =
   fn (state, env, vs1, vs2) => List.fold2 (vs1, vs2, env, fn (v1, v2, env) => replaceVar (state, env, v1, v2))

  (* Given a list of potentially mutually recursive vDefs,
   * topo sort into connected components, turn each component
   * into a Rec or Nonrec as appropriate, and return them
   * in reverse order.
   *)
  val mkRec : state * env * AS.vDef List.t -> AS.vDefg List.t =
   fn (state, env, vds) =>
      let
        val c = getConfig (state, env)
        val vars = VS.fromList (ASU.VDef.variablesDefd vds)
        val vvds = List.map (vds, fn vd => (ASU.VDef.variableDefd vd, (vd, VS.intersection (vars, FVS.vDef (c, vd)))))
        val depsOf = fn (_, (_, vs)) => vs
        val scc = I.variableTopoSort (vvds, depsOf)
        val build =
         fn cc =>
            case cc
             of []          => NONE
              | [(v, (vd, fvs))] => if VS.member (fvs, v) then
                                      SOME (AS.Rec [vd])
                                    else
                                      SOME (AS.Nonrec vd)
              | l           => SOME (AS.Rec (List.map (l, #1 o #2)))
        val vdgs = List.keepAllMap (scc, build)
      in List.rev vdgs
      end

  (* bnds in reverse order *)
  val mkLet : AS.vDefg List.t * AS.exp -> AS.exp =
   fn (rbnds, e) => List.fold (rbnds, e, fn (vdg, e) => AS.Let (vdg, e))

  val newLocalVariable =
   fn (state, env, s, t) => IM.variableFresh (getStm (state, env), s, (t, AS.VkLocal))

  val deriveLocalVariable =
   fn (state, env, v, s, t) => IM.variableRelated (getStm (state, env), v, s, (t, AS.VkLocal))

  val cloneVariable =
   fn (state, env, v) => IM.variableClone (getStm (state, env), v)

  val cloneVariables =
   fn (state, env, vs) => List.map (vs, fn v => cloneVariable (state, env, v))

  val cloneBinder =
   fn (state, env, (v, t)) => (cloneVariable (state, env, v), t)

  val cloneBinders =
   fn (state, env, bs) => List.map (bs, fn b => cloneBinder (state, env, b))

  val typeToReturnTypes =
   fn (state, env, t) =>
      (case TypeRep.repToBase t
        of AS.Arr (_, tys, _) => tys
         | _               => fail ("typeToReturnType", "Not a function type"))

  val unboxTy =
   fn (state, env, t) =>
      (case TypeRep.repToBase t
        of AS.Thunk t => t
         | _          => fail ("unboxTy", "Not a thunk type"))

  val vDefgToVDefs =
   fn vdg =>
      (case vdg
        of AS.Rec vDefs      => SOME vDefs
         | AS.Nonrec vDef    => SOME [vDef]
         | AS.Vdef _         => NONE)

  val vDefgsToVDefs =
   fn vdgs =>
      let
        val doIt = fn (vdg, accO) =>
                      (case (vDefgToVDefs vdg, accO)
                        of (SOME l1, SOME l2) => SOME (List.appendRev (l1, l2))
                         | _                  => NONE)
      in case List.fold (vdgs, SOME [], doIt)
          of SOME l => SOME (List.rev l)
           | NONE   => NONE
      end

  val rec expNonDuplicative =
   fn (state, env, e) =>
      (case e
        of AS.Return vs                    => true
         | AS.PrimApp (s, vs)              => true
         | AS.ExtApp (pname, cc, s, t, vs) => true
         | AS.ConApp (c, vs)               => true
         | AS.App (f, vs, _)               => true
         | AS.Let (defG, e)                => vDefgNonDuplicative (state, env, defG)
                                              andalso expNonDuplicative (state, env, e)
         | AS.Case (v, alts)               => false
         | AS.Lit (l, t)                   => true
         | AS.Cast v                       => true
         | AS.Eval v                       => true)
  and rec vDefgNonDuplicative =
   fn (state, env, vdg) =>
      (case vdg
        of AS.Rec vDefs   => false
         | AS.Nonrec vDef => false
         | AS.Vdef (_, e) => expNonDuplicative (state, env, e))

  val appEffect =
   fn (state, env, f) => getFnFx (state, env, f)

  val evalEffect =
   fn (state, env, f) => getFnFx (state, env, f)

  val rec expIsValue =
   fn (state, env, e) =>
      (case e
        of AS.Return vs                    => true
         | AS.PrimApp (s, vs)              => false
         | AS.ExtApp (pname, cc, s, t, vs) => false
         | AS.ConApp (c, vs)               => true
         | AS.App (f, vs, _)               => false
         | AS.Let (defG, e)                => vDefgIsValue (state, env, defG) andalso expIsValue (state, env, e)
         | AS.Case (v, alts)               => false
         | AS.Lit (l, t)                   => true
         | AS.Cast c                       =>
           (case c
             of AS.FromAddr _              => false
              | AS.ToAddr _                => false
              | AS.NullRef                 => true
              | AS.Bottom _                => false)
         | AS.Eval v                       => false)

  and rec vDefIsValue =
   fn (state, env, vd) =>
      (case vd
        of AS.Vfun _ => true
         | AS.Vthk _ => true)
  and rec vDefgIsValue =
   fn (state, env, vdg) =>
      (case vdg
        of AS.Rec vDefs   => vDefsAreValue (state, env, vDefs)
         | AS.Nonrec vDef => vDefIsValue (state, env, vDef)
         | AS.Vdef (_, e) => expIsValue (state, env, e))
  and rec vDefsAreValue =
   fn (state, env, vDefs) => List.forall (vDefs, fn vd => vDefIsValue (state, env, vd))
  and rec vDefgsAreValue =
   fn (state, env, vDefgs) => List.forall (vDefgs, fn vdg => vDefgIsValue (state, env, vdg))

  val addThunkDef =
   fn (state, env, v, vl, e) =>
      let
        val d = DThunk {t = vl, e = e}
        val env = addDef (state, env, v, d)
      in env
      end

  val showModule =
   fn (stm, pd, m) =>
      let
        val si = I.SymbolInfo.SiManager stm
        val c = PD.getConfig pd
        val l = LayoutUtils.indent (ASL.module (c, si, m))
        val () = LayoutUtils.printLayout l
      in ()
      end

  val showResults =
   fn ((stm, tm), pd, nm, m) =>
      let
        val l = Layout.align [Layout.str ("Results of "^nm^" : ")]
        val () = LayoutUtils.printLayout l
        val () = showModule (stm, pd, m)
      in ()
      end

  structure Census =
  struct
    val show : state * env -> unit =
     fn (state, env) =>
        if showCensus (getPd (state, env)) then
          let
            val ref vd = stateGetVars state
            val stm = getStm (state, env)
            val layout =
             fn (v, (ls, uc)) =>
                let
                  val live = varIsLive (state, env, v)
                  val count = varUseCount (state, env, v)
                  val lv = IM.layoutVariable (v, stm)
                  val ls = if live then L.str " is live with use count "
                           else L.str " is dead with use count "
                  val header =
                      case getFnInfoRefOption (state, env, v)
                       of SOME (ref (FI {size, fx})) =>
                          L.seq [L.str "Function ", lv, L.str " of size ", Int.layout size]
                        | NONE                   => L.seq [L.str "Variable ", lv]
                in L.seq [header, ls, Int.layout count]
                end
            val l = VD.layout (vd, layout)
            val () = LayoutUtils.printLayout l
          in ()
          end
        else
          ()

    val rec doExp : state * env * AS.exp -> effect =
     fn (state, env, e) =>
        let
          val fx =
              case e
               of AS.Return vs                    => (useVars (state, env, vs); ETotal)
                | AS.PrimApp (s, vs)              => (useVars (state, env, vs);
                                                      if Effect.subset (GPO.effects s, Effect.Total) then
                                                        ETotal
                                                      else
                                                        EThreaded)
                | AS.ExtApp (pname, cc, s, t, vs) => (useVars (state, env, vs); EImplicit)
                | AS.ConApp (c, vs)               => (useVars (state, env, vs); ETotal)
                | AS.App (f, vs, effect)          =>
                  let
                    val () = useVar (state, env, f)
                    val () = useVars (state, env, vs)
                  in appEffect (state, env, f)
                  end
                | AS.Let (defG, e) =>
                  let
                    val fx1 = doVDefg (state, env, defG)
                    val fx2 = doExp (state, env, e)
                  in joinEffect (fx1, fx2)
                  end
                | AS.Case (v, alts) =>
                  let
                    val () = useVar (state, env, v)
                    val fx = doAlts (state, env, alts)
                  in fx
                  end
                | AS.Lit (l, t)           => ETotal
                | AS.Cast (AS.FromAddr v) => (useVar (state, env, v); ETotal)
                | AS.Cast (AS.ToAddr v)   => (useVar (state, env, v); ETotal)
                | AS.Cast (AS.NullRef)    => ETotal
                | AS.Cast (AS.Bottom v)   => (useVar (state, env, v); EPartial)
                | AS.Eval v               => (useVar (state, env, v); evalEffect (state, env, v))
        in fx
        end

    and rec doAlts : state * env * AS.alt List.t -> effect  =
     fn (state, env, alts) => List.fold (alts, ETotal, fn (alt, fx) => (joinEffect (fx, doAlt (state, env, alt))))

    and rec doAlt : state * env * AS.alt -> effect =
     fn (state, env, alt) =>
        let
          val fx =
              case alt
               of AS.Acon (con, binds, e) =>
                  let
                    val () = addVars1 (state, env, binds)
                    val fx = doExp (state, env, e)
                  in fx
                  end
                | AS.Alit (l, t, e)       => doExp (state, env, e)
                | AS.Adefault e           => doExp (state, env, e)
        in fx
        end

    and rec doVDef : state * env * AS.vDef -> effect =
     fn (state, env, vd) =>
        let
          val fx =
              case vd
               of AS.Vfun {name, ty, escapes, recursive, fvs, args, body} =>
                  let
                    val () =
                        let
                          val () = addVars1 (state, env, args)
                          val env = enterBinding (state, env, name)
                          val env = enterNewFunction (state, env, name)
                          val fx = doExp (state, env, body)
                          val () = setFnFx (state, env, name, fx)
                        in ()
                        end
                  in ETotal
                  end
                | AS.Vthk {name, ty, escapes, recursive, fvs, body} =>
                  let
                    val () =
                        let
                          val env = enterBinding (state, env, name)
                          val env = enterNewThunk (state, env, name)
                          val fx = case doExp (state, env, body)
                                    of ETotal => ETotal
                                     | _      => EPartial
                          val () = setFnFx (state, env, name, fx)
                        in ()
                        end
                  in ETotal
                  end
        in fx
        end

    and rec doVDefg : state * env * AS.vDefg -> effect =
     fn (state, env, vdg) =>
        let
          val () = incCurFnsSize (state, env)
          val fx =
              case vdg
               of AS.Rec vDefs =>
                  let
                    val vs = ASU.VDef.variablesDefd vDefs
                    val () = addVars (state, env, vs)
                    val fx = List.fold (vDefs, ETotal, fn (vd, fx) => joinEffect (fx, doVDef (state, env, vd)))
                  in fx
                  end
                | AS.Nonrec vDef =>
                  let
                    val v = ASU.VDef.variableDefd vDef
                    val () = addVar (state, env, v)
                    val fx = doVDef (state, env, vDef)
                  in fx
                  end
                | AS.Vdef (vts, e) =>
                  let
                    val (vs, ts) = List.unzip vts
                    val () = addParVars (state, env, vs)
                    val fx =
                        let
                          val env = enterParBinding (state, env, vs)
                        in doExp (state, env, e)
                        end
                    val () = if effectTotal fx then
                               ()
                             else
                               List.foreach (vs, fn v => effectVar (state, env, v))
                  in fx
                  end
        in fx
        end

    val doModule : state * env * AS.module -> unit =
     fn (state, env, m) =>
        let
          val AS.Module (v, vdgs) = m
          (* val () = addVars1 (state, env, tds) *)
          val () = List.foreach (vdgs, fn vdg => let val _ = doVDefg (state, env, vdg) in () end)
          val () = useVar (state, env, v)
        in ()
        end

    val finalize : state * env -> unit =
     fn (state, env) =>
        let
          val finish =
           fn (v, _) => ignore (varUseCount (state, env, v))
          val vars = stateGetVars state
          val () = VD.foreach (!vars, finish)
        in ()
        end

    val census : AS.symbolTableManager * AS.typeManager * PD.t * AS.module -> (state * env) =
     fn (stm, tm, pd, m as AS.Module (_, _)) =>
        let
          val state = S {fns = ref VD.empty, stm = stm, vars = ref VD.empty, tm = tm}
          val env = E {current = ref LsLive,
                       curFns   = [],
                       control = C {uncurry = false},
                       defs = VD.empty,
                       pd = pd}
          val () = doModule (state, env, m)
          (* Finalize, the graph, so that the census
           * functions can be used to adjust use counts during
           * optimization
           *)
          val () = finalize (state, env)
        in (state, env)
        end
  end  (* structure Census *)

  structure Optimize1 =
  struct
    val inlineSmallLimit = 12

    val doVar : state * env * AS.var -> AS.var =
     fn (state, env, v) =>
        let
          val v =
              case getDef (state, env, v)
               of DSubst v => v
                | _       => v
        in v
        end

    val doVars : state * env * AS.var List.t -> AS.var List.t =
     fn (state, env, vs) => List.map (vs, fn v => doVar (state, env, v))

    val filterVDefgs =
     fn (state, env, vdgs) =>
        let
          val filterVDef =
           fn vDef =>
              if varIsLive (state, env, ASU.VDef.variableDefd vDef) then
                SOME vDef
              else
                NONE

          val map =
           fn vDefg =>
              (case vDefg
                of AS.Rec vDefs =>
                   (case List.keepAllMap (vDefs, filterVDef)
                     of [] => NONE
                      | l  => SOME (AS.Rec l))
                 | AS.Nonrec vDef => Option.map (filterVDef vDef, AS.Nonrec)
                 | AS.Vdef (vts, _) =>
                   if varsAreLive (state, env, vts) then
                     SOME vDefg
                   else
                     NONE)
        in List.keepAllMap (vdgs, map)
        end

  (* bnds in reverse order *)
    val mkFilteredLet : state * env * AS.vDefg List.t * AS.exp -> AS.exp =
     fn (state, env, rbnds, e) =>
        let
          val rbnds = filterVDefgs (state, env, rbnds)
        in mkLet (rbnds, e)
        end

    val rec doExp : state * env * AS.exp * AS.ty List.t -> AS.exp =
     fn (state, env, e, tys) =>
        let
          val (env, rbnds, vs) = doExpFlattenNamed (state, env, e, tys)
          val e = AS.Return vs
        in mkFilteredLet (state, env, rbnds, e)
        end

    and rec doBetaCase =
     fn (state, env, e, tys, alts) =>
        let
          val fx = Census.doExp (state, env, e)
          val () = Click.caseBeta (getPd (state, env))
          val find =
           fn alt =>
              (case (e, alt)
                of (AS.ConApp (c1, vs), AS.Acon (c2, ps, b)) =>
                   if c1 = c2 andalso List.length vs = List.length ps then
                     SOME (vs, List.map (ps, #1), b)
                   else
                     NONE
                | (AS.Lit (l1, _), AS.Alit (l2, _, b)) =>
                   if CHU.eqCoreLit (l1, l2) then
                     SOME ([], [], b)
                   else
                     NONE
                | _ => NONE)

          val findD =
           fn alt =>
              (case alt
                of AS.Adefault e => SOME e
                 | _             => NONE)

          val (env, e) =
              case List.peekMap (alts, find)
               of SOME (vs, ps, b) => (replaceVars (state, env, ps, vs), b)
                | NONE =>
                  (case List.peekMap (alts, findD)
                    of SOME b => (env, b)
                     | NONE   => fail ("doBetaCase", "Match failure"))

        in doExpFlatten (state, env, e, tys)
        end

    and rec doEval =
     fn (state, env, v, tys) =>
        let
          val v = doVar (state, env, v)

          val inlineOnce =
           fn e =>
              let
                val () = Click.thunkInOnce (getPd (state, env))
                val e = ASC.exp (getStm (state, env), getConfig (state, env), e)
                val _ = Census.doExp (state, env, e)
                val () = setVarLive (state, env, v, false)
                val (env, rbnds, e) = doExpFlatten (state, env, e, tys)
              in (env, rbnds, e)
              end

          val inlineVal =
           fn e =>
              let
                val () = Click.thunkInVal (getPd (state, env))
                val e = ASC.exp (getStm (state, env), getConfig (state, env), e)
                val _ = Census.doExp (state, env, e)
              in doExpFlatten (state, env, e, tys)
              end
        in
          case (varUseCount (state, env, v), getDef (state, env, v))
           of (1, DThunk {e, ...})    => inlineOnce e
            | (_, DThunk {t=true, e}) => inlineVal e
            | _                       => (env, [], AS.Eval v)
        end

    and rec doApp : state * env * AS.var * AS.var List.t * AS.ty List.t * AS.effect -> env * AS.vDefg List.t * AS.exp =
     fn (state, env, f, vs, tys, effect) =>
        let
          val f = doVar (state, env, f)
          val vs = doVars (state, env, vs)
          val inline =
           fn (vs, ps, e) =>
              let
                val e = ASC.exp (getStm (state, env), getConfig (state, env), e)
                val _ = Census.doExp (state, env, e)
                val env = replaceVars (state, env, ps, vs)
                val (env, rbnds, e) = doExpFlatten (state, env, e, tys)
              in (env, rbnds, e)
              end
          val inlineOnce =
           fn (vs, ps, e) =>
              let
                val () = Click.funcInOnce (getPd (state, env))
                val () = setVarLive (state, env, f, false)
              in inline (vs, ps, e)
              end
          val inlineSmall =
           fn (vs, ps, e) =>
              let
                val () = Click.funcInTotal (getPd (state, env))
              in inline (vs, ps, e)
              end
        in
          case (getDef (state, env, f), varUseCount (state, env, f))
           of (DFunc {vs = ps, e, ...}, 1) => inlineOnce (vs, ps, e)
            | (DFunc {i, r, vs = ps, e}, _) =>
              if expNonDuplicative (state, env, e)
                 andalso ((effectTotal (getFnFx (state, env, f)))
                          orelse (not r andalso i < inlineSmallLimit)) then
                inlineSmall (vs, ps, e)
              else
                (env, [], AS.App (f, vs, effect))
            | _        => (env, [], AS.App (f, vs, effect))
        end

    and rec doExpFlattenNamed : state * env * AS.exp * AS.ty List.t -> env * AS.vDefg List.t * AS.var List.t =
     fn (state, env, e, tys) =>
        let
          val mkOne =
           fn ty =>
              let
                val v = newLocalVariable (state, env, "atmp", ty)
              in v
              end
          val (env, rbnds, e) = doExpFlatten (state, env, e, tys)
          val (rbnds, vs) =
              case e
               of AS.Return vs => (rbnds, vs)
                | _        =>
                  let
                    val vs = List.map (tys, mkOne)
                    val () = addParVars (state, env, vs)
                    val vd = AS.Vdef (List.zip (vs, tys), e)
                    val _ = Census.doExp (state, env, AS.Return vs)
                  in (vd::rbnds, vs)
                  end
        in (env, rbnds, vs)
        end

    and rec doExpFlatten : state * env * AS.exp * AS.ty List.t -> env * AS.vDefg List.t * AS.exp =
     fn (state, env, e, tys) =>
        let
          val return = fn e => (env, [], e)
          val r =
              case e
               of AS.Return vs =>
                  let
                    val vs = doVars (state, env, vs)
                    val e = AS.Return vs
                  in return e
                  end
                | AS.PrimApp (s, vs) =>
                  let
                    val vs = doVars (state, env, vs)
                    val e = AS.PrimApp (s, vs)
                  in return e
                  end
                | AS.ExtApp (pname, cc, s, t, vs) =>
                  let
                    val vs = doVars (state, env, vs)
                    val e = AS.ExtApp (pname, cc, s, t, vs)
                  in return e
                  end
                | AS.ConApp (c, vs) =>
                  let
                    val vs = doVars (state, env, vs)
                    val e = AS.ConApp (c, vs)
                  in return e
                  end
                | AS.App (f, vs, effect) => doApp (state, env, f, vs, tys, effect)
                | AS.Let (defG, e) =>
                  let
                    val outerEnv = env
                    val (env, rbnds1) = doVDefg (state, env, defG)
                    val (env, rbnds2, e) = doExpFlatten (state, env, e, tys)
                  in (env, rbnds2 @ rbnds1, e)
                  end
                | AS.Case (v, [AS.Adefault e])        => doExpFlatten (state, env, e, tys)
                | AS.Case (v, [AS.Alit (l, t, e)])    => doExpFlatten (state, env, e, tys)
                | AS.Case (v, [AS.Acon (con, [], e)]) => doExpFlatten (state, env, e, tys)
                | AS.Case (v, alts) =>
                  let
                    val v = doVar (state, env, v)
                    val r =
                        case getDef (state, env, v)
                         of DDef (e as (AS.Lit lit))        => doBetaCase (state, env, e, tys, alts)
                          | DDef (e as (AS.ConApp (c, vs))) => doBetaCase (state, env, e, tys, alts)
                          | _                               => return (AS.Case (v, doAlts (state, env, alts, tys)))
                  in r
                  end
                | AS.Lit (l, t) =>
                  let
                    val e = AS.Lit (l, t)
                  in return e
                  end
                | AS.Cast cast =>
                  let
                    fun doV v = doVar (state, env, v)
                    val cast = case cast
                                 of AS.FromAddr v => AS.FromAddr (doV v)
                                  | AS.ToAddr v => AS.ToAddr (doV v)
                                  | AS.Bottom v => AS.Bottom (doV v)
                                  | _ => cast

                  in return (AS.Cast cast)
                  end
                | AS.Eval v => doEval (state, env, v, tys)
        in r
        end

    and rec doAlts : state * env * AS.alt List.t * AS.ty List.t -> AS.alt List.t  =
     fn (state, env, alts, tys) => List.map (alts, fn alt => doAlt (state, env, alt, tys))

    and rec doAlt : state * env * AS.alt * AS.ty List.t-> AS.alt =
     fn (state, env, alt, tys) =>
        let
          val alt =
              case alt
               of AS.Acon (con, binds, e) =>
                  let
                    val e = doExp (state, env, e, tys)
                  in AS.Acon (con, binds, e)
                  end
                | AS.Alit (l, t, e) =>
                  let
                    val e = doExp (state, env, e, tys)
                  in AS.Alit (l, t, e)
                  end
                | AS.Adefault e =>
                  let
                    val e = doExp (state, env, e, tys)
                  in AS.Adefault e
                  end
        in alt
        end

    and rec doVDef : state * env * bool * AS.vDef -> env * AS.vDefg List.t * AS.vDef List.t =
     fn (state, env, recursiveB, vd) =>
        let
          val r =
              case vd
               of AS.Vfun {name, ty, escapes, recursive, fvs, args, body} =>
                  let
                    val body =
                        let
                          val env = clearThunkDefs (state, env)
                          val env = enterFunction (state, env, name)
                          val tys = typeToReturnTypes (state, env, ty)
                          val body = doExp (state, env, body, tys)
                        in body
                        end
                    val size = getFnSize (state, env, name)
                    val def = DFunc {i = size, r = recursiveB,
                                     vs = List.map (args, #1), e = body}
                    val env = addDef (state, env, name, def)
                    val recursive =
                        not (effectTotal (getFnFx (state,env, name))) andalso recursive
                    val vd = AS.Vfun {name = name, ty = ty, escapes = escapes, recursive = recursive,
                                      fvs = fvs, args = args, body = body}
                  in (env, [], [vd])
                  end
                | AS.Vthk {name, ty, escapes, recursive, fvs, body} =>
                  let
                    val outerEnv = env
                    val uTy = unboxTy (state, env, ty)
                    val (innerEnv, rbnds, rvs) = doExpFlattenNamed (state, env, body, [uTy])
                    val e = AS.Return rvs
                    val value = vDefgsAreValue (state, innerEnv, rbnds)
                    val vDefGHoistable =
                     fn vDefG =>
                        (case vDefG
                          of AS.Rec _    => true
                           | AS.Nonrec _ => true
                           | AS.Vdef _   => false)
                    val recursive =
                        not (effectTotal (getFnFx (state,env, name))) andalso recursive
                    val (env, rbnds, vds) =
                        if value andalso not recursiveB then
                          let
                            val env = innerEnv
                            val () = case rbnds
                                      of _::_ => Click.toThunkVal (getPd (state, env))
                                       | _    => ()
                            val (vds, env) =
                                let
                                  val default =
                                   fn () =>
                                      let
                                        val vd = AS.Vthk {name = name, ty = ty,
                                                          escapes = escapes, recursive = recursive,
                                                          fvs = fvs, body = e}
                                        val vds = [vd]
                                        val env = addThunkDef (state, env, name, true, e)
                                      in (vds, env)
                                      end
                                in
                                  case e
                                   of AS.Return [v2] =>
                                      (case getDef (state, env, v2)
                                        of DDef (AS.Eval v3) =>
                                           let
                                             val () = Click.thunkReIntro (getPd (state, env))
                                             val vds = []
                                             val env = replaceVar (state, env, name, v3)
                                           in (vds, env)
                                           end
                                         | _ => default ())
                                    | _ => default ()
                                end
                          in (env, rbnds, vds)
                          end
                        else if value andalso recursiveB andalso List.forall (rbnds, vDefGHoistable) then
                          let
                            val env = innerEnv
                            val () = case rbnds
                                      of _::_ => Click.toThunkVal (getPd (state, env))
                                       | _    => ()
                            val vd = AS.Vthk {name = name, ty = ty, escapes = escapes, recursive = recursive,
                                              fvs = fvs, body = e}
                            val vds = vd :: valOf (vDefgsToVDefs rbnds)
                            (* Hoisting out of a recursive binding may change the recursive status
                             * of defs in the context
                             *)
                            val env = outerEnv
                            val env = addThunkDef (state, env, name, true, e)
                          in (env, [], vds)
                          end
                        else
                          let
                            val e = mkFilteredLet (state, env, rbnds, e)
                            val doEta =
                             fn v2 =>
                                let
                                  val () = Click.thunkEta (getPd (state, env))
                                  val env = replaceVar (state, env, name, v2)
                                  val vds =
                                      if recursiveB then
                                        let
                                          val () = incVarUseCount (state, env, v2)
                                          val vd = AS.Vthk {name = name, ty = ty,
                                                            escapes = escapes, recursive = recursive,
                                                            fvs = fvs, body = e}
                                        in [vd]
                                        end
                                      else
                                        []
                                in (env, vds)
                                end
                            val doDefault =
                             fn () =>
                                let
                                  val env = outerEnv
                                  val env = addThunkDef (state, env, name, false, e)
                                  val vd = AS.Vthk {name = name, ty = ty,
                                                    escapes = escapes, recursive = recursive,
                                                    fvs = fvs, body = e}
                                  val vds = [vd]
                                in (env, vds)
                                end
                            val (env, vds) =
                                (case e
                                  of AS.Eval v2 =>  doEta v2
                                   | AS.Let (AS.Vdef ([(v3,_)], AS.Eval v2), AS.Return [v33])  =>
                                     if v3 = v33 then
                                       doEta v2
                                     else
                                       doDefault ()
                                   | _ => doDefault ())
                          in (env, [], vds)
                          end
                  in (env, rbnds, vds)
                  end
        in r
        end

    and rec doVDefg : state * env * AS.vDefg -> env * AS.vDefg List.t =
     fn (state, env, vdg) =>
        let
          val dead =
           fn vd =>
              let
                val live = varIsLive (state, env, ASU.VDef.variableDefd vd)
                val () = if live then () else Click.deadCode (getPd (state, env))
              in not live
              end

          val r =
              case vdg
               of AS.Rec vDefs =>
                  let
                    val doOne =
                     fn (vDef, (env, vDefgs1, vDefs1)) =>
                        let
                          val (env, vDefgs2, vDefs2) = doVDef (state, env, true, vDef)
                          val vDefs = vDefs2 @ vDefs1
                          val vDefgs = vDefgs2 @ vDefgs1
                        in (env, vDefgs, vDefs)
                        end
                    val vDefs = List.removeAll (vDefs, dead)
                    val (env, vDefgs, vDefs) = List.fold (vDefs, (env, [], []), doOne)
                    val vDefgs2 = mkRec (state, env, vDefs)
                  in (env, vDefgs2 @ vDefgs)
                  end
                | AS.Nonrec vDef =>
                  if dead vDef then
                    (env, [])
                  else
                    let
                      val (env, vDefgs, vDefs) = doVDef (state, env, false, vDef)
                      val vDefgs = List.map (vDefs, AS.Nonrec) @ vDefgs
                    in (env, vDefgs)
                    end
                | AS.Vdef (vts, e) =>
                  if List.isEmpty vts orelse List.exists (vts, fn (v, _) => varIsLive (state, env, v)) then
                    let
                      val (vs, ts) = List.unzip vts
                      val (env, rbnds, e) = doExpFlatten (state, env, e, ts)
                      val (env, rbnds) =
                          case (vs, e)
                           of (_, AS.Return rs) =>
                              if List.length vs = List.length rs then
                                let
                                  val env = List.fold2 (vs, rs, env, fn (v, r, env) => replaceVar (state, env, v, r))
                                in (env, rbnds)
                                end
                              else
                                fail ("doVDef", "Mismatched return")
                            | ([v], _) => (addDef (state, env, v, DDef e),
                                           AS.Vdef (vts, e) :: rbnds)
                            | _         => (env, AS.Vdef (vts, e) :: rbnds)
                    in (env, rbnds)
                    end
                  else
                    (env, [])
        in r
        end

    val sinkMain : PD.t * AS.module -> AS.module =
     fn (pd, m) =>
        let
          val AS.Module (mainV, vdgs) = m
          val rec loop =
           fn (vdgs, rbnds) =>
              (case vdgs
                of []             => m
                 | [AS.Nonrec (AS.Vfun {name = f, ty = ty, fvs = [], args = binds, body = e, ...}),
                    main as AS.Nonrec (AS.Vthk {name = t, ty = tyt, fvs = [], body = AS.Return [f'], ...})] =>
                   if f = f' andalso mainV = t then
                     let
                       val mainF = AS.Nonrec (AS.Vfun {name = f, ty = ty, escapes = true, recursive = false,
                                                       fvs = [], args = binds, body = mkLet (rbnds, e)})
                       val vdgs = [mainF, main]
                     in AS.Module (mainV, vdgs)
                     end
                   else
                     m
                   | vdg::vdgs => loop (vdgs, vdg::rbnds))
        in loop (vdgs, [])
        end

    val doModule : (AS.symbolTableManager * AS.typeManager) * PD.t * AS.module -> AS.module =
     fn ((stm, tm), pd, m) =>
        let
          val m = if noSinkMain pd then m else sinkMain (pd, m)
          val (state, env) = Census.census (stm, tm, pd, m)
          val () = Census.show (state, env)
          val AS.Module (v, vdgs) = m
          val doOne =
           fn (vdg, (env, vdgs)) =>
              let
                val (env, vdgs2) = doVDefg (state, env, vdg)
              in (env, vdgs2 @ vdgs)
              end
          val (env, vdgs) = List.fold (vdgs, (env, []), doOne)
          val v = doVar (state, env, v)
          val vdgs = List.rev vdgs
          val vdgs = filterVDefgs (state, env, vdgs)
          val m = AS.Module (v, vdgs)
        in m
        end

  end  (* structure Optimize1 *)

  structure Strictness =
  struct
    structure VarLat = FlatLatticeFn(struct
                                        type element = AS.var
                                        val equal = op =
                                      end)

    datatype strictness = StEffect | StStrictIn of VS.t

    datatype fnInfo = FI of {frees : VS.t,
                             pattern : (bool * bool) List.t}

    datatype sumInfo = SI of {fields : {strict  : bool option,
                                        used    : bool,
                                        escapes : bool,
                                        def     : VarLat.t} List.t}

    datatype state = S of {escapes : VS.t ref,
                           used    : VS.t ref,
                           strict  : VS.t ref,
                           fns     : fnInfo ref VD.t ref,
                           sums    : sumInfo ref ND.t ref,
                           stm     : AS.symbolTableManager,
                           tm      : AS.typeManager}

    datatype env = E of {pd      : PD.t,
                         evals   : AS.var VD.t,
                         inScope : VS.t,
                         newFns  : AS.var VD.t}

    val ((stateSetEscapes, stateGetEscapes),
         (stateSetUsed, stateGetUsed),
         (stateSetStrict, stateGetStrict),
         (stateSetFns, stateGetFns),
         (stateSetSums, stateGetSums),
         (stateSetStm, stateGetStm),
         (stateSetTm, stateGetTm)) =
        let
          val r2t = fn S {escapes, used, strict, fns, sums, stm, tm} =>
                       (escapes, used, strict, fns, sums, stm, tm)
          val t2r = fn (escapes, used, strict, fns, sums, stm, tm) =>
                       S {escapes = escapes, used = used, strict = strict,
                          fns = fns, sums = sums, stm = stm, tm = tm}
        in FunctionalUpdate.mk7 (r2t, t2r)
        end

    val ((envSetPd, envGetPd),
         (envSetEvals, envGetEvals),
         (envSetInScope, envGetInScope),
         (envSetNewFns, envGetNewFns)
        ) =
        let
          val r2t = fn E {pd, evals, inScope, newFns} => (pd, evals, inScope, newFns)
          val t2r = fn (pd, evals, inScope, newFns) => E {pd = pd, evals = evals, inScope = inScope, newFns = newFns}
        in FunctionalUpdate.mk4 (r2t, t2r)
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

    val escapeVar =
     fn (state, env, v) =>
        let
          val er = stateGetEscapes state
          val () = er := VS.insert (!er, v)
        in ()
        end

    val varEscapes =
     fn (state, env, v) =>
        let
          val er = stateGetEscapes state
        in VS.member (!er, v)
        end

    val useVar =
     fn (state, env, v) =>
        let
          val er = stateGetUsed state
          val () = er := VS.insert (!er, v)
        in ()
        end

    val varUsed =
     fn (state, env, v) =>
        let
          val er = stateGetUsed state
        in VS.member (!er, v)
        end

    val strictVar =
     fn (state, env, v) =>
        let
          val er = stateGetStrict state
          val () = er := VS.insert (!er, v)
        in ()
        end

    val varStrict =
     fn (state, env, v) =>
        let
          val er = stateGetStrict state
        in VS.member (!er, v)
        end

    val dropVars =
        fn (state, env) =>
           let
             val er = stateGetEscapes state
             val ur = stateGetUsed state
             val () = er := VS.empty
             val () = ur := VS.empty
           in ()
           end

    val saveVars =
        fn (state, env) =>
           let
             val er as (ref e) = stateGetEscapes state
             val ur as (ref u) = stateGetUsed state
             val () = er := VS.empty
             val () = ur := VS.empty
           in (e, u)
           end

    val mergeVars =
     fn (state, env, (e, u)) =>
        let
          val er = stateGetEscapes state
          val ur = stateGetUsed state
          val () = er := VS.union (!er, e)
          val () = ur := VS.union (!ur, u)
        in ()
        end

    val getFnInfo =
     fn (state, env, v) => VD.lookup (!(stateGetFns state), v)

    val fnInfoLayout =
     fn (state, env, FI {frees, pattern}) =>
        let
          val pat = fn (s, e) => L.seq [L.str (if s then "s" else "n"), L.str (if e then "?" else "!")]
          val ps = List.map (pattern, pat)
          val p = LU.angleSeq ps
          val si = getSi (state, env)
          val config = getConfig (state, env)
          val f = VS.layout (frees, fn v => ASL.var (config, si, v))
        in (p, f)
        end

    val fnInfoStrictness =
     fn (FI {frees, pattern}) => (StStrictIn frees, pattern)

    val fnInfoFrees =
     fn (FI {frees, pattern}) => frees

    val fnInfoNe =
     fn (FI {frees = f1, pattern = p1}, FI {frees = f2, pattern = p2}) =>
        let
          val patternNe = fn ((s1, e1), (s2, e2)) => (s1 <> s2) orelse (e1 <> e2)
        in
          (not (VS.equal (f1, f2))) orelse
          List.fold2 (p1, p2, false, fn (p1, p2, b) => b orelse patternNe (p1, p2))
        end

    val getOptimizedCallingConvention =
     fn (state, env, f) =>
        (case getFnInfo (state, env, f)
          of SOME (ref fi) =>
             let
               val FI {frees, pattern} = fi
             in
               if List.exists (pattern, fn (s, k) => s orelse not k) then
                 SOME fi
               else
                 NONE
             end
           | NONE => NONE)

    val allocateFunction =
     fn (state, env, vd) =>
        let
          val add =
           fn (v, args) =>
             let
               val info = FI {frees = VS.empty,
                              pattern = List.map (args, fn _ => (false, false))}
               val fns = stateGetFns state
               val () = fns := VD.insert (!fns, v, ref info)
             in ()
             end
        in
          case vd
           of AS.Vfun {name, args, ...} => add (name, args)
            | AS.Vthk {name, ...}       => add (name, [])
        end

    val updateFunction =
     fn (state, env, v, args, strictness) =>
        let
          val strict =
              case strictness
               of StEffect          => VS.empty
                | StStrictIn strict => strict
          val fr as (ref (fi1 as (FI {frees, pattern}))) = valOf (getFnInfo (state, env, v))
          val outerStrict = List.fold (args, strict, fn (v, s) => VS.remove (s, v))
          val frees = VS.union (outerStrict, frees)
          val pattern =
              let
                val doOne =
                 fn v =>
                    let
                      val strict = VS.member (strict, v)
                      val keep = varEscapes (state, env, v) orelse
                                 ((not strict) andalso varUsed (state, env, v))
                    in (strict, keep)
                    end
              in List.map (args, doOne)
              end
          val fi = FI {frees = frees, pattern = pattern}
          val changed = fnInfoNe (fi1, fi)
          val () = fr := fi
        in changed
        end

    val sumInfoLayout =
     fn (state, env, SI {fields}) =>
        let
          val config = getConfig (state, env)
          val si = getSi (state, env)
          val layoutStrict =
           fn so =>
              (case so
                of SOME s => L.str (if s then "s" else "n")
                 | NONE   => L.str "?")
          val lb = fn b => L.str (if b then "?" else "!")
          val pat = fn (s, u, e) => L.seq [layoutStrict s, lb u, lb e]
          val ldef = VarLat.layout (fn v => ASL.var (config, si, v))
          val doOne =
              fn {strict, used, escapes, def} => L.seq [pat (strict, used, escapes), L.str " = ", ldef def]
          val p = LU.angleSeq (List.map (fields, doOne))
        in p
        end

    val newSumDefinition =
     fn (state, env, name, l) =>
        let
          val dr = stateGetSums state
          val p = SI {fields = List.map (l, fn d => {strict = NONE, used = false, escapes = false, def = VarLat.bot})}
          val r = ref p
          val () = dr := ND.insert (!dr, name, r)
        in r
        end

    val getSumInfo =
     fn (state, env, n, l) =>
        (case ND.lookup (!(stateGetSums state), n)
          of SOME i => i
           | NONE   => newSumDefinition (state, env, n, l))

    val addSumDefinition =
     fn (state, env, name, defs) =>
        let
          val (r as ref (SI {fields})) = getSumInfo (state, env, name, defs)
          val () = if List.length fields <> List.length defs then
                     fail ("addSumDefinition", "Mismatched sum def/pattern")
                   else
                     ()
          val doOne =
           fn (defN, {strict, used, escapes, def}) =>
              {strict = strict, used = used, escapes = escapes, def = VarLat.join (VarLat.elt defN, def)}
          val fields = List.map2 (defs, fields, doOne)
          val () = r := SI {fields = fields}
        in ()
        end

    val addSumUse =
     fn (state, env, name, info) =>
        let
          val (r as ref (SI {fields})) = getSumInfo (state, env, name, info)
          val () = if List.length info <> List.length fields then
                     fail ("addSumUse", "Mismatched sum use/pattern")
                   else
                     ()
          val joinStrict =
           fn (strict, strictO) =>
              (case strictO
                of NONE         => SOME strict
                 | SOME strict' => SOME (strict andalso strict'))
          val doOne =
           fn ((strictUse, usedUse, escapesUse), {strict, used, escapes, def}) =>
              {strict = joinStrict (strictUse, strict),
               used = usedUse orelse used,
               escapes = escapesUse orelse escapes,
               def = def}
          val fields = List.map2 (info, fields, doOne)
          val () = r := SI {fields = fields}
        in ()
        end

    val getSumPattern =
     fn (state, env, name, l) =>
        let
          val (r as ref (SI {fields})) = getSumInfo (state, env, name, l)
          val doOne =
           fn {strict, used, escapes, def} =>
              let
                val strict = case strict of SOME s => s | NONE => false
                val keep = used orelse escapes
                val def = VarLat.get def
              in (strict, keep, def)
              end
          val p = List.map (fields, doOne)
        in p
        end

    val nonStrict = StStrictIn VS.empty

    val effectStrict = StEffect

    val seqStrict =
     fn (s1, s2) =>
        (case (s1, s2)
          of (StStrictIn s1, StStrictIn s2) => StStrictIn (VS.union (s1, s2))
           | (StEffect,      _            ) => StEffect
           | (_,             StEffect     ) => s1)

    val intersectStrict =
     fn (s1, s2) =>
        (case (s1, s2)
          of (StStrictIn s1, StStrictIn s2) => StStrictIn (VS.intersection (s1, s2))
           | (StEffect,      _            ) => StEffect
           | (_,             StEffect     ) => StEffect)

    val removeStrict =
     fn (s, v) =>
        (case s
          of StStrictIn s => StStrictIn (VS.remove (s, v))
           | StEffect     => StEffect)

    val insertStrict =
     fn (s, v) =>
        (case s
          of StStrictIn s => StStrictIn (VS.insert (s, v))
           | StEffect     => StEffect)

    val memberStrict =
     fn (s, v) =>
        (case s
          of StStrictIn s => VS.member (s, v)
           | StEffect     => false)

    val variableTy =
     fn (state, env, v) =>
        let
          val stm = getStm (state, env)
          val (t, k) = IM.variableInfo (stm, v)
        in t
        end

    val setVariableTy =
     fn (state, env, v, ty) =>
        let
          val stm = getStm (state, env)
          val (_, k) = IM.variableInfo (stm, v)
          val () = IM.variableSetInfo (stm, v, (ty, k))
        in ()
        end

    val unboxTy =
     fn (state, env, v, t) =>
        (case TypeRep.repToBase t
          of AS.Thunk t => t
           | _          =>
             let
               val l = ASL.ty (getConfig (state, env), getSi (state, env), t)
               val s = L.toString l
             in fail ("unboxTy", IM.variableString (getStm (state, env), v) ^ " is not a thunk type: "^s)
             end)

    val unboxedTy =
     fn (state, env, v) => unboxTy (state, env, v, variableTy (state, env, v))

    val setEvalForVariable =
     fn (state, env, v, uv) => envSetEvals (env, VD.insert (envGetEvals env, v, uv))

    val evalForVariable =
     fn (state, env, v) => VD.lookup (envGetEvals env, v)

    val evalVariable =
     fn (state, env, v) =>
        let
          val stm = getStm (state, env)
          val t = unboxedTy (state, env, v)
          val uv = IM.variableRelated (stm, v, "ubx", (t, AS.VkLocal))
          val env = setEvalForVariable (state, env, v, uv)
        in (env, (uv, t))
        end

    val evalVariables =
     fn (state, env, vs) =>
        let
          val doOne =
           fn (v, env) =>
              let
                val (env, bind) = evalVariable (state, env, v)
              in (bind, env)
              end
          val (binds, env) = Utils.List.mapFoldl (vs, env, doOne)
        in (env, binds)
        end

    val mkFnVariable =
     fn (state, env, f, t) =>
        let
          val stm = getStm (state, env)
          val v = IM.variableRelated (stm, f, "opt", (t, AS.VkLocal))
          val env = envSetNewFns (env, VD.insert (envGetNewFns env, f, v))
        in (env, v)
        end

    val getFnVariable =
     fn (state, env, f) => VD.lookup (envGetNewFns env, f)

    val cloneVariable =
     fn (state, env, v) => IM.variableClone (getStm (state, env), v)

    val cloneVariables =
     fn (state, env, vs) => List.map (vs, fn v => cloneVariable (state, env, v))

    val varInScope =
     fn (state, env, v) => VS.member (envGetInScope env, v)

    val addVarToScope =
     fn (state, env, v) => envSetInScope (env, VS.insert (envGetInScope env, v))

    val addVarsToScope =
     fn (state, env, vs) => envSetInScope (env, VS.insertList (envGetInScope env, vs))

    val addBindsToScope =
     fn (state, env, bs) => addVarsToScope (state, env, List.map (bs, fn (v, _) => v))

    structure Analyze =
    struct

      val bindVar =
       fn (state, env, strictness, v) =>
          if memberStrict (strictness, v) then
            let
              val () = strictVar (state, env, v)
            in removeStrict (strictness, v)
            end
          else
            strictness

      val bindVars =
       fn (state, env, strictness, vs) =>
          let
            val doOne = fn (v, strictness) => bindVar (state, env, strictness, v)
          in List.fold (vs, strictness, doOne)
          end

      val doVar : state * env * AS.var -> unit =
       fn (state, env, v) =>
          let
            val () = escapeVar (state, env, v)
            val () = useVar (state, env, v)
          in ()
          end

      val applyVar : state * env * AS.var * AS.var List.t-> strictness =
       fn (state, env, v, vs) =>
          (case getFnInfo (state, env, v)
            of SOME (ref fi) =>
               let
                 val (frees, pattern) = fnInfoStrictness fi
                 val () = if List.length pattern <> List.length vs then
                            fail ("applyVar", "Mismatched formals/actuals")
                          else
                            ()
                 val doOne =
                  fn (v, (strict, keep), s) =>
                     let
                       (* Pass through strictness *)
                       val s = if strict then insertStrict (s, v) else s
                       (* Strict variables are used (their eval is needed),
                        * but not (necessarily) escaping
                        *)
                       val () = if strict then useVar (state, env, v) else ()
                       (* Kept variable escape *)
                       val () = if keep then doVar (state, env, v) else ()
                     in s
                     end
                 val strictness = List.fold2 (vs, pattern, frees, doOne)
                 val () = useVar (state, env, v)
               in strictness
               end
             | _      =>
               let
                 val () = List.foreach (vs, fn v => doVar (state, env, v))
                 val () = useVar (state, env, v)
               in nonStrict
               end)

      val doVars : state * env * AS.var List.t -> unit =
       fn (state, env, vs) => List.foreach (vs, fn v => doVar (state, env, v))

      val rec doExp : state * env * AS.exp -> strictness =
       fn (state, env, e) =>
          let
            val strictness =
                case e
                 of AS.Return vs =>
                    let
                      val () = doVars (state, env, vs)
                    in nonStrict
                    end
                  | AS.PrimApp (s, vs) =>
                    let
                      val () = doVars (state, env, vs)
                    in nonStrict
                    end
                  | AS.ExtApp (pname, cc, s, t, vs) =>
                    let
                      val () = doVars (state, env, vs)
                    in effectStrict
                    end
                  | AS.ConApp (c, vs) =>
                    let
                      val () = doVars (state, env, vs)
                      val (name, _) = c
                      val () = addSumDefinition (state, env, name, vs)
                    in nonStrict
                    end
                  | AS.App (f, vs, effect) =>
                    let
                      val strictness = applyVar (state, env, f, vs)
                    in strictness
                    end
                  | AS.Let (defG, e) =>
                    let
                      val (env, strictness1) = doVDefg (state, env, defG)
                      val strictness2 = doExp (state, env, e)
                      val strictness = seqStrict (strictness1, strictness2)
                      val vs = ASU.VDefg.variableDefd defG
                      val strictness = bindVars (state, env, strictness, vs)
                    in strictness
                    end
                  | AS.Case (v, alts) =>
                    let
                      val () = doVar (state, env, v)
                      val strictness = doAlts (state, env, alts)
                    in strictness
                    end
                  | AS.Lit (l, t) =>
                    let
                    in nonStrict
                    end
                  | AS.Cast cast =>
                    let
                      fun doV v = doVar (state, env, v)
                      val cast = case cast
                                  of AS.FromAddr v => doV v
                                   | AS.ToAddr v   => doV v
                                   | AS.NullRef    => ()
                                   | AS.Bottom v   => doV v
                    in nonStrict
                    end
                  | AS.Eval v =>
                    let
                      val strictness = applyVar (state, env, v, [])
                      val strictness = insertStrict (strictness, v)
                    in strictness
                    end
          in strictness
          end

      and rec doAlts : state * env * AS.alt List.t -> strictness  =
          fn (state, env, alts) =>
             let
               val ss = List.map (alts, fn alt => doAlt (state, env, alt))
               val s =
                   case ss
                    of []    => nonStrict
                     | s::ss => List.fold (ss, s, fn (s1, s2) => intersectStrict (s1, s2))
             in s
             end

      and rec doAlt : state * env * AS.alt -> strictness =
          fn (state, env, alt) =>
             let
               val strictness =
                   case alt
                    of AS.Acon (con as (name, _), binds, e) =>
                       let
                         val strictness = doExp (state, env, e)
                         val strictness = bindVars (state, env, strictness, List.map (binds, #1))
                         val doOne = fn (v, _) => (varStrict (state, env, v),
                                                   varUsed (state, env, v),
                                                   varEscapes (state, env, v))
                         val info = List.map (binds, doOne)
                         val () = addSumUse (state, env, name, info)
                       in strictness
                       end
                     | AS.Alit (l, t, e) =>
                       let
                         val strictness = doExp (state, env, e)
                       in strictness
                       end
                     | AS.Adefault e =>
                       let
                         val strictness = doExp (state, env, e)
                       in strictness
                       end
             in strictness
             end

      and rec doVDef : state * env * AS.vDef -> bool =
          fn (state, env, vd) =>
             let
               val changed =
                   case vd
                    of AS.Vfun {name, ty, escapes, recursive, fvs, args, body} =>
                       let
                         val strictness = doExp (state, env, body)
                         val args = List.map (args, #1)
                         val changed = updateFunction (state, env, name, args, strictness)
                       in changed
                       end
                     | AS.Vthk {name, ty, escapes, recursive, fvs, body} =>
                       let
                         val strictness = doExp (state, env, body)
                         val changed = updateFunction (state, env, name, [], strictness)
                       in changed
                       end
             in changed
             end

      and rec doVDefg : state * env * AS.vDefg -> env * strictness =
          fn (state, env, vdg) =>
             let
               val strictness =
                   case vdg
                    of AS.Rec vDefs =>
                       let
                         val () = List.foreach (vDefs, fn vd => allocateFunction (state, env, vd))
                         val vars = saveVars (state, env)
                         val rec loop =
                          fn () =>
                             if List.fold (vDefs, false, fn (vd, b) => doVDef (state, env, vd) orelse b) then
                               loop (dropVars (state, env))
                             else
                               ()
                         val () = loop ()
                         val () = mergeVars (state, env, vars)
                       in (env, nonStrict)
                       end
                     | AS.Nonrec vDef =>
                       let
                         val () = allocateFunction (state, env, vDef)
                         val changed = doVDef (state, env, vDef)
                       in (env, nonStrict)
                       end
                     | AS.Vdef (vts, e) =>
                       let
                         val strictness = doExp (state, env, e)
                       in (env, strictness)
                       end
             in strictness
             end

      val show : state * env -> unit =
       fn (state, env) =>
          if showStrictnessAnalysis (getPd (state, env)) then
            let
              val si = getSi (state, env)
              val config = getConfig (state, env)
              val escapes = !(stateGetEscapes state)
              val used = !(stateGetUsed state)
              val lv =
                  let
                    val fns = !(stateGetFns state)
                    val doOne =
                     fn (v, ref fi) =>
                        let
                          val e1 = if VS.member (escapes, v) then L.str "(?)" else L.str "(!)"
                          val e2 = if VS.member (used, v) then L.str "(?)" else L.str "(!)"
                          val v = ASL.var (config, si, v)
                          val (p, f) = fnInfoLayout (state, env, fi)
                        in
                          L.mayAlign [L.seq [v, e1, e2, L.str " args are ", p],
                                      L.seq[L.str " also strict in ", f]]
                        end
                  in
                    VD.layout (fns, doOne)
                  end
              val ln =
                  let
                    val sums = !(stateGetSums state)
                    val doOne =
                     fn (n, ref i) =>
                        let
                          val n = SI.layoutName (n, si)
                          val li = sumInfoLayout (state, env, i)
                        in L.seq [n, L.str " = ", li]
                        end
                  in
                    ND.layout (sums, doOne)
                  end
              val l = L.align [L.str "Functions",
                               LU.indent lv,
                               L.str "Constructors",
                               LU.indent ln]
              val ()= LU.printLayout l
            in ()
            end
          else
            ()

      val doModule : state * env * AS.module -> unit =
       fn (state, env, m) =>
        let
          val AS.Module (v, vdgs) = m
          val e = mkLet (List.rev vdgs, AS.Return [v])
          val strictness = doExp (state, env, e)
          val () = show (state, env)
        in ()
        end
    end (* structure Analyze *)

    structure Rewrite =
    struct

      val mkEval : state * env * AS.var -> env * AS.var * AS.vDefg option =
       fn (state, env, v) =>
          (case evalForVariable (state, env, v)
            of SOME vu => (env, vu, NONE)
             | NONE    =>
               let
                 val (env, (vu, tu)) = evalVariable (state, env, v)
                 val vd = AS.Vdef ([(vu, tu)], AS.Eval v)
               in (env, vu, SOME vd)
               end)

      (* Returns bindings in reverse order *)
      val mkEvals : state * env * AS.var List.t -> env * AS.var List.t * AS.vDefg List.t =
       fn (state, env, vs) =>
          let
            val genEval =
             fn (v, (env, vus, rbnds)) =>
                let
                  val (env, vu, bo) = mkEval (state, env, v)
                  val rbnds = case bo of SOME bnd => bnd::rbnds | NONE => rbnds
                in (env, vu::vus, rbnds)
                end
            val (env, vsR, rbnds) = List.fold (vs, (env, [], []), genEval)
          in (env, List.rev vsR, rbnds)
          end

      val doFnInfo : state * env * fnInfo * AS.var List.t
                     -> {std   : AS.var List.t,
                         ubx   : AS.var List.t,
                         frees : VS.t} =
       fn (state, env, fi, args) =>
          let
            val FI {frees, pattern} = fi

            val args = List.zip (args, pattern)

            val std =
                let
                  val keep =
                   fn (v, (strict, keep)) => if keep then SOME v else NONE
                in List.keepAllMap (args, keep)
                end

            val ubx =
                let
                  val keep =
                   fn (v, (strict, keep)) => if strict then SOME v else NONE
                in List.keepAllMap (args, keep)
                end

          in {std = std, ubx = ubx, frees = frees}
          end

      val doConstructor' =
       fn (state, env, name, aa) =>
          let
            val p = getSumPattern (state, env, name, aa)
            val () = if List.length p <> List.length aa then
                       fail ("doConstructor'", "Mismatched sum fields/pattern")
                     else
                       ()
            val getStd =
             fn (a, (strict, keep, def)) => if keep then SOME (a, def) else NONE
            val getUbx =
             fn (a, (strict, keep, def)) => if strict then SOME (a, def) else NONE
            val info = List.zip (aa, p)
            val std = List.keepAllMap (info, getStd)
            val ubx = List.keepAllMap (info, getUbx)
          in (std, ubx)
          end

      val doConstructor =
       fn (state, env, name, aa) =>
          let
            val (std, ubx) = doConstructor' (state, env, name, aa)
            val std = List.map (std, #1)
            val ubx = List.map (ubx, #1)
          in (std, ubx)
          end

      val rec doTy =
       fn (state, env, ty) =>
          let
            val tm = stateGetTm state
            val doTy' = fn ty => doTy (state, env, ty)
            val ty =
                case TypeRep.repToBase ty
                 of AS.Boxed => AS.Boxed
                  | AS.Prim pt => AS.Prim (GPT.mapPrimTy (pt, doTy'))
                  | AS.Arr (ts, rts, effect) => AS.Arr (List.map (ts, doTy'), List.map (rts, doTy'), effect)
                  | AS.Sum arms =>
                    let
                      val doArm =
                       fn (con as (name, _), tys) =>
                          let
                            val tys = List.map (tys, doTy')
                            val (std, _) = doConstructor (state, env, name, tys)
                          in (con, std)
                          end
                      val arms = List.map (arms, doArm)
                    in AS.Sum arms
                    end
                  | AS.Thunk t => AS.Thunk (doTy' t)
          in
            TypeRep.newRep (tm, ty)
          end


      val rec doExp : state * env * AS.exp -> AS.exp =
       fn (state, env, e) =>
          let
            val (env, rbnds, e) = doExpFlatten (state, env, e)
          in mkLet (rbnds, e)
          end

      and rec doExpFlatten : state * env * AS.exp -> (env * AS.vDefg List.t * AS.exp) =
       fn (state, env, e) =>
          let
            val pd = getPd (state, env)
            val return = fn e => (env, [], e)
            val r =
              case e
               of AS.Return vs => return e
                | AS.PrimApp (s, vs) => return e
                | AS.ExtApp (pname, cc, s, t, vs) => return (AS.ExtApp (pname, cc, s, doTy (state, env, t), vs))
                | AS.ConApp (c as (name, _), vs) =>
                  let
                    val (std, _) = doConstructor (state, env, name, vs)
                    val () =
                        let
                          val ol = List.length vs
                          val nl = List.length std
                        in Int.for (0, ol - nl, fn _ => Click.deadField pd)
                        end
                    val e = AS.ConApp (c, std)
                  in (env, [], e)
                  end
                | AS.App (f, vs, effect) =>
                  (case getFnInfo (state, env, f)
                    of SOME (ref fi) =>
                       let
                         val {std, ubx, frees} = doFnInfo (state, env, fi, vs)
                         val (env, ubx, rbnds1) = mkEvals (state, env, ubx)
                         val (f, vs) =
                             case getFnVariable (state, env, f)
                              of SOME fnew =>
                                 (fnew, std @ ubx)
                               | NONE      => (f, vs)
                         val e = AS.App (f, vs, effect)
                       in (env, rbnds1, e)
                       end
                     | _ => return e)
                | AS.Let (defG, e) =>
                  let
                    val (env, rbnds1) = doVDefg (state, env, defG)
                    val (env, rbnds2, e) = doExpFlatten (state, env, e)
                  in (env, rbnds2 @ rbnds1, e)
                  end
                | AS.Case (v, alts) =>
                  let
                    val alts = doAlts (state, env, alts)
                  in (env, [], AS.Case (v, alts))
                  end
                | AS.Lit (l, t) => return (AS.Lit (l, doTy (state, env, t)))
                | AS.Cast cast => return e
                | AS.Eval v    =>
                  (case evalForVariable (state, env, v)
                    of SOME vu => (env, [], AS.Return [vu])
                     | NONE => return e)
          in r
          end

    and rec doAlts : state * env * AS.alt List.t -> AS.alt List.t  =
     fn (state, env, alts) => List.map (alts, fn alt => doAlt (state, env, alt))

    and rec doAlt : state * env * AS.alt -> AS.alt =
     fn (state, env, alt) =>
        let
          val alt =
              case alt
               of AS.Acon (con as (name, _), binds, e) =>
                  let
                    val env = addBindsToScope (state, env, binds)
                    val binds = List.map (binds, fn (v, t) => (v, doTy (state, env, t)))
                    val (std, _) = doConstructor' (state, env, name, binds)
                    val check =
                     fn defO => Option.map (defO, fn v => (v, varInScope (state, env, v)))
                    val (env, binds, rbnds1) =
                        let
                          val doOne =
                           fn ((bind as (v, t), defO), (env, binds, rbnds)) =>
                              let
                                val (binds, rbnds) =
                                    case check defO
                                     of SOME (vd, true) =>
                                        let
                                          val bind = (cloneVariable (state, env, v), t)
                                          val bnd = AS.Vdef ([(v, t)], AS.Return [vd])
                                        in (bind::binds, bnd::rbnds)
                                        end
                                      | _ => (bind::binds, rbnds)
                                val (env, rbnds) =
                                    if varStrict (state, env, v) then
                                      (case mkEval (state, env, v)
                                        of (env, _, SOME bnd) => (env, bnd::rbnds)
                                        | (env, _, NONE)      => (env, rbnds))
                                    else
                                      (env, rbnds)
                              in (env, binds, rbnds)
                              end
                          val (env, rbinds, rbnds) = List.fold (std, (env, [], []), doOne)
                        in (env, List.rev rbinds, rbnds)
                        end
                    val (env, rbnds2, e) = doExpFlatten (state, env, e)
                    val e = mkLet (rbnds2 @ rbnds1, e)
                  in AS.Acon (con, binds, e)
                  end
                | AS.Alit (l, t, e) =>
                  let
                    val t = doTy (state, env, t)
                    val e = doExp (state, env, e)
                  in AS.Alit (l, t, e)
                  end
                | AS.Adefault e =>
                  let
                    val e = doExp (state, env, e)
                  in AS.Adefault e
                  end
        in alt
        end

    and rec doVDef0 : state * env * AS.vDef -> env =
     fn (state, env, vd) =>
        let
          val env =
              case vd
               of AS.Vfun {name, ty, escapes, recursive, fvs, args, body} =>
                  (case getOptimizedCallingConvention (state, env, name)
                    of SOME fi =>
                       let
                         val vs = List.map (args, #1)
                         val {std, ubx, frees} = doFnInfo (state, env, fi, vs)
                         val ty =
                             let
                               (* Symboltable is already rewritten *)
                               val std = List.map (std, fn v => variableTy (state, env, v))
                               val ubx = List.map (ubx, fn v => unboxedTy (state, env, v))
                               val ts = std @ ubx
                               val ty =
                                   case TypeRep.repToBase (doTy (state, env, ty))
                                    of AS.Arr (_, rt, effect) => AS.Arr (ts, rt, effect)
                                     | _              => fail ("doVDef", "Not a function type")
                             in ty
                             end
                         val (env, fNew) = mkFnVariable (state, env, name, TypeRep.newRep_ ty)
                       in env
                       end
                     | NONE => env)
                | AS.Vthk _ => env
          val env = addVarToScope (state, env, ASU.VDef.variableDefd vd)
        in env
        end

    and rec doVDef1 : state * env * AS.vDef * bool -> env * AS.vDefg List.t * AS.vDef List.t =
     fn (state, env, vd, recursive) =>
        let
          val r =
              case vd
               of AS.Vfun {name, ty, escapes = escapes0, recursive = recursive0,
                           fvs, args, body} =>
                  let
                    val ty = doTy (state, env, ty)
                    val args = List.map (args, fn (v, t) => (v, doTy (state, env, t)))
                  in
                    case getOptimizedCallingConvention (state, env, name)
                     of SOME fi =>
                        let
                          val pd = getPd (state, env)
                          val escapes = varEscapes (state, env, name)
                          val vs = List.map (args, #1)
                          val optF =
                              let
                                val {std, ubx, frees} = doFnInfo (state, env, fi, vs)
                                val () = List.foreach (ubx, fn _ => Click.strictArg pd)
                                val std = List.map (std, fn v => (v, variableTy (state, env, v)))
                                val (env, ubx) = evalVariables (state, env, ubx)
                                val args = std @ ubx
                                val body =
                                    if evalFrees pd then
                                      let
                                        val fvs = ASFV.exp (PD.getConfig pd, body)
                                        val frees = VS.toList (VS.intersection (frees, fvs))
                                        val (env, frees, rbnds) = mkEvals (state, env, frees)
                                        val body = doExp (state, env, body)
                                      in mkLet (rbnds, body)
                                      end
                                    else
                                      doExp (state, env, body)
                                val fNew = valOf (getFnVariable (state, env, name))
                                val tNew = variableTy (state, env, fNew)
                              in AS.Vfun {name = fNew, ty = tNew,
                                          escapes = escapes0 andalso escapes, recursive = recursive0,
                                          fvs = [], args = args, body = body}
                              end
                          val vds =
                              if escapes then
                                let
                                  val vs = cloneVariables (state, env, vs)
                                  val effect = case TypeRep.repToBase ty
                                                 of AS.Arr (_, _, efx) => efx
                                                  | _ => Effect.Control (* TODO: shall we error here? *)
                                  val body = doExp (state, env, AS.App (name, vs, effect))
                                  val args = List.map2 (vs, args, fn (v, (_, t)) => (v, t))
                                  val escapeF = AS.Vfun {name = name, ty = ty, escapes = true, recursive = recursive0,
                                                         fvs = [], args = args, body = body}
                                in [escapeF, optF]
                                end
                              else
                                [optF]
                        in (env, [], vds)
                        end
                      | NONE =>
                        let
                          val body = doExp (state, env, body)
                          val vd = AS.Vfun {name = name, ty = ty, escapes = escapes0, recursive = recursive0,
                                            fvs = [], args = args, body = body}
                        in (env, [], [vd])
                        end
                  end
                | AS.Vthk {name, ty, escapes, recursive = recursive0, fvs, body} =>
                  let
                    val ty = doTy (state, env, ty)
                  in
                    case getFnInfo (state, env, name)
                     of SOME (ref fi) =>
                        let
                          val pd = getPd (state, env)
                          val outerEnv = env
                          val {std, ubx, frees} = doFnInfo (state, env, fi, [])
                          val (env, rbnds, e) =
                              if evalFrees pd then
                                let
                                  val fvs = ASFV.exp (PD.getConfig pd, body)
                                  val frees = VS.toList (VS.intersection (frees, fvs))
                                  val (env, frees, rbnds1) = mkEvals (state, env, frees)
                                  val (env, rbnds2, e) = doExpFlatten (state, env, body)
                                in (env, rbnds2 @ rbnds1, e)
                                end
                              else
                                doExpFlatten (state, env, body)
                          val (env, rbnds, body) =
                              if varStrict (state, env, name) andalso not recursive then
                                let
                                  val () = Click.toThunkVal pd
                                in
                                  (env, rbnds, e)
                                end
                              else
                                (outerEnv, [], mkLet (rbnds, e))
                          val vd = AS.Vthk {name = name, ty = ty, escapes = escapes, recursive = recursive0,
                                            fvs = fvs, body = body}
                        in (env, rbnds, [vd])
                        end
                      | NONE =>
                        let
                          val body = doExp (state, env, body)
                          val vd = AS.Vthk {name = name, ty = ty, escapes = escapes, recursive = recursive0,
                                            fvs = fvs, body = body}
                        in (env, [], [vd])
                        end
                  end
        in r
        end

    and rec doVDefg : state * env * AS.vDefg -> env * AS.vDefg List.t =
     fn (state, env, vdg) =>
        let
          val (env, vdgs) = doVDefg0 (state, env, vdg)
          val () = if showStrictnessSteps (getPd (state, env)) then
                     let
                       val lv = fn vdg => ASL.vDefg (getConfig (state, env), getSi (state, env), vdg)
                       val l = L.align [L.mayAlign [L.str "Rewriting :", LU.indent (lv vdg)],
                                        L.mayAlign [L.str "to ", L.align (List.map (vdgs, lv))]]
                     in LU.printLayout l
                     end
                   else
                     ()
        in (env, vdgs)
        end

    and rec doVDefg0 : state * env * AS.vDefg -> env * AS.vDefg List.t =
     fn (state, env, vdg) =>
        let
          val r =
              case vdg
               of AS.Rec vDefs =>
                  let
                    val env = List.fold (vDefs, env, fn (vd, env) => doVDef0 (state,env, vd))
                    val doOne =
                     fn (vd, (env, rbnds1, vds1)) =>
                        let
                          val (env, rbnds2, vds2) = doVDef1 (state, env, vd, true)
                        in (env, rbnds2 @ rbnds1, vds1 @ vds2)
                        end
                    val (env, rbnds, vds) = List.fold (vDefs, (env, [], []), doOne)
                    val vdg = AS.Rec (List.rev vds)
                  in (env, vdg::rbnds)
                  end
                | AS.Nonrec vDef =>
                  let
                    val env = doVDef0 (state, env, vDef)
                    val (env, rbnds, vds) = doVDef1 (state, env, vDef, false)
                    val vdgs = List.map (vds, AS.Nonrec)
                  in (env, vdgs @ rbnds)
                  end
                | AS.Vdef (vts, e) =>
                  let
                    val vts = List.map (vts, fn (v, t) => (v, doTy (state, env, t)))
                    val (env, rbnds, e) = doExpFlatten (state, env, e)
                    val env = addBindsToScope (state, env, vts)
                    val env =
                        case e
                         of AS.Eval v =>
                            (case evalForVariable (state, env, v)
                              of SOME uv => env
                               | NONE    => setEvalForVariable (state, env, v, #1 (hd vts)))
                          | _ => env
                    val rbnds = AS.Vdef (vts, e) :: rbnds
                  in (env, rbnds)
                  end
        in r
        end

    val doStm : state * env * AS.symbolTableManager -> unit =
     fn (state, env, stm) =>
        let
          val vars = IM.variablesList stm
          val doOne =
           fn v =>
              let
                val (t, k) = IM.variableInfo (stm, v)
              in IM.variableSetInfo (stm, v, (doTy (state, env, t), k))
              end
        in List.foreach (vars, doOne)
        end

    val doModule : state * env * AS.module -> AS.module =
     fn (state, env, m) =>
        let
          val () = doStm (state, env, getStm (state, env))
          val AS.Module (v, vdgs) = m
          val doOne =
           fn (vdg, (env, vdgs)) =>
              let
                val (env, vdgs2) = doVDefg (state, env, vdg)
              in (env, vdgs2 @ vdgs)
              end
          val (env, vdgs) = List.fold (vdgs, (env, []), doOne)
          val vdgs = List.rev vdgs
          val m = AS.Module (v, vdgs)
        in m
        end

    end (* structure Rewrite *)

    val doModule : (AS.symbolTableManager * AS.typeManager) * PD.t * AS.module -> AS.module =
     fn ((stm, tm), pd, m as AS.Module (_, _)) =>
        let
          val state = S {fns = ref VD.empty,
                         sums = ref ND.empty,
                         stm = stm,
                         escapes = ref VS.empty,
                         strict = ref VS.empty,
                         used = ref VS.empty,
                         tm = tm}
          val env = E {pd = pd, evals = VD.empty, inScope = VS.empty, newFns = VD.empty}
          val () = Analyze.doModule (state, env, m)
          val m = Rewrite.doModule (state, env, m)
        in m
        end

  end  (* structure Strictness *)

  structure Uncurry =
  struct

    val filterVDefgs =
     fn (state, env, vdgs) =>
        let
          (* Each variable that has an eta def has a live status.
           * All other variables are live.  *)
          val varIsLive =
           fn v =>
              case getDef (state, env, v)
               of DEta _ => varIsLive (state, env, v)
                | _      => true

          val varsAreLive =
            fn vts => List.isEmpty vts orelse (List.exists (vts, fn (v, _) => varIsLive v))

          val filterVDef =
           fn vDef =>
              if varIsLive (ASU.VDef.variableDefd vDef) then
                SOME vDef
              else
                NONE

          val map =
           fn vDefg =>
              (case vDefg
                of AS.Rec vDefs =>
                   (case List.keepAllMap (vDefs, filterVDef)
                     of [] => NONE
                      | l  => SOME (AS.Rec l))
                 | AS.Nonrec vDef => Option.map (filterVDef vDef, AS.Nonrec)
                 | AS.Vdef (vts, _) =>
                   if varsAreLive vts then
                     SOME vDefg
                   else
                     NONE)
        in List.keepAllMap (vdgs, map)
        end

  (* bnds in reverse order *)
    val mkFilteredLet : state * env * AS.vDefg List.t * AS.exp -> AS.exp =
     fn (state, env, rbnds, e) =>
        let
          val rbnds = filterVDefgs (state, env, rbnds)
        in mkLet (rbnds, e)
        end

    val doVar : state * env * AS.var -> AS.var =
     fn (state, env, v) =>
        let
          (* If we encounter a non-application use of something we
           * are uncurrying, we must keep it (and hence all of the
           * partials used to build it, live.
           *)
          val () =
              case getDef (state, env, v)
               of DEta _ => effectVar (state, env, v)
                | _      => ()
        in v
        end

    val doVars : state * env * AS.var List.t -> AS.var List.t =
     fn (state, env, vs) => List.map (vs, fn v => doVar (state, env, v))

    val rec doExp : state * env * AS.exp -> AS.exp =
     fn (state, env, e) =>
        let
          val return = fn e => e
          val r =
              case e
               of AS.Return vs =>
                  let
                    val vs = doVars (state, env, vs)
                    val e = AS.Return vs
                  in return e
                  end
                | AS.PrimApp (s, vs) =>
                  let
                    val vs = doVars (state, env, vs)
                    val e = AS.PrimApp (s, vs)
                  in return e
                  end
                | AS.ExtApp (pname, cc, s, t, vs) =>
                  let
                    val vs = doVars (state, env, vs)
                    val e = AS.ExtApp (pname, cc, s, t, vs)
                  in return e
                  end
                | AS.ConApp (c, vs) =>
                  let
                    val vs = doVars (state, env, vs)
                    val e = AS.ConApp (c, vs)
                  in return e
                  end
                | AS.App (f, vs, effect) =>
                  let
                    val default =
                     fn () =>
                        let
                          val f = doVar (state, env, f)
                          val vs = doVars (state, env, vs)
                          val e = AS.App (f, vs, effect)
                        in e
                        end
                   (* Not all apps are named *)
                    val e =
                        (case getDef (state, env, f)
                          of DEta {remaining, uncurry, args} =>
                             let
                               val remaining = remaining - (List.length vs)
                             in if remaining = 0 then
                                  let
                                    val () = Click.uncurry (getPd (state, env))
                                    val vs = doVars (state, env, vs)
                                    val args = List.appendRev (vs, args)
                                  in
                                    AS.App (uncurry, List.rev args, Effect.Control)
                                  end
                                else
                                  default ()
                             end
                           | _ => default ())
                  in return e
                  end
                | AS.Let (defG, e) =>
                  let
                    val (env, rbnds) = doVDefg (state, env, defG)
                    val e = doExp (state, env, e)
                  in mkFilteredLet (state, env, rbnds, e)
                  end
                | AS.Case (v, alts) =>
                  let
                    val v = doVar (state, env, v)
                    val alts = doAlts (state, env, alts)
                  in return (AS.Case (v, alts))
                  end
                | AS.Lit (l, t) =>
                  let
                    val e = AS.Lit (l, t)
                  in return e
                  end
                | AS.Cast cast =>
                  let
                    fun doV v = doVar (state, env, v)
                    val cast = case cast
                                 of AS.FromAddr v => AS.FromAddr (doV v)
                                  | AS.ToAddr v => AS.ToAddr (doV v)
                                  | AS.Bottom v => AS.Bottom (doV v)
                                  | _ => cast
                  in return (AS.Cast cast)
                  end
                | AS.Eval v =>
                  let
                    val v = doVar (state, env, v)
                    val e = AS.Eval v
                  in return e
                  end
        in r
        end

    and rec doAlts : state * env * AS.alt List.t -> AS.alt List.t  =
     fn (state, env, alts) => List.map (alts, fn alt => doAlt (state, env, alt))

    and rec doAlt : state * env * AS.alt -> AS.alt =
     fn (state, env, alt) =>
        let
          val alt =
              case alt
               of AS.Acon (con, binds, e) =>
                  let
                    val e = doExp (state, env, e)
                  in AS.Acon (con, binds, e)
                  end
                | AS.Alit (l, t, e) =>
                  let
                    val e = doExp (state, env, e)
                  in AS.Alit (l, t, e)
                  end
                | AS.Adefault e =>
                  let
                    val e = doExp (state, env, e)
                  in AS.Adefault e
                  end
        in alt
        end

    and rec doVDef : state * env * AS.vDef -> AS.vDef =
     fn (state, env, vd) =>
        let
          val r =
              case vd
               of AS.Vfun {name, ty, escapes, recursive, fvs, args, body} =>
                  let
                    val body = doExp (state, env, body)
                  in AS.Vfun {name = name, ty = ty, escapes = escapes, recursive = recursive,
                              fvs = fvs, args = args, body = body}
                  end
                | AS.Vthk {name, ty, escapes, recursive, fvs, body} =>
                  let
                    val body = doExp (state, env, body)
                  in AS.Vthk {name = name, ty = ty, escapes = escapes, recursive = recursive,
                              fvs = fvs, body = body}
                  end
        in r
        end

    and rec buildUncurry : state * env * AS.vDef -> env * (AS.vDef option * AS.vDef) =
     fn (state, env, vd) =>
        let
          val rec matchLambdas =
           fn e =>
              (case e
                of AS.Let (AS.Nonrec (AS.Vfun {name = v1, ty = t, args = binds, body = e1, ...}), AS.Return [v2]) =>
                   if v1 = v2 then
                     let
                       val (lambdas, body) = matchLambdas e1
                     in ((v1, t, binds)::lambdas, body)
                     end
                   else
                     ([], e)
                 | _ => ([], e))

          val uncurryTyp =
           fn (t, lambdas) =>
              let
                val tm = stateGetTm state
                val rec loop =
                 fn (t, ll, acc) =>
                    (case (TypeRep.repToBase t, ll)
                      of (_            , (f, t, binds)::ll) => loop (t, ll, List.appendRev (binds, acc))
                       | (AS.Arr (_, t, effect), [])        =>
                          TypeRep.newRep (tm, AS.Arr (List.revMap (acc, #2), t, effect))
                       | (_            , [])                =>
                         fail ("uncurryTyp", "Inner lambda doesn't have lambda type"))
              in loop (t, lambdas, [])
              end
          val (env, vd) =
              case vd
               of AS.Vfun {name = f, ty = t, escapes, recursive, fvs, args = binds, body = e} =>
                  (case matchLambdas e
                    of ([], body) => (env, (NONE, vd))
                     | (ll, body)  =>
                       let
                         val all = (f, t, binds)::ll
                         val uncurryT = uncurryTyp (t, all)
                         val uncurryFormals =
                             let
                               val doOne =
                                fn ((_, _, binds), args) => binds @ args
                             in List.foldr (all, [], doOne)
                             end
                         val binds = cloneBinders (state, env, binds)
                         val ll = List.map (ll, fn (v, t, binds) => (v, t, cloneBinders (state, env, binds)))
                         val uncurryActuals =
                             let
                               val doOne =
                                fn ((_, _, binds), args) => ((List.map (binds, #1)) @ args)
                             in List.foldr ((f, t, binds)::ll, [], doOne)
                             end
                         val uncurryF = deriveLocalVariable (state, env, f, "uncurry", uncurryT)
                         val curryVDef =
                             let
                               (* The inner application may be (semantically) recursive, but all of the outer
                                * functions are not. *)
                               val curryBody = AS.App (uncurryF, uncurryActuals, Effect.Control)
                               val mk = fn ((f, t, binds), (e, recursive)) =>
                                           let
                                             val vd = AS.Vfun {name = f, ty = t, escapes = true, recursive = recursive,
                                                               fvs = [], args = binds, body = e}
                                           in
                                             (AS.Let (AS.Nonrec vd, AS.Return [f]), false)
                                           end
                               val (e, recursive) = List.foldr (ll, (curryBody, recursive), mk)
                             in AS.Vfun {name = f, ty = t, escapes = escapes, recursive = recursive,
                                         fvs = [], args = binds, body = e}
                             end
                         val uncurryVDef = AS.Vfun {name = uncurryF, ty = uncurryT,
                                                    escapes = false, recursive = recursive,
                                                    fvs = [], args = uncurryFormals, body = body}
                         val d = DEta {remaining = List.length uncurryActuals, uncurry = uncurryF, args = []}
                         val env = addDef (state, env, f, d)
                         val () = addVar (state, env, f)
                       in (env, (SOME uncurryVDef, curryVDef))
                       end)
                | _                             => (env, (NONE, vd))
        in (env, vd)
        end
    and rec buildUncurries : state * env * AS.vDef List.t -> (env * (AS.vDef option * AS.vDef) List.t) =
     fn (state, env, vDefs) =>
        let
          val (pairs, env) =
              let
                val doIt =
                 fn (vDef, env) =>
                    let
                      val (env, vDef) = buildUncurry (state, env, vDef)
                    in (vDef, env)
                    end
              in Utils.List.mapFoldl (vDefs, env, doIt)
              end
        in (env, pairs)
        end
    and rec doVDefg : state * env * AS.vDefg -> env * AS.vDefg List.t =
     fn (state, env, vdg) =>
        let
          val (env, vdgs) = doVDefg0 (state, env, vdg)
          val () = if showUncurrySteps (getPd (state, env)) then
                     let
                       val lv = fn vdg => ASL.vDefg (getConfig (state, env), getSi (state, env), vdg)
                       val l = L.align [L.mayAlign [L.str "Rewriting :", LU.indent (lv vdg)],
                                        L.mayAlign [L.str "to ", L.align (List.map (vdgs, lv))]]
                     in LU.printLayout l
                     end
                   else
                     ()
        in (env, vdgs)
        end
    and rec doVDefg0 : state * env * AS.vDefg -> env * AS.vDefg List.t =
     fn (state, env, vdg) =>
        let
          val doPair =
           fn (state, env, (uncurryO, curry)) =>
              (case uncurryO
                of SOME uncurry => [curry, doVDef (state, env, uncurry)]
                 | NONE         => [doVDef (state, env, curry)])
          val r =
              case vdg
               of AS.Rec vDefs =>
                  let
                    val (env, pairs) = buildUncurries (state, env, vDefs)
                    val ls = List.map (pairs, fn pair => doPair (state, env, pair))
                    val vds = List.concat ls
                    val vdgs = mkRec (state, env, vds)
                  in (env, vdgs)
                  end
                | AS.Nonrec vDef =>
                    let
                      val (env, pair) = buildUncurry (state, env, vDef)
                      val vds = doPair (state, env, pair)
                      val vdgs = List.map (vds, AS.Nonrec)
                    in (env, vdgs)
                    end
                | AS.Vdef (vts, e) =>
                  let
                    val (env, e) =
                        case e
                         of AS.App (f, vs, effect) =>
                            (case getDef (state, env, f)
                              of DEta {remaining, uncurry, args} =>
                                 let
                                   val remaining = remaining - (List.length vs)
                                   val vs = doVars (state, env, vs)
                                   val args = List.appendRev (vs, args)
                                 in if remaining = 0 then
                                      let
                                        val () = Click.uncurry (getPd (state, env))
                                      in
                                        (env, AS.App (uncurry, List.rev args, Effect.Control))
                                      end
                                    else if remaining > 0 andalso List.length vts = 1 then
                                      let
                                        val (v, t) = hd vts
                                        (* The previous application is live if this application is live *)
                                        val () =
                                            let
                                              val () = addVar (state, env, v)
                                              val env = enterBinding (state, env, v)
                                            in useVar (state, env, f)
                                            end
                                        val d = DEta {remaining = remaining, uncurry = uncurry, args = args}
                                        val env = addDef (state, env, v, d)
                                      in (env, e)
                                      end
                                    else if remaining > 0 then
                                      (env, e)
                                    else
                                      fail ("Uncurry.doVDefg", "Too many arguments!")
                                 end
                               | _ => (env, doExp (state, env, e)))
                          | _ => (env, doExp (state, env, e))
                    val vd = AS.Vdef (vts, e)
                  in (env, [vd])
                  end
        in r
        end

    val doModule : (AS.symbolTableManager * AS.typeManager) * PD.t * AS.module -> AS.module =
     fn ((stm, tm), pd, m) =>
        let
          val AS.Module (v, vdgs) = m
          val state = S {fns = ref VD.empty, stm = stm, vars = ref VD.empty, tm = tm}
          val env = E {curFns = [],
                       current = ref LsLive,
                       control = C {uncurry = true},
                       defs = VD.empty,
                       pd = pd}
          val doOne =
           fn (vdg, (env, vdgs)) =>
              let
                val (env, vdgs2) = doVDefg (state, env, vdg)
              in (env, vdgs2 @ vdgs)
              end
          val (env, vdgs) = List.fold (vdgs, (env, []), doOne)
          val v = doVar (state, env, v)
          val vdgs = List.rev vdgs
          val vdgs = filterVDefgs (state, env, vdgs)
          val m = AS.Module (v, vdgs)
        in m
        end

  end  (* structure Uncurry *)

  val phase =
      let
        val opts = fn name => ANSS.O {id = SOME name}
        val stdOut = Pervasive.TextIO.stdOut
        val measure =
         fn ((stm, tm), pd, name, m) =>
            let
              val st = IM.finish stm
              val p = (m, st, tm)
              val () = ANSS.program (PD.getConfig pd, opts name, p, stdOut)
            in ()
            end
        val print = showResults
        val log = Chat.log1
      in PD.phase {measure = measure, print = print, log = log}
      end

  val uncurry    =
      phase {name = "Uncurry",
             doIt = Uncurry.doModule, printItP = showUncurry, measureItP = measurePhases,
             statItP = statPhases, skipItP = skipUncurry}

  val optimize1  =
      phase {name = "Optimize1",
             doIt = Optimize1.doModule, printItP = showOptimize1, measureItP = measurePhases,
             statItP = statPhases, skipItP = skipOptimize1}

  val strictness =
      phase {name = "Strictness",
             doIt = Strictness.doModule, printItP = showStrictness, measureItP = measurePhases,
             statItP = statPhases, skipItP = skipStrictness}

  (* XXX should factor this out of here and mil compile --leaf *)
  val opts =
      [
        (#"O", optimize1 , "General optimization"),
        (#"S", strictness, "Strictness"          ),
        (#"U", uncurry   , "Uncurry"             )
      ]

  val getOptInfo =
   fn c =>
      let
        fun match (c', p, d) = if c = c' then SOME (p, d) else NONE
      in List.peekMap (opts, match)
      end

  val getOpt =
   fn c => Option.map (getOptInfo c, #1)

  val getOptDescription =
   fn c => Option.map (getOptInfo c, #2)

  datatype controlItem = CiPrint
                       | CiPass of ((AS.symbolTableManager * AS.typeManager) * PD.t * AS.module -> AS.module) * string

  fun parseControl s =
      let
        val || = Parse.||
        val && = Parse.&&
        val -&& = Parse.-&&
        infixr 6 -&&
        infixr && ||
        val $ = Parse.$

        val simple : controlItem Parse.t =
            let
              fun doOne c =
                  case c
                   of #"+" => SOME CiPrint
                    | _    => Option.map (getOptInfo c, CiPass)
            in Parse.satisfyMap doOne
            end

        val consume = fn c => Parse.ignore (Parse.satisfy (fn c' => c = c'))

        val lparen = consume #"("
        val rparen = consume #")"
        val lbracket = consume #"["
        val rbracket = consume #"]"
        val lbrace = consume #"{"
        val rbrace = consume #"}"

        val whitespace =
            let
              val whiteChar =
               fn c => c = Char.space orelse c = Char.newline orelse c = #"\t" orelse c = #"\r"
              val white = Parse.satisfy whiteChar
            in Parse.ignore (Parse.oneOrMore white)
            end

        val delimited : unit Parse.t * 'a Parse.t * unit Parse.t -> 'a Parse.t =
         fn (left, item, right) =>
            let
              val p = left && item && right
              val f = fn ((), (i, ())) => i
            in Parse.map (p, f)
            end

        val nat : int Parse.t =
            let
              val p = Parse.oneOrMore (Parse.satisfy Char.isDigit)
              val f = Int.fromString o String.implode
              val p = Parse.map (p, f)
              val p = Parse.required (p, "Expected natural number")
            in p
            end
        val exponentiated : 'a Parse.t -> 'a list Parse.t =
         fn p =>
            let
              val suffix = (consume #"^") && nat
              val p = p && (Parse.optional suffix)
              val f = fn (p, opt) =>
                         (case opt
                           of SOME ((), n) => List.duplicate (n, fn () => p)
                            | NONE => [p])
              val p = Parse.map (p, f)
            in p
            end
        val rec pass' : unit -> controlItem list Parse.t =
         fn () => Parse.map ($passSeq', List.concat)
        and rec passSeq' : unit -> controlItem list list Parse.t =
         fn () => Parse.oneOrMore ($passHead')
        and rec passHead' : unit -> controlItem list Parse.t =
         fn () => Parse.oneOrMore simple || $iterated'  || $constructed' || (whitespace -&& $passHead')
        and rec iterated' : unit -> controlItem list Parse.t =
         fn () => Parse.map(exponentiated ($parenthesized'), List.concat)
        and rec parenthesized' : unit -> controlItem list Parse.t =
         fn () => delimited (lparen, $pass', rparen)
        and rec constructed' : unit -> controlItem list Parse.t =
         fn () =>
            let
              val p = $interleave' && $pass' && $interleave'
              val p = delimited (lbracket, p, rbracket)
              val f = fn (pre, (pass, post)) => List.concatMap (pass, fn e => pre @ [e] @ post)
            in Parse.map (p, f)
            end
        and rec interleave' : unit -> controlItem list Parse.t =
         fn () => Parse.map ($interleave0', fn opt => Utils.Option.get (opt, []))
        and rec interleave0' : unit -> controlItem list option Parse.t =
         fn () => Parse.optional ($interleave1')
        and rec interleave1' : unit -> controlItem list Parse.t =
         fn () => delimited (lbrace, $pass', rbrace)

        val eof = Parse.atEnd || Parse.error "Expected end of string"
        val pass = Parse.map ($pass' && eof, #1)

        val cis =
            case Parse.parse (pass, (s, 0))
             of Parse.Success (_, cis) => SOME cis
              | Parse.Failure          => SOME []
              | Parse.Error _          => NONE

      in cis
      end

  val o0String = ""
  val o1String = "O"
  val o2String = "[{O}SU]O"
  val o3String = "OOOO[{O}USUS]O"

  val o0Control = Option.valOf (parseControl o0String)
  val o1Control = Option.valOf (parseControl o1String)
  val o2Control = Option.valOf (parseControl o2String)
  val o3Control = Option.valOf (parseControl o3String)

  val dftControls =
      Vector.fromList [o0Control, o1Control, o2Control, o3Control]

  fun dftControl c = Vector.sub (dftControls, Config.flrcOpt c)

  fun describeOpt (c, p, d) =
      L.seq [Char.layout c, L.str " => ", L.str d]

  fun describeControl () =
      L.align
      [L.align [L.str (passname ^ " control string is of the form:"),
                LU.indent (L.str "Desc ::= Pass* | [{Desc}Desc{Desc}] | ( Desc )^n")],
       L.str " ",
       L.align [L.str "A descriptor of the form [{Desc0}Desc{Desc1}] consists of the passes",
                LU.indent (L.str "described by Desc with Desc0 prepended to each pass, and Desc1 appended"),
                LU.indent (L.str "to each pass.  The {Desc} fields are optional. ")],
       L.str " ",
       L.align [L.str "A descriptor of the form (Desc)^n consists of n successive copies of Desc.",
                LU.indent (L.str "The ^n is optional, and is assumed to be 1 if elided.")],
       L.str " ",
       L.str "A pass consists of any of the following characters:",
       LU.indent
         (L.align (List.map (opts, describeOpt) @
                   [L.str "+ => Print"])),
       L.str "defaults are:",
       LU.indent
         (L.align [L.seq [L.str "-O 0 => ", L.str o0String],
                   L.seq [L.str "-O 1 => ", L.str o1String],
                   L.seq [L.str "-O 2 => ", L.str o2String],
                   L.seq [L.str "-O 3 => ", L.str o3String]])]

  val (control, controlGet) =
      Config.Control.mk (passname, describeControl, parseControl, dftControl)

  val runItem =
   fn ci =>
      fn (stm, tm, pd, m) =>
         (case ci
           of CiPrint          => let val () = showModule (stm, pd, m) in m end
            | CiPass (pass, _) => pass ((stm, tm), pd, m))

  val run =
   fn (stm, tm, pd, m, cis) =>
      List.fold (cis, m, fn (ci, m) => runItem ci (stm, tm, pd, m))

  val program : AS.t * PD.t -> AS.t =
   fn (p as (m, im, tm), pd) =>
      let
        val stm = IM.fromExistingAll im

        val config = PassData.getConfig pd
        val cis = controlGet config
        val m = run (stm, tm, pd, m, cis)
        val st = IM.finish stm
        val () = PD.report (pd, passname)
      in (m, st, tm)
      end

  val stater = ANormStrictStats.layout (ANormStrictStats.O {id = SOME passname})

  val description = {name        = passname,
                     description = "Cleanup and optimize ANorm",
                     inIr        = { printer = Utils.Function.flipIn ASL.layout,
                                     stater  = stater },
                     outIr       = { printer = Utils.Function.flipIn ASL.layout,
                                     stater  = stater },
                     mustBeAfter = [],
                     stats       = Click.stats}

  val associates = {controls = [control], debugs = debugs, features = features, subPasses = []}

  val pass = Pass.mkOptPass (description, associates, program)

end
