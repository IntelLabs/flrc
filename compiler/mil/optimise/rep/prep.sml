(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_REP_PREP = 
sig
  val debugs : Config.Debug.debug list
  val features : Config.Feature.feature list
  val program : PassData.t * Mil.t -> Mil.t
end

structure MilRepPrep :> MIL_REP_PREP = 
struct
  val passname = "MilRepPrep"
  val fail = 
   fn (fname, msg) => Fail.fail ("prep.sml", fname, msg)

  structure M = Mil
  structure MU = MilUtils
  structure PD = PassData

  val debugs = []

  val mkFeature = 
   fn (tag, description) => PD.mkFeature (passname ^":"^ tag, description)

  val (splitAggressiveF, splitAggressive) =
      mkFeature ("split-aggressive", "Split globals aggressively")

  val (noSplittingF, noSplitting) =
      mkFeature ("no-global-splitting", "Disable global splitting")

  val features = [splitAggressiveF, noSplittingF]

  structure Split = 
  struct
    structure I = Identifier
    structure IM = Identifier.Manager
    structure VD = I.VariableDict
    structure IVD = I.ImpVariableDict
    structure VS = I.VariableSet
    structure M = Mil
    structure MSTM = MilUtils.SymbolTableManager
    structure MRC = MilRewriterClient 
    structure MFV = MilFreeVars

    datatype state = S of {stm : MSTM.t, 
                           splitVariables : M.variable list IVD.t}
                          
    datatype env = E of {pd : PassData.t,
                         candidates : VS.t}
                          
    local
      val getS = fn g => fn (S t) => g t
      val getE = fn g => fn (E t) => g t
    in
      val stateGetStm = getS #stm
      val stateGetSplitVariables = getS #splitVariables
      val envGetPD = getE #pd
      val envGetCandidates = getE #candidates
      val envSetCandidates = 
       fn (E {pd, ...}, candidates) => E {pd = pd, candidates = candidates}
    end
           
    val envGetConfig = PD.getConfig o envGetPD

    val freshSplitVariable = 
     fn (state, env, v) => MSTM.variableClone (stateGetStm state, v)

    val generateSplitVariable = 
     fn (state, env, v) =>
        let
          val d = stateGetSplitVariables state
          val v = 
              (case IVD.lookup (d, v)
                of SOME l => 
                   let
                     val vNew = freshSplitVariable (state, env, v)
                     val () = IVD.insert (d, v, vNew::l)
                   in vNew
                   end
                 | NONE => 
                   let
                     val () = IVD.insert (d, v, [])
                   in v
                   end)
        in v
        end

    val splitCandidate = 
     fn (state, env, v) => VS.member (envGetCandidates env, v)

    val splitVariable = 
     fn (state, env, v) => 
        if splitCandidate (state, env, v) then
          SOME (generateSplitVariable (state, env, v))
        else
          NONE

    structure Rename = 
    MilRewriterF (struct
                    type state      = state
                    type env        = env
                    val config      = envGetConfig
                    val label       = fn _ => MRC.Continue
                    val variable    = 
                     fn (state, env, v) => 
                        (case splitVariable (state, env, v) 
                          of SOME v => MRC.StopWith (env, v)
                           | NONE => MRC.Stop)
                    val operand     = fn _ => MRC.Continue
                    val instruction = fn _ => MRC.Continue
                    val transfer    = fn _ => MRC.Continue
                    val block       = fn _ => MRC.Continue
                    val global      = fn _ => MRC.Continue
                    val bind        = fn (_, env, _) => (env, NONE)
                    val bindLabel   = fn (_, env, _) => (env, NONE)
                    val indent      = 2
                    val cfgEnum     = fn (_, _, t) => MilUtils.CodeBody.dfsTrees t
                  end)

    val addCandidatesForScc = 
     fn (state, env, cc) => 
        let
          val splittable = 
           fn g => 
              (case g 
                of M.GCode _ => false 
                 | _         => MU.Global.immutable g)
          val help = 
           fn ((v, g), s) => if splittable g then VS.insert (s, v) else s
          val candidates = List.fold (cc, envGetCandidates env, help)
          val env = envSetCandidates (env, candidates)
        in env
        end

    val splitGlobal = 
     fn (state, env, vg as (v, g)) => 
        (case IVD.lookup (stateGetSplitVariables state, v)
          of SOME vs => vg :: (List.map (vs, fn v => (v, g)))
           | NONE => [vg])

    val renameGlobal = Rename.global

    val renameGlobals = 
     fn (state, env, gs) => List.map (gs, fn vg => renameGlobal (state, env, vg))

    val rewriteGlobal = 
     fn (state, env, vg) => 
        if splitAggressive (envGetPD env) then
          let
            val globals = splitGlobal (state, env, vg)
            val globals = renameGlobals (state, env, globals)
          in globals
          end
        else
          let
            val vg = renameGlobal (state, env, vg)
            val globals = splitGlobal (state, env, vg)
          in globals
          end 

    val rec doSccs = 
     fn (state, env, scc) => 
        (case scc 
          of [] => []
           | cc::scc => 
             let
               val globals = 
                   let
                     val env = addCandidatesForScc (state, env, cc)
                     val globals = doSccs (state, env, scc)
                   in globals 
                   end
               val globals =
                   List.fold (cc, globals, fn (vg, globals) => rewriteGlobal (state, env, vg) @ globals)
             in globals
             end)

    val doGlobals = 
     fn (state, env, globals) => 
        let
          val config = envGetConfig env
          val depsOf = fn (v, g) => MFV.global (config, v, g)
          val scc = I.variableTopoSort (VD.toList globals, depsOf)
          val globals = doSccs (state, env, scc)
          val globals = List.fold (globals, VD.empty, fn ((v, g), d) => VD.insert (d, v, g))
        in globals
        end

    val program = 
     fn (pd, p) => 
        let
          val M.P {includes, externs, symbolTable, globals, entry} = p
          val stm = IM.fromExistingAll symbolTable
          val state = S {stm = stm, splitVariables = IVD.empty ()}
          val env = E {pd = pd, candidates = VS.empty}
          val globals = doGlobals (state, env, globals)
          val st = IM.finish stm
          val p = M.P {includes = includes, externs = externs, symbolTable = st, globals = globals, entry = entry}
        in p
        end

  end (* structure Split *)

  val program = 
   fn (pd, p) => 
      let
        val config = PD.getConfig pd
        val p = if noSplitting pd then p else Split.program (pd, p)
        val name = fn c => true
        val p = MilNameSmallValues.program (config, name, p)
        val (p, i) = MilNumberInstructions.program (config, p)
      in p
      end
end
