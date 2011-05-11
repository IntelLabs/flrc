(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_REP = 
sig
  val pass : (BothMil.t, BothMil.t) Pass.t
end

structure MilRep :> MIL_REP = 
struct
  val passname = "MilRep"
  val fail = 
   fn (fname, msg) => Fail.fail ("rep.sml", fname, msg)

  structure M = Mil
  structure PD = PassData

  structure Chat = ChatF (struct 
                            type env = PD.t
                            val extract = PD.getConfig
                            val name = passname
                            val indent = 2
                          end)


  val mkDebug = 
   fn (tag, description) => PD.mkDebug (passname^":"^tag, description)

  val (debugPassD, debugPass) =
      PD.mkDebug (passname ^ ":debug", "Debug rep analysis according to debug level")

  val mkLevelDebug = 
   fn (tag, description, level) => PD.mkLevelDebug (passname, passname^":"^tag, description, level, debugPass)

  val (checkPhasesD, checkPhases) =
      mkLevelDebug ("check-phases", "Check IR between each phase", 0)

  val (showPhasesD, showPhases) =
      mkLevelDebug ("show-phases", "Show IR between each phase", 0)

  val (showAnalysisD, showAnalysis) =
      mkLevelDebug ("show-analysis", "Show analysis results", 1)

  val (annotateProgramD, annotateProgram) =
      mkDebug ("annotate", "Annotate program variables with class")

  val (skipReconstructionD, skipReconstruction) =
      mkDebug ("skip-reconstruction", "Don't reconstruct types")

  val (skipOptimizationD, skipOptimization) =
      mkDebug ("skip-optimization", "Don't optimize")

  val debugs = [annotateProgramD, checkPhasesD, debugPassD, showAnalysisD, showPhasesD, 
                skipReconstructionD, skipOptimizationD] 
               @ MilRepPrep.debugs
               @ MilRepReconstruct.debugs 
               @ MilRepSummary.debugs
               @ MilRepOptimize.debugs

  val features = MilRepBase.features

  val statPhases = MilRepBase.statPhases

  val stats = []
              @ MilRepPrep.stats
              @ MilRepOptimize.stats

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

  val preProcess = doPhase (fn _ => false, MilRepPrep.program, "Pre-processing")

  val optimize = fn (pd, summary, p) => 
                    doPhase (skipOptimization, 
                          fn (pd, p) => MilRepOptimize.program (pd, summary, p),
                             "Optimization") (pd, p)

  val reconstruct = fn (pd, summary, p) => 
                       doPhase (skipReconstruction, 
                                fn (pd, p) => MilRepReconstruct.program (pd, summary, p),
                                "Reconstruction") (pd, p)

  val annotate = fn (pd, summary, p) => 
                    doPhase (fn pd => not (annotateProgram pd),
                             fn (pd, p) => MilRepShow.annotate (pd, summary, p),
                             "Annotation") (pd, p)

  val analyze = 
   fn (pd, p) => 
      let
          val () = Chat.log1 (pd, "Doing analysis")
          val s = Time.now ()
          val summary = MilRepAnalyze.program (pd, p)
          val e = Time.toString (Time.- (Time.now (), s))
          val () = Chat.log1 (pd, "Done with analysis in "^e^"s")
          val () = 
              if showAnalysis pd then
                MilRepShow.printAnalysis (pd, summary, p)
              else
                ()
      in summary
      end

  val program = 
   fn (pd, p) =>
      let
        val p = preProcess (pd, p)
        val summary = analyze (pd, p)
        val p = optimize (pd, summary, p)
        val p = reconstruct (pd, summary, p)
        val p = annotate (pd, summary, p)
        val () = PD.report (pd, passname)
      in p
      end

  val description = {name        = passname,
                     description = "Representation optimisation",
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

end
