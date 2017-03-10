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


signature MIL_REP_PASS = 
sig
  val pass : (BothMil.t, BothMil.t) Pass.t
end

signature MIL_REP_OPTIMIZATION = 
sig
  val passname : string
  val description : string
  val reconstructTypes : bool
  val debugs : Config.Debug.debug list
  val features : Config.Feature.feature list
  val stats : (string * string) list
  val debugPass : PassData.t -> bool
  val program : PassData.t * MilRepSummary.summary * Mil.t -> Mil.t
end

functor MilRepDriverF(structure Optimization : MIL_REP_OPTIMIZATION) :> MIL_REP_PASS = 
struct
  val passname = Optimization.passname

  structure M = Mil
  structure PD = PassData

  structure Chat = ChatF (struct 
                            type env = PD.t
                            val extract = PD.getConfig
                            val name = passname
                            val indent = 2
                          end)

  val debugPass = Optimization.debugPass

  val mkDebug = 
   fn (tag, description) => PD.mkDebug (passname^":"^tag, description)

  val mkLevelDebug = 
   fn (tag, description, level) => PD.mkLevelDebug (passname, passname^":"^tag, description, level, debugPass)

  val (checkPhasesD, checkPhases) =
      mkLevelDebug ("check-phases", "Check IR between each phase", 0)

  val (showPhasesD, showPhases) =
      mkLevelDebug ("show-phases", "Show IR between each phase", 0)

  val (showAnalysisD, showAnalysis) =
      mkLevelDebug ("show-analysis", "Show analysis results", 1)

  val (showReasonsD, showReasons) =
      mkLevelDebug ("show-reasons", "Show analysis escape/intrudes reasons", 1)

  val (annotateProgramD, annotateProgram) =
      mkDebug ("annotate", "Annotate program variables with class")

  val (skipOptimizationD, skipOptimization) =
      mkDebug ("skip-optimization", "Don't optimize")


  val debugs = [annotateProgramD, checkPhasesD, showAnalysisD, showPhasesD, showReasonsD,
                skipOptimizationD] 
               @ Optimization.debugs

  val mkLogFeature = 
   fn (tag, description, level) => PD.mkLogFeature (passname, passname^":"^tag, description, level)

  val (statPhasesF, statPhases) = 
      mkLogFeature ("stat-phases", "Show stats between each phase", 2)

  val features = [statPhasesF] 
                 @ Optimization.features
                 @ MilRepAnalyze.features

  val stats = []
              @ Optimization.stats

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
                          fn (pd, p) => Optimization.program (pd, summary, p),
                             "Optimization") (pd, p)

  val reconstruct = fn (pd, summary, p) => 
                       doPhase (fn _ => not Optimization.reconstructTypes, 
                                fn (pd, p) => MilRepReconstruct.program (pd, summary, false, p),
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
          val () = 
              if showReasons pd then
                MilRepShow.printReasons (pd, summary, p)
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
                     description = Optimization.description,
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
