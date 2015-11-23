(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)

structure MilRep :>
                  sig 
                    val debugs : Config.Debug.debug list
                    val features : Config.Feature.feature list
                    structure Flatten : MIL_REP_PASS
                    structure Dce : MIL_REP_PASS
                    structure Optimize : MIL_REP_PASS
                  end = 
struct
  val debugs = MilRepPrep.debugs
               @ MilRepReconstruct.debugs 

  val features = MilRepPrep.features 
                 @ MilRepReconstruct.features

  structure Flatten :> MIL_REP_PASS = MilRepDriverF(structure Optimization = MilRepFlattenOptimization)
  structure Dce :> MIL_REP_PASS = MilRepDriverF(structure Optimization = MilRepDceOptimization)
  structure Optimize :> MIL_REP_PASS = MilRepDriverF(structure Optimization = MilRepOptimization)

end (* structure MilRep *)


