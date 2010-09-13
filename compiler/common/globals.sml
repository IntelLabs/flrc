(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

signature GLOBALS = 
sig

  val cspAltSem                    : Config.t -> bool
  val cspDirect                    : Config.t -> bool
  val cspHybrid                    : Config.t -> bool
  val cspToMil                     : Config.t -> bool
  val disableOptimizedRationals    : Config.t -> bool
  val disableOptimizedIntegers     : Config.t -> bool

  val features : Config.Feature.feature list
  val debugs   : Config.Debug.debug list
  val controls : Config.Control.control list

end;

structure Globals :> GLOBALS = 
struct

  val (cspAltSemF, cspAltSem) = Config.Feature.mk ("PPiler:csp-alt-sem", "use alternate semantics for Core SP")
  val (cspDirectF, cspDirect) = Config.Feature.mk ("PPiler:csp-direct", "use direct scheme for Core SP compilation")
  val (cspGeneralF, cspGeneral) =
      Config.Feature.mk ("PPiler:csp-general", "use general scheme for Core SP compilation")
  val (cspHybridF, cspHybrid) =
      Config.Feature.mk ("PPiler:csp-hybrid", "use hybrid scheme for Core SP compilation")
  val (cspToMilF, cspToMil) =
      Config.Feature.mk ("PPiler:csp-to-mil", "use Mil path for Core SP compilation")

  val (disableOptimizedRationalsF, disableOptimizedRationals) =
      Config.Feature.mk ("PPiler:disable-optimized-rationals",
                         "disable optimized rational rep")

  val (disableOptimizedIntegersF, disableOptimizedIntegers) =
      Config.Feature.mk ("PPiler:disable-optimized-integers",
                         "disable optimized integer rep")

  val features = [cspAltSemF, cspDirectF, cspHybridF, cspToMilF, disableOptimizedRationalsF, disableOptimizedIntegersF]
  val debugs = []
  val controls = []

end;
