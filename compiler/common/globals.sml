signature GLOBALS = 
sig
  val disableOptimizedRationals : Config.t -> bool
  val features : Config.Feature.feature list
  val debugs   : Config.Debug.debug list
  val controls : Config.Control.control list
end
structure Globals :> GLOBALS = 
struct
  val (disableOptimizedRationalsF, disableOptimizedRationals) =
      Config.Feature.mk ("PPiler:disable-optimized-rationals",
                         "disable optimized rational rep")

   val features = [disableOptimizedRationalsF]
   val debugs = []
   val controls = []
end