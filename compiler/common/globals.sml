(* The Intel FL to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature GLOBALS = 
sig

  val disableOptimizedRationals    : Config.t -> bool
  val disableOptimizedIntegers     : Config.t -> bool

  val features : Config.Feature.feature list
  val debugs   : Config.Debug.debug list
  val controls : Config.Control.control list

end;

structure Globals :> GLOBALS = 
struct

  val (disableOptimizedRationalsF, disableOptimizedRationals) =
      Config.Feature.mk ("IFLC:disable-optimized-rationals",
                         "disable optimized rational rep")

  val (disableOptimizedIntegersF, disableOptimizedIntegers) =
      Config.Feature.mk ("IFLC:disable-optimized-integers",
                         "disable optimized integer rep")

  val features = [disableOptimizedRationalsF, disableOptimizedIntegersF]
  val debugs = []
  val controls = []

end;
