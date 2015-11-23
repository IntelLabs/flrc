(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)
signature BOTH_MIL =
sig
  datatype t = Mil of Mil.t | IMil of IMil.t
  val toMil       : PassData.t * t -> Mil.t
  val toIMil      : PassData.t * t -> IMil.t
  val layout      : t * Config.t -> Layout.t
  val stater      : t * Config.t -> Layout.t
  val irHelpers   : t Pass.irHelpers
  val mkMilPass   : (Mil.t * PassData.t -> Mil.t) -> (t * PassData.t -> t)
  val mkIMilPass' : (IMil.t * PassData.t -> IMil.t) -> (t * PassData.t -> t)
  val mkIMilPass  : (IMil.t * PassData.t -> unit) -> (t * PassData.t -> t)
  val out         : t -> Mil.t
end



structure BothMil :> BOTH_MIL =
struct
  datatype t = Mil of Mil.t | IMil of IMil.t
  fun out p =
      case p
       of Mil p => p
        | IMil p => IMil.T.unBuild p
  fun toMil (pd, p) = out p
  fun toIMil (pd, p) =
      case p
       of Mil p => IMil.T.build (PassData.getConfig pd, p)
        | IMil p => p
  fun layout (p, c) =
      case p
       of Mil p => MilExtendedLayout.layout (c, p)
        | IMil p => MilExtendedLayout.layout (c, IMil.T.unBuild p)
  fun stater (p, c) =
      case p
       of Mil p => MilStats.layout (MilStats.O {id = NONE}) (p, c)
        | IMil p => MilStats.layout (MilStats.O {id = NONE}) (IMil.T.unBuild p, c)
  val irHelpers = {printer = layout, stater = stater}
  fun mkMilPass f = fn (p, pd) => Mil (f (toMil (pd, p), pd))
  fun mkIMilPass' f = fn (p, pd) => IMil (f (toIMil (pd, p), pd))
  fun mkIMilPass f = 
      mkIMilPass' 
        (fn (p, pd) => 
            let
              val () = f (p, pd)
            in p
            end)
end

