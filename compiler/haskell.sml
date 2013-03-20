(* The Intel Haskell to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature HASKELL =
sig
  val addPasses : Pass.driverInfo -> Pass.driverInfo
  val controls : Config.Control.control list
  val debugs : Config.Debug.debug list
  val features : Config.Feature.feature list
  val exts : (string * (unit, Mil.t * Config.t) Pass.processor) list
  val keeps : StringSet.t
  val stops : StringSet.t
  val langVersions : string list
end;

structure Haskell :> HASKELL =
struct

  val modname = "Driver/Haskell"

  structure Chat = ChatF(type env = Config.t
                         fun extract x = x
                         val name = modname
                         val indent = 0)

  structure FrontEndHs = 
  struct
    val description = {name        = "FrontEndHs",
                       description = "Compile HS (*.hs or *.lhs) to GHC Core (*.hcr)",
                       inIr        = Pass.unitHelpers,
                       outIr       = Pass.unitHelpers,
                       mustBeAfter = [],
                       stats       = []}
    val associates = {controls  = [],
                      debugs    = [],
                      features  = [],
                      subPasses = []}
    fun frontEnd ext ((), pd, path) =
        let
          val config = PassData.getConfig pd
          val fe = Path.fromString "ghc"
          val path = Config.pathToHostString (config, path) ^ "." ^ ext
          val args = ["-D__PPILER__", "--make", "-c", "-fforce-recomp", "-fext-core"] 
                     @ List.rev (Config.ghcOpt config) @ [path]
        in
          Pass.run (config, Chat.log0, fe, args)
        end

    fun pass ext = Pass.mkFilePass (description, associates, frontEnd ext)
  end (* FrontEndHs *)

  val topLevelPassesUU = [FrontEndHs.pass "hs"]
  val topLevelPassesUC = [CoreHsParse.pass]
  val topLevelPassesCC = [CoreHsNormalize.pass]
  val topLevelPassesCA = [CoreHsToANormLazy.pass]
  val topLevelPassesAA = [ANormLazyStrictness.pass]
  val topLevelPassesAS = [ANormLazyToStrict.pass]
  val topLevelPassesSS = [ANormStrictOptimize.pass]
  val topLevelPassesCL = [ANormStrictClosureConvert.pass]
  val topLevelPassesFM = [ANormStrictToMil.pass]
  val topLevelPassesMM = [CoreHsLinkOption.pass]

  fun addPasses x =
      let
        val x = List.fold (topLevelPassesUU, x, Pass.addPassDriverInfo)
        val x = List.fold (topLevelPassesUC, x, Pass.addPassDriverInfo)
        val x = List.fold (topLevelPassesCC, x, Pass.addPassDriverInfo)
        val x = List.fold (topLevelPassesCA, x, Pass.addPassDriverInfo)
        val x = List.fold (topLevelPassesAA, x, Pass.addPassDriverInfo)
        val x = List.fold (topLevelPassesAS, x, Pass.addPassDriverInfo)
        val x = List.fold (topLevelPassesSS, x, Pass.addPassDriverInfo)
        val x = List.fold (topLevelPassesCL, x, Pass.addPassDriverInfo)
        val x = List.fold (topLevelPassesFM, x, Pass.addPassDriverInfo)
        val x = List.fold (topLevelPassesMM, x, Pass.addPassDriverInfo)
      in x
      end

  val controls = CoreHsLayout.controls @ ANormLazyLayout.controls @ 
                 ANormStrictLayout.controls  

  val debugs = ANormStrictLayout.debugs

  val features = []

  local

    val doPass = Pass.doPass
    val stopAt = Pass.stopAt
    val >> = Pass.>>
    infixr >>

  in

  val doGhcCore =
      doPass CoreHsParse.pass >>
      doPass CoreHsLinkOption.pass >>
      stopAt "hsc" >> Pass.first (
      doPass CoreHsNormalize.pass >> 
      doPass CoreHsToANormLazy.pass >> 
      doPass ANormLazyStrictness.pass >>
      doPass ANormLazyToStrict.pass >> 
      doPass ANormStrictOptimize.pass >> 
      doPass ANormStrictClosureConvert.pass >> 
      stopAt "ans" >>
      doPass ANormStrictToMil.pass)

  fun doGhc ext =
      doPass (FrontEndHs.pass ext) >>
      doGhcCore

  end

  val exts =
      [("hcr", doGhcCore),
       ("hs",  doGhc "hs"),
       ("lhs", doGhc "lhs")]

  val keeps = StringSet.fromList ["hcr"]

  val stops = StringSet.fromList ["hsc", "ans"]

  val langVersion = "GHC 7.6.1"

  val langVersions = [langVersion]

end;
