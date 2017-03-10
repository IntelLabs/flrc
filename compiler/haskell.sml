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
