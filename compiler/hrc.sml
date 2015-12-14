(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* This version can compile Haskell *)

local

  val addPasses = Haskell.addPasses
  val controls = Haskell.controls
  val debugs = Haskell.debugs
  val features = Haskell.features
  val exts = Haskell.exts
  val keeps = Haskell.keeps
  val stops = Haskell.stops
  val langVersions = Haskell.langVersions

  structure D = Driver (val addPasses = addPasses
                        val controls = controls
                        val debugs = debugs
                        val features = features
                        val exts = exts
                        val keeps = keeps
                        val stops = stops
                        val langVersions = langVersions)

in

val () = D.main ()

end
