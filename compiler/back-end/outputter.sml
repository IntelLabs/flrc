(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

(* The C/Pillar Outputter *)

signature OUTPUTTER =
sig
  val pass : (Mil.t, unit) Pass.t
end;

structure Outputter :> OUTPUTTER = struct

   val passname = "Outputter"

  fun writeToFile (pd, path, mil) =
      let 
        val fname = Path.toCygwinString path
        val l = MilToPil.program (pd, fname, mil)
        val suffix = ".c"
        val file = fname ^ suffix
        val () = LayoutUtils.writeLayout (l, file)
      in ()
      end
      
  fun outputPil (m, pd, basename) = writeToFile (pd, basename, m)

  val description = {name        = passname,
                     description = "Mil to Pil; output to file",
                     inIr        = MilUtils2.irHelpers,
                     outIr       = Pass.unitHelpers,
                     mustBeAfter = [],
                     stats       = []}

  val associates = {controls = [],
                    debugs = [],
                    features = MilToPil.features,
                    subPasses = []}

  val pass = Pass.mkFilePass (description, associates, outputPil)

end;
