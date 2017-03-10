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

(* The C/Pillar Outputter *)

signature OUTPUTTER =
sig
  val pass : (Mil.t, unit) Pass.t
end;

structure Outputter :> OUTPUTTER = struct

   val passname = "Outputter"

  fun writeToFile (pd, path, mil) =
      let 
        val fname = Config.pathToHostString (PassData.getConfig pd, path)
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
