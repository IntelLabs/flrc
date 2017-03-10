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

