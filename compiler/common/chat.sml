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


(* Talking to the user *)

signature CHAT = sig

  type env
  type level = int

  val error : env * string -> unit

  (* See config.sml for documentation of the warn, log, and debug levels. *)

  val warn : env * level * string -> unit
  val log  : env * level * string -> unit

  val warn0 : env * string -> unit
  val warn1 : env * string -> unit
  val warn2 : env * string -> unit

  val log0  : env * string -> unit
  val log1  : env * string -> unit
  val log2  : env * string -> unit
  val log3  : env * string -> unit

end

functor ChatF(type env
              val extract : env -> Config.t
              val name : string
              val indent : int)
  :> CHAT where type env = env =
struct

    type env = env
    type level = int

    fun error (env, s) =
        Out.output (Out.error, "flrc: error: " ^ s ^ "\n")

    fun say (cfg, s) =
        let
          val i = StringCvt.padLeft #" " indent ""
        in
          print (i^s)
        end

    fun warn (cfg, level, msg) =
        let val cfg = extract cfg
            val warnLevel = Config.warnLevel (cfg, name)
        in
            if level <= warnLevel then
                say (cfg, "warning: " ^ name ^ ": " ^ msg ^ "\n")
            else
                ()
        end

    fun log (cfg, level, msg) =
        let val cfg = extract cfg
            val logLevel = Config.logLevel (cfg, name)
        in
            if level <= logLevel then
                say (cfg, name ^ ": " ^ msg ^ "\n")
            else
                ()
        end

    fun warn0 (cfg, msg) = warn (cfg, 0, msg)
    fun warn1 (cfg, msg) = warn (cfg, 1, msg)
    fun warn2 (cfg, msg) = warn (cfg, 2, msg)

    fun log0 (cfg, msg) = log (cfg, 0, msg)
    fun log1 (cfg, msg) = log (cfg, 1, msg)
    fun log2 (cfg, msg) = log (cfg, 2, msg)
    fun log3 (cfg, msg) = log (cfg, 3, msg)

end;
