(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)

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
