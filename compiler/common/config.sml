(* The Intel FL to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* The configuration of the compiler
 * All options for the compiler are recorded in the configuration record
 * and passed down into all parts of the compiler.
 *)

signature CONFIG =
sig
  datatype agcProg = AgcGcMf | AgcTgc | AgcCgc
  datatype gcStyle = GcsNone | GcsConservative | GcsAccurate
  type gcConfig = {registerVtables: bool,
		   reportGlobals: bool,
		   reportRoots: bool,
		   rootsInGlobals: bool,
		   style: gcStyle,
		   tagOnly: bool}
  datatype os = OsCygwin | OsLinux | OsMinGW 
  datatype outputKind = OkC | OkPillar
  datatype parStyle = PNone | PAll | PAuto | PPar
  type passInfo = {enable: bool,
		   showPost: bool,
		   showPre: bool,
		   statPost: bool,
		   statPre: bool}
  type runtimeConfig = {stackWorker    : int option,
                        stackMain      : int option,
                        singleThreaded : bool}
  datatype toolset = TsIpc (* Intel Pillar Compiler *)
                   | TsOpc (* Old Pillar Compiler *)
                   | TsGcc (* Gcc *)
                   | TsIcc (* Intel C compiler *)
  datatype verbosity = VSilent | VQuiet | VInfo | VTop
  datatype wordSize = Ws32 | Ws64
  datatype vectorISA = ViANY            (* unconstrained *)
                     | ViAVX            (* AVX *)
                     | ViEMU            (* emulated *)
                     | ViSSE of int*int (* SSE x.y *)
  (* isa: The target ISA
   * instructions : explicitly enabled/disabled/emulated instructions 
   *  - overrides the isa setting
   * sizes : explicitly enabled/disabled/emulated sizes
   *  - overrides the isa setting
   *)
  datatype vectorConfig = VC of {isa          : vectorISA,
                                 instructions : {disabled : string List.t, 
                                                 emulated : string List.t, 
                                                 enabled  : string List.t},
                                 sizes        : {disabled : string List.t, 
                                                 emulated : string List.t, 
                                                 enabled  : string List.t}
                                }
  datatype t = C of {agc : agcProg,
		     control_ : string StringDict.t,
		     debugLev : verbosity,
		     debug_ : StringSet.t,
		     feature_ : StringSet.t,
		     gc: {registerVtables: bool,
			  reportGlobals: bool,
			  reportRoots: bool,
			  rootsInGlobals: bool,
			  style: gcStyle,
			  tagOnly: bool},
		     ghcOpt : string list,
		     home : Path.t,
                     host : os,
                     iflcLibDirectory : Path.t,
		     iflcOpt : int,
		     keep: StringSet.t,
		     linkStr: string list,
		     linkDirectories: string list,
		     linkLibraries: string list,
		     logLev: verbosity,
		     output: outputKind,
		     parStyle: parStyle,
		     passes: {enable: bool,
			      showPost: bool,
			      showPre: bool,
			      statPost: bool,
			      statPre: bool} StringDict.t,
		     pilcStr: string list,
		     pilDebug: bool,
		     pilOpt: int,
		     report: StringSet.t,
                     runtime : runtimeConfig,
		     sloppyFp: bool,
		     stop: string,
		     targetWordSize: wordSize,
		     timeExecution: string option,
		     toolset: toolset,
                     vectorConfig : vectorConfig,
		     warnLev: verbosity}
  val agc: t -> agcProg
  val debug: bool
  val debugLevel: t * 'a -> int
  val gc: t
	  -> {registerVtables: bool,
	      reportGlobals: bool,
	      reportRoots: bool,
	      rootsInGlobals: bool,
	      style: gcStyle,
	      tagOnly: bool}
  val ghcOpt: t -> string list
  val home : t -> Path.t
  val host : t -> os
  val iflcLibDirectory : t -> Path.t
  val iflcOpt: t -> int
  val keep: t * string -> bool
  val linkStr: t -> string list
  val linkDirectories: t -> string list
  val linkLibraries: t -> string list
  val logLevel: t * 'a -> int
  val output: t -> outputKind
  val parStyle: t -> parStyle
  val passEnabled: t * string -> bool
  val passGet: t * string
	       -> {enable: bool,
		   showPost: bool,
		   showPre: bool,
		   statPost: bool,
		   statPre: bool}
  val passIs: t * string -> bool
  val passShowPost: t * string -> bool
  val passShowPre: t * string -> bool
  val passStatPost: t * string -> bool
  val passStatPre: t * string -> bool
  val pathToHostString : t * Path.t -> string
  val pilDebug: t -> bool
  val pilOpt: t -> int
  val pilcStr: t -> string list
  val reportEnabled: t * string -> bool
  val runtime : t -> runtimeConfig
  val silent: t -> bool
  val sloppyFp: t -> bool
  val stop: t -> string
  val targetWordSize: t -> wordSize
  val targetWordSize': t -> IntArb.size
  val timeExecution: t -> string option
  val toolset: t -> toolset
  val verbose: t -> bool
  val vectorConfig: t -> vectorConfig
  val warnLevel: t * 'a -> int
  structure Control : sig
    type control
    val add: ({check: string -> bool, 
               describe: unit -> Layout.t} StringDict.t
	      * string StringDict.t ref)
	     * string
	     * string
	     -> bool
    val describeControl: ({check: string -> bool, 
                           describe: unit -> Layout.t} StringDict.t
			  * 'a)
			 * string
			 -> Layout.t
    val finalise: 'a * 'b ref -> 'b
    val isControl: ('a StringDict.t * 'b) * string -> bool
    val listControls: 'a StringDict.t * 'b -> string list
    val mk: string
	    * (unit -> Layout.t)
	    * (string -> 'a option)
	    * (t -> 'a)
	    -> control * (t -> 'a)
    val mks: control list
	     -> {check: string -> bool, 
                 describe: unit -> Layout.t} StringDict.t
		* 'a StringDict.t ref
  end
  structure Debug : sig
    type debug
    val add: ('a StringDict.t * StringSet.t ref) * string -> bool
    val finalise: 'a * 'b ref -> 'b
    val mk: string * string -> debug * (t -> bool)
    val mks: debug list
	     -> string StringDict.t * StringSet.t ref
    val usage: string StringDict.t * 'a -> string
  end
  structure Feature : sig
    type feature
    val add: ('a StringDict.t * StringSet.t ref) * string -> bool
    val finalise: 'a * 'b ref -> 'b
    val mk: string * string -> feature * (t -> bool)
    val mks: feature list
	     -> string StringDict.t * StringSet.t ref
    val usage: string StringDict.t * 'a -> string
  end
end

structure Config :> CONFIG =
struct

    (* Static debug or production build of iflc *)
    val debug : bool = true

    datatype outputKind = OkC | OkPillar
    datatype toolset = TsIpc (* Intel Pillar Compiler *)
                     | TsOpc (* Old Pillar Compiler *)
                     | TsGcc (* Gcc *)
                     | TsIcc (* Intel C compiler *)
    datatype parStyle = PNone | PAll | PAuto | PPar

    datatype gcStyle = GcsNone | GcsConservative | GcsAccurate

    datatype os = OsCygwin | OsLinux | OsMinGW

    datatype agcProg = AgcGcMf | AgcTgc | AgcCgc

    (* tagOnly means generate vtables with only tags in them, no size or ref
     * information.
     * registerVtables means call the GC to register vtables.
     * report roots means call the GC to report global roots
     * rootsInGlobals means if reporting roots then include roots in globals
     * reportGlobals means report global objects to the GC
     * Note that not all combinations make sense, user beware!
     *)
    type gcConfig =
         { tagOnly : bool, registerVtables : bool,
           reportRoots : bool, rootsInGlobals: bool,
           reportGlobals : bool, style : gcStyle }

    datatype verbosity = VSilent | VQuiet | VInfo | VTop

    datatype wordSize = Ws32 | Ws64

    datatype vectorISA = ViEMU | ViSSE of int*int | ViAVX | ViANY

    datatype vectorConfig = VC of {isa          : vectorISA,
                                   instructions : {disabled : string List.t, 
                                                   emulated : string List.t, 
                                                   enabled  : string List.t},
                                   sizes        : {disabled : string List.t, 
                                                   emulated : string List.t, 
                                                   enabled  : string List.t}
                                  }

    type passInfo = {
         enable   : bool,
         showPre  : bool,
         statPre  : bool,
         showPost : bool,
         statPost : bool
    }
    (* stackWorker is the default stack size to use for the worker threads
     * stackMain is the default stack size to use for the main thread 
     * singleThreaded true means that no concurrent runtime constructs are allowed 
     *)
    type runtimeConfig = {stackWorker    : int option,
                          stackMain      : int option,
                          singleThreaded : bool}

    datatype t = C of {
         agc              : agcProg,
         control_         : string StringDict.t,
         debug_           : StringSet.t,
         debugLev         : verbosity,
         feature_         : StringSet.t,
         gc               : gcConfig,
         ghcOpt           : string list,
         home             : Path.t,
         host             : os,
         iflcLibDirectory : Path.t,
         iflcOpt          : int,
         keep             : StringSet.t,
         linkStr          : string list,
         linkDirectories  : string list,
         linkLibraries    : string list,
         logLev           : verbosity,
         output           : outputKind,
         parStyle         : parStyle,
         passes           : passInfo StringDict.t,
         pilcStr          : string list,
         pilDebug         : bool,
         pilOpt           : int,
         report           : StringSet.t,
         runtime          : runtimeConfig,
         stop             : string,
         sloppyFp         : bool,
         targetWordSize   : wordSize,
         timeExecution    : string option,
         toolset          : toolset,
         vectorConfig     : vectorConfig,
         warnLev          : verbosity
    }

    (*** Straight Getters ***)

    fun get (C config, p) = p config

    fun agc c                         = get (c, #agc)
    fun gc c                          = get (c, #gc)
    fun ghcOpt c                      = get (c, #ghcOpt)
    fun home c                        = get (c, #home)
    fun host c                        = get (c, #host)
    fun iflcLibDirectory c            = get (c, #iflcLibDirectory)
    fun iflcOpt c                     = get (c, #iflcOpt)
    fun linkStr c                     = get (c, #linkStr)
    fun linkDirectories c             = get (c, #linkDirectories)
    fun linkLibraries c               = get (c, #linkLibraries)
    fun output c                      = get (c, #output)
    fun parStyle c                    = get (c, #parStyle)
    fun pilcStr c                     = get (c, #pilcStr)
    fun pilDebug c                    = get (c, #pilDebug)
    fun pilOpt c                      = get (c, #pilOpt)
    fun runtime c                     = get (c, #runtime)
    fun stop c                        = get (c, #stop)
    fun sloppyFp c                    = get (c, #sloppyFp)
    fun targetWordSize c              = get (c, #targetWordSize)
    fun timeExecution c               = get (c, #timeExecution)
    fun toolset c                     = get (c, #toolset)
    fun vectorConfig c                = get (c, #vectorConfig)

    (*** Derived Getters ***)

   (* These are for communicating debug information.  Debug printing is
    * disabled if Config.debug is false.
    * Debug levels:
    * 0 => High level debug information.  This should not generate an
    *   excessive amount of output, even for large programs.  Examples might
    *   include printing out analysis summaries for optimization passes.
    * 1 => Moderately detailed debug information.  This may generate excessive
    *   amounts of output, but should be practical for medium sized programs.  
    *   Examples might include printing out per-procedure information,
    *   including during iteration.
    * 2 => Extremely detailed debug information.  This may be impractical for
    *   anything but small programs.  Examples include printing out
    *  per-instruction information while iterating over the IR.
    *)
    fun debugLevel (C cfg, name) =
        case #debugLev cfg
         of VSilent => ~1
          | VQuiet => 0
          | VInfo => 1
          | VTop => 2

    fun keep (C c, s) = StringSet.member (#keep c, s)

   (* These are for communicating with the user.
    * Log levels:
    * 0 => Toplevel pass information
    * 1 => Subpass information
    * 2 => Pass part information
    * 3 => Summary information about what the pass is doing
    * 4 => Detailed information about what the pass is doing
    *)
    fun logLevel (C cfg, name) =
        case #logLev cfg
         of VSilent => ~1
          | VQuiet => 0
          | VInfo => 1
          | VTop => 10

    fun verbose (C cfg) = (#logLev cfg) = VTop
    fun silent (C cfg) = (#logLev cfg) = VSilent

    fun passIs (C cfg, name) =
        Option.isSome (StringDict.lookup (#passes cfg, name))

    fun passGet (C cfg, name) = 
        case StringDict.lookup (#passes cfg, name)
         of SOME i => i
          | NONE   =>
            Fail.fail ("Config", "passGet", "unknown pass " ^ name)

    fun passEnabled (cfg, name) = #enable (passGet (cfg, name))
    fun passShowPre (cfg, name)  = #showPre (passGet (cfg, name))
    fun passStatPre (cfg, name)  = #statPre (passGet (cfg, name))
    fun passShowPost (cfg, name)  = #showPost (passGet (cfg, name))
    fun passStatPost (cfg, name)  = #statPost (passGet (cfg, name))

    fun pathToHostString (cfg, path) = 
        (case host cfg
          of OsCygwin => Path.toWindowsString path
           | OsLinux  => Path.toUnixString path
           | OsMinGW  => Path.toWindowsString path)

    fun reportEnabled (C cfg, name) = StringSet.member (#report cfg, name)

    fun targetWordSize' config =
        case targetWordSize config
         of Ws32 => IntArb.S32
          | Ws64 => IntArb.S64

   (* These are for controlling internal compiler warnings.  Warnings are used
    * for unexpected conditions which do not prevent correct compilation. 
    * Warn levels:
    * 0 => Very unexpected condition.  Probably a performance bug, and/or may 
    *   reflect a bug elsewhere in the compiler.  Correct compilation is still 
    *   possible, but the issue should be addressed ASAP.
    * 
    * 1 => Unexpected condition.  Impact on performance minor, and probably not
    *   indicative of a bug elsewhere.  Correct compilation and most
    *   optimization is still possible, but the issue should be investigated
    *   at some point.
    *
    * 2 => Minor observations.  No impact on performance. Something is
    *   unexpected about the IR, but neither optimization nor correctness is
    *   affected.  At some point, it may be desirable to understand why the
    *   warning is being generated and eliminate it.
    * 
    *)
    fun warnLevel (C cfg, name) =
        case #warnLev cfg
         of VSilent => ~1
          | VQuiet => 0
          | VInfo => 1
          | VTop => 2

    (*** Compiler debugging options ***)

    structure Debug =
    struct

      datatype debug = D of string * string

      fun mk (name, description) =
          if debug then 
            let
              fun enabled c = StringSet.member (get (c, #debug_), name)
              val d = D (name, description)
            in (d, enabled)
            end
          else
            let
              fun enabled c = false
              val d = D (name, description)
            in (d, enabled)
            end

      fun mks l =
          if debug then
            let
              fun doOne (D (n, d)) = (n, d)
              val debugs = StringDict.fromList (List.map (l, doOne))
            in (debugs, ref StringSet.empty)
            end
          else
            let
              val debugs = StringDict.fromList []
            in (debugs, ref StringSet.empty)
            end

      fun add ((debugs, ds), d) =
          if debug then
            if StringDict.contains (debugs, d) then
              let
                val () = ds := StringSet.insert (!ds, d)
              in true
              end
            else
              false
          else
            false

      fun usage (debugs, _) =
          if debug then
            let
              val l = StringDict.toList debugs
              fun doOne (n, d) = "  " ^ n ^ ": " ^ d ^ "\n"
              val ss = List.map (l, doOne)
              val s = String.concat ss
            in s
            end
          else
            "Debug flags not available in release mode"

      fun finalise (_, ds) = !ds

    end

    (*** Compiler features ***)

    structure Feature =
    struct

      datatype feature = F of string * string

      fun mk (name, description) =
          let
            fun enabled c = StringSet.member (get (c, #feature_), name)
            val f = F (name, description)
          in (f, enabled)
          end

      fun mks l =
          let
            fun doOne (F (n, d)) = (n, d)
            val features = StringDict.fromList (List.map (l, doOne))
          in (features, ref StringSet.empty)
          end

      fun add ((features, fs), f) =
          if StringDict.contains (features, f) then
            let
              val () = fs := StringSet.insert (!fs, f)
            in true
            end
          else
            false

      fun usage (features, _) =
          let
            val l = StringDict.toList features
            fun doOne (n, d) = "  " ^ n ^ ": " ^ d ^ "\n"
            val ss = List.map (l, doOne)
            val s = String.concat ss
          in s
          end

      fun finalise (_, fs) = !fs

    end

    (*** Pass/module control ***)

    structure Control =
    struct

      datatype control = C of {
               name     : string,
               describe : unit -> Layout.t,
               check    : string -> bool
      }

      fun mk (n, d, parse, dft) =
          let
            fun get_ c =
                case StringDict.lookup (get (c, #control_), n)
                 of NONE => dft c
                  | SOME x => Option.valOf (parse x)
            fun check s = Option.isSome (parse s)
            val c = C {name = n, describe = d, check = check}
          in (c, get_)
          end

      type controls =
           {describe : unit -> Layout.t, 
            check : string -> bool} StringDict.t

      fun mks l =
          let
            fun doOne (C {name, describe, check}) =
                (name, {describe = describe, check = check})
            val controls = StringDict.fromList (List.map (l, doOne))
          in (controls : controls, ref StringDict.empty)
          end

      fun isControl ((controls, _), c) = StringDict.contains (controls, c)

      fun add ((controls : controls, cs), c, s) =
          if #check (Option.valOf (StringDict.lookup (controls, c))) s then
            let
              val () = cs := StringDict.insert (!cs, c, s)
            in true
            end
          else
            false

      fun describeControl ((controls : controls, _), c) =
          #describe (Option.valOf (StringDict.lookup (controls, c))) ()

      fun listControls (controls, _) =
          List.map (StringDict.toList controls, #1)

      fun finalise (_, cs) = !cs
          
    end

end;
