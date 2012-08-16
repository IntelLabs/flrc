(* The Intel FL to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* This file contains the top level driver for the compiler *)

signature DRIVER =
sig
  val main : unit -> unit
end;

functor Driver
  (val addPasses : Pass.driverInfo -> Pass.driverInfo
   val controls : Config.Control.control list
   val debugs : Config.Debug.debug list
   val features : Config.Feature.feature list
   val exts : (string * (unit, Mil.t * string Identifier.VariableDict.t option) Pass.processor) list
   val keeps : StringSet.t
   val stops : StringSet.t
   val langVersions : string list)
  :> DRIVER =
struct

  val modname = "Driver"

  structure Chat = ChatF(type env = Config.t
                         fun extract x = x
                         val name = modname
                         val indent = 0)

  structure L = Layout
  structure SD = StringDict
  structure SS = StringSet

  structure Passes =
  struct

    structure Link = BackEnd.Link
    structure PilCompile = BackEnd.PilCompile

    val topLevelPassesUU = [PilCompile.pass, Link.pass]
    val topLevelPassesUM = [MilParse.pass]
    val topLevelPassesBB = [MilCompile.pass]
    val topLevelPassesMU = [Outputter.pass]

    val nonPassDebugs =
        Globals.debugs @ IMil.debugs @ MilLayout.debugs @ MilExtendedLayout.debugs @ debugs

    val nonPassFeatures = Globals.features @ features
    
    val nonPassControls =
        Globals.controls @ MilLayout.controls @ MilExtendedLayout.controls @ IMil.controls @ controls

    val (controls, debugs, features, stats, passMap) =
        let
          val x = (nonPassControls, nonPassDebugs, nonPassFeatures, [], SD.empty)
          val x = List.fold (topLevelPassesUU, x, Pass.addPassDriverInfo)
          val x = List.fold (topLevelPassesUM, x, Pass.addPassDriverInfo)
          val x = List.fold (topLevelPassesBB, x, Pass.addPassDriverInfo)
          val x = List.fold (topLevelPassesMU, x, Pass.addPassDriverInfo)
          val x = addPasses x
        in x
        end

    val keeps = List.fold (["pil", "obj"], keeps, SS.insert o Utils.flip2)
    val stops = List.fold (["m", "pil", "obj", "exe"], stops, SS.insert o Utils.flip2)

  end (* Passes *)

  structure Compilation =
  struct

    local

      val doPass = Pass.doPass
      fun doB p = Pass.doPassWrap (p, BothMil.Mil, BothMil.out)
      val stopAt = Pass.stopAt
      val first = Pass.first
      val >> = Pass.>>
      val >>> = Pass.>>>
      infixr >>
      infixr >>>
      val ifC = Pass.ifC
      open Passes

    in

    val doObj =
        doPass Link.pass
    val doPil =
        doPass PilCompile.pass >>
        stopAt "obj" >>
        doObj
    val doMil =
        first (doB MilCompile.pass) >>
        stopAt "m" >>
        doPass CoreHsLinkOption.pass >>>
        doPass Outputter.pass >>
        stopAt "pil" >>
        doPil
    val doMilF =
        doPass MilParse.pass >>
        doMil

    val exts =
        [("c", doPil),
         ("mil", doMilF),
         ("pil", doPil),
         ("o", doObj),
         ("obj", doObj)] @
        (List.map (exts, fn (ext, p) => (ext, p >> doMil)))

    end
         
    fun compileWP (config : Config.t, fname : string) : unit =
        case OS.Path.splitBaseExt fname
         of {ext = NONE, ...} =>
            Chat.error (config, fname ^ " - unknown file type")
          | {base, ext = SOME ext} =>
            let
              val pd = PassData.mk (config, Passes.stats)
              val doFile = 
                  case List.peek (exts, fn (e, _) => e = ext)
                   of NONE => (fn _ => Chat.error (config, fname ^ " - unknown file type"))
                    | SOME (_, p) => Pass.apply p
              val path = Path.fromString base
              val () = Pass.startFile (config, fname)
              val () = ((doFile (pd, path, ()))
                        handle Pass.Done => ())
              val () = Pass.endFile (config, fname)
            in ()
            end

  end (* Compilation *)

  structure PassData =
  struct

    datatype t = PD of {
      description : string,
      optional : bool,
      enabled : bool ref,
      showPre : bool ref,
      statPre : bool ref,
      showPost : bool ref,
      statPost : bool ref
    }

    fun mk (name, {description, optional}) =
        PD {description = description, optional = optional, enabled = ref true,
            showPre = ref false, statPre = ref false, showPost = ref false,
            statPost = ref false}

    fun getDescription (PD {description, ...}) = description
    fun isOptional (PD {optional, ...}) = optional
    fun setEnabled (PD {enabled, ...}, e) = enabled := e
    fun setShowPre (PD {showPre, ...}, s) = showPre := s
    fun setStatPre (PD {statPre, ...}, s) = statPre := s
    fun setShowPost (PD {showPost, ...}, s) = showPost := s
    fun setStatPost (PD {statPost, ...}, s) = statPost := s

    fun out (PD {enabled, showPre, statPre, showPost, statPost, ...}) =
        {enable = !enabled, showPre = !showPre, statPre = !statPre,
         showPost = !showPost, statPost = !statPost}

  end (* PassData *)

  fun printVersion () =
      let
        val version =
            "Intel Functional Language Compiler " ^ Version.iflcVersion ^ (if Config.debug then " (DEBUG)" else "")
        val () = Out.outputl (Out.standard, version)
        val () = List.foreach (langVersions, fn s => Out.outputl (Out.standard, s))
        val () = Out.outputl (Out.standard, "Build " ^ Version.build)
      in ()
      end

  fun printVersionAndExit () =
      let
        val () = printVersion ()
        val () = OS.Process.exit OS.Process.success
      in ()
      end

  fun parseCommandLine () =
      let
        val initLibDir =
            case Process.getEnv "IFLCLIB"
             of SOME d => d
              | _ => OS.Path.getParent (OS.Path.dir (CommandLine.name ()))
        val initLibDir = Path.fromString (OS.Path.mkCanonical initLibDir)
        val initHomeDir =
            case Process.getEnv "IFLCHOME"
             of SOME d => Path.fromString (OS.Path.mkCanonical d)
              | _ => initLibDir

        val agc         = ref Config.AgcTgc
        val expert      = ref false
        val futures     = ref Config.PNone
        val gcs         = ref NONE
        val gcTagOnly   = ref true
        val host        = ref (case MLton.Platform.OS.host
                                of MLton.Platform.OS.Cygwin => Config.OsCygwin
                                 | MLton.Platform.OS.Linux => Config.OsLinux
                                 | MLton.Platform.OS.MinGW => Config.OsMinGW
			         | p => raise Fail ("Unsupported host: " ^ MLton.Platform.OS.toString p))
        val libDir      = ref initLibDir
        val keeps       = Passes.keeps
        val keep        = ref SS.empty
        val output      = ref NONE
        val timeExe     = ref NONE
        val single      = ref NONE
        val sloppyFp    = ref false
        val stackMain   = ref NONE
        val stackWorker = ref NONE
        val stops       = Passes.stops
        val stop        = ref "exe"
        val toolset     = ref Config.TsIpc
        val logLev      = ref Config.VSilent
        val warnLev     = ref Config.VSilent
        val debugLev    = ref Config.VSilent
        val ws          = ref (if MLton.Pointer.sizeofPointer = Word.fromInt 4 then Config.Ws32 else Config.Ws64)
        val va          = ref Config.ViEMU
        val vInstrs     = ref {disabled = [], emulated = [], enabled = []}
        val vSizes      = ref {disabled = [], emulated = [], enabled = []}

        val passes = SD.map (Passes.passMap, PassData.mk)
        val passesString =
            let
              fun doOne (n, pd) =
                  "  " ^ n ^ ": " ^ PassData.getDescription pd ^ "\n"
              val s = String.concat (List.map (SD.toList passes, doOne))
            in s
            end
        fun invalidPass name = "invalid pass arg: " ^ name ^ "\ntry one of:\n" ^ passesString
        fun liftPassF (usage, name, f) =
            case SD.lookup (passes, name)
             of SOME pd => f pd
              | NONE => usage (invalidPass name)
        fun setEnabled (usage, name, b) = 
            liftPassF (usage, name,
                    fn pd =>
                       if PassData.isOptional pd orelse b then
                         PassData.setEnabled (pd, b)
                       else
                         usage ("pass not disableable: " ^ name))
        fun setShowPre (usage, name, b) = 
            liftPassF (usage, name, fn pd => PassData.setShowPre (pd, b))
        fun setStatPre (usage, name, b) = 
            liftPassF (usage, name, fn pd => PassData.setStatPre (pd, b))
        fun setShowPost (usage, name, b) = 
            liftPassF (usage, name, fn pd => PassData.setShowPost (pd, b))
        fun setStatPost (usage, name, b) = 
            liftPassF (usage, name, fn pd => PassData.setStatPost (pd, b))

        fun rest (s, i) = String.substring (s, i, String.size s - i)

        fun doPrint (usage, s) =
            case String.sub (s, 0)
             of #"-" =>
                if String.sub (s, 1) = #"+" then
                  let
                    val name = rest (s, 2)
                    val () = setShowPre (usage, name, true)
                    val () = setShowPost (usage, name, true)
                  in ()
                  end
                else
                  setShowPre (usage, rest (s, 1), true)
              | #"+" =>
                if String.sub (s, 1) = #"-" then
                  let
                    val name = rest (s, 2)
                    val () = setShowPre (usage, name, true)
                    val () = setShowPost (usage, name, true)
                  in ()
                  end
                else
                  setShowPost (usage, rest (s, 1), true)
              | #"*" =>
                let
                  fun doOne (_, pd) = PassData.setShowPost (pd, true)
                  val () = SD.foreach (passes, doOne)
                in ()
                end
              | _ => if (s = "all") then
                       let
                         fun doOne (_, pd) = PassData.setShowPost (pd, true)
                         val () = SD.foreach (passes, doOne)
                       in ()
                       end
                     else 
                       setShowPost (usage, s, true)

        fun doStatIR (usage, s) =
            case String.sub (s, 0)
             of #"-" =>
                if String.sub (s, 1) = #"+" then
                  let
                    val name = rest (s, 2)
                    val () = setStatPre (usage, name, true)
                    val () = setStatPost (usage, name, true)
                  in ()
                  end
                else
                  setStatPre (usage, rest (s, 1), true)
              | #"+" =>
                if String.sub (s, 1) = #"-" then
                  let
                    val name = rest (s, 2)
                    val () = setStatPre (usage, name, true)
                    val () = setStatPost (usage, name, true)
                  in ()
                  end
                else
                  setStatPost (usage, rest (s, 1), true)
              | #"*" =>
                let
                  fun doOne (_, pd) = PassData.setStatPost (pd, true)
                  val () = SD.foreach (passes, doOne)
                in ()
                end
              | _ => setStatPost (usage, s, true)
                     
        fun doVectorArg (usage, r, s) =
            case String.sub (s, 0)
             of #"-" =>
                let
                  val name = rest (s, 2)
                  val {disabled, emulated, enabled} = !r
                  val () = r := {disabled = name :: disabled, 
                                 emulated = emulated, 
                                 enabled  = enabled}
                in ()
                end
              | #"+" =>
                let
                  val name = rest (s, 2)
                  val {disabled, emulated, enabled} = !r
                  val () = r := {disabled = disabled, 
                                 emulated = emulated, 
                                 enabled  = name :: enabled}
                in ()
                end
              | #"=" =>
                let
                  val name = rest (s, 2)
                  val {disabled, emulated, enabled} = !r
                  val () = r := {disabled = disabled, 
                                 emulated = name :: emulated, 
                                 enabled  = enabled}
                in ()
                end
              | _ => usage ()

        val debugs = Config.Debug.mks Passes.debugs
        fun addDebug (usage, s) =
            if s = "describe" then
              print ("Compiler debugs:\n" ^ Config.Debug.usage debugs)
            else if Config.Debug.add (debugs, s) then
              ()
            else
              usage ("bad debug: " ^ s ^ "\n" ^ Config.Debug.usage debugs)

        val features = Config.Feature.mks Passes.features
        fun addFeature (usage, s) =
            if s = "describe" then
              print ("Compiler features:\n" ^ Config.Feature.usage features)
            else if Config.Feature.add (features, s) then
              ()
            else
              usage ("bad feature: " ^ s ^ "\n" ^ Config.Feature.usage features)

        val controls = Config.Control.mks Passes.controls
        fun listControls () =
            let
              val cs = Config.Control.listControls controls
              val ls = List.map (cs, Layout.str)
              val l =
                  Layout.align
                    [Layout.str "Compiler controls:",
                     LayoutUtils.indent (Layout.sequence ("", "", ",") ls)]
              val s = LayoutUtils.toString l ^ "\n"
            in s
            end
        fun addControl (usage, c, s) =
            if c = "list" then
              print (listControls ())
            else if s = "describe" then
              if Config.Control.isControl (controls, c) then
                LayoutUtils.printLayout
                  (Config.Control.describeControl (controls, c))
              else
                usage ("bad control: " ^ c ^ "\n" ^ listControls ())
            else
              if Config.Control.isControl (controls, c) then
                if Config.Control.add (controls, c, s) then
                  ()
                else
                  let
                    val s1 = "bad control string for " ^ c ^ ": " ^ s ^ ".\n"
                    val l = Config.Control.describeControl (controls, c)
                    val s2 = LayoutUtils.toString l
                  in usage (s1 ^ s2)
                  end
              else
                usage ("bad control: " ^ c ^ "\n" ^ listControls ())

        val report = ref SS.empty
        fun addReport s = 
            let
              val add = fn s => report := SS.insert (!report, s)
            in
              if s = "*" then 
                List.foreach (SD.toList passes, fn (s, _) => add s)
              else
                add s
            end

        val pilDebug = ref false
        val pilOpt = ref 3
        val iflcOpt = ref 3
        val ghcOpt = ref []
        val pilcStr = ref []
        val linkStr = ref []

        fun makeOptions {usage=Usage} = 
            let
              fun usage s = (ignore (Usage s); raise Fail "unreachable")
              fun mkOne (s, n, a, d, opt) =
                  {arg = a, desc = d, name = n, opt = opt, style = s}
              val opts =
                  (* Keep these in case-insensitive alphabetic order *)
                  [

                   (Popt.Normal, "agc", " {mf|tgc|cgc}",
                    "specify collector to use (default tgc)",
                    Popt.SpaceString
                      (fn s =>
                          agc :=
                          (case s
                            of "mf" => Config.AgcGcMf
                             | "tgc" => Config.AgcTgc
                             | "cgc" => Config.AgcCgc
                             | _ => usage ("invalid -agc arg: " ^s)))),

                   (Popt.Normal, "control", " pass {string|describe}",
                    "set control string for pass, or describe",
                    Popt.SpaceString2
                      (fn (s1, s2) => addControl (usage, s1, s2))),
                             
                   (Popt.Normal, "D", "",
                    "generate debug info/checks",
                    Popt.trueRef pilDebug),

                   (Popt.Expert, "debug", " string",
                    "debug compiler (describe to list)",
                    Popt.SpaceString (fn s => addDebug (usage, s))),

                   (Popt.Normal, "debugLevel", " {0|1|2}", 
                    "compiler debugging levels to print",
                    Popt.SpaceString
                      (fn s =>
                          debugLev :=
                          (case s of
                             "0" => Config.VQuiet
                           | "1" => Config.VInfo
                           | "2" => Config.VTop
                           | _ => usage ("invalid -debugLevel arg: " ^ s)))),

                   (Popt.Normal, "disable", " pass", "disable specified pass",
                    Popt.SpaceString (fn s => setEnabled (usage, s, false))),

                   (Popt.Normal, "enable", " pass", "enable specified pass",
                    Popt.SpaceString (fn s => setEnabled (usage, s, true))),

                   (Popt.Normal, "expert", "",
                    "enable expert status", Popt.trueRef expert),

                   (Popt.Expert, "F", " string",
                    "activate feature (describe to list)",
                    Popt.SpaceString (fn s => addFeature (usage, s))),

                   (Popt.Expert, "fsloppy", "",
                    "allow sloppy FP optimisation",
                    Popt.trueRef sloppyFp),

                   (Popt.Normal, "futures", " {all|auto|par}",
                    "parallelize with futures",
                    Popt.SpaceString
                      (fn s => case s
                                of "all"  => futures := Config.PAll
                                 | "auto" => futures := Config.PAuto
                                 | "par"  => futures := Config.PPar
                                 | _ => usage ("invalid -futures arg: " ^ s))),

                   (Popt.Normal, "gc", " {none|conservative|accurate}",
                    "type of GC used",
                    Popt.SpaceString
                      (fn s => case s
                                of "none" => gcs := SOME Config.GcsNone
                                 | "conservative" =>
                                   gcs := SOME Config.GcsConservative
                                 | "accurate" => gcs := SOME Config.GcsAccurate
                                 | _ => usage ("invalid -gc arg: " ^ s))),

                   (Popt.Expert, "gcFullVTables", "", "generate full vtable",
                    Popt.falseRef gcTagOnly),

                   (Popt.Normal, "ghcO", " string",
                    "pass string to GHC",
                    Popt.SpaceString (fn s => ghcOpt := s :: !ghcOpt)),

                   (Popt.Expert, "host", " {cygwin|linux|mingw}",
                    "host os",
                    Popt.SpaceString
                      (fn s => case s
                                of "cygwin" => host := Config.OsCygwin
                                 | "linux"  => host := Config.OsLinux
                                 | "mingw"  => host := Config.OsMinGW
                                 | _ => usage ("invalid -host arg: " ^ s))),

                   (Popt.Normal, "iflcLib", " directory", "use alternate iflc library",
                    Popt.SpaceString (fn s => libDir := Path.fromString (OS.Path.mkCanonical s))),

                   (Popt.Normal, "iflcO", " {0|1|2|3}",
                    "set iflc optimization level",
                    Popt.Int
                      (fn i =>
                          if i <= 3 then 
                            (iflcOpt := i)
                          else
                            usage ("invalid -iflcO arg: " ^ (Int.toString i)))),

                   (Popt.Normal, "keep",
                    let
                      val opts = SS.fold (keeps, "", fn (s, opts) => opts ^ s ^ "|")
                      val opts = " {" ^ String.dropLast opts ^ "}"
                    in opts
                    end,
                    "keep generated files",
                    Popt.SpaceString
                      (fn s =>
                          if SS.member (keeps, s)
                          then keep := SS.insert (!keep, s)
                          else usage ("invalid -keep arg: " ^ s))),

                   (Popt.Normal, "link", " string",
                    "pass string to linker",
                    Popt.SpaceString (fn s => linkStr := s :: !linkStr)),

                   (Popt.Normal, "list", "",
                    "list passes (see -print,-enable)",
                    Popt.None (fn () => print passesString)),
                   
                   (Popt.Normal, "O", " {0|1|2|3}", 
                    "set overall optimization level",
                    Popt.Int
                      (fn i =>
                          if i <= 3 then 
                            (pilOpt := i; iflcOpt := i)
                          else
                            usage ("invalid -O arg: " ^ (Int.toString i)))),

                   (Popt.Normal, "output", " {c|pillar}",
                    "generate C or Pillar",
                    Popt.SpaceString
                      (fn s =>
                          output :=
                          (case s of
                             "c"      => SOME Config.OkC
                           | "pillar" => SOME Config.OkPillar
                           | _ => usage ("invalid -output arg: " ^ s)))),

                   (Popt.Normal, "pilc", " string",
                    "pass string to pil compiler",
                    Popt.SpaceString (fn s => pilcStr := s :: !pilcStr)),

                   (Popt.Normal, "pilO", " {0|1|2|3}",
                    "set pil optimization level",
                    Popt.Int
                      (fn i =>
                          if i <= 3 then 
                            (pilOpt := i)
                          else
                            usage ("invalid -pilO arg: " ^ (Int.toString i)))),

                   (Popt.Normal, "print", " pass", "print specified pass",
                    Popt.SpaceString (fn s => doPrint (usage, s))),

                   (Popt.Expert, "st", " {false|true}",
                    "use the single threaded runtime",
                    Popt.Bool (fn b => single := SOME b)),

                   (Popt.Normal, "stack", " <i>",
                    "set main stack reserve size to i bytes",
                    Popt.Int
                      (fn i => stackMain := SOME i)),

                   (Popt.Normal, "stackWorker", " <i>",
                    "set worker stack reserve size to i bytes",
                    Popt.Int
                      (fn i => stackWorker := SOME i)),

                   (Popt.Expert, "statIR", " pass", "stat specified pass",
                    Popt.SpaceString (fn s => doStatIR (usage, s))),

                   (Popt.Expert, "statPass", " string",
                    "report pass stats: * for all passes",
                    Popt.SpaceString addReport),

                   (Popt.Normal, "stop",
                    let
                      val opts = SS.fold (stops, "", fn (s, opts) => opts ^ s ^ "|")
                      val opts = " {" ^ String.dropLast opts ^ "}"
                    in opts
                    end,
                    "specify where to stop in compilation",
                    Popt.SpaceString
                      (fn s =>
                          if SS.member (stops, s)
                          then stop := s
                          else usage ("invalid -stop arg: " ^ s))),

                   (Popt.Normal, "timeExecution", " string",
                    "time the program",
                    Popt.SpaceString (fn s => timeExe := SOME s)),

                   (Popt.Normal, "toolset", " {ipc|opc|gcc|icc}",
                    "choose assembler/linker toolset",
                    Popt.SpaceString
                      (fn s =>
                          toolset :=
                          (case s of
                             "ipc" => Config.TsIpc
                           | "opc" => Config.TsOpc
                           | "gcc" => Config.TsGcc
                           | "icc" => Config.TsIcc
                           | _ => usage ("invalid -toolset arg: " ^ s)))),

                   (Popt.Normal, "Varch", " {ViANY|ViAVX|ViEMU|ViSSE}",
                    "target vector ISA arch",
                    Popt.SpaceString 
                      (fn s => 
                          va :=
                          (case s 
                            of "ViANY" => Config.ViANY
                             | "ViAVX" => Config.ViAVX
                             | "ViEMU" => Config.ViEMU
                             | "ViSSE" => Config.ViSSE (4, 2)
                             | _ => 
                               usage ("invalid -va args: " ^ s)))),

                   (Popt.Normal, "verbose", " {0|1|2}", "how verbose to be",
                    Popt.SpaceString
                      (fn s =>
                          logLev :=
                          (case s of
                             "0" => Config.VQuiet
                           | "1" => Config.VInfo
                           | "2" => Config.VTop
                           | _ => usage ("invalid -verbose arg: " ^ s)))),
                   
                   (Popt.Normal, "version", "", "print version and exit",
                    Popt.None printVersionAndExit),

                   (Popt.Expert, "Vinstr", " {-,=,+}instr",
                    "vector ISA instr override",
                    Popt.SpaceString 
                    (fn s => doVectorArg (fn () => usage ("Bad -Vinstr arg"), vInstrs, s))),

                   (Popt.Expert, "Vsize", " {-,=,+}sz",
                    "vector ISA size override",
                    Popt.SpaceString 
                    (fn s => doVectorArg (fn () => usage ("Bad -Vsize arg"), vSizes, s))),

                   (Popt.Normal, "warnLevel", " {0|1|2}", 
                    "compiler internal warn levels to print",
                    Popt.SpaceString
                      (fn s =>
                          warnLev :=
                          (case s of
                             "0" => Config.VQuiet
                           | "1" => Config.VInfo
                           | "2" => Config.VTop
                           | _ => usage ("invalid -warnLevel arg: " ^ s)))),
                   
                   (Popt.Expert, "ws", " {32|64}",
                    "target word size is 32 or 64 bit",
                    Popt.Int
                      (fn i =>
                          ws := (case i 
                                  of 32 => Config.Ws32
                                   | 64 => Config.Ws64
                                   | _ =>
                                     usage ("invalid -ws arg: " ^
                                            (Int.toString i)))))]

          in
            List.map (opts, mkOne)
          end

      val extStr = String.dropLast (List.fold (Compilation.exts, "", fn ((ext, _), s) => s ^ "," ^ ext))
      val mainUsage = "ppiler [option ...] [file.{" ^ extStr ^ "} ...]"
      val {usage, parse} =
          Popt.makeUsage {mainUsage = mainUsage,
                          makeOptions = makeOptions,
                          showExpert = fn () => !expert}

      val usage = 
       fn msg => (usage msg; Fail.fail ("Ppiler", "parseCommandLine", "unreachable"))

    in

      case parse (CommandLine.arguments ()) 
       of Result.Yes files =>
          let
            val home =
                case Process.getEnv "IFLCHOME"
                 of SOME d => Path.fromString (OS.Path.mkCanonical d)
                  | _ => !libDir

            val output = 
                case !output
                 of NONE => 
                    (case !toolset
                      of Config.TsGcc => Config.OkC
                       | Config.TsIcc => Config.OkC
                       | Config.TsIpc => Config.OkPillar
                       | Config.TsOpc => Config.OkPillar)
                  | SOME ok => 
                    (case (ok, !toolset)
                      of (Config.OkPillar, Config.TsGcc) => usage "Pillar cannot be compiled with the gcc toolset"
                       | (Config.OkPillar, Config.TsIcc) => usage "Pillar cannot be compiled with the icc toolset"
                       | (Config.OkC,      Config.TsIpc) => usage "C cannot be compiled with the ipc toolset"
                       | (Config.OkC,      Config.TsOpc) => usage "C cannot be compiled with the opc toolset"
                       | (Config.OkC,      Config.TsGcc) => ok
                       | (Config.OkC,      Config.TsIcc) => ok
                       | (Config.OkPillar, Config.TsIpc) => ok
                       | (Config.OkPillar, Config.TsOpc) => ok)
            val () = 
                case (output, !toolset, !futures)
                 of (Config.OkC, _,            Config.PAll)  => usage "-futures all not supported under C"
                  | (Config.OkC, Config.TsGcc, Config.PNone) => ()
                  | (Config.OkC, Config.TsGcc, _)            => usage "-futures not supported with GNU toolset"
                  | _                                        => ()

(*            val () =
                if !futures <> Config.PNone andalso
                   (!thunks = Config.TsDirect orelse !thunks = Config.TsEither) 
                then
                  usage "Futures should not be used with the direct scheme"
                else
                  ()*)
            val () = 
                case (!gcs, output)
                 of (SOME Config.GcsConservative, Config.OkPillar) => usage "Conservative GC not supported on Pillar"
                  | (SOME Config.GcsAccurate,     Config.OkC)      => usage "Accurate GC not supported on C"
                  | _                                              => ()

            fun doOne (_, pd) = PassData.out pd
            val passes = SD.map (passes, doOne)
            val parStyle = !futures
            val gcs =
                case !gcs
                 of NONE =>
                    (case output
                      of Config.OkC      => Config.GcsConservative
                       | Config.OkPillar => Config.GcsAccurate)
                  | SOME gcs => gcs
            val rvt =
                if gcs = Config.GcsAccurate then
                  let val () = gcTagOnly := false in true end
                else
                  false
            val gci = {tagOnly = !gcTagOnly, registerVtables = rvt,
                       reportRoots = rvt, rootsInGlobals = false,
                       reportGlobals = true, style = gcs}

            (* Futures require multi-threading.
             * Otherwise, we default to single-threaded for C, multi-threaded for pillar
             *)
            val singleThreaded = 
                case !single
                 of SOME b => 
                    (case parStyle
                      of Config.PNone => b
                       | _            => 
                         if b then
                           usage "Can't combine futures with single threaded runtime"
                         else
                           b)
                  | NONE => 
                    (case (parStyle, output)
                      of (Config.PNone, Config.OkC) => true
                       | _                          => false)

            val runtimei = {stackMain      = !stackMain,
                            stackWorker    = !stackWorker,
                            singleThreaded = singleThreaded}

            val vectorConfig = Config.VC {isa = !va, instructions = !vInstrs, sizes = !vSizes}

          in
            (Config.C {
              agc              = !agc,
              control_         = Config.Control.finalise controls,
              debug_           = Config.Debug.finalise debugs,
              debugLev         = !debugLev,
              feature_         = Config.Feature.finalise features,
              gc               = gci,
              home             = home,
              host             = !host,
              iflcLibDirectory = !libDir,
              iflcOpt          = !iflcOpt,
              keep             = !keep,
              linkStr          = List.rev (!linkStr),
              linkDirectories  = [],
              linkLibraries    = [],
              logLev           = !logLev,
              output           = output,
              parStyle         = parStyle,
              passes           = passes,
              pilcStr          = List.rev (!pilcStr),
              pilDebug         = !pilDebug,
              pilOpt           = !pilOpt,
              ghcOpt           = !ghcOpt,
              report           = !report,
              runtime          = runtimei,
              sloppyFp         = !sloppyFp,
              stop             = !stop,
              targetWordSize   = !ws,
              timeExecution    = !timeExe,
              toolset          = !toolset,
              vectorConfig     = vectorConfig,
              warnLev          = !warnLev
             },
             files)
          end
      | Result.No msg => usage msg

      end

  fun main () =
      let
        val (config, files) = parseCommandLine ()
        val () = if not (Config.silent config) then printVersion () else ()
      in
        List.foreach (files, fn fname => Compilation.compileWP (config, fname))
      end

end; (* Driver *)
