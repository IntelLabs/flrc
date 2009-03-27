(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

(* Compile C/Pillar and Link *)

signature BACK_END = sig

  structure PilCompile : sig
    val pass : (unit, unit) Pass.t
  end

  structure Link : sig
    val pass : (unit, unit) Pass.t
  end

end

structure BackEnd :> BACK_END =
struct

  val passname = "BackEnd"

   structure Chat = ChatF(type env = Config.t
                          val extract = Utils.Function.id
                          val name = "BackEnd"
                          val indent = 0)
       
   local open OS.Path
   in

   fun rundir config = concat (Config.home config, fromUnixPath "runtime/")
   fun addPath (path, file) = 
       mkCanonical (joinDirFile{dir = path,
                                file = file})
   fun concatPath (path1, path2) = 
       mkCanonical(concat(path1, fromUnixPath path2))

   fun psh (config : Config.t) =
       addPath (concat (Config.home config, fromUnixPath "bin/"), "pillar.sh")

   fun plibdir (config : Config.t) = 
       let
         val pd = case Process.getEnv "PLIB"
                   of SOME d => d
                    | _ => Fail.fail ("BackEnd",
                                      "plibdir",
                                      "$PLIB isn't set")
       in pd
       end
         

   fun pliblib (config, lib) = 
       let
         val libd = concatPath (plibdir config, "lib")
       in 
         addPath(libd, lib)
       end

   fun plibexe (config, exe) = 
       let
         val bind = concatPath (plibdir config, "bin")
       in 
         addPath(bind,exe)
       end

   fun plibinc (config, dir) =
       concatPath (concatPath (plibdir config, "include"), dir)


   end

   fun useFutures (config : Config.t) = 
       case Config.parStyle config
        of Config.PNone => false
         | Config.PAuto => true
         | Config.PAll => true
         | Config.PPar => true
   
   val (gcWriteBarriersF, gcWriteBarriers) =
       Config.Feature.mk ("Plsr:gc-write-barriers",
                          "generate GC write barriers for refs")

   val (gcAllBarriersF, gcAllBarriers) =
       Config.Feature.mk ("Plsr:all-barriers",
                          "generate non-optional write barriers")

   val instrumentAllocationSites = MilToPil.instrumentAllocationSites

   val (usePillarTaggedRationalsF, usePillarTaggedRationals) =
       Config.Feature.mk ("Plsr:pillar-use-tagged-rationals",
                         "use tagged small ints for small rats in pillar")

   val (disableCTaggedRationalsF, disableCTaggedRationals) =
       Config.Feature.mk ("Plsr:c-disable-tagged-rationals",
                          "disable tagged small ints for small rats in c")
      
   val useTaggedRationals = 
    fn config => 
       case Config.output config
        of Config.OkPillar => usePillarTaggedRationals config
         | Config.OkC      => not (disableCTaggedRationals config)

   val (instrumentAllocationF, instrumentAllocation) =
      Config.Feature.mk ("Plsr:instrument-allocation",
                         "gather allocation statistics")

   val (instrumentVtbAllocationF, instrumentVtbAllocation) =
      Config.Feature.mk ("Plsr:instrument-vtb-alc",
                         "gather allocation statistics per vtable")

   val (vtableChangeF, vtableChange) =
       Config.Feature.mk ("Plsr:change-vtables",
                          "do vtable changing for immutability etc.")


   fun defines (config : Config.t) =
       let
         val ws =
             case Config.targetWordSize config
              of Config.Ws32 => "P_WORD_SIZE=4"
               | Config.Ws64 => "P_WORD_SIZE=8"

         val gc =
             case #style (Config.gc config)
              of Config.GcsNone => []
               | Config.GcsConservative => ["P_USE_CGC"]
               | Config.GcsAccurate =>
                 ["P_USE_AGC",
                  "P_AGC_LOCK_PARAM=" ^
                  (case Config.agc config
                    of Config.AgcGcMf => "0"
                     | Config.AgcTgc  => "1"
                     | Config.AgcCgc  => "1")]
                 @
                 (if Config.agc config = Config.AgcTgc orelse
                     Config.agc config = Config.AgcCgc
                  then ["P_USE_FAST_ALLOC"]
                  else [])
                 @
                 (if gcWriteBarriers config
                  then ["P_USE_GC_WRITE_BARRIERS"]
                  else [])
                 @
                 (if gcAllBarriers config
                  then ["P_ALL_BARRIERS"]
                  else [])

         val pbase = 
             case Config.output config
              of Config.OkPillar => ["P_USE_PILLAR", "WIN32"]
               | Config.OkC      => []

         val debug = 
             if Config.pilDebug config then
               ["GC_DEBUG"]
             else
               ["NDEBUG"]

         val futures = 
             if useFutures config then ["P_USE_PARALLEL_FUTURES"] else []

         val vi = 
             if Config.vi config then ["P_USE_VNI"] else []

         val instr =
             List.concat
             [if instrumentAllocation config
              then ["P_INSTRUMENT_ALLOCATION"]
              else [],
              if instrumentVtbAllocation config orelse
                 instrumentAllocationSites config
              then ["P_INSTRUMENT_VTB_ALC"]
              else []]

         val runtime = 
             List.concat
             [
              if useTaggedRationals config then
                ["P_USE_TAGGED_RATIONALS"]
              else
                []
             ]
         val vtbChg =
             if vtableChange config then ["P_DO_VTABLE_CHANGE"] else []
       in
         List.concat [runtime, 
                      vi, 
                      [ws], 
                      gc, 
                      futures, 
                      debug, 
                      pbase, 
                      instr, 
                      vtbChg]
       end

   val pillarStack =   2097152  (* Decimal integer in bytes (  0x200000) *)
   val smallStack  =  33554432  (* Decimal integer in bytes ( 0x2000000) *)
   val largeStack  = 536870912  (* Decimal integer in bytes (0x20000000) *) 

   fun stackSize (config : Config.t) = 
       (case (Config.stack config, Config.output config)
         of (SOME i, _) => i
          | (NONE, Config.OkPillar) => pillarStack
          | (NONE, Config.OkC) => 
            if useFutures config then smallStack else largeStack)

   fun stackStr (config : Config.t) = 
       let
         val i = stackSize config
         val s = Int.toString i
       in s
       end

   fun icc (config : Config.t, fname) = 
       let
         val infile = 
             fname^".c"
         val outfile = 
             "-Fo"^fname^".obj"
               
         val cc  = "icl"

         val options = 
             let
               val dOpts = ["-Zi", "-debug"]
                        
               val oOpts =
                   case Config.pilOpt config
                    of 0 => ["-Od"]
                     | 1 => ["-O1"]
                     | 2 => ["-O3"]
                     (* | 3 => ["-O3"]  FIXME: WL: make the default to 4 *)
                     | 3 => ["-Ox", "-Qip", "-QaxT", 
                             "-Qvec-report0", "-Qdiag-disable:cpu-dispatch"]
                     | _ => Fail.fail ("Backend", "icc",
                                       "Bad opt level")
                            
               val fpOpts = 
                   if Config.sloppyFp config then
                     ["-fp:fast=2"]
                   else
                     ["-fp:strict", "-Qfp-port"]
                       
                       (* 177 = unused variable
                        * 279 = constant controlling expression
                        *)
               val wOpts = ["-W3", "-Qwd 177", "-Qwd 279"]
               val lOpts = ["-TC", "-Qc99", "-c"]
                           
               val mOpts = if useFutures config then ["/MT"] else []
             in List.concat [dOpts, oOpts, fpOpts, wOpts, lOpts, mOpts]
             end

         val defs = List.map (defines config, fn s => "/D" ^ s)

         val includes = 
             let
               val gcinc =
                   ["-I", plibinc (config, "gc-bdw"), "-I", rundir config]
               val finc = ["-I", plibinc (config, "prt")]
               val minc = if useFutures config
                          then ["-I", plibinc (config, "mcrt")]
                          else []
             in gcinc @ finc @ minc
             end
         val args =
             List.concat[options, defs, [infile], includes, [outfile], 
                         Config.pilcStr config]
         val cleanup = fn () => if Config.keepPil config then ()
                                else File.remove infile
       in (cc, args, cleanup)
       end


   fun gcc (config : Config.t, fname) = 
       let
         val infile = 
             fname^".c"
         val outfile = 
             "-o"^fname^".o"
               
         val cc  = "gcc"
         val options = 
             let
               val dOpts =
                   if Config.pilDebug config then
                     ["-g"]
                   else
                     []
                        
               val oOpts =
                   case Config.pilOpt config
                    of 0 => ["-O0"]
                     | 1 => ["-O1"]
                     | 2 => ["-O2"]
                     | 3 => ["-O3"]
                     | _ => Fail.fail ("Backend", "gcc",
                                       "Bad opt level")
                            
               val fpOpts = 
                   if Config.sloppyFp config then
                     ["-ffast-math"]
                   else
                     ["-ffloat-store"]
                       

               val wOpts = [(*"-Wall"*)]
               val lOpts = ["-std=c99", "-c"]

             in List.concat [dOpts, oOpts, fpOpts, wOpts, lOpts]
             end

         val defs = List.map (defines config, fn s => "-D" ^ s)
         val includes =
             ["-I", plibinc (config, "gc-bdw"), "-I", rundir config] @
             ["-I", plibinc (config, "prt")] @ (* for toolkit futures *)
             (if useFutures config
              then ["-I", plibinc (config, "mcrt")]
              else [])
         val args =
             List.concat[options, defs, [infile], includes, [outfile],
                         Config.pilcStr config ]
         val cleanup = fn () => if Config.keepPil config then ()
                                else File.remove infile
       in (cc, args, cleanup)
       end

   fun picc (config : Config.t, fname) = 
       let
         val infile = 
             fname^".c"
         val outfile = 
             "-Fo"^fname^".obj"

         val cmd = plibexe(config, "pilicl")

         val options = 
             let
               val dOpts = ["-Zi", "-debug"]

               val () = if (Config.pilOpt config) < 2 then
                          Chat.warn0 (config, 
                                      "Ignoring optimization flag to avoid Pillar bug")
                        else
                          ()
               val oOpts =
                   case Config.pilOpt config
                    of 0 => ["-O2", "-Ob0"]
                     | 1 => ["-O2", "-Ob0"]
                     | 2 => ["-O2", "-Ob0"]
                     | 3 => ["-O2", "-Ob0"] 
                     | _ => Fail.fail ("Backend", "picc",
                                       "Bad opt level")

               val fpOpts = 
                   if Config.sloppyFp config then
                     ["-fp:fast=2"]
                   else
                     ["-fp:strict", "-Qfp-port"]

               val wOpts = ["-W3", "-Qwd 177", "-Qwd 279", "-Qwd 188"]
               val lOpts = ["-TC", "-Qc99", "-c", 
                            "-mCG_opt_mask=0xfffe",
                            "/Qtlsregister:ebx",
                            "/Qoffsetvsh:0", 
                            "/Qoffsetusertls:4", 
                            "/Qoffsetstacklimit:16"]
               val mOpts = ["/MT"]
             in List.concat [dOpts, oOpts, fpOpts, wOpts, lOpts, mOpts]
             end
         val defs = List.map (defines config, fn s => "/D" ^ s)
         val includes = ["-I", rundir config,
                         "-I", plibinc (config, "prt"),
                         "-I", plibinc (config, "pgc")] @
                        (if useFutures config
                         then ["-I", plibinc (config, "mcrt")]
                         else [])
         val args =
             List.concat[options, defs, [infile], includes, [outfile],
                         Config.pilcStr config]
         val cleanup = fn () => if Config.keepPil config then ()
                                else File.remove infile
       in (cmd, args, cleanup)
       end


   fun ilink(config : Config.t, fname) =  
       let
         val infile = 
             fname^".obj"
         val outfile = 
             "/Fe"^fname^".exe"

         val ld  = "icl"

         val options = 
             let
               val opts = ["/link",
                           "/nologo", 
                           "/INCREMENTAL:NO",
                           (* "/NODEFAULTLIB:libcmt", *)
                           "/stack:"^(stackStr config)]
               val dOpts = ["/debug"]
             in List.concat [opts, dOpts]
             end

         val uobjs = []
         val libs = 
             let
               val mt = useFutures config
               val debug = Config.pilDebug config
               val gclibs =
                   case #style (Config.gc config)
                    of Config.GcsNone => []
                     | Config.GcsConservative =>
                       (case (mt, debug) 
                         of (true, true)   => 
                            [pliblib (config, "gc-bdw-dlld.lib")]
                          | (true, false)  => 
                            [pliblib (config, "gc-bdw-dll.lib")]
                          | (false, true)  => [pliblib (config, "gc-bdwd.lib")]
                          | (false, false) => [pliblib (config, "gc-bdw.lib")])
                     | Config.GcsAccurate =>
                       Fail.fail ("BackEnd", "ilink", 
                                  "accurate GC not supported on C")
               val flibs =
                   (if mt
                    then [pliblib (config, "mcrtd.lib")]
                    else []) @
                   let
                     val nm =
                         case (mt, debug)
                          of (false, false) => "sequential"
                           | (false, true ) => "sequentiald"
                           | (true, false) => "parallel"
                           | (true, true ) => "paralleld"
                   in
                     [pliblib (config, "ptkfutures_" ^ nm ^ ".lib")]
                   end
             in List.concat [["user32.lib"], gclibs @ flibs]
             end
         val args = List.concat[[outfile], [infile], options, uobjs, libs,
                                Config.linkStr config]
         val cleanup = fn () => if Config.keepObj config then ()
                                else File.remove infile
       in (ld, args, cleanup)
       end


   fun ld(config : Config.t, fname) = 
       let
         val infile = 
             fname^".o"
         val outfile = 
             "-o"^fname^".exe"

         val ld  = "gcc"
         val options = ["-O2", "--stack="^(stackStr config)]
         val uobjs = [] 
         val libs = 
             let
               val gclibs =
                   case (#style (Config.gc config), Config.pilDebug config)
                    of (Config.GcsNone, _) => []
                     | (Config.GcsConservative, true) =>
                       [pliblib (config, "libgc-bdwd.a")]
                     | (Config.GcsConservative, false) =>
                       [pliblib (config, "libgc-bdw.a")]
                     | (Config.GcsAccurate, _) =>
                       Fail.fail ("BackEnd", "ld",
                                  "accurate GC not supported on C")
               val flibs =
                   (if useFutures config
                    then [pliblib (config, "mcrtd.lib")] 
                    else []) @
                   let
                     val mt = useFutures config
                     val nm =
                         case (mt, Config.pilDebug config)
                          of (false, false) => "sequential"
                           | (false, true ) => "sequentiald"
                           | (true,  false) => "parallel"
                           | (true,  true ) => "paralleld"
                   in
                     [pliblib (config, "ptkfutures_gcc_" ^ nm ^ ".lib")]
                   end
             in List.concat [gclibs, flibs]
             end

         val args = List.concat[options, [infile], uobjs, libs, [outfile],
                                Config.linkStr config]
         val cleanup = fn () => if Config.keepObj config then ()
                                else File.remove infile
       in (ld, args, cleanup)
       end

   fun plink(config : Config.t, fname) =  
       let
         val infile = 
             fname^".obj"
         val outfile = 
             "/out:"^fname^".exe"

         val cmd = plibexe(config, "pilink")

         val options = 
             let
               val opts =
                   ["/nologo", 
                    "/INCREMENTAL:NO",
                    (* "/NODEFAULTLIB:libcmt", *)
                    "/stack:"^(stackStr config)]          
               val dOpts = ["/debug"]
             in List.concat [opts, dOpts]
             end

         val mlibs =
             let
               val flibs =
                   let
                     val mt = useFutures config
                     val nm =
                         case (mt, Config.pilDebug config)
                          of (false, false) => "sequential"
                           | (false, true ) => "sequentiald"
                           | (true,  false) => "parallel"
                           | (true,  true ) => "paralleld"
                   in
                     [pliblib (config, "ptkfutures_pillar_" ^ nm ^ ".obj")]
                   end
             in flibs
             end
         val mobjs = [infile]
         val ulibs = 
             let
               val gclibs =
                   case (#style (Config.gc config), Config.pilDebug config)
                    of (Config.GcsNone, _) => []
                     | (Config.GcsConservative, _) =>
                       Fail.fail ("BackEnd", "plink",
                                  "conservative GC not supported on Pillar")
                     | (Config.GcsAccurate, true) =>
                       [
                        pliblib (config, "pgcd.lib"),
                        "imagehlp.lib",
                        case Config.agc config
                         of Config.AgcGcMf => pliblib (config, "gc-mfd.lib")
                          | Config.AgcTgc  => pliblib (config, "gc-tgcd.lib")
                          | Config.AgcCgc  => pliblib (config, "gc-cgcd.lib")
                       ]
                     | (Config.GcsAccurate, false) =>
                       [
                        pliblib (config, "pgc.lib"),
                        "imagehlp.lib",
                        case Config.agc config
                         of Config.AgcGcMf => pliblib (config, "gc-mf.lib")
                          | Config.AgcTgc  => pliblib (config, "gc-tgc.lib")
                          | Config.AgcCgc  => pliblib (config, "gc-cgc.lib")
                       ]
             in gclibs
             end
         val uobjs = []
         val prtlib = if Config.pilDebug config then
                        pliblib (config, "pillard.lib")
                      else  
                        pliblib (config, "pillar.lib")
         val mcrtlib = if Config.pilDebug config then
                         pliblib (config, "mcrtd.lib")
                       else  
                         pliblib (config, "mcrt.lib")

         val prtbegin = if Config.pilDebug config then 
                          pliblib (config, "crt_prtbegind.obj")
                        else
                          pliblib (config, "crt_prtbegin.obj")
         val prtend = if Config.pilDebug config then
                        pliblib (config, "crt_prtendd.obj")
                      else  
                        pliblib (config, "crt_prtend.obj")

         val args = List.concat[[outfile],
                                [prtbegin],
                                mobjs, mlibs,
                                [prtend],
                                uobjs, ulibs,
                                [prtlib, mcrtlib],
                                options,
                                Config.linkStr config]

         val cleanup = fn () => if Config.keepObj config then ()
                                else File.remove infile
       in (cmd, args, cleanup)
       end

   fun compile (config : Config.t, fname) =
       let 
         val (c, args, cleanup) =
             case Config.output config
              of Config.OkC => 
                 (case Config.toolset config
                   of Config.Intel => icc (config, fname)
                    | Config.Gnu   => gcc (config, fname))
               | Config.OkPillar   => picc(config, fname)
         val () = 
             Exn.finally (fn () => Pass.run (config, Chat.log0, c, args),
                          cleanup)
       in ()
       end
       
   fun link (config : Config.t, fname) =
       let 
         val (c, args, cleanup) =
             case Config.output config 
              of Config.OkC => 
                 (case Config.toolset config
                   of Config.Intel => ilink (config, fname)
                    | Config.Gnu   => ld    (config, fname))
               | Config.OkPillar   => plink (config, fname)
         val () = 
             Exn.finally (fn () => Pass.run (config, Chat.log0, c, args),
                          cleanup)
       in 
         ()
       end

   structure PilCompile =
   struct
     val description = {name        = "PilCompile",
                        description = "Compile Pil",
                        inIr        = Pass.unitHelpers,
                        outIr       = Pass.unitHelpers,
                        mustBeAfter = [],
                        stats       = []}
     val associates = {controls = [],
                       debugs = [],
                       features = [gcWriteBarriersF, 
                                   gcAllBarriersF,
                                   usePillarTaggedRationalsF,
                                   disableCTaggedRationalsF,
                                   instrumentAllocationF,
                                   instrumentVtbAllocationF,
                                   vtableChangeF],
                       subPasses = []}
     fun pilCompile ((), pd, basename) =
         compile (PassData.getConfig pd, basename)
     val pass = Pass.mkFilePass (description, associates, pilCompile)
   end

   structure Link =
   struct
     val description = {name        = "Link",
                        description = "Link the executable",
                        inIr        = Pass.unitHelpers,
                        outIr       = Pass.unitHelpers,
                        mustBeAfter = [],
                        stats       = []}
     val associates = {controls = [],
                       debugs = [],
                       features = [],
                       subPasses = []}
     fun link' ((), pd, basename) = link (PassData.getConfig pd, basename)
     val pass = Pass.mkFilePass (description, associates, link')
   end

end;
