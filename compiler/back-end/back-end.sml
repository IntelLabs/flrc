(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

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

  val fail = fn (f, m) => Fail.fail (passname, f, m)

  structure Chat = ChatF(struct
                           type env = Config.t
                           val extract = Utils.Function.id
                           val name = "BackEnd"
                           val indent = 0
                         end)
       
  val runtimeDirectory = 
   fn config => Path.snoc (Config.home config, "runtime")

  val iflcLibDirectory = 
   fn config => Config.iflcLibDirectory config
      
  val iflcLibLibDirectory = 
   fn config => Path.snoc (iflcLibDirectory config, "lib")

  val iflcLibIncludeDirectory =
   fn config => Path.snoc (iflcLibDirectory config, "include")
                
  val iflcLibBinDirectory = 
   fn config => Path.snoc (iflcLibDirectory config, "bin")

  val iflcLibLibrary = 
   fn (config, file) => Path.snoc (iflcLibDirectory config, file)

  val iflcLibInclude = 
   fn (config, file) => Path.snoc (iflcLibIncludeDirectory config, file)

  val iflcLibExe = 
   fn (config, exe) => Path.snoc (iflcLibBinDirectory config, exe)

  fun useFutures (config : Config.t) = 
       case Config.parStyle config
        of Config.PNone => false
         | Config.PAuto => true
         | Config.PAll => true
         | Config.PPar => true

  fun singleThreaded (config : Config.t) = #singleThreaded (Config.runtime config)

  fun multiThreaded (config : Config.t) = useFutures config orelse not(singleThreaded config)

  fun synchronizeThunks (config : Config.t) = useFutures config orelse Config.synchronizeThunks config

  fun ifDebug (config, ad, a) = if Config.pilDebug config then ad else a
   
  val (gcWriteBarriersF, gcWriteBarriers) =
      Config.Feature.mk ("Plsr:gc-write-barriers",
                         "generate GC write barriers for refs")
      
  val (gcAllBarriersF, gcAllBarriers) =
      Config.Feature.mk ("Plsr:all-barriers",
                         "generate non-optional write barriers")
      
  val instrumentAllocationSites = MilToPil.instrumentAllocationSites

  val backendYields = MilToPil.backendYields

  val lightweightThunks = MilToPil.lightweightThunks

  val interceptCuts = MilToPil.interceptCuts

  val noGMP = MilToPil.noGMP

  val (gcIndirectionsF, gcIndirections) = 
      Config.Feature.mk ("Plsr:gc-indirections",
                         "GC cleans up thunk indirections")

  val (gmpUseGcMallocF, gmpUseGcMalloc) = 
      Config.Feature.mk ("Plsr:gmp-use-gc-malloc",
                         "use gc-malloc for gmp integer wrappers")

  val (gmpUseMallocF, gmpUseMalloc) = 
      Config.Feature.mk ("Plsr:gmp-use-malloc",
                         "use malloc for gmp integer wrappers")

  val (gmpUsePinningF, gmpUsePinning) = 
      Config.Feature.mk ("Plsr:gmp-use-pinning",
                         "use pinning for gmp integer wrappers")

  val (gmpUsePCDeclF, gmpUsePCDecl) = 
      Config.Feature.mk ("Plsr:gmp-use-pcdecl",
                         "use pcdecl for gmp integer wrappers")

  val (gmpUseForcedGcF, gmpUseForcedGc) = 
      Config.Feature.mk ("Plsr:gmp-use-forced-gc",
                         "use gc forcing gmp integer wrappers")

  val (gmpUseGAllocateF, gmpUseGAllocate) = 
      Config.Feature.mk ("Plsr:gmp-use-gallocate",
                         "use guaranteed allocation gmp integers")

  val (instrumentAllocationF, instrumentAllocation) =
      Config.Feature.mk ("Plsr:instrument-allocation",
                         "gather allocation statistics")

  val (instrumentGCsF, instrumentGCs) =
      Config.Feature.mk ("Plsr:instrument-gcs",
                         "gather allocation statistics per gc")

  val (instrumentVtbAllocationF, instrumentVtbAllocation) =
      Config.Feature.mk ("Plsr:instrument-vtb-alc",
                         "gather allocation statistics per vtable")

  val (vtableChangeF, vtableChange) =
      Config.Feature.mk ("Plsr:change-vtables",
                         "do vtable changing for immutability etc.")

   val (noThunkSubsumptionF, noThunkSubsumption) = 
       Config.Feature.mk ("Plsr:no-thunk-subsumption", "don't use thunk/value subsumption")

  val (usePortableTaggedIntsF, usePortableTaggedInts) = 
      Config.Feature.mk ("Plsr:tagged-ints-portable",
                         "tagged ints don't assume two's complement")

  val (assumeSmallIntsF, assumeSmallInts) = 
      Config.Feature.mk ("Plsr:tagged-ints-assume-small",
                         "use 32 bit ints for tagged ints (unchecked)")

  val (noTaggedIntRecoverF, noTaggedIntRecover) = 
      Config.Feature.mk ("Plsr:no-recover-tagged-ints",
                         "don't check output of AP arithmetic for small ints")

  val (disableTailCallF, disableTailCall) = 
      Config.Feature.mk ("Plsr:disable-tail-call",
                         "implement tail calls as ordinary calls")

  datatype compiler = CcGCC | CcICC | CcOpc | CcIpc
  datatype linker = LdGCC | LdICC | LdOpc | LdIpc

  val opcStack    =   2097152  (* Decimal integer in bytes (  0x200000) *)
  val smallStack  =  33554432  (* Decimal integer in bytes ( 0x2000000) *)
  val largeStack  = 536870912  (* Decimal integer in bytes (0x20000000) *) 

  fun defaultStackSize (config : Config.t, ld) = 
      (case (#stackMain (Config.runtime config), Config.output config, ld)
        of (SOME i, _, _)                 => i
         | (NONE, Config.OkPillar, LdOpc) => opcStack
         | (NONE, Config.OkPillar,     _) => smallStack
         | (NONE, Config.OkC,      _    ) => smallStack)

  fun defaultStackStr (config : Config.t, ld) = 
      let
        val i = defaultStackSize (config, ld)
        val s = Int.toString i
      in s
      end

  fun mainStackSize (config : Config.t, cc) = 
      (case (#stackMain (Config.runtime config), cc)
        of (SOME i,   _) => i
         | (NONE, CcOpc) => opcStack
         | (NONE,     _) => smallStack) div (1024 * 1024)

  fun mainStackStr (config : Config.t, cc) = 
      let
        val i = mainStackSize (config, cc)
        val s = Int.toString i
      in s
      end

  fun workerStackSize (config : Config.t, cc) = 
      (case (#stackWorker (Config.runtime config), cc)
        of (SOME i,   _) => i
         | (NONE, CcOpc) => opcStack
         | (NONE,     _) => opcStack*2) div (1024 * 1024)

  fun workerStackStr (config : Config.t, cc) = 
      let
        val i = workerStackSize (config, cc)
        val s = Int.toString i
      in s
      end

  fun sourceFile (config, compiler, fname) = fname^".c"

  fun objectFile (config, compiler, fname) = 
      (case compiler 
        of CcGCC    => fname^".o"
         | CcICC    => fname^".obj"
         | CcOpc    => fname^".obj"
         | CcIpc    => fname^".obj")

  fun exeFile (config, compiler, fname) = fname^".exe"

  fun compiler (config, compiler) = 
      (case compiler 
        of CcGCC    => (Pass.run, Path.fromString "gcc")
         | CcICC    => (Pass.run, Path.fromString "icl")
         | CcOpc    => (Pass.runWithSh, iflcLibExe (config, "pilicl"))
         | CcIpc    => (Pass.runWithSh, iflcLibExe (config, "pilicl")))
      
  fun includes (config, compiler) = 
      let
        val mcrt = 
            if useFutures config then
              [iflcLibInclude (config, "mcrt")]
            else []
        val gmp = 
            if noGMP config then
              []
            else 
              [iflcLibInclude (config, "gmp")]
        val files = 
            (case compiler
              of CcGCC => 
                 [iflcLibInclude (config, "gc-bdw"), runtimeDirectory config, iflcLibInclude (config, "prt-pthreads")] 
                 @ mcrt @ gmp
               | CcICC => 
                 [iflcLibInclude (config, "gc-bdw"), runtimeDirectory config, iflcLibInclude (config, "prt-pthreads")] 
                 @ mcrt @ gmp
               | CcOpc => 
                 [runtimeDirectory config, iflcLibInclude (config, "prt"), iflcLibInclude (config, "pgc")] 
                 @ mcrt @gmp
               | CcIpc => 
                 [runtimeDirectory config, iflcLibInclude (config, "prt-pthreads"), iflcLibInclude (config, "pgc")]
                 @gmp)

        val flags = List.map (files, fn s => "-I" ^ Config.pathToHostString (config, s))
      in flags
      end

  fun defines (config : Config.t, compiler : compiler) =
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
                (case Config.agc config
                  of Config.AgcGcMf => ["P_AGC_LOCK_PARAM=0", "P_USE_AGC=PlsrAKMf"]
                   | Config.AgcTgc  => ["P_AGC_LOCK_PARAM=1", "P_USE_AGC=PlsrAKTgc"]
                   | Config.AgcCgc  => ["P_AGC_LOCK_PARAM=1", "P_USE_AGC=PlsrAKCgc"])
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

        val debug = ifDebug (config, ["GC_DEBUG"], ["NDEBUG"])

        val futures = 
            if useFutures config then ["P_USE_PARALLEL_FUTURES"] else []

        val synchThunks = 
            if synchronizeThunks config then ["PLSR_THUNK_SYNCHRONIZE"] else []

        val singleThreaded = 
            if singleThreaded config then ["PLSR_SINGLE_THREADED"] else []

        val thunks = 
            (if lightweightThunks config then ["PLSR_LIGHTWEIGHT_THUNKS"] else []) @
            (if interceptCuts config then ["PLSR_THUNK_INTERCEPT_CUTS"] else []) @
            (if noThunkSubsumption config then ["PLSR_THUNK_NO_SUBSUMPTION"] else []) @
            (if gcIndirections config then ["PLSR_GC_INDIRECTIONS"] else [])

        val instr =
            List.concat
              [if instrumentAllocation config
               then ["PLSR_INSTRUMENT_ALLOCATION"]
               else [],
               if instrumentVtbAllocation config orelse
                  instrumentAllocationSites config
               then ["PLSR_INSTRUMENT_VTB_ALC"]
               else [],
               if instrumentGCs config then 
                 ["PLSR_INSTRUMENT_GCS"]
               else
                 []
              ]

        val tailcall = 
            if disableTailCall config then ["PLSR_DISABLE_TAILCALL"] else []

        val vtbChg =
            if vtableChange config then ["P_DO_VTABLE_CHANGE"] else []

        val va = 
            let
              val Config.VC {isa, ...} = Config.vectorConfig config
            in
              case isa
               of Config.ViAVX   => ["P_USE_VI_AVX"]
                | Config.ViMIC   => ["P_USE_VI_MIC"]
                | Config.ViSSE _ => ["P_USE_VI_SSE"]
                | _              => []
            end

        val numericDefines =
            (if PObjectModelLow.Rat.useUnsafeIntegers config then 
               ["P_PRAT_IS_SINTP"]
             else 
               []) @
            (if Globals.disableOptimizedRationals config then
               []
             else  
               ["P_USE_TAGGED_RATIONALS"]) @
            (if Globals.disableOptimizedIntegers config then
               []
             else  
               ["P_USE_TAGGED_INTEGERS"]) @
            (if usePortableTaggedInts config then ["P_TAGGED_INT32_PORTABLE"] 
             else if assumeSmallInts config then ["P_TAGGED_INT32_ASSUME_SMALL"] 
             else if MilToPil.assertSmallInts config then ["P_TAGGED_INT32_ASSERT_SMALL"]
             else []) @
            (if noGMP config then
               ["PLSR_NO_GMP_INTEGERS"]
             else
               []) @
            (if noTaggedIntRecover config then
               []
             else
               ["PLSR_TAGGED_INTEGER_RECOVER"]) @
            (if gmpUseMalloc config then
               ["PLSR_GMP_USE_MALLOC"]
             else if gmpUsePinning config then
               ["PLSR_GMP_USE_PINNING"]
             else if gmpUsePCDecl config then 
               ["PLSR_GMP_USE_PCDECL"]
             else if gmpUseGcMalloc config then 
               ["PLSR_GMP_USE_GCMALLOC"]
             else if gmpUseForcedGc config then 
               ["PLSR_GMP_USE_FORCE_GC"]
             else if gmpUseGAllocate config then 
               ["PLSR_GMP_USE_GALLOCATE"]
             else
               ["PLSR_GMP_USE_DEFAULT"])

        val backend = 
            (case compiler
              of CcGCC    => ["PPILER_BACKEND_GCC"] @
                (if Config.host config = Config.OsLinux
                 then ["PLSR_LINUX", "LINUX"]
                 else [])
               | CcICC    => ["PPILER_BACKEND_ICC"]
               | CcOpc    => ["PPILER_BACKEND_OPC"]
               | CcIpc    => ["PPILER_BACKEND_IPC"])

        val stackSize = ["PLSR_STACK_SIZE_WORKER="^workerStackStr (config, compiler),
                         "PLSR_STACK_SIZE_MAIN="^mainStackStr (config, compiler)]
        val ds = 
            List.concat [[ws], 
                         gc, 
                         futures, 
                         singleThreaded,
                         synchThunks,
                         stackSize,
                         thunks,
                         debug, 
                         pbase, 
                         instr, 
                         tailcall,
                         vtbChg,
                         va,
                         numericDefines, 
                         backend]
        val flags = 
            List.map (ds, fn s => "-D" ^ s)
      in flags
      end

  fun libDirs (config : Config.t, linker : linker) : string list =
      let
        val iflcLibDir = iflcLibLibDirectory config
        val libs = 
            case linker
             of LdGCC    => [iflcLibDir] (*[Path.snoc (iflcLibDir, "gcc")]*)
              | LdICC    => [iflcLibDir]
              | LdOpc    => [iflcLibDir]
              | LdIpc    => [iflcLibDir, Path.snoc (iflcLibDir, "vs2010")]
        val libs = List.map (libs, fn l => Config.pathToHostString(config, l))
      in libs
      end

  structure CcOptions =
  struct

    fun start (config, compiler) = 
        (case compiler
          of CcIpc => ["-p2c"]
           | _     => [])

    fun out (config, compiler) = ["-c"]

    fun obj ((config, compiler), fname) = 
        (case compiler 
          of CcGCC    => ["-o"^fname]
           | CcICC    => ["-Fo"^fname]
           | CcOpc    => ["-Fo"^fname]
           | CcIpc    => ["-Fo"^fname])

    fun debug (config, compiler) =
        (case compiler
          of CcGCC    => ifDebug (config, ["-g"], [])
           | CcICC    => ["-Zi", "-debug"]
           | CcOpc    => ["-Zi", "-debug"]
           | CcIpc    => ["-Zi", "-debug"])

    fun arch (config, compiler) = 
        (case compiler
          of CcGCC    => ["-msse3"] (* without -msse, we should use -ffloat-store in float*)
           | CcICC    => ["-QxT"]
           | CcOpc    => ["-QxB"]
           | CcIpc    => 
             let
               val Config.VC {isa, ...} = Config.vectorConfig config
             in case isa
                 of Config.ViAVX        => ["-QxAVX"]
                  | Config.ViSSE (a, b) => ["-QxSSE"^Int.toString a^"."^Int.toString b]
                  | _                   => ["-QxHost"]
             end)

    fun opt (config, compiler) =
        let
          val level = Config.pilOpt config
          val ps = 
              (case compiler
                of CcGCC  =>
                   (case level
                     of 0 => ["-O0"]
                      | 1 => ["-O1"]
                      | 2 => ["-O2"]
                      | 3 => ["-O3"]
                      | _ => fail ("gcc", "Bad opt level"))
                 | CcICC  => 
                   (case level
                     of 0 => ["-Od"]
                      | 1 => ["-O1"]
                      | 2 => ["-O2"]
                      | 3 => ["-O3", "-Qip",
                              "-Qvec-report0", "-Qdiag-disable:cpu-dispatch"]
                      | _ => fail ("icc", "Bad opt level"))
                 | CcOpc => 
                   let
                     val oLevel = 
                         (case level
                           of 0 => ["-Od"]
                            | 1 => 
                              let
                                val () = Chat.warn0 (config, 
                                                     "Ignoring optimization flag to avoid Pillar bug")
                              in ["-O2"]
                              end
                            | 2 => ["-O2"]
                            | 3 => ["-O2"]
                            | _ => fail ("picc", "Bad opt level"))
                     val opts = 
                         oLevel @[ "-Ob0", (* disable inlining*)
                                   "-mP2OPT_pre=false", (* disable PRE *)
                                   "-mCG_opt_mask=0xfffe",
                                   "-mP3OPT_pcg_ra_region_picking=0" (* Prevents shrink wrapping *)
                                 ]
                   in opts
                   end
                 | CcIpc => 
                   let
                     val opts = 
                         (case level
                           of 0 => ["-Od"]
                            | 1 => ["-O1"]
                            | 2 => ["-O2"]
                            | 3 => ["-O3", "-Qip",
                                    "-Qvec-report0", "-Qdiag-disable:cpu-dispatch"]
                            | _ => fail ("picc", "Bad opt level"))
                   in opts
                   end
              )
        in ps
        end

    fun float (config, compiler) =
        let
          val sloppy = Config.sloppyFp config
          val os = 
              (case (compiler, sloppy)
                of (CcGCC,    true)  => ["-ffast-math"]
                                       (* fpmath only works if -msse{|1|2} is set *)
                                       (* without -msse, we should use -ffloat-store*)
                 | (CcGCC,    false) => ["-mieee-fp", "-mfpmath=sse"] 
                                       (* Pillar doesn't have -Qftz *)
                 | (CcICC,    true)  => ["-fp:fast", "-Qftz"]
                 | (CcICC,    false) => ["-fp:source", "-Qftz-", "-Qprec-div", "-Qprec-sqrt", "-Qvec-"]
                 | (CcOpc,    true)  => ["-fp:fast"]
                 | (CcOpc,    false) => ["-fp:source", "-Qprec-div", "-Qprec-sqrt", "-Qvec-"]
                 | (CcIpc,    true)  => ["-fp:fast", "-Qftz"]
                 | (CcIpc,    false) => ["-fp:source", "-Qftz-", "-Qprec-div", "-Qprec-sqrt", "-Qvec-"]
              )
        in os
        end

    fun warn (config, compiler) =
        (case compiler
          of CcGCC    => [(*"-Wall"*)]
           | CcICC    => ["-W3", 
                          "-Qwd 177", (* Unused variable *)
                          "-Qwd 279"  (* Controlling expression is constant*)
                         ]
           | CcOpc    => ["-W3", "-Qwd 177", "-Qwd 279"]
           | CcIpc    => ["-W3", "-Qwd 177", "-Qwd 279"]
        )

    fun align (config, compiler) = 
        (case compiler 
          of CcGCC => ["-malign-double"]
           | _     => [])

    fun lang (config, compiler) =
        (case compiler
          of CcGCC    => ["-std=c99"]
           | CcICC    => ["-TC", "-Qc99"]
           | CcOpc    => ["-TC", "-Qc99",
                          "-Qtlsregister:ebx",
                          "-Qoffsetvsh:0", 
                          "-Qoffsetusertls:4", 
                          "-Qoffsetstacklimit:16"]
           | CcIpc    => ["-TC", "-Qc99"]
        )

    fun runtime (config, compiler) = 
        (case (compiler, backendYields config)
          of (CcOpc,    false) => ["-Qnoyield"]
           | (CcIpc,    false) => ["-Qnoyield"]
           | _                 => [])

    fun mt (config, compiler) =
        (case compiler
          of CcGCC    => []
           | CcICC    => [ifDebug (config, "-MTd", "-MT")] 
           | CcOpc    => [ifDebug (config, "-MTd", "-MT")]
           | CcIpc    => [ifDebug (config, "-MTd", "-MT")])

  end (* structure CcOptions *)

  fun compile (config : Config.t, ccTag, fname) = 
      let
        val fname = Config.pathToHostString (config, fname)
        val inFile = sourceFile (config, ccTag, fname)
        val outFile = objectFile (config, ccTag, fname)
        val cfg = (config, ccTag)
        val cc = compiler cfg
        val options = 
            [
             CcOptions.start cfg,
             CcOptions.out cfg,
             CcOptions.debug cfg,
             CcOptions.arch cfg,
             CcOptions.opt cfg,
             CcOptions.float cfg,
             CcOptions.warn cfg,
             CcOptions.lang cfg,
             CcOptions.align cfg,
             CcOptions.runtime cfg,
             CcOptions.mt cfg
            ]
        val options = List.concat options
        val defs = defines (config, ccTag)
        val incs = includes cfg
        val args = [options, defs, [inFile], incs, CcOptions.obj (cfg, outFile), Config.pilcStr config]
        val args = List.concat args
        val cleanup = fn () => if Config.keep (config, "pil") then ()
                               else File.remove inFile
      in (cc, args, cleanup)
      end

  fun linker (config, ld) = 
      (case ld
        of LdGCC    => (Pass.run, Path.fromString "gcc")
         | LdICC    => (Pass.run, Path.fromString "icl")
         | LdOpc    => (Pass.runWithSh, iflcLibExe (config, "pilink"))
         | LdIpc    => (Pass.runWithSh, iflcLibExe (config, "pilink")))
      
  structure LdOptions =
  struct

    fun exe ((config, ld), fname) = 
        (case ld
          of LdGCC    => ["-o"^fname]
           | LdICC    => ["-Fe"^fname]
           | LdOpc    => ["-out:"^fname]
           | LdIpc    => ["-out:"^fname])

    fun libPath ((config, ld), dname) =
        (case ld
          of LdGCC    => "-L" ^ dname
           | LdICC    => "/LIBPATH:" ^ dname
           | LdOpc    => "/LIBPATH:" ^ dname
           | LdIpc    => "/LIBPATH:" ^ dname
        )

    (* Turn a lib filename "lib<name>.a" into "-l<name>". Only works for
     * filename that is a base name, and only for GCC *)
    fun fixGCCLibName s = 
        if (not (String.contains (s, #"\\") orelse String.contains (s, #"/"))) andalso 
           String.hasPrefix (s, { prefix = "lib" }) andalso String.hasSuffix (s, { suffix = ".a" }) 
          then "-l" ^ String.dropPrefix (String.dropSuffix (s, 2), 3) 
          else s

    fun lib ((config, ld), lname) =
        (case ld
          of LdGCC   => fixGCCLibName lname
           | LdICC   => lname
           | LdOpc    => lname
           | LdIpc    => lname
        )

    fun start (config, ld) = 
        (case ld
          of LdIpc => ["-p2c"]
           | _     => [])

    fun link (config, ld) = 
        (case ld
          of LdGCC    => []
           | LdICC    => ["-link"]
           | LdOpc    => []
           | LdIpc    => []
        )

    fun opt (config, ld) = 
        (case ld
          of LdGCC    => ["-O2"]
           | LdICC    => []
           | LdOpc    => []
           | LdIpc    => []
        )

    fun defaultStack (config, ld) = 
        (case ld
          of LdGCC    => if Config.host config = Config.OsLinux then [] (* TODO: find a Linux solution *)
                         else ["--stack="^(defaultStackStr (config, ld))]
           | LdICC    => ["-stack:"^(defaultStackStr (config, ld))]
           | LdOpc    => ["-stack:"^(defaultStackStr (config, ld))]
           | LdIpc    => []
        )

    fun control (config, ld) = 
        (case ld
          of LdGCC    => []
           | LdICC    => ["-nologo", "-INCREMENTAL:NO"]
           | LdOpc    => ["-nologo", "-INCREMENTAL:NO"]
           | LdIpc    => ["-nologo", "-INCREMENTAL:NO"]
        )

    fun debug (config, ld) = 
        (case (ld, Config.pilDebug config)
          of (LdGCC, _)     => ["-g"]
           | (LdICC, true)  => ["-debug", "-NODEFAULTLIB:LIBCMT"] 
           (* The NODEFAULTLIB is a temporary hack because gc-bdwd.lib is pulling in libcmt -leaf *)
           | (LdICC, false) => ["-debug"] 
           | (LdOpc, _)     => ["-debug"]
           | (LdIpc, _)     => ["-debug"]
        )

  end (* structure LdOptions *)

  fun gcLibraries (config, ldTag) = 
      let

        val mt = multiThreaded config
        val gcs = #style (Config.gc config)
        fun agc (config, opc) =
            (case Config.agc config
              of Config.AgcGcMf => ifDebug (config, "gc-mfd.lib", "gc-mf.lib")
               | Config.AgcTgc  => if opc then
                                     ifDebug (config, "gc-tgcd.lib", "gc-tgc.lib")
                                   else
                                     ifDebug (config, "gc-tgcd_pthread.lib", "gc-tgc_pthread.lib")
               | Config.AgcCgc  => if opc then 
                                     ifDebug (config, "gc-cgcd.lib", "gc-cgc.lib")
                                   else
                                     ifDebug (config, "gc-cgcd_pthread.lib", "gc-cgc_pthread.lib"))
        val failPillar = fn () => fail ("gcLibraries", "Conservative GC not supported on Pillar")
        val failC      = fn () => fail ("gcLibraries", "Accurate GC not supported on C")
        val libs =
            (case (gcs, ldTag, mt)
              of (Config.GcsNone,         _,        _    ) => []
               | (Config.GcsConservative, LdGCC,    _    ) => [ifDebug (config, "libgc-bdwd.a", "libgc-bdw.a")]
               | (Config.GcsConservative, LdICC,    true ) => [ifDebug (config, "gc-bdw-dlld.lib", "gc-bdw-dll.lib")]
               | (Config.GcsConservative, LdICC,    false) => [ifDebug (config, "gc-bdwd.lib", "gc-bdw.lib")]
               | (Config.GcsConservative, LdOpc, _    )    => failPillar ()
               | (Config.GcsAccurate,     LdOpc, _    )    => [ifDebug (config, "pgcd.lib", "pgc.lib"), 
                                                               "imagehlp.lib", agc (config, true)]
               | (Config.GcsConservative, LdIpc,    _    ) => failPillar ()
               | (Config.GcsAccurate,     LdIpc,    _    ) => [ifDebug (config, "pgcd_pthread.lib", "pgc_pthread.lib"),
                                                               "imagehlp.lib", agc (config, false)]
               | (Config.GcsAccurate,     _,        _    ) => failC ())

      in libs
      end

  fun futureLibraries (config, ldTag) = 
      let
        val nm =
            if synchronizeThunks config then
              ifDebug (config, "paralleld", "parallel")
            else
              ifDebug (config, "sequentiald", "sequential")

        val gcs =
            (case #style (Config.gc config) 
              of Config.GcsConservative => "bdw_"
               | _                      => "")

        val file = 
            (case ldTag
              of LdGCC    => "libptkfutures_gcc_" ^ gcs ^ nm ^ ".a"
               | LdICC    => "ptkfutures_" ^ gcs ^ nm ^ ".lib"
               | LdOpc    => "ptkfutures_pillar_" ^ nm ^ ".obj"
               | LdIpc    => "ptkfutures_p2c_" ^ nm ^ ".obj")

      in [file]
      end

  fun unmanagedLibraries (config, ldTag) = 
      let
        val libs = 
            (case ldTag
              of LdOpc    => [ifDebug (config, "pillard.lib", "pillar.lib")] 
               | LdIpc    => [ifDebug (config, "pillard_pthread.lib", "pillar_pthread.lib")]
               | LdICC    => ["user32.lib"] 
               | LdGCC    => ["libm.a"])
        val mcrtLib = [ifDebug (config, "mcrtd.lib", "mcrt.lib")]
        val threads =
            (case (ldTag, useFutures config)
              of (LdOpc, _)    => mcrtLib
               | (LdIpc,    _) => [(*XXX temp removed to work with old pilicl "pillar2c_pthread.asm.o", *)"Ws2_32.lib",
                                   ifDebug (config, "pthreadVC2d.lib", "pthreadVC2.lib")]
               | (LdGCC, true) => fail ("unmanagedLibraries", "gcc does not link with mcrt")
               | (_,     true) => mcrtLib
               | _             => [])
        val gmpLibs = 
            if noGMP config then [] else ["libgmp.a", "libgcc.a"]
      in threads @ libs @ gmpLibs
      end

  fun libraries (config, ldTag) = 
      let
        val (prtBegin, prtEnd) = 
            (case ldTag
              of LdOpc    => ([ifDebug (config, "crt_prtbegind.obj", "crt_prtbegin.obj")], 
                              [ifDebug (config, "crt_prtendd.obj", "crt_prtend.obj")])
               | LdIpc    => ([ifDebug (config, "pillar2c_crt_begind.obj", "pillar2c_crt_begin.obj")], 
                              [ifDebug (config, "pillar2c_crt_endd.obj", "pillar2c_crt_end.obj")])
               | _        => ([], []))
        val gcLibs = gcLibraries (config, ldTag)
        val futureLibs = futureLibraries (config, ldTag)
        val unmanagedLibs = unmanagedLibraries (config, ldTag)
        val pre = prtBegin
        val post = List.concat [futureLibs, prtEnd, gcLibs, unmanagedLibs]
      in (pre, post)
      end

  fun link (config, ccTag, ldTag, fname) = 
      let
        val fname = Config.pathToHostString (config, fname)
        val inFile = objectFile (config, ccTag, fname)
        val outFile = exeFile (config, ldTag, fname)
        val cfg = (config, ldTag)
        val ld = linker cfg
        val iflcLibLibs = libDirs (config, ldTag)
        val iflcLibOptions = List.map (iflcLibLibs, fn lib => LdOptions.libPath (cfg, lib))
        val options = List.concat [LdOptions.link cfg,
                                   iflcLibOptions,
                                   LdOptions.opt cfg, 
                                   LdOptions.defaultStack cfg,
                                   LdOptions.control cfg,
                                   LdOptions.debug cfg]
        val (preLibs, postLibs) = libraries (config, ldTag)
        val preLibs = List.map (preLibs, fn l => LdOptions.lib (cfg, l))
        val postLibs = List.map (postLibs, fn l => LdOptions.lib (cfg, l))
        val extraPaths = List.map (Config.linkDirectories config, fn p => LdOptions.libPath (cfg, p))
        val extraLibs = List.map (Config.linkLibraries config, fn l => LdOptions.lib (cfg, l))
        val args = List.concat [LdOptions.exe (cfg, outFile),
                                LdOptions.start cfg,
                                options,
                                preLibs,
                                [inFile],
                                postLibs,
                                extraPaths,
                                extraLibs,
                                Config.linkStr config]
        val cleanup = fn () => if Config.keep (config, "obj") then ()
                               else File.remove inFile
      in (ld, args, cleanup)
      end

  val compile = 
   fn (config : Config.t, fname) =>
      let 

        val ccTag = 
            (case Config.toolset config
              of Config.TsIcc => CcICC
               | Config.TsGcc => CcGCC
               | Config.TsOpc => CcOpc
               | Config.TsIpc => CcIpc)

        val ((run, cmd), args, cleanup) = compile (config, ccTag, fname)

        val () = 
            Exn.finally (fn () => run (config, Chat.log0, cmd, args),
                         cleanup)
      in ()
      end

  val link = 
   fn (config : Config.t, fname) =>
      let 

        val (ccTag, ldTag) = 
            (case (Config.toolset config)
              of Config.TsIcc => (CcICC, LdICC)
               | Config.TsGcc => (CcGCC, LdGCC)
               | Config.TsOpc => (CcOpc, LdOpc)
               | Config.TsIpc => (CcIpc, LdIpc))

        val ((run, cmd), args, cleanup) = link (config, ccTag, ldTag, fname)

        val () = 
            Exn.finally (fn () => run (config, Chat.log0, cmd, args),
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
                      features = [assumeSmallIntsF,
                                  disableTailCallF,
                                  gcWriteBarriersF, 
                                  gcAllBarriersF,
                                  gcIndirectionsF,
                                  gmpUseForcedGcF,
                                  gmpUseGAllocateF,
                                  gmpUseGcMallocF,
                                  gmpUseMallocF,
                                  gmpUsePCDeclF,
                                  gmpUsePinningF,
                                  instrumentAllocationF,
                                  instrumentGCsF,
                                  instrumentVtbAllocationF,
                                  noTaggedIntRecoverF,
                                  noThunkSubsumptionF,
                                  vtableChangeF,
                                  usePortableTaggedIntsF],
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
