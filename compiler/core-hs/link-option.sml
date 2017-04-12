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

signature CORE_HS_LINK_OPTION =
sig
  val pass : (CoreHs.t * StringSet.t * CoreHsParse.pkgMap ref, CoreHs.t * Config.t) Pass.t
end

structure CoreHsLinkOption :> CORE_HS_LINK_OPTION =
struct
  structure P = Process
  structure C = Config
  structure CP = CoreHsParse
  structure SD = StringDict
  structure SS = StringSet
  structure VS = Identifier.VariableSet
  structure VD = Identifier.VariableDict

  val passname = "CoreHsLinkOption"
  structure Chat = ChatF (struct
                            type env = Config.t
                            val extract = fn a => a
                            val name = passname
                            val indent = 2
                          end)

  fun stripLN s = String.dropTrailing (s, #"\n")
  fun stripCR s = String.dropTrailing (s, #"\r")

  val ghcLibRoot = ref NONE
  fun getGhcLibRoot cfg =
      case !ghcLibRoot
        of SOME d => d
         | NONE =>
          let
            fun run (cmd, args) = Pass.runCmd (cmd, args, [], true)
            val d = stripCR (stripLN (run ("ghc", ["--print-libdir"])))
            val () = ghcLibRoot := SOME d
          in
            d
          end

  fun addLinkStr (Config.C cfg, (dirs, libs, opts)) = Config.C
      { agc              = #agc cfg
      , control_         = #control_ cfg
      , debug_           = #debug_ cfg
      , debugLev         = #debugLev cfg
      , feature_         = #feature_ cfg
      , gc               = #gc cfg
      , home             = #home cfg
      , host             = #host cfg
      (* , flrcLibDirectory = #flrcLibDirectory cfg *)
      , flrcOpt          = #flrcOpt cfg
      , keep             = #keep cfg
      , linkStr          = #linkStr cfg @ opts (* TODO: due to platform differences, we ignore extra opts for now *)
      , linkDirectories  = #linkDirectories cfg @ dirs
      , linkLibraries    = #linkLibraries cfg @ List.map (libs, fn s => "lib" ^ s ^ ".a")
      , logLev           = #logLev cfg
      , output           = #output cfg
      , parStyle         = #parStyle cfg
      , passes           = #passes cfg
      , pilcStr          = #pilcStr cfg
      , pilDebug         = #pilDebug cfg
      , pilOpt           = #pilOpt cfg
      , ghcOpt           = #ghcOpt cfg
      , report           = #report cfg
      , runtime          = #runtime cfg
      , sloppyFp         = #sloppyFp cfg
      , stop             = #stop cfg
      , synchThunks      = #synchThunks cfg
      , targetWordSize   = #targetWordSize cfg
      , timeExecution    = #timeExecution cfg
      , vectorConfig     = #vectorConfig cfg
      , warnLev          = #warnLev cfg
      }

  fun joinOptions options =
      let
        val (a, b, c) = Utils.List.unzip3 (List.map (options, fn { dirs, libs, opts } => (dirs, libs, opts)))
      in
        (List.concat a, List.concat b, List.concat c)
      end

  fun linearizeOpts (pkgs : CP.pkgMap) : CoreHsParse.options list =
      if SD.isEmpty pkgs then []
        else
          let
            val (ns, opts) = List.unzip (SD.fold (pkgs, [], fn (name, { depends, options, ... }, ns) =>
                               if SS.isEmpty depends then (name, options) :: ns else ns))
            val ns = SS.fromList ns
            val pkgs = SD.keepAllMap (pkgs, fn (_, x as { version, depends, options }) =>
                         if SS.isEmpty depends then NONE
                           else SOME { version = version,
                                       depends = SS.difference (depends, ns),
                                       options = options })
          in
            if SS.isEmpty ns andalso not (SD.isEmpty pkgs)
              then Fail.fail (passname, "linearizeOpts", "circular GHC package dependency detected: " ^
                     List.toString (fn (p, { depends, options, ... }) =>
                       ("(" ^ p ^ ", { " ^ List.toString (fn s => s) (SS.toList depends) ^ " })"))
                       (SD.toList pkgs))
              else opts @ linearizeOpts pkgs
          end

  fun getGhcPkgAll (cfg, cache, name, pkgs)
    = let
        val pkg as { depends, ... } = CP.getGhcPkg (cfg, cache, name)
        val pkgs = SD.insert (pkgs, name, pkg)
        val depends = SS.keepAll (depends, fn p => not (SD.contains (pkgs, p)))
      in
        if SS.isEmpty depends then pkgs
          else List.fold (SS.toList depends, pkgs,
                 fn (name, pkgs) => getGhcPkgAll (cfg, cache, name, pkgs))
      end


  fun linkOption ((prog, pkgs, cache),  pd) =
      let
        val cfg = PassData.getConfig pd
        val pkgs = SS.remove (pkgs, "")
        val pkgs = SS.remove (pkgs, "main")
        val pkgs = SS.remove (pkgs, "integer-simple") (* remove this one with no version number *)
        val pkgs = SS.insert (pkgs, "base") (* base will re-introduce integer-simple (versioned) as a dependency *)
        val pkgs = List.fold (SS.toList pkgs, SD.empty, fn (pkg, m) => getGhcPkgAll (cfg, cache, pkg, m))
        val baseVer = case SD.lookup (pkgs, "base")
                        of SOME { version, ... } => version
                         | NONE => ""
        val pkgs = case SD.lookup (pkgs, "base-" ^ baseVer)
                     of SOME _ => SD.remove (pkgs, "base") (* drop this one with no version number *)
                      | NONE   => pkgs
        val () = Chat.log1 (cfg, "package list: " ^ Layout.toString (List.layout String.layout (SD.domain pkgs)))
        val (dirs, libs, opts) = joinOptions (List.rev (linearizeOpts pkgs))
        fun addMinGWDir (libs, dirs)
          = let
              val libRoot = Path.dropLast (Path.fromWindowsString (getGhcLibRoot cfg))
              val minGW   = Path.snoc (libRoot, "mingw")
              val minGWLib = Path.snoc (minGW, "lib")
              val dirs = Path.toWindowsString minGWLib :: dirs
              val binDir = Config.pathToHostString (cfg, Path.snoc (minGW, "bin"))
              val () = Chat.log1 (cfg, "run: gcc --print-search-dirs in " ^ binDir)
              val gccDirs = Pass.runCmd ("gcc", ["--print-search-dirs"], [binDir], true)
              val gccLib = case String.split (gccDirs, #"\n")
                             of d::_ =>
                               let
                                 val d = String.dropPrefix (stripCR d, 9)
                                 val () = Chat.log2 (cfg, "got libgcc.a path: " ^ d)
                               in
                                 SOME d
                               end
                              | _ => NONE
            in
              case gccLib
                of SOME p => (libs @ ["gcc"], dirs @ [p])
                 | _ => (libs, dirs)
            end
        val (libs, dirs) = case C.host cfg
                     of C.OsCygwin => addMinGWDir (libs, dirs)
                      | C.OsMinGW  => addMinGWDir (libs, dirs)
                      | _          => (libs, dirs)
        val ghc76 = case String.explode baseVer
                      of (#"4" :: #"." :: #"6" :: _) => true
                       | _ => false
        val libs = if ghc76 then libs
                   else List.map (libs, fn s => if String.hasSuffix (s, { suffix = "_hrc" })
                                                  then String.substring2 (s, { start = 0, finish = String.length s - 4})
                                                  else s)
      in
        (prog, addLinkStr (cfg, (dirs, libs, opts)))
      end

      (*
  val linkOption =
   fn ((p, pso), pd) =>
      case pso
       of NONE => (p, PassData.getConfig pd)
        | SOME ps => linkOption ((p, ps), pd)
        *)

  fun layout  (module, config) = CoreHsLayout.layoutModule (config, #1 module)
  fun layout' (module, config) = CoreHsLayout.layoutModule (config, #1 module)

  val description = {name        = passname,
                     description = "Insert Extra Options to Link to GHC Library",
                     inIr        = { printer = layout, stater = layout },
                     outIr       = { printer = layout', stater = layout' },
                     mustBeAfter = [],
                     stats       = []}

  val associates = {controls = [], debugs = [], features = [], subPasses = []}

  val pass = Pass.mkCompulsoryPass (description, associates, linkOption)

end
