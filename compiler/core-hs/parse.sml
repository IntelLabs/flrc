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


signature CORE_HS_PARSE =
sig
  type options = { dirs : string list, libs : string list, opts : string list }
  type ghcPkg = { version : string, depends : StringSet.t, options : options }
  type pkgMap = ghcPkg StringDict.t
  val getGhcPkg : Config.t * pkgMap ref * string -> ghcPkg
  val pass : (unit, CoreHs.t * StringSet.t * pkgMap ref) Pass.t
  val parseFile : string * Config.t -> CoreHs.t
  val noMainWrapper : Config.t -> bool
end

structure CoreHsParse :> CORE_HS_PARSE =
struct
  val passname = "CoreHsParse"
  fun failMsg (f, m) = Fail.fail (passname, f, m)

  val (noMainWrapperF, noMainWrapper) =
      Config.Feature.mk (passname ^ ":noMainWrapper", "do not generate the usual GHC main wrapper")

  structure Chat = ChatF (struct
                            type env = Config.t
                            val extract = fn a => a
                            val name = passname
                            val indent = 2
                          end)
  structure TextIO = Pervasive.TextIO
  structure CH = CoreHs
  structure CHU = CoreHsUtils
  structure CHP = CoreHsPrims
  structure IM = Identifier.Manager
  structure U  = Utils
  structure UF = Utils.Function
  structure UO = Utils.Option
  structure SS = StringSet
  structure MS = SetF (struct type t = CH.anMName val compare = CHU.compareAnMName end)
  structure MD = DictF (struct type t = CH.anMName' val compare = CHU.compareAnMName' end)
  structure QS = SetF (struct type t = CH.identifier CH.qualified val compare = CHU.compareQName end)
  structure QD = DictF (struct type t = CH.identifier CH.qualified val compare = CHU.compareQName end)
  structure SD = StringDict
  structure QTS = TopoSortF (struct structure Dict = QD structure Set = QS end)

  structure Debug =
  struct
    val (debugPassD, debugPass) = Config.Debug.mk (passname, "debug CoreHs parser")
    fun debug   (config, s) = if Config.debug andalso debugPass config then print (s ^ "\n") else ()
  end

  (* routines for ML-Yacc parser *)
  structure CoreHsLrVals =
    CoreHsYaccLrValsFun(structure Token = LrParser.Token)

  structure CoreHsLex =
    CoreHsLexFun(structure Tokens = CoreHsLrVals.Tokens)

  structure CoreHsYacc =
    Join(structure LrParser = LrParser
	 structure ParserData = CoreHsLrVals.ParserData
	 structure Lex = CoreHsLex)

  fun yaccParse (filename, strm, strmWithPos) =
      let val strmRef = ref strm
          val () = CoreHsLex.UserDeclarations.pos := 0
          val lexer = CoreHsYacc.makeLexer (fn _ =>
                let
                  val s = TextIO.StreamIO.inputLine (!strmRef)
                in case s
                     of SOME (s, strm) => (s before strmRef := strm)
                      | NONE => ""
                end)
          fun invoke lexstream =
              let fun print_error (s,i:int,_) =
                      failMsg ("yaccParse:", filename ^ " line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
               in CoreHsYacc.parse (15, lexstream, print_error, ())
              end

	  val dummyEOF = CoreHsLrVals.Tokens.EOF(0,0)
	  fun loop lexer =
	      let val (result,lexer) = invoke lexer
		  val (nextToken,lexer) = CoreHsYacc.Stream.get lexer
	          (* if CoreHsYacc.sameToken(nextToken,dummyEOF) *)
              in
	        case result
	          of SOME r => SOME (strmWithPos, r)
	           | NONE   => NONE
	      end
       in loop lexer
      end

  fun parseFile (f : string, config:Config.t) : CH.t =
      let
        val strm = TextIO.openIn f
        val instrm = TextIO.getInstream strm
        val instrmWithPos = InStreamWithPos.mk instrm
        val result = yaccParse (f, instrm, instrmWithPos)
        val () = TextIO.closeIn strm
      in
        case result
          of SOME (_, prog) => prog
           | NONE => failMsg (f, "parse GHC Core file failed!")
      end

  fun idToName s =
      if String.hasPrefix (s, { prefix = "builtin_" })
        then String.substring2 (s, { start = 8, finish = String.length s })
        else case List.rev (String.split (s, #"-"))
               of x :: xs => String.concatWith (List.rev xs, "-")
                | _       => Fail.fail ("HsToMilUtils", "idToName", "cannot parse package ID: " ^ s)

  type options = { dirs : string list, libs : string list, opts : string list }
  type ghcPkg = { version : string, depends : SS.t, options : options }
  type pkgMap = ghcPkg SD.t

  val version = "version"
  val libdirs = "library-dirs"
  val hslibs  = "hs-libraries"
  val extralibs = "extra-libraries"
  val ldopts  = "ld-options"

  val fields = [ version, libdirs, hslibs, extralibs, ldopts ]

  fun words s = List.keepAll (String.split (s, #" "), not o String.isEmpty)

  fun cleanPath s =
      let
        val s = String.deleteSurroundingWhitespace s
        val s = if String.length s >= 2 andalso
                   String.sub(s, 0) = #"\"" andalso
                   String.last s = #"\""
                  then String.substituteAll (String.dropFirst (String.dropLast s),
                         { substring = "\\\\", replacement = "\\" })
                  else s
      in
        s
      end

  fun runGhcPkg (cfg : Config.t, pkgName : string) : ghcPkg =
      let
        val prog = "ghc-pkg"
        val args = ["field", pkgName, String.concatWith ("depends" :: fields, ",")]
        val () = Chat.log1 (cfg, "run: " ^ String.concatWith (prog :: args, " "))
        fun run (cmd, args) = Pass.runCmd (cmd, args, [], true)
        val out = run (prog, args)
        val ()  = Chat.log2 (cfg, "got:\n" ^ out)
        fun split l = if String.length l = 0 orelse String.sub(l, 0) = #" " then NONE
                         else case String.split (l, #":")
                                of k::v::vs => SOME (k, String.concatWith (v::vs, ":"))
                                |  _ => NONE
        fun parseLine (l, (m, state)) =
            let
              fun stripCR s = String.dropTrailing (s, #"\r")
              val l = stripCR l
            in
              case (split l, state)
                of (SOME (k, v), SOME (kk, vv)) => (SD.insert (m, kk, vv), SOME (k, v))
                |  (SOME (k, v), NONE)          => (m, SOME (k, v))
                |  (NONE,        SOME (kk, vv)) => (m, SOME (kk, vv ^ l))
                |  (NONE,        NONE)          => (m, NONE)
            end
        fun parse s = List.fold (String.split (s, #"\n"), (SD.empty, NONE), parseLine)
        val m = case parse out
                  of (m, SOME (kk, vv)) => SD.insert (m, kk, vv)
                   | (m, NONE) => m
        fun lookup' g (m, f) =
            case SD.lookup (m, f)
              of SOME s => g s
               | NONE   => []
        val lookup = lookup' words
        val dirs = lookup' (fn x => [cleanPath x]) (m, libdirs)
        val libs = List.map (lookup (m, hslibs), fn s => s ^ "_hrc") @ lookup (m, extralibs)
        val opts = lookup (m, ldopts)
        (* To stay clean, we skip the rts package! *)
        fun insertName (i, ss) = let val name = idToName i in if name = "rts" then ss else SS.insert (ss, name) end
        val depends = case SD.lookup (m, "depends")
                        of SOME s => List.fold (words s, SS.empty, insertName)
                         | NONE   => SS.empty
        val version = case SD.lookup (m, "version")
                        of SOME s => String.deleteSurroundingWhitespace s
                         | NONE   => ""
        val depends = SS.keepAll (depends, fn p => not (String.hasPrefix (p, { prefix = "ghc-prim" })))
        val pkg = { version = version, depends = depends, options = { dirs = dirs, libs = libs, opts = opts } }
        val () = Chat.log2 (cfg, "dirs: " ^ Layout.toString (List.layout Layout.str dirs))
        val () = Chat.log2 (cfg, "libs: " ^ Layout.toString (List.layout Layout.str libs))
        val () = Chat.log2 (cfg, "opts: " ^ Layout.toString (List.layout Layout.str opts))
      in
        pkg
      end

  fun getGhcPkg (cfg, cache, pkgName) =
      case SD.lookup (!cache, pkgName)
        of SOME pkg => pkg
         | NONE =>
          let
            val pkg = runGhcPkg (cfg, pkgName)
            val version = #version pkg
            val (short, full) =
                 if String.hasSuffix (pkgName, { suffix = version })
                   then (String.dropSuffix (pkgName, String.length version + 1),
                         pkgName)
                   else (pkgName, pkgName ^ "-" ^ version)
            (* insert both short and full names *)
            val _ = cache := SD.insert (SD.insert (!cache, short, pkg), full, pkg)
          in
            pkg
          end

  fun foldL (l, m, f) = List.foldr (l, ([], m), fn (x, (xs, m)) =>
                          let val (x, m) = f (x, m) in (x :: xs, m) end)

  fun foldL' (l, (e, m), f) = List.foldr (l, ([], e, m), fn (x, (xs, e, m)) =>
                          let val (x, e, m) = f e (x, m) in (x :: xs, e, m) end)

  fun mNameToPath (CH.M (CH.P pname, prefix, name)) =
      let
        val d = CHU.zDecodeString pname
        val prefix = List.map (prefix, CHU.zDecodeString)
        val name = CHU.zDecodeString name
        (* look for main package in current directory *)
        val p = Path.fromString "."
        val p = List.fold (prefix, p, Utils.Function.flipIn Path.snoc)
        val p = Path.snoc (p, name)
      in
        (d, p)
      end


  (*
   * A definition is either for a value or a type.
   *
   * The DCon is just a place holder for data constructors
   * since they are defined as part of a type definition.
   *)
  datatype definition = VDef of CH.vDef | TDef of CH.tDef | DCon

  (*
   * A defDict is a mapping between names and the definition they represent,
   * as well as a set of names that they depend on. Alternatively we can
   * think of it as a dependency graph between definitions.
   *)
  type defDict = (definition * QS.t) QD.t

  (*
   * Given a program , return a mapping from qualified names to their
   * definitions (either value or type) as well as the set of names
   * they depend on.
   *)
  val scanModule : CH.module -> defDict =
    fn (CH.Module (mname as (CH.M (CH.P pName, _, _)), tdefs, vdefgs)) =>
      let

        fun scanQName ((NONE, x), m) = scanQName ((SOME mname, x), m)
          | scanQName (y, m) = (y, QS.insert (m, y))

        fun scanTy (x as (CH.Tcon name), m) =
            if name = CHU.tcArrow then (x, m)
              else let
                     val (name, m) = scanQName (name, m)
                   in
                     (CH.Tcon name, m)
                   end
          | scanTy (CH.Tapp (t1, t2), m) =
            let
              val (t1, m) = scanTy (t1, m)
              val (t2, m) = scanTy (t2, m)
            in
              (CH.Tapp (t1, t2), m)
            end
          | scanTy (CH.Tforall (b, t), m) =
            let
              val (t, m) = scanTy (t, m)
            in
              (CH.Tforall (b, t), m)
            end
          | scanTy (CH.TransCoercion (t1, t2), m) =
            let
              val (t1, m) = scanTy (t1, m)
              val (t2, m) = scanTy (t2, m)
            in
              (CH.TransCoercion (t1, t2), m)
            end
          | scanTy (CH.SymCoercion t, m) =
            let
              val (t, m) = scanTy (t, m)
            in
              (CH.SymCoercion t, m)
            end
          | scanTy (CH.UnsafeCoercion (t1, t2), m) =
            let
              val (t1, m) = scanTy (t1, m)
              val (t2, m) = scanTy (t2, m)
            in
              (CH.UnsafeCoercion (t1, t2), m)
            end
          | scanTy (CH.InstCoercion (t1, t2), m)=
            let
              val (t1, m) = scanTy (t1, m)
              val (t2, m) = scanTy (t2, m)
            in
              (CH.InstCoercion (t1, t2), m)
            end
          | scanTy (CH.LeftCoercion t, m) =
            let
              val (t, m) = scanTy (t, m)
            in
              (CH.LeftCoercion t, m)
            end
          | scanTy (CH.RightCoercion t, m) =
            let
              val (t, m) = scanTy (t, m)
            in
              (CH.RightCoercion t, m)
            end
          | scanTy (CH.NthCoercion (i, t), m) =
            let
              val (t, m) = scanTy (t, m)
            in
              (CH.NthCoercion (i, t), m)
            end
          | scanTy (x, m) = (x, m)

        fun scanTBind env ((v, k), m) =
            let
              val env = QS.insert (env, (NONE, v))
            in
              ((v, k), env, m)
            end

        fun scanVBind env ((v, t), m) =
            let
              val (t, m) = scanTy (t, m)
              val env = QS.insert (env, (NONE, v))
            in
              ((v, t), env, m)
            end

        fun scanBind env (CH.Vb vb, m) =
            let
              val (vb, env, m) = scanVBind env (vb, m)
            in
              (CH.Vb vb, env, m)
            end
          | scanBind env (CH.Tb tb, m) =
            let
              val (tb, env, m) = scanTBind env (tb, m)
            in
              (CH.Tb tb, env, m)
            end

         fun scanTBinds env (tbs, m) = foldL' (tbs, (env, m), scanTBind)

         fun scanVBinds env (vbs, m) = foldL' (vbs, (env, m), scanVBind)

         fun scanTys (tys, m) = foldL (tys, m, scanTy)

        fun scanCDef (CH.Constr ((q, n), tbs, tys), m) =
            let
              val q = case q of NONE => SOME mname | _ => q
              val (tys, sts) = List.unzip tys
              val (tys, m) = scanTys (tys, m)
            in
              (CH.Constr ((q, n), tbs, List.zip (tys, sts)), m)
            end

        fun scanCDefs (cdefs, m) = foldL (cdefs, m, scanCDef)

        fun scanTDef (CH.Data (tname, tbinds, cdefs), m) =
            let
              val (tname, m) = scanQName (tname, m)
              val (cdefs, m) = scanCDefs (cdefs, m)
            in
              (CH.Data (tname, tbinds, cdefs), m)
            end
          | scanTDef (CH.Newtype (tname1, tname2, tbinds, ty), m) =
            let
              val (tname1, m) = scanQName (tname1, m)
              val (tname2, m) = scanQName (tname2, m)
              val (ty, m) = scanTy (ty, m)
            in
              (CH.Newtype (tname1, tname2, tbinds, ty), m)
            end

        fun scanVDefg env (CH.Rec vdefs, m) =
            let
              val env = List.fold (vdefs, env, fn (CH.Vdef (v, _, _), env) => QS.insert (env, v))
              val (vdefs, m) = foldL (vdefs, m, scanVDef env)
            in
              (CH.Rec vdefs, env, m)
            end
          | scanVDefg env (CH.Nonrec vdef, m) =
            let
              val (vdef as (CH.Vdef (v, _, _)), m) = scanVDef env (vdef, m)
              val env = QS.insert (env, v)
            in
              (CH.Nonrec vdef, env, m)
            end

        and scanVDef env (CH.Vdef (v, t, e), m) =
            let
              val (t, m) = scanTy (t, m)
              val (e, m) = scanExp env (e, m)
            in
              (CH.Vdef (v, t, e), m)
            end

        and scanExp env (CH.Var name, m) =
            if QS.member (env, name)
              then (CH.Var name, m)
              else let
                     val (name, m) = scanQName (name, m)
                   in
                     (CH.Var name, m)
                   end
          | scanExp env (CH.Dcon name, m) =
            let
              val (name, m) = scanQName (name, m)
            in
              (CH.Dcon name, m)
            end
          | scanExp env (CH.Lit l, m) =
            let
              val (l, m) = scanLit (l, m)
            in
              (CH.Lit l, m)
            end
          | scanExp env (CH.App (f, e), m) =
            let
              val (f, m) = scanExp env (f, m)
              val (e, m) = scanExp env (e, m)
            in
              (CH.App (f, e), m)
            end
          | scanExp env (CH.Appt (e, t), m) =
            let
              val (e, m) = scanExp env (e, m)
              val (t, m) = scanTy (t, m)
            in
              (CH.Appt (e, t), m)
            end
          | scanExp env (CH.Lam (b, e), m) =
            let
              val (b, env, m) = scanBind env (b, m)
              val (e, m) = scanExp env (e, m)
            in
              (CH.Lam (b, e), m)
            end
          | scanExp env (CH.Let (vdefg, e), m) =
            let
              val (vdefg, env, m) = scanVDefg env (vdefg, m)
              val (e, m) = scanExp env (e, m)
            in
              (CH.Let (vdefg, e), m)
            end
          | scanExp env (CH.Case (e, vb, t, alts), m) =
            let
              val (e, m) = scanExp env (e, m)
              val (vb, env, m) = scanVBind env (vb, m)
              val (t, m) = scanTy (t, m)
              val (alts, m) = scanAlts env (alts, m)
            in
              (CH.Case (e, vb, t, alts), m)
            end
          | scanExp env (CH.Cast (e, t), m) =
            let
              val (e, m) = scanExp env (e, m)
              val (t, m) = scanTy (t, m)
            in
              (CH.Cast (e, t), m)
            end
          | scanExp env (CH.Note (s, e), m) =
            let
              val (e, m) = scanExp env (e, m)
            in
              (CH.Note (s, e), m)
            end
          | scanExp env (CH.External (_, c, s, t), m) =
            let
              val (t, m) = scanTy (t, m)
            in
              (* TODO: pName here is imprecise because this external call might be a
               * result of inlining, so this is only a best effort guess. *)
              (CH.External (pName, c, s, t), m)
            end


        and scanLit (CH.Literal (v, t), m) =
            let
              val (t, m) = scanTy (t, m)
            in
              (CH.Literal (v, t), m)
            end

        and scanAlt env (CH.Acon (name, tbs, vbs, e), m) =
            let
              val (name, m) = scanQName (name, m)
              val (tbs, env, m) = scanTBinds env (tbs, m)
              val (vbs, env, m) = scanVBinds env (vbs, m)
              val (e, m) = scanExp env (e, m)
            in
              (CH.Acon (name, tbs, vbs, e), m)
            end
          | scanAlt env (CH.Alit (l, e), m) =
            let
              val (l, m) = scanLit (l, m)
              val (e, m) = scanExp env (e, m)
            in
              (CH.Alit (l, e), m)
            end
          | scanAlt env (CH.Adefault e, m) =
            let
              val (e, m) = scanExp env (e, m)
            in
              (CH.Adefault e, m)
            end

        and scanAlts env (alts, m) = foldL (alts, m, scanAlt env)

        fun fromVDefg (CH.Rec defs) = defs
          | fromVDefg (CH.Nonrec def) = [def]

        fun fromVDefgs defgs = List.concat (List.map (defgs, fromVDefg))

        fun qualify (CH.Vdef ((p, n), t, e)) = CH.Vdef ((case p of NONE => SOME mname | _ => p, n), t, e)

        val vdefs = List.map (fromVDefgs vdefgs, fn d => scanVDef QS.empty (qualify d, QS.empty))
        val tdefs = List.map (tdefs, fn d => scanTDef (d, QS.empty))
        val defd  = List.fold (vdefs, QD.empty, fn ((x as CH.Vdef (n, _, _), m), d) => QD.insert (d, n, (VDef x, m)))
        val defd  = List.fold (tdefs, defd,
                        fn ((x as CH.Data (n as (p, q), _, cdefs), m), d) =>
                            let val n' = (p, q ^ "_")
                            in
                            List.fold (cdefs, QD.insert (d, n', (TDef x, m)),
                                fn (CH.Constr (c, _, _), d) => QD.insert (d, c,
                                  (DCon, QS.singleton n')))
                            end
                         | ((x as CH.Newtype (n as (p, q), c, _, _), m), d) =>
                            let val n' = (p, q ^ "_")
                            in
                            QD.insert (QD.insert (d, n', (TDef x, m)), c, (DCon,
                            QS.singleton n'))
                            end)
      in
        defd
      end

  (* linearize turns the dependency graph of value definitions into a linear list.
   *
   * Note that the result is not the tightest grouping, we might ended up with
   * group two independent recursive groups into the same letrec.
   *)
  fun linearize (defdict : (CH.vDef * QS.t) QD.t) : CH.vDefg list =
      let
        val toDefg
          = fn [] => failMsg ("linearize", "impossible: topo-sorted component can not be empty")
             | [(n, (def, s))] => if QS.member (s, n) then CH.Rec [def] else CH.Nonrec def
             | defs  => CH.Rec (List.map (defs, #1 o #2))
        val sorted = QTS.sort (QD.toList defdict, #2 o #2)
      in
        List.map (sorted, toDefg)
      end

  fun readModule ((), pd, basename) =
      let
        val cache = ref SD.empty              (* cache for all pkgMap data *)
        val config = PassData.getConfig pd
        fun debug s = Debug.debug (config, s)
        fun verbose s = Chat.log1 (config, s)
        val stats = ref SD.empty
        fun lookupStat n = case SD.lookup (!stats, n) of SOME t => t | NONE => Time.zero
        val time =
         fn name =>
         fn f =>
         fn a =>
            let
              val s = Time.now ()
              val r = f a
              val e = Time.now ()
              val acc = lookupStat name
              val () = stats := SD.insert (!stats, name, Time.+(acc, Time.-(e, s)))
            in r
            end
        fun chatStats () =
            let
              fun chat n = verbose ("  " ^ n ^ "\t" ^ Time.toString (lookupStat n) ^ "s")
              val () = verbose "time spent in "
              val () = chat "parsing"
              val () = chat "scanning"
              val () = chat "linearizing"
            in
              ()
            end
        val odir = ref ""
        val _ = List.map (Config.ghcOpt config, fn s =>
                    if String.hasPrefix (s, { prefix = "-odir " })
                      then odir := String.substring2 (s, { start = 6, finish = String.length s})
                      else ())
        val opath = if (!odir) = "" then Path.fromString "." else Path.fromString (!odir)
        val () = debug ("odir = " ^ !odir ^ " opath = " ^ Config.pathToHostString (config, opath))
        val basename = Config.pathToHostString (config, basename)
        val infile = if (!odir) = "" then basename ^ ".hcr"
                        else Config.pathToHostString (config, Path.snoc(opath, "Main.hcr"))
        val pkgs = ref SS.empty

        fun readOne (mname : CH.anMName, defd : defDict, scanned : MS.t) =
            if MS.member (scanned, mname)
              then (defd, scanned)
              else
                let
                  val (pname, path) = mNameToPath mname
                  val () = pkgs := SS.insert (!pkgs, pname)
                  val hcrRoot =
                      case pname
                        of "main" => opath
                         | _ => (case #dirs (#options (getGhcPkg (config, cache, pname)))
                           of [p] => Path.fromString p
                            | ps => failMsg ("readModule",
                                             "invalid lib path returned by ghc-pkg " ^
                                             Layout.toString (List.layout String.layout ps)))
                  val () = debug ("hcrRoot = " ^ Config.pathToHostString (config, hcrRoot))
                  (* val f1 = Config.pathToHostString (config, path) ^ ".hcr" *)
                  val path = Path.append (hcrRoot, path)
                  val file = Config.pathToHostString (config, path) ^ ".hcr"
                  val () = verbose ("parse " ^ Layout.toString (CoreHsLayout.layoutAnMName (config, mname)) ^ " from " ^ file)
                  fun scan module =
                      let
                        val defd' = time "scanning" scanModule module
                        val scanned = MS.insert (scanned, mname)
                        val defd  = QD.union (defd, defd', #2)
                      in
                        (defd, scanned)
                      end
                in
                  if File.doesExist file
                    then scan (time "parsing" parseFile (file, config))
                    else failMsg ("readModule", "file " ^ file ^ " is not found")
                end

        fun traceDef (name, def as (_, depends), state as (defd, traced, scanned)) =
            case QD.lookup (traced, name)
              of SOME _ => state
               | NONE   =>
                let
                  val () = debug ("traceDef: " ^ CoreHsLayout.qNameToString name ^ " => " ^
                             QS.fold (depends, "", fn (n, s) => CoreHsLayout.qNameToString n ^ " " ^ s))
                  val traced = QD.insert (traced, name, def)
                  fun trace (name as (SOME m, n), state as (defd, traced, scanned)) =
                      if m = CHU.primMname then state
                        else
                          let
                            val (defd, scanned) = readOne (m, defd, scanned)
                            val name' = (SOME m, n ^ "_")
                          in
                            case QD.lookup (defd, name)
                              of SOME def => traceDef (name, def, (defd, traced, scanned))
                               | NONE => (case QD.lookup (defd, name')
                                 of SOME def => traceDef (name', def, (defd, traced, scanned))
                                  | NONE =>
                                    (* Some types have no type constructors so they are not in ExtCore *)
                                    if not (String.isEmpty n) andalso (Char.isUpper (String.sub (n, 0)))
                                      then state
                                      else failMsg ("traceDef", CoreHsLayout.qNameToString name ^ " not found"))
                          end
                    | trace ((NONE,   _), state) = state

                in
                  QS.fold (depends, (defd, traced, scanned), trace)
                end

        fun readAll (mname, defd, scanned, mainVar) =
            case QD.lookup (defd, mainVar)
             of NONE => Fail.fail (passname, "readModule", "main program not found")
              | SOME def =>
                let
                  val (_, traced, _) = traceDef (mainVar, def, (defd, QD.empty, scanned))
                  val () = debug ("traced = " ^ Layout.toString (Layout.sequence ("{", "}", ",")
                                  (List.map (QD.domain traced, fn n => CoreHsLayout.layoutQName (config, n)))))
                  val (tdefs, vdefs) = QD.fold (traced, ([], []), fn (_, (TDef d, _), (ts, vs)) => (d :: ts, vs)
                                                                   | (n, (VDef d, s), (ts, vs)) => (ts, (n, (d, s)) :: vs)
                                                                   | (_, _, s) => s)
                  val names = QS.keepAll (List.fold (vdefs, QS.empty, fn ((_, (_, p)), q) => QS.union (p, q)),
                                       fn (m, _) => m = SOME CHU.primMname)
                  val () = debug ("GHCH.Prims needed " ^ QS.fold (names, "",
                             fn (n, s) => s ^ "\n    " ^ CoreHsLayout.qNameToString n))
                  val vdefgs = time "linearizing" linearize (QD.fromList vdefs)
                in
                  CH.Module (mname, tdefs, vdefgs)
                end

        fun cleanup () =
            if Config.keep (config, "hcr") then ()
            else File.remove infile

        fun process () =
            let
              val module as (CH.Module (mname, _, _)) = time "parsing" parseFile (infile, config)
              val defd = time "scanning" scanModule module
              val scanned = MS.fromList [mname, CHU.primMname]
              val mainVar = if noMainWrapper config then CHU.mainVar else CHU.wrapperMainVar
              val m = readAll (mname, defd, scanned, mainVar)
              val () = chatStats ()
            in (m, !pkgs, cache)
            end
      in
        Exn.finally (process, cleanup)
      end

  fun layout ((module, _, _), cfg) = CoreHsLayout.layoutModule (cfg, module)

  val description : (unit, CoreHs.t * StringSet.t * pkgMap ref) Pass.description
    = {name        = passname,
                     description = "Parser for GHC core",
                     inIr        = Pass.unitHelpers,
                     outIr       = { printer = layout,
                                     stater  = layout},
                     mustBeAfter = [],
                     stats       = []}

  val associates = {controls = [], debugs = [Debug.debugPassD], features = [noMainWrapperF], subPasses = []}

  val pass = Pass.mkFilePass (description, associates, readModule)

end
