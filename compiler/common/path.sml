(* The Intel FL to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* Utilities for dealing with different os path syntax *)

(* This is a simplified model of the OS path syntax that is 
 * intended to play nice with cygwin, and to be simpler to 
 * use and reason about.  It doesn't not support the full
 * generality of the Basis OS.Path facility (but does
 * deal with cygwin paths, unlike OS.Path).
 * 
 * The model of a path is as a volume and a series of links.
 * 
 * Volumes may be a drive letter (e.g. c) or they may be the
 * the special root volume.  
 *
 * An absolute path is one which has a volume.
 *
 * We deviate from the standard path model in that
 * we allow mixed seperators (that is, / \ in the same path).
 * This means that some valid (but odd) paths will be incorrectly
 * parsed (e.g. weird\file is a valid unix file name, but will be
 * treated as a path).
 *
 * Paths may be reifed as MingGW, Windows, Cygwin, or Unix path
 * strings.  Relative paths are valid for all three.  
 * An absolute path is valid for:
 *   Windows always
 *   Unix if the volume is root
 *   Cygwin always
 *   MinGW always
 * 
 * Examples:
 *   If volume is C and the links are ["documents and settings", "lpeterse"], 
 *    then the reifications are:
 *    Windows:  C:\documents and settings\lpeterse
 *    Unix: error
 *    Cygwin: /cygdrive/c/documents and settings/lpeterse
 *    MinGW: /c/documents and settings/lpeterse
 *
 *   If volume is root and the links are ["usr", "lib"],
 *    then the reifications are:
 *    Windows: \usr\lib
 *    Unix: /usr/lib
 *    Cygwin: /usr/lib
 *    MinGW: /usr/lib
 * 
 *)

signature PATH = 
sig
  type t 
  datatype volume = Root | Drive of char
  (* fromString tries to parse a path from the string.  This is necessarily heuristic.
   * For example, "C:\Documents and Settings" is technically a valid unix path (the relative
   * path designating a file of that name), and is also a valid windows absolute path.
   * The heuristic is simply to first try to parse the string as a windows path.  If this fails,
   * we try to parse it as a unix style path.  If the leading directories of this path
   * are /cygdrive/X for some character X, we treat X as the volume.  Otherwise we treat
   * it as a rooted path.
   *)
  val fromString : string -> t
  val fromCygwinString : string -> t
  val fromMinGWString : string -> t
  val fromUnixString : string -> t
  val fromWindowsString : string -> t
  val toCygwinString : t -> string
  val toWindowsString : t -> string
  val toUnixString : t -> string
  val toMinGWString : t -> string
  val layout : t -> Layout.t
  (* cons adds to the head (the root end). Path must be relative *)
  val cons : string * t -> t
  (* snoc adds to the tail (the child end). *) 
  val snoc : t * string -> t
  (* second path must be relative *)
  val append : t * t -> t
  val setVolume : t * volume -> t
  val removeVolume : t -> t
  val dropLast : t -> t
end

structure Path :> PATH =
struct

  datatype volume = Root | Drive of char

  (* arcs are in reverse order *)
  datatype t = PRel of string list
             | PAbs of volume * (string list)


  structure SP = StringParser
  val || = SP.||
  val && = SP.&&

  infix || &&
  val isChar = fn c => SP.satisfy (fn c' => c = c')
  val alpha = SP.satisfy Char.isAlpha
  val ::: = fn (a, b) => SP.map (a && b, fn (a, b) => a ::b)
  val -&& = fn (a, b) => SP.map (a && b, fn (a, b) => b)
  val &&- = fn (a, b) => SP.map (a && b, fn (a, b) => a)
  infixr 5 :::
  infix -&& &&-

  val parseRelative : (string SP.t * char SP.t) -> string List.t SP.t = 
   fn (filename, sep) => 
      let
        val eof = SP.map (SP.atEnd, fn () => [])
        val rec relative = 
         fn () => (filename ::: (sep -&& (SP.$ relative))) ||
                  (filename ::: (sep -&& eof)) ||
                  (filename ::: eof)
        val p = SP.map (SP.$ relative, List.rev)
      in p
      end

  val parseWindowsStyle : t StringParser.t = 
      let
        val invalidFileChar = 
         fn c => (Char.ord c < 32) orelse
                 (c = #"<") orelse
                 (c = #">") orelse
                 (c = #":") orelse
                 (c = #"\"") orelse
                 (c = #"/") orelse
                 (c = #"\\") orelse
                 (c = #"|") orelse
                 (c = #"?") orelse
                 (c = #"*")

        val validFileChar = not o invalidFileChar
        val filename = SP.map (SP.oneOrMore (SP.satisfy validFileChar), String.implode)
        val sep = (isChar #"\\") || (isChar #"/")
        val relative = parseRelative (filename, sep)
        val volume = alpha &&- (isChar #":") &&- sep
        val absolute = volume && relative
        val rooted = sep -&& relative

        val p = (SP.map (absolute, fn (drive, s) => PAbs (Drive drive, s))) ||
                (SP.map (rooted, fn s => PAbs (Root, s))) ||
                (SP.map (relative, fn s => PRel s))
      in p
      end

  val parseUnixStyle : t StringParser.t = 
      let
        val invalidFileChar =  (* Disallow backslash to support mixed paths *)
         fn c => (Char.ord c = 0) orelse (c = #"/") orelse (c = #"\\")  
        val validFileChar = not o invalidFileChar
        val filename = SP.map (SP.oneOrMore (SP.satisfy validFileChar), String.implode)
        val sep = (isChar #"/") || (isChar #"\\")
        val relative = parseRelative (filename, sep)
        val absolute = sep -&& relative

        val p = (SP.map (absolute, fn s => PAbs (Root, s))) ||
                (SP.map (relative, fn s => PRel s))
      in p
      end

  val parsePath = parseWindowsStyle || parseUnixStyle

  val convertCygwinVolume = 
   fn p => 
      (case p
        of PAbs (Root, arcs) => 
           (case List.rev arcs
             of ("cygdrive" :: vol :: arcs) => 
                if String.length vol = 1 then 
                  PAbs (Drive (String.sub (vol, 0)), List.rev arcs)
                else
                  p
              | _ => p)
         | _ => p)
           
  val fromString : string -> t = 
   fn s => 
      let
        val path = 
            (case SP.parse (parsePath, (s, 0))
              of SP.Success (_, p) => convertCygwinVolume p
               | _ => Fail.fail ("Path", "fromString", "Can't parse path: "^s))
      in path
      end

  val fromCygwinString : string -> t =
   fn s => 
      let
        val path = 
            (case SP.parse (parseUnixStyle, (s, 0))
              of SP.Success (_, p) => convertCygwinVolume p
               | _ => Fail.fail ("Path", "fromCygwinString", "Can't parse path: "^s))
      in path
      end

  val fromMinGWString : string -> t = 
   fn s => 
      let
        val path = 
            (case SP.parse (parseUnixStyle, (s, 0))
              of SP.Success (_, p) => p
               | _ => Fail.fail ("Path", "fromMinGWString", "Can't parse path: "^s))
      in path
      end

  val fromUnixString : string -> t = 
   fn s => 
      let
        val path = 
            (case SP.parse (parseUnixStyle, (s, 0))
              of SP.Success (_, p) => p
               | _ => Fail.fail ("Path", "fromUnixString", "Can't parse path: "^s))
      in path
      end

  val fromWindowsString : string -> t = 
   fn s => 
      let
        val path = 
            (case SP.parse (parseWindowsStyle, (s, 0))
              of SP.Success (_, p) => p
               | _ => Fail.fail ("Path", "fromWindowsString", "Can't parse path: "^s))
      in path
      end

  val collapseArcs : string list * string -> string = 
      String.concat o List.rev o List.separate 

  val toCygwinString : t -> string = 
      (fn path => 
          let
            val collapse = fn arcs => collapseArcs (arcs, "/")
            val s = 
                (case path
                  of PRel arcs => collapse arcs
                   | PAbs (Root, arcs) => "/"^collapse arcs
                   | PAbs (Drive c, arcs) => 
                     "/cygdrive/"^(Char.toString c)^"/"^collapse arcs)
          in s
          end)

  val toWindowsString : t -> string =
      (fn path => 
          let
            val collapse = fn arcs => collapseArcs (arcs, "\\")
            val s = 
                (case path
                  of PRel arcs => collapse arcs
                   | PAbs (Root, arcs) => Fail.fail ("Path", "toWindowsString", "Can't use root volume in windows")
                   | PAbs (Drive c, arcs) => (Char.toString c)^":\\"^collapse arcs)
          in s
          end)

  val toUnixString : t -> string =
      (fn path => 
          let
            val collapse = fn arcs => collapseArcs (arcs, "/")
            val s = 
                (case path
                  of PRel arcs => collapse arcs
                   | PAbs (Root, arcs) => "/"^collapse arcs
                   | PAbs (Drive c, arcs) => Fail.fail ("Path", "toUnixString", "Can't use Drive in windows"))
          in s
          end)

  val toMinGWString : t -> string = 
      (fn path => 
          let
            val collapse = fn arcs => collapseArcs (arcs, "/")
            val s = 
                (case path
                  of PRel arcs => collapse arcs
                   | PAbs (Root, arcs) => "/"^collapse arcs
                   | PAbs (Drive c, arcs) => 
                     "/"^(Char.toString c)^"/"^collapse arcs)
          in s
          end)

  val layout : t -> Layout.t = 
      (fn path => 
          let
            val lArcs = List.layout Layout.str 
            val l = 
                (case path
                  of PRel arcs            => Layout.seq [Layout.str "Relative: ", lArcs arcs]
                   | PAbs (Root, arcs)    => Layout.seq [Layout.str "Rooted: ", lArcs arcs]
                   | PAbs (Drive c, arcs) => 
                     let
                       val c = Char.toString c
                     in Layout.seq [Layout.str "Drive ", Layout.str c, Layout.str ": ", lArcs arcs]
                     end)
          in l
          end)

  (* must be relative *)
  val cons : string * t -> t = 
   fn (s, path) => 
      (case path 
        of PRel arcs => PRel (arcs @ [s])
         | PAbs _ => Fail.fail ("Path", "cons", "Can't cons to absolute path"))

  val snoc : t * string -> t =
   fn (path, s) => 
      (case path 
        of PRel arcs => PRel (s::arcs)
         | PAbs (vol, arcs) => PAbs (vol, s::arcs))

  (* second path must be relative *)
  val append : t * t -> t = 
   fn (path1, path2) => 
      (case (path1, path2)
        of (PRel arcs1, PRel arcs2) => PRel (arcs2 @ arcs1)
         | (PAbs (vol, arcs1), PRel arcs2) => PAbs (vol, arcs2 @ arcs1)
         | _ => Fail.fail ("Path", "append", "Can't append absolute path"))

  val setVolume : t * volume -> t = 
   fn (path, vol) => 
      (case path
        of PRel arcs => PAbs (vol, arcs)
         | PAbs (_, arcs) => PAbs (vol, arcs))

  val removeVolume : t -> t = 
   fn path => 
      (case path
        of PRel arcs => path
         | PAbs (_, arcs) => PRel arcs)

  val dropLast : t -> t =
    fn path =>
      (case path
        of PRel (_::arcs) => PRel arcs
         | PAbs (v, _::arcs) => PAbs (v, arcs)
         | _ => Fail.fail ("Path", "dropLast", "cannot drop empty path"))

end (* structure Path *)
