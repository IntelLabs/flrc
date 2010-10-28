(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, April 2010 *)

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
 * Paths may be reifed as Windows, Cygwin, or Unix path
 * strings.  Relative paths are valid for all three.  
 * An absolute path is valid for:
 *   Windows if the volume is a drive letter
 *   Unix if the volume is root
 *   Cygwin always
 * 
 * Examples:
 *   If volume is C and the links are ["documents and settings", "lpeterse"], 
 *    then the reifications are:
 *    Windows:  C:\\documents and settings\lpeterse
 *    Unix: exception Path
 *    Cygwin: /cygdrive/c/documents and settings/lpeterse
 *
 *   If volume is root and the links are ["usr", "lib"],
 *    then the reifications are:
 *    Windows:  exception Path
 *    Unix: /usr/lib
 *    Cygwin: /usr/lib
 * 
 *)

signature PATH = 
sig
  type t 
  datatype volume = Root | Drive of char
  exception Path of string
  val fromString : string -> t
  val toCygwinString : t -> string
  val toWindowsString : t -> string
  val toUnixString : t -> string
  (* must be relative *)
  val cons : string * t -> t
  val snoc : t * string -> t
  (* second path must be relative *)
  val append : t * t -> t
  val setVolume : t * volume -> t
  val removeVolume : t -> t
end

structure Path :> PATH =
struct

  datatype volume = Root | Drive of char

  (* arcs are in reverse order *)
  datatype t = PRel of string list
             | PAbs of volume * (string list)

  exception Path of string

  val fromString : string -> t = 
   fn s => 
      let
        val {isAbs, vol, arcs} = 
            (OS.Path.fromString s) 
            handle OS.Path.Path => raise (Path ("Bad path string: "^s))
        val path = 
            if isAbs then
              let
                val (volume, arcs) = 
                    if vol = "" then
                      (case arcs
                        of ("cygdrive"::vol::restArcs) => 
                           if String.length vol = 1 then 
                             (Drive (String.sub (vol, 0)), restArcs)
                           else
                             (Root, arcs)
                         | _ => (Root, arcs))
                    else
                      (case String.sub (vol, 1)
                        of #":" => (Drive (String.sub (vol, 0)), arcs)
                         | _ => raise (Path ("Bad volume: "^vol)))
                val arcs = List.rev arcs
              in PAbs (volume, arcs)
              end
            else
              if vol = "" then 
                PRel (List.rev arcs)
              else
                raise (Path ("Relative path with volume"))
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
            val s = OS.Path.mkCanonical s
          in s
          end)

  val toWindowsString : t -> string =
      (fn path => 
          let
            val collapse = fn arcs => collapseArcs (arcs, "\\")
            val s = 
                (case path
                  of PRel arcs => collapse arcs
                   | PAbs (Root, arcs) => raise (Path ("Can't use root volume in windows"))
                   | PAbs (Drive c, arcs) => (Char.toString c)^":\\"^collapse arcs)
            val s = OS.Path.mkCanonical s
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
                   | PAbs (Drive c, arcs) => raise (Path ("Can't use Drive in windows")))
            val s = OS.Path.mkCanonical s
          in s
          end)

  (* must be relative *)
  val cons : string * t -> t = 
   fn (s, path) => 
      (case path 
        of PRel arcs => PRel (arcs @ [s])
         | PAbs _ => raise (Path ("Can't cons to absolute path")))

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
         | _ => raise (Path ("Can't append absolute path")))

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

end (* structure Path *)
