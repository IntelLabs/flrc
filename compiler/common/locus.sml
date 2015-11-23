(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature LOCUS =
sig
  datatype lineCol = LC of {line : int, col : int}
  datatype t = L of {file : string, start : lineCol, finish : lineCol}
  val dummy : t
  val toString : t -> string
  val compare : t * t -> order
  structure Dict : DICT where type key = t
  val layout : t -> Layout.t
end;

structure Locus :> LOCUS =
struct

  datatype lineCol = LC of {line : int, col : int}

  datatype t = L of {file : string, start : lineCol, finish : lineCol}

  val dummyLineCol = LC {line = 0, col = 0}

  val dummy = L {file = "xxx", start = dummyLineCol, finish = dummyLineCol}

  fun toString (L {file, start = LC {line = sl, col = sc}, finish = LC {line = el, col = ec}}) =
      file ^ "(" ^
      (
       if sl = 0 andalso sc = 0 andalso el = 0 andalso ec = 0 then
         ""
       else if sl = el then
         if ec <= sc + 1 then
           Int.toString (sl + 1) ^ ":" ^ Int.toString (sc + 1)
         else
           Int.toString (sl + 1) ^ ":" ^ Int.toString (sc + 1) ^ ".." ^ Int.toString (ec + 1)
       else
         Int.toString (sl + 1) ^ ":" ^ Int.toString (sc + 1) ^ ".." ^
         Int.toString (el + 1) ^ ":" ^ Int.toString (ec + 1)
      ) ^ ")"

  fun compareLineCol (LC x1, LC x2) = Compare.rec2 (#line, Int.compare, #col, Int.compare) (x1, x2)

  fun compare (L x1, L x2) =
      Compare.rec3 (#file, String.compare, #start, compareLineCol, #finish, compareLineCol) (x1, x2)

  structure Dict = DictF(struct type t = t val compare = compare end)

  fun layout l = Layout.str (toString l)

end;
