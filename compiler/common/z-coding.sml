(* The Intel FL to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* GHC's Z coding *)

signature Z_CODING =
sig
  val encode : string -> string
  val encodeExceptUnderscore : string -> string
  val decode : string -> string
end;

structure ZCoding :> Z_CODING =
struct

  val modname = "ZCoding"

  fun toHexOne (i : int) : string = Char.toString (String.sub ("0123456789abcdef", i))

  fun toHexString (i : int) : string =
      if i < 16
      then toHexOne i
      else toHexString (Int.div (i, 16)) ^ toHexOne (Int.mod (i, 16))

  fun charToZString (c : int) : string =
      let
        val hexStr = toHexString c
        val hexStr = if Char.isDigit (String.sub (hexStr, 0)) then hexStr else "0" ^ hexStr
        val s = "z" ^ hexStr ^ "U"
      in s
      end

  val unencodedChar : char -> bool =
   fn #"Z" => false
    | #"z" => false
    | c    => Char.isAlphaNum c

  val encodeCh : bool * char -> string =
   fn (u, c) =>
      if unencodedChar c then
        Char.toString c
      else
        case c
         of #"(" => "ZL"
          | #")" => "ZR"
          | #"[" => "ZM"
          | #"]" => "ZN"
          | #":" => "ZC"
          | #"Z" => "ZZ"
          | #"z" => "zz"
          | #"&" => "za"
          | #"|" => "zb"
          | #"^" => "zc"
          | #"$" => "zd"
          | #"=" => "ze"
          | #">" => "zg"
          | #"#" => "zh"
          | #"." => "zi"
          | #"<" => "zl"
          | #"-" => "zm"
          | #"!" => "zn"
          | #"+" => "zp"
          | #"'" => "zq"
          | #"\\"=> "zr"
          | #"/" => "zs"
          | #"*" => "zt"
          | #"_" => if u then "zu" else "_"
          | #"%" => "zv"
          | c    => charToZString (ord c)

  val maybeTuple : string -> string option =
    fn "(# #)" => SOME "Z1H"
     | "()"    => SOME "Z0T"
     | s       => 
       let fun count_commas n (#"," :: cs) = count_commas (n+1) cs
             | count_commas n cs	   = (n,cs)
       in 
         case explode s 
           of #"(" :: #"#" :: cs =>
              (case count_commas 0 cs
                 of (n, #"#" :: #")" :: _) => SOME ("Z" ^ Int.toString (n+1) ^ "H")
                  | _                      => NONE)
            | #"(" :: cs =>
              (case count_commas 0 cs
                 of (n, #")" :: _) => SOME ("Z" ^ Int.toString (n+1) ^ "T")
                  | _              => NONE)
            | _ => NONE
       end

  fun encodeA (u : bool, cs : string) : string =
      let
        fun go [] = ""
          | go (c::cs) = encodeCh (u, c) ^ go cs
      in
        case maybeTuple cs
         of SOME n => n
          | _      => go (explode cs)
      end

  fun encode (s : string) : string = encodeA (true, s)

  fun encodeExceptUnderscore (s : string) : string = encodeA (false, s)

  val decodeUpper : char -> char =
    fn #"L" => #"("
     | #"R" => #")"
     | #"M" => #"["
     | #"N" => #"]"
     | #"C" => #":"
     | #"Z" => #"Z"
     | ch   => ch

  val decodeLower : char -> char =
    fn #"z" => #"z"
     | #"a" => #"&"
     | #"b" => #"|"
     | #"c" => #"^"
     | #"d" => #"$"
     | #"e" => #"="
     | #"g" => #">"
     | #"h" => #"#"
     | #"i" => #"."
     | #"l" => #"<"
     | #"m" => #"-"
     | #"n" => #"!"
     | #"p" => #"+"
     | #"q" => #"'"
     | #"r" => #"\\"
     | #"s" => #"/"
     | #"t" => #"*"
     | #"u" => #"_"
     | #"v" => #"%"
     | ch   => ch

  fun decode []                  = []
    | decode (#"Z" :: d :: rest) = if Char.isDigit d then decodeTuple (d, rest) else decodeUpper d :: decode rest
    | decode (#"z" :: d :: rest) = if Char.isDigit d then decodeNumEsc (d, rest) else decodeLower d :: decode rest
    | decode (c    :: rest)      = c :: decode rest

  and decodeNumEsc (d : char, rest : char list) : char list =
      let
        fun fail () = raise Fail ("decodeNumEsc: " ^ implode (d :: rest))
        fun go n (c :: cs) =
            if Char.isHexDigit c then
              go (16 * n + Char.toHexDigit c) cs
            else if c = #"U" then
              (if n >= 0 andalso n < 256 then
                chr n :: decode cs
               else explode (charToZString n) @ decode cs)
            else fail ()
          | go n [] = fail ()
      in
        go (Char.toHexDigit d) rest
      end

  and decodeTuple (d : char, rest : char list) : char list =
      let
        fun fail () = raise Fail ("decodeTuple: " ^ implode (d :: rest))
        fun go 0 (#"T" :: cs) = #"(" :: #")" :: decode cs
          | go n (#"T" :: cs) = (#"(" :: List.duplicate (n - 1, fn _ => #",")) @ (#")" :: decode cs)
          | go 1 (#"H" :: cs) = explode "(# #)" @ decode cs
          | go n (#"H" :: cs) = (#"(" :: #"#" :: List.duplicate (n-1,  fn _ => #",")) @ (#"#" :: #")" :: decode cs)
          | go n (c :: cs)    = if Char.isDigit c then go (10 * n + Char.toInt c) cs else fail ()
          | go _ []           = fail ()
    in go (Char.toHexDigit d) rest
    end

  val decode : string -> string = implode o decode o explode

end;
