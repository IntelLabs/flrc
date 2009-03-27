(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

(* Integral types *)

signature INT_ARB =
sig
  datatype size = S8 | S16 | S32 | S64
  datatype signed = Signed | Unsigned
  datatype typ = T of size * signed
  type t
  val stringOfSize : size -> string
  val stringOfSigned : signed -> string
  val stringOfTyp : typ -> string
  val stringOfTypShort : typ -> string
  val stringOf : t -> string
  val layoutTyp : typ -> Layout.t
  val layout : t -> Layout.t
  val bytes : size -> int
  val bits : size -> int
  val equalTyps : typ * typ -> bool
  val compareSizes : size * size -> order
  val compareSigns : signed * signed -> order
  val compareTyps : typ * typ -> order
  val isTyp : t * typ -> bool
  val sameTyps : t * t -> bool
  val typOf : t -> typ
  val fromInt : typ * int -> t
  val fits : typ * IntInf.t -> bool
  val fromIntInf : typ * IntInf.t -> t (* truncates arg to fit *)
  val toIntInf : t -> IntInf.t
  val convert : t * typ -> t
  val equals : t * t -> bool
  val compare : t * t -> order
  val < : t * t -> bool
  val <= : t * t -> bool
  val + : t * t -> t
  val - : t * t -> t
  val ~ : t -> t
  val * : t * t -> t
  val quot : t * t -> t
  val rem : t * t -> t
  val maxValue : typ -> IntInf.t
end;

structure IntArb :> INT_ARB =
struct

  datatype size = S8 | S16 | S32 | S64

  datatype signed = Signed | Unsigned

  datatype typ = T of size * signed

  datatype t = X of typ * IntInf.t
    (* Invariant: int inf fits in the range of the type *)

  fun stringOfSize sz =
      case sz
       of S8 => "8" | S16 => "16" | S32 => "32" | S64 => "64"

  fun stringOfSigned signed =
      case signed of Signed => "S" | Unsigned => "U"

  fun stringOfTyp (T (sz, signed)) =
      stringOfSigned signed ^ "Int" ^ stringOfSize sz

  fun stringOfTypShort (T (sz, signed)) =
      stringOfSigned signed ^ stringOfSize sz

  fun layoutTyp typ = Layout.str (stringOfTyp typ)

  fun stringOf (X (t, i)) =
      stringOfTypShort t ^ "(" ^ IntInf.toString i ^ ")"

  fun layout (X (t, i)) =
      Layout.seq [Layout.str (stringOfTypShort t),
                  Layout.paren (IntInf.layout i)]

  fun equalTyps (t1, t2) = t1 = t2

  fun compareSizes (sz1, sz2) =
      case (sz1, sz2)
       of (S8,   S8  ) => EQUAL
        | (_,    S8  ) => GREATER
        | (S8,   _   ) => LESS
        | (S16,  S16 ) => EQUAL
        | (_,    S16 ) => GREATER
        | (S16,  _   ) => LESS
        | (S32,  S32 ) => EQUAL
        | (_,    S32 ) => GREATER
        | (S32,  _   ) => LESS
        | (S64,  S64 ) => EQUAL

  fun compareSigns (s1, s2) =
      case (s1, s2)
       of (Unsigned, Unsigned) => EQUAL
        | (Unsigned, Signed  ) => LESS
        | (Signed,   Unsigned) => GREATER
        | (Signed,   Signed  ) => EQUAL

  fun compareTyps (T (sz1, signed1), T (sz2, signed2)) =
      Compare.pair (compareSizes, compareSigns)
                   ((sz1, signed1), (sz2, signed2))

  fun isTyp (X (t1, _), t2) = equalTyps (t1, t2)

  fun sameTyps (X (t1, _), X (t2, _)) = equalTyps (t1, t2)

  fun typOf (X (t, _)) = t

  fun equals (X (t1, i1), X (t2, i2)) =
      if equalTyps (t1, t2) then
        IntInf.equals (i1, i2)
      else
        Fail.fail ("IntArb", "equals", "mismatched types")

  fun compare (X (t1, i1), X (t2, i2)) =
      if equalTyps (t1, t2) then
        IntInf.compare (i1, i2)
      else
        Fail.fail ("IntArb", "compare", "mismatched types")

  fun < (X (t1, i1), X (t2, i2)) =
      if equalTyps (t1, t2) then
        IntInf.< (i1, i2)
      else
        Fail.fail ("IntArb", "<", "mismatched types")

  fun <= (X (t1, i1), X (t2, i2)) =
      if equalTyps (t1, t2) then
        IntInf.<= (i1, i2)
      else
        Fail.fail ("IntArb", "<=", "mismatched types")

  fun bytes sz = 
      case sz of S8 => 1 | S16 => 2 | S32 => 4 | S64 => 8

  fun bits sz =
      case sz of S8 => 8 | S16 => 16 | S32 => 32 | S64 => 64

  fun range (t as (T (sz, signed))) =
      let
        val nb = bits sz
        val nbw =
            Word.fromInt (case signed of Signed => nb - 1 | Unsigned => nb)
        val upper = IntInf.<< (IntInf.one, nbw)
        val lower =
            case signed of Signed => IntInf.~ upper | Unsigned => IntInf.zero
      in (lower, upper)
      end

  fun maxValue t = IntInf.- (#2 (range t), IntInf.one)

  fun fits (t, i) =
      let
        val (lower, upper) = range t
      in
        IntInf.<= (lower, i) andalso IntInf.< (i, upper)
      end

  fun truncResult (t as T (sz, signed), i) =
      let
        val bits = bits sz
        val maxu = IntInf.<< (IntInf.one, Word.fromInt bits)
        val maxs = IntInf.<< (IntInf.one, Word.fromInt (bits - 1))
        val mask = IntInf.sub1 maxu
        val i =
            case signed
             of Unsigned => IntInf.andb (i, mask)
              | Signed =>
                IntInf.- (IntInf.andb (IntInf.+ (i, maxs), mask), maxs)
      in X (t, i)
      end

  fun toIntInf (X (_, i)) = i

  fun fromIntInf (t, i) = truncResult (t, i)

  fun fromInt (t, i) = fromIntInf (t, IntInf.fromInt i)

  fun convert (X (_, i), t) = truncResult (t, i)

  fun + (X (t1, i1), X (t2, i2)) =
      if equalTyps (t1, t2) then
        truncResult (t1, IntInf.+ (i1, i2))
      else
        Fail.fail ("IntArb", "+", "mismatched types")

  fun ~ (X (t, i)) = truncResult (t, IntInf.~ i)

  fun - (X (t1, i1), X (t2, i2)) =
      if equalTyps (t1, t2) then
        truncResult (t1, IntInf.- (i1, i2))
      else
        Fail.fail ("IntArb", "-", "mismatched types")

  fun * (X (t1, i1), X (t2, i2)) =
      if equalTyps (t1, t2) then
        truncResult (t1, IntInf.* (i1, i2))
      else
        Fail.fail ("IntArb", "*", "mismatched types")

  fun quot (X (t1, i1), X (t2, i2)) =
      if equalTyps (t1, t2) then
        truncResult (t1, IntInf.quot (i1, i2))
      else
        Fail.fail ("IntArb", "div", "mismatched types")

  fun rem (X (t1, i1), X (t2, i2)) =
      if equalTyps (t1, t2) then
        truncResult (t1, IntInf.rem (i1, i2))
      else
        Fail.fail ("IntArb", "rem", "mismatched types")

end;
