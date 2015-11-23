(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* Integral types *)

signature INT_ARB =
sig
  datatype size = S8 | S16 | S32 | S64
  datatype signed = Signed | Unsigned
  datatype typ = T of size * signed
  type t
  val band : t * t -> t
  val bnot : t -> t
  val bor : t * t -> t
  val bxor : t * t -> t
  val stringOfSize : size -> string
  val sizeFromString : string -> size option
  val stringOfSigned : signed -> string
  val signedFromString : string -> signed option
  val stringOfTyp : typ -> string
  val stringOfTypShort : typ -> string
  val stringOf : t -> string
  val layoutTyp : typ -> Layout.t
  val layout : t -> Layout.t
  val bytes : size -> int
  val bits : size -> int
  val typSize : typ -> size
  val equalTyps : typ * typ -> bool
  val compareSizes : size * size -> order
  val compareSigns : signed * signed -> order
  val compareTyps : typ * typ -> order
  val isTyp : t * typ -> bool
  val sameTyps : t * t -> bool
  val typIsSigned : typ -> bool
  val typOf : t -> typ
  val fromInt : typ * int -> t
  val toInt : t -> int option
  val fits : typ * IntInf.t -> bool
  val fromIntInf : typ * IntInf.t -> t (* truncates arg to fit *)
  val toIntInf : t -> IntInf.t
  val convert : t * typ -> t
  val equalsSyntactic : t * t -> bool
  val compareSyntactic : t * t -> order
  (* All of the following binary functions are defined only on integers of 
   * the same size and sign.  Calling them on integers of different precision
   * will result in an exception.  The caller is responsible for verifying
   * that the operands are appropriate.
   *)
  val equalsNumeric : t * t -> bool
  val compareNumeric : t * t -> order
  val < : t * t -> bool
  val <= : t * t -> bool
  val + : t * t -> t
  val - : t * t -> t
  val ~ : t -> t
  val * : t * t -> t
  val quot : t * t -> t
  val rem : t * t -> t
  val divT : t * t -> t
  val modT : t * t -> t
  val divF : t * t -> t
  val modF : t * t -> t
  val maxValue : typ -> IntInf.t
  val minValue : typ -> IntInf.t
  val maxValueT : typ -> t
  val minValueT : typ -> t
  structure Signed :
  sig
    structure Dec :
    sig
      val signed   : signed -> unit option
      val unsigned : signed -> unit option
    end (* structure Dec *)
  end (* structure Signed *)
  structure Size :
  sig
    structure Dec :
    sig
      val s8  : size -> unit option
      val s16 : size -> unit option
      val s32 : size -> unit option
      val s64 : size -> unit option
    end (* structure Dec *)
  end (* structure Size *)
end;

structure IntArb :> INT_ARB =
struct

  datatype size = S8 | S16 | S32 | S64

  datatype signed = Signed | Unsigned

  datatype typ = T of size * signed

  datatype t = X of typ * IntInf.t
    (* Invariant: int inf fits in the range of the type *)

  fun stringOfSize sz =
      case sz of S8 => "8" | S16 => "16" | S32 => "32" | S64 => "64"

  val sizeFromString = 
      fn s => case s of "8" => SOME S8 | "16" => SOME S16 | "32" => SOME S32 | "64" => SOME S64 | _ => NONE

  fun stringOfSigned signed =
      case signed of Signed => "S" | Unsigned => "U"

  val signedFromString = 
   fn s => case s of "S" => SOME Signed | "U" => SOME Unsigned | _ => NONE

  fun stringOfTyp (T (sz, signed)) =
      stringOfSigned signed ^ "Int" ^ stringOfSize sz

  fun stringOfTypShort (T (sz, signed)) =
      stringOfSigned signed ^ stringOfSize sz

  fun layoutTyp typ = Layout.str (stringOfTyp typ)

  fun stringOf (X (t, i)) =
      stringOfTypShort t ^ "(" ^ IntInf.toString i ^ ")"

  fun layout (X (t, i)) =
      Layout.seq [Layout.str (stringOfTypShort t), Layout.paren (IntInf.layout i)]

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

  fun typIsSigned (T (sz, signed)) = signed = Signed

  fun typOf (X (t, _)) = t

  fun equalsSyntactic (X (t1, i1), X (t2, i2)) =
      equalTyps (t1, t2) andalso IntInf.equals (i1, i2)

  fun compareSyntactic (X p1, X p2) =
      Compare.pair (compareTyps, IntInf.compare) (p1, p2)

  fun equalsNumeric (X (t1, i1), X (t2, i2)) =
      if equalTyps (t1, t2) then
        IntInf.equals (i1, i2)
      else
        Fail.fail ("IntArb", "equals", "mismatched types")

  fun compareNumeric (X (t1, i1), X (t2, i2)) =
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

  fun typSize (T (sz, _)) = sz

  fun range (t as (T (sz, signed))) =
      let
        val nb = bits sz
        val nbw = Word.fromInt (case signed of Signed => nb - 1 | Unsigned => nb)
        val upper = IntInf.<< (IntInf.one, nbw)
        val lower = case signed of Signed => IntInf.~ upper | Unsigned => IntInf.zero
      in (lower, upper)
      end

  fun maxValue t = IntInf.- (#2 (range t), IntInf.one)

  fun minValue t = #1 (range t)

  fun maxValueT t = X (t, maxValue t)

  fun minValueT t = X (t, minValue t)

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

  val toInt = 
   fn t => 
      let
        val ti = toIntInf t
        val r = 
            (case (Int.maxInt, Int.minInt)
              of (SOME max, SOME min) => 
                 if IntInf.>= (ti, IntInf.fromInt min) andalso IntInf.<= (ti, IntInf.fromInt max) then
                   SOME (IntInf.toInt ti)
                 else
                   NONE
               | _ => SOME (IntInf.toInt ti))
      in r
      end

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

  val divT = quot
  val modT = rem

  fun divF (X (t1, i1), X (t2, i2)) =
      if equalTyps (t1, t2) then
        truncResult (t1, IntInf.div (i1, i2))
      else
        Fail.fail ("IntArb", "divF", "mismatched types")

  fun modF (X (t1, i1), X (t2, i2)) =
      if equalTyps (t1, t2) then
        truncResult (t1, IntInf.mod (i1, i2))
      else
        Fail.fail ("IntArb", "modF", "mismatched types")

  fun bnot (X (t, i)) = X (t, IntInf.notb i)

  fun band (X (t1, i1), X (t2, i2)) =
      if equalTyps (t1, t2) then
        X (t1, IntInf.andb (i1, i2))
      else
        Fail.fail ("IntArb", "band", "mismatched types")

  fun bor (X (t1, i1), X (t2, i2)) =
      if equalTyps (t1, t2) then
        X (t1, IntInf.orb (i1, i2))
      else
        Fail.fail ("IntArb", "bor", "mismatched types")

  fun bxor (X (t1, i1), X (t2, i2)) =
      if equalTyps (t1, t2) then
        X (t1, IntInf.xorb (i1, i2))
      else
        Fail.fail ("IntArb", "bxor", "mismatched types")

  structure Signed =
  struct
    structure Dec =
    struct
      val signed   : signed -> unit option = 
       fn a => case a of Signed => SOME () | _ => NONE
      val unsigned : signed -> unit option =
       fn a => case a of Unsigned => SOME () | _ => NONE
    end (* structure Dec *)
  end (* structure Signed *)
  structure Size =
  struct
    structure Dec =
    struct
      val s8  : size -> unit option = 
       fn a => case a of S8 => SOME () | _ => NONE
      val s16 : size -> unit option = 
       fn a => case a of S16 => SOME () | _ => NONE
      val s32 : size -> unit option = 
       fn a => case a of S32 => SOME () | _ => NONE
      val s64 : size -> unit option = 
       fn a => case a of S64 => SOME () | _ => NONE
    end (* structure Dec *)
  end (* structure Size *)

end;
