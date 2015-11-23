(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* Rationals *)

signature RAT =
sig
    type t
    val rat : IntInf.t * IntInf.t -> t
    val toInts : t -> IntInf.t * IntInf.t
    val numerator : t -> t
    val denominator : t -> t
    val toString : t -> string
    val layout : t -> Layout.t
    val fromInt : int -> t
    val fromIntInf : IntInf.t -> t
    val equals : t * t -> bool
    val < : t * t -> bool
    val <= : t * t -> bool
    val compare : t * t -> order
    val isInt : t -> bool
    val toInt : t -> int option
    val toIntInf : t -> IntInf.t option
    val toReal32 : t -> Real32.t
    val toReal64 : t -> Real64.t
    val zero : t
    val one : t
    val + : t * t -> t
    val - : t * t -> t
    val * : t * t -> t
    val / : t * t -> t
    val ~ : t -> t
    val rem : t * t -> t
    val quot : t * t -> t
end;

structure Rat :> RAT =
struct
    (* (c0,c1) represents the x such that c0+c1*x=0 *)
    (* invariants: c1>0, gcd(c0,c1)=1 *)
    type t = IntInf.t * IntInf.t
    fun toString (c0, c1) =
        IntInf.toString (IntInf.~ c0) ^ "/" ^ IntInf.toString c1
    local
        open Layout
    in
    fun layout (c0, c1) =
        seq ((IntInf.layout (IntInf.~ c0))::
             (if c1 = IntInf.one then
                  []
              else
                  [str "/", IntInf.layout c1]))
    end
    fun normalise (c0, c1) =
        let val gcd = IntInf.gcd (c0, c1)
            val (c0, c1) = (IntInf.div (c0, gcd), IntInf.div (c1, gcd))
            val (c0, c1) =
                if c1 < IntInf.zero
                then (IntInf.~ c0, IntInf.~ c1)
                else (c0, c1)
        in
            (c0, c1)
        end
    fun fromIntInf i = (IntInf.~ i, IntInf.one)
    fun fromInt i = fromIntInf (IntInf.fromInt i)
    (*Should this be eager?*)
    fun rat (num, denom) = normalise (IntInf.~ num, denom)

    (* Because of eager normalization *)
    fun equals (r1, r2) = r1 = r2
    fun op < ((c10, c11), (c20, c21)) =
        IntInf.< (IntInf.* (c20, c11), IntInf.* (c10, c21))
    fun op <= ((c10, c11), (c20, c21)) =
        IntInf.<= (IntInf.* (c20, c11), IntInf.* (c10, c21))

    fun compare ((c10, c11), (c20, c21)) =
        IntInf.compare (IntInf.* (c20, c11), IntInf.* (c10, c21))

    fun isInt (c0 : IntInf.t, c1 : IntInf.t) : bool =
        let
          val c = IntInf.~ c0
          val bigEnough = 
              (case Int.minInt
                of SOME min => IntInf.<= (IntInf.fromInt min, c)
                 | NONE => true)
          val smallEnough = 
              (case Int.maxInt
                of SOME max => IntInf.<= (c, IntInf.fromInt max)
                 | NONE => true)
        in 
          bigEnough andalso
          smallEnough andalso
          c1 = IntInf.one 
        end

             
    fun toInt (c0, c1) =
        if isInt (c0, c1) then
          SOME (IntInf.toInt (~c0))
        else
            NONE

    fun toIntInf (c0, c1) =
        if c1 = IntInf.one then
            SOME (IntInf.~ c0)
        else
            NONE

    fun toInts (c0, c1) = (IntInf.~ c0, c1)

    fun numerator (c0, c1) = (c0, IntInf.one)

    fun denominator (c0, c1) = (IntInf.~ c1, IntInf.one)

    val zero = fromInt 0
    val one = fromInt 1

    fun ~ (num, den) = (IntInf.~ num, den)

    fun recip  (num, den) = 
        if IntInf.< (num, 0) then 
          (IntInf.~ den, IntInf.~ num)
        else
          (den, num)

    fun + ((num1, den1), (num2, den2)) = 
        let
          val num = IntInf.+ (IntInf.* (num1, den2),
                              IntInf.* (den1, num2))
          val den = IntInf.* (den1, den2)
          val res = normalise (num, den)
        in res
        end

    fun - (r1, r2) = r1 + ~ r2

    fun * ((num1, den1), (num2, den2)) = 
        let
          val num = IntInf.* (IntInf.~ num1, num2)
          val den = IntInf.* (den1, den2)
          val res = normalise (num, den)
        in res
        end

    fun / (r1, r2) = r1 * recip r2

    fun rem (r1, r2) = 
        let
          val (num, den) = r1 / r2
          val num = IntInf.~ num
          val rem = IntInf.rem (num, den)
          val res = fromIntInf rem
        in res
        end

    fun quot (r1, r2) = 
        let
          val (num, den) = r1 / r2
          val num = IntInf.~ num
          val num = IntInf.quot (num, den)
          val res = fromIntInf num
        in res
        end

    (*
    fun debug (s, v) = print (s ^ " " ^ v ^ "\n")
    fun debugI (s, v) = debug (s, Int.toString v)
    fun debugJ (s, v) = debug (s, IntInf.toString v)
    *)

    fun roundingMode (m : IntInf.t, h : int) : int = 
        let
          val c = IntInf.<< (IntInf.one, Word.fromInt h)
          val r = IntInf.andb (m, IntInf.sub1 (IntInf.+ (c, c)))
        in
          case IntInf.compare (c, r) 
            of LESS    => 2
             | GREATER => 0
             | EQUAL   => 1
        end

    fun isPowerOf2 (n : IntInf.t) : bool = n = IntInf.<< (IntInf.one, Word.fromInt (IntInf.log2 n))

    fun toReal (encode, me : int, md : int, n : IntInf.t, d : IntInf.t) = 
        let
          fun encodeFloat (x, y) = if IntInf.isPositive n then encode (IntInf.~ x, y) else encode (x, y)
          val n = IntInf.abs n
          val ld = IntInf.log2 d
          fun + (x, y) = Int.+ (x, y)
          fun - (x, y) = Int.- (x, y)
          fun > (x, y) = Int.> (x, y)
          fun < (x, y) = Int.< (x, y)
          fun >= (x, y) = Int.>= (x, y)
          fun <= (x, y) = Int.<= (x, y)
          fun << (x, y) = IntInf.<< (x, Word.fromInt y)
          fun >> (x, y) = IntInf.~>> (x, Word.fromInt y)
        in
          if d = << (IntInf.one, ld)
            then
              let
                val ln = IntInf.log2 n
              in
                if ln > (ld + me)
                  then
                    if ln < md
                      then encodeFloat (<< (n, md - 1 - ln), ln + 1 - ld - md)
                      else
                        let 
                          val n1 = >> (n, ln + 1 - md)
                          val n2 = case roundingMode (n, ln - md)
                                    of 0 => n1
                                     | 2 => IntInf.+ (n1, 1)
                                     | _ => if IntInf.isEven n1 then n1 else IntInf.+ (n1, 1)
                        in
                          encodeFloat (n2, ln - ld + 1 - md)
                        end
                  else
                    let
                      val ld' = ld + me - md
                    in
                      case Int.compare (ld', ln + 1)
                        of GREATER => encodeFloat (IntInf.zero, 0)
                         | EQUAL => if isPowerOf2 n then encodeFloat (IntInf.zero, 0) else encodeFloat (IntInf.one, me - md)
                         | LESS =>
                            if ld' <= 0 then encodeFloat (n, me - md - ld')
                              else 
                                let 
                                  val n' = >> (n, ld')
                                in
                                  case roundingMode (n, ld' - 1)
                                    of 0 => encodeFloat (n', me - md)
                                     | 1 => if IntInf.isEven n'
                                              then encodeFloat (n', me - md)
                                              else encodeFloat (IntInf.+ (n', 1), me - md)
                                     | _ => encodeFloat (IntInf.+ (n', 1), me - md)
                                end
                    end
              end
            else
              let
                val ln = IntInf.log2 n
                val p0 = Int.max (me, ln - ld)
                val (n', d') = case Int.compare (p0, md)
                                 of LESS    => (<< (n, md - p0), d)
                                  | EQUAL   => (n, d) 
                                  | GREATER => (n, << (d, p0 - md))
                fun scale (p, a, b) = if IntInf.<= (<< (b, md), a) then (p + 1, a, << (b, 1))
                                        else (p, a, b)
                val (p', n'', d'') = scale (p0 - md, n', d')
                val (q, r) = IntInf.quotRem (n'', d'')
                val rdq = case IntInf.compare (<< (r, 1), d'')
                            of LESS    => q
                             | EQUAL   => if IntInf.isEven q then q else IntInf.+ (q, 1)
                             | GREATER => IntInf.+ (q, 1)
              in
                encodeFloat (rdq, p')
              end
        end

    val DBL_MIN_EXP = ~1021
    val DBL_MAX_EXP = 1024
    val DBL_MANT_DIG = 53
    val FLT_MIN_EXP = ~125
    val FLT_MAX_EXP = 128
    val FLT_MANT_DIG = 24

    fun toReal64 (n, d) = 
        if n = 0 then Real64.zero
          else if d = 0 then Real64./ (Real64.fromIntInf (IntInf.~ n), Real64.zero)
            else toReal (fn (m, n) => Real64.* (Real64.fromIntInf m, Real64.pow (Real64.two, Real64.fromInt n)), 
                         DBL_MIN_EXP, DBL_MANT_DIG, n, d)
    fun toReal32 (n, d) = 
        if n = 0 then Real32.zero
          else if d = 0 then Real32./ (Real32.fromIntInf (IntInf.~ n), Real32.zero)
            else toReal (fn (m, n) => Real32.* (Real32.fromIntInf m, Real32.pow (Real32.two, Real32.fromInt n)), 
                         FLT_MIN_EXP, FLT_MANT_DIG, n, d)
         
end;
