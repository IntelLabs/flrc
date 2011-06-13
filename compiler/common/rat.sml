(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* Rationals *)

signature RAT = sig
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

structure Rat :> RAT = struct
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

end;
