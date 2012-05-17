(* The Intel FL to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature INTR =
sig

  type t

  val empty : t
  val all : t
  val nat : t

  val range : {lower : IntInf.t option, upper : IntInf.t option} -> t
  val interval : {lower : IntInf.t, upper : IntInf.t} -> t

  val single : IntInf.t -> t
  val fromInt : int -> t
  val toIntInf : t -> IntInf.t option
  val toInt : t -> int option

  val check : t -> bool
  val isEmpty : t -> bool

(*  val complement : t -> t
  val union : t * t -> t*)
  val intersection : t * t -> t

  val out : t -> {lower : IntInf.t option, upper : IntInf.t option}

  val layout : t -> Layout.t

end;

structure Intr :> INTR =
struct

  datatype t = IR of {lower : IntInf.t option, upper : IntInf.t option}

  val empty = IR {lower = SOME IntInf.one, upper = SOME IntInf.zero}
  val all = IR {lower = NONE, upper = NONE}
  val nat = IR {lower = SOME IntInf.zero, upper = NONE}

  fun range {lower, upper} = IR {lower = lower, upper = upper}

  fun interval {lower, upper} = IR {lower = SOME lower, upper = SOME upper}

  fun single i = IR {lower = SOME i, upper = SOME i}

  fun fromInt i = single (IntInf.fromInt i)

  fun toIntInf (IR {upper, lower}) =
      case (lower, upper)
       of (SOME l, SOME u) => if IntInf.equals (l, u) then SOME l else NONE
        | _                => NONE

  fun toInt ir = Option.map (toIntInf ir, IntInf.toInt)

  fun check _ = true

  fun isEmpty (IR {upper, lower}) =
      case (upper, lower)
       of (SOME l, SOME u) => IntInf.> (l, u)
        | _                => false

  fun isSingle (IR {upper, lower}) =
      case (upper, lower)
       of (SOME l, SOME u) => IntInf.equals (l, u)
        | _                => false

  fun intersection (ir1, ir2) =
      let
        val IR {lower = l1, upper = u1} = ir1
        val IR {lower = l2, upper = u2} = ir2
        val l =
            case (l1, l2)
             of (NONE,    x      ) => x
              | (x,       NONE   ) => x
              | (SOME l1, SOME l2) => SOME (IntInf.max (l1, l2))
        val u =
            case (u1, u2)
             of (NONE,    x      ) => x
              | (x,       NONE   ) => x
              | (SOME u1, SOME u2) => SOME (IntInf.min (u1, u2))
        val ir = IR {lower = l, upper = u}
      in
        if isEmpty ir then empty else ir
      end

  fun out (IR {lower, upper}) = {lower = lower, upper = upper}

  local
    structure L = Layout
    structure LU = LayoutUtils
  in
  fun layout (ir as IR {lower, upper}) =
      let
        val ll =
            case lower
             of NONE   => L.str "-w"
              | SOME i => IntInf.layout i
        val lu =
            case upper
             of NONE   => L.str "w"
              | SOME i => IntInf.layout i
        val l = if isSingle ir then ll else L.seq [ll, L.str "..", lu]
        val l = if isEmpty ir then L.empty else l
        val l = LU.bracket l
      in l
      end
  end

end;

(*
  (* This data specifies sets of integers.
   * flips lists integers where membership in the set flips from in to out and vice versea
   *   - flips must be strictly increasing
   * neginf specifies whether the integers before the first entry of flips are in (true) or out (false)
   * In other words:
   * If neginf then
   *   if flips = [o1, i1, ..., on, in] then the set is
   *     -infinity..o1-1, i1..o2-1, ..., i(n-1)..on-1, in..+infinity
   *   if flips = [o1, i1, ..., on, in, o(n+1)] then the set is
   *     -inifinity..o1-1, i1..o2-1, ..., in..o(n+1)-1
   * else
   *   if flips = [i1, o1, ..., in, on] then the set is
   *     i1..o1-1, ..., in..on-1
   *   if flisp = [i1, o1, ..., in, on, i(n+1)] then the set is
   *     i1..o1-1, ..., in..on, i(n+1)...+infinity
   *)
  datatype t = IR of {neginf : bool, flips : IntInf.t Vector.t}

structure Intr :> INTR =
struct

  open IntInf

  datatype t = IR of {neginf : bool, flips : IntInf.t Vector.t}

  val empty = IR {neginf = false, flips = Vector.new0 ()}
  val all = IR {neginf = true, flips = Vector.new0 ()}

  fun single i = IR {neginf = false, flips = Vector.new2 (i, i + 1)}

  fun fromInt i = single (IntInf.fromInt i)

  fun toIntInf (IR {neginf, flips}) =
      if neginf orelse Vector.length flips <> 2 then
        NONE
      else
        let
          val i = Vector.sub (flips, 0)
        in
          if equals (i + 1, Vector.sub (flips, 1)) then SOME i else NONE
        end

  fun toInt ir = Option.map (toIntInf ir, IntInf.toInt)

  fun check (IR {neginf, flips}) =
      let
        fun checkOne (i, cur) = i = 0 orelse Vector.sub (flips, Int.- (i, 1)) < cur
        val ok = Vector.foralli (flips, checkOne)
      in ok
      end

  fun isEmpty (ir as IR {neginf, flips}) =
      let
        val () = Fail.assert ("Intr", "isEmpty", "bad intr", fn () => check ir)
      in
        not neginf andalso Vector.length flips = 0
      end

  fun complement (IR {neginf, flips}) = IR {neginf = not neginf, flips = flips}

  fun intersection (ir1, ir2) =
      let
        val IR {neginf = ni1, flips = fs1} = ir1
        val IR {neginf = ni2, flips = fs2} = ir2
        val ni = ni1 andalso ni2
        fun exit (ino, flips, acc) =
            if ino then
              List.appendRev (acc, flips)
            else
              List.rev acc
        fun loop (in1, flips1, in2, flips2, acc) =
            case flips1
             of [] => exit (in1, flips2, acc)
              | i1::flips1' =>
                case flips2
                 of [] => exit (in2, flips1, acc)
                  | i2::flips2' =>
                    if i1 < i2 then
                      loop (not in1, flips1', in2, flips2, if in2 then i1::acc else acc)
                    else if i2 < i1 then
                      loop (in1, flips1, not in2, flips2', if in1 then i2::acc else acc)
                    else (* i1 = i2 *)
                      loop (not in1, flips1', not in2, flips2', if not (Bool.equals (in1, in2)) then i1::acc else acc)
        val flips = loop (ni1, Vector.toList fs1, ni2, Vector.toList fs2, [])
        val ir = IR {neginf = ni, flips = Vector.fromList flips}
      in ir
      end

  fun union (ir1, ir2) = complement (intersection (complement ir1, complement ir2))

  local
    structure L = Layout
    structure LU = LayoutUtils
  in
  fun layout (IR {neginf, flips}) =
      let
        fun r (i', i) = L.seq [IntInf.layout i', L.str "..", IntInf.layout (i - 1)]
        fun loop (prev, flips, acc) =
            case flips
             of [] =>
                (case prev
                  of NONE => acc
                   | SOME NONE => [L.str "-w..w"]
                   | SOME (SOME i) => (L.seq [IntInf.layout i, L.str "..w"])::acc)
              | i::flips =>
                (case prev
                  of NONE => loop (SOME (SOME i), flips, acc)
                   | SOME NONE => 
                     loop (NONE, flips, (L.seq [L.str "-w..", IntInf.layout (i - 1)])::acc)
                   | SOME (SOME i') =>
                     loop (NONE, flips, (if equals (i' + 1, i) then IntInf.layout i' else r (i', i))::acc))
        val ls = loop (if neginf then SOME NONE else NONE, Vector.toList flips, [])
        val l = LU.bracketSeq (List.rev ls)
      in l
      end
  end

end;
*)