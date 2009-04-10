(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

signature INTR =
sig

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

  val empty : t
  val all : t
  val single : IntInf.t -> t

  val check : t -> bool
  val isEmpty : t -> bool

  val complement : t -> t
  val union : t * t -> t
  val intersection : t * t -> t

  val layout : t -> Layout.t

end;

structure Intr :> INTR =
struct

  open IntInf

  datatype t = IR of {neginf : bool, flips : IntInf.t Vector.t}

  val empty = IR {neginf = false, flips = Vector.new0 ()}
  val all = IR {neginf = true, flips = Vector.new0 ()}

  fun single i = IR {neginf = false, flips = Vector.new2 (i, i + 1)}

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
