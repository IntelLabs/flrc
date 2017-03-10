(* The Haskell Research Compiler *)
(*
 * Redistribution and use in source and binary forms, with or without modification, are permitted 
 * provided that the following conditions are met:
 * 1.   Redistributions of source code must retain the above copyright notice, this list of 
 * conditions and the following disclaimer.
 * 2.   Redistributions in binary form must reproduce the above copyright notice, this list of
 * conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 * BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)


(* This defines a class of sequences parameterized over an arbitrary
 * element type. Sequences over a type t are of the form:
 * S ::= ... | [t] | | <> | < t, S>
 *  where ... is a bottom element (any sequence)
 *  [a] describes a homogenous sequence of zero or more of a
 *  <> describes a zero length sequence
 *  <a, S> describes a sequence of length >= 1 consisting of an a followed by S
 *
 * Or in terms of sets of sequences S of elements of t:
 * let S* be the set of all finite sequences of elements of t
 *   | <> | = <>
 *   | ... | = S*
 *   | [a] | = {<>} U { <a, s> s.t. s in |[a]|}
 *   | <a, S> | = {<a, s> s.t. s in |S|}
 *
 * Sequences have a least upper bound operation which returns an upper bound
 * where it exists, using an inclusion of the least upper bound for the
 * element type.  Least upper bounds for sequences correspond to set intersection
 * in the interpretation given above, with the empty set corresponding to a return
 * value of NONE.
 * As an example, if we use an element type with two unrelated elements a and b,
 * then the lattice induced by the lub operation is (schematically)
 * as follows:
 * 
 *     ----------- <> --------------------
 *     |                                 |
 *                 etc
 *     |                                 |
 *     |     <a, a>                      |
 *     |      \        /                 |
 *     |      <a, a, [a]>                |
 *     |  <a>   /    \     /             |
 *     |    \  /     <a, a, ...>   /     |
 *     |  <a, [a]>  |   <a, b, ...>      |
 *     |  /    \    |  /           /     |
 *    [a]      < a, ...>   <b, ...>     [b]
 *     \            \     /            /
 *      -----------  <...> ------------
 *
 * If the element type has a non-trivial lattice structure, then
 * the above is extended with additional points and edges using
 * the pointwise relation induced by the lattice structure of the 
 * element type.
 *)
                                          

signature MIL_REP_SEQ = sig

  datatype 'a t = 
           Seq of 'a            (* [a] *)
         | SeqCons of 'a * 'a t (* <a, S> *)
         | SeqZ                 (* <> *)
         | SeqBot               (* ... *)

  (* The sequence elements plus the terminator, where
   * NONE => SeqBot
   * SOME NONE => SeqZ
   * SOME (SOME a) => Seq a
   *)
  val deconstruct : 'a t -> ('a Vector.t * 'a option option)
           
  val lub : ('a * 'a -> 'a option) -> (('a t * 'a t) -> 'a t option)

  val union : ('a t * 'a t) -> 'a t * (('a * 'a) list)

  val layout : ('a -> Layout.t) -> ('a t -> Layout.t)
                  
  val fold : 'a t * 'acc * ('a * 'acc -> 'acc) -> 'acc

  val foreach : 'a t * ('a -> unit) -> unit

  val filter : 'a t * ('a -> bool) -> 'a t

  (* Given a sequence of possibly fixed length, return
   * the sequence denoting all extensions with the same prefix
   * elements.
   *)
  val seq2OpenSeq : 'a t -> 'a t

  val fromVectorMap : 'a Vector.t * ('a -> 'b) -> 'b t

  val fromVectorOpen : 'a Vector.t -> 'a t
  val fromVectorClosed : 'a Vector.t -> 'a t
  val fromVector : 'a Vector.t * 'a t -> 'a t

 (* tabulate (length, term, f) => <a0, <a1, ... <ai, term>>> 
  *   where i = length-1.
  *)
  val tabulate : int * 'a t * (int -> 'a) -> 'a t

  val seq0 : 'a t
  val seq1 : 'a -> 'a t
  val seq2 : ('a * 'a) -> 'a t
  val seq3 : ('a * 'a * 'a) -> 'a t
  val seq4 : ('a * 'a * 'a * 'a) -> 'a t

  val equal : ('a * 'a -> bool) -> ('a t * 'a t) -> bool

end

structure MilRepSeq :> MIL_REP_SEQ = 
struct
  structure L = Layout
  structure LU = LayoutUtils

  datatype 'a t = 
           Seq of 'a            (* [a] *)
         | SeqCons of 'a * 'a t (* <a, S> *)
         | SeqZ                 (* <> *)
         | SeqBot               (* ... *)

  val deconstruct =
   fn s => 
      let
        val rec loop = 
         fn (acc, s) => 
            (case s
              of Seq a => (acc, SOME (SOME a))
               | SeqZ => (acc, SOME NONE)
               | SeqBot => (acc, NONE)
               | SeqCons (a, s) => loop (a::acc, s))
        val (acc, terminator) = loop ([], s)
        val elts = Vector.fromListRev acc
      in (elts, terminator)
      end

  val rec lub = 
   fn eltLub => 
      Try.lift
      (fn (s1, s2) => 
          let
            val eltLub' = Try.<- o eltLub
            val lub' = Try.<- o (lub eltLub)
            val s = 
                (case (s1, s2)
                  of (SeqBot, _) => s2
                   | (_, SeqBot) => s1

                   | (Seq a1, Seq a2) => (case eltLub (a1, a2)
                                           of SOME a => Seq a
                                            | NONE => SeqZ)

                   | (Seq a1, SeqZ) => SeqZ
                   | (SeqZ, Seq a2) => SeqZ

                   | (Seq a1, SeqCons (a2, s3)) => SeqCons (eltLub' (a1, a2), lub' (s1, s3))
                   | (SeqCons (a1, s3), Seq a2) => SeqCons (eltLub' (a1, a2), lub' (s3, s2))

                   | (SeqCons (a1, s1), SeqCons (a2, s2)) =>  SeqCons (eltLub' (a1, a2), lub' (s1, s2))

                   | (SeqCons _, SeqZ) => Try.fail ()
                   | (SeqZ, SeqCons _) => Try.fail ()

                   | (SeqZ, SeqZ)     => SeqZ)
          in s
          end)

  val rec union = 
      fn (s1, s2) => 
         let
           val add = 
            fn ((s, edges2), edges1) => (s, edges1 @ edges2)
           val s = 
               (case (s1, s2)
                 of (SeqBot, _) => (s2, [])
                  | (_, SeqBot) => (s1, [])
                                   
                  | (Seq a1, Seq a2) => (Seq a1, [(a1, a2)])
                  | (Seq a1, SeqZ) => (Seq a1, [])
                  | (SeqZ, Seq a2) => (Seq a2, [])

                  | (Seq a1, SeqCons (a2, s3)) => add (union (s1, s3), [(a1, a2)])
                  | (SeqCons (a1, s3), Seq a2) => add (union (s3, s2), [(a1, a2)])

                  | (SeqCons (a1, s1), SeqCons (a2, s2)) => 
                    let
                      val (s, edges) = union (s1, s2)
                      val edges = (a1, a2) :: edges
                    in (SeqCons (a1, s), edges)
                    end

                  | (SeqCons (a1, s), SeqZ) => (s1, []) (*union (Seq a1, s)*)
                  | (SeqZ, SeqCons (a1, s)) => (s2, []) (*union (s, Seq a1)*)

                  | (SeqZ, SeqZ)     => (SeqZ, []))
            in s
            end
      
  val layout = 
   fn layoutElt => 
      (fn s => 
          let
            val rec gather = 
             fn s => 
                (case s
                  of SeqZ => []
                   | SeqBot => [L.str "..."]
                   | SeqCons (a, b) => (layoutElt a)::(gather b)
                   | _ => [layout' s])
            and rec layout' = 
             fn s => 
                (case s
                  of SeqZ => L.str "<>"
                   | SeqBot => L.str "<...>"
                   | Seq s => L.seq [L.str "<.", 
                                     layoutElt s,
                                     L.str ".>"]
                   | SeqCons _ => 
                     let
                       val elts = gather s
                       val elts = L.separateRight (elts, ",")
                     in
                       L.align [L.str "<",
                                LU.indent (L.mayAlign elts),
                                L.str ">"]
                     end)
          in layout' s
          end)

  val rec fold = 
   fn (seq, a, f) => 
      (case seq
        of Seq e => f (e, a)
         | SeqCons (e, s2) => fold(s2, f (e, a), f)
         | SeqBot => a
         | SeqZ => a)
      
  val rec foreach = 
   fn (seq, f) => 
      (case seq
        of Seq e => f e
         | SeqCons (e, s2) => (f e;
                               foreach(s2, f))
         | SeqZ => ()
         | SeqBot => ())

  val rec filter = 
   fn (seq, f) => 
      (case seq
        of Seq e => if f e then SeqZ else seq
         | SeqCons (e, s2) => 
           let
             val s2 = filter (s2, f)
           in
             if f e then s2 else SeqCons (e, s2)
           end
         | SeqZ => seq
         | SeqBot => seq)

  val rec seq2OpenSeq = 
   fn seq => 
      case seq
       of Seq _ => seq
        | SeqCons (e, s2) => SeqCons(e, seq2OpenSeq s2)
        | SeqZ => SeqBot
        | SeqBot => seq

  val fromVectorMap = 
   fn (vec, f) => Vector.foldr (vec, SeqZ, 
                                (fn (e, s) => SeqCons(f e, s)))

  val fromVector = 
   fn (vec, term) => Vector.foldr (vec, term, 
                                   (fn (e, s) => SeqCons(e, s)))
  val fromVectorClosed =
   fn vec => fromVector (vec, SeqZ)

  val fromVectorOpen =
   fn vec => fromVector (vec, SeqBot)

  val tabulate = 
   fn (count, base, f) => 
      let
        val rec loop = 
         fn (index) => 
            if index >= count then
              base
            else
              SeqCons (f index, loop (index + 1))
      in loop 0
      end

  val seq0 = SeqZ
  val seq1 = fn a => SeqCons (a, SeqZ)
  val seq2 = fn (a, b) => SeqCons (a, seq1 b)
  val seq3 = fn (a, b, c) => SeqCons (a, seq2 (b, c))
  val seq4 = fn (a, b, c, d) => SeqCons (a, seq3 (b, c, d))

  val equal = 
   fn equalA => 
      let
        val rec eq = 
         fn (s1, s2) => 
            let
              val isEqual = 
                  (case (s1, s2)
                    of (Seq a1, Seq a2) => equalA (a1, a2)
                     | (SeqCons (a1, s1), SeqCons (a2, s2)) => 
                       equalA (a1, a2) andalso eq (s1, s2)
                     | (SeqZ, SeqZ) => true
                     | (SeqBot, SeqBot) => true
                     | _ => false)
            in isEqual
            end
      in eq
      end
end (* structure MilRepSeq *)
