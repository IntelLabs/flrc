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


signature LUB =
sig

  type 'a lubber = ('a * 'a) -> 'a option
  val option : 'a lubber -> 'a option lubber
  val vector : 'a lubber ->  'a Vector.t lubber
  val pair   : ('a lubber * 'b lubber) -> ('a * 'b) lubber
  val triple : ('a lubber * 'b lubber * 'c lubber) -> ('a * 'b * 'c) lubber
  (* (equal eq) (a, b) => SOME c iff eq (a, b) 
   * where c is one of a or b *)
  val equal : ('a * 'a -> bool) -> 'a lubber
  (* (cmp f) = (equal (fn p => (cmp p) = EQUAL)) *)
  val cmp : ('a * 'a -> order) -> 'a lubber

  val pairWise : 
      ('dict * 'dict * (('key * 'elt option * 'elt option ) -> 'elt) -> 'dict) -> 'elt lubber -> 'dict lubber

  (* No widening *)
  val pairWiseStrict : 
      ('dict * 'dict * (('key * 'elt option * 'elt option) -> 'elt) -> 'dict) -> 'elt lubber -> 'dict lubber

end

structure Lub :> LUB = 
struct

  type 'a lubber = ('a * 'a)
                   -> 'a option

  val vector = 
   fn f => 
   fn (v1, v2) => 
      Try.try
        (fn () => 
            let
              val () = Try.require (Vector.length v1 = Vector.length v2)
              val v3 = Vector.map2 (v1, v2, Try.<- o f)
            in v3
            end)

  val cmp = 
   fn cmp => 
   fn (t1, t2) =>
      case cmp (t1, t2)
       of EQUAL => SOME t1
        | _ => NONE

  val fail = 
   fn (fname, msg) => Fail.fail ("lub.sml", fname, msg)

  val equal = 
   fn eq => 
   fn (t1, t2) =>
      if eq (t1, t2) then
        SOME t1
      else
        NONE

  val option = 
   fn f => 
   fn (t1, t2) => 
      Try.try
        (fn () => 
            case (t1, t2)
             of (SOME a, SOME b) => SOME (Try.<- (f (a, b)))
              | (NONE, NONE) => NONE
              |  _ => Try.fail ())

  val triple = 
   fn (f1, f2, f3) => 
   fn ((a1, b1, c1), 
       (a2, b2, c2)) => 
      Try.try
        (fn () => 
            (Try.<- (f1 (a1, a2)),
             Try.<- (f2 (b1, b2)),
             Try.<- (f3 (c1, c2))))

  val pair = 
   fn (f1, f2) => 
   fn ((a1, b1), 
       (a2, b2)) => 
      Try.try
        (fn () => 
            (Try.<- (f1 (a1, a2)),
             Try.<- (f2 (b1, b2))))

  val pairWise = 
   fn map2 => 
   fn lub => 
   fn (d1, d2) =>
      Try.try 
        (fn () =>
            let
              val help = 
               fn (k, a, b) => 
                  (case (a, b)
                    of (SOME a, SOME b) => Try.<- (lub (a, b))
                     | (SOME a, NONE) => a
                     | (NONE, SOME b) => b
                     | (NONE, NONE) => fail ("Lub", "Bad map2"))
            in map2(d1, d2, help)
            end)

  val pairWiseStrict =
   fn map2 => 
   fn lub => 
   fn (d1, d2) =>
      Try.try 
        (fn () =>
            let
              val help = 
               fn (k, a, b) => 
                  let
                    val a = Try.<- a
                    val b = Try.<- b
                  in Try.<- (lub (a, b))
                  end
            in map2(d1, d2, help)
            end)
end
