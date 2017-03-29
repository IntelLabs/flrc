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

(*
 * Abstract Domain with bottom, top, function, and tuples.
 *)
signature ABS_DOMAIN = 
sig
    type t
    val layout : t -> Layout.t
    val top : t
    val bottom : t
    val tuple : t list -> t
    val func : (t -> t) -> t
    val isBottom : t -> bool
    val isTop : t -> bool
    val isTuple : t -> (t list) option
    val isFunc : t -> (t -> t) option
    val glb : t list -> t
    val lub : t list -> t
end

structure Pointed : ABS_DOMAIN = 
struct
  datatype t = Bottom | Top | Func of t -> t | Tuple of t list

  val top = Top
  val bottom = Bottom
  val tuple = fn xs => Tuple xs
  val func = fn f => Func f

  fun isBottom Bottom = true
    | isBottom _ = false

  fun isTop Top = true
    | isTop _ = false

  fun isTuple (Tuple xs) = SOME xs
    | isTuple _ = NONE

  fun isFunc (Func f) = SOME f
    | isFunc _ = NONE

  val zip = List.zip

  fun lower (Top, x) = x
    | lower (x, Top) = x
    | lower (Bottom, x) = Bottom
    | lower (x, Bottom) = Bottom
    | lower (Tuple xs, Tuple ys) = Tuple (List.map (zip (xs, ys), lower))
    | lower (Func f, Func g) = Func (fn x => lower (f x, g x))
    | lower _ = Bottom

  fun upper (Top, x) = Top 
    | upper (x, Top) = Top
    | upper (Bottom, x) = x
    | upper (x, Bottom) = x
    | upper (Tuple xs, Tuple ys) = Tuple (List.map (zip (xs, ys), upper))
    | upper (Func f, Func g) = Func (fn x => upper (f x, g x))
    | upper _ = Top

  fun glb [] = Fail.fail ("Pointed", "glb", "argument is empty list")
    | glb (x::xs) = List.fold (xs, x, lower)

  fun lub [] = Fail.fail ( "Pointed", "lub", "argument is empty list")
    | lub (x::xs) = List.fold (xs, x, upper)

  fun layout Top = Layout.str "%top"
    | layout Bottom = Layout.str "%bot"
    | layout (Func f) = Layout.str "%func"
    | layout (Tuple l) = LayoutUtils.sequence ("<", ">", ",") (List.map (l, layout))

end

