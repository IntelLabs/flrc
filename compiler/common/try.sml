(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation *)

signature TRY = 
sig
  type 'a t = 'a option

  (* Constructors/combinators.  These create, but do not fail *)
  val try  : (unit -> 'a) -> 'a t
  val lift : ('a -> 'b) -> ('a -> 'b t)
  val exec : (unit -> unit) -> unit
  val ||   : ('a -> 'b) * ('a -> 'b) -> ('a -> 'b t) (* use infix 4 *)
  val or   : ('a -> 'b t) * ('a -> 'b t) -> ('a -> 'b t) (* use infix 4 *)
  val oo : ('b -> 'c t) * ('a -> 'b t) -> ('a -> 'c t)  (* use infix 3 *)
  val om : ('b -> 'c) * ('a -> 'b t) -> ('a -> 'c t)  (* use infix 3 *)
  val success : 'a -> 'a t
  val failure : unit -> 'a t
  val ? : bool -> unit t 
  val otherwise : 'a t * 'a -> 'a
  val bool : 'a t -> bool
  val option : 'a t -> 'a option
 

  (* Destructors.  These all might fail *)
  val fail : unit -> 'a
  val <- : 'a t -> 'a
  val <@ : ('a -> 'b t) -> 'a -> 'b 
  val << : ('b -> 'c t) * ('a -> 'b t) -> ('a -> 'c)  (* use infix 3 *)
  val <! : ('b -> 'c) * ('a -> 'b t) -> ('a -> 'c)  (* use infix 3 *)
  val when : bool -> (unit -> 'a) -> 'a
  val require : bool -> unit
  val not : bool -> unit

  structure V : sig
    val sub : 'a Vector.t * int -> 'a
    val singleton : 'a Vector.t -> 'a
    val lenEq : 'a Vector.t * int -> unit
    val isEmpty : 'a Vector.t -> unit
  end

end

structure Try :> TRY = 
struct

                 
  type 'a t = 'a option
  exception Fail

  fun try f = 
      ((SOME (f())) handle Fail => NONE)

  val lift = 
   fn f => fn a => try (fn () => f a)

  fun exec f = ignore (try(f))

  fun || (f, g) =
   fn arg => 
      (case try (fn () => f arg)
        of NONE => try (fn () => g arg)
         | ans => ans)

  val or = 
   fn (f1, f2) => 
   fn args => 
      (case f1 args
        of NONE => f2 args
         | r => r)

  val oo = 
   fn (f, g) => 
   fn x => 
      (case g x
        of SOME y => f y
         | NONE => NONE)

  val om = 
   fn (f, g) => 
   fn x => 
      (case g x
        of SOME y => SOME (f y)
         | NONE => NONE)
      
  fun success a = SOME a
  fun failure a = NONE
  fun fail () = raise Fail
  fun <- t = 
      (case t
        of SOME a => a
         | NONE => raise Fail)

  val <@ = 
   fn f => 
   fn x => <- (f x)

  val << = 
   fn (f, g) => (<@ f) o (<@ g)

  val <! = fn (f, g) => f o <@ g

  fun otherwise (at, a) = 
      (case at
        of SOME b => b
         | NONE => a)

  fun ? b = if b then SOME () else NONE
  fun bool t = isSome t
  fun option t = t
  val require = <- o ?
  val not = <- o ? o not
  fun when b f = (require b;f())

  structure V = 
  struct
    fun sub (v, i) = 
        let
          val () = require ((i >= 0) andalso (i < Vector.length v))
          val r = Vector.sub (v, i)
        in r
        end

    fun singleton v = 
        let
          val () = require (Vector.length v = 1)
          val r = Vector.sub (v, 0)
        in r
        end

    fun lenEq (v, i) = require (Vector.length v = i)

    val isEmpty 
      = fn v => lenEq (v, 0)
  end

end
