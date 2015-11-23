(* The Haskell Research Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* This provides a syntactically lightweight analog to the failure (option) monad.
 * We implement failure using a canonical exception, and provide combinators
 * for mediating between failure via exception ('a t) and failure via the option 
 * type ('a option).  
 * The type 'a t is purely for documentation purposes.  It marks combinators 
 * which may raise the failure exception instad of returning a value.
 * 
 * CODING CONVENTIONS
 * This module provides syntactic convenience, at the expense of not preventing
 * abusive uses.  For clarity of code, it is generally best to avoid propogating
 * failure inter-procedurally.  The intended use is that failure via exception 
 * ('a t) is used to escape from a function body, but that failure via the option
 * type is used to communicate between functions.
 * Another way of saying this is that the only user code with type 'a -> 'b t
 * should occur syntactically as an argument to Try.try or one of the other
 * combinators that wrap up failure via exception as failure via the option type.
 *)

signature TRY = 
sig
  (* 'a t marks combinators or functions which may fail (may throw the failure exception).
   *)
  type 'a t = 'a

  (* This combinator takes a function which fails (via exceptions) and reifies the
   * result into the option type by representing failure as NONE, and sucess with
   * result A as SOME A.  All failing user code should be a syntactic argument
   * to this combinator (or one of it's variants) 
   *)
  val lift : ('a -> 'b t) -> ('a -> 'b option)
  (* try f == lift f () *)
  val try  : (unit -> 'a t) -> 'a option
  (* exec f = ignore (try f) *)
  val exec : (unit -> unit t) -> unit

  val ||   : ('a -> 'b t) * ('a -> 'b t) -> ('a -> 'b option) (* use infix 4 *)
  val or   : ('a -> 'b option) * ('a -> 'b option) -> ('a -> 'b option) (* use infix 4 *)
  val orL  : ('a -> 'b option) list -> ('a -> 'b option)
  val oo   : ('b -> 'c option) * ('a -> 'b option) -> ('a -> 'c option)  (* use infix 3 *)
  val om   : ('b -> 'c t) * ('a -> 'b option) -> ('a -> 'c option)  (* use infix 3 *)

  val combine : ('a -> 'b t) list * ('a -> 'b) -> ('a -> 'b)
  val combineDo : (unit -> 'a t) list * (unit -> 'a) -> 'a

  (* Fail (via an exception) *)
  val fail : unit -> 'a t
  (* Reify the option type into failure *)
  val <-   : 'a option -> 'a t
  (* <@ f x == <- (f x) *)
  val <@ : ('a -> 'b option) -> ('a -> 'b t)
  (* f << g == (<@ f) o (<@ g) 
   * (f << g) x == <- (f (<- (g x))) 
   *)
  val << : ('b -> 'c option) * ('a -> 'b option) -> ('a -> 'c t)  (* use infix 3 *)
  (* f <! g == f o (<@ g) 
   * (f <! g) x == (f (<- (g x)))
   *)
  val <! : ('b -> 'c t) * ('a -> 'b option) -> ('a -> 'c t)  (* use infix 3 *)
  (* Fail when false *)
  val require    : bool -> unit t

  structure V : sig
    val sub : 'a Vector.t * int -> 'a t
    val singleton : 'a Vector.t -> 'a t
    val doubleton : 'a Vector.t -> ('a * 'a) t
    val tripleton : 'a Vector.t -> ('a * 'a * 'a) t
    val lenEq : 'a Vector.t * int -> unit t
    val isEmpty : 'a Vector.t -> unit t
  end

end

structure Try :> TRY = 
struct

  type 'a t = 'a

  exception Fail

  val lift = 
   fn f => fn a => (SOME (f a)) handle Fail => NONE

  val try =
      fn f => lift f ()

  val exec =
      fn f => ignore (try f)

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

  fun orL fs = List.fold (fs, fn _ => NONE, or)

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

  fun combine (fs, g) =
      List.foldr (fs, g, fn (f, g) => fn arg => case try (fn () => f arg) of NONE => g arg | SOME res => res)

  fun combineDo (fs, g) =
      List.foldr (fs, g, fn (f, g) => fn () => case try f of NONE => g () | SOME res => res) ()

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

  val require = 
   fn b => if b then () else fail ()

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

    fun doubleton v = 
        let
          val () = require (Vector.length v = 2)
          val r1 = Vector.sub (v, 0)
          val r2 = Vector.sub (v, 1)
        in (r1, r2)
        end

    fun tripleton v = 
        let
          val () = require (Vector.length v = 3)
          val r1 = Vector.sub (v, 0)
          val r2 = Vector.sub (v, 1)
          val r3 = Vector.sub (v, 2)
        in (r1, r2, r3)
        end

    fun lenEq (v, i) = require (Vector.length v = i)

    val isEmpty 
      = fn v => lenEq (v, 0)
  end

end
