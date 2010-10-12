(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

(* Utility stuff not in the MLton library *)

(* This allows us to use the MLton extended Int type, while
 * checking that we are using 32 bit ints.  If 32 bit ints are
 * not required, just use Int.  We should revisit this.  *)
structure Int32 :> INTEGER where type t = Int32.int = Int
structure Word32 = Word32
structure Utils = struct

    (* General utilities *)
    fun flip2 (a,b) = (b,a) 

    structure Imperative = 
    struct
      val block : unit list -> unit = fn l => ()
    end

    structure Function = 
    struct
      val id : 'a -> 'a = fn x => x
      val flipIn : ('a * 'b -> 'c) -> ('b * 'a -> 'c) = 
       fn f => f o flip2
      val flipOut : ('a -> 'b * 'c) -> ('a -> 'c * 'b) = 
       fn f => flip2 o f
      val flip : ('a * 'b -> 'c * 'd) -> ('b * 'a -> 'd * 'c) = 
       fn f => flip2 o f o flip2
               
      val disj : ('a -> bool) * ('a -> bool) -> ('a -> bool) = 
       fn (f1, f2) => 
       fn a => (f1 a) orelse (f2 a)
      val conj : ('a -> bool) * ('a -> bool) -> ('a -> bool) =
       fn (f1, f2) => 
       fn a => (f1 a) andalso (f2 a)

      val apply2 : ('a -> 'b) -> ('a * 'a) -> ('b * 'b) = 
       fn f => fn (a, b) => (f a, f b)
                 
      (* infix 3 @@ *)
      val @@ : ('a -> 'b) * 'a -> 'b = 
       fn (f, a) => f a

      (*(* infix left-associative application *)
      infix 3 $
      val $ : 'a -> ('a -> 'b) -> 'b =
         fn f => (fn x => f x)*)


      (* MLton sectioning and application operators *)
      (* infix 3 <\ \> *)
      val <\ = fn (x, f) => fn y => f (x, y)
      val \> = fn (f, y) => f y
      (* infixr 3 /> </ *)    
      val /> = fn (f, y) => fn x => f (x, y)
      val </ = fn (x, f) => f x

     (* infix 1 >| *)
      val >| = </
     (* infixr 1 |< *)
      val |< = \>
      val ` = fn f => fn x => fn () => f x
    end (* structure Function *)

    structure MltonVector = Vector
    structure Vector = 
    struct 
      val cons : 'a * 'a vector -> 'a vector = 
       fn (a, v) => Vector.concat [Vector.new1 a, v]
      fun snoc (v, a) = Vector.concat [v, Vector.new1 a]
      val update : 'a vector * int * 'a -> 'a vector = 
       fn (vec, i , elem) =>
          Vector.mapi (vec, fn(i', elem') => if (i = i') then elem else elem')
      val count : 'a vector * ('a -> bool) -> int = 
       fn (vec, p) =>
          Vector.fold (vec, 0, fn (a, i) => if p a then i+1 else i)
      val split : 'a vector * int -> 'a vector * 'a vector = 
       fn (vec, i) =>
          let
            val a = Vector.prefix (vec, i)
            val b = Vector.dropPrefix (vec, i)
          in (a, b)
          end
      val toListOnto : 'a vector * 'a list -> 'a list = 
       fn (v, l) => Vector.foldr (v, l, op ::)
      val toListMap2 : 'a vector * 'b vector * (('a * 'b) ->  'c) -> 'c list =
       fn (v1, v2, f) => Vector.foldr2 (v1, v2, [], fn (a, b, l) => f (a, b) :: l)
      val rec concatToList : 'a vector list -> 'a list = 
       fn vl => 
          (case vl
            of [] => []
             | v::vl => toListOnto (v, concatToList vl))
      val lookup : 'a vector * int -> 'a option = 
       fn (v, i) => (SOME (Vector.sub (v, i))) handle Subscript => NONE
      val allEq : 'a vector * ('a * 'a -> bool) -> bool = 
          fn (v, eq) => 
             if Vector.isEmpty v then
               true
             else
               let
                 val b = Vector.sub (v, 0)
                 val equal = Vector.forall (v, fn a => eq (a, b))
               in equal
               end
      val transpose : 'a vector vector -> 'a vector vector = 
          fn v =>
             let
               val newCols = Vector.length v
               val newRows = if newCols > 0 then 
                               Vector.length (Vector.sub (v, 0))
                             else 
                               0
               val oldElt = 
                fn (row, col) => 
                   Vector.sub (Vector.sub (v, row), col)
               val v = Vector.tabulate (newRows, fn row => Vector.tabulate (newCols, fn col => oldElt (col, row)))
             in v
             end
      val fromOption : 'a option -> 'a vector = 
       fn opt => 
          (case opt
            of SOME a => Vector.new1 a
             | NONE => Vector.new0 ())
    end (* structure Vector *)

    structure MltonList = List

    structure List = 
    struct
    (* List utilities *)

    fun concatOption (l : 'a option MltonList.t) : 'a MltonList.t = MltonList.keepAllMap (l, Function.id)

    fun allEq eq l = 
        (case l
          of [] => true
           | a::aa =>  
             let
               val eq1 = 
                fn b => eq(a, b)
               val eq = 
                   MltonList.forall (aa, eq1)
             in eq
             end)

    fun unzip3 l =
        let
          fun doOne ((a, b, c), (l1, l2, l3)) = (a::l1, b::l2, c::l3)
          val (l1, l2, l3) = MltonList.fold (l, ([], [], []), doOne)
          val l1 = MltonList.rev l1
          val l2 = MltonList.rev l2
          val l3 = MltonList.rev l3
        in (l1, l2, l3)
        end

    fun mapFoldl (l, ix, f) =
        let fun aux (item, (cx, a)) =
                let val (nitem, nx) = f (item, cx) in (nx, nitem::a) end
          val (fx, l) = MltonList.fold (l, (ix, []), aux)
        in
          (MltonList.rev l, fx)
        end

    fun mapFoldli (l, ix, f) =
        let fun aux (item, (cx, i, a)) =
                let val (nitem, nx) = f (i, item, cx)
                in
                  (nx, i+1, nitem::a)
                end
          val (fx, _, l) = MltonList.fold (l, (ix, 0, []), aux)
        in
          (MltonList.rev l, fx)
        end

    fun consIf (b, a, l) = if a then a::l else l

    fun uniqueList ([], equal) = []
      | uniqueList ([x], equal) = [x]
      | uniqueList (x::xs, equal) = 
        if MltonList.exists (xs, fn n => equal (x, n)) then
          uniqueList (xs, equal)
        else
          x::(uniqueList (xs, equal))

    fun fromTree t = Tree.foldPost (t, [], fn (x, l) => x::l)

    end (* structure List *)

    structure Option = 
    struct
      val out : 'a option * (unit -> 'a) -> 'a = 
       fn (opt, f) => 
          (case opt
            of SOME v => v
             | NONE => f())

      val get : 'a option * 'a -> 'a =
       fn (opt, d) => 
          (case opt
            of NONE => d
             | SOME x => x)

      val bind : 'a option * ('a -> 'b option) -> 'b option =
       fn (opt, f) => 
          (case opt
            of NONE => NONE
             | SOME v => f v)

      val dispatch : 'a option * ('a -> 'b) * (unit -> 'b) -> 'b = 
       fn (opt, f1, f2) => 
          (case opt
            of SOME a => f1 a
             | NONE => f2 ())

      val dispatchMap : 'a option * ('a -> 'b) * (unit -> unit) -> 'b option = 
       fn (opt, f1, f2) => 
          (case opt
            of SOME a => SOME (f1 a)
             | NONE => (f2 (); NONE))

      val compose : ('b -> 'c option) * ('a -> 'b option) -> ('a -> 'c option) =
       fn (f, g) =>
       fn a =>
          (case g a
            of SOME b => f b
             | NONE => NONE)

      (* If at most one are present, return it *)
      val atMostOneOf : ('a option) * ('a option) -> 'a option = 
       fn p => 
          (case p
            of (NONE, snd) => snd
             | (fst, NONE) => fst
             | _ => NONE)

      (* If either one are present, return it.  If both present, prefers left *)
      val eitherOneOf : ('a option) * ('a option) -> 'a option = 
       fn (fst, snd) => 
          (case fst
            of NONE => snd
             | _ => fst)

      val map2 : ('a option) * ('b option) * ('a * 'b -> 'c) -> 'c option = 
          fn (o1, o2, f) => 
             (case (o1, o2)
               of (SOME a, SOME b) => SOME (f (a, b))
                | _ => NONE)

      val union : ('a option) * ('a option) * ('a * 'a -> 'a) -> 'a option = 
          fn (o1, o2, f) => 
             (case (o1, o2)
               of (a, NONE) => a
                | (NONE, b) => b
                | (SOME a, SOME b) => SOME (f (a, b)))

      val fromVector : 'a vector -> 'a option option = 
       fn v => 
          (case MltonVector.length v
            of 0 => SOME NONE
             | 1 => SOME (SOME (MltonVector.sub (v, 0)))
             | _ => NONE)

      val toList : 'a option -> 'a list = 
       fn opt => 
          (case opt
            of SOME a => [a]
             | NONE => [])

    end (* structure Option *)

    structure Ref = 
    struct
      val inc : int ref -> int = 
       fn (r as ref i) => (r := i + 1;i)

      val dec : int ref -> int = 
       fn (r as ref i) => (r := i - 1;i)

    end (* structure Ref *)

    (* Numeric stuff *)

    fun wordToReal32 (w : LargeWord.t) : Real32.t =
        let
          val a32 = Word8Array.tabulate (4, fn _ => 0wx0)
          val () = PackWord32Little.update (a32, 0, w)
          val r = PackReal32Little.subArr (a32, 0)
        in
          r
        end
    fun wordToReal64 (w : LargeWord.t) : Real64.t =
        let
          val a64 = Word8Array.tabulate (8, fn _ => 0wx0)
          val () = PackWord64Little.update (a64, 0, w)
          val r = PackReal64Little.subArr (a64, 0)
        in
          r
        end

    fun real32ToWord (r : Real32.t) : LargeWord.t =
        let
          val a32 = Word8Array.tabulate (4, fn _ => 0wx0)
          val () = PackReal32Little.update (a32, 0, r)
          val w = PackWord32Little.subArr (a32, 0)
        in w
        end
    fun real64ToWord (r : Real64.t) : LargeWord.t =
        let
          val a32 = Word8Array.tabulate (8, fn _ => 0wx0)
          val () = PackReal64Little.update (a32, 0, r)
          val w = PackWord64Little.subArr (a32, 0)
        in w
        end

    (* Return a list of 32 bit digits (msd first) corresponding
     * to the base 2^32 representation of the absolute value of the 
     * number.  Returns [] on zero *)
    fun intInfAbsDigits32 (i : IntInf.t) : Word32.word MltonList.t = 
        let
          val i = IntInf.abs i
          val base = IntInf.pow (2, 32)
          val rec loop = 
           fn (i, l) => 
              if i = 0 then 
                l
              else 
                loop (i div base, Word32.fromLargeInt (i mod base)::l)
        in loop (i, [])
        end


    datatype ('a, 'b) oneof = Inl of 'a | Inr of 'b

    (* Grow arr such that idx is in range (if not already in range) *)
    (* New fields are filled with the default element*)
    fun growArrayR (arrR : 'a Array.t ref, idx : int, dflt : 'a) : unit = 
        if idx < Array.length (!arrR) then ()
        else
          let
            val len = 2 * idx + 1 (* Handle zero length arrays *)
            val arr = !arrR
            val narr = Array.new(len, dflt)
            val () = Array.foreachi(arr, fn (i, a) => Array.update(narr, i, a))
            val () = arrR := narr
          in ()
          end

    fun count i = MltonList.tabulate(i, fn i => i)

end;  (* structure Utils *)

structure LayoutUtils = struct

    structure L = Layout

    val indentAmount = 2

    fun indent l = L.indent (l, indentAmount)

    val space = L.str " "

    fun char c = L.str (String.fromChar c)

    fun layoutOption f ov =
        case ov of
          SOME v => f v
        | NONE => L.str "None"
                     
    fun layoutBool b =
        if b then L.str "True"
        else L.str "False"

    fun layoutBool' b =
        if b then L.str "1"
        else L.str "0"

    fun sequence (start, finish, sep) ts =
        L.seq [L.str start, L.mayAlign (L.separateRight (ts, sep)),
               L.str finish]

    fun parenSeq   bs = sequence ("(",")",",") bs
    fun bracketSeq bs = sequence ("[","]",",") bs
    fun braceSeq   bs = sequence ("{","}",",") bs
    fun angleSeq   bs = sequence ("<",">",",") bs

    fun paren        b = L.seq [L.str "(", b, L.str ")"]
    fun bracket      b = L.seq [L.str "[", b, L.str "]"]
    fun brace        b = L.seq [L.str "{", b, L.str "}"]
    fun angleBracket b = L.seq [L.str "<", b, L.str ">"]

    fun spaceSeparated l = L.separateRight (l, " ")

    fun printLayoutToStream (l, s) = Layout.outputWidth (Layout.seq [l, Layout.str "\n"], 115, s)

    fun printLayout l =
        (printLayoutToStream(l, Pervasive.TextIO.stdOut); Pervasive.TextIO.flushOut Pervasive.TextIO.stdOut)

    fun writeLayout' (l: Layout.t, fname: string, append: bool) =
        let
          val os = if append then Pervasive.TextIO.openAppend fname else Pervasive.TextIO.openOut fname
          val () = printLayoutToStream(l, os)
	  val () = Pervasive.TextIO.closeOut os
        in ()
        end

    fun writeLayout (l: Layout.t, fname: string) = writeLayout' (l, fname, false)

    fun toString l =
        let
          val ss = ref []
          fun prn s = ss := s::(!ss)
          val () = Layout.print (l, prn)
        in
          String.concat (List.rev (!ss))
        end

end;

signature ORD = sig
  type t
  val compare : t * t -> order
end;

signature SET = sig
    type element
    type t
    val empty : t
    val singleton : element -> t
    val fromList : element list -> t
    val toList : t -> element list
    val fromVector : element vector -> t
    val toVector : t -> element vector
    val isEmpty : t -> bool
    val size : t -> int
    val equal : t * t -> bool
    val compare : t * t -> order
    (* is the first a subset of the second*)
    val isSubset : t * t -> bool
    val insert : t * element -> t
    val insertList : t * element list -> t
    val remove : t * element -> t
    val union : t * t -> t
    val intersection : t * t -> t
    val difference : t * t -> t
    val member : t * element -> bool
    val forall : t * (element -> bool) -> bool
    val exists     : t * (element -> bool) -> bool
    val partition  : t * (element -> bool) -> {no: t, yes: t}
    (* Order of traversal is arbitrary *)
    val foreach : t * (element -> unit) -> unit
    val keepAll : t * (element -> bool) -> t
    (* Order of fold is arbitrary *)
    val fold : t * 'b * (element * 'b -> 'b) -> 'b
    val getAny : t -> element option
    (* Order is arbitrary *)
    val layout : t * (element -> Layout.t) -> Layout.t
end;

functor SetF (Element : ORD)
  :> SET where type element = Element.t =
struct
    type element = Element.t
    structure OrdKey = struct
        type ord_key = element
        val compare = Element.compare
    end
    structure RBT = RedBlackSetFn(OrdKey)
    type t = RBT.set
    val empty = RBT.empty
    val singleton = RBT.singleton
    fun fromList l = RBT.addList (empty, l)
    val toList = RBT.listItems
    fun fromVector v = 
        Vector.fold (v, empty, RBT.add o Utils.flip2)
    val toVector = Vector.fromList o RBT.listItems
    val isEmpty = RBT.isEmpty
    val size = RBT.numItems
    val equal = RBT.equal
    val compare = RBT.compare
    val isSubset = RBT.isSubset
    val insert = RBT.add
    val insertList = RBT.addList
    fun remove (s, e) = RBT.delete (s, e) handle NotFound => s
    fun union (s1, s2) = 
        if isEmpty s1 then s2
        else if isEmpty s2 then s1 
        else RBT.union (s1, s2)
    fun intersection (s1, s2) = 
        if (isEmpty s1) orelse (isEmpty s2) then empty
        else RBT.intersection (s1, s2)
    fun difference (s1, s2) = 
        if isEmpty s1 then empty
        else if isEmpty s2 then s1
        else RBT.difference (s1, s2)
    val member = RBT.member
    fun forall (s, p) = not (RBT.exists (not o p) s)
    fun exists (s, p) = RBT.exists p s
    fun partition (s, p) =
        let
          val (yes, no) = RBT.partition p s
        in
          {yes = yes, no = no}
        end
    fun foreach (s, f) = RBT.app f s
    fun keepAll (s, p) = RBT.filter p s
    fun fold (s, i, f) = RBT.foldl f i s
    fun getAny s = RBT.find (fn _ => true) s
    fun layout (s, f) =
        let
          val l = RBT.listItems s
          val ls = List.map (l, f)
          val s = Layout.sequence ("{", "}", ",") ls
        in s
        end

end;

structure IntSet =
    SetF (struct type t = int val compare = Int.compare end);

structure StringSet =
    SetF (struct type t = string val compare = String.compare end);

signature DICT = sig
    type key
    type 'a t
    val empty : 'a t
    val choose : 'a t -> ('a t * key * 'a) option
    val compare : 'a t * 'a t * ('a * 'a -> order) -> order
    val singleton : key * 'a -> 'a t
    val fromList : (key * 'a) list -> 'a t
    val fromList2 : key list * 'a list -> 'a t
    (* Order of entries is arbitrary *)
    val toList : 'a t -> (key * 'a) list
    val domain : 'a t -> key list
    val range  : 'a t -> 'a list
    val fromVector : (key * 'a) vector -> 'a t
    val toVector : 'a t -> (key * 'a) vector
    val isEmpty : 'a t -> bool
    val size : 'a t -> int
    val lookup   : 'a t * key -> 'a option
    val contains : 'a t * key -> bool
    (* If key already exists then replaces value otherwise adds key *)
    val insert : 'a t * key * 'a -> 'a t
    val insertAll : 'a t * (key * 'a) list -> 'a t
    (* If key does not exist then identity *)
    val remove : 'a t * key -> 'a t
    (* Order of fold is arbitrary *)
    val fold : ('a t * 'b * (key * 'a * 'b -> 'b)) -> 'b
    (* Order of foreach is arbitrary *)
    val foreach : 'a t * (key * 'a -> unit) -> unit
    val map : 'a t * (key * 'a -> 'b) -> 'b t
    val keepAllMap : 'a t * (key * 'a -> 'b option) -> 'b t
    val mapFold :
        'a t * 'b * (key * 'a * 'b -> 'c * 'b) -> 'c t * 'b
    val union : 'a t * 'a t * (key * 'a * 'a -> 'a) -> 'a t
    val intersect : 'a t * 'b t * (key * 'a * 'b -> 'c) -> 'c t
    val forall : 'a t * (key * 'a -> bool) -> bool
    val exists : 'a t * (key * 'a -> bool) -> bool
   (* false if domains are different *)
    val forall2 : 'a t * 'b t * (key * 'a * 'b -> bool) -> bool
    val map2    : 'a t * 'b t * (key * 'a option * 'b option -> 'c option) -> 'c t
    (* Order of entries is arbitrary *)
    val layout : 'a t * (key * 'a -> Layout.t) -> Layout.t
end;

functor DictF (Key : ORD)
  :> DICT where type key = Key.t =
struct
    type key = Key.t
    structure OrdKey = struct
        type ord_key = key
        val compare = Key.compare
    end
    structure RBT = RedBlackMapFn(OrdKey)
    type 'a t = 'a RBT.map
    val empty = RBT.empty
    fun compare (d1, d2, c) = RBT.collate c (d1, d2)
    val singleton = RBT.singleton
    fun fromList l =
        List.fold (l, empty, fn ((k, v), d) => RBT.insert (d, k, v))
    fun fromList2 (l1, l2) =
        List.fold2 (l1, l2, empty, fn (k, v, d) => RBT.insert (d, k, v))
    val toList = RBT.listItemsi
    val domain = RBT.listKeys
    val range = RBT.listItems
    fun fromVector v =
        Vector.fold (v, empty, fn ((k, v), d) => RBT.insert (d, k, v))
    fun toVector d = Vector.fromList (toList d)
    val isEmpty = RBT.isEmpty
    val size = RBT.numItems
    val lookup = RBT.find
    val contains = RBT.inDomain
    val insert = RBT.insert
    fun insertAll (d, l) = List.fold (l, d, fn ((k, v), d) => insert (d, k, v))
    fun remove (d, k) = #1 (RBT.remove (d, k)) handle NotFound => d
    fun fold (d, i, f) = RBT.foldli f i d
    fun forall (d, f) = fold (d, true, fn (k, d, b) => b andalso f (k, d))
    fun exists (d, f) = fold (d, false, fn (k, d, b) => b orelse f (k, d))
    fun foreach (d, f) = RBT.appi f d
    fun map (d, f) = RBT.mapi f d
    fun keepAllMap (d, f) = RBT.mapPartiali f d
    fun mapFold (d, i, f) =
        let
          fun doOne (k, v, (d, i)) =
              let
                val (nv, ni) = f (k, v, i)
              in
                (insert (d, k, nv), ni)
              end
        in
          fold (d, (empty, i), doOne)
        end
    fun union (d1, d2, f) = RBT.unionWithi f (d1, d2)
    fun intersect (d1, d2, f) = RBT.intersectWithi f (d1, d2)
    fun forall2 (d1, d2, f) =
        let
          fun aux (_, NONE,    _      ) = SOME false
            | aux (_, _,       NONE   ) = SOME false
            | aux (k, SOME i1, SOME i2) = SOME (f (k, i1, i2))
          fun band (_, b1, b2) = b1 andalso b2
        in
          fold (RBT.mergeWithi aux (d1, d2), false, band)
        end
    fun map2 (d1, d2, f) =
        RBT.mergeWithi f (d1, d2)
    fun layout (d, f) =
        Layout.sequence ("{", "}", ",") (List.map (RBT.listItemsi d, f))

    val choose : 'a t -> ('a t * key * 'a) option = 
        fn d => 
           case RBT.firsti d
            of SOME (l, a) => SOME (remove (d, l), l, a)
             | NONE => NONE
end;

structure IntDict =
    DictF (struct type t = int val compare = Int.compare end);

structure StringDict =
    DictF (struct type t = string val compare = String.compare end);

structure CharDict =
    DictF (struct type t = char val compare = Char.compare end);

structure IntIntDict = DictF(struct
                               type t = int * int
                               val compare = Compare.pair (Int.compare, Int.compare)
                             end)

signature DICT_IMP =
sig
  type key
  type 'a t

  val empty : unit -> 'a t            
  (* Add all elements of the second to the first*)
  val add : 'a t * 'a t  -> unit
  val addWith : 'a t * 'a t * (key * 'a * 'a -> 'a) -> unit
  val choose : 'a t -> (key * 'a) option 
  val fromList : (key * 'a) list -> 'a t
  val fromList2 : key list * 'a list -> 'a t
  (* Order of entries is arbitrary *)
  val toList : 'a t -> (key * 'a) list
  val range  : 'a t -> 'a list
  val domain : 'a t -> key list
  val fromVector : (key * 'a) vector -> 'a t
  val isEmpty : 'a t -> bool
  val size : 'a t -> int
  val lookup   : 'a t * key -> 'a option
  val contains : 'a t * key -> bool
  (* If key already exists then replaces value otherwise adds key *)
  val insert : 'a t * key * 'a -> unit
  val insertAll : 'a t * (key * 'a) list -> unit
  (* If key does not exist then identity *)
  val remove : 'a t * key -> unit
  (* Order of fold is arbitrary *)
  val fold : ('a t * 'b * (key * 'a * 'b -> 'b)) -> 'b
  (* Order of foreach is arbitrary *)
  val foreach : 'a t * (key * 'a -> unit) -> unit
  (* false if domains are different *)
  val forall2 : 'a t * 'b t * (key * 'a * 'b -> bool) -> bool
  (* Order of entries is arbitrary *)
  val layout : 'a t * (key * 'a -> Layout.t) -> Layout.t
  val modify : 'a t * ('a -> 'a) -> unit
  val copy : 'a t -> 'a t
  val copyWith : 'a t * (key * 'a -> 'b) -> 'b t
  val clear : 'a t -> unit
end


functor DictImpF (Key : ORD)
  :> DICT_IMP where type key = Key.t =
struct

  structure D = DictF (Key)

  type key = Key.t
  type 'a t = 'a D.t ref
             
  fun empty () = ref D.empty
  fun addWith (a, b, f) = a := D.union(!a, !b, f)
  fun add (a, b) = addWith (a, b, fn (k, a, b) => b)
  fun fromList l = ref (D.fromList l)
  fun fromList2 (k, a) = ref (D.fromList2 (k, a))
  fun toList d = D.toList (!d)
  fun range d = D.range (!d)
  fun domain d = D.domain (!d)
  fun fromVector v = ref (D.fromVector v)
  fun isEmpty d = D.isEmpty (!d)
  fun size d = D.size (!d)
  fun lookup (d, v) = D.lookup (!d, v)
  fun contains (d, v) = D.contains (!d, v)
  fun insert (d, v, i) = d := D.insert (!d, v, i)
  fun insertAll (d, vl) = d := D.insertAll (!d, vl)
  fun remove (d, v) = d := D.remove(!d, v)
  fun fold (d, i, f) = D.fold (!d, i, f)
  fun foreach (d, f) = D.foreach(!d, f)
  fun forall2 (d, b, f) = D.forall2(!d, !b, f)
  fun layout (d, f) = D.layout (!d, f)
  fun modify (d, f) = d := D.map (!d, fn (_, i) => f i)
  fun copy d = ref (!d)
  fun copyWith (d, f) = ref (D.map (!d, f))
  fun choose d = 
      case D.choose (!d)
       of SOME (dnew, l, a) => (d := dnew;SOME (l, a))
        | NONE => NONE
  fun clear d = d := D.empty
end

structure ImpIntDict =
    DictImpF (struct type t = int val compare = Int.compare end);

structure ImpStringDict =
    DictImpF (struct type t = string val compare = String.compare end);

structure ImpCharDict =
    DictImpF (struct type t = char val compare = Char.compare end);


signature DLIST = 
sig
  type 'a t
  type 'a cursor

  val empty       : unit -> 'a t
  val insert      : 'a t * 'a -> 'a cursor
  val insertLast  : 'a t * 'a -> 'a cursor
  val isEmpty     : 'a t -> bool
  val first       : 'a t -> 'a cursor option
  val last        : 'a t -> 'a cursor option
  val append      : 'a t * 'a t -> unit
  val all         : 'a t -> 'a cursor list
  val toList      : 'a t -> 'a list
  val toListRev   : 'a t -> 'a list
  val toVector    : 'a t -> 'a vector
  val toVectorRev : 'a t -> 'a vector

  val toListUnordered   : 'a t -> 'a list
  val toVectorUnordered : 'a t -> 'a vector

  val fromList    : 'a list -> 'a t

  (* You might think this should map over cursors.
   * You'd be wrong.  Use List.foreach (DList.all, ...)
   * and preserve your sanity.  *)
  val foreach : 'a t * ('a -> unit) -> unit
  val toListMap : 'a t * ('a -> 'b) -> 'b list

  val remove   : 'a cursor -> unit
  val insertL  : 'a cursor * 'a -> 'a cursor
  val insertR  : 'a cursor * 'a -> 'a cursor
  val next     : 'a cursor -> 'a cursor option
  val prev     : 'a cursor -> 'a cursor option

  val getVal   : 'a cursor -> 'a
  val layout   : 'a t * ('a -> Layout.t) -> Layout.t
end

structure DList :> DLIST = 
struct


  type 'a ptr = 'a option ref

  datatype 'a node = 
           Start of 'a data ptr
         | Elt of 'a data

  and 'a data = 
      D of {prev : 'a node ptr,
            data : 'a,
            next : 'a data ptr}

  type 'a cursor = 'a data

  type 'a t = 'a data ptr

  fun empty () : 'a t = ref NONE

  fun newData (e : 'a) : 'a cursor = 
      let
        val prev = ref NONE
        val next = ref NONE
        val cursor = D {prev = prev,
                        data = e,
                        next = next}
      in cursor
      end

  fun prevp (D {prev, ...}) : 'a node ptr = prev
  fun nextp (D {next, ...}) : 'a data ptr = next
  fun data  (D {data, ...}) : 'a = data

  val getVal = data

  fun nodeNextp (n : 'a node) : 'a data ptr = 
      (case n
        of Start p => p
         | Elt c => nextp c)


  fun isEmpty (l : 'a t) : bool = not (isSome(!l))
      
  fun first (l : 'a t) : 'a cursor option = !l

  fun insert (l : 'a t, e : 'a) : 'a cursor = 
      let
        val cursor = newData e
        val () = (nextp cursor) := !l
        val () = (prevp cursor) := SOME (Start l)
        val () = case !l
                  of NONE => ()
                   | SOME d => (prevp d) := SOME (Elt cursor)
        val () = l := SOME cursor
      in cursor
      end

  fun last (l : 'a t) : 'a cursor option = 
      let
        fun loop d = 
            (case !(nextp d)
              of NONE => d
               | SOME d => loop d)
        val res = 
            (case !l
              of NONE => NONE
               | SOME d => SOME (loop d))
      in res
      end

  fun append (l1 : 'a t, l2 : 'a t) : unit = 
      let
        val () = 
            (case !l2
              of NONE => ()
               | SOME d2 => 
                 (case last l1
                   of NONE => 
                      let
                        val () = l1 := SOME d2
                        val () = (prevp d2) := SOME (Start l1)
                      in ()
                      end
                    | SOME d1  => 
                      let
                        val () = (nextp d1) := SOME d2
                        val () = (prevp d2) := SOME (Elt d1)
                      in ()
                      end))
        val () = l2 := NONE
      in ()
      end

  fun insertLast (l : 'a t, e : 'a) : 'a cursor = 
      let
        val l2 = empty()
        val c = insert(l2, e)
        val () = append (l, l2)
      in 
        c
      end

  fun all (l : 'a t) : 'a cursor list = 
      let 
        fun loop (l, acc) = 
            (case !l
              of NONE => rev acc
               | SOME d => 
                 loop(nextp d, d :: acc))
      in loop (l, [])
      end

  fun foreach (l : 'a t, f : 'a -> unit) : unit = 
      let 
        fun loop l = 
            case !l
             of NONE => ()
              | SOME d => (f (data d); loop (nextp d))
      in loop l
      end

  fun toListMap (l : 'a t, f : 'a -> 'b) : 'b list = 
      List.map (all l, f o getVal)

  fun toListRev (l : 'a t) : 'a list = 
      let 
        fun loop (l, acc) = 
            (case !l
              of NONE => acc
               | SOME d => 
                 loop(nextp d, data d:: acc))
      in loop (l, [])
      end

  fun toList (l : 'a t) : 'a list = rev (toListRev l)

  val toListUnordered = toListRev

  fun toVectorRev (l : 'a t) : 'a vector = Vector.fromList (toListRev l)

  fun toVector (l : 'a t) : 'a vector = Vector.fromListRev (toListRev l)

  val toVectorUnordered = toVectorRev

  fun remove (node : 'a cursor) : unit = 
      let
        val pp = prevp node
        val np = nextp node
        val () = 
            case pp
             of ref NONE => ()
              | ref (SOME l) => 
                (pp := NONE;
                 (nodeNextp l) := !np;
                 case np
                  of ref NONE => ()
                   | ref (SOME c) => 
                     (
                      (prevp c) := SOME l;
                      np := NONE
                     )
                )
      in ()
      end

  fun link (n1 : 'a cursor, n2 : 'a cursor) : unit = 
      (
       nextp n1 := SOME n2;
       prevp n2 := SOME (Elt n1)
       )

  fun linkNode (n1 : 'a node, n2 : 'a cursor) : unit = 
      (
       nodeNextp n1 := SOME n2;
       prevp n2 := SOME n1
       )

  fun insertL (n1 : 'a cursor, e : 'a) : 'a cursor = 
      let
        val n2 = newData e
        val () = case !(prevp n1)
                  of SOME n3 => linkNode(n3, n2)
                   | NONE => ()
        val () = link (n2, n1)
      in n2
      end

  fun insertR (n1 : 'a cursor, e : 'a) : 'a cursor = 
      let
        val n2 = newData e
        val () = case !(nextp n1)
                  of SOME n3 => link(n2, n3)
                   | NONE => ()
        val () = link (n1, n2)
      in n2
      end

  fun next (n1 : 'a cursor) : 'a cursor option = !(nextp n1)

  fun prev (n1 : 'a cursor) : 'a cursor option = 
      case !(prevp n1)
       of NONE => NONE
        | SOME (Start _) => NONE
        | SOME (Elt data) => SOME data
                           
  fun layout (l : 'a t, f : 'a -> Layout.t) = 
      LayoutUtils.bracketSeq (List.map (all l, f o getVal))

  fun fromList (l : 'a list) : 'a t = 
      let
        val dl = empty()
        fun loop (c, l) = 
            (case l
              of [] => ()
               | a::l => loop(insertR (c, a), l))
        val () = 
            case l
             of [] => ()
              | a::l => loop(insert (dl, a), l)
      in dl
      end


end
(*
structure DList :> DLIST = 
struct


  type 'a ptr = 'a option ref

  fun $ r =
      case Ref.! r of
        NONE => Fail.fail ("Dereference of Null pointer")
      | SOME v => v

  datatype 'a data = 
           D of {prev : 'a node,
                 data : 'a,
                 next : 'a node}
  withtype 'a node = 'a data ptr
                  
  type 'a t = 'a node ref
  type 'a cursor = 'a data

  fun empty () : 'a t = ref (ref NONE)

  fun isEmpty (l : 'a t) : bool = not (isSome(!!l))

  fun newData (e : 'a) : 'a cursor = 
      let
        val prev = ref NONE
        val next = ref NONE
        val cursor = D {prev = prev,
                        data = SOME e,
                        next = next}
      in cursor
      end

  fun singleton (e : 'a) : 'a t = 
      let
        val l = ref NONE
        val p = ref NONE
        val n = ref NONE
        val cursor = D {prev = p,
                        data = e,
                        next = n}
        val () = l := cursor
        val () = p := cursor
        val () = n := cursor
      in ref l
      end

  fun prevp (D {prev, ...}) : 'a node ptr = prev
  fun nextp (D {next, ...}) : 'a data ptr = next

  fun data  (D {data, ...}) : 'a = data

  fun first (l : 'a t) : 'a cursor option = !!l
  fun last  (l : 'a t) : 'a cursor option = 
      (case !!l
        of SOME (D{prev, ...}) => SOME ($ prev)
         | NONE                => NONE)


  fun link (d1 as (D {next, ...}), 
            d2 as (D {prev, ...})) = 
      let
        val () = next := d2
        val () = prev := d1
      in ()
      end

  fun seq (l1, l2) = 
      (case (!l1, !l2)
        of (ref NONE, l)   => l1 := l
         | (l, ref NONE)   => l2 := l
         | (n1 as (ref (SOME d1)),
            n2 as (ref (SOME d2))) => 
           let
             val first = d1
             val midR  = $ (prevp d1)
             val midL  = d2
             val last  = $ (prevp d2)
             val () = link (midR, midL)
             val () = link (last, first)
             val () = n2 := SOME d1
           in ()
           end

  fun insert (l : 'a t, e : 'a) : 'a cursor = 
      let
        val s = singleton e
        val c = $!s
        val () = seq (s, l)
      in c
      end

  fun insertLast (l : 'a t, e : 'a) : 'a cursor = 
      let
        val s = singleton e
        val c = $!s
        val () = seq (l, s)
      in c
      end

  fun all (l : 'a t) : 'a cursor list = 
      (case !!l 
        of NONE => []
         | SOME d => 
           let
             val sentinal = nextp ($ (prevp d))
             fun loop (d, acc) = 
                 let
                   val acc = d::acc
                   val n = nextp d
                 in if n = sentinal then
                      rev acc
                    else => loop ($n, acc)
                 end
           in loop (d, [])
           end)

  fun foreach (l : 'a t, f : 'a -> unit) : unit = 
      (case !!l
        of NONE => ()
         | SOME d => 
           let 
             val sentinal = nextp ($ (prevp d))
             fun loop (d, acc) = 
                 let
                   val () = f (data d)
                   val n = nextp d
                 in if n = sentinal then ()
                    else loop ($n)
                 end
           in loop d
           end)

  fun toList (l : 'a t) : 'a list = List.map (all l, data)

  fun toVector (l : 'a t) : 'a vector = Vector.fromList (toList l)

  fun remove (node : 'a cursor) : unit = 
      let
        val pp = prevp node
        val np = nextp node
        val () = 
            case pp
             of ref NONE => ()
              | ref (SOME l) => 
                (pp := NONE;
                 (nodeNextp l) := !np;
                 case np
                  of ref NONE => ()
                   | ref (SOME c) => 
                     (
                      (prevp c) := SOME l;
                      np := NONE
                     )
                )
      in ()
      end

  fun link (n1 : 'a cursor, n2 : 'a cursor) : unit = 
      (
       nextp n1 := SOME n2;
       prevp n2 := SOME (Elt n1)
       )

  fun linkNode (n1 : 'a node, n2 : 'a cursor) : unit = 
      (
       nodeNextp n1 := SOME n2;
       prevp n2 := SOME n1
       )

  fun insertL (n1 : 'a cursor, e : 'a) : 'a cursor = 
      let
        val n2 = newData e
        val () = case !(prevp n1)
                  of SOME n3 => linkNode(n3, n2)
                   | NONE => ()
        val () = link (n2, n1)
      in n2
      end

  fun insertR (n1 : 'a cursor, e : 'a) : 'a cursor = 
      let
        val n2 = newData e
        val () = case !(nextp n1)
                  of SOME n3 => link(n2, n3)
                   | NONE => ()
        val () = link (n1, n2)
      in n2
      end

  fun next (n1 : 'a cursor) : 'a cursor option = !(nextp n1)

  fun prev (n1 : 'a cursor) : 'a cursor option = 
      case !(prevp n1)
       of NONE => NONE
        | SOME (Start _) => NONE
        | SOME (Elt data) => SOME data
                           
  fun getVal (D{data, ...} : 'a cursor) : 'a = data

  fun layout (l : 'a t, f : 'a -> Layout.t) = 
      LayoutUtils.bracketSeq (List.map (all l, f o getVal))

  fun fromList (l : 'a list) : 'a t = 
      let
        val dl = empty()
        fun loop (c, l) = 
            (case l
              of [] => ()
               | a::l => loop(insertR (c, a), l))
        val () = 
            case l
             of [] => ()
              | a::l => loop(insert (dl, a), l)
      in dl
      end

end
*)

signature STATS = 
sig
  type t

  val new : unit -> t

  val addToStat : t * string * int -> unit
  val incStat : t * string -> unit
  val getStat : t * string -> int
  val hasStat : t * string -> bool
  val newStat : t * string * string -> unit
  val fromList : (string * string) list -> t
 (* Add all statistics in s2 to s1
  * merge (s1, s2) => 
  *   For every statistic s:
  *     if s was in s2 and not s1, it is added to s1, with s1.s = s2.s
  *     if s was in s2 and s1, s1.s is incremented by s2.s
  *     Any subsequent increment of s2.s also increments s1.s
  *       (increments to s1.s do not affect s2.s)
  * Currently, new statistics subsequently added to s2 will not be added
  * to s1.  This functionality can be added if desired.
  *)
  val merge : t * t -> unit
  (* Push scope of stats
   * push s =>
   *   Return a new statistics sets with every statistic of s but with count 0
   *   Any subsequent increment of new stat also increments old stat
   *   (But not vice versa.)
   * Currently, new statistics subsequently added to the new set will not be
   * added to the old set.  This functionality can be added if desired.
   *)
  val push : t -> t
  val layout : t -> Layout.t
  val report : t -> unit
end

structure Stats :> STATS = 
struct
  structure ISD = ImpStringDict
  datatype count = C of (int ref) * (count list ref)
           
  type t = (string * count) ISD.t

  fun get (d, s) = 
      (case ISD.lookup(d, s) 
        of SOME (cmt, c) => c
         | NONE => Fail.fail ("stats.sml",
                              "get",
                              "unknown stat " ^ s))
  fun addToStat (d, s, n) = 
      let
        val C (r, _) = get(d, s)
      in r := !r + n
      end

  fun incStat (d, s) = addToStat(d, s, 1)

  fun sumCount (C (r, counts)) = 
      List.fold (!counts, !r, fn (c, s) => s + sumCount c)
  fun getStat (d, s) = sumCount(get(d, s))
  fun hasStat (d, s) = ISD.contains(d, s)

  fun new () = ISD.empty ()
  fun newStat (d, s, cmt) = 
      let
        val c = C (ref 0, ref [])
      in ISD.insert(d, s, (cmt, c))
      end

  fun fromList l = 
      let
        val d = new () 
        val () = List.foreach (l, fn (s, cmt) => newStat (d, s, cmt))
      in d
      end

  fun merge (s1, s2) = 
      let
        fun help (s, (cmt, count)) = 
            case (ISD.lookup (s1, s))
             of SOME (cmt, C (_, c)) => c := count :: !c
              | NONE => ISD.insert (s1, s, (cmt, C (ref 0, ref [count])))
        val () = List.foreach (ISD.toList s2, help)
      in ()
      end

  fun push s1 =
      let
        fun doOne (_, (cmt, C (_, c))) =
            let
              val count = C (ref 0, ref [])
              val () = c := count :: !c
            in (cmt, count)
            end
        val s2 = ISD.copyWith (s1, doOne)
      in s2
      end

  fun layoutOne (s, (cmt, r)) = 
      let
        val i = sumCount r
        val lo = 
            if i > 0 then 
              SOME (Layout.seq[Int.layout i, 
                               Layout.str " ", 
                               Layout.str cmt])
            else
              NONE
      in lo
      end
  fun layout d = Layout.align ((Layout.str "Statistics:")::
                               List.keepAllMap (ISD.toList d, layoutOne))
  fun report d = LayoutUtils.printLayout(layout d)
end


signature ImpQueue = 
sig
  type 'a t
  val new : unit -> 'a t
  val fromList : 'a list -> 'a t
  val enqueue : 'a t * 'a -> unit
  val enqueueList : 'a t * 'a list -> unit
  val dequeue : 'a t -> 'a
  val head : 'a t -> 'a
  val peek : 'a t -> 'a option
  val isEmpty : 'a t -> bool
end

structure ImpQueue :> ImpQueue = 
struct
  type 'a t = 'a Queue.queue

  val new = Queue.mkQueue

  fun enqueueList (q, l) = 
      let
        val () = List.foreach (l, fn a => Queue.enqueue(q, a))
      in ()
      end

  fun fromList l = 
      let
        val q = new() 
        val () = enqueueList(q, l)
      in q
      end

  val enqueue = Queue.enqueue

  val dequeue = Queue.dequeue
  val head = Queue.head
  val peek = Queue.peek
  val isEmpty = Queue.isEmpty
end


(* Fixed size imperative bit sets *)
signature IMP_BIT_SET = sig
    type t
    val new : Int.t -> t
    val copy : t -> t
    val fromList   : Int.t * Int.t list -> t
    val fromVector : Int.t * Int.t vector -> t
    val insertList   : t * Int.t list -> unit
    val insertVector : t * Int.t vector -> unit
   (* Return elements in increasing order *)
    val toList   : t -> Int.t list
    val toVector : t -> Int.t vector
    val isEmpty : t -> bool
    val count   : t -> int
    val equal : t * t -> bool
    val isSubset : t * t -> bool
    val insert   : t * Int.t -> unit
    val remove   : t * Int.t -> unit
    (* binary operators modify first set *)
    val union        : t * t -> unit
    val intersection : t * t -> unit
    val difference   : t * t -> unit
    val complement   : t -> unit
    val member       : t * Int.t -> bool
    val forall       : t * (Int.t -> bool) -> bool
    val exists       : t * (Int.t -> bool) -> bool
    val partition    : t * (Int.t -> bool) -> {no: t, yes: t}
    val foreach : t * (Int.t -> unit) -> unit
    val keepAll : t * (Int.t -> bool) -> unit
    val fold : t * 'b * (Int.t * 'b -> 'b) -> 'b
    val getAny : t -> Int.t option
    val layout : t * (Int.t -> Layout.t) -> Layout.t
end;


(* Fixed size imperative bit sets *)
structure ImpBitSet :> IMP_BIT_SET = 
struct
  structure BA = BitArray
  type t = BA.array

  val new : Int.t -> t =
    fn i => BA.array (i, false)

  val copy : t -> t = 
    fn s => BA.extend0 (s, BA.length s)

  val insert   : t * Int.t -> unit = BA.setBit

  val member       : t * Int.t -> bool = BA.sub

  val fromList   : Int.t * Int.t list -> t = BA.bits

  val insertList   : t * Int.t list -> unit = 
   fn (s, l) =>
      List.foreach (l, fn i => insert (s, i))

  val insertVector : t * Int.t vector -> unit = 
   fn (s, l) =>
      Vector.foreach (l, fn i => insert (s, i))

  val fromVector : Int.t * Int.t vector -> t = 
   fn (i, elts) =>
      let
        val s = new i
        val () = insertVector (s, elts)
      in s
      end

  val toList   : t -> Int.t list = BA.getBits

  val toVector : t -> Int.t vector = Vector.fromList o toList

  val isEmpty : t -> bool = BA.isZero

  val fold : t * 'b * (Int.t * 'b -> 'b) -> 'b = 
   fn (s, a, f) => 
      BA.foldli (fn (i, b, a) => if b then f (i, a) else a) a s

  val count : t -> int = 
   fn s => fold (s, 0, fn (_, c) => c+1)
      
  val equal : t * t -> bool = BA.equal

  val remove   : t * Int.t -> unit = BA.clrBit

  val foreach : t * (Int.t -> unit) -> unit = 
   fn (s, f) => 
      BA.appi (fn (i, b) => if b then f i else ()) s

  val forall       : t * (Int.t -> bool) -> bool = 
   fn (s, f) => 
      let
        val length = BA.length s
        val rec loop = 
         fn i =>
            if i = length then true
            else BA.sub (s, i) andalso loop(i+1)
      in loop 0
      end

  val isSubset : t * t -> bool = 
      fn (s1, s2) => BA.isZero(BA.andb(s1, BA.notb s2, BA.length s1))
      
  (* binary operators modify first set *)
  val union        : t * t -> unit = 
      fn (s1, s2) => BA.union s1 s2

  val intersection : t * t -> unit = 
      fn (s1, s2) => BA.intersection s1 s2

  val difference   : t * t -> unit = 
      fn (s1, s2) => foreach (s2, fn i => (remove (s1, i)))

  val complement   : t -> unit = BA.complement
      
  val exists       : t * (Int.t -> bool) -> bool = 
   fn (s, f) => 
      let
        val length = BA.length s
        val rec loop = 
         fn i =>
            if i = length then false
            else BA.sub (s, i) orelse loop(i+1)
      in loop 0
      end

  val partition    : t * (Int.t -> bool) -> {no: t, yes: t} = 
   fn (s, p) => 
      let
        val l = BA.length s
        val yes = new l
        val no = new l
        val help = 
         fn i => 
            if p i then
               insert (yes, i)
            else
              insert (no, i)
        val () = 
            foreach (s, help)
      in {yes = yes, 
          no  = no}
      end

  val keepAll : t * (Int.t -> bool) -> unit = 
      fn (s, p) => 
         BA.modifyi (fn (i, b) => b andalso (p i)) s

  val getAny : t -> Int.t option = 
   fn s => 
      let
        val length = BA.length s
        val rec loop = 
         fn i => 
            if i = length then 
              NONE
            else if BA.sub (s, i) then 
              SOME i
            else 
              loop(i+1)
      in loop 0
      end

  val layout : t * (Int.t -> Layout.t) -> Layout.t = 
   fn (s, lf) =>
      let
        val elts = toList s
        val l = List.layout lf elts
      in l
      end
end      

signature EQUIVALENCE_CLASS = 
sig
  type 'a t
  val new   : 'a -> 'a t
  (* Returns true iff previously disjoint *)
  val join  : 'a t * 'a t -> bool
  (* joinWith (a, b, f) = joins a and b.  If a and b were not already equal, 
   * it then sets the contents to be f(ad, bd), where ad and bd are 
   * the original contents of and b.
   *)
  val joinWith : 'a t * 'a t * ('a * 'a -> 'a) -> bool
  val equal : 'a t * 'a t -> bool
  val set   : 'a t * 'a -> unit
  val get   : 'a t -> 'a
end

structure EquivalenceClass :> EQUIVALENCE_CLASS = 
struct

  datatype 'a tS = 
           Root of 'a * int
         | Child of 'a t
  withtype 'a t = 'a tS ref

  val new   : 'a -> 'a t = 
   fn a => ref (Root (a, 0))
           
  val rec find  : 'a t -> 'a t * 'a * int = 
   fn a => 
      (case !a
        of Root (d, i) => (a, d, i)
         | Child p => 
           let
             val (p, d, i) = find p
             val () = a := Child p
           in (p, d, i)
           end)

  val equal : 'a t * 'a t -> bool = 
   fn (a, b) => 
      let
        val (a, _, _) = find a
        val (b, _, _) = find b
      in
        a = b
      end

  val set   : 'a t * 'a -> unit = 
   fn (a, d) => 
      let
        val (a, _, ar) = find a
      in a := Root (d, ar)
      end

  val get   : 'a t -> 'a = 
   fn a => 
      let
        val (_, d, _) = find a
      in d
      end

  val join  : 'a t * 'a t -> bool = 
   fn (a, b) => 
      let
        val (a, ad, ar) = find a
        val (b, bd, br) = find b
      in
        if (a = b) then 
          false
        else
          let
            val (p, c, r) = 
                case Int.compare (ar, br)
                 of LESS    => (b, a, br)
                  | GREATER => (a, b, ar)
                  | EQUAL   => (a, b, br+1)
            val () = p := Root(ad, r)
            val () = c := Child p
          in true
          end
      end

  val joinWith : 'a t * 'a t * ('a * 'a -> 'a) -> bool = 
   fn (a, b, f) => 
      let
        val ad = get a
        val bd = get b
        val b = join (a, b)
        val () = if b then 
                   set (a, f (ad, bd))
                 else
                   ()

      in b
      end      

end;

signature POLY_LATTICE = 
sig
  type 'a t

  val top : 'a t
  val bot : 'a t
  val elt : 'a -> 'a t 

  val isTop : 'a t -> bool
  val isBot : 'a t -> bool
  val isElt : 'a t -> bool

  val get : 'a t -> 'a option

  (* If no lub, result it top *)
  val join : {lub : 'a * 'a -> 'a option} -> ('a t * 'a t -> 'a t)

  val layout : ('a -> Layout.t) -> ('a t -> Layout.t)
  val equal : ('a * 'a -> bool) -> ('a t * 'a t -> bool)
end;

structure PolyLattice :> POLY_LATTICE = 
struct
  datatype 'a t = 
           Top 
         | Bot 
         | Elt of 'a

  val top = Top
  val bot = Bot
  val elt = Elt

  val isTop = fn t => case t of Top => true | _ => false
  val isBot = fn t => case t of Bot => true | _ => false
  val isElt = fn t => case t of Elt _ => true | _ => false


  val get = 
   fn t => 
      (case t 
        of Elt e => SOME e
         | _ => NONE)

      
  val rec join = 
   fn {lub} => 
   fn (t1, t2) => 
      case (t1, t2)
       of (Top, _) => Top
        | (_, Top) => Top
        | (Bot, _) => t2
        | (_, Bot) => t1
        | (Elt e1, Elt e2) => 
          (case lub (e1, e2)
            of SOME e => Elt e
             | NONE => Top)

  local 
    structure L = Layout
  in
  val layout = 
      (fn layoutElt =>
          (fn t => 
              (case t
                of Top => L.str "_T_"
                 | Bot => L.str "_B_"
                 | Elt e => L.seq [L.str "E(", layoutElt e, 
                                   L.str ")"])))
  end

  val equal = 
   fn eqT =>
   fn (t1, t2) => 
      (case (t1, t2) 
        of (Top, Top) => true
         | (Bot, Bot)  => true
         | (Elt e1, Elt e2) => eqT (e1, e2)
         | _ => false)
end

(* Semi-Lattices, and functors for injecting partially ordered sets into 
 * bounded semi-lattices.
 *)
signature LATTICE = 
sig
  type t
  type element

  val top : t
  val bot : t
  val elt : element -> t 

  val isTop : t -> bool
  val isBot : t -> bool
  val isElt : t -> bool

  val get : t -> element option

  val join : t * t -> t

  val layout : (element -> Layout.t) -> (t -> Layout.t)
  val equal : (element * element -> bool) -> (t * t -> bool)
end;

(* Base lattice functor, which supports recursively defined lattice
 * structure.
 *)
functor RecLatticeFn(type 'a element
                     (* Given the join operation on the lattice, 
                      * return the least upper bound of the elements if it exists.  
                      * *)
                     val lub : (('a * 'a) -> 'a) -> 
                               'a element * 'a element -> 'a element option)
        :>
         sig
           type t
           type element = t element
                
           val top : t
           val bot : t
           val elt :  element -> t 
                                 
           val isTop : t -> bool
           val isBot : t -> bool
           val isElt : t -> bool
                            
           val get : t -> element option
                          
           val join : t * t -> t

           val layout : (element -> Layout.t) -> (t -> Layout.t)
           val equal : (element * element -> bool) -> (t * t -> bool)
         end
  =
struct
  datatype t = 
           Top 
         | Bot 
         | Elt of t element
  type element = t element
  val top = Top
  val bot = Bot
  val elt = Elt

  val isTop = fn t => case t of Top => true | _ => false
  val isBot = fn t => case t of Bot => true | _ => false
  val isElt = fn t => case t of Elt _ => true | _ => false


  val get = 
   fn t => 
      (case t 
        of Elt e => SOME e
         | _ => NONE)

  val rec join = 
   fn (t1, t2) => 
      case (t1, t2)
       of (Top, _) => Top
        | (_, Top) => Top
        | (Bot, _) => t2
        | (_, Bot) => t1
        | (Elt e1, Elt e2) => 
          (case lub join (e1, e2)
            of SOME e => Elt e
             | NONE => Top)

  local 
    structure L = Layout
  in
  val layout = 
      (fn layoutElt =>
          (fn t => 
              (case t
                of Top => L.str "_T_"
                 | Bot => L.str "_B_"
                 | Elt e => L.seq [L.str "E(", layoutElt e, 
                                   L.str ")"])))
  end

  val equal = 
   fn eqT =>
   fn (t1, t2) => 
      (case (t1, t2) 
        of (Top, Top) => true
         | (Bot, Bot)  => true
         | (Elt e1, Elt e2) => eqT (e1, e2)
         | _ => false)
end

(* Basic non-recursive lattice
 *)
functor LatticeFn(type element
                  val lub : element * element -> element option
                  )
        :> LATTICE where type element = element = 
struct
  structure Lat = RecLatticeFn (struct
                                  type 'a element = element
                                  val lub = fn _ => lub
                                end)
  open Lat
end;

(* Turn equality into a degenerate lub
 * for use in creating flat latttices 
 *)
functor MkFlatFuns(type element
                   val equal : element * element -> bool
                   ) 
: sig
    type element = element
    val lub : element * element -> element option
  end = 
struct
  type element = element
  val lub = 
   fn (a, b) => 
      if equal (a, b) then SOME a else NONE
end;

(* Make a flat lattice, where lub(a, b) exists iff a = b
 *)
functor FlatLatticeFn(type element
                      val equal : element * element -> bool
                      ) 
        :> LATTICE where type element = element = 
struct
  structure Lat = LatticeFn(MkFlatFuns(struct 
                                         type element = element
                                         val equal = equal
                                       end))
  open Lat
end;

(* A lattice whose elements consist of vectors of lattice elements. 
 * The join of two equal length vectors is  the vector
 * of joins of their elements, and similarly for meets.  Unequal lengths 
 * meet at bottom and join at top. 
 *)
functor LatticeVectorLatticeFn(structure Lattice : LATTICE)
        :> LATTICE where type element = Lattice.t Vector.t = 
struct
  val lub = 
   fn (a, b) => 
      if Vector.length a = Vector.length b then
        SOME (Vector.map2 (a, b, Lattice.join))
      else 
        NONE

  structure Lat = LatticeFn(struct
                              type element = Lattice.t Vector.t
                              val lub = lub
                            end)
  open Lat
end;


(* A lattice whose elements consists of optional values.
* NONE and SOME are unrelated, SOMEs are related according to the
* meet/join of their contents *)
functor OptionLatticeFn(type element
                        val lub : element * element -> element option
                        )
        :> LATTICE where type element = element option =
struct
  val lub = 
      (fn (a, b) => 
          (case (a, b)
            of (NONE, NONE) => SOME NONE
             | (SOME a, SOME b) => 
               Option.map(lub (a, b), SOME)
             | _ => NONE))

  structure Lattice = LatticeFn(struct
                                  type element = element option
                                  val lub = lub
                                end)
  open Lattice
end;

(* A lattice whose elements are optional values, where the components
 * of the option type are related only by equality
 *)
functor FlatOptionLatticeFn(type element
                            val equal : element * element -> bool
                            )
        :> LATTICE where type element = element option =
struct
  structure Lat = OptionLatticeFn(MkFlatFuns(struct
                                               type element = element
                                               val equal = equal
                                             end))
  open Lat
end


signature BACK_PATCH = 
sig
  type 'a t
  val new : unit -> 'a t
  val fill : 'a t * 'a -> unit
  val get : 'a t -> 'a

  type ('a, 'b) func = ('a -> 'b) t
  val apply : ('a, 'b) func -> ('a -> 'b)
end
structure BackPatch :> BACK_PATCH = 
struct
  type 'a t = 'a option ref

  val new = 
   fn () => ref NONE
  val fill = 
   fn (b, a) => 
      (case !b
        of SOME _ => Fail.fail ("utils.sml", "fill", "Already filled")
         | NONE => b := SOME a)
  val get = 
   fn b => 
      (case !b
        of SOME a => a
         | NONE => Fail.fail ("utils.sml", "get", "Not yet filled"))

  type ('a, 'b) func = ('a -> 'b) t

  val apply = 
   fn f => fn a => get f a

end


signature PARSER = 
sig
  type elt
  type stream
  type pos
  type error
  type 'a t
  datatype 'a result = Success of stream * 'a | Failure | Error of pos * error
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> ('b t)) -> 'b t
  val succeed : 'a -> 'a t
  val fail : 'a t
  val error : error -> 'a t
  val map : 'a t * ('a -> 'b) -> 'b t 
  val parse : 'a t * stream -> 'a result
  val || : 'a t * 'a t -> 'a t
  val && : 'a t * 'b t -> ('a * 'b) t
  val any : 'a t list -> 'a t
  val all : 'a t list -> 'a list t
  val get : elt t
  val satisfy : (elt -> bool) -> elt t 
  val satisfyMap : (elt -> 'a option) -> 'a t 
  val atEnd : error -> unit t
  val zeroOrMore : 'a t -> 'a list t
  val oneOrMore : 'a t -> 'a list t
  val zeroOrMoreV : 'a t -> 'a Vector.t t
  val oneOrMoreV : 'a t -> 'a Vector.t t
  val seqSep : 'a t * unit t -> 'a list t
  val seqSepV : 'a t * unit t -> 'a Vector.t t
  (* sep & right should succeed or fail to match the separator or closer *)
  (* error is an appropriate error to indicate expecting sep or right *)
  val sequence : {left : unit t, sep : unit t, right : unit t, err : error, item : 'a t} -> 'a list t
  val sequenceV : {left : unit t, sep : unit t, right : unit t, err : error, item : 'a t} -> 'a Vector.t t
  val optional : 'a t -> 'a option t
  val required : 'a option t * error -> 'a t
  val succeeds : 'a t -> bool t
  val ignore : 'a t -> unit t
  val $ : (unit -> 'a t) -> 'a t 
  val $$ : ('a -> 'b t) -> 'a -> 'b t
end (* signature PARSER *)

(* Simple infinite lookahead parser combinators 
 *)
functor ParserF
  (type elt
   type stream
   type pos
   type error
   val pos : stream -> pos
   val next : stream -> (stream * elt) option)
  :> PARSER where type elt = elt
              and type stream = stream
              and type pos = pos
              and type error = error
  =
struct

  type elt = elt
  type stream = stream
  type pos = pos
  type error = error
  datatype 'a result = Success of stream * 'a | Failure | Error of pos * error

  type 'a t = stream -> 'a result

  val return : 'a -> 'a t = fn x => fn cs => Success (cs, x)

  val bind : 'a t -> ('a -> ('b t)) -> 'b t = 
   fn p => 
   fn f => 
   fn cs => 
       case p cs
        of Success (cs, a) => f a cs
         | Failure => Failure
         | Error pe => Error pe

  val succeed : 'a -> 'a t = return

  val fail : 'a t = fn _ => Failure

  val error : error -> 'a t = fn e => fn cs => Error (pos cs, e)

  val map : 'a t * ('a -> 'b) -> 'b t = 
   fn (p, f) => 
   fn cs => 
      (case p cs
        of Success (cs, r) => Success (cs, f r)
         | Failure => Failure
         | Error pe => Error pe)

  val parse : 'a t * stream -> 'a result = fn (p, s) => p s

  val get : elt t = 
   fn cs => (case next cs 
              of NONE => Failure
               | SOME arg => Success arg)

  val satisfy : (elt -> bool) -> elt t = 
   fn p => bind get (fn c => if p c then return c else fail)

  val satisfyMap : (elt -> 'a option) -> 'a t  = 
   fn f => bind get (fn c => case f c of SOME a => return a | NONE => fail)

  fun atEnd (e : error) : unit t =
   fn cs => (case next cs
              of NONE => Success (cs, ())
               | SOME _ => Error (pos cs, e))

  val ignore : 'a t -> unit t = fn p => map (p, ignore)

  val || : 'a t * 'a t -> 'a t = 
   fn (p1, p2) => fn cs => case p1 cs 
                            of Failure => p2 cs
                             | result => result
                                         
  val && : 'a t * 'b t -> ('a * 'b) t = 
   fn (p1, p2) => bind p1 (fn a => bind p2 (fn b => return (a, b)))

  infix || &&

  val rec any : 'a t list -> 'a t = 
   fn l => 
      (case l
        of [] => fail
         | p::ps => p || any ps)
      
  val rec all : 'a t list -> 'a list t = 
   fn l => 
      (case l
        of [] => return []
         | p::ps => map (p && (all ps), List.cons))

  fun zeroOrMore (p : 'a t) : 'a list t =
      oneOrMore p || return []
  and oneOrMore (p : 'a t) : 'a list t =
      bind p (fn x => bind (zeroOrMore p) (fn rest => return (x::rest)))

  fun zeroOrMoreV (p : 'a t) : 'a Vector.t t = map (zeroOrMore p, Vector.fromList)
  fun oneOrMoreV  (p : 'a t) : 'a Vector.t t = map (oneOrMore  p, Vector.fromList)

  fun seqSep (p : 'a t, sep : unit t) : 'a list t =
      bind p (fn x => bind (zeroOrMore (bind sep (fn _ => p))) (fn rest => return (x::rest))) || return []

  fun seqSepV (p : 'a t, sep : unit t) : 'a Vector.t t = map (seqSep (p, sep), Vector.fromList)

  val optional : 'a t -> 'a option t = 
   fn p => (bind p (fn a => return (SOME a))) || (return NONE)

  val required : 'a option t * error -> 'a t = 
   fn (p, e) =>
      bind p (fn opt => case opt of SOME a => return a | NONE => fail) || error e

  fun succeeds (p : 'a t) : bool t = bind p (fn _ => return true) || return false

  fun $$ (f : 'a -> 'b t) (x : 'a) : 'b t = fn cs => f x cs

  fun $ (f : unit -> 'a t) : 'a t = $$ f ()

  fun sequence {left : unit t, sep : unit t, right : unit t, err : error, item : 'a t} : 'a list t =
      let
        fun pr () =
            map (right, fn () => []) ||
            map (sep && item && $ pr, fn ((_, i), is) => i::is) || 
            error err
        val p = map (right, fn () => []) || map (item && $ pr, fn (i, is) => i::is)
        val p = map (left && p, #2)
      in p
      end

  fun sequenceV x : 'a Vector.t t = map (sequence x, Vector.fromList)

end (* functor ParserF *)

structure StringParser = 
  ParserF(type elt = char
          type stream = string * int
          type pos = int
          type error = string
          fun pos (_, i) = i
          val next = fn (s, i) => if i < String.length s then SOME ((s, i+1), String.sub (s, i)) else NONE)

structure InStreamWithPos
  :> sig
       type pos = {line : int, col : int}
       type t
       val mk : Pervasive.TextIO.StreamIO.instream -> t
       val input1 : t -> (char * t) option
       val pos : t -> pos
     end =
struct

  type pos = {line : int, col : int}

  type instreamBase = Pervasive.TextIO.StreamIO.instream

  type t = instreamBase * int * int

  fun mk (ins : instreamBase) : t = (ins, 1, 0)

  fun input1 ((ins, l, c) : t) : (char * t) option =
      case Pervasive.TextIO.StreamIO.input1 ins
       of NONE => NONE
        | SOME (ch, ins) => SOME (ch, if ch = Char.newline then (ins, l+1, 0) else (ins, l, c+1))

  fun pos ((ins, l, c) : t) : pos = {line = l, col = c}

end

structure FileParser =
  ParserF(type elt = char
          type stream = InStreamWithPos.t
          type pos = InStreamWithPos.pos
          type error = string
          val pos = InStreamWithPos.pos
          fun next s = Option.map (InStreamWithPos.input1 s, Utils.flip2))


signature BIJECTION = 
sig
  type ('a, 'b) t = {inject : 'a -> 'b,
                     project : 'b -> 'a}
  val compose : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t
  val pair : ('a, 'b) t * ('c, 'd) t -> ('a * 'c, 'b * 'd) t 
end (* signature BIJECTION *)

structure Bijection :> BIJECTION = 
struct

  type ('a, 'b) t = {inject : 'a -> 'b,
                     project : 'b -> 'a}

  val compose : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t =
   fn ({inject = inj1, project = proj1},
       {inject = inj2, project = proj2}) => 
      {inject = inj2 o inj1,
       project = proj1 o proj2}

  val pair : ('a, 'b) t * ('c, 'd) t -> ('a * 'c, 'b * 'd) t = 
   fn ({inject = inj1, project = proj1},
       {inject = inj2, project = proj2}) => 
      {inject = fn (a, c) => (inj1 a, inj2 c),
       project = fn (b, d) => (proj1 b, proj2 d)}

end (* structure Bijection *)

signature FINITE_ORDINAL = 
sig
  structure Base : 
  sig 
    type t
    val compare : t Compare.t
    val eq      : t * t -> bool
    val hash    : t -> Word32.word
  end
  type 'a t
  val base : Base.t -> 'a t
  val unBase : 'a t -> Base.t 
  val basePair : Base.t * Base.t -> 'a t
  val baseVector : Base.t Vector.t -> 'a t
  val pair : 'a t * 'b t -> 'c t 
  val shift : Base.t * 'a t -> 'b t
  val cast : 'a t -> 'b t
  val compare : ('a t) Compare.t
  val eq : 'a t * 'a t -> bool
  val hash : 'a t -> Word32.word 
end (* signature FINITE_ORDINAL *)

functor FiniteOrdinalF(structure Base : 
                       sig 
                         type t
                         val zero    : t
                         val compare : t Compare.t
                         val eq      : t * t -> bool
                         val hash    : t -> Word32.word
                       end) :> FINITE_ORDINAL where type Base.t = Base.t = 
struct

  structure Base = Base

  datatype rep = FoBase of Base.t | FoPair of (rep * rep)

  datatype 'a t = FOrd of {hash : Word32.word, rep : rep}

  val base : Base.t -> 'a t = 
      fn bs => FOrd {hash = Base.hash bs, rep = FoBase bs}

  val pair : 'a t * 'b t -> 'c t = 
      fn (FOrd {hash = h1, rep = r1},
          FOrd {hash = h2, rep = r2}) => FOrd {hash = h1 + Word32.<< (h2, 0w5) + h2 + 0w720,
                                               rep = FoPair (r1, r2)}

  val shift : Base.t * 'a t -> 'b t =  fn (b, f) => pair (base b, f)

  val basePair : Base.t * Base.t -> 'a t = fn (a, b) => pair (base a, base b)

  val baseVector : Base.t Vector.t -> 'a t = 
   fn v => Vector.fold (v, base Base.zero, shift)

  val unBase : 'a t -> Base.t = 
   fn FOrd fo => (case #rep fo
                   of FoBase base => base
                    | FoPair _    => Fail.fail ("utils.sml", 
                                                "FiniteOrdinalF.unBase", 
                                                "Not a base ordinal"))

  val cast : 'a t -> 'b t = fn FOrd {hash, rep} => FOrd {hash = hash, rep = rep}

  val rec compareRep : rep Compare.t = 
   fn p => 
      (case p
        of (FoBase b1, FoBase b2) => Base.compare (b1, b2)
         | (_, FoBase _)          => GREATER
         | (FoBase _, _)          => LESS
         | (FoPair p1, FoPair p2) => Compare.pair (compareRep, compareRep) (p1, p2))

  val compare : ('a t) Compare.t = 
   fn (FOrd {hash=h1, rep=r1}, FOrd {hash = h2, rep = r2}) => 
      (case Word32.compare (h1, h2) 
        of EQUAL => compareRep (r1, r2)
         | other => other)
      
  val rec eqRep : rep * rep -> bool = 
   fn p => 
      (case p
        of (FoBase b1, FoBase b2) => Base.eq (b1, b2)
         | (FoPair (r11, r12), 
            FoPair (r21, r22))    => eqRep (r11, r21) andalso eqRep (r21, r22)
         | (_, FoBase _)          => false
         | (FoBase _, _)          => false)

  val eq : 'a t * 'a t -> bool = 
   fn (FOrd {hash=h1, rep=r1}, FOrd {hash = h2, rep = r2}) => 
      (h1 = h2) andalso eqRep (r1, r2)

  val hash : 'a t -> Word32.word = 
   fn FOrd {hash, rep} => hash

end (* functor FiniteOrdinalF *)

structure IntFiniteOrdinal = FiniteOrdinalF(structure Base = 
                                            struct 
                                              type t = Int32.t
                                              val zero    : t = 0
                                              val compare : t Compare.t = Int32.compare
                                              val eq      : t * t -> bool = Int32.equals
                                              val hash    : t -> Word32.word = Word32.fromInt
                                            end)
