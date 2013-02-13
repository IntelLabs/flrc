(* COPYRIGHT_NOTICE_1 *)
(* TypeRep provides a manager to store data types using hashconsing to
 * save memory usage. No recursive types are allowed.
 *)

signature TYPE_REP =
sig
  type 'base rep     (* abstract type representation *)
  type 'base manager (* mutable data structure that facilitates operations on base types *)
  type 'base baseHash = ('base rep -> word) -> 'base -> word
  type 'base baseEq   = ('base rep * 'base rep -> bool) -> 'base * 'base -> bool
  (* hash function for rep *)
  val hashRep : ('base -> word) -> 'base rep -> word
  val hashRepWithManager : 'base manager * 'base rep -> word
  (* create an empty rep manager *)
  val newManager  : 'base baseHash * 'base baseEq -> 'base manager
  (* create an empty rep manager *)
  val size        : 'base manager -> int
  (* create a managed rep, store it with the manager *)
  val newRep      : 'base manager * 'base -> 'base rep 
  (* create an un-managed rep, which should only be used for temporary cases *)
  val newRep_     : 'base -> 'base rep 
  (* unwrap the rep and return the base type *)
  val repToBase   : 'base rep -> 'base
  (* helper functions to compute hash *)
  val hash2 : word * word -> word
  val hash3 : word * word * word -> word
  val hash4 : word * word * word * word -> word
  val hashList : word list -> word
end

structure TypeRep : TYPE_REP =
struct
  structure H = HashTable
  structure UO = Utils.Option

  (* Exception is required by sml-nj's HashTable, though we don't use it *)
  exception NotFound 

  (* The optional meta fields are to speed up hashing and comparison operations *)
  datatype 'base rep = Ty of { base : 'base, meta : { hash : word, uid : word } option }

  (* 
   * The manager uses a stamp to generate uids for rep, and a hash table to
   * store reps for hash-consing.
   *) 
  datatype 'base manager = Tbl of { stamp : word ref, 
                                    table : ('base, 'base rep) H.hash_table,
                                    hashBase : 'base -> word }

  type 'base baseHash = ('base rep -> word) -> 'base -> word

  type 'base baseEq   = ('base rep * 'base rep -> bool) -> 'base * 'base -> bool

  val hashRep : ('base -> word) -> 'base rep -> word
    = fn hashBase => fn Ty { base, meta } => 
      UO.dispatch (meta, fn { hash, ... } => hash, fn () => hashBase base)

  val hashRepWithManager : 'base manager * 'base rep -> word
    = fn (Tbl { hashBase, ... }, x) => hashRep hashBase x

  val eqRep : ('base * 'base -> bool) -> 'base rep * 'base rep -> bool
    = fn eqBase => fn (Ty { base = x, meta = u }, Ty { base = y, meta = v}) =>
        (case (u, v)
          of (SOME u, SOME v) => #uid u = #uid v
           | _ => eqBase (x, y))

  val newManager : 'base baseHash * 'base baseEq -> 'base manager
    = fn (hash, eq) => 
      let
        fun hashBase x = hash (hashRep hashBase) x
        fun eqBase x = eq (eqRep eqBase) x
        val table = H.mkTable (hashBase, eqBase) (32, NotFound)
      in
        Tbl { stamp = ref 0w0, table = table, hashBase = hashBase }
      end

  val size : 'base manager -> int
    = fn Tbl tbl => H.numItems (#table tbl)

  val newRep : 'base manager * 'base -> 'base rep
    = fn (tbl as Tbl { stamp, table, hashBase }, base) =>
      UO.dispatch (H.find table base, fn x => x,
        fn () =>
          let
            val () = stamp := Word.+(!stamp, 0w1)
            val x = Ty { base = base, meta = SOME { hash = hashBase base, uid = !stamp }}
            val () = H.insert table (base, x)
          in
            x
          end)

  val newRep_ : 'base -> 'base rep
    = fn b => Ty { base = b, meta = NONE }

  val repToBase : 'base rep -> 'base
    = fn Ty x => #base x

  fun hash2 (a, b) = Word.+ (Word.* (a, 0wx133), b)
  fun hash3 (a, b, c) = hash2 (a, hash2 (b, c))
  fun hash4 (a, b, c, d) = hash2 (a, hash2 (b, hash2 (c, d)))
  fun hashList l = List.fold (l, 0wx0, hash2)
end
