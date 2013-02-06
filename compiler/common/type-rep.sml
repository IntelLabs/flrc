(* COPYRIGHT_NOTICE_1 *)
(* TypeRep provides a manager to store data types using hashconsing to
 * save memory usage. No recursive types are allowed.
 *)

signature TYPE_REP =
sig
  type 'base rep     (* abstract type representation *)
  type 'base manager (* mutable data structure that facilitates operations on base types *)
  type 'base baseHash = 'base manager * 'base -> string
  type 'base repHash  = 'base manager * 'base rep -> string 
  (* hash function for rep *)
  val hashRep     : 'base repHash
  (* create an empty rep manager *)
  val newManager  : 'base baseHash -> 'base manager
  (* create an empty rep manager *)
  val size        : 'base manager -> int
  (* create a managed rep, store it with the manager *)
  val newRep      : 'base manager * 'base -> 'base rep 
  (* create an un-managed rep, which should only be used for temporary cases *)
  val newRep_     : 'base -> 'base rep 
  (* unwrap the rep and return the base type *)
  val repToBase   : 'base rep -> 'base
end

structure TypeRep : TYPE_REP =
struct
  (* Internally, rep uses a string as its unique identifier *)
  datatype 'base rep = Ty of 'base * string option
  datatype 'base manager = Tbl of 'base baseHash * 'base rep StringDict.t ref
  withtype 'base baseHash = 'base manager * 'base -> string
  type 'base repHash = 'base manager * 'base rep -> string

  val newManager : 'base baseHash -> 'base manager
    = fn hash => Tbl (hash, ref StringDict.empty)

  val size : 'base manager -> int
    = fn Tbl (_, dict) => StringDict.size (!dict)

  val hashRep : 'base repHash
    = fn (tbl as Tbl (hash, dict), Ty (x, id)) =>
      Utils.Option.dispatch (id, fn s => s, fn () => hash (tbl, x))

  val newRep : 'base manager * 'base -> 'base rep
    = fn (tbl as Tbl (hash, dict), x) =>
      let
        val id = hash (tbl, x)
      in
        case StringDict.lookup (!dict, id)
          of SOME r => r
           | NONE => 
            let
              val rep = Ty (x, SOME id)
              val ()  = dict := StringDict.insert (!dict, id, rep)
            in
              rep
            end
      end

  val newRep_ : 'base -> 'base rep
    = fn x => Ty (x, NONE)

  val repToBase : 'base rep -> 'base
    = fn Ty (x, id) => x

end

