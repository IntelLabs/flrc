(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, July 2008 *)

(*  Dominance module: Takes  a dominator  tree and  provides dominance
 * information. *)

signature DOMINANCE =
sig

  type t
  type node

  val new      : node Tree.t -> t
  val getTree  : t -> node Tree.t
  val dominates: t * node * node -> bool
  val contains : t * node -> bool

end

functor DominanceF (type node
                    val compare : node * node -> order
                   ) :> DOMINANCE where type node = node =
struct

  type node = node

  structure NodeDict = DictF (struct 
                                type t = node;
                                val compare = compare;
                              end);

  datatype t = DT of {tree : node Tree.t, 
                      ints : (int * int) NodeDict.t}

  val getTree : t -> node Tree.t =
   fn (DT {tree, ...}) => tree

  fun mkInts (t) =
      let
        val ints = ref NodeDict.empty
        fun addInt (n, (s, e)) = ints := NodeDict.insert (!ints, n, (s, e))
        fun doTree (Tree.T (n, children), s) =
            let
              val e = Vector.fold (children, s + 1, doTree)
              val () = addInt (n, (s, e))
            in e + 1
            end
        val _ = doTree (t, 0)
      in 
        !ints
      end
      
  val new : node Tree.t -> t =
   fn (tree) => DT {tree = tree, ints = mkInts (tree)}
   
  val contains : t * node -> bool =
   fn (DT {ints, ...}, n) => NodeDict.contains (ints, n)
             
  val dominates: t * node * node -> bool =
   fn (DT {ints, ...}, l1, l2) =>
      let
        val (s1, e1) = Option.valOf (NodeDict.lookup (ints, l1))
        val (s2, e2) = Option.valOf (NodeDict.lookup (ints, l2))
      in
        s1 <= s2 andalso e1 >= e2
      end

end
