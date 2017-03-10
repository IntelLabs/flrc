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


(*  Dominance module: Takes  a dominator  tree and  provides dominance
 * information.
 *)

signature DOMINANCE =
sig

  type t
  type node

  (* Note: Cfgs may contain nodes that are not in the obvious dominator
   * tree.
   *)
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
