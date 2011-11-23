(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

signature MIL_REP_BASE = 
sig

  datatype 'node iInfo = 
           IiCode of {cargs : 'node Mil.callConv, args : 'node Vector.t, returns : 'node Vector.t}
         | IiMetaData of {pok : Mil.pObjKind, pinned : bool, fixed : 'node Vector.t, array : (int * 'node) option}
         | IiTupleDescriptor of {fixed : 'node Vector.t, array : 'node option}
         | IiThunk of {typ : 'node, fvs : 'node Vector.t}
         | IiClosure of 'node Vector.t
         | IiSum of 'node * ('node Vector.t)

  val layoutIInfo : Config.t * Mil.symbolInfo * 'node iInfo * ('node -> Layout.t) -> Layout.t

  structure PObjKindLat : LATTICE where type element = Mil.pObjKind
  structure NameIntDict : DICT where type key = Mil.name * int
  datatype 'a edge = EUnify of 'a * 'a | EFlow of 'a * 'a

end  (* signature MIL_REP_BASE *)

structure MilRepBase :> MIL_REP_BASE = 
struct
  structure M = Mil
  structure MU = MilUtils
  structure LU = LayoutUtils
  structure L = Layout
  structure PD = PassData

  datatype 'node iInfo = 
           IiCode of {cargs : 'node Mil.callConv, args : 'node Vector.t, returns : 'node Vector.t}
         | IiMetaData of {pok : Mil.pObjKind, pinned : bool, fixed : 'node Vector.t, array : (int * 'node) option}
         | IiTupleDescriptor of {fixed : 'node Vector.t, array : 'node option}
         | IiThunk of {typ : 'node, fvs : 'node Vector.t}
         | IiClosure of 'node Vector.t
         | IiSum of 'node * ('node Vector.t)

  val layoutIInfo =
   fn (config, si, info, node) => 
      let
        val vector = fn nv => Vector.toListMap (nv, node)
        val l = 
            (case info
              of IiCode {cargs, args, returns} => 
                 let
                   val cargs = MilLayout.layoutCallConv (fn (_, _, n) => node n) (config, si, cargs)
                   val args = LU.parenSeq (vector args)
                   val returns = L.seq [L.str " => ", LU.parenSeq (vector returns)]
                 in  L.mayAlign [cargs, args, returns]
                 end
               | IiMetaData {pok, pinned, fixed, array} => 
                 let
                   val pok = L.str (MU.PObjKind.toString pok)
                   val pinned = if pinned then L.str "!" else L.str ""
                   val fixed = vector fixed
                   val array = 
                       (case array
                         of NONE => []
                          | SOME (i, n) => [LU.bracket (L.seq [Int.layout i, L.str " : ", node n])])
                   val elts = LU.angleSeq (fixed @ array)
                 in L.mayAlign [L.seq [L.str "MetaData ", pok, pinned], elts]
                 end
               | IiTupleDescriptor {fixed, array} => 
                 let
                   val fixed = vector fixed
                   val array = 
                       (case array
                         of NONE => []
                          | SOME n => [LU.bracket (node n)])
                   val elts = LU.angleSeq (fixed @ array)
                 in L.mayAlign [L.str "TupleDesc ", elts]
                 end
               | IiThunk {typ, fvs} =>
                 L.seq [L.str "Thunk", LU.paren (node typ), LU.angleSeq (vector fvs)]
               | IiClosure fvs =>
                 L.seq [L.str "Closure", LU.angleSeq (vector fvs)]
               | IiSum (n, v) => 
                 L.seq [L.str "Sum", LU.paren (node n), LU.angleSeq (vector v)])
      in l
      end

  structure PObjKindLat = FlatLatticeFn (struct
                                           type element = M.pObjKind
                                           val equal = MilUtils.PObjKind.eq
                                         end)
 
  structure NameIntDict = DictF (struct
                                   type t = Mil.name * int
                                   val compare = Compare.pair (Identifier.nameCompare, Int.compare)
                                 end)

  datatype 'a edge = EUnify of 'a * 'a | EFlow of 'a * 'a

end (* structure MilRepBase *)