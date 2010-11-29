(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation *)

(* This module defines how P concepts are modeled in core Mil.  There are three
 * modules defined here:
 *   PObjectModelCommon : P_OBJECT_MODEL_COMMON
 *   PObjectModelHigh : P_OBJECT_MODEL_HIGH
 *   PObjectModelLow : P_OBJECT_MODEL_LOW
 *
 * The Common structure defines P object model abstractions which are safe to use
 * at any point in the compiler.  This should be used in preference to the others
 * unless a pass needs to commit to an object model choice.
 *
 * The High structure includes the Common object model abstractions, and adds
 * abstractions that are only safe to use before lowering.  This structure 
 * should only be used in places where it can be guaranteed that lowering has 
 * not yet occurred, either through the observed presence of un-lowered constructs
 * or because of phase-ordering restrictions.
 *
 * The Low structure includes the Common object model abstractions, and adds
 * abstractions that are only safe to use after lowering.  This structure 
 * should only be used in places where it can be guaranteed that lowering has
 * occurred.  This is generally only possible via phase-ordering restrictions.
 *
 * Passes which require access to object model assumptions and cannot meet the 
 * the necessary guarantees as described above should be functorized over the
 * appropriate object model.  So for example, pass which intends to create 
 * P functions from scratch and which will be run both before and after
 * lowering might need to be functorized over the object model.
 * 
 * To faciliate this, the signatures are proper (matching) extensions as follows:
 *
 * P_OBJECT_MODEL_LOW <# P_OBJECT_MODEL_HIGH <# P_OBJECT_MODEL_COMMON
 *
 *)

(*************** Common object model assumptions ******************************)

signature P_OBJECT_MODEL_COMMON = 
sig

  structure Double : sig
    val td : Mil.tupleDescriptor
    val typ : Mil.typ
    val mk : Config.t * Mil.operand -> Mil.rhs
    val mkGlobal : Config.t * Real64.t -> Mil.global
    val ofValIndex : int
    val extract : Config.t * Mil.variable -> Mil.rhs
  end

  structure Float : sig
    val td : Mil.tupleDescriptor
    val typ : Mil.typ
    val mk : Config.t * Mil.operand -> Mil.rhs
    val mkGlobal : Config.t * Real32.t -> Mil.global
    val ofValIndex : int
    val extract : Config.t * Mil.variable -> Mil.rhs
  end

  structure IndexedArray : sig
    val tdVar : Config.t * Mil.fieldKind -> Mil.tupleDescriptor
    val fixedTyp : Config.t * int Identifier.NameDict.t * Mil.typ Vector.t
                   -> Mil.typ
    val varTyp : Config.t * Mil.typ -> Mil.typ
    val lenIndex : int
    val idxIndex : int
    val newFixed : Config.t
                   * int Identifier.NameDict.t
                   * Mil.fieldKind Vector.t
                   * Mil.variable
                   * Mil.operand Vector.t
                   -> Mil.rhs
    val idxSub : Config.t * Mil.fieldKind * Mil.variable * Mil.operand
                 -> Mil.rhs
  end

  structure OrdinalArray : sig
    val tdVar : Config.t * Mil.fieldKind -> Mil.tupleDescriptor
    val fixedTyp : Config.t * Mil.typ Vector.t -> Mil.typ
    val varTyp : Config.t * Mil.typ -> Mil.typ
    val lenIndex : int
    val newFixed : Config.t * Mil.fieldKind Vector.t * Mil.operand Vector.t
                   -> Mil.rhs
    val newVar : Config.t * Mil.fieldKind * Mil.operand -> Mil.rhs
    val length : Config.t * Mil.variable -> Mil.rhs
    val sub : Config.t * Mil.fieldKind * Mil.variable * Mil.operand -> Mil.rhs
    val update : Config.t * Mil.fieldKind * Mil.variable * Mil.operand
                 * Mil.operand
                 -> Mil.rhs
    val inited : Config.t * Mil.fieldKind * Mil.variable -> Mil.rhs
  end

  structure Rat : sig
    val useUnsafeIntegers : Config.t -> bool
    val td : Config.t -> Mil.tupleDescriptor
    val typ : Config.t -> Mil.typ
    val unboxTyp : Config.t -> Mil.typ
    val mk : Config.t * Mil.operand -> Mil.rhs
    val mkGlobal : Config.t * Mil.simple -> Mil.global
    val ofValIndex : Config.t -> int
    val extract : Config.t * Mil.variable -> Mil.rhs
  end

  val features : Config.Feature.feature list

end

(*************** High level object model assumptions **************************)
   signature P_OBJECT_MODEL_FUNCTION_HIGH =
   sig
     val closureTyp : Mil.typ Vector.t * Mil.typ Vector.t -> Mil.typ
     val mkUninit : Config.t * Mil.fieldKind Vector.t -> Mil.rhs
     val mkInit : Config.t
                  * Mil.variable option
                  * (Mil.fieldKind * Mil.operand) Vector.t
                  -> Mil.rhs
     val mkGlobal : Config.t 
                    * Mil.variable option 
                    * (Mil.fieldKind * Mil.operand) Vector.t 
                    -> Mil.global
     val init : Config.t
                * Mil.variable
                * Mil.variable option
                * (Mil.fieldKind * Mil.operand) Vector.t
                -> Mil.rhs list
     val getFv : Config.t * Mil.fieldKind Vector.t * Mil.variable * int
                 -> Mil.rhs
   end

   signature P_OBJECT_MODEL_OPTION_SET_HIGH =
   sig
     val typ : Mil.typ -> Mil.typ
     val empty : Config.t -> Mil.rhs
     val emptyGlobal : Config.t -> Mil.global
     val mk : Config.t * Mil.operand -> Mil.rhs
     val mkGlobal : Config.t * Mil.simple -> Mil.global
     val get : Config.t * Mil.variable -> Mil.rhs
   end

   signature P_OBJECT_MODEL_REF_HIGH =
   sig
     val typ : Mil.typ -> Mil.typ
   end

   signature P_OBJECT_MODEL_SUM_HIGH =
   sig
     val typ : Mil.typ Identifier.NameDict.t -> Mil.typ
     val mk : Config.t * Mil.name * Mil.fieldKind * Mil.operand -> Mil.rhs
     val mkGlobal : Config.t * Mil.name * Mil.fieldKind * Mil.simple
                    -> Mil.global
     val getVal : Config.t * Mil.variable * Mil.fieldKind * Mil.name -> Mil.rhs
   end

   signature P_OBJECT_MODEL_TYPE_HIGH =
   sig
     val typ : Mil.typ -> Mil.typ
     val mk : unit -> Mil.rhs
     val mkGlobal : unit -> Mil.global
   end

signature P_OBJECT_MODEL_HIGH = 
sig
  include P_OBJECT_MODEL_COMMON
  structure Function : P_OBJECT_MODEL_FUNCTION_HIGH
  structure OptionSet : P_OBJECT_MODEL_OPTION_SET_HIGH
  structure Ref : P_OBJECT_MODEL_REF_HIGH
  structure Sum : P_OBJECT_MODEL_SUM_HIGH
  structure Type : P_OBJECT_MODEL_TYPE_HIGH
end

(*************** Low level object model assumptions **************************)
   signature P_OBJECT_MODEL_FUNCTION_LOW =
   sig
     include P_OBJECT_MODEL_FUNCTION_HIGH
     val td : Config.t * Mil.fieldKind Vector.t -> Mil.tupleDescriptor
     val codeTyp : Mil.typ * Mil.typ Vector.t * Mil.typ Vector.t -> Mil.typ
     val codeIndex : int
     val fvIndex : int -> int
     val getCode : Config.t * Mil.variable -> Mil.rhs
     val doCall : Config.t
                  * Mil.variable         (* code pointer *)
                  * Mil.codes            (* possible codes *)
                  * Mil.variable         (* closure *)
                  * Mil.operand Vector.t (* args *)
                  -> Mil.call * Mil.operand Vector.t
   end

   signature P_OBJECT_MODEL_OPTION_SET_LOW =
   sig
     include P_OBJECT_MODEL_OPTION_SET_HIGH
     val td : Config.t -> Mil.tupleDescriptor
     val ofValIndex : int
     val query : Config.t * Mil.variable -> Mil.rhs * Mil.typ * Mil.constant
   end

   signature P_OBJECT_MODEL_REF_LOW =
   sig
     include P_OBJECT_MODEL_REF_HIGH
   end

   signature P_OBJECT_MODEL_SUM_LOW =
   sig
     include P_OBJECT_MODEL_SUM_HIGH
     val tagIndex : int
     val ofValIndex : int
     val getTag : Config.t * Mil.variable * Mil.fieldKind -> Mil.rhs
     val td : Config.t * Mil.fieldKind -> Mil.tupleDescriptor
   end

   signature P_OBJECT_MODEL_TYPE_LOW =
   sig
     include P_OBJECT_MODEL_TYPE_HIGH
     val td : Mil.tupleDescriptor
   end

signature P_OBJECT_MODEL_LOW = 
sig
  include P_OBJECT_MODEL_COMMON
  structure Function : P_OBJECT_MODEL_FUNCTION_LOW
  structure OptionSet : P_OBJECT_MODEL_OPTION_SET_LOW
  structure Ref : P_OBJECT_MODEL_REF_LOW
  structure Sum : P_OBJECT_MODEL_SUM_LOW
  structure Type : P_OBJECT_MODEL_TYPE_LOW
end


(*************** Object model implementations  **************************)

structure PObjectModelCommon :> P_OBJECT_MODEL_COMMON = 
struct

  structure M = Mil
  structure MU = MilUtils
  structure B = MU.Boxed
  structure OA = MU.OrdinalArray
  structure IA = MU.IndexedArray

  structure Double =
  struct

    val td = B.td (M.FkDouble)

    val typ = B.t (M.PokDouble, MU.Prims.NumericTyp.tDouble)

    fun mk (c, opnd) = B.box (c, M.PokDouble, M.FkDouble, opnd)

    fun mkGlobal (c, d) =
        B.boxGlobal (c, M.PokDouble, M.FkDouble,
                     M.SConstant (M.CDouble d))

    val ofValIndex = B.ofValIndex

    fun extract (c, v) = B.unbox (c, M.FkDouble, v)

  end

  structure Float =
  struct

    val td = B.td (M.FkFloat)

    val typ = B.t (M.PokFloat, MU.Prims.NumericTyp.tFloat)

    fun mk (c, opnd) = B.box (c, M.PokFloat, M.FkFloat, opnd)

    fun mkGlobal (c, f) =
        B.boxGlobal (c, M.PokFloat, M.FkFloat, M.SConstant (M.CFloat f))

    val ofValIndex = B.ofValIndex

    fun extract (c, v) = B.unbox (c, M.FkFloat, v)

  end

  structure IndexedArray =
  struct

    fun tdVar (c, fk) = IA.tdVar (c, fk)

    fun fixedTyp (c, d, ts) = IA.fixedTyp (c, M.PokDict, d, ts)

    fun varTyp (c, t) = IA.varTyp (c, M.PokDict, t)

    val lenIndex = IA.lenIndex
    val idxIndex = IA.idxIndex

    fun newFixed (c, d, fks, v, os) =
        IA.newFixed (c, M.PokDict, d, fks, v, os)

    fun idxSub (c, fk, v, opnd) = IA.idxSub (c, fk, v, opnd)

  end

  structure OrdinalArray =
  struct

    fun tdVar (c, fk) = OA.tdVar (c, fk)

    fun fixedTyp (c, ts) = OA.fixedTyp (c, M.PokArray, ts)

    fun varTyp (c, t) = OA.varTyp (c, M.PokArray, t)

    val lenIndex = OA.lenIndex

    fun newFixed (c, fks, os) = OA.newFixed (c, M.PokArray, fks, os)

    fun newVar (c, fk, opnd) = OA.newVar (c, M.PokArray, fk, opnd)

    fun length (c, v) = OA.length (c, v)

    fun sub (c, fk, v, opnd) = OA.sub (c, fk, v, opnd)

    fun update (c, fk, v, o1, o2) = OA.update (c, fk, v, o1, o2)

    fun inited (c, fk, v) = OA.inited (c, M.PokArray, fk, v)

  end

  structure Rat =
  struct

    val (useUnsafeIntegersF, useUnsafeIntegers) =
       Config.Feature.mk ("PObjectModel:prats-use-ints", "p rats implemented with machine ints (unsafe)")

    val fk = fn c => if useUnsafeIntegers c then 
                       MU.Sintp.fieldKind c
                     else
                       M.FkRef

    val td = fn c => B.td (fk c)

    val unboxTyp = fn c => if useUnsafeIntegers c then
                             MU.Sintp.t c
                           else 
                             MU.Prims.NumericTyp.tRat

    val typ = fn c => B.t (M.PokRat, unboxTyp c)

    val mk = fn (c, opnd) => B.box (c, M.PokRat, fk c, opnd)

    val mkGlobal = fn (c, s) => B.boxGlobal (c, M.PokRat, fk c, s)

    val ofValIndex = fn c => B.ofValIndex

    val extract = fn (c, v) => B.unbox (c, fk c, v)

  end

  val features = [Rat.useUnsafeIntegersF]
end (* structure PObjectModelCommon *)

structure PObjectModelHigh :> P_OBJECT_MODEL_HIGH = 
struct

  structure M = Mil
  structure MU = MilUtils
  structure B = MU.Boxed
  structure OA = MU.OrdinalArray
  structure IA = MU.IndexedArray
  structure POMC = PObjectModelCommon

  structure Double = POMC.Double

  structure Float = POMC.Float

  structure Function  =
  struct

    fun closureTyp (args, ress) = M.TClosure {args = args, ress = ress}

    fun mkUninit (c, fks) = M.RhsClosureMk {fvs = fks}

    fun mkInit (c, code, fkos) = M.RhsClosureInit {cls = NONE, code = code, fvs = fkos}

    fun mkGlobal (c, code, fvs) = M.GClosure {code = code, fvs = fvs}

    fun init (c, cls, code, fkos) = [M.RhsClosureInit {cls = SOME cls, code = code, fvs = fkos}]

    fun getFv (c, fks, cls, idx) = M.RhsClosureGetFv {fvs = fks, cls = cls, idx = idx}

  end (* structure Function *)

  structure IndexedArray = POMC.IndexedArray

  structure OrdinalArray = POMC.OrdinalArray

  structure OptionSet =
  struct

    fun typ t = M.TPType {kind = M.TkE, over = t}
    fun empty c = M.RhsSimple (M.SConstant M.COptionSetEmpty)
    fun emptyGlobal c = M.GSimple (M.SConstant M.COptionSetEmpty)
    fun mk (c, opnd) = M.RhsPSetNew opnd
    fun mkGlobal (c, s) = M.GPSet s
    fun get (c, v) = M.RhsPSetGet v
    fun query (c, opnd) = M.RhsPSetQuery opnd

  end (* structure OptionSet *)

  structure Rat = POMC.Rat

  structure Ref =
  struct

    fun typ t = Fail.unimplemented ("PObjectModelHigh.Ref", "typ", "*")

  end (* structure Ref *)

  structure Sum =
  struct

    fun typ nts = M.TPSum nts
    fun mk (c, tag, fk, ofVal) = M.RhsPSum {tag = tag, typ = fk, ofVal = ofVal}
    fun mkGlobal (c, tag, fk, ofVal) = M.GPSum {tag = tag, typ = fk, ofVal = ofVal}
    fun getVal (c, v, fk, tag) = M.RhsPSumProj {typ = fk, sum = v, tag = tag}

  end (* structure Sum *)

  structure Type =
  struct

    fun typ t = M.TPType {kind = M.TkI, over = M.TNone}
    fun mk () = M.RhsSimple (M.SConstant M.CTypePH)
    fun mkGlobal () = M.GSimple (M.SConstant M.CTypePH)

  end (* structure Type *)

  val features = []

end (* structure PObjectModelHigh *)

structure PObjectModelLow :> P_OBJECT_MODEL_LOW = 
struct

  structure M = Mil
  structure MU = MilUtils
  structure B = MU.Boxed
  structure OA = MU.OrdinalArray
  structure IA = MU.IndexedArray
  structure POMC = PObjectModelCommon

  structure Double = POMC.Double
  structure Float = POMC.Float

  structure Function  =
  struct

    fun codeTyp (cls, args, ress) =
        M.TCode {cc = M.CcCode, args = Utils.Vector.cons (cls, args), ress = ress}

    fun closureTyp (args, ress) =
        let
          (* The code's first argument is the closure itself.
           * To express this properly would require a recursive
           * type.  Instead we approximate with TRef.
           *)
          val ct = codeTyp (M.TRef, args, ress)
          val fts = Vector.new1 (ct, M.FvReadOnly)
        in
          MU.Typ.fixedArray (M.PokFunction, fts)
        end


    val codeIndex = 0
    fun fvIndex i = i + 1

    fun td (c, fks) =
      let
        val fks = Utils.Vector.cons (MU.FieldKind.nonRefPtr c, fks)
        fun doOne fk = M.FD {kind = fk, var = M.FvReadOnly}
        val fds = Vector.map (fks, doOne)
        val td = M.TD {fixed = fds, array = NONE}
      in td
      end

    fun mdd (c, fks) =
      let
        val pok = M.PokFunction
        val fks = Utils.Vector.cons (MU.FieldKind.nonRefPtr c, fks)
        fun doOne fk = M.FD {kind = fk, var = M.FvReadOnly}
        val fds = Vector.map (fks, doOne)
        val mdd = M.MDD {pok = pok, fixed = fds, array = NONE}
      in mdd
      end

    fun mkUninit (c, fks) =
        M.RhsTuple {mdDesc = mdd (c, fks), inits = Vector.new0 ()}

    fun codeOptToCodePtr (c, vo) = 
        case vo
         of SOME v => M.SVariable v
          | NONE => M.SConstant (MU.Uintp.zero c)

    fun mkInit (c, vo, fkos) =
        let
          val code = codeOptToCodePtr (c, vo)
          val (fks, os) = Vector.unzip fkos
        in
          M.RhsTuple {mdDesc = mdd (c, fks), inits = Utils.Vector.cons (code, os)}
        end

    fun mkGlobal (c, vo, fvs) =
        let
          val code = codeOptToCodePtr (c, vo)
          val (fks, inits) = Vector.unzip fvs
          val inits = Utils.Vector.cons (code, inits)
        in
          M.GTuple {mdDesc = mdd (c, fks), inits = inits}
        end

    fun init (c, cls, vo, fkos) =
        let
          val code = codeOptToCodePtr (c, vo)
          val (fks, os) = Vector.unzip fkos
          val td = td (c, fks)
          val codetf =
              M.TF {tupDesc = td, tup = cls, field = M.FiFixed codeIndex}
          val coderhs = M.RhsTupleSet {tupField = codetf, ofVal = code}
          fun doOne (i, opnd) =
              let
                val f = M.FiFixed (fvIndex i)
                val tf = M.TF {tupDesc = td, tup = cls, field = f}
                val rhs = M.RhsTupleSet {tupField = tf, ofVal = opnd}
              in rhs
              end
          val fvsrhs = List.mapi (Vector.toList os, doOne)
        in coderhs::fvsrhs
        end

    fun getCode (c, cls) =
        let
          val td = td (c, Vector.new0 ())
          val f = M.FiFixed codeIndex
          val rhs = M.RhsTupleSub (M.TF {tupDesc = td, tup = cls, field = f})
        in rhs
        end

    fun getFv (c, fks, cls, idx) =
        let
          val td = td (c, fks)
          val f = M.FiFixed (fvIndex idx)
          val rhs = M.RhsTupleSub (M.TF {tupDesc = td, tup = cls, field = f})
        in rhs
        end

    fun doCall (c, codev, code, clsv, args) =
        (M.CCode {ptr = codev, code = code}, Utils.Vector.cons (M.SVariable clsv, args))

  end

  structure IndexedArray = POMC.IndexedArray

  structure OrdinalArray = POMC.OrdinalArray

  structure OptionSet =
  struct

    fun typ t = MU.Typ.fixedArray (M.PokOptionSet, Vector.new1 (t, M.FvReadOnly))

    val ofValIndex = B.ofValIndex

    fun nulConst c = MU.Uintp.zero c

    fun empty c =
        let
          val pok = M.PokOptionSet
          val fd = M.FD {kind = M.FkRef, var = M.FvReadOnly}
          val mdd = M.MDD {pok = pok, fixed = Vector.new1 fd, array = NONE}
          val zero = M.SConstant (nulConst c)
        in M.RhsTuple {mdDesc = mdd, inits = Vector.new1 zero}
        end

    fun emptyGlobal c =
        let
          val pok = M.PokOptionSet
          val fd = M.FD {kind = M.FkRef, var = M.FvReadOnly}
          val mdd = M.MDD {pok = pok, fixed = Vector.new1 fd, array = NONE}
          val zero = M.SConstant (nulConst c)
        in M.GTuple {mdDesc = mdd, inits = Vector.new1 zero}
        end

    fun td c =
        let
          val fixed = Vector.new1 (M.FD {kind = M.FkRef, var = M.FvReadOnly})
          val td = M.TD {fixed = fixed, array = NONE}
        in td
        end

    fun mdd c =
        let
          val pok = M.PokOptionSet
          val fixed = Vector.new1 (M.FD {kind = M.FkRef, var = M.FvReadOnly})
          val mdd = M.MDD {pok = pok, fixed = fixed, array = NONE}
        in mdd
        end

    fun mk (c, opnd) =
        let
          val mdd = mdd c
          val rhs = M.RhsTuple {mdDesc = mdd, inits = Vector.new1 opnd}
        in rhs
        end

    fun mkGlobal (c, s) =
        let
          val mdd = mdd c
          val g = M.GTuple {mdDesc = mdd, inits = Vector.new1 s}
        in g
        end

    val ofValIndex = 0

    fun get (c, v) =
        let
          val td = td c
          val f = M.FiFixed ofValIndex
          val rhs = M.RhsTupleSub (M.TF {tupDesc = td, tup = v, field = f})
        in rhs
        end

    fun query (c, v) =
        (get (c, v), M.TRef, nulConst c)

  end

  structure Rat = POMC.Rat

  structure Ref =
  struct

    fun typ t =
        Fail.unimplemented ("PObjectModelLow.Ref", "typ", "*")

  end

  structure Sum = 
  struct

    fun typ nts = MU.Typ.fixedArray (M.PokTagged, Vector.new1 (M.TName, M.FvReadOnly))

    fun td (c, fk) =
        let
          val fds = Vector.new2 (M.FD {kind = M.FkRef, var = M.FvReadOnly},
                                 M.FD {kind = fk,      var = M.FvReadOnly})
          val td = M.TD {fixed = fds, array = NONE}
        in td
        end

    fun mdd (c, fk) =
        let
          val fds = Vector.new2 (M.FD {kind = M.FkRef, var = M.FvReadOnly},
                                 M.FD {kind = fk,      var = M.FvReadOnly})
          val mdd = M.MDD {pok = M.PokTagged, fixed = fds, array = NONE}
        in mdd
        end

    fun mk (c, tag, fk, ofVal) =
        let
          val mdd = mdd (c, fk)
          val inits = Vector.new2 (M.SConstant (M.CName tag), ofVal)
          val rhs = M.RhsTuple {mdDesc = mdd, inits = inits}
        in rhs
        end

    fun mkGlobal (c, tag, fk, ofVal) =
        let
          val mdd = mdd (c, fk)
          val inits = Vector.new2 (M.SConstant (M.CName tag), ofVal)
          val g = M.GTuple {mdDesc = mdd, inits = inits}
        in g
        end

    val tagIndex = 0
    val ofValIndex = 1

    fun getTag (c, v, fk) =
        let
          val td = td (c, fk)
          val f = M.FiFixed tagIndex
          val rhs = M.RhsTupleSub (M.TF {tupDesc = td, tup = v, field = f})
        in rhs
        end

    fun getVal (c, v, fk, tag) =
        let
          val td = td (c, fk)
          val f = M.FiFixed ofValIndex
          val rhs = M.RhsTupleSub (M.TF {tupDesc = td, tup = v, field = f})
        in rhs
        end

  end

  structure Type =
  struct

    fun typ t = MU.Typ.fixedArray (M.PokType, Vector.new0 ())

    val td = M.TD {fixed = Vector.new0 (), array = NONE}

    val mdd = M.MDD {pok = M.PokType, fixed = Vector.new0 (), array = NONE}

    fun mk () = M.RhsTuple {mdDesc = mdd, inits = Vector.new0 ()}
                    
    fun mkGlobal () = M.GTuple {mdDesc = mdd, inits = Vector.new0 ()}
                    
  end

  val features = []
end (* structure PObjectModelLow *)

