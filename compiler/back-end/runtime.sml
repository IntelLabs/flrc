(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

(* The Compiler/Runtime Contract/Interface *)

signature RUNTIME =
sig

  structure MD :
  sig
    val static : Pil.identifier 
    val isRefTyp : Pil.identifier 
    val pObjKindTag : Mil.pObjKind -> Pil.identifier
    val pObjKindMetaData : Mil.pObjKind -> Pil.identifier
    val alwaysMutable   : Pil.identifier 
    val createdMutable  : Pil.identifier 
    val alwaysImmutable : Pil.identifier 
    val register : Pil.identifier 
  end

  structure GC :
  sig
    val rseCallBack        : Pil.identifier
    val reportRoots        : Pil.identifier
    val registerGlobals    : Pil.identifier
    val writeBarrierRef    : Pil.identifier
    val writeBarrierRefOpt : Pil.identifier
    val vtableChange       : Pil.identifier
  end

  structure Name :
  sig
    val static              : Pil.identifier
    val getTag              : Pil.identifier
    val registerCoreCharOrd : Pil.identifier
  end

  structure T :
  sig

    val object    : Pil.identifier
    val rat       : Pil.identifier
    val integer   : Pil.identifier
    val idx       : Pil.identifier
    val pAny      : Pil.identifier
    val boolean   : Pil.identifier

    val viVec8    : Pil.identifier
    val viVec16   : Pil.identifier
    val viVec32   : Pil.identifier
    val viVec64   : Pil.identifier

    val vtable    : Pil.identifier
    val objectU   : Pil.identifier
    val ratU      : Pil.identifier
    val integerU  : Pil.identifier
    val nameU     : Pil.identifier
    val idxU      : Pil.identifier

    val futureStatus : Pil.identifier

  end

  structure Tuple :
  sig

    val vtable : Pil.identifier

    val fixedField : int -> Pil.identifier

    val xtras : Pil.identifier

    val static      : Pil.identifier
    val newFixed    : Pil.identifier
    val newVariable : Pil.identifier

  end

  structure Rat :
  sig

    val optMax : IntInf.t
    val optMin : IntInf.t

    val smallMax : Pil.identifier
    val smallMin : Pil.identifier

    val checkOpt : IntInf.t -> unit

    val staticDef     : Pil.identifier
    val staticRef     : Pil.identifier

    val optFromSInt32 : Pil.identifier

  end

  structure Integer :
  sig

    val optMax : IntInf.t
    val optMin : IntInf.t

    val smallMax : Pil.identifier
    val smallMin : Pil.identifier

    val checkOpt : IntInf.t -> unit

    val optFromSInt32 : Pil.identifier

    val signNeg       : Pil.identifier
    val signPos       : Pil.identifier
    val signZero      : Pil.identifier
    val staticCons    : Pil.identifier
    val staticEmpty   : Pil.identifier
    val static        : Pil.identifier
    val fromCString   : Pil.identifier

    val staticConsUnboxedDef   : Pil.identifier
    val staticConsRef          : Pil.identifier
    val staticDef              : Pil.identifier
    val staticRef              : Pil.identifier

  end


  structure Prim :
  sig
    val numericTyp : Mil.Prims.numericTyp -> Pil.T.t
    val call : Mil.Prims.t * bool * Pil.E.t option * Pil.E.t list -> Pil.S.t
  end

  structure Object :
  sig

    val getKind             : Pil.identifier
    val field               : Pil.identifier
    val extra               : Pil.identifier

    val fieldsBase          : Pil.identifier
    val setOffset           : Pil.identifier
    val setSize             : Pil.identifier
    val typeSize            : Pil.identifier
    val ratOffset           : Pil.identifier
    val ratSize             : Pil.identifier
    val floatOffset         : Pil.identifier
    val floatSize           : Pil.identifier
    val doubleOffset        : Pil.identifier
    val doubleSize          : Pil.identifier
    val arrayOLenOffset     : Pil.identifier
    val arrayOEltOffset     : Pil.identifier
    val arrayOBaseSize      : Pil.identifier
    val arrayILenOffset     : Pil.identifier
    val arrayIIdxOffset     : Pil.identifier
    val arrayIEltOffset     : Pil.identifier
    val arrayIBaseSize      : Pil.identifier
    val functionCodeOffset  : Pil.identifier
    val functionSize        : Pil.identifier
    val sumTagOffset        : Pil.identifier
    val sumValOffset        : Pil.identifier
    val sumSize             : Pil.identifier

  end

  structure Idx :
  sig

    val static      : Pil.identifier
    val staticEmpty : Pil.identifier
    val staticElt   : Pil.identifier
    val get         : Pil.identifier
    val set         : Pil.identifier

    val chooseLen   : int -> int
  end

  structure Thunk :
  sig

    val boxedTyp    : Mil.fieldKind -> Pil.identifier 
    val unboxedTyp  : Mil.fieldKind -> Pil.identifier 

    val staticValue : Mil.fieldKind -> Pil.identifier 
    val new         : Mil.fieldKind -> Pil.identifier 
    val newValue    : Mil.fieldKind -> Pil.identifier 
    val init        : Mil.fieldKind -> Pil.identifier 
    val setValue    : Mil.fieldKind -> Pil.identifier 
    val spawn       : Mil.fieldKind -> Pil.identifier 
    val isEvaled    : Mil.fieldKind -> Pil.identifier 
    val eval        : Mil.fieldKind -> Pil.identifier 
    val evalDirect  : Mil.fieldKind -> Pil.identifier 
    val return      : Mil.fieldKind -> Pil.identifier 
    val cut         : Mil.fieldKind -> Pil.identifier 
    val fixedSize   : Mil.fieldKind -> Pil.identifier 
    val vTable      : Mil.fieldKind -> Pil.identifier 
  end

  val exit      : Pil.identifier 
  val pmain     : Pil.identifier 
  val gErrorVal : Pil.identifier 

end

structure Runtime :> RUNTIME =
struct

  structure M = Mil
(*
  structure Vec =
  struct

    structure VI = VectorInstructions

    val static = Pil.identifier "pLsrViVectorStatic"

    fun elemType et = VI.stringOfElemTypeShort et

    fun loadF et   = Pil.identifier ("pLsrViLoad"    ^ elemType et ^ "Field")
    fun loadV et   = Pil.identifier ("pLsrViLoad"    ^ elemType et ^ "Extra")
    fun gather et  = Pil.identifier ("pLsrViGather"  ^ elemType et ^ "Extra")
    fun storeF et  = Pil.identifier ("pLsrViStore"   ^ elemType et ^ "Field")
    fun storeV et  = Pil.identifier ("pLsrViStore"   ^ elemType et ^ "Extra")
    fun scatter et = Pil.identifier ("pLsrViScatter" ^ elemType et ^ "Extra")

  end
*)


  structure MD =
  struct

    val static = Pil.identifier "pLsrVTableStatic"

    val isRefTyp = Pil.identifier "PgcIsRef"

    fun pObjKindTag pok =
        Pil.identifier
          (case pok
            of M.PokNone      => "VNoneTag"
             | M.PokRat       => "VRatTag"
             | M.PokFloat     => "VFloatTag"
             | M.PokDouble    => "VDoubleTag"
             | M.PokName      => "VNameTag"
             | M.PokFunction  => "VFunctionTag"
             | M.PokArray     => "VArrayTag"
             | M.PokDict      => "VArrayIdxTag"
             | M.PokTagged    => "VSumTag"
             | M.PokOptionSet => "VSetTag"
             | M.PokType      => "VTypeTag"
             | M.PokPtr       => "VRefTag"
             | M.PokCell      => "VThunkTag")
        
    fun pObjKindMetaData pok =
        Pil.identifier
          (case pok
            of M.PokNone      => "pLsrVTableNone"
             | M.PokRat       => "pLsrPRatVTable"
             | M.PokFloat     => "pLsrPFloatVTable"
             | M.PokDouble    => "pLsrPDoubleVTable"
             | M.PokName      => "pLsrPNameVTable"
             | M.PokFunction  => "pLsrClosureVTable"
             | M.PokArray     => "pLsrPArrayOVTable"
             | M.PokDict      => "pLsrPArrayIVTable"
             | M.PokTagged    => "pLsrPSumVTable"
             | M.PokOptionSet => "pLsrPSetVTable"
             | M.PokType      => "pLsrPTypeVTable"
             | M.PokPtr       => "pLsrPRefVTable"
             | M.PokCell      => Fail.fail ("Runtime", "pObjKindVtable", "Must use Thunk.vTable"))

    val alwaysMutable   = Pil.identifier "PGC_ALWAYS_MUTABLE"
    val createdMutable  = Pil.identifier "PGC_CREATED_MUTABLE"
    val alwaysImmutable = Pil.identifier "PGC_ALWAYS_IMMUTABLE"

    val register = Pil.identifier "pLsrVTableRegister"

  end

  structure GC =
  struct

    val rseCallBack        = Pil.identifier "PrtRseCallback"

    val reportRoots        = Pil.identifier "pLsrGcReportRoots"
    val registerGlobals    = Pil.identifier "pLsrGcRegisterGlobals"

    val writeBarrierRef    = Pil.identifier "pLsrWriteBarrierRefBase"
    val writeBarrierRefOpt = Pil.identifier "pLsrWriteBarrierRefOptBase"

    val vtableChange       = Pil.identifier "pLsrObjectChangeVTable"

  end

  structure Name =
  struct

    val static              = Pil.identifier "pLsrPNameStatic"
    val getTag              = Pil.identifier "pLsrPNameGetTag"

    val registerCoreCharOrd = Pil.identifier "pLsrRegisterCoreCharOrd"

  end

  structure T =
  struct

    val object    = Pil.identifier "PlsrObjectB"
    val rat       = Pil.identifier "PlsrRational"
    val integer   = Pil.identifier "PlsrInteger"
    val idx       = Pil.identifier "PlsrIdxB"
    val pAny      = Pil.identifier "PlsrPAny"
    val boolean   = Pil.identifier "PlsrBoolean"

    val viVec8    = Pil.identifier "PlsrViVec8"
    val viVec16   = Pil.identifier "PlsrViVec16"
    val viVec32   = Pil.identifier "PlsrViVec32"
    val viVec64   = Pil.identifier "PlsrViVec64"

    val vtable    = Pil.identifier "PlsrVTable"
    val objectU   = Pil.identifier "PlsrObjectU"
    val ratU      = Pil.identifier "PlsrRationalU"
    val integerU  = Pil.identifier "PlsrIntegerU"
    val nameU     = Pil.identifier "PlsrNameU"
    val idxU      = Pil.identifier "PlsrIdxU"

    val futureStatus = Pil.identifier "PlsrFutureStatus"

  end

  structure Tuple =
  struct

    val vtable = Pil.identifier "vtable"

    fun fixedField i = Pil.identifier ("f_" ^ Int.toString i)

    val xtras = Pil.identifier "extras"

    val static      = Pil.identifier "pLsrTupleStatic"
    val newFixed    = Pil.identifier "pLsrTupleNewFixed"
    val newVariable = Pil.identifier "pLsrTupleNewVariable"

  end

  structure Rat =
  struct

    val optMax = MilUtils.Rational.Opt.max
    val optMin = MilUtils.Rational.Opt.min

    val smallMax = Pil.identifier "pLsrPSmallRationalMax"
    val smallMin = Pil.identifier "pLsrPSmallRationalMin"

    fun checkOpt i =
        if IntInf.< (i, optMin) orelse IntInf.> (i, optMax) then
          Fail.fail ("Runtime", "checkOpt", "bad optimised rational")
        else
          ()

    val staticDef     = Pil.identifier "pLsrRationalStaticUnboxedDef"
    val staticRef     = Pil.identifier "pLsrRationalStaticRef"

    val optFromSInt32 = Pil.identifier "pLsrSmallRationalFromSInt32"

  end

  structure Integer =
  struct

    val optMax = MilUtils.Integer.Opt.max
    val optMin = MilUtils.Integer.Opt.min

    val smallMax = Pil.identifier "pLsrPSmallIntegerMax"
    val smallMin = Pil.identifier "pLsrPSmallIntegerMin"

    fun checkOpt i =
        if IntInf.< (i, optMin) orelse IntInf.> (i, optMax) then
          Fail.fail ("Runtime", "checkOpt", "bad optimised integer")
        else
          ()

    val optFromSInt32 = Pil.identifier "pLsrSmallIntegerFromSInt32"

    val signNeg       = Pil.identifier "pLsrIntegerSignNeg"
    val signPos       = Pil.identifier "pLsrIntegerSignPos"
    val signZero      = Pil.identifier "pLsrIntegerSignZero"
    val staticCons    = Pil.identifier "pLsrIntegerDigitListStaticCons"
    val staticEmpty   = Pil.identifier "pLsrIntegerDigitListStaticEmpty"
    val static        = Pil.identifier "pLsrIntegerStatic"
    val fromCString   = Pil.identifier "pLsrIntegerFromCString"

    val staticConsUnboxedDef   
      = Pil.identifier "pLsrIntegerDigitListStaticConsUnboxedDef"
    val staticConsRef   
      = Pil.identifier "pLsrIntegerDigitListStaticConsRef"
    val staticDef   
      = Pil.identifier "pLsrIntegerStaticUnboxedDef"
    val staticRef    
      = Pil.identifier "pLsrIntegerStaticRef"

  end

  structure Prim =
  struct
    structure P = Mil.Prims
    structure PU = MilUtils.Prims.Utils

    val numericTyp : P.numericTyp -> Pil.T.t = 
     fn nt => 
        (case nt
          of P.NtRat                                  => Pil.T.named T.rat
           | P.NtInteger ip                           => 
             (case ip
               of P.IpArbitrary                       => Pil.T.named T.integer
                | P.IpFixed (IntArb.T x)              =>
                  (case x
                    of (IntArb.S8,   IntArb.Signed  ) => Pil.T.sint8
                     | (IntArb.S16,  IntArb.Signed  ) => Pil.T.sint16
                     | (IntArb.S32,  IntArb.Signed  ) => Pil.T.sint32
                     | (IntArb.S64,  IntArb.Signed  ) => Pil.T.sint64
                     | (IntArb.S8,   IntArb.Unsigned) => Pil.T.uint8
                     | (IntArb.S16,  IntArb.Unsigned) => Pil.T.uint16
                     | (IntArb.S32,  IntArb.Unsigned) => Pil.T.uint32
                     | (IntArb.S64,  IntArb.Unsigned) => Pil.T.uint64))
           | P.NtFloat fp                             => 
             (case fp
               of P.FpSingle                          => Pil.T.float
                | P.FpDouble                          => Pil.T.double))


    fun thnk t = if t then "T" else ""

    val getFloatPrecisionName = 
     fn fp => 
        (case fp
          of P.FpSingle => "Float32"
           | P.FpDouble => "Float64")

    val getIntPrecisionName = 
        fn ip => 
           (case ip
             of P.IpArbitrary => "Integer"
              | P.IpFixed t   => IntArb.stringOfTyp t)

    val getNumericTypName =
     fn nt =>
        (case nt
          of P.NtRat         => "Rational"
           | P.NtInteger ip  => getIntPrecisionName ip
           | P.NtFloat fp    => getFloatPrecisionName fp)

    val getDivKindName = PU.ToString.divKind

    val getArithOpName = PU.ToString.arithOp 

    val getFloatOpName = PU.ToString.floatOp

    val getBitwiseOpName = PU.ToString.bitwiseOp

    val getLogicOpName = PU.ToString.logicOp

    val getStringOpName = PU.ToString.stringOp

    val getCompareOpName =
     fn c => 
        case c
         of P.CEq => "EQ"
          | P.CNe => "NE"
          | P.CLt => "LT"
          | P.CLe => "LE"

    val getNumArithName 
      = fn {typ, operator} =>
        "pLsrPrim" ^ (getNumericTypName typ) ^ (getArithOpName operator)

    val getFloatOpName =
     fn {typ, operator} =>
        "pLsrPrim" ^ (getFloatPrecisionName typ) ^ (getFloatOpName operator)

    val getNumCompareName = 
     fn {typ, operator} =>
        "pLsrPrim" ^ (getNumericTypName typ) ^ (getCompareOpName operator)

    val getNumConvertName = 
     fn {to, from} =>
        "pLsrPrim" ^ (getNumericTypName to) ^ "From" ^ (getNumericTypName from)

    val getBitwiseName = 
     fn {typ, operator} =>
        "pLsrPrim" ^ (getIntPrecisionName typ) ^ (getBitwiseOpName operator)

    val getBooleanName = 
     fn l =>
        "pLsrPrim" ^ "Boolean" ^ (getLogicOpName l)

    val getCStringName = 
     fn s =>
        "pLsrPrim" ^ "CString" ^ (getStringOpName s)

    val getPrimName = 
     fn (p, t) =>
        (case p
          of P.PNumArith r            => getNumArithName r
           | P.PFloatOp r             => getFloatOpName r
           | P.PNumCompare r          => getNumCompareName r 
           | P.PNumConvert r          => getNumConvertName r
           | P.PBitwise r             => getBitwiseName r
           | P.PBoolean r             => getBooleanName r
           | P.PCString r             => getCStringName r)

    val getRuntimeName = 
     fn (rt, t) => "pLsr" ^ PU.ToString.runtime rt ^ thnk t

(*    local
      open VectorInstructions
    in

    fun genElemTypSuffix et =
        case et
         of P.ViUInt8   => "UI8"
          | P.ViUInt16  => "UI16"
          | P.ViUInt32  => "UI32"
          | P.ViUInt64  => "UI64"
          | P.ViSInt8   => "SI8"
          | P.ViSInt16  => "SI16"
          | P.ViSInt32  => "SI32"
          | P.ViSInt64  => "SI64"
          | P.ViFloat16 => "F16"
          | P.ViFloat32 => "F32"
          | P.ViFloat64 => "F64"

    fun getViName (p, t) = 
        case p 
         of P.ViShiftL et        => "pLsrViShiftL" ^ genElemTypSuffix et
          | P.ViShiftA et        => "pLsrViShiftA" ^ genElemTypSuffix et
          | P.ViRotateL et       => "pLsrViRotateL" ^ genElemTypSuffix et
          | P.ViRotateR et       => "pLsrViRotateR" ^ genElemTypSuffix et
          | P.ViBitNot et        => "pLsrViBitNot" ^ genElemTypSuffix et
          | P.ViBitAnd et        => "pLsrViBitAnd" ^ genElemTypSuffix et
          | P.ViBitXor et        => "pLsrViBitXor" ^ genElemTypSuffix et
          | P.ViBitOr et         => "pLsrViBitOr" ^ genElemTypSuffix et
          | P.ViNot et           => "pLsrViNot" ^ genElemTypSuffix et
          | P.ViAnd et           => "pLsrViAnd" ^ genElemTypSuffix et
          | P.ViOr et            => "pLsrViOr" ^ genElemTypSuffix et
          | P.ViMaskNot et       => "pLsrViMaskNot" ^ genElemTypSuffix et
          | P.ViMaskAnd et       => "pLsrViMaskAnd" ^ genElemTypSuffix et
          | P.ViMaskOr  et       => "pLsrViMaskOr" ^ genElemTypSuffix et
          | P.ViAdd et           => "pLsrViAdd" ^ genElemTypSuffix et
          | P.ViSub et           => "pLsrViSub" ^ genElemTypSuffix et
          | P.ViMul et           => "pLsrViMul" ^ genElemTypSuffix et
          | P.ViDiv et           => "pLsrViDiv" ^ genElemTypSuffix et
          | P.ViMod et           => "pLsrViMod" ^ genElemTypSuffix et
          | P.ViFma et           => "pLsrViFma" ^ genElemTypSuffix et
          | P.ViFms et           => "pLsrViFms" ^ genElemTypSuffix et
          | P.ViMax et           => "pLsrViMax" ^ genElemTypSuffix et
          | P.ViMin et           => "pLsrViMin" ^ genElemTypSuffix et
          | P.ViNeg et           => "pLsrViNeg" ^ genElemTypSuffix et
          | P.ViSqrt et          => "pLsrViSqrt" ^ genElemTypSuffix et
          | P.ViSqrtRcp et       => "pLsrViSqrtRcp" ^ genElemTypSuffix et
          | P.ViRcp et           => "pLsrViRcp" ^ genElemTypSuffix et
          | P.ViExp2 et          => "pLsrViExp2" ^ genElemTypSuffix et
          | P.ViExp2m1 et        => "pLsrViExp2m1" ^ genElemTypSuffix et
          | P.ViLog2 et          => "pLsrViLog2" ^ genElemTypSuffix et
          | P.ViLog2p1 et        => "pLsrViLog2p1" ^ genElemTypSuffix et
          | P.ViSin et           => "pLsrViSin" ^ genElemTypSuffix et
          | P.ViAsin et          => "pLsrViAsin" ^ genElemTypSuffix et
          | P.ViCos et           => "pLsrViCos" ^ genElemTypSuffix et
          | P.ViAcos et          => "pLsrViAcos" ^ genElemTypSuffix et
          | P.ViTan et           => "pLsrViTan" ^ genElemTypSuffix et
          | P.ViAtan et          => "pLsrViAtan" ^ genElemTypSuffix et
          | P.ViSign et          => "pLsrViSign" ^ genElemTypSuffix et
          | P.ViAbs et           => "pLsrViAbs" ^ genElemTypSuffix et
          | P.ViEq et            => "pLsrViEq" ^ genElemTypSuffix et
          | P.ViNe et            => "pLsrViNe" ^ genElemTypSuffix et
          | P.ViGt et            => "pLsrViGt" ^ genElemTypSuffix et
          | P.ViGe et            => "pLsrViGe" ^ genElemTypSuffix et
          | P.ViLt et            => "pLsrViLt" ^ genElemTypSuffix et
          | P.ViLe et            => "pLsrViLe" ^ genElemTypSuffix et
          | P.ViSelect et        => "pLsrViSelect" ^ genElemTypSuffix et
          | P.ViPermute (et, _)  => "pLsrViPermute" ^ genElemTypSuffix et
          | P.ViInit et          => "pLsrViInit" ^ genElemTypSuffix et

    end
    *)
    fun getName (p, t) =
        let
          val s =
              case p
               of P.Prim p     => getPrimName (p, t)
                | P.Runtime rt => getRuntimeName (rt, t)
                | P.Vector p       => Fail.fail ("Runtime.Prims", "getName", "Vector instructions not implemented")
        in Pil.identifier s
        end

    fun call (p, t, d, args) = 
      let
        val m = Pil.E.namedConstant (getName (p, t))
        val s = 
            Pil.S.expr (case (d, p)
                         of (SOME d, P.Prim p) => Pil.E.call (m, d::args)
                          | (SOME d, _)        => Pil.E.assign (d, Pil.E.call (m, args))
                          | (NONE, _)          => Pil.E.call (m, args))
      in s
      end

  end

  structure Object =
  struct

    val getKind             = Pil.identifier "pLsrObjectGetKind"
    val field               = Pil.identifier "pLsrObjectField"
    val extra               = Pil.identifier "pLsrObjectExtra"

    val fieldsBase          = Pil.identifier "pLsrObjectFieldsBase"
    val setOffset           = Pil.identifier "pLsrPSetOffset"
    val setSize             = Pil.identifier "pLsrPSetSize"
    val typeSize            = Pil.identifier "pLsrPTypeSize"
    val ratOffset           = Pil.identifier "pLsrPRatOffset"
    val ratSize             = Pil.identifier "pLsrPRatSize"
    val floatOffset         = Pil.identifier "pLsrPFloatOffset"
    val floatSize           = Pil.identifier "pLsrPFloatSize"
    val doubleOffset        = Pil.identifier "pLsrPDoubleOffset"
    val doubleSize          = Pil.identifier "pLsrPDoubleSize"
    val arrayOLenOffset     = Pil.identifier "pLsrPArrayOLenOffset"
    val arrayOEltOffset     = Pil.identifier "pLsrPArrayOEltOffset"
    val arrayOBaseSize      = Pil.identifier "pLsrPArrayOBaseSize"
    val arrayILenOffset     = Pil.identifier "pLsrPArrayILenOffset"
    val arrayIIdxOffset     = Pil.identifier "pLsrPArrayIIdxOffset"
    val arrayIEltOffset     = Pil.identifier "pLsrPArrayIEltOffset"
    val arrayIBaseSize      = Pil.identifier "pLsrPArrayIBaseSize"
    val functionCodeOffset  = Pil.identifier "pLsrPFunctionCodeOffset"
    val functionSize        = Pil.identifier "pLsrPFunctionSize"
    val sumTagOffset        = Pil.identifier "pLsrPSumTagOffset"
    val sumValOffset        = Pil.identifier "pLsrPSumValOffset"
    val sumSize             = Pil.identifier "pLsrPSumSize"

  end

  structure Idx =
  struct

    val static      = Pil.identifier "pLsrIdxStatic"
    val staticEmpty = Pil.identifier "pLsrIdxEmpty"
    val staticElt   = Pil.identifier "pLsrIdxEltStatic"
    val get         = Pil.identifier "pLsrIdxGet"
    val set         = Pil.identifier "pLsrIdxSet"

    fun chooseLen len =
        let
          fun loop dlen = 
              if 2*dlen < 3*len then loop (dlen *2)
              else dlen
        in (loop 1)
        end

  end

  structure Thunk =
  struct

    fun typ fk =
        case fk
         of M.FkRef         => "Ref"
          | M.FkBits M.Fs8  =>
            Fail.unimplemented ("Runtime.Thunk", "typ", "B8")
          | M.FkBits M.Fs16 =>
            Fail.unimplemented ("Runtime.Thunk", "typ", "B16")
          | M.FkBits M.Fs32 => "32"
          | M.FkBits M.Fs64 => "64"
          | M.FkFloat       => "Float"
          | M.FkDouble      => "Double"

    fun boxedTyp    fk = Pil.identifier ("PlsrThunkB"           ^ typ fk)
    fun unboxedTyp  fk = Pil.identifier ("PlsrThunkU"           ^ typ fk)

    fun staticValue fk = Pil.identifier ("pLsrThunkStaticValue" ^ typ fk)
    fun new         fk = Pil.identifier ("pLsrThunkNew"         ^ typ fk)
    fun newValue    fk = Pil.identifier ("pLsrThunkNewValue"    ^ typ fk)
    fun init        fk = Pil.identifier ("pLsrThunkSetInit"     ^ typ fk)
    fun setValue    fk = Pil.identifier ("pLsrThunkSetValue"    ^ typ fk)
    fun spawn       fk = Pil.identifier ("pLsrThunkSpawn"       ^ typ fk)
    fun isEvaled    fk = Pil.identifier ("pLsrThunkIsEvaled"    ^ typ fk)
    fun eval        fk = Pil.identifier ("pLsrThunkEval"        ^ typ fk)
    fun evalDirect  fk = Pil.identifier ("pLsrThunkEvalDirect"  ^ typ fk)
    fun return      fk = Pil.identifier ("pLsrThunkReturn"      ^ typ fk)
    fun cut         fk = Pil.identifier ("pLsrThunkCut"         ^ typ fk)
    fun fixedSize   fk = Pil.identifier ("pLsrThunkFixedSize"   ^ typ fk)
    fun vTable      fk = Pil.identifier ("pLsrThunkValVTable"   ^ typ fk)
  end

  val exit = Pil.identifier "pLsrExit"
  val pmain = Pil.identifier "__pmain"
  val gErrorVal = Pil.identifier "pLsrErrorVal"

end;
