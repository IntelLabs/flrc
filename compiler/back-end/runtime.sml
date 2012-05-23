(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

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
    val paddingField : int -> Pil.identifier

    val xtras : Pil.identifier

    val static            : Pil.identifier
    val newFixed          : Pil.identifier
    val newVariable       : Pil.identifier
    val newPinnedFixed    : Pil.identifier
    val newPinnedVariable : Pil.identifier


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

    val staticMk      : Pil.identifier
    val staticInit    : Pil.identifier

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
    val staticUnboxed : Pil.identifier
    val staticInit    : Pil.identifier

    val staticConsUnboxedDef   : Pil.identifier
    val staticConsRef          : Pil.identifier
    val staticDef              : Pil.identifier
    val staticRef              : Pil.identifier

  end

  structure Prims :
  sig
    val vectorTyp      : Config.t * Mil.Prims.vectorSize * Mil.fieldKind -> Pil.T.t
    val numericTyp     : Config.t * Mil.Prims.numericTyp -> Pil.T.t
    (*                    dests              prim          thnk?  typs                     args            call  *)
    val call : Config.t * Pil.E.t Vector.t * Mil.Prims.t * bool * Mil.fieldKind Vector.t * Pil.E.t list -> Pil.S.t
    val vectorLoadF    : Config.t * Mil.Prims.vectorDescriptor * Mil.fieldKind -> Pil.identifier
    val vectorLoadVS   : Config.t * Mil.Prims.vectorDescriptor * Mil.fieldKind -> Pil.identifier
    val vectorLoadVI   : Config.t * Mil.Prims.vectorDescriptor * Mil.fieldKind -> Pil.identifier
    val vectorLoadVVS  : Config.t * Mil.Prims.vectorDescriptor * Mil.fieldKind -> Pil.identifier
    val vectorLoadVVI  : Config.t * Mil.Prims.vectorDescriptor * Mil.fieldKind -> Pil.identifier
    val vectorStoreF   : Config.t * Mil.Prims.vectorDescriptor * Mil.fieldKind -> Pil.identifier
    val vectorStoreVS  : Config.t * Mil.Prims.vectorDescriptor * Mil.fieldKind -> Pil.identifier
    val vectorStoreVI  : Config.t * Mil.Prims.vectorDescriptor * Mil.fieldKind -> Pil.identifier
    val vectorStoreVVS : Config.t * Mil.Prims.vectorDescriptor * Mil.fieldKind -> Pil.identifier
    val vectorStoreVVI : Config.t * Mil.Prims.vectorDescriptor * Mil.fieldKind -> Pil.identifier

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

    val boxedTyp       : Mil.fieldKind -> Pil.identifier 
    val unboxedTyp     : Mil.fieldKind -> Pil.identifier 
    val returnTyp      : Mil.fieldKind -> Pil.identifier

    val staticValue    : Mil.fieldKind -> Pil.identifier 
    val new            : Mil.fieldKind -> Pil.identifier 
    val newValue       : Mil.fieldKind -> Pil.identifier 
    val init           : Mil.fieldKind -> Pil.identifier 
    val initValue      : Mil.fieldKind -> Pil.identifier 
    val setValue       : Mil.fieldKind -> Pil.identifier 
    val spawn          : Mil.fieldKind -> Pil.identifier 
    val isEvaled       : Mil.fieldKind -> Pil.identifier 
    val eval           : Mil.fieldKind -> Pil.identifier 
    val evalDirect     : Mil.fieldKind -> Pil.identifier 
    val call           : Mil.fieldKind -> Pil.identifier 
    val callDirect     : Mil.fieldKind -> Pil.identifier 
    val tailCall       : Mil.fieldKind -> Pil.identifier 
    val tailCallDirect : Mil.fieldKind -> Pil.identifier 
    val return         : Mil.fieldKind -> Pil.identifier 
    val cut            : Mil.fieldKind -> Pil.identifier 
    val fixedSize      : Mil.fieldKind -> Pil.identifier 
    val resultOffset   : Mil.fieldKind -> Pil.identifier 
    val vTable         : Mil.fieldKind -> Pil.identifier 
    val blackHole      : Mil.fieldKind -> Pil.identifier
    val zeroFV         : unit -> Pil.identifier
  end

  val exit      : Pil.identifier 
  val halt      : Pil.identifier
  val haltV     : Pil.identifier
  val pmain     : Pil.identifier 
  val gErrorVal : Pil.identifier 

end

structure Runtime :> RUNTIME =
struct

  structure M = Mil
  structure P = M.Prims
  structure PU = MilUtils.Prims.Utils

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
             | M.PokPtr       => "VPtrTag"
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

    val reportRoots        = Pil.identifier "pLsrPPilerReportRoots"
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
    fun paddingField i = Pil.identifier ("p_" ^ Int.toString i)

    val xtras = Pil.identifier "extras"

    val static            = Pil.identifier "pLsrTupleStatic"
    val newFixed          = Pil.identifier "pLsrTupleNewFixed"
    val newVariable       = Pil.identifier "pLsrTupleNewVariable"
    val newPinnedFixed    = Pil.identifier "pLsrTupleNewPinnedFixed"
    val newPinnedVariable = Pil.identifier "pLsrTupleNewPinnedVariable"

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

    val staticMk      = Pil.identifier "pLsrRationalStaticUnboxedMk"
    val staticInit    = Pil.identifier "pLsrRationalStaticInit"

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
    val staticUnboxed = Pil.identifier "pLsrIntegerStaticUnboxed"
    val staticInit    = Pil.identifier "pLsrIntegerStaticGlobalInit"

    val staticConsUnboxedDef   
      = Pil.identifier "pLsrIntegerDigitListStaticConsUnboxedDef"
    val staticConsRef   
      = Pil.identifier "pLsrIntegerDigitListStaticConsRef"
    val staticDef   
      = Pil.identifier "pLsrIntegerStaticUnboxedDef"
    val staticRef    
      = Pil.identifier "pLsrIntegerStaticRef"

  end

  structure Prims =
  struct
    structure P = Mil.Prims
    structure PU = MilUtils.Prims.Utils

    val getFieldKindName = 
        fn (config, fk) => 
           (case fk 
             of M.FkRef         => "Ref"
              | M.FkBits M.Fs8  => "B8"
              | M.FkBits M.Fs16 => "B16"
              | M.FkBits M.Fs32 => "B32"
              | M.FkBits M.Fs64 => "B64"
              | M.FkFloat       => "F32"
              | M.FkDouble      => "F64")

    val getVectorSizeName : Config.t * Mil.Prims.vectorSize -> string = PU.ToString.vectorSize 
    val getVectorTypName : Config.t * Mil.Prims.vectorSize -> string = 
        fn (config, vs) => "PlsrVector" ^ getVectorSizeName (config, vs)

    val vectorTyp : Config.t * Mil.Prims.vectorSize * Mil.fieldKind -> Pil.T.t = 
     fn (config, vs, fk) => Pil.T.named (Pil.identifier (getVectorTypName (config, vs) ^ getFieldKindName (config, fk)))

    val numericTyp : Config.t * Mil.Prims.numericTyp -> Pil.T.t = 
     fn (config, nt) => 
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
     fn (config, fp) => 
        (case fp
          of P.FpSingle => "Float32"
           | P.FpDouble => "Float64")

    val getIntPrecisionName = 
        fn (config, ip) => 
           (case ip
             of P.IpArbitrary => "Integer"
              | P.IpFixed t   => IntArb.stringOfTyp t)

    val getNumericTypName =
     fn (config, nt) =>
        (case nt
          of P.NtRat         => "Rational"
           | P.NtInteger ip  => getIntPrecisionName (config, ip)
           | P.NtFloat fp    => getFloatPrecisionName (config, fp))

    val getDivKindName = PU.ToString.divKind

    val getArithOpName = PU.ToString.arithOp 

    val getFloatOpName = PU.ToString.floatOp

    val getBitwiseOpName = PU.ToString.bitwiseOp

    val getLogicOpName = PU.ToString.logicOp

    val getNameOpName = PU.ToString.nameOp

    val getStringOpName = PU.ToString.stringOp

    val getDataOpName = 
     fn (config, d, typs) => 
        let
          val d = PU.ToString.dataOp (config, d)
          val typs = Vector.map (typs, fn t => getFieldKindName (config, t))
          val typs = String.concatV typs
        in d ^ typs
        end

    val getAssocName = PU.ToString.assoc

    val getCompareOpName =
     fn (config, c) => 
        case c
         of P.CEq => "EQ"
          | P.CNe => "NE"
          | P.CLt => "LT"
          | P.CLe => "LE"

    val getNumArithName 
      = fn (config, {typ, operator}) =>
        (getNumericTypName (config, typ)) ^ (getArithOpName (config, operator))

    val getFloatOpName =
     fn (config, {typ, operator}) =>
        (getFloatPrecisionName (config, typ)) ^ (getFloatOpName (config, operator))

    val getNumCompareName = 
     fn (config, {typ, operator}) =>
        (getNumericTypName (config, typ)) ^ (getCompareOpName (config, operator))

    val getNumConvertName = 
     fn (config, {to, from}) =>
        (getNumericTypName (config, to)) ^ "From" ^ (getNumericTypName (config, from))

    val getBitwiseName = 
     fn (config, {typ, operator}) =>
        (getIntPrecisionName (config, typ)) ^ (getBitwiseOpName (config, operator))

    val getBooleanName = 
     fn (config, l) =>
        "Boolean" ^ (getLogicOpName (config, l))

    fun getNameName (config, no) = "Name" ^ getNameOpName (config, no)

    val getCStringName = 
     fn (config, s) =>
        "CString" ^ (getStringOpName (config, s))

    val getPrimName = 
     fn (config, p) =>
        (case p
          of P.PNumArith r            => getNumArithName(config, r)
           | P.PFloatOp r             => getFloatOpName(config, r)
           | P.PNumCompare r          => getNumCompareName(config, r) 
           | P.PNumConvert r          => getNumConvertName(config, r)
           | P.PBitwise r             => getBitwiseName(config, r)
           | P.PBoolean r             => getBooleanName(config, r)
           | P.PName r                => getNameName(config, r)
           | P.PCString r             => getCStringName(config, r)
           | P.PPtrEq                 => "PtrEq")

    val getRuntimeName = 
     fn (config, rt, t) => PU.ToString.runtime (config, rt) ^ thnk t

    val getVectorName = 
     fn (config, v, typs) => 
        let
          val doOne = 
           fn (name, descriptor1, descriptor2O, operator) =>
              let
                val vsName = fn desc => getVectorSizeName (config, PU.VectorDescriptor.vectorSize desc)
                val sz1 = vsName descriptor1
                val sz2 = case descriptor2O
                           of SOME descriptor2 => vsName descriptor2
                            | NONE => ""
              in sz1 ^ sz2 ^ name ^ operator
              end
          val res = 
              (case v
                of P.ViPointwise {descriptor, masked, operator} =>
                   let
                     val name = if masked then "PointwiseM" else "Pointwise"
                   in doOne (name, descriptor, NONE, getPrimName (config, operator))
                   end
	         | Mil.Prims.ViConvert {to, from}     => 
                   let
                     val operator = Mil.Prims.PNumConvert {to = #typ to, from = #typ from}
                   in doOne ("Convert", #descriptor to, SOME (#descriptor from), getPrimName (config, operator))
                   end
	        | Mil.Prims.ViCompare {descriptor, typ, operator}     => 
                  let
                    val operator = Mil.Prims.PNumCompare {typ = typ, operator = operator}
                  in doOne ("Compare", descriptor, NONE, getPrimName (config, operator))
                  end
	        | Mil.Prims.ViReduction {descriptor, associativity, operator}   => 
                  let
                    val name = "Reduce" ^ getAssocName (config, associativity)
                  in doOne (name, descriptor, NONE, getPrimName (config, operator))
                  end
	        | Mil.Prims.ViData {descriptor, operator}        => 
                  doOne ("Data", descriptor, NONE, getDataOpName (config, operator, typs))
	        | Mil.Prims.ViMaskData {descriptor, operator}    => 
                  doOne ("MaskData", descriptor, NONE, getDataOpName (config, operator, typs))
	        | Mil.Prims.ViMaskBoolean {descriptor, operator} => 
                  doOne ("MaskBool", descriptor, NONE, getLogicOpName (config, operator))
	        | Mil.Prims.ViMaskConvert {to, from} => 
                  doOne ("MaskConvert", to, SOME from, ""))
        in res
        end

    fun getName (config, p, t, typs) =
        let
          val s =
              case p
               of P.Prim p     => "pLsrPrimP" ^ getPrimName (config, p)
                | P.Runtime rt => "pLsrPrimR" ^ getRuntimeName (config, rt, t)
                | P.Vector p   => "pLsrPrimV" ^ getVectorName (config, p, typs)
        in Pil.identifier s
        end

    fun call (config, ds, p, t, typs, args) = 
      let
        val m = Pil.E.namedConstant (getName (config, p, t, typs))
        val e = 
            case (p, Vector.length ds)
             of (P.Prim _, _)   => Pil.E.call (m, (Vector.toList ds)@args)
              | (P.Vector _, _) => Pil.E.call (m, (Vector.toList ds)@args)
              | (_,        0)   => Pil.E.call (m, args)
              | (_,        1)   => Pil.E.assign (Vector.sub (ds, 0), Pil.E.call (m, args))
              | (_,        _)   => Fail.fail ("Runtime.Prims", "call", "Multiple dests only supported for Prims")
        val s = Pil.S.expr e
      in s
      end

    val vectorLoadStoreHelp : string -> Config.t * Mil.Prims.vectorDescriptor * Mil.fieldKind -> Pil.identifier = 
     fn name => 
     fn (config, vd, fk) => 
        let
          val vs = getVectorSizeName (config, PU.VectorDescriptor.vectorSize vd)
          val fk = getFieldKindName (config, fk)
        in Pil.identifier ("pLsrPrimV" ^ vs ^ fk ^ name)
        end
        
    val vectorLoadF   = vectorLoadStoreHelp "LoadF"
    val vectorLoadVS  = vectorLoadStoreHelp "LoadVS"
    val vectorLoadVI  = vectorLoadStoreHelp "LoadVI"
    val vectorLoadVVS = vectorLoadStoreHelp "LoadVVS"
    val vectorLoadVVI = vectorLoadStoreHelp "LoadVVI"

    val vectorStoreF   = vectorLoadStoreHelp "StoreF"
    val vectorStoreVS  = vectorLoadStoreHelp "StoreVS"
    val vectorStoreVI  = vectorLoadStoreHelp "StoreVI"
    val vectorStoreVVS = vectorLoadStoreHelp "StoreVVS"
    val vectorStoreVVI = vectorLoadStoreHelp "StoreVVI"

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

    fun boxedTyp        fk = Pil.identifier ("PlsrThunkB"             ^ typ fk)
    fun unboxedTyp      fk = Pil.identifier ("PlsrThunkU"             ^ typ fk)
    fun returnTyp       fk = Pil.identifier ("PlsrThunkReturnType"    ^ typ fk)

    fun blackHole       fk = Pil.identifier ("pLsrThunkBlackHole"     ^ typ fk)
    fun call            fk = Pil.identifier ("pLsrThunkCall"          ^ typ fk)
    fun callDirect      fk = Pil.identifier ("pLsrThunkCallDirect"    ^ typ fk)
    fun cut             fk = Pil.identifier ("pLsrThunkCut"           ^ typ fk)
    fun eval            fk = Pil.identifier ("pLsrThunkEval"          ^ typ fk)
    fun evalDirect      fk = Pil.identifier ("pLsrThunkEvalDirect"    ^ typ fk)
    fun fixedSize       fk = Pil.identifier ("pLsrThunkFixedSize"     ^ typ fk)
    fun init            fk = Pil.identifier ("pLsrThunkSetInit"       ^ typ fk)
    fun initValue       fk = Pil.identifier ("pLsrThunkValueInit"     ^ typ fk)
    fun isEvaled        fk = Pil.identifier ("pLsrThunkIsEvaled"      ^ typ fk)
    fun new             fk = Pil.identifier ("pLsrThunkNew"           ^ typ fk)
    fun newValue        fk = Pil.identifier ("pLsrThunkNewValue"      ^ typ fk)
    fun resultOffset    fk = Pil.identifier ("pLsrThunkResultOffset"  ^ typ fk)
    fun return          fk = Pil.identifier ("pLsrThunkReturn"        ^ typ fk)
    fun setValue        fk = Pil.identifier ("pLsrThunkSetValue"      ^ typ fk)
    fun spawn           fk = Pil.identifier ("pLsrThunkSpawn"         ^ typ fk)
    fun staticValue     fk = Pil.identifier ("pLsrThunkStaticValue"   ^ typ fk)
    fun tailCall        fk = Pil.identifier ("pLsrThunkTailCall"      ^ typ fk)
    fun tailCallDirect  fk = Pil.identifier ("pLsrThunkTailCallDirect"^ typ fk)
    fun vTable          fk = Pil.identifier ("pLsrThunkValVTable"     ^ typ fk)
    fun zeroFV          () = Pil.identifier ("pLsrThunkZeroFV"                )
  end

  val exit = Pil.identifier "pLsrExit"
  val halt  = Pil.identifier "pLsrHalt"
  val haltV = Pil.identifier "pLsrHaltV"
  val pmain = Pil.identifier "__pmain"
  val gErrorVal = Pil.identifier "pLsrErrorVal"

end;
