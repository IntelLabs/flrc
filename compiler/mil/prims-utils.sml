(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2010 *)

signature PRIMS_UTILS =
sig

  structure Compare : 
  sig
    type 'a t = 'a Compare.t
    val vectorSize       : Mil.Prims.vectorSize t
    val vectorDescriptor : Mil.Prims.vectorDescriptor t
    val floatPrecision   : Mil.Prims.floatPrecision t
    val intPrecision     : Mil.Prims.intPrecision t
    val numericTyp       : Mil.Prims.numericTyp t
    val divKind          : Mil.Prims.divKind t
    val arithOp          : Mil.Prims.arithOp t
    val floatOp          : Mil.Prims.floatOp t
    val bitwiseOp        : Mil.Prims.bitwiseOp t
    val logicOp          : Mil.Prims.logicOp t
    val compareOp        : Mil.Prims.compareOp t
    val nameOp           : Mil.Prims.nameOp t
    val stringOp         : Mil.Prims.stringOp t
    val prim             : Mil.Prims.prim t
    val assoc            : Mil.Prims.assoc t
    val dataOp           : Mil.Prims.dataOp t
    val vector           : Mil.Prims.vector t
    val runtime          : Mil.Prims.runtime t
    val t                : Mil.Prims.t t
  end (* structure Compare *)

  structure Eq : 
  sig
    type 'a t = 'a * 'a -> bool
    val vectorSize       : Mil.Prims.vectorSize t
    val vectorDescriptor : Mil.Prims.vectorDescriptor t
    val floatPrecision   : Mil.Prims.floatPrecision t
    val intPrecision     : Mil.Prims.intPrecision t
    val numericTyp       : Mil.Prims.numericTyp t
    val divKind          : Mil.Prims.divKind t
    val arithOp          : Mil.Prims.arithOp t
    val floatOp          : Mil.Prims.floatOp t
    val bitwiseOp        : Mil.Prims.bitwiseOp t
    val logicOp          : Mil.Prims.logicOp t
    val compareOp        : Mil.Prims.compareOp t
    val nameOp           : Mil.Prims.nameOp t
    val stringOp         : Mil.Prims.stringOp t
    val prim             : Mil.Prims.prim t
    val assoc            : Mil.Prims.assoc t
    val dataOp           : Mil.Prims.dataOp t
    val vector           : Mil.Prims.vector t
    val runtime          : Mil.Prims.runtime t
    val t                : Mil.Prims.t t
  end (* structure Compare *)

  structure Hash : 
  sig
    type 'a t = 'a -> Word32.word
    val vectorSize       : Mil.Prims.vectorSize t
    val vectorDescriptor : Mil.Prims.vectorDescriptor t
    val floatPrecision   : Mil.Prims.floatPrecision t
    val intPrecision     : Mil.Prims.intPrecision t
    val numericTyp       : Mil.Prims.numericTyp t
    val divKind          : Mil.Prims.divKind t
    val arithOp          : Mil.Prims.arithOp t
    val floatOp          : Mil.Prims.floatOp t
    val bitwiseOp        : Mil.Prims.bitwiseOp t
    val logicOp          : Mil.Prims.logicOp t
    val compareOp        : Mil.Prims.compareOp t
    val nameOp           : Mil.Prims.nameOp t
    val stringOp         : Mil.Prims.stringOp t
    val prim             : Mil.Prims.prim t
    val assoc            : Mil.Prims.assoc t
    val dataOp           : Mil.Prims.dataOp t
    val vector           : Mil.Prims.vector t
    val runtime          : Mil.Prims.runtime t
    val t                : Mil.Prims.t t
  end (* structure Hash *)

  structure Dec :
  sig   
    structure VectorSize : 
    sig
      type t = Mil.Prims.vectorSize
      val vs64   : t -> unit option
      val vs128  : t -> unit option
      val vs256  : t -> unit option
      val vs512  : t -> unit option
      val vs1024 : t -> unit option
    end (* structure VectorSize *)

    structure VectorDescriptor : 
    sig
      type t = Mil.Prims.vectorDescriptor
      val vd : t -> {vectorSize : Mil.Prims.vectorSize, elementSize : Mil.fieldSize} option
    end (* structure VectorDescriptor *)
  
    structure FloatPrecision : 
    sig
      type t = Mil.Prims.floatPrecision
      val fpSingle : t -> unit option
      val fpDouble : t -> unit option
    end (* structure FloatPrecision *)

    structure IntPrecision : 
    sig
      type t = Mil.Prims.intPrecision
      val ipArbitrary : t -> unit option
      val ipFixed     : t -> IntArb.typ option
    end (* structure IntPrecision *)
  
    structure NumericTyp : 
    sig
      type t = Mil.Prims.numericTyp
      val ntRat     : t -> unit option
      val ntInteger : t -> Mil.Prims.intPrecision option
      val ntFloat   : t -> Mil.Prims.floatPrecision option
    end (* structure NumericTyp *)

    structure DivKind : 
    sig
      type t = Mil.Prims.divKind
      val dkT : t -> unit option
      val dkF : t -> unit option
      val dkE : t -> unit option
    end (* structure DivKind *)

    structure ArithOp : 
    sig
      type t = Mil.Prims.arithOp
      val aAbs       : t -> unit option
      val aNegate    : t -> unit option
      val aNegateSat : t -> unit option
      val aDivide    : t -> unit option
      val aDiv       : t -> Mil.Prims.divKind option
      val aMax       : t -> unit option
      val aMin       : t -> unit option
      val aMinus     : t -> unit option
      val aMinusSat  : t -> unit option
      val aMod       : t -> Mil.Prims.divKind option
      val aPlus      : t -> unit option
      val aPlusSat   : t -> unit option
      val aTimes     : t -> unit option
      val aTimesSat  : t -> unit option
      val aDivMod    : t -> Mil.Prims.divKind option
    end (* structure ArithOp *)

    structure FloatOp : 
    sig
      type t = Mil.Prims.floatOp
      val faACos  : t -> unit option
      val faASin  : t -> unit option
      val faCeil  : t -> unit option
      val faCos   : t -> unit option
      val faFloor : t -> unit option
      val faRcp   : t -> unit option
      val faSin   : t -> unit option
      val faSqrt  : t -> unit option
      val faTan   : t -> unit option
      val faTrunc : t -> unit option
      val faPow   : t -> unit option
    end (* structure FloatOp *)

    structure BitwiseOp : 
    sig
      type t = Mil.Prims.bitwiseOp
      val bNot    : t -> unit option
      val bAnd    : t -> unit option
      val bOr     : t -> unit option
      val bRotL   : t -> unit option
      val bRotR   : t -> unit option
      val bShiftL : t -> unit option
      val bShiftR : t -> unit option
      val bXor    : t -> unit option
    end (* structure BitwiseOp *)

    structure LogicOp : 
    sig
      type t = Mil.Prims.logicOp
      val lNot : t -> unit option
      val lAnd : t -> unit option
      val lOr  : t -> unit option
      val lXor : t -> unit option
      val lEq  : t -> unit option
    end (* structure LogicOp *)

    structure CompareOp : 
    sig
      type t = Mil.Prims.compareOp
      val cEq : t -> unit option
      val cNe : t -> unit option
      val cLt : t -> unit option
      val cLe : t -> unit option
    end (* structure CompareOp *)

    structure NameOp :
    sig
      type t = Mil.Prims.nameOp
      val nGetString : t -> unit option
      val nGetHash   : t -> unit option
    end (* structure NameOp *)

    structure StringOp : 
    sig
      type t = Mil.Prims.stringOp
      val sAllocate   : t -> unit option
      val sDeallocate : t -> unit option
      val sGetLen     : t -> unit option
      val sGetChar    : t -> unit option
      val sSetChar    : t -> unit option
      val sEqual      : t -> unit option
    end (* structure StringOp *)

    structure Prim : 
    sig
      type t = Mil.Prims.prim
      val pNumArith   : t -> {typ : Mil.Prims.numericTyp, operator : Mil.Prims.arithOp} option
      val pFloatOp    : t -> {typ : Mil.Prims.floatPrecision, operator : Mil.Prims.floatOp} option
      val pNumCompare : t -> {typ : Mil.Prims.numericTyp, operator : Mil.Prims.compareOp} option
      val pNumConvert : t -> {to : Mil.Prims.numericTyp, from : Mil.Prims.numericTyp} option
      val pBitwise    : t -> {typ : Mil.Prims.intPrecision, operator : Mil.Prims.bitwiseOp} option
      val pBoolean    : t -> Mil.Prims.logicOp option
      val pName       : t -> Mil.Prims.nameOp option
      val pCString    : t -> Mil.Prims.stringOp option
      val pPtrEq      : t -> unit option
    end (* structure Prim *)

    structure Assoc : 
    sig
      type t = Mil.Prims.assoc
      val aLeft  : t -> unit option
      val aRight : t -> unit option
      val aAny   : t -> unit option
    end (* structure Assoc *)

    structure DataOp : 
    sig
      type t = Mil.Prims.dataOp
      val dBroadcast : t -> unit option
      val dVector    : t -> unit option
      val dSub       : t -> int option
      val dPermute   : t -> int Vector.t option
      val dBlend     : t -> unit option
      val dSplit     : t -> unit option
      val dConcat    : t -> unit option
    end (* structure DataOp *)

    structure Vector : 
    sig
      type t = Mil.Prims.vector
      val viPointwise   : t -> {descriptor : Mil.Prims.vectorDescriptor, masked: bool, operator : Mil.Prims.prim} option
      val viConvert     : t
                          ->
                          {to :   {descriptor : Mil.Prims.vectorDescriptor, typ : Mil.Prims.numericTyp}, 
                           from : {descriptor : Mil.Prims.vectorDescriptor, typ : Mil.Prims.numericTyp}} option
      val viCompare     : t -> {descriptor : Mil.Prims.vectorDescriptor, 
                                typ : Mil.Prims.numericTyp, 
                                operator : Mil.Prims.compareOp} option
      val viReduction   : t -> {descriptor : Mil.Prims.vectorDescriptor, 
                                associativity : Mil.Prims.assoc, 
                                operator : Mil.Prims.prim} option
      val viData        : t -> {descriptor : Mil.Prims.vectorDescriptor, operator : Mil.Prims.dataOp} option
      val viMaskData    : t -> {descriptor : Mil.Prims.vectorDescriptor, operator : Mil.Prims.dataOp} option
      val viMaskBoolean : t -> {descriptor : Mil.Prims.vectorDescriptor, operator : Mil.Prims.logicOp} option
      val viMaskConvert : t -> {to : Mil.Prims.vectorDescriptor, from : Mil.Prims.vectorDescriptor} option
    end (* structure Vector *)

    structure Runtime : 
    sig
      type t = Mil.Prims.runtime
      val rtFloatMk           : t -> unit option
      val rtWriteln           : t -> unit option
      val rtReadln            : t -> unit option
      val rtAssert            : t -> unit option
      val rtError             : t -> unit option
      val rtDebug             : t -> unit option
      val rtOpenOut           : t -> unit option
      val rtGetStdout         : t -> unit option
      val rtOutputByte        : t -> unit option
      val rtCloseOut          : t -> unit option
      val rtOpenIn            : t -> unit option
      val rtGetStdin          : t -> unit option
      val rtInputByte         : t -> unit option
      val rtInputString       : t -> unit option
      val rtInputAll          : t -> unit option
      val rtIsEOF             : t -> unit option
      val rtCloseIn           : t -> unit option
      val rtCommandLine       : t -> unit option
      val rtStringToNat       : t -> unit option
      val rtStringToFloat     : t -> unit option
      val rtFloatToString     : t -> unit option
      val rtFloatToStringI    : t -> unit option
      val rtRatNumerator      : t -> unit option
      val rtRatDenominator    : t -> unit option
      val rtEqual             : t -> unit option
      val rtDom               : t -> unit option
      val rtNub               : t -> unit option
      val rtRatToUIntpChecked : t -> unit option
      val rtRatToString       : t -> unit option
      val rtStringToRat       : t -> unit option
      val rtResetTimer        : t -> unit option
      val rtGetTimer          : t -> unit option
      val rtVtuneAttach       : t -> unit option
      val rtVtuneDetach       : t -> unit option
      val rtArrayEval         : t -> unit option
      val rtIntegerHash       : t -> unit option
    end (* structure Runtime *)

    structure T : 
    sig
      type t = Mil.Prims.t
      val prim    : t -> Mil.Prims.prim option
      val runtime : t -> Mil.Prims.runtime option
      val vector  : t -> Mil.Prims.vector option
    end (* structure T *)
  end (* structure Dec *)


  structure Effects : 
  sig
    type 'a t = 'a -> Effect.set
    val prim             : Mil.Prims.prim t
    val vector           : Mil.Prims.vector t
    val runtime          : Mil.Prims.runtime t
    val t                : Mil.Prims.t t
  end (* structure Effects *)

  structure Arity : 
  sig
    datatype arity = ArAtoA | ArAAtoA | ArAAtoB | ArOther of Int.t * Int.t
    type 'a t = 'a -> arity

    val count : arity -> Int.t * Int.t

    val arithOp          : Mil.Prims.arithOp t
    val floatOp          : Mil.Prims.floatOp t
    val bitwiseOp        : Mil.Prims.bitwiseOp t
    val logicOp          : Mil.Prims.logicOp t
    val compareOp        : Mil.Prims.compareOp t
    val nameOp           : Mil.Prims.nameOp t
    val stringOp         : Mil.Prims.stringOp t
    val prim             : Mil.Prims.prim t
    val dataOp           : Mil.Prims.vectorDescriptor -> Mil.Prims.dataOp t
    val vector           : Mil.Prims.vector t
    val runtime          : Mil.Prims.runtime t
    val t                : Mil.Prims.t t
  end (* structure Arity *)

  structure Layout : 
  sig
    type 'a t = Config.t * 'a -> Layout.t
    val vectorSize       : Mil.Prims.vectorSize t
    val vectorDescriptor : Mil.Prims.vectorDescriptor t
    val floatPrecision   : Mil.Prims.floatPrecision t
    val intPrecision     : Mil.Prims.intPrecision t
    val numericTyp       : Mil.Prims.numericTyp t
    val divKind          : Mil.Prims.divKind t
    val arithOp          : Mil.Prims.arithOp t
    val floatOp          : Mil.Prims.floatOp t
    val bitwiseOp        : Mil.Prims.bitwiseOp t
    val logicOp          : Mil.Prims.logicOp t
    val compareOp        : Mil.Prims.compareOp t
    val stringOp         : Mil.Prims.stringOp t
    val prim             : Mil.Prims.prim t
    val assoc            : Mil.Prims.assoc t
    val dataOp           : Mil.Prims.dataOp t
    val vector           : Mil.Prims.vector t
    val runtime          : Mil.Prims.runtime t
    val t                : Mil.Prims.t t
  end (* structure Layout *)

  structure Parse :
  sig
    type 'a t = 'a FileParser.t
    val vectorSize       : Config.t -> Mil.Prims.vectorSize t
    val vectorDescriptor : Config.t -> Mil.Prims.vectorDescriptor t
    val floatPrecision   : Config.t -> Mil.Prims.floatPrecision t
    val intPrecision     : Config.t -> Mil.Prims.intPrecision t
    val numericTyp       : Config.t -> Mil.Prims.numericTyp t
    val divKind          : Config.t -> Mil.Prims.divKind t
    val arithOp          : Config.t -> Mil.Prims.arithOp t
    val floatOp          : Config.t -> Mil.Prims.floatOp t
    val bitwiseOp        : Config.t -> Mil.Prims.bitwiseOp t
    val logicOp          : Config.t -> Mil.Prims.logicOp t
    val compareOp        : Config.t -> Mil.Prims.compareOp t
    val stringOp         : Config.t -> Mil.Prims.stringOp t
    val prim             : Config.t -> Mil.Prims.prim t
    val assoc            : Config.t -> Mil.Prims.assoc t
    val dataOp           : Config.t -> Mil.Prims.dataOp t
    val vector           : Config.t -> Mil.Prims.vector t
    val runtime          : Config.t -> Mil.Prims.runtime t
    val t                : Config.t -> Mil.Prims.t t
  end (* signature Parse *)

  structure ToString : 
  sig
    type 'a t = Config.t * 'a -> string
    val vectorSize       : Mil.Prims.vectorSize t
    val vectorDescriptor : Mil.Prims.vectorDescriptor t
    val floatPrecision   : Mil.Prims.floatPrecision t
    val intPrecision     : Mil.Prims.intPrecision t
    val numericTyp       : Mil.Prims.numericTyp t
    val divKind          : Mil.Prims.divKind t
    val arithOp          : Mil.Prims.arithOp t
    val floatOp          : Mil.Prims.floatOp t
    val bitwiseOp        : Mil.Prims.bitwiseOp t
    val logicOp          : Mil.Prims.logicOp t
    val compareOp        : Mil.Prims.compareOp t
    val nameOp           : Mil.Prims.nameOp t
    val stringOp         : Mil.Prims.stringOp t
    val prim             : Mil.Prims.prim t
    val assoc            : Mil.Prims.assoc t
    val dataOp           : Mil.Prims.dataOp t
    val vector           : Mil.Prims.vector t
    val runtime          : Mil.Prims.runtime t
    val t                : Mil.Prims.t t
  end (* structure ToString *)

  structure VectorSize : 
  sig
    type t = Mil.Prims.vectorSize
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    val toString : Config.t * t -> string
    val numBits : t -> int
    val fromBits : int -> t option
    val toValueSize : t -> Mil.valueSize
    val halfSize : t -> t option
    val doubleSize : t -> t option
    val platformSize : Config.t -> t
    structure Dec : 
    sig
      val vs64   : t -> unit option
      val vs128  : t -> unit option
      val vs256  : t -> unit option
      val vs512  : t -> unit option
      val vs1024 : t -> unit option
    end (* structure Dec *)
  end (* structure VectorSize *)

  structure VectorDescriptor : 
  sig
    type t = Mil.Prims.vectorDescriptor
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    val toString : Config.t * t -> string
    val numBits : t -> int
    val elementCount : t -> int
    val vectorSize : t -> VectorSize.t
    val elementSize : t -> Mil.fieldSize
    structure Dec : 
    sig
      val vd : t -> {vectorSize : Mil.Prims.vectorSize, elementSize : Mil.fieldSize} option
    end (* structure Dec *)
  end (* structure VectorDescriptor *)

  structure FloatPrecision : 
  sig
    type t = Mil.Prims.floatPrecision
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    structure Dec : 
    sig
      val fpSingle : t -> unit option
      val fpDouble : t -> unit option
    end (* structure Dec *)
  end (* structure FloatPrecision *)

  structure IntPrecision : 
  sig
    type t = Mil.Prims.intPrecision
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    structure Dec : 
    sig
      val ipArbitrary : t -> unit option
      val ipFixed     : t -> IntArb.typ option
    end (* structure Dec *)
  end (* structure IntPrecision *)

  structure NumericTyp : 
  sig
    type t = Mil.Prims.numericTyp
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    structure Dec : 
    sig
      val ntRat     : t -> unit option
      val ntInteger : t -> Mil.Prims.intPrecision option
      val ntFloat   : t -> Mil.Prims.floatPrecision option
    end (* structure Dec *)
  end (* structure NumericTyp *)

  structure DivKind : 
  sig
    type t = Mil.Prims.divKind
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    structure Dec : 
    sig
      val dkT : t -> unit option
      val dkF : t -> unit option
      val dkE : t -> unit option
    end (* structure Dec *)
  end (* structure DivKind *)

  structure ArithOp : 
  sig
    type t = Mil.Prims.arithOp
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    structure Dec : 
    sig
      val aAbs       : t -> unit option
      val aNegate    : t -> unit option
      val aNegateSat : t -> unit option
      val aDivide    : t -> unit option
      val aDiv       : t -> Mil.Prims.divKind option
      val aMax       : t -> unit option
      val aMin       : t -> unit option
      val aMinus     : t -> unit option
      val aMinusSat  : t -> unit option
      val aMod       : t -> Mil.Prims.divKind option
      val aPlus      : t -> unit option
      val aPlusSat   : t -> unit option
      val aTimes     : t -> unit option
      val aTimesSat  : t -> unit option
      val aDivMod    : t -> Mil.Prims.divKind option
    end (* structure Dec *)
  end (* structure ArithOp *)

  structure FloatOp : 
  sig
    type t = Mil.Prims.floatOp
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    structure Dec : 
    sig
      val faACos  : t -> unit option
      val faASin  : t -> unit option
      val faCeil  : t -> unit option
      val faCos   : t -> unit option
      val faFloor : t -> unit option
      val faRcp   : t -> unit option
      val faSin   : t -> unit option
      val faSqrt  : t -> unit option
      val faTan   : t -> unit option
      val faTrunc : t -> unit option
      val faPow   : t -> unit option
    end (* structure Dec *)
  end (* structure FloatOp *)

  structure BitwiseOp : 
  sig
    type t = Mil.Prims.bitwiseOp
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    structure Dec : 
    sig
      val bNot    : t -> unit option
      val bAnd    : t -> unit option
      val bOr     : t -> unit option
      val bRotL   : t -> unit option
      val bRotR   : t -> unit option
      val bShiftL : t -> unit option
      val bShiftR : t -> unit option
      val bXor    : t -> unit option
    end (* structure Dec *)
  end (* structure BitwiseOp *)

  structure LogicOp : 
  sig
    type t = Mil.Prims.logicOp
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    structure Dec : 
    sig
      val lNot : t -> unit option
      val lAnd : t -> unit option
      val lOr  : t -> unit option
      val lXor : t -> unit option
      val lEq  : t -> unit option
    end (* structure Dec *)
  end (* structure LogicOp *)

  structure CompareOp : 
  sig
    type t = Mil.Prims.compareOp
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    structure Dec : 
    sig
      val cEq : t -> unit option
      val cNe : t -> unit option
      val cLt : t -> unit option
      val cLe : t -> unit option
    end (* structure Dec *)
  end (* structure CompareOp *)

  structure NameOp :
  sig
    type t = Mil.Prims.nameOp
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    structure Dec :
    sig
      val nGetString : t -> unit option
      val nGetHash   : t -> unit option
    end (* structure Dec *)
  end (* structure NameOp *)

  structure StringOp : 
  sig
    type t = Mil.Prims.stringOp
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    structure Dec : 
    sig
      val sAllocate   : t -> unit option
      val sDeallocate : t -> unit option
      val sGetLen     : t -> unit option
      val sGetChar    : t -> unit option
      val sSetChar    : t -> unit option
      val sEqual      : t -> unit option
    end (* structure Dec *)
  end (* structure StringOp *)

  structure Prim : 
  sig
    type t = Mil.Prims.prim
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    structure Dec : 
    sig
      val pNumArith   : t -> {typ : Mil.Prims.numericTyp, operator : Mil.Prims.arithOp} option
      val pFloatOp    : t -> {typ : Mil.Prims.floatPrecision, operator : Mil.Prims.floatOp} option
      val pNumCompare : t -> {typ : Mil.Prims.numericTyp, operator : Mil.Prims.compareOp} option
      val pNumConvert : t -> {to : Mil.Prims.numericTyp, from : Mil.Prims.numericTyp} option
      val pBitwise    : t -> {typ : Mil.Prims.intPrecision, operator : Mil.Prims.bitwiseOp} option
      val pBoolean    : t -> Mil.Prims.logicOp option
      val pName       : t -> Mil.Prims.nameOp option
      val pCString    : t -> Mil.Prims.stringOp option
      val pPtrEq      : t -> unit option
    end (* structure Dec *)
  end (* structure Prim *)

  structure Assoc : 
  sig
    type t = Mil.Prims.assoc
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    structure Dec : 
    sig
      val aLeft  : t -> unit option
      val aRight : t -> unit option
      val aAny   : t -> unit option
    end (* structure Dec *)
  end (* structure Assoc *)

  structure DataOp : 
  sig
    type t = Mil.Prims.dataOp
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    structure Dec : 
    sig
      val dBroadcast : t -> unit option
      val dVector    : t -> unit option
      val dSub       : t -> int option
      val dPermute   : t -> int Vector.t option
      val dBlend     : t -> unit option
      val dSplit     : t -> unit option
      val dConcat    : t -> unit option
    end (* structure Dec *)
  end (* structure DataOp *)

  structure Vector : 
  sig
    type t = Mil.Prims.vector
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    structure Dec : 
    sig
      val viPointwise   : t -> {descriptor : Mil.Prims.vectorDescriptor, masked: bool, operator : Mil.Prims.prim} option
      val viConvert     : t
                          ->
                          {to :   {descriptor : Mil.Prims.vectorDescriptor, typ : Mil.Prims.numericTyp}, 
                           from : {descriptor : Mil.Prims.vectorDescriptor, typ : Mil.Prims.numericTyp}} option
      val viCompare     : t -> {descriptor : Mil.Prims.vectorDescriptor, 
                                typ : Mil.Prims.numericTyp, 
                                operator : Mil.Prims.compareOp} option
      val viReduction   : t -> {descriptor : Mil.Prims.vectorDescriptor, 
                                associativity : Mil.Prims.assoc, 
                                operator : Mil.Prims.prim} option
      val viData        : t -> {descriptor : Mil.Prims.vectorDescriptor, operator : Mil.Prims.dataOp} option
      val viMaskData    : t -> {descriptor : Mil.Prims.vectorDescriptor, operator : Mil.Prims.dataOp} option
      val viMaskBoolean : t -> {descriptor : Mil.Prims.vectorDescriptor, operator : Mil.Prims.logicOp} option
      val viMaskConvert : t -> {to : Mil.Prims.vectorDescriptor, from : Mil.Prims.vectorDescriptor} option
    end (* structure Dec *)
  end (* structure Vector *)

  structure Runtime : 
  sig
    type t = Mil.Prims.runtime
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    structure Dec : 
    sig
      val rtFloatMk           : t -> unit option
      val rtWriteln           : t -> unit option
      val rtReadln            : t -> unit option
      val rtAssert            : t -> unit option
      val rtError             : t -> unit option
      val rtDebug             : t -> unit option
      val rtOpenOut           : t -> unit option
      val rtGetStdout         : t -> unit option
      val rtOutputByte        : t -> unit option
      val rtCloseOut          : t -> unit option
      val rtOpenIn            : t -> unit option
      val rtGetStdin          : t -> unit option
      val rtInputByte         : t -> unit option
      val rtInputString       : t -> unit option
      val rtInputAll          : t -> unit option
      val rtIsEOF             : t -> unit option
      val rtCloseIn           : t -> unit option
      val rtCommandLine       : t -> unit option
      val rtStringToNat       : t -> unit option
      val rtStringToFloat     : t -> unit option
      val rtFloatToString     : t -> unit option
      val rtFloatToStringI    : t -> unit option
      val rtRatNumerator      : t -> unit option
      val rtRatDenominator    : t -> unit option
      val rtEqual             : t -> unit option
      val rtDom               : t -> unit option
      val rtNub               : t -> unit option
      val rtRatToUIntpChecked : t -> unit option
      val rtRatToString       : t -> unit option
      val rtStringToRat       : t -> unit option
      val rtResetTimer        : t -> unit option
      val rtGetTimer          : t -> unit option
      val rtVtuneAttach       : t -> unit option
      val rtVtuneDetach       : t -> unit option
      val rtArrayEval         : t -> unit option
      val rtIntegerHash       : t -> unit option
    end (* structure Dec *)
  end (* structure Runtime *)

  structure T : 
  sig
    type t = Mil.Prims.t
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    structure Dec : 
    sig
      val prim    : t -> Mil.Prims.prim option
      val runtime : t -> Mil.Prims.runtime option
      val vector  : t -> Mil.Prims.vector option
    end (* structure Dec *)
  end (* structure T *)

end (* signature PRIMS_UTILS *)

functor PrimsUtilsF (structure FieldSize :
                      sig 
                        type t = Mil.Prims.fieldSize
                        val ord : t -> int
                        val compare : t Compare.t
                        val toString : t -> string
                        val numBits : t -> int
                        val intArbSz : IntArb.size -> t
                        structure Dec :
                        sig
                          val fs8  : t -> unit option
                          val fs16 : t -> unit option
                          val fs32 : t -> unit option
                          val fs64 : t -> unit option
                        end (* structure Dec *)
                      end) :> PRIMS_UTILS =
struct

  structure Prims = Mil.Prims

  structure IFO = IntFiniteOrdinal

  structure Dec = 
  struct
    structure VectorSize = 
    struct
      type t = Mil.Prims.vectorSize
      val vs64   = fn ve => (case ve of Mil.Prims.Vs64 => SOME () | _ => NONE)
      val vs128  = fn ve => (case ve of Mil.Prims.Vs128 => SOME () | _ => NONE)
      val vs256  = fn ve => (case ve of Mil.Prims.Vs256 => SOME () | _ => NONE)
      val vs512  = fn ve => (case ve of Mil.Prims.Vs512 => SOME () | _ => NONE)
      val vs1024 = fn ve => (case ve of Mil.Prims.Vs1024 => SOME () | _ => NONE)
    end (* structure VectorSize *)

    structure VectorDescriptor = 
    struct
      type t = Mil.Prims.vectorDescriptor
      val vd = fn ve => (case ve of Mil.Prims.Vd r => SOME r)
    end (* structure VectorDescriptor *)

    structure FloatPrecision = 
    struct
      type t = Mil.Prims.floatPrecision
      val fpSingle = fn fl => (case fl of Mil.Prims.FpSingle => SOME () | _ => NONE)
      val fpDouble = fn fl => (case fl of Mil.Prims.FpDouble => SOME () | _ => NONE)
    end (* structure FloatPrecision *)

    structure IntPrecision = 
    struct
      type t = Mil.Prims.intPrecision
      val ipArbitrary = fn ip => (case ip of Mil.Prims.IpArbitrary => SOME () | _ => NONE)
      val ipFixed     = fn ip => (case ip of Mil.Prims.IpFixed r => SOME r | _ => NONE)
    end (* structure IntPrecision *)

    structure NumericTyp = 
    struct
      type t = Mil.Prims.numericTyp
      val ntRat     = fn nu => (case nu of Mil.Prims.NtRat => SOME () | _ => NONE)
      val ntInteger = fn nu => (case nu of Mil.Prims.NtInteger r => SOME r | _ => NONE)
      val ntFloat   = fn nu => (case nu of Mil.Prims.NtFloat r => SOME r | _ => NONE)
    end (* structure NumericTyp *)

    structure DivKind = 
    struct
      type t = Mil.Prims.divKind
      val dkT = fn di => (case di of Mil.Prims.DkT => SOME () | _ => NONE)
      val dkF = fn di => (case di of Mil.Prims.DkF => SOME () | _ => NONE)
      val dkE = fn di => (case di of Mil.Prims.DkE => SOME () | _ => NONE)
    end (* structure DivKind *)

    structure ArithOp = 
    struct
      type t = Mil.Prims.arithOp
      val aAbs       = fn ar => (case ar of Mil.Prims.AAbs => SOME () | _ => NONE)
      val aNegate    = fn ar => (case ar of Mil.Prims.ANegate => SOME () | _ => NONE)
      val aNegateSat = fn ar => (case ar of Mil.Prims.ANegateSat => SOME () | _ => NONE)
      val aDivide    = fn ar => (case ar of Mil.Prims.ADivide => SOME () | _ => NONE)
      val aDiv       = fn ar => (case ar of Mil.Prims.ADiv r => SOME r | _ => NONE)
      val aMax       = fn ar => (case ar of Mil.Prims.AMax => SOME () | _ => NONE)
      val aMin       = fn ar => (case ar of Mil.Prims.AMin => SOME () | _ => NONE)
      val aMinus     = fn ar => (case ar of Mil.Prims.AMinus => SOME () | _ => NONE)
      val aMinusSat  = fn ar => (case ar of Mil.Prims.AMinusSat => SOME () | _ => NONE)
      val aMod       = fn ar => (case ar of Mil.Prims.AMod r => SOME r | _ => NONE)
      val aPlus      = fn ar => (case ar of Mil.Prims.APlus => SOME () | _ => NONE)
      val aPlusSat   = fn ar => (case ar of Mil.Prims.APlusSat => SOME () | _ => NONE)
      val aTimes     = fn ar => (case ar of Mil.Prims.ATimes => SOME () | _ => NONE)
      val aTimesSat  = fn ar => (case ar of Mil.Prims.ATimesSat => SOME () | _ => NONE)
      val aDivMod    = fn ar => (case ar of Mil.Prims.ADivMod r => SOME r | _ => NONE)
    end (* structure ArithOp *)

    structure FloatOp = 
    struct
      type t = Mil.Prims.floatOp
      val faASin  = fn fl => (case fl of Mil.Prims.FaACos => SOME () | _ => NONE)
      val faACos  = fn fl => (case fl of Mil.Prims.FaASin => SOME () | _ => NONE)
      val faCeil  = fn fl => (case fl of Mil.Prims.FaCeil => SOME () | _ => NONE)
      val faCos   = fn fl => (case fl of Mil.Prims.FaCos => SOME () | _ => NONE)
      val faFloor = fn fl => (case fl of Mil.Prims.FaFloor => SOME () | _ => NONE)
      val faRcp   = fn fl => (case fl of Mil.Prims.FaRcp => SOME () | _ => NONE)
      val faSin   = fn fl => (case fl of Mil.Prims.FaSin => SOME () | _ => NONE)
      val faSqrt  = fn fl => (case fl of Mil.Prims.FaSqrt => SOME () | _ => NONE)
      val faTan   = fn fl => (case fl of Mil.Prims.FaTan => SOME () | _ => NONE)
      val faTrunc = fn fl => (case fl of Mil.Prims.FaTrunc => SOME () | _ => NONE)
      val faPow   = fn fl => (case fl of Mil.Prims.FaPow => SOME () | _ => NONE)
    end (* structure FloatOp *)

    structure BitwiseOp = 
    struct
      type t = Mil.Prims.bitwiseOp
      val bNot    = fn bi => (case bi of Mil.Prims.BNot => SOME () | _ => NONE)
      val bAnd    = fn bi => (case bi of Mil.Prims.BAnd => SOME () | _ => NONE)
      val bOr     = fn bi => (case bi of Mil.Prims.BOr => SOME () | _ => NONE)
      val bRotL   = fn bi => (case bi of Mil.Prims.BRotL => SOME () | _ => NONE)
      val bRotR   = fn bi => (case bi of Mil.Prims.BRotR => SOME () | _ => NONE)
      val bShiftL = fn bi => (case bi of Mil.Prims.BShiftL => SOME () | _ => NONE)
      val bShiftR = fn bi => (case bi of Mil.Prims.BShiftR => SOME () | _ => NONE)
      val bXor    = fn bi => (case bi of Mil.Prims.BXor => SOME () | _ => NONE)
    end (* structure BitwiseOp *)

    structure LogicOp = 
    struct
      type t = Mil.Prims.logicOp
      val lNot = fn lo => (case lo of Mil.Prims.LNot => SOME () | _ => NONE)
      val lAnd = fn lo => (case lo of Mil.Prims.LAnd => SOME () | _ => NONE)
      val lOr  = fn lo => (case lo of Mil.Prims.LOr => SOME () | _ => NONE)
      val lXor = fn lo => (case lo of Mil.Prims.LXor => SOME () | _ => NONE)
      val lEq  = fn lo => (case lo of Mil.Prims.LEq => SOME () | _ => NONE)
    end (* structure LogicOp *)

    structure CompareOp = 
    struct
      type t = Mil.Prims.compareOp
      val cEq = fn co => (case co of Mil.Prims.CEq => SOME () | _ => NONE)
      val cNe = fn co => (case co of Mil.Prims.CNe => SOME () | _ => NONE)
      val cLt = fn co => (case co of Mil.Prims.CLt => SOME () | _ => NONE)
      val cLe = fn co => (case co of Mil.Prims.CLe => SOME () | _ => NONE)
    end (* structure CompareOp *)

    structure NameOp =
    struct
      type t = Mil.Prims.nameOp
      val nGetString = fn st => (case st of Mil.Prims.NGetString => SOME () | _ => NONE)
      val nGetHash   = fn st => (case st of Mil.Prims.NGetHash   => SOME () | _ => NONE)
    end (* structure NameOp *)

    structure StringOp = 
    struct
      type t = Mil.Prims.stringOp
      val sAllocate   = fn st => (case st of Mil.Prims.SAllocate => SOME () | _ => NONE)
      val sDeallocate = fn st => (case st of Mil.Prims.SDeallocate => SOME () | _ => NONE)
      val sGetLen     = fn st => (case st of Mil.Prims.SGetLen => SOME () | _ => NONE)
      val sGetChar    = fn st => (case st of Mil.Prims.SGetChar => SOME () | _ => NONE)
      val sSetChar    = fn st => (case st of Mil.Prims.SSetChar => SOME () | _ => NONE)
      val sEqual      = fn st => (case st of Mil.Prims.SEqual => SOME () | _ => NONE)
    end (* structure StringOp *)

    structure Prim = 
    struct
      type t = Mil.Prims.prim
      val pNumArith   = fn pr => (case pr of Mil.Prims.PNumArith r => SOME r | _ => NONE)
      val pFloatOp    = fn pr => (case pr of Mil.Prims.PFloatOp r => SOME r | _ => NONE)
      val pNumCompare = fn pr => (case pr of Mil.Prims.PNumCompare r => SOME r | _ => NONE)
      val pNumConvert = fn pr => (case pr of Mil.Prims.PNumConvert r => SOME r | _ => NONE)
      val pBitwise    = fn pr => (case pr of Mil.Prims.PBitwise r => SOME r | _ => NONE)
      val pBoolean    = fn pr => (case pr of Mil.Prims.PBoolean r => SOME r | _ => NONE)
      val pName       = fn pr => (case pr of Mil.Prims.PName r => SOME r | _ => NONE)
      val pCString    = fn pr => (case pr of Mil.Prims.PCString r => SOME r | _ => NONE)
      val pPtrEq      = fn pr => (case pr of Mil.Prims.PPtrEq => SOME () | _ => NONE)
    end (* structure Prim *)

    structure Assoc = 
    struct
      type t = Mil.Prims.assoc
      val aLeft  = fn a => (case a of Mil.Prims.ALeft => SOME () | _ => NONE)
      val aRight = fn a => (case a of Mil.Prims.ARight => SOME () | _ => NONE)
      val aAny   = fn a => (case a of Mil.Prims.AAny => SOME () | _ => NONE)
    end (* structure Assoc *)

    structure DataOp = 
    struct
      type t = Mil.Prims.dataOp
      val dBroadcast = fn da => (case da of Mil.Prims.DBroadcast => SOME () | _ => NONE)
      val dVector    = fn da => (case da of Mil.Prims.DVector => SOME () | _ => NONE)
      val dSub       = fn da => (case da of Mil.Prims.DSub r => SOME r | _ => NONE)
      val dPermute   = fn da => (case da of Mil.Prims.DPermute r => SOME r | _ => NONE)
      val dBlend     = fn da => (case da of Mil.Prims.DBlend => SOME () | _ => NONE)
      val dSplit     = fn da => (case da of Mil.Prims.DSplit => SOME () | _ => NONE)
      val dConcat    = fn da => (case da of Mil.Prims.DConcat => SOME () | _ => NONE)
    end (* structure DataOp *)

    structure Vector = 
    struct
      type t = Mil.Prims.vector
      val viPointwise   = fn ve => (case ve of Mil.Prims.ViPointwise r => SOME r | _ => NONE)
      val viConvert     = fn ve => (case ve of Mil.Prims.ViConvert r => SOME r | _ => NONE)
      val viCompare     = fn ve => (case ve of Mil.Prims.ViCompare r => SOME r | _ => NONE)
      val viReduction   = fn ve => (case ve of Mil.Prims.ViReduction r => SOME r | _ => NONE)
      val viData        = fn ve => (case ve of Mil.Prims.ViData r => SOME r | _ => NONE)
      val viMaskData    = fn ve => (case ve of Mil.Prims.ViMaskData r => SOME r | _ => NONE)
      val viMaskBoolean = fn ve => (case ve of Mil.Prims.ViMaskBoolean r => SOME r | _ => NONE)
      val viMaskConvert = fn ve => (case ve of Mil.Prims.ViMaskConvert r => SOME r | _ => NONE)
    end (* structure Vector *)

    structure Runtime = 
    struct
      type t = Mil.Prims.runtime
      val rtFloatMk           = fn ru => (case ru of Mil.Prims.RtFloatMk => SOME () | _ => NONE)
      val rtWriteln           = fn ru => (case ru of Mil.Prims.RtWriteln => SOME () | _ => NONE)
      val rtReadln            = fn ru => (case ru of Mil.Prims.RtReadln => SOME () | _ => NONE)
      val rtAssert            = fn ru => (case ru of Mil.Prims.RtAssert => SOME () | _ => NONE)
      val rtError             = fn ru => (case ru of Mil.Prims.RtError => SOME () | _ => NONE)
      val rtDebug             = fn ru => (case ru of Mil.Prims.RtDebug => SOME () | _ => NONE)
      val rtOpenOut           = fn ru => (case ru of Mil.Prims.RtOpenOut => SOME () | _ => NONE)
      val rtGetStdout         = fn ru => (case ru of Mil.Prims.RtGetStdout => SOME () | _ => NONE)
      val rtOutputByte        = fn ru => (case ru of Mil.Prims.RtOutputByte => SOME () | _ => NONE)
      val rtCloseOut          = fn ru => (case ru of Mil.Prims.RtCloseOut => SOME () | _ => NONE)
      val rtOpenIn            = fn ru => (case ru of Mil.Prims.RtOpenIn => SOME () | _ => NONE)
      val rtGetStdin          = fn ru => (case ru of Mil.Prims.RtGetStdin => SOME () | _ => NONE)
      val rtInputByte         = fn ru => (case ru of Mil.Prims.RtInputByte => SOME () | _ => NONE)
      val rtInputString       = fn ru => (case ru of Mil.Prims.RtInputString => SOME () | _ => NONE)
      val rtInputAll          = fn ru => (case ru of Mil.Prims.RtInputAll => SOME () | _ => NONE)
      val rtIsEOF             = fn ru => (case ru of Mil.Prims.RtIsEOF => SOME () | _ => NONE)
      val rtCloseIn           = fn ru => (case ru of Mil.Prims.RtCloseIn => SOME () | _ => NONE)
      val rtCommandLine       = fn ru => (case ru of Mil.Prims.RtCommandLine => SOME () | _ => NONE)
      val rtStringToNat       = fn ru => (case ru of Mil.Prims.RtStringToNat => SOME () | _ => NONE)
      val rtStringToFloat     = fn ru => (case ru of Mil.Prims.RtStringToFloat => SOME () | _ => NONE)
      val rtFloatToString     = fn ru => (case ru of Mil.Prims.RtFloatToString => SOME () | _ => NONE)
      val rtFloatToStringI    = fn ru => (case ru of Mil.Prims.RtFloatToStringI => SOME () | _ => NONE)
      val rtRatNumerator      = fn ru => (case ru of Mil.Prims.RtRatNumerator => SOME () | _ => NONE)
      val rtRatDenominator    = fn ru => (case ru of Mil.Prims.RtRatDenominator => SOME () | _ => NONE)
      val rtEqual             = fn ru => (case ru of Mil.Prims.RtEqual => SOME () | _ => NONE)
      val rtDom               = fn ru => (case ru of Mil.Prims.RtDom => SOME () | _ => NONE)
      val rtNub               = fn ru => (case ru of Mil.Prims.RtNub => SOME () | _ => NONE)
      val rtRatToUIntpChecked = fn ru => (case ru of Mil.Prims.RtRatToUIntpChecked => SOME () | _ => NONE)
      val rtRatToString       = fn ru => (case ru of Mil.Prims.RtRatToString => SOME () | _ => NONE)
      val rtStringToRat       = fn ru => (case ru of Mil.Prims.RtStringToRat => SOME () | _ => NONE)
      val rtResetTimer        = fn ru => (case ru of Mil.Prims.RtResetTimer => SOME () | _ => NONE)
      val rtGetTimer          = fn ru => (case ru of Mil.Prims.RtGetTimer => SOME () | _ => NONE)
      val rtVtuneAttach       = fn ru => (case ru of Mil.Prims.RtVtuneAttach => SOME () | _ => NONE)
      val rtVtuneDetach       = fn ru => (case ru of Mil.Prims.RtVtuneDetach => SOME () | _ => NONE)
      val rtArrayEval         = fn ru => (case ru of Mil.Prims.RtArrayEval => SOME () | _ => NONE)
      val rtIntegerHash       = fn ru => (case ru of Mil.Prims.RtIntegerHash => SOME () | _ => NONE)
    end (* structure Runtime *)

    structure T = 
    struct
      type t = Mil.Prims.t
      val prim    = fn t => (case t of Mil.Prims.Prim r => SOME r | _ => NONE)
      val runtime = fn t => (case t of Mil.Prims.Runtime r => SOME r | _ => NONE)
      val vector  = fn t => (case t of Mil.Prims.Vector r => SOME r | _ => NONE)
    end (* structure T *)
  end (* structure Dec *) 

  structure ParserUnParser =
  struct

    structure L = Layout
    structure Parser = FileParser

    structure PUP = FileParserUnParser

    val && = PUP.&&
    val || = PUP.||
    infix 7 || &&

    val exactly : char -> (Config.t, char, char) PUP.t = fn a => PUP.satisfy (fn a' => a = a')

    val rec exactlyL : char list-> (Config.t, char list, char list) PUP.t = 
     fn l => (case l
               of [] => PUP.return ([], [])
                | (a::aa) => 
                  let 
                    val p1 = exactly a
                    val p2 = exactlyL aa
                    val p = p1 && p2
                    val p = PUP.isoPartialOut (op ::, Utils.List.dec) p
                  in p
                  end)

    val charsToString : (Config.t, char List.t, char List.t) PUP.t -> (Config.t, string, string) PUP.t = 
        PUP.iso (String.implode, String.explode)
    val alpha : (Config.t, char, char) PUP.t = PUP.satisfy Char.isAlpha
    val alphaNum : (Config.t, char, char) PUP.t = PUP.satisfy Char.isAlphaNum
    val alphaNums : (Config.t, char List.t, char List.t) PUP.t = PUP.zeroOrMore alphaNum
    val stringAlphaNums : (Config.t, string, string) PUP.t = charsToString alphaNums
    val exactlyC : char -> (Config.t, char, char) PUP.t = fn c => PUP.satisfy (fn c' => c' = c)
    val exactlyCs : char list -> (Config.t, char list, char list) PUP.t = exactlyL
    val exactlyS : string -> (Config.t, string, string) PUP.t = charsToString o exactlyCs o String.explode

    val digit : (Config.t, char, char) PUP.t = PUP.satisfy Char.isDigit
    val atLeastOneDigit : (Config.t, char List.t, char List.t) PUP.t = PUP.oneOrMore digit
    val atLeastOneDigitS : (Config.t, string, string) PUP.t = charsToString atLeastOneDigit

    val atLeastOneAlpha : (Config.t, char List.t, char List.t) PUP.t = PUP.oneOrMore alphaNum
    val atLeastOneAlphaS : (Config.t, string, string) PUP.t = charsToString atLeastOneAlpha

    val whiteDigit : (Config.t, char, char) PUP.t = 
        PUP.satisfy (fn c => c = Char.space orelse c = Char.newline orelse c = #"\t" orelse c = #"\r")
    val whiteSpace : (Config.t, char List.t, char List.t) PUP.t = PUP.zeroOrMore whiteDigit

    val eatWhiteSpace : (Config.t, unit, unit) PUP.t = PUP.iso (fn _ => (), fn () => []) whiteSpace

    val eatWhiteSpaceAfter : (Config.t, 'a, 'b) PUP.t -> (Config.t, 'a, 'b) PUP.t =
     fn p =>
        let
          val p : (Config.t, 'a * unit, 'b * unit) PUP.t = p && eatWhiteSpace
          val p : (Config.t, 'a * unit, 'b) PUP.t = PUP.layout (fn (a, ()) => a) p
          val p : (Config.t, 'a, 'b) PUP.t = PUP.leftIso (fn (a, ()) => a, fn a => (a, ())) p
        in p 
        end
    val eatWhiteSpaceBefore : (Config.t, 'a, 'b) PUP.t -> (Config.t, 'a, 'b) PUP.t =
     fn p =>
        let
          val p : (Config.t, unit * 'a, unit * 'b) PUP.t = eatWhiteSpace && p
          val p : (Config.t, unit * 'a, 'b) PUP.t = PUP.layout (fn ((), a) => a) p
          val p : (Config.t, 'a, 'b) PUP.t = PUP.leftIso (fn ((), a) => a, fn a => ((), a)) p
        in p 
        end

    val eatWhiteSpaceAround : (Config.t, 'a, 'b) PUP.t -> (Config.t, 'a, 'b) PUP.t = 
     fn p => eatWhiteSpaceAfter (eatWhiteSpaceBefore  p)

    type 'a u = (Config.t, 'a, Layout.t) PUP.t

    val -&& : unit u * 'a u -> 'a u = 
     fn (a, b) => 
        let
          val p : (Config.t, unit * 'a, Layout.t * Layout.t) PUP.t = a && b
          val p : (Config.t, 'a, Layout.t * Layout.t) PUP.t = PUP.leftIso (fn ((), a) => a, fn a => ((), a)) p
          val p : 'a u = PUP.layout (fn (a, b) => L.seq [a, b]) p
        in p
        end

    val &&- : 'a u * unit u -> 'a u = 
     fn (a, b) => 
        let
          val p : (Config.t, 'a * unit, Layout.t * Layout.t) PUP.t = a && b
          val p : (Config.t, 'a, Layout.t * Layout.t) PUP.t = PUP.leftIso (fn (a, ()) => a, fn a => (a, ())) p
          val p : 'a u = PUP.layout (fn (a, b) => L.seq [a, b]) p
        in p
        end

    val &&& : 'a u * 'b u -> ('a * 'b) u = 
     fn (a, b) => 
        let
          val p : (Config.t, 'a * 'b, Layout.t * Layout.t) PUP.t = a && b
          val p : ('a * 'b) u = PUP.layout (fn (a, b) => L.seq [a, b]) p
        in p
        end

    infix  6 &&- &&&
    infixr 5 -&&

    val !! : (Config.t, unit, L.t list) PUP.t = PUP.return ((), [])

    val *:: : unit u * (Config.t, 'a, L.t list) PUP.t -> (Config.t, 'a, L.t list) PUP.t = 
     fn (a, b) => 
        let
          val p : (Config.t, unit * 'a, Layout.t * (Layout.t list)) PUP.t = a && b
          val p : (Config.t, 'a, Layout.t * (Layout.t list)) PUP.t = PUP.leftIso (fn ((), a) => a, fn a => ((), a)) p
          val p : (Config.t, 'a, Layout.t list) PUP.t = PUP.layout (op ::) p
        in p
        end

    val ::: : 'a u * (Config.t, 'b, L.t list) PUP.t -> (Config.t, 'a * 'b, L.t list) PUP.t = 
     fn (a, b) => 
        let
          val p : (Config.t, 'a * 'b, Layout.t * Layout.t list) PUP.t = a && b
          val p : (Config.t, 'a * 'b, Layout.t list) PUP.t = PUP.layout (op ::) p
        in p
        end

    val seq : (Config.t, 'a, L.t list) PUP.t -> 'a u = fn p => PUP.layout L.seq p

    infixr 5 *:: :::

    val identifier : (Config.t, string, string) PUP.t = 
        PUP.isoPartialOut (String.implode o (op::), Utils.List.dec o String.explode) (alpha && alphaNums)

    val fromStrToStr : (string -> 'a option) * ('a -> string) -> (Config.t, string, string) PUP.t -> 'a u = 
     fn (from, to) => fn p => PUP.leftIsoPartialIn (from, to) (PUP.layout L.str p)

                                                   (* literal s is a unit parse/unparse pair that parses exactly s to produce (), 
                                                    * and lays out () as L.str s.
                                                    * Note: matches prefixes of words
                                                    *)
    val literal : string -> unit u = 
     fn s => PUP.leftIso (fn s => (), fn () => s) (PUP.layout L.str (exactlyS s) )

                         
                         (* keyword s is a unit parse/unparse pair that parses an alphabetic string
                          * s to produce (), and layouts out () as s.  A maximal alphabetic string
                          * is parsed.
                          *)
    val keyword : string -> unit u = 
     fn s => 
        let
          val p : string u = PUP.layout L.str atLeastOneAlphaS
          val p : unit u = 
              PUP.leftIsoPartial (fn s' => if s = s' then SOME () else NONE, 
                                  fn () => SOME s)  p
        in p
        end

    val base : ('a * ('a -> unit option) * string) -> 'a u = 
     fn (con, dec, s) => 
        let
          val p = PUP.leftIsoPartialOut (fn () => con, dec) (literal s)
                                        (*        val p = debugParser (p, 
                                                                       (fn {line, col} => print ("Trying to parse "^s^" at line "^Int.toString line^" and col "^Int.toString col^"\n")),
                                                                       (fn _ => print ("Succeed in parsing "^s^"\n")),
                                                                       (fn _ => print ("Failed to parse "^s^"\n")),
                                                                       (fn _ => print ("Error when parsing "^s^"\n"))) *)
        in p
        end

    val unary : ('a -> 'b) * ('b -> 'a option) * 'a u -> 'b u = 
     fn (f1, f2, p) => PUP.leftIsoPartialOut (f1, f2) p

    val rec2 : ('a * 'b -> 'rec) * ('rec -> 'a * 'b) -> ('a * 'b) u -> 'rec u = PUP.leftIso
    val rec3 = PUP.leftIso
    val rec4 = PUP.leftIso

    val pair = rec2

    val nonNegativeInt : int u = PUP.layout Int.layout (PUP.isoPartialIn (Int.fromString, Int.toString) atLeastOneDigitS)

    val booleanOptionalLiteral : string -> bool u = 
     fn s => 
        let
          val t = PUP.leftIsoPartialOut (fn () => true, fn b => if b then SOME () else NONE) (literal s)
          val f = PUP.leftIsoPartialOut (fn () => false, fn b => if b then SOME () else NONE) (literal "")
        in t || f
        end

    val commaWSSeparatedList : 'a u -> ('a List.t) u =
     fn p => 
        let
          val p = eatWhiteSpaceAround p
          val p' = literal "," -&& p
          val tail : (Config.t, 'a List.t, L.t List.t) PUP.t = PUP.zeroOrMore p'
          val ps : (Config.t, 'a * ('a List.t), L.t * (L.t List.t)) PUP.t = p && tail
          val ps = PUP.layout (fn (a, b) => L.seq (a::b)) ps
          val nonEmptyList = PUP.leftIsoPartialOut (op ::, Utils.List.dec) ps
          val emptyList = PUP.return ([], L.str "")
        in nonEmptyList || emptyList
        end

    val commaWSSeparatedVector : 'a u -> ('a vector) u = 
     fn p => PUP.leftIso (Vector.fromList, Vector.toList) (commaWSSeparatedList p)

    val platform : (IntArb.size -> 'a) * string -> 'a u =
     fn (con, s) => 
        let
          val f = 
           fn config => 
              PUP.leftIsoPartialOut (fn () => con (Config.targetWordSize' config), fn _ => NONE) (literal s)
        in PUP.withEnv f
        end

    val fieldSize        : Mil.fieldSize u = 
        let
          val fs8  = base (Mil.Fs8,  FieldSize.Dec.fs8, "8")
          val fs16 = base (Mil.Fs16, FieldSize.Dec.fs16, "16")
          val fs32 = base (Mil.Fs32, FieldSize.Dec.fs32, "32")
          val fs64 = base (Mil.Fs64, FieldSize.Dec.fs64, "64")
          val fsp = platform (FieldSize.intArbSz, "p")
        in fs8 || fs16 || fs32 || fs64 || fsp
        end

    val intArbTyp : IntArb.typ u = 
        let
          val signed = 
              let
                val s = base (IntArb.Signed, IntArb.Signed.Dec.signed, "S")
                val u = base (IntArb.Unsigned, IntArb.Signed.Dec.unsigned, "U")
              in s || u
              end
          val size = 
              let
                val s8  = base (IntArb.S8, IntArb.Size.Dec.s8, "8")
                val s16 = base (IntArb.S16, IntArb.Size.Dec.s16, "16")
                val s32 = base (IntArb.S32, IntArb.Size.Dec.s32, "32")
                val s64 = base (IntArb.S64, IntArb.Size.Dec.s64, "64")
                val sp = platform (fn sz => sz, "p")
              in s8 || s16 || s32 || s64 || sp
              end
          val p = signed &&- literal "Int" &&& size
          val p = pair (fn (sgn, sz) => IntArb.T (sz, sgn), fn (IntArb.T (sz, sgn)) => (sgn, sz)) p
        in p
        end

    val vectorSize       : Mil.Prims.vectorSize u = 
        let
          val vs64   = base (Mil.Prims.Vs64,   Dec.VectorSize.vs64,   "64")
          val vs128  = base (Mil.Prims.Vs128,  Dec.VectorSize.vs128,  "128")
          val vs256  = base (Mil.Prims.Vs256,  Dec.VectorSize.vs256,  "256")
          val vs512  = base (Mil.Prims.Vs512,  Dec.VectorSize.vs512,  "512")
          val vs1024 = base (Mil.Prims.Vs1024, Dec.VectorSize.vs1024, "1024")
        in vs64 || vs128 || vs256 || vs512 || vs1024
        end

    val vectorDescriptor : Mil.Prims.vectorDescriptor u =
        let
          val r2t = fn (Mil.Prims.Vd {vectorSize, elementSize}) => (vectorSize, elementSize)
          val t2r = fn (v, e) => Mil.Prims.Vd {vectorSize = v, elementSize = e}
        in rec2 (t2r, r2t) (vectorSize &&- literal "x" &&& fieldSize)
        end


    val floatPrecision   : Mil.Prims.floatPrecision u = 
        let
          val fpSingle = base (Mil.Prims.FpSingle, Dec.FloatPrecision.fpSingle, "Float32")
          val fpDouble = base (Mil.Prims.FpDouble, Dec.FloatPrecision.fpDouble, "Float64")
        in fpSingle || fpDouble
        end


    val intPrecision     : Mil.Prims.intPrecision u = 
        let
          val ipArbitrary = base  (Mil.Prims.IpArbitrary, Dec.IntPrecision.ipArbitrary, "Int") 
          val ipFixed     = unary (Mil.Prims.IpFixed, Dec.IntPrecision.ipFixed, intArbTyp)
        in ipArbitrary || ipFixed
        end

    val numericTyp       : Mil.Prims.numericTyp u = 
        let
          val ntRat     = base (Mil.Prims.NtRat, Dec.NumericTyp.ntRat, "Rat")
          val ntInteger = unary (Mil.Prims.NtInteger, Dec.NumericTyp.ntInteger, intPrecision)
          val ntFloat   = unary (Mil.Prims.NtFloat, Dec.NumericTyp.ntFloat, floatPrecision)
        in ntRat || ntInteger || ntFloat
        end
        
    val divKind          : Mil.Prims.divKind u =
        let
          val dkT = base (Mil.Prims.DkT, Dec.DivKind.dkT, "T")
          val dkF = base (Mil.Prims.DkF, Dec.DivKind.dkF, "F")
          val dkE = base (Mil.Prims.DkE, Dec.DivKind.dkE, "E")
        in dkT || dkF || dkE
        end

    val arithOp          : Mil.Prims.arithOp u =
        let
          val aAbs       = base  (Mil.Prims.AAbs, Dec.ArithOp.aAbs, "Abs")
	  val aNegate    = base  (Mil.Prims.ANegate, Dec.ArithOp.aNegate, "Negate")
	  val aNegateSat = base  (Mil.Prims.ANegateSat, Dec.ArithOp.aNegateSat, "NegateSat")
	  val aDivide    = base  (Mil.Prims.ADivide, Dec.ArithOp.aDivide, "Divide")
	  val aDiv       = unary (Mil.Prims.ADiv, Dec.ArithOp.aDiv, literal "Div" -&& divKind)
	  val aMax       = base  (Mil.Prims.AMax, Dec.ArithOp.aMax, "Max")
	  val aMin       = base  (Mil.Prims.AMin, Dec.ArithOp.aMin, "Min")
	  val aMinus     = base  (Mil.Prims.AMinus, Dec.ArithOp.aMinus, "Minus")
	  val aMinusSat  = base  (Mil.Prims.AMinusSat, Dec.ArithOp.aMinusSat, "MinusSat")
          val aMod       = unary (Mil.Prims.AMod, Dec.ArithOp.aMod, literal "Mod" -&& divKind)
	  val aPlus      = base  (Mil.Prims.APlus, Dec.ArithOp.aPlus, "Plus")
	  val aPlusSat   = base  (Mil.Prims.APlusSat, Dec.ArithOp.aPlusSat, "PlusSat")
	  val aTimes     = base  (Mil.Prims.ATimes, Dec.ArithOp.aTimes, "Times")
	  val aTimesSat  = base  (Mil.Prims.ATimesSat, Dec.ArithOp.aTimesSat, "TimesSat")
          val aDivMod    = unary (Mil.Prims.ADivMod, Dec.ArithOp.aDivMod, literal "Divmod" -&& divKind)
        in aAbs || aNegate || aNegateSat || aDivide || aDiv 
                || aMax || aMinus || aMin || aMinusSat || aMod || aPlus || aPlusSat 
                || aTimes || aTimesSat || aDivMod 
        end
        
    val floatOp          : Mil.Prims.floatOp u =
        let
          val faACos  = base  (Mil.Prims.FaACos,  Dec.FloatOp.faACos,  "ACos")
          val faASin  = base  (Mil.Prims.FaASin,  Dec.FloatOp.faASin,  "ASin")
          val faCeil  = base  (Mil.Prims.FaCeil,  Dec.FloatOp.faCeil,  "Ceil")
          val faCos   = base  (Mil.Prims.FaCos,  Dec.FloatOp.faCos,  "Cos")
          val faFloor = base  (Mil.Prims.FaFloor,  Dec.FloatOp.faFloor,  "Floor")
          val faRcp   = base  (Mil.Prims.FaRcp,  Dec.FloatOp.faRcp,  "Rcp")
          val faSin   = base  (Mil.Prims.FaSin,  Dec.FloatOp.faSin,  "Sin")
          val faSqrt  = base  (Mil.Prims.FaSqrt,  Dec.FloatOp.faSqrt,  "Sqrt")
          val faTan   = base  (Mil.Prims.FaTan,  Dec.FloatOp.faTan,  "Tan")
          val faTrunc = base  (Mil.Prims.FaTrunc,  Dec.FloatOp.faTrunc,  "Trunc")
          val faPow   = base  (Mil.Prims.FaPow,  Dec.FloatOp.faPow,  "Pow")
          val res = faACos 
                      || faASin 
                      || faCeil 
                      || faCos 
                      || faFloor 
                      || faRcp 
                      || faSin 
                      || faSqrt 
                      || faTan 
                      || faTrunc 
                      || faPow
        in
          res
        end

    val bitwiseOp        : Mil.Prims.bitwiseOp u =
        let
          val bNot    = base  (Mil.Prims.BNot,  Dec.BitwiseOp.bNot,  "BNot")
          val bAnd    = base  (Mil.Prims.BAnd,  Dec.BitwiseOp.bAnd,  "BAnd")
          val bOr     = base  (Mil.Prims.BOr,  Dec.BitwiseOp.bOr,  "BOr")
          val bRotL   = base  (Mil.Prims.BRotL,  Dec.BitwiseOp.bRotL,  "BRotL")
          val bRotR   = base  (Mil.Prims.BRotR,  Dec.BitwiseOp.bRotR,  "BRotR")
          val bShiftL = base  (Mil.Prims.BShiftL,  Dec.BitwiseOp.bShiftL,  "BShiftL")
          val bShiftR = base  (Mil.Prims.BShiftR,  Dec.BitwiseOp.bShiftR,  "BShiftR")
          val bXor    = base  (Mil.Prims.BXor,  Dec.BitwiseOp.bXor,  "BXor")
          val res = bNot || bAnd || bOr || bRotL || bRotR || bShiftL || bShiftR || bXor
        in
          res
        end

    val logicOp          : Mil.Prims.logicOp u =
        let
          val lNot = base  (Mil.Prims.LNot,  Dec.LogicOp.lNot,  "Not")
          val lAnd = base  (Mil.Prims.LAnd,  Dec.LogicOp.lAnd,  "And")
          val lOr  = base  (Mil.Prims.LOr,  Dec.LogicOp.lOr,  "Or")
          val lXor = base  (Mil.Prims.LXor,  Dec.LogicOp.lXor,  "Xor")
          val lEq  = base  (Mil.Prims.LEq,  Dec.LogicOp.lEq,  "Eq")
          val res = lNot || lAnd || lOr || lXor || lEq
        in
          res
        end

    val compareOp        : Mil.Prims.compareOp u =
        let
          val cEq = base  (Mil.Prims.CEq,  Dec.CompareOp.cEq,  "Eq")
          val cNe = base  (Mil.Prims.CNe,  Dec.CompareOp.cNe,  "Ne")
          val cLt = base  (Mil.Prims.CLt,  Dec.CompareOp.cLt,  "Lt")
          val cLe = base  (Mil.Prims.CLe,  Dec.CompareOp.cLe,  "Le")
          val res = cEq || cNe || cLt || cLe
        in
          res
        end

    val nameOp           : Mil.Prims.nameOp u =
        let
          val nGetString = base (Mil.Prims.NGetString, Dec.NameOp.nGetString, "GetString")
          val nGetHash   = base (Mil.Prims.NGetHash,   Dec.NameOp.nGetHash,   "GetHash")
          val res = nGetString || nGetHash
        in res
        end

    val stringOp         : Mil.Prims.stringOp u =
        let
          val sAllocate   = base  (Mil.Prims.SAllocate,  Dec.StringOp.sAllocate,  "Allocate")
          val sDeallocate = base  (Mil.Prims.SDeallocate,  Dec.StringOp.sDeallocate,  "Deallocate")
          val sGetLen     = base  (Mil.Prims.SGetLen,  Dec.StringOp.sGetLen,  "GetLen")
          val sGetChar    = base  (Mil.Prims.SGetChar,  Dec.StringOp.sGetChar,  "GetChar")
          val sSetChar    = base  (Mil.Prims.SSetChar,  Dec.StringOp.sSetChar,  "SetChar")
          val sEqual      = base  (Mil.Prims.SEqual,  Dec.StringOp.sEqual,  "Equal")
          val res = sAllocate || sDeallocate || sGetLen || sGetChar || sSetChar || sEqual
        in
          res
        end

        
    val prim             : Mil.Prims.prim u =
        let
          val r2t : {typ : 'a, operator : 'b} -> ('a * 'b) = fn {typ, operator} => (typ, operator)
          val t2r : ('a * 'b) -> {typ : 'a, operator : 'b} = fn (typ, operator) => {typ = typ, operator = operator}
          val doR = fn p => rec2 (t2r, r2t) p
          val pNumArith   = unary (Mil.Prims.PNumArith,  Dec.Prim.pNumArith,  doR (numericTyp &&& arithOp))
          val pFloatOp    = unary (Mil.Prims.PFloatOp,  Dec.Prim.pFloatOp,  doR (floatPrecision &&& floatOp))
          val pNumCompare = unary (Mil.Prims.PNumCompare, Dec.Prim.pNumCompare, doR (numericTyp &&& compareOp))
          val pNumConvert = 
              let
                val r2t = fn {from, to} => (from, to)
                val t2r = fn (from ,to) => {to = to, from = from}
                val doR = fn p => rec2 (t2r, r2t) p
                val r = doR (numericTyp &&- literal "To" &&& numericTyp)
              in unary (Mil.Prims.PNumConvert,  Dec.Prim.pNumConvert, r)
              end  
          val pBitwise    = unary (Mil.Prims.PBitwise,  Dec.Prim.pBitwise,  doR (intPrecision &&& bitwiseOp))
          val pBoolean    = unary (Mil.Prims.PBoolean,  Dec.Prim.pBoolean,  logicOp)
          val pName       = unary (Mil.Prims.PName,     Dec.Prim.pName,     literal "Name" -&& nameOp)
          val pCString    = unary (Mil.Prims.PCString,  Dec.Prim.pCString,  literal "CString" -&& stringOp)
          val pPtrEq      = base  (Mil.Prims.PPtrEq,    Dec.Prim.pPtrEq,    "PtrEq")
          val res =
              pNumArith || pFloatOp || pNumCompare || pNumConvert || pBitwise || pBoolean || pName || pCString || pPtrEq
        in
          res
        end


    val assoc            : Mil.Prims.assoc u = 
        let
          val aLeft  = base  (Mil.Prims.ALeft,  Dec.Assoc.aLeft,  "L")
          val aRight = base  (Mil.Prims.ARight,  Dec.Assoc.aRight,  "R")
          val aAny   = base  (Mil.Prims.AAny,  Dec.Assoc.aAny,  "A")
          val res = aLeft || aRight || aAny
        in
          res
        end
        
    val dataOp           : Mil.Prims.dataOp u =
        let
          val dBroadcast = base  (Mil.Prims.DBroadcast,  Dec.DataOp.dBroadcast,  "Broadcast")
          val dVector    = base  (Mil.Prims.DVector,  Dec.DataOp.dVector,  "Vector")
          val dSub       = unary (Mil.Prims.DSub,  Dec.DataOp.dSub,  nonNegativeInt)
          val dPermute   = 
              let
                val doVector = literal "<" -&& commaWSSeparatedVector nonNegativeInt &&- literal ">"
              in unary (Mil.Prims.DPermute,  Dec.DataOp.dPermute,  doVector)
              end
          val dBlend     = base  (Mil.Prims.DBlend,  Dec.DataOp.dBlend,  "Blend")
          val dSplit     = base  (Mil.Prims.DSplit,  Dec.DataOp.dSplit,  "Split")
          val dConcat    = base  (Mil.Prims.DConcat,  Dec.DataOp.dConcat,  "Concat")
          val res = dBroadcast || dVector || dSub || dPermute || dBlend || dSplit || dConcat
        in
          res
        end

    val vector           : Mil.Prims.vector u =
        let
          val angle : 'a u -> 'a u = fn p => literal "<" -&& p &&- literal ">"
          val square : 'a u -> 'a u = fn p => literal "[" -&& p &&- literal "]"

          val viPointwise   = 
              let
                val r2t = fn {descriptor, masked, operator} => ((masked, descriptor), operator)
                val t2r = fn ((masked, descriptor), operator) => 
                             {descriptor = descriptor, masked = masked, operator = operator}
                val masked = booleanOptionalLiteral "?"
                val p = literal "VPointWise" -&& masked &&& square vectorDescriptor &&& angle prim
              in unary (Mil.Prims.ViPointwise, Dec.Vector.viPointwise, rec2 (t2r, r2t) p)
              end
          val viConvert     = 
              let 
                val r2t = 
                 fn {from = {descriptor = d1, typ = t1}, to = {descriptor = d2, typ = t2}} => ((d1, d2), (t1, t2))
                val t2r = 
                 fn ((d1, d2), (t1, t2)) => {from = {descriptor = d1, typ = t1}, to = {descriptor = d2, typ = t2}}
                val p = literal "VConvert" -&& square vectorDescriptor &&& square vectorDescriptor &&& 
                                angle (numericTyp &&- literal "To" &&& numericTyp)
              in unary (Mil.Prims.ViConvert, Dec.Vector.viConvert, rec3 (t2r, r2t) p)
              end

          val viCompare     = 
              let
                val r2t = fn {descriptor = d1, typ = t1, operator = op1} => (d1, (t1, op1))
                val t2r = fn (d1, (t1, op1)) => {descriptor = d1, typ = t1, operator = op1} 
                val p = literal "VCompare" -&& square vectorDescriptor &&& angle (numericTyp &&& compareOp)
              in unary (Mil.Prims.ViCompare, Dec.Vector.viCompare, rec2 (t2r, r2t) p)
              end

          val viReduction   = 
              let
                val r2t = fn {descriptor = d1, associativity = a1, operator = p1} => ((a1, d1), p1)
                val t2r = fn ((a1, d1), p1) => {descriptor = d1, associativity = a1, operator = p1}
                val p = literal "VReduction" -&& assoc &&& square vectorDescriptor &&& prim
              in unary (Mil.Prims.ViReduction, Dec.Vector.viReduction, rec2 (t2r, r2t) p)
              end

          val mk = 
           fn (name, operator) => 
              let
                val r2t = fn {descriptor = d1, operator = op1} => (d1, op1)
                val t2r = fn (d1, op1) => {descriptor = d1, operator = op1} 
                val p = literal name -&& square vectorDescriptor &&& angle operator
              in rec2 (t2r, r2t) p
              end
          val viData        = unary (Mil.Prims.ViData, Dec.Vector.viData, mk ("VData", dataOp))
          val viMaskData    = unary (Mil.Prims.ViMaskData, Dec.Vector.viMaskData, mk ("VMaskData", dataOp))
          val viMaskBoolean = unary (Mil.Prims.ViMaskBoolean, Dec.Vector.viMaskBoolean, mk ("VMaskBoolean", logicOp))
          val viMaskConvert = 
              let 
                val r2t = fn {from = f1, to = t1} => (f1, t1)
                val t2r = fn (f1, t1) => {from = f1, to = t1} 
                val p = literal "VMaskConvert" -&& square vectorDescriptor &&& square vectorDescriptor &&- literal "<>"
              in unary (Mil.Prims.ViMaskConvert, Dec.Vector.viMaskConvert, rec2 (t2r, r2t) p)
              end

          val res = viPointwise 
                      || viConvert 
                      || viCompare 
                      || viReduction 
                      || viData 
                      || viMaskData 
                      || viMaskBoolean 
                      || viMaskConvert
        in
          res
        end

    val runtime          : Mil.Prims.runtime u =
        let
          val rtFloatMk           = base  (Mil.Prims.RtFloatMk,  Dec.Runtime.rtFloatMk,  "FloatMk")
          val rtWriteln           = base  (Mil.Prims.RtWriteln,  Dec.Runtime.rtWriteln,  "Writeln")
          val rtReadln            = base  (Mil.Prims.RtReadln,  Dec.Runtime.rtReadln,  "Readln")
          val rtAssert            = base  (Mil.Prims.RtAssert,  Dec.Runtime.rtAssert,  "Assert")
          val rtError             = base  (Mil.Prims.RtError,  Dec.Runtime.rtError,  "Error")
          val rtDebug             = base  (Mil.Prims.RtDebug,  Dec.Runtime.rtDebug,  "Debug")
          val rtOpenOut           = base  (Mil.Prims.RtOpenOut,  Dec.Runtime.rtOpenOut,  "OpenOut")
          val rtGetStdout         = base  (Mil.Prims.RtGetStdout,  Dec.Runtime.rtGetStdout,  "GetStdout")
          val rtOutputByte        = base  (Mil.Prims.RtOutputByte,  Dec.Runtime.rtOutputByte,  "OutputByte")
          val rtCloseOut          = base  (Mil.Prims.RtCloseOut,  Dec.Runtime.rtCloseOut,  "CloseOut")
          val rtOpenIn            = base  (Mil.Prims.RtOpenIn,  Dec.Runtime.rtOpenIn,  "OpenIn")
          val rtGetStdin          = base  (Mil.Prims.RtGetStdin,  Dec.Runtime.rtGetStdin,  "GetStdin")
          val rtInputByte         = base  (Mil.Prims.RtInputByte,  Dec.Runtime.rtInputByte,  "InputByte")
          val rtInputString       = base  (Mil.Prims.RtInputString,  Dec.Runtime.rtInputString,  "InputString")
          val rtInputAll          = base  (Mil.Prims.RtInputAll,  Dec.Runtime.rtInputAll,  "InputAll")
          val rtIsEOF             = base  (Mil.Prims.RtIsEOF,  Dec.Runtime.rtIsEOF,  "IsEOF")
          val rtCloseIn           = base  (Mil.Prims.RtCloseIn,  Dec.Runtime.rtCloseIn,  "CloseIn")
          val rtCommandLine       = base  (Mil.Prims.RtCommandLine,  Dec.Runtime.rtCommandLine,  "CommandLine")
          val rtStringToNat       = base  (Mil.Prims.RtStringToNat,  Dec.Runtime.rtStringToNat,  "StringToNat")
          val rtStringToFloat     = base  (Mil.Prims.RtStringToFloat,  Dec.Runtime.rtStringToFloat,  "StringToFloat")
          val rtFloatToString     = base  (Mil.Prims.RtFloatToString,  Dec.Runtime.rtFloatToString,  "FloatToString")
          val rtFloatToStringI    = base  (Mil.Prims.RtFloatToStringI,  Dec.Runtime.rtFloatToStringI,  "FloatToStringI")
          val rtRatNumerator      = base  (Mil.Prims.RtRatNumerator,  Dec.Runtime.rtRatNumerator,  "RatNumerator")
          val rtRatDenominator    = base  (Mil.Prims.RtRatDenominator,  Dec.Runtime.rtRatDenominator,  "RatDenominator")
          val rtEqual             = base  (Mil.Prims.RtEqual,  Dec.Runtime.rtEqual,  "Equal")
          val rtDom               = base  (Mil.Prims.RtDom,  Dec.Runtime.rtDom,  "Dom")
          val rtNub               = base  (Mil.Prims.RtNub,  Dec.Runtime.rtNub,  "Nub")
          val rtRatToUIntpChecked = base  (Mil.Prims.RtRatToUIntpChecked, Dec.Runtime.rtRatToUIntpChecked, "RatToUIntpChecked")
          val rtRatToString       = base  (Mil.Prims.RtRatToString,  Dec.Runtime.rtRatToString,  "RatToString")
          val rtStringToRat       = base  (Mil.Prims.RtStringToRat,  Dec.Runtime.rtStringToRat,  "StringToRat")
          val rtResetTimer        = base  (Mil.Prims.RtResetTimer,  Dec.Runtime.rtResetTimer,  "ResetTimer")
          val rtGetTimer          = base  (Mil.Prims.RtGetTimer,  Dec.Runtime.rtGetTimer,  "GetTimer")
          val rtVtuneAttach       = base  (Mil.Prims.RtVtuneAttach,  Dec.Runtime.rtVtuneAttach,  "VtuneAttach")
          val rtVtuneDetach       = base  (Mil.Prims.RtVtuneDetach,  Dec.Runtime.rtVtuneDetach,  "VtuneDetach")
          val rtArrayEval         = base  (Mil.Prims.RtArrayEval,  Dec.Runtime.rtArrayEval,  "ArrayEval")
          val rtIntegerHash       = base  (Mil.Prims.RtIntegerHash, Dec.Runtime.rtIntegerHash, "IntHash")
          val res = rtFloatMk 
                      || rtWriteln 
                      || rtReadln 
                      || rtAssert 
                      || rtError 
                      || rtDebug 
                      || rtOpenOut 
                      || rtGetStdout 
                      || rtOutputByte 
                      || rtCloseOut 
                      || rtOpenIn 
                      || rtGetStdin 
                      || rtInputByte 
                      || rtInputString 
                      || rtInputAll 
                      || rtIsEOF 
                      || rtCloseIn 
                      || rtCommandLine 
                      || rtStringToNat 
                      || rtStringToFloat 
                      || rtFloatToString 
                      || rtFloatToStringI 
                      || rtRatNumerator 
                      || rtRatDenominator 
                      || rtEqual 
                      || rtDom 
                      || rtNub 
                      || rtRatToUIntpChecked 
                      || rtRatToString 
                      || rtStringToRat 
                      || rtResetTimer 
                      || rtGetTimer 
                      || rtVtuneAttach 
                      || rtVtuneDetach 
                      || rtArrayEval
                      || rtIntegerHash
        in
          res
        end

    val t                : Mil.Prims.t u = 
        let
          val prim    = unary (Mil.Prims.Prim,  Dec.T.prim,  prim)
          val runtime = unary (Mil.Prims.Runtime,  Dec.T.runtime,  runtime)
          val vector  = unary (Mil.Prims.Vector,  Dec.T.vector,  vector)
          val res = prim || runtime || vector
        in
          res
        end

    structure Layout = 
    struct
      type 'a t = Config.t * 'a -> Layout.t
      val lift = 
       fn f => 
       fn (config, a) => 
          let
            val {parse, layout} = FileParserUnParser.get f config
            val g = UnParser.run layout
          in case g a
              of SOME l => l
               | NONE   => Fail.fail ("PrimsLayout", "lift", "Layout failed")
          end
      val vectorSize       : Mil.Prims.vectorSize t       = lift vectorSize
      val vectorDescriptor : Mil.Prims.vectorDescriptor t = lift vectorDescriptor
      val floatPrecision   : Mil.Prims.floatPrecision t   = lift floatPrecision
      val intPrecision     : Mil.Prims.intPrecision t     = lift intPrecision
      val numericTyp       : Mil.Prims.numericTyp t       = lift numericTyp
      val divKind          : Mil.Prims.divKind t          = lift divKind
      val arithOp          : Mil.Prims.arithOp t          = lift arithOp
      val floatOp          : Mil.Prims.floatOp t          = lift floatOp
      val bitwiseOp        : Mil.Prims.bitwiseOp t        = lift bitwiseOp
      val logicOp          : Mil.Prims.logicOp t          = lift logicOp
      val compareOp        : Mil.Prims.compareOp t        = lift compareOp
      val nameOp           : Mil.Prims.nameOp t           = lift nameOp
      val stringOp         : Mil.Prims.stringOp t         = lift stringOp
      val prim             : Mil.Prims.prim t             = lift prim
      val assoc            : Mil.Prims.assoc t            = lift assoc
      val dataOp           : Mil.Prims.dataOp t           = lift dataOp
      val vector           : Mil.Prims.vector t           = lift vector
      val runtime          : Mil.Prims.runtime t          = lift runtime
      val t                : Mil.Prims.t t                = lift t
    end (* structure Layout *)

    structure ToString = 
    struct
      type 'a t = Config.t * 'a -> string
      val lift = 
       fn f => fn (config, a) => L.toString (f (config, a))
      val vectorSize       : Mil.Prims.vectorSize t       = lift Layout.vectorSize
      val vectorDescriptor : Mil.Prims.vectorDescriptor t = lift Layout.vectorDescriptor
      val floatPrecision   : Mil.Prims.floatPrecision t   = lift Layout.floatPrecision
      val intPrecision     : Mil.Prims.intPrecision t     = lift Layout.intPrecision
      val numericTyp       : Mil.Prims.numericTyp t       = lift Layout.numericTyp
      val divKind          : Mil.Prims.divKind t          = lift Layout.divKind
      val arithOp          : Mil.Prims.arithOp t          = lift Layout.arithOp
      val floatOp          : Mil.Prims.floatOp t          = lift Layout.floatOp
      val bitwiseOp        : Mil.Prims.bitwiseOp t        = lift Layout.bitwiseOp
      val logicOp          : Mil.Prims.logicOp t          = lift Layout.logicOp
      val compareOp        : Mil.Prims.compareOp t        = lift Layout.compareOp
      val nameOp           : Mil.Prims.nameOp t           = lift Layout.nameOp
      val stringOp         : Mil.Prims.stringOp t         = lift Layout.stringOp
      val prim             : Mil.Prims.prim t             = lift Layout.prim
      val assoc            : Mil.Prims.assoc t            = lift Layout.assoc
      val dataOp           : Mil.Prims.dataOp t           = lift Layout.dataOp
      val vector           : Mil.Prims.vector t           = lift Layout.vector
      val runtime          : Mil.Prims.runtime t          = lift Layout.runtime
      val t                : Mil.Prims.t t                = lift Layout.t
    end (* structure ToString *)

    structure Parse = 
    struct
      type 'a t = 'a FileParser.t
      structure PUP = FileParserUnParser
      val lift = fn p => #parse o (PUP.get p)
      val vectorSize       : Config.t -> Mil.Prims.vectorSize t       = lift vectorSize
      val vectorDescriptor : Config.t -> Mil.Prims.vectorDescriptor t = lift vectorDescriptor
      val floatPrecision   : Config.t -> Mil.Prims.floatPrecision t   = lift floatPrecision
      val intPrecision     : Config.t -> Mil.Prims.intPrecision t     = lift intPrecision
      val numericTyp       : Config.t -> Mil.Prims.numericTyp t       = lift numericTyp
      val divKind          : Config.t -> Mil.Prims.divKind t          = lift divKind
      val arithOp          : Config.t -> Mil.Prims.arithOp t          = lift arithOp
      val floatOp          : Config.t -> Mil.Prims.floatOp t          = lift floatOp
      val bitwiseOp        : Config.t -> Mil.Prims.bitwiseOp t        = lift bitwiseOp
      val logicOp          : Config.t -> Mil.Prims.logicOp t          = lift logicOp
      val compareOp        : Config.t -> Mil.Prims.compareOp t        = lift compareOp
      val nameOp           : Config.t -> Mil.Prims.nameOp t           = lift nameOp
      val stringOp         : Config.t -> Mil.Prims.stringOp t         = lift stringOp
      val prim             : Config.t -> Mil.Prims.prim t             = lift prim
      val assoc            : Config.t -> Mil.Prims.assoc t            = lift assoc
      val dataOp           : Config.t -> Mil.Prims.dataOp t           = lift dataOp
      val vector           : Config.t -> Mil.Prims.vector t           = lift vector
      val runtime          : Config.t -> Mil.Prims.runtime t          = lift runtime
      val t                : Config.t -> Mil.Prims.t t                = lift t
    end (* structure Parse *)

  end (* structure ParserUnParser *)

  structure Layout = ParserUnParser.Layout

  structure Parse = ParserUnParser.Parse

  structure ToString = ParserUnParser.ToString

  structure Ord =
  struct

    type 'a t = 'a -> 'a IFO.t

    val fieldSize        : Mil.fieldSize t        = IFO.base o FieldSize.ord

    val vectorSize       : Mil.Prims.vectorSize t = 
        let
          val inject = 
           fn vs => 
              (case vs
                of Prims.Vs64   => 0
                 | Prims.Vs128  => 1
                 | Prims.Vs256  => 2
                 | Prims.Vs512  => 3
                 | Prims.Vs1024 => 4)
        in IFO.base o inject
        end
        
    val mkPair = 
     fn (f1, f2) => 
     fn (a1, a2) => IFO.pair (f1 a1, f2 a2)

    val mkTriple = 
     fn (f1, f2, f3) => 
     fn (a1, a2, a3) => IFO.pair (f1 a1, IFO.pair (f2 a2, f3 a3))

    val vectorDescriptor : Mil.Prims.vectorDescriptor t = 
        let
          val inject = 
           fn (Mil.Prims.Vd {vectorSize=vs, elementSize=es}) => 
              mkPair (vectorSize, fieldSize) (vs, es)
        in inject
        end

    val floatPrecision   : Mil.Prims.floatPrecision t = 
        let
          val inject = 
           fn fp =>
              (case fp 
                of Prims.FpSingle => 0
                 | Prims.FpDouble => 1)
        in IFO.base o inject
        end

    val intPrecision     : Mil.Prims.intPrecision t = 
        let

          val injectSize = 
           fn sz => 
              (case sz
                of IntArb.S8  => 0 
                 | IntArb.S16 => 1
                 | IntArb.S32 => 2
                 | IntArb.S64 => 3)

          val injectSigned = 
           fn sg =>
              (case sg 
                of IntArb.Signed   => 0
                 | IntArb.Unsigned => 1)

          val injectIntArb = 
           fn t => 
              (case t
                of IntArb.T p => mkPair (IFO.base o injectSize, IFO.base o injectSigned) p)

          val inject = 
           fn ip => 
              (case ip
                of Prims.IpArbitrary => IFO.base 0
                 | Prims.IpFixed ia  => IFO.shift (1, injectIntArb ia))
        in inject
        end

    val numericTyp       : Mil.Prims.numericTyp t = 
        let
          val inject = 
           fn nt => 
              (case nt
                of Prims.NtRat        => IFO.base 0
                 | Prims.NtInteger ip => IFO.shift (1, intPrecision ip)
                 | Prims.NtFloat fp   => IFO.shift (2, floatPrecision fp))
        in inject
        end
           
    val divKind          : Mil.Prims.divKind t = 
        let 
          val number = 
           fn dk => 
              (case dk 
                of Prims.DkT => 0 
                 | Prims.DkF => 1 
                 | Prims.DkE => 2)
        in IFO.base o number
        end

    val arithOp          : Mil.Prims.arithOp t = 
        let
          val inject = 
           fn ao => 
              (case ao
                of Prims.AAbs       => IFO.base 0
                 | Prims.ANegate    => IFO.base 1
                 | Prims.ANegateSat => IFO.base 2
                 | Prims.ADivide    => IFO.base 3
                 | Prims.ADiv dk    => IFO.shift (4, divKind dk)
                 | Prims.AMax       => IFO.base 5
                 | Prims.AMin       => IFO.base 6
                 | Prims.AMinus     => IFO.base 7
                 | Prims.AMinusSat  => IFO.base 8
                 | Prims.AMod dk    => IFO.shift (9, divKind dk)
                 | Prims.APlus      => IFO.base 10
                 | Prims.APlusSat   => IFO.base 11
                 | Prims.ATimes     => IFO.base 12
                 | Prims.ATimesSat  => IFO.base 13
                 | Prims.ADivMod dk => IFO.shift (14, divKind dk))

        in inject
        end

    val floatOp          : Mil.Prims.floatOp t = 
        let
          val number = 
           fn fo => 
              (case fo
                of Prims.FaACos  => 0
                 | Prims.FaASin  => 1
                 | Prims.FaCeil  => 2
                 | Prims.FaCos   => 3
                 | Prims.FaFloor => 4
                 | Prims.FaRcp   => 5
                 | Prims.FaSin   => 6
                 | Prims.FaSqrt  => 7
                 | Prims.FaTan   => 8
                 | Prims.FaTrunc => 9
                 | Prims.FaPow   => 10)
        in IFO.base o number
        end

    val bitwiseOp        : Mil.Prims.bitwiseOp t = 
        let
          val number = 
           fn bo => 
              (case bo
                of Prims.BNot    => 0
                 | Prims.BAnd    => 1
                 | Prims.BOr     => 2
                 | Prims.BRotL   => 3
                 | Prims.BRotR   => 4
                 | Prims.BShiftL => 5
                 | Prims.BShiftR => 6
                 | Prims.BXor    => 7)
        in IFO.base o number
        end

    val logicOp          : Mil.Prims.logicOp t = 
        let
          val number = 
           fn lo => 
              (case lo
                of Prims.LNot => 0
                 | Prims.LAnd => 1
                 | Prims.LOr  => 2
                 | Prims.LXor => 3
                 | Prims.LEq  => 4)
        in IFO.base o number
        end

    val compareOp        : Mil.Prims.compareOp t = 
        let
          val number = 
           fn co => 
              (case co
                of Prims.CEq => 0
                 | Prims.CNe => 1
                 | Prims.CLt => 2
                 | Prims.CLe => 3)
        in IFO.base o number
        end

    val nameOp           : Mil.Prims.nameOp t =
     fn no =>
        IFO.base (case no
                   of Prims.NGetString => 0
                    | Prims.NGetHash   => 1)

    val stringOp         : Mil.Prims.stringOp t = 
        let
          val number = 
           fn so => 
              (case so
                of Prims.SAllocate   => 0
                 | Prims.SDeallocate => 1
                 | Prims.SGetLen     => 2
                 | Prims.SGetChar    => 3
                 | Prims.SSetChar    => 4
                 | Prims.SEqual      => 5)
        in IFO.base o number
        end

    val prim             : Mil.Prims.prim t = 
        let
          val inject = 
           fn p => 
              (case p
                of Prims.PNumArith  {typ, operator}  => IFO.shift (0, mkPair (numericTyp, arithOp) (typ, operator))
                 | Prims.PFloatOp {typ, operator}    => IFO.shift (1, mkPair (floatPrecision, floatOp) (typ, operator))
                 | Prims.PNumCompare {typ, operator} => IFO.shift (2, mkPair (numericTyp, compareOp) (typ, operator))
                 | Prims.PNumConvert {to, from}      => IFO.shift (3, mkPair (numericTyp, numericTyp) (to, from))
                 | Prims.PBitwise {typ, operator}    => IFO.shift (4, mkPair (intPrecision, bitwiseOp) (typ, operator))
                 | Prims.PBoolean l                  => IFO.shift (5, logicOp l)
                 | Prims.PName n                     => IFO.shift (6, nameOp n)
                 | Prims.PCString s                  => IFO.shift (7, stringOp s)
                 | Prims.PPtrEq                      => IFO.base 8)
        in inject
        end

    val assoc            : Mil.Prims.assoc t = 
        let
          val number =
           fn a => 
              (case a
                of Prims.ALeft  => 0
                 | Prims.ARight => 1
                 | Prims.AAny   => 2)
        in IFO.base o number
        end

    val dataOp           : Mil.Prims.dataOp t = 
        let
          val inject = 
           fn d => 
              (case d
                of Prims.DBroadcast => IFO.base 0
                 | Prims.DVector    => IFO.base 1
                 | Prims.DSub i     => IFO.basePair (2, i)
                 | Prims.DPermute v => IFO.shift (3, IFO.baseVector v)
                 | Prims.DBlend     => IFO.base 4
                 | Prims.DSplit     => IFO.base 5
                 | Prims.DConcat    => IFO.base 6)
        in inject
        end

    val vector           : Mil.Prims.vector t = 
        let 
          val boolean = 
           fn b => IFO.base (if b then 0 else 1)

          val dtPair = 
              fn {descriptor, typ} => mkPair (vectorDescriptor, numericTyp) (descriptor, typ)

          val inject = 
           fn v => 
              (case v
                of Prims.ViPointwise {descriptor, masked, operator} => 
                   IFO.shift (0, mkTriple (vectorDescriptor, boolean, prim) (descriptor, masked, operator))
                 | Prims.ViConvert {to, from} => 
                   IFO.shift (1, mkPair (dtPair, dtPair) (to, from))
                 | Prims.ViCompare {descriptor, typ, operator} => 
                   IFO.shift (2, mkTriple (vectorDescriptor, numericTyp, compareOp) (descriptor, typ, operator))
                 | Prims.ViReduction {descriptor, associativity, operator} =>
                   IFO.shift (3, mkTriple (vectorDescriptor, assoc, prim) (descriptor, associativity, operator))
                 | Prims.ViData {descriptor, operator} => 
                   IFO.shift (4, mkPair (vectorDescriptor, dataOp) (descriptor, operator))
                 | Prims.ViMaskData {descriptor, operator} => 
                   IFO.shift (5, mkPair (vectorDescriptor, dataOp) (descriptor, operator))
                 | Prims.ViMaskBoolean {descriptor, operator} => 
                   IFO.shift (6, mkPair (vectorDescriptor, logicOp) (descriptor, operator))
                 | Prims.ViMaskConvert {to, from} => 
                   IFO.shift (7, mkPair (vectorDescriptor, vectorDescriptor) (to, from)))
        in inject
        end

    val runtime          : Mil.Prims.runtime t = 
        let
          val number = 
           fn rt => 
              (case rt
                of Prims.RtFloatMk           => 0
                 | Prims.RtWriteln           => 1
                 | Prims.RtReadln            => 2
                 | Prims.RtAssert            => 3
                 | Prims.RtError             => 4
                 | Prims.RtDebug             => 5
                 | Prims.RtOpenOut           => 6
                 | Prims.RtGetStdout         => 7
                 | Prims.RtOutputByte        => 8
                 | Prims.RtCloseOut          => 9
                 | Prims.RtOpenIn            => 10
                 | Prims.RtGetStdin          => 11
                 | Prims.RtInputByte         => 12
                 | Prims.RtInputString       => 13
                 | Prims.RtInputAll          => 14
                 | Prims.RtIsEOF             => 15
                 | Prims.RtCloseIn           => 16
                 | Prims.RtCommandLine       => 17
                 | Prims.RtStringToNat       => 18
                 | Prims.RtStringToFloat     => 19
                 | Prims.RtFloatToString     => 20
                 | Prims.RtFloatToStringI    => 21
                 | Prims.RtRatNumerator      => 22
                 | Prims.RtRatDenominator    => 23
                 | Prims.RtEqual             => 24
                 | Prims.RtDom               => 25
                 | Prims.RtNub               => 26
                 | Prims.RtRatToUIntpChecked => 27
                 | Prims.RtRatToString       => 28
                 | Prims.RtStringToRat       => 29
                 | Prims.RtResetTimer        => 30
                 | Prims.RtGetTimer          => 31
                 | Prims.RtVtuneAttach       => 32
                 | Prims.RtVtuneDetach       => 33
                 | Prims.RtArrayEval         => 34
                 | Prims.RtIntegerHash       => 35)
        in IFO.base o number
        end

    val t                : Mil.Prims.t t = 
        let
          val inject = 
           fn t => 
              (case t
                of Prims.Prim p    => IFO.shift (0, prim p)
                 | Prims.Runtime r => IFO.shift (1, runtime r)
                 | Prims.Vector v  => IFO.shift (2, vector v))
        in inject
        end
  end (* structure Ord *)

  structure Compare =
  struct
    type 'a t = 'a Compare.t
    val vectorSize       : Mil.Prims.vectorSize t        = IFO.compare o (Utils.Function.apply2 Ord.vectorSize)
    val vectorDescriptor : Mil.Prims.vectorDescriptor t  = IFO.compare o (Utils.Function.apply2 Ord.vectorDescriptor)
    val floatPrecision   : Mil.Prims.floatPrecision t    = IFO.compare o (Utils.Function.apply2 Ord.floatPrecision)
    val intPrecision     : Mil.Prims.intPrecision t      = IFO.compare o (Utils.Function.apply2 Ord.intPrecision)
    val numericTyp       : Mil.Prims.numericTyp t        = IFO.compare o (Utils.Function.apply2 Ord.numericTyp)
    val divKind          : Mil.Prims.divKind t           = IFO.compare o (Utils.Function.apply2 Ord.divKind)
    val arithOp          : Mil.Prims.arithOp t           = IFO.compare o (Utils.Function.apply2 Ord.arithOp)
    val floatOp          : Mil.Prims.floatOp t           = IFO.compare o (Utils.Function.apply2 Ord.floatOp)
    val bitwiseOp        : Mil.Prims.bitwiseOp t         = IFO.compare o (Utils.Function.apply2 Ord.bitwiseOp)
    val logicOp          : Mil.Prims.logicOp t           = IFO.compare o (Utils.Function.apply2 Ord.logicOp)
    val compareOp        : Mil.Prims.compareOp t         = IFO.compare o (Utils.Function.apply2 Ord.compareOp)
    val nameOp           : Mil.Prims.nameOp t            = IFO.compare o (Utils.Function.apply2 Ord.nameOp)
    val stringOp         : Mil.Prims.stringOp t          = IFO.compare o (Utils.Function.apply2 Ord.stringOp)
    val prim             : Mil.Prims.prim t              = IFO.compare o (Utils.Function.apply2 Ord.prim)
    val assoc            : Mil.Prims.assoc t             = IFO.compare o (Utils.Function.apply2 Ord.assoc)
    val dataOp           : Mil.Prims.dataOp t            = IFO.compare o (Utils.Function.apply2 Ord.dataOp)
    val vector           : Mil.Prims.vector t            = IFO.compare o (Utils.Function.apply2 Ord.vector)
    val runtime          : Mil.Prims.runtime t           = IFO.compare o (Utils.Function.apply2 Ord.runtime)
    val t                : Mil.Prims.t t                 = IFO.compare o (Utils.Function.apply2 Ord.t)
  end (* structure Compare *)

  structure Eq =
  struct
    type 'a t = 'a * 'a -> bool
    val vectorSize       : Mil.Prims.vectorSize t        = IFO.eq o (Utils.Function.apply2 Ord.vectorSize)
    val vectorDescriptor : Mil.Prims.vectorDescriptor t  = IFO.eq o (Utils.Function.apply2 Ord.vectorDescriptor)
    val floatPrecision   : Mil.Prims.floatPrecision t    = IFO.eq o (Utils.Function.apply2 Ord.floatPrecision)
    val intPrecision     : Mil.Prims.intPrecision t      = IFO.eq o (Utils.Function.apply2 Ord.intPrecision)
    val numericTyp       : Mil.Prims.numericTyp t        = IFO.eq o (Utils.Function.apply2 Ord.numericTyp)
    val divKind          : Mil.Prims.divKind t           = IFO.eq o (Utils.Function.apply2 Ord.divKind)
    val arithOp          : Mil.Prims.arithOp t           = IFO.eq o (Utils.Function.apply2 Ord.arithOp)
    val floatOp          : Mil.Prims.floatOp t           = IFO.eq o (Utils.Function.apply2 Ord.floatOp)
    val bitwiseOp        : Mil.Prims.bitwiseOp t         = IFO.eq o (Utils.Function.apply2 Ord.bitwiseOp)
    val logicOp          : Mil.Prims.logicOp t           = IFO.eq o (Utils.Function.apply2 Ord.logicOp)
    val compareOp        : Mil.Prims.compareOp t         = IFO.eq o (Utils.Function.apply2 Ord.compareOp)
    val nameOp           : Mil.Prims.nameOp t            = IFO.eq o (Utils.Function.apply2 Ord.nameOp)
    val stringOp         : Mil.Prims.stringOp t          = IFO.eq o (Utils.Function.apply2 Ord.stringOp)
    val prim             : Mil.Prims.prim t              = IFO.eq o (Utils.Function.apply2 Ord.prim)
    val assoc            : Mil.Prims.assoc t             = IFO.eq o (Utils.Function.apply2 Ord.assoc)
    val dataOp           : Mil.Prims.dataOp t            = IFO.eq o (Utils.Function.apply2 Ord.dataOp)
    val vector           : Mil.Prims.vector t            = IFO.eq o (Utils.Function.apply2 Ord.vector)
    val runtime          : Mil.Prims.runtime t           = IFO.eq o (Utils.Function.apply2 Ord.runtime)
    val t                : Mil.Prims.t t                 = IFO.eq o (Utils.Function.apply2 Ord.t)
  end (* structure Eq *)

  structure Hash =
  struct
    type 'a t = 'a -> Word32.word
    val vectorSize       : Mil.Prims.vectorSize t        = IFO.hash o Ord.vectorSize
    val vectorDescriptor : Mil.Prims.vectorDescriptor t  = IFO.hash o Ord.vectorDescriptor
    val floatPrecision   : Mil.Prims.floatPrecision t    = IFO.hash o Ord.floatPrecision
    val intPrecision     : Mil.Prims.intPrecision t      = IFO.hash o Ord.intPrecision
    val numericTyp       : Mil.Prims.numericTyp t        = IFO.hash o Ord.numericTyp
    val divKind          : Mil.Prims.divKind t           = IFO.hash o Ord.divKind
    val arithOp          : Mil.Prims.arithOp t           = IFO.hash o Ord.arithOp
    val floatOp          : Mil.Prims.floatOp t           = IFO.hash o Ord.floatOp
    val bitwiseOp        : Mil.Prims.bitwiseOp t         = IFO.hash o Ord.bitwiseOp
    val logicOp          : Mil.Prims.logicOp t           = IFO.hash o Ord.logicOp
    val compareOp        : Mil.Prims.compareOp t         = IFO.hash o Ord.compareOp
    val nameOp           : Mil.Prims.nameOp t            = IFO.hash o Ord.nameOp
    val stringOp         : Mil.Prims.stringOp t          = IFO.hash o Ord.stringOp
    val prim             : Mil.Prims.prim t              = IFO.hash o Ord.prim
    val assoc            : Mil.Prims.assoc t             = IFO.hash o Ord.assoc
    val dataOp           : Mil.Prims.dataOp t            = IFO.hash o Ord.dataOp
    val vector           : Mil.Prims.vector t            = IFO.hash o Ord.vector
    val runtime          : Mil.Prims.runtime t           = IFO.hash o Ord.runtime
    val t                : Mil.Prims.t t                 = IFO.hash o Ord.t
  end (* structure Hash *)

  structure Effects =
  struct
    type 'a t = 'a -> Effect.set
    val stringOp         : Mil.Prims.stringOp t =
        (fn so => 
            let
              val fx = 
                  (case so
                    of Mil.Prims.SAllocate   => Effect.single Effect.HeapGen
                     | Mil.Prims.SDeallocate => Effect.single Effect.HeapWrite
                     | Mil.Prims.SGetLen     => Effect.single Effect.HeapRead
                     | Mil.Prims.SGetChar    => Effect.single Effect.HeapRead
                     | Mil.Prims.SSetChar    => Effect.single Effect.HeapWrite
                     | Mil.Prims.SEqual      => Effect.Total)
            in fx
            end)

    val prim             : Mil.Prims.prim t = 
        (fn p => 
            let
              val total = Effect.Total
              val fx = 
                  (case p
                    of Mil.Prims.PNumArith  {typ, operator}  => total
                     | Mil.Prims.PFloatOp {typ, operator}    => total
                     | Mil.Prims.PNumCompare {typ, operator} => total
                     | Mil.Prims.PNumConvert {to, from}      => total
                     | Mil.Prims.PBitwise {typ, operator}    => total
                     | Mil.Prims.PBoolean l                  => total
                     | Mil.Prims.PName n                     => total
                     | Mil.Prims.PCString s                  => stringOp s
                     | Mil.Prims.PPtrEq                      => total)
            in fx
            end)

    val vector           : Mil.Prims.vector t =
        (fn v =>
            let
              val total = Effect.Total
              val fx = 
                  (case v
                    of Mil.Prims.ViPointwise {descriptor, masked, operator}        => prim operator
                     | Mil.Prims.ViConvert {to, from}                              => total
                     | Mil.Prims.ViCompare {descriptor, typ, operator}             => total
                     | Mil.Prims.ViReduction {descriptor, associativity, operator} => prim operator
                     | Mil.Prims.ViData {descriptor, operator}                     => total
                     | Mil.Prims.ViMaskData {descriptor, operator}                 => total
                     | Mil.Prims.ViMaskBoolean {descriptor, operator}              => total
                     | Mil.Prims.ViMaskConvert {to, from}                          => total)
            in fx
            end)

    val runtime          : Mil.Prims.runtime t = 
        (fn r => 
            let
              val total = Effect.Total
              val io = Effect.single Effect.Io
              val partial = Effect.single Effect.Partial
              val any = Effect.PAny
              val fx = 
                  (case r
                    of Mil.Prims.RtFloatMk           => total
                     | Mil.Prims.RtWriteln           => io
                     | Mil.Prims.RtReadln            => io
                     | Mil.Prims.RtAssert            => partial
                     | Mil.Prims.RtError             => partial
                     | Mil.Prims.RtDebug             => partial
                     | Mil.Prims.RtOpenOut           => io
                     | Mil.Prims.RtGetStdout         => io
                     | Mil.Prims.RtOutputByte        => io
                     | Mil.Prims.RtCloseOut          => io
                     | Mil.Prims.RtOpenIn            => io
                     | Mil.Prims.RtGetStdin          => io
                     | Mil.Prims.RtInputByte         => io
                     | Mil.Prims.RtInputString       => io
                     | Mil.Prims.RtInputAll          => io
                     | Mil.Prims.RtIsEOF             => io
                     | Mil.Prims.RtCloseIn           => io
                     | Mil.Prims.RtCommandLine       => total
                     | Mil.Prims.RtStringToNat       => total
                     | Mil.Prims.RtStringToFloat     => total
                     | Mil.Prims.RtFloatToString     => total
                     | Mil.Prims.RtFloatToStringI    => total
                     | Mil.Prims.RtRatNumerator      => total
                     | Mil.Prims.RtRatDenominator    => total
                     | Mil.Prims.RtEqual             => total
                     | Mil.Prims.RtDom               => total
                     | Mil.Prims.RtNub               => total
                     | Mil.Prims.RtRatToUIntpChecked => total
                     | Mil.Prims.RtRatToString       => total
                     | Mil.Prims.RtStringToRat       => total
                     | Mil.Prims.RtResetTimer        => io
                     | Mil.Prims.RtGetTimer          => io
                     | Mil.Prims.RtVtuneAttach       => io
                     | Mil.Prims.RtVtuneDetach       => io
                     | Mil.Prims.RtArrayEval         => any
                     | Mil.Prims.RtIntegerHash       => total)
            in fx
            end)

    val t                : Mil.Prims.t t =
        (fn t => 
            (case t
              of Mil.Prims.Prim p => prim p
               | Mil.Prims.Vector v => vector v
               | Mil.Prims.Runtime r => runtime r))
  end (* structure Effects *)

  structure VectorSize = 
  struct
    type t = Mil.Prims.vectorSize
    val compare = Compare.vectorSize
    val eq      = Eq.vectorSize
    val hash    = Hash.vectorSize
    val fromBits = 
     fn bits => 
        (case bits
          of 64   => SOME Mil.Prims.Vs64
           | 128  => SOME Mil.Prims.Vs128
           | 256  => SOME Mil.Prims.Vs256
           | 512  => SOME Mil.Prims.Vs512
           | 1024 => SOME Mil.Prims.Vs1024
           | _    => NONE)
    val numBits : t -> int = 
     fn vs => 
        (case vs
          of Mil.Prims.Vs64   => 64
           | Mil.Prims.Vs128  => 128
           | Mil.Prims.Vs256  => 256
           | Mil.Prims.Vs512  => 512
           | Mil.Prims.Vs1024 => 1024)
    val toString = ToString.vectorSize
    val toValueSize : t -> Mil.valueSize = 
     fn vs => 
        (case vs
          of Mil.Prims.Vs64   => Mil.Vs64
           | Mil.Prims.Vs128  => Mil.Vs128
           | Mil.Prims.Vs256  => Mil.Vs256
           | Mil.Prims.Vs512  => Mil.Vs512
           | Mil.Prims.Vs1024 => Mil.Vs1024)
    val halfSize : t -> t option = fn vs => fromBits ((numBits vs) div 2)
    val doubleSize : t -> t option = fn vs => fromBits ((numBits vs) * 2)
    val platformSize : Config.t -> t = 
     fn c =>
        (case Config.targetVectorSize c
          of Config.Vs128 => Mil.Prims.Vs128
           | Config.Vs256 => Mil.Prims.Vs256
           | Config.Vs512 => Mil.Prims.Vs512)
    structure Dec = Dec.VectorSize
  end (* structure VectorSize *)

  structure VectorDescriptor = 
  struct
    type t = Mil.Prims.vectorDescriptor
    val compare = Compare.vectorDescriptor
    val eq      = Eq.vectorDescriptor
    val hash    = Hash.vectorDescriptor
    val toString = ToString.vectorDescriptor
    val numBits : t -> int = 
     fn (Mil.Prims.Vd {vectorSize, elementSize}) => VectorSize.numBits vectorSize
    val elementCount : t -> int = 
     fn (Mil.Prims.Vd {vectorSize, elementSize}) => (VectorSize.numBits vectorSize) div (FieldSize.numBits elementSize)
    val vectorSize : t -> VectorSize.t = 
     fn (Mil.Prims.Vd {vectorSize, elementSize}) => vectorSize
    val elementSize : t -> Mil.fieldSize = 
     fn (Mil.Prims.Vd {vectorSize, elementSize}) => elementSize
    structure Dec = Dec.VectorDescriptor
  end (* structure VectorDescriptor *)

  structure FloatPrecision = 
  struct
    type t = Mil.Prims.floatPrecision
    val compare = Compare.floatPrecision
    val eq      = Eq.floatPrecision
    val hash    = Hash.floatPrecision
    structure Dec = Dec.FloatPrecision
  end (* structure FloatPrecision *)

  structure IntPrecision = 
  struct
    type t = Mil.Prims.intPrecision
    val compare = Compare.intPrecision
    val eq      = Eq.intPrecision
    val hash    = Hash.intPrecision
    structure Dec = Dec.IntPrecision
  end (* structure IntPrecision *)

  structure NumericTyp = 
  struct
    type t = Mil.Prims.numericTyp
    val compare = Compare.numericTyp
    val eq      = Eq.numericTyp
    val hash    = Hash.numericTyp
                     
    structure Dec = Dec.NumericTyp
  end (* structure NumericTyp *)

  structure DivKind = 
  struct
    type t = Mil.Prims.divKind
    val compare = Compare.divKind
    val eq      = Eq.divKind
    val hash    = Hash.divKind
    structure Dec = Dec.DivKind
  end (* structure DivKind *)

  structure ArithOp = 
  struct
    type t = Mil.Prims.arithOp
    val compare = Compare.arithOp
    val eq      = Eq.arithOp
    val hash    = Hash.arithOp
    structure Dec = Dec.ArithOp
  end (* structure ArithOp *)

  structure FloatOp = 
  struct
    type t = Mil.Prims.floatOp
    val compare = Compare.floatOp
    val eq      = Eq.floatOp
    val hash    = Hash.floatOp
    structure Dec = Dec.FloatOp
  end (* structure FloatOp *)

  structure BitwiseOp = 
  struct
    type t = Mil.Prims.bitwiseOp
    val compare = Compare.bitwiseOp
    val eq      = Eq.bitwiseOp
    val hash    = Hash.bitwiseOp
    structure Dec = Dec.BitwiseOp
  end (* structure BitwiseOp *)

  structure LogicOp = 
  struct
    type t = Mil.Prims.logicOp
    val compare = Compare.logicOp
    val eq      = Eq.logicOp
    val hash    = Hash.logicOp
    structure Dec = Dec.LogicOp
  end (* structure LogicOp *)

  structure CompareOp = 
  struct
    type t = Mil.Prims.compareOp
    val compare = Compare.compareOp
    val eq      = Eq.compareOp
    val hash    = Hash.compareOp
    structure Dec = Dec.CompareOp
  end (* structure CompareOp *)

  structure NameOp =
  struct
    type t = Mil.Prims.nameOp
    val compare = Compare.nameOp
    val eq      = Eq.nameOp
    val hash    = Hash.nameOp
    structure Dec = Dec.NameOp
  end (* structure NameOp *)

  structure StringOp = 
  struct
    type t = Mil.Prims.stringOp
    val compare = Compare.stringOp
    val eq      = Eq.stringOp
    val hash    = Hash.stringOp
    structure Dec = Dec.StringOp
  end (* structure StringOp *)

  structure Prim = 
  struct
    type t = Mil.Prims.prim
    val compare = Compare.prim
    val eq      = Eq.prim
    val hash    = Hash.prim
    structure Dec = Dec.Prim
  end (* structure Prim *)

  structure Assoc = 
  struct
    type t = Mil.Prims.assoc
    val compare = Compare.assoc
    val eq      = Eq.assoc
    val hash    = Hash.assoc
    structure Dec = Dec.Assoc
  end (* structure Assoc *)

  structure DataOp = 
  struct
    type t = Mil.Prims.dataOp
    val compare = Compare.dataOp
    val eq      = Eq.dataOp
    val hash    = Hash.dataOp
    structure Dec = Dec.DataOp
  end (* structure DataOp *)

  structure Vector = 
  struct
    type t = Mil.Prims.vector
    val compare = Compare.vector
    val eq      = Eq.vector
    val hash    = Hash.vector
    structure Dec = Dec.Vector
  end (* structure Vector *)

  structure Runtime = 
  struct
    type t = Mil.Prims.runtime
    val compare = Compare.runtime
    val eq      = Eq.runtime
    val hash    = Hash.runtime
    structure Dec = Dec.Runtime
  end (* structure Runtime *)

  structure T = 
  struct
    type t = Mil.Prims.t
    val compare = Compare.t
    val eq      = Eq.t
    val hash    = Hash.t
    structure Dec = Dec.T
  end (* structure T *)

  structure Arity =
  struct
    datatype arity = ArAtoA | ArAAtoA | ArAAtoB | ArOther of Int.t * Int.t

    type 'a t = 'a -> arity

    val count : arity -> Int.t * Int.t = 
     fn ar => 
        (case ar
          of ArAtoA      => (1, 1)
           | ArAAtoA     => (2, 1)
           | ArAAtoB     => (2, 1)
           | ArOther p => p)

    val arithOp          : Mil.Prims.arithOp t =
        (fn ao => 
            (case ao
              of Prims.AAbs       => ArAtoA
               | Prims.ANegate    => ArAtoA
               | Prims.ANegateSat => ArAtoA
               | Prims.ADivide    => ArAAtoA
               | Prims.ADiv dk    => ArAAtoA
               | Prims.AMax       => ArAAtoA
               | Prims.AMin       => ArAAtoA
               | Prims.AMinus     => ArAAtoA
               | Prims.AMinusSat  => ArAAtoA
               | Prims.AMod dk    => ArAAtoA
               | Prims.APlus      => ArAAtoA
               | Prims.APlusSat   => ArAAtoA
               | Prims.ATimes     => ArAAtoA
               | Prims.ATimesSat  => ArAAtoA
               | Prims.ADivMod dk => ArOther (2, 2)))

    val floatOp          : Mil.Prims.floatOp t =
        (fn fo => 
            (case fo 
              of Prims.FaACos  => ArAtoA
               | Prims.FaASin  => ArAtoA
               | Prims.FaCeil  => ArAtoA
               | Prims.FaCos   => ArAtoA
               | Prims.FaFloor => ArAtoA
               | Prims.FaRcp   => ArAtoA
               | Prims.FaSin   => ArAtoA
               | Prims.FaSqrt  => ArAtoA
               | Prims.FaTan   => ArAtoA
               | Prims.FaTrunc => ArAtoA
               | Prims.FaPow   => ArAAtoA))

    val bitwiseOp        : Mil.Prims.bitwiseOp t =
        (fn bo => 
            (case bo
              of Prims.BNot    => ArAtoA
               | Prims.BAnd    => ArAAtoA
               | Prims.BOr     => ArAAtoA
               | Prims.BRotL   => ArOther (2, 1)
               | Prims.BRotR   => ArOther (2, 1)
               | Prims.BShiftL => ArOther (2, 1)
               | Prims.BShiftR => ArOther (2, 1)
               | Prims.BXor    => ArAAtoA))

    val logicOp          : Mil.Prims.logicOp t =
        (fn lo => 
            (case lo
              of Prims.LNot => ArAtoA
               | Prims.LAnd => ArAAtoA
               | Prims.LOr  => ArAAtoA
               | Prims.LXor => ArAAtoA
               | Prims.LEq  => ArAAtoA))

    val compareOp        : Mil.Prims.compareOp t = 
        (fn co => 
            (case co
              of Prims.CEq => ArAAtoB
               | Prims.CNe => ArAAtoB
               | Prims.CLt => ArAAtoB
               | Prims.CLe => ArAAtoB))

    val nameOp           : Mil.Prims.nameOp t =
     fn no =>
        case no
         of Mil.Prims.NGetString => ArOther (1, 1)
          | Mil.Prims.NGetHash => ArOther (1, 1)

    val stringOp         : Mil.Prims.stringOp t =
        (fn so => 
            (case so
              of Mil.Prims.SAllocate   => ArOther (1, 1)
               | Mil.Prims.SDeallocate => ArOther (1, 0)
               | Mil.Prims.SGetLen     => ArOther (1, 1)
               | Mil.Prims.SGetChar    => ArOther (2, 1)
               | Mil.Prims.SSetChar    => ArOther (3, 1)
               | Mil.Prims.SEqual      => ArAAtoB))

    val dataOp           : Mil.Prims.vectorDescriptor -> Mil.Prims.dataOp t = 
        (fn vd => 
         fn d =>
            (case d 
              of Prims.DBroadcast => ArOther (1, 1)
               | Prims.DVector    => ArOther (VectorDescriptor.elementCount vd, 1)
               | Prims.DSub i     => ArOther (1, 1)
               | Prims.DPermute v => ArAtoA 
               | Prims.DBlend     => ArAAtoA
               | Prims.DSplit     => ArOther (1, 2)
               | Prims.DConcat    => ArAAtoB))


    val prim             : Mil.Prims.prim t = 
        (fn p => 
            (case p
              of Mil.Prims.PNumArith  {typ, operator}  => arithOp operator
               | Mil.Prims.PFloatOp {typ, operator}    => floatOp operator
               | Mil.Prims.PNumCompare {typ, operator} => compareOp operator
               | Mil.Prims.PNumConvert {to, from}      => ArOther (1, 1)
               | Mil.Prims.PBitwise {typ, operator}    => bitwiseOp operator
               | Mil.Prims.PBoolean l                  => logicOp l
               | Mil.Prims.PName n                     => nameOp n
               | Mil.Prims.PCString s                  => stringOp s
               | Mil.Prims.PPtrEq                      => ArAAtoB))

    val vector           : Mil.Prims.vector t =
        (fn v =>
            (case v
              of Mil.Prims.ViPointwise {descriptor, masked, operator}        => prim operator
               | Mil.Prims.ViConvert {to, from}                              => ArOther (1, 1)
               | Mil.Prims.ViCompare {descriptor, typ, operator}             => compareOp operator
               | Mil.Prims.ViReduction {descriptor, associativity, operator} => prim operator
               | Mil.Prims.ViData {descriptor, operator}                     => dataOp descriptor operator
               | Mil.Prims.ViMaskData {descriptor, operator}                 => dataOp descriptor operator
               | Mil.Prims.ViMaskBoolean {descriptor, operator}              => logicOp operator
               | Mil.Prims.ViMaskConvert {to, from}                          => ArOther (1, 1)))

    val runtime          : Mil.Prims.runtime t = 
        (fn r => 
            (case r
	        of Mil.Prims.RtFloatMk              => ArAAtoB
	         | Mil.Prims.RtWriteln              => ArOther (1, 0)
	         | Mil.Prims.RtReadln               => ArOther (0, 1)
	         | Mil.Prims.RtAssert               => ArOther (1, 1)
	         | Mil.Prims.RtError                => ArAtoA
	         | Mil.Prims.RtDebug                => ArOther (1, 0)
	         | Mil.Prims.RtOpenOut              => ArOther (1, 1)
	         | Mil.Prims.RtGetStdout            => ArOther (0, 1)
	         | Mil.Prims.RtOutputByte           => ArOther (2, 0)
	         | Mil.Prims.RtCloseOut             => ArOther (1, 0)
	         | Mil.Prims.RtOpenIn               => ArOther (1, 1)
	         | Mil.Prims.RtGetStdin             => ArOther (0, 1)
	         | Mil.Prims.RtInputByte            => ArAtoA
	         | Mil.Prims.RtInputString          => ArOther (2, 1)
	         | Mil.Prims.RtInputAll             => ArOther (1, 1)
	         | Mil.Prims.RtIsEOF                => ArOther (1, 1)
	         | Mil.Prims.RtCloseIn              => ArOther (1, 0)
	         | Mil.Prims.RtCommandLine          => ArOther (0, 1)
	         | Mil.Prims.RtStringToNat          => ArOther (1, 1)
	         | Mil.Prims.RtStringToFloat        => ArOther (1, 1)
	         | Mil.Prims.RtFloatToString        => ArOther (2, 1)
	         | Mil.Prims.RtFloatToStringI       => ArOther (2, 1)
	         | Mil.Prims.RtRatNumerator         => ArAtoA
	         | Mil.Prims.RtRatDenominator       => ArAtoA
	         | Mil.Prims.RtEqual                => ArAAtoB
	         | Mil.Prims.RtDom                  => ArOther (1, 1)
	         | Mil.Prims.RtNub                  => ArOther (1, 1)
	         | Mil.Prims.RtRatToUIntpChecked    => ArOther (1, 1)
	         | Mil.Prims.RtRatToString          => ArOther (1, 1)
	         | Mil.Prims.RtStringToRat          => ArOther (1, 1)
	         | Mil.Prims.RtResetTimer           => ArOther (1, 0)
	         | Mil.Prims.RtGetTimer             => ArOther (1, 1)
	         | Mil.Prims.RtVtuneAttach          => ArOther (0, 0)
	         | Mil.Prims.RtVtuneDetach          => ArOther (0, 0)
	         | Mil.Prims.RtArrayEval            => ArOther (1, 0)
                 | Mil.Prims.RtIntegerHash          => ArOther (1, 1)))

    val t                : Mil.Prims.t t =
        (fn t => 
            (case t
              of Mil.Prims.Prim p => prim p
               | Mil.Prims.Vector v => vector v
               | Mil.Prims.Runtime r => runtime r))
  end (* structure Arity *)

end (* structure PrimsUtils *)
