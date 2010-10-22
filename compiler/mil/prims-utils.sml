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
    val stringOp         : Mil.Prims.stringOp t
    val prim             : Mil.Prims.prim t
    val assoc            : Mil.Prims.assoc t
    val dataOp           : Mil.Prims.dataOp t
    val vector           : Mil.Prims.vector t
    val runtime          : Mil.Prims.runtime t
    val t                : Mil.Prims.t t
  end (* structure Hash *)

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
    val stringOp         : Mil.Prims.stringOp t
    val prim             : Mil.Prims.prim t
    val dataOp           : Mil.Prims.vectorDescriptor -> Mil.Prims.dataOp t
    val vector           : Mil.Prims.vector t
    val runtime          : Mil.Prims.runtime t
    val t                : Mil.Prims.t t
  end (* structure Arity *)

  structure ToString : 
  sig
    type 'a t = 'a -> string
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
  end (* structure ToString *)

  structure Layout : 
  sig
    type 'a t = 'a -> Layout.t
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

  structure VectorSize : 
  sig
    type t = Mil.Prims.vectorSize
    val compare : t Compare.t
    val eq      : t Eq.t
    val hash    : t Hash.t
    val toString : t -> string
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
    val toString : t -> string
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
      val faMod   : t -> unit option
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
      val pCString    : t -> Mil.Prims.stringOp option
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
                      end) :> PRIMS_UTILS =
struct

  structure Prims = Mil.Prims

  structure IFO = IntFiniteOrdinal

  structure ToString =
  struct
    type 'a t = 'a -> string

    val vectorSize       : Mil.Prims.vectorSize t = 
     fn vs => 
        (case vs
          of Mil.Prims.Vs64   => "64"
           | Mil.Prims.Vs128  => "128"
           | Mil.Prims.Vs256  => "256"
           | Mil.Prims.Vs512  => "512"
           | Mil.Prims.Vs1024 => "1024")

    val vectorDescriptor : Mil.Prims.vectorDescriptor t =
     fn (Mil.Prims.Vd {vectorSize = vs, elementSize}) => 
        vectorSize vs ^ "x" ^ FieldSize.toString elementSize

    val floatPrecision   : Mil.Prims.floatPrecision t = 
     fn fp => 
        (case fp
          of Mil.Prims.FpSingle => "Float"
           | Mil.Prims.FpDouble => "Double")

    val intPrecision     : Mil.Prims.intPrecision t = 
     fn ip => 
        (case ip
          of Mil.Prims.IpArbitrary => "Int"
           | Mil.Prims.IpFixed ia  => IntArb.stringOfTyp ia)

    val numericTyp       : Mil.Prims.numericTyp t = 
     fn p => 
        let
          val res = 
	      case p
	       of Mil.Prims.NtRat        => "Rat"
	        | Mil.Prims.NtInteger r1 => intPrecision r1
	        | Mil.Prims.NtFloat r1   => floatPrecision r1
        in
          res
        end
 
    val divKind          : Mil.Prims.divKind t =
     fn p => 
        let
          val res = 
	      case p
	       of Mil.Prims.DkT    => "T"
	        | Mil.Prims.DkF    => "F"
	        | Mil.Prims.DkE    => "E"
        in
          res
        end
 
    val arithOp          : Mil.Prims.arithOp t =
     fn p => 
        let
          val res = 
	      case p
	       of Mil.Prims.AAbs          => "Abs"
	        | Mil.Prims.ANegate       => "Negate"
	        | Mil.Prims.ANegateSat    => "NegateSat"
	        | Mil.Prims.ADivide       => "Divide"
	        | Mil.Prims.ADiv r1       => "Div" ^ divKind r1
	        | Mil.Prims.AMax          => "Max"
	        | Mil.Prims.AMin          => "Min"
	        | Mil.Prims.AMinus        => "Minus"
	        | Mil.Prims.AMinusSat     => "MinusSat"
	        | Mil.Prims.AMod r1       => "Mod" ^ divKind r1
	        | Mil.Prims.APlus         => "Plus"
	        | Mil.Prims.APlusSat      => "PlusSat"
	        | Mil.Prims.ATimes        => "Times"
	        | Mil.Prims.ATimesSat     => "TimesSat"
	        | Mil.Prims.ADivMod r1    => "DivMod" ^ divKind r1
        in
          res
        end
 
    val floatOp          : Mil.Prims.floatOp t =
     fn p => 
        let
          val res = 
	      case p
	       of Mil.Prims.FaACos     => "ACos"
	        | Mil.Prims.FaASin     => "ASin"
                | Mil.Prims.FaCeil     => "Ceil"
	        | Mil.Prims.FaCos      => "Cos"
	        | Mil.Prims.FaFloor    => "Floor"
	        | Mil.Prims.FaMod      => "Mod"
	        | Mil.Prims.FaRcp      => "Rcp"
	        | Mil.Prims.FaSin      => "Sin"
	        | Mil.Prims.FaSqrt     => "Sqrt"
	        | Mil.Prims.FaTan      => "Tan"
	        | Mil.Prims.FaTrunc    => "Trunc"
	        | Mil.Prims.FaPow      => "Pow"
        in
          res
        end

    val bitwiseOp        : Mil.Prims.bitwiseOp t =
     fn p => 
        let
          val res = 
	      case p
	       of Mil.Prims.BNot       => "BNot"
	        | Mil.Prims.BAnd       => "BAnd"
	        | Mil.Prims.BOr        => "BOr"
	        | Mil.Prims.BRotL      => "BRotL"
	        | Mil.Prims.BRotR      => "BRotR"
	        | Mil.Prims.BShiftL    => "BShiftL"
	        | Mil.Prims.BShiftR    => "BShiftR"
	        | Mil.Prims.BXor       => "BXor"
        in
          res
        end
        
    val logicOp          : Mil.Prims.logicOp t =
     fn p => 
        let
          val res = 
	      case p
	       of Mil.Prims.LNot    => "Not"
	        | Mil.Prims.LAnd    => "And"
	        | Mil.Prims.LOr     => "Or"
	        | Mil.Prims.LXor    => "Xor"
	        | Mil.Prims.LEq     => "Eq"
        in
          res
        end
        
    val compareOp        : Mil.Prims.compareOp t =
     fn p => 
        let
          val res = 
	      case p
	       of Mil.Prims.CEq    => "Eq"
	        | Mil.Prims.CNe    => "Ne"
	        | Mil.Prims.CLt    => "Lt"
	        | Mil.Prims.CLe    => "Le"
        in
          res
        end

    val stringOp         : Mil.Prims.stringOp t =
     fn p => 
        let
          val res = 
	      case p
	       of Mil.Prims.SAllocate      => "Allocate"
	        | Mil.Prims.SDeallocate    => "Deallocate"
	        | Mil.Prims.SGetLen        => "GetLen"
	        | Mil.Prims.SGetChar       => "GetChar"
	        | Mil.Prims.SSetChar       => "SetChar"
	        | Mil.Prims.SEqual         => "Equal"
        in
          res
        end
 
    val prim             : Mil.Prims.prim t =
     fn p => 
        let
          val ipShort = 
           fn ip =>
              (case ip
                of Mil.Prims.IpArbitrary => "I"
                 | Mil.Prims.IpFixed ia  => IntArb.stringOfTypShort ia)
          val fpShort = 
           fn fp => 
              (case fp
                of Mil.Prims.FpSingle => "F"
                 | Mil.Prims.FpDouble => "D")
          val ntShort = 
           fn nt => 
              (case nt
                of Mil.Prims.NtRat          => "R"
                 | Mil.Prims.NtInteger ip   => ipShort ip
                 | Mil.Prims.NtFloat fp     => fpShort fp)
          val res = 
	      case p
	       of Mil.Prims.PNumArith r1   => ntShort (#typ r1) ^ arithOp (#operator r1)
	        | Mil.Prims.PFloatOp r1    => fpShort (#typ r1) ^ floatOp (#operator r1)
	        | Mil.Prims.PNumCompare r1 => ntShort (#typ r1) ^ compareOp (#operator r1)
	        | Mil.Prims.PNumConvert r1 => numericTyp (#from r1) ^ "To" ^ numericTyp (#to r1)
	        | Mil.Prims.PBitwise r1    => ipShort (#typ r1) ^ bitwiseOp (#operator r1)
	        | Mil.Prims.PBoolean r1    => logicOp r1
	        | Mil.Prims.PCString r1    => "CString" ^ stringOp r1
        in
          res
        end
 
    val assoc            : Mil.Prims.assoc t = 
     fn p => 
        let
          val res = 
	      case p
	       of Mil.Prims.ALeft     => "L"
	        | Mil.Prims.ARight    => "R"
	        | Mil.Prims.AAny      => "A"
        in
          res
        end
 
    val dataOp           : Mil.Prims.dataOp t =
     fn p => 
        let
          val intVector = 
           fn iv => 
              let
                val l = LayoutUtils.angleSeq (Vector.toListMap (iv, Int.layout))
                val s = Layout.toString l
              in s
              end
          val res = 
	      case p
	       of Mil.Prims.DBroadcast    => "Broadcast"
	        | Mil.Prims.DVector       => "Vector"
	        | Mil.Prims.DSub r1       => "Sub" ^ Int.toString r1
	        | Mil.Prims.DPermute r1   => "Permute" ^ intVector r1
	        | Mil.Prims.DBlend        => "Blend"
	        | Mil.Prims.DSplit        => "Split"
	        | Mil.Prims.DConcat       => "Concat"
        in
          res
        end
        
    val vector           : Mil.Prims.vector t =
     fn p => 
        let
          val angle = fn s => Layout.toString (LayoutUtils.angleBracket (Layout.str s))
          val square = fn s => Layout.toString (LayoutUtils.bracket (Layout.str s))
          val doOne = 
              fn (prefix, src, tgtO, operator) =>
                 let
                   val from = square (vectorDescriptor src)
                   val to = case tgtO
                             of SOME tgt => square (vectorDescriptor tgt)
                              | NONE => ""
                   val oper = angle operator
                 in prefix ^ from ^ to ^ oper
                 end

          val res = 
	      case p
	       of Mil.Prims.ViPointwise r1   => 
                  let
                    val prefix = if #masked r1 then
                                   "VPointwiseM"
                                 else
                                   "VPointwise"
                  in doOne (prefix, #descriptor r1, NONE, prim (#operator r1))
                  end
	        | Mil.Prims.ViConvert r1     => 
                  let
                    val to = #to r1
                    val from = #from r1
                    val oper = prim (Mil.Prims.PNumConvert {to = #typ to, from = #typ from})
                  in doOne ("VConvert", #descriptor from, SOME (#descriptor to), oper)
                  end
	        | Mil.Prims.ViCompare r1     => 
                  let
                    val oper = prim (Mil.Prims.PNumCompare {typ = #typ r1, operator = #operator r1})
                  in doOne ("VCompare", #descriptor r1, NONE, oper)
                  end
	        | Mil.Prims.ViReduction r1   => 
                  let
                    val prefix = "VReduce" ^ assoc (#associativity r1)
                  in doOne (prefix, #descriptor r1, NONE, prim (#operator r1))
                  end
	        | Mil.Prims.ViData r1        => doOne ("VData", #descriptor r1, NONE, dataOp (#operator r1))
	        | Mil.Prims.ViMaskData r1    => doOne ("VMaskData", #descriptor r1, NONE, dataOp (#operator r1))
	        | Mil.Prims.ViMaskBoolean r1 => doOne ("VMaskBool", #descriptor r1, NONE, logicOp (#operator r1))
	        | Mil.Prims.ViMaskConvert r1 => doOne ("VMaskConvert", #to r1, SOME (#from r1), "")
        in
          res
        end

    val runtime          : Mil.Prims.runtime t =
     fn p => 
        let
          val res = 
	      case p
	       of Mil.Prims.RtFloatMk              => "PFloatMk"
	        | Mil.Prims.RtWriteln              => "PWriteln"
	        | Mil.Prims.RtReadln               => "PReadln"
	        | Mil.Prims.RtAssert               => "PAssert"
	        | Mil.Prims.RtError                => "PError"
	        | Mil.Prims.RtDebug                => "PDebug"
	        | Mil.Prims.RtOpenOut              => "POpenOut"
	        | Mil.Prims.RtGetStdout            => "PGetStdout"
	        | Mil.Prims.RtOutputByte           => "POutputByte"
	        | Mil.Prims.RtCloseOut             => "PCloseOut"
	        | Mil.Prims.RtOpenIn               => "POpenIn"
	        | Mil.Prims.RtGetStdin             => "PGetStdin"
	        | Mil.Prims.RtInputByte            => "PInputByte"
	        | Mil.Prims.RtInputString          => "PInputString"
	        | Mil.Prims.RtInputAll             => "PInputAll"
	        | Mil.Prims.RtIsEOF                => "PIsEOF"
	        | Mil.Prims.RtCloseIn              => "PCloseIn"
	        | Mil.Prims.RtCommandLine          => "PCommandLine"
	        | Mil.Prims.RtStringToNat          => "PStringToNat"
	        | Mil.Prims.RtStringToFloat        => "PStringToFloat"
	        | Mil.Prims.RtFloatToString        => "PFloatToString"
	        | Mil.Prims.RtFloatToStringI       => "PFloatToStringI"
	        | Mil.Prims.RtRatNumerator         => "PRatNumerator"
	        | Mil.Prims.RtRatDenominator       => "PRatDenominator"
	        | Mil.Prims.RtEqual                => "PEqual"
	        | Mil.Prims.RtDom                  => "PDom"
	        | Mil.Prims.RtNub                  => "PNub"
	        | Mil.Prims.RtRatToUIntpChecked    => "PRatToUIntpChecked"
	        | Mil.Prims.RtRatToString          => "PRatToString"
	        | Mil.Prims.RtStringToRat          => "PStringToRat"
	        | Mil.Prims.RtResetTimer           => "PResetTimer"
	        | Mil.Prims.RtGetTimer             => "PGetTimer"
	        | Mil.Prims.RtVtuneAttach          => "PVTuneAttach"
	        | Mil.Prims.RtVtuneDetach          => "PVTuneDetach"
	        | Mil.Prims.RtArrayEval            => "PArrayEval"
        in
          res
        end
 
    val t                : Mil.Prims.t t = 
     fn p => 
        let
          val res = 
	      case p
	       of Mil.Prims.Prim r1    => prim r1
	        | Mil.Prims.Runtime r1 => runtime r1
	        | Mil.Prims.Vector r1  => vector r1
        in
          res
        end

  end (* structure ToString *)

  structure Layout =
  struct
    type 'a t = 'a -> Layout.t
    val vectorSize       : Mil.Prims.vectorSize t       = Layout.str o ToString.vectorSize
    val vectorDescriptor : Mil.Prims.vectorDescriptor t = Layout.str o ToString.vectorDescriptor
    val floatPrecision   : Mil.Prims.floatPrecision t   = Layout.str o ToString.floatPrecision
    val intPrecision     : Mil.Prims.intPrecision t     = Layout.str o ToString.intPrecision
    val numericTyp       : Mil.Prims.numericTyp t       = Layout.str o ToString.numericTyp
    val divKind          : Mil.Prims.divKind t          = Layout.str o ToString.divKind
    val arithOp          : Mil.Prims.arithOp t          = Layout.str o ToString.arithOp
    val floatOp          : Mil.Prims.floatOp t          = Layout.str o ToString.floatOp
    val bitwiseOp        : Mil.Prims.bitwiseOp t        = Layout.str o ToString.bitwiseOp
    val logicOp          : Mil.Prims.logicOp t          = Layout.str o ToString.logicOp
    val compareOp        : Mil.Prims.compareOp t        = Layout.str o ToString.compareOp
    val stringOp         : Mil.Prims.stringOp t         = Layout.str o ToString.stringOp
    val prim             : Mil.Prims.prim t             = Layout.str o ToString.prim
    val assoc            : Mil.Prims.assoc t            = Layout.str o ToString.assoc
    val dataOp           : Mil.Prims.dataOp t           = Layout.str o ToString.dataOp
    val vector           : Mil.Prims.vector t           = Layout.str o ToString.vector
    val runtime          : Mil.Prims.runtime t          = Layout.str o ToString.runtime
    val t                : Mil.Prims.t t                = Layout.str o ToString.t
  end (* structure Layout *)

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
                 | Prims.FaMod   => 5
                 | Prims.FaRcp   => 6
                 | Prims.FaSin   => 7
                 | Prims.FaSqrt  => 8
                 | Prims.FaTan   => 9
                 | Prims.FaTrunc => 10
                 | Prims.FaPow   => 11)
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
                 | Prims.PCString s                  => IFO.shift (6, stringOp s))
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
                 | Prims.RtArrayEval         => 34)
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
                     | Mil.Prims.PCString s                  => stringOp s)
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
                     | Mil.Prims.RtArrayEval         => any)
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
    structure Dec = 
    struct
      val vs64   = fn ve => (case ve of Mil.Prims.Vs64 => SOME () | _ => NONE)
      val vs128  = fn ve => (case ve of Mil.Prims.Vs128 => SOME () | _ => NONE)
      val vs256  = fn ve => (case ve of Mil.Prims.Vs256 => SOME () | _ => NONE)
      val vs512  = fn ve => (case ve of Mil.Prims.Vs512 => SOME () | _ => NONE)
      val vs1024 = fn ve => (case ve of Mil.Prims.Vs1024 => SOME () | _ => NONE)
    end (* structure Dec *)
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
    structure Dec = 
    struct
      val vd = fn ve => (case ve of Mil.Prims.Vd r => SOME r)
    end (* structure Dec *)
  end (* structure VectorDescriptor *)

  structure FloatPrecision = 
  struct
    type t = Mil.Prims.floatPrecision
    val compare = Compare.floatPrecision
    val eq      = Eq.floatPrecision
    val hash    = Hash.floatPrecision
    structure Dec = 
    struct
      val fpSingle = fn fl => (case fl of Mil.Prims.FpSingle => SOME () | _ => NONE)
      val fpDouble = fn fl => (case fl of Mil.Prims.FpDouble => SOME () | _ => NONE)
    end (* structure Dec *)
  end (* structure FloatPrecision *)

  structure IntPrecision = 
  struct
    type t = Mil.Prims.intPrecision
    val compare = Compare.intPrecision
    val eq      = Eq.intPrecision
    val hash    = Hash.intPrecision
    structure Dec = 
    struct
      val ipArbitrary = fn ip => (case ip of Mil.Prims.IpArbitrary => SOME () | _ => NONE)
      val ipFixed     = fn ip => (case ip of Mil.Prims.IpFixed r => SOME r | _ => NONE)
    end (* structure Dec *)
  end (* structure IntPrecision *)

  structure NumericTyp = 
  struct
    type t = Mil.Prims.numericTyp
    val compare = Compare.numericTyp
    val eq      = Eq.numericTyp
    val hash    = Hash.numericTyp
                     
    structure Dec = 
    struct
      val ntRat     = fn nu => (case nu of Mil.Prims.NtRat => SOME () | _ => NONE)
      val ntInteger = fn nu => (case nu of Mil.Prims.NtInteger r => SOME r | _ => NONE)
      val ntFloat   = fn nu => (case nu of Mil.Prims.NtFloat r => SOME r | _ => NONE)
    end (* structure Dec *)
  end (* structure NumericTyp *)

  structure DivKind = 
  struct
    type t = Mil.Prims.divKind
    val compare = Compare.divKind
    val eq      = Eq.divKind
    val hash    = Hash.divKind
    structure Dec = 
    struct
      val dkT = fn di => (case di of Mil.Prims.DkT => SOME () | _ => NONE)
      val dkF = fn di => (case di of Mil.Prims.DkF => SOME () | _ => NONE)
      val dkE = fn di => (case di of Mil.Prims.DkE => SOME () | _ => NONE)
    end (* structure Dec *)
  end (* structure DivKind *)

  structure ArithOp = 
  struct
    type t = Mil.Prims.arithOp
    val compare = Compare.arithOp
    val eq      = Eq.arithOp
    val hash    = Hash.arithOp
    structure Dec = 
    struct
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
    end (* structure Dec *)
  end (* structure ArithOp *)

  structure FloatOp = 
  struct
    type t = Mil.Prims.floatOp
    val compare = Compare.floatOp
    val eq      = Eq.floatOp
    val hash    = Hash.floatOp
    structure Dec = 
    struct
      val faASin  = fn fl => (case fl of Mil.Prims.FaACos => SOME () | _ => NONE)
      val faACos  = fn fl => (case fl of Mil.Prims.FaASin => SOME () | _ => NONE)
      val faCeil  = fn fl => (case fl of Mil.Prims.FaCeil => SOME () | _ => NONE)
      val faCos   = fn fl => (case fl of Mil.Prims.FaCos => SOME () | _ => NONE)
      val faFloor = fn fl => (case fl of Mil.Prims.FaFloor => SOME () | _ => NONE)
      val faMod   = fn fl => (case fl of Mil.Prims.FaMod => SOME () | _ => NONE)
      val faRcp   = fn fl => (case fl of Mil.Prims.FaRcp => SOME () | _ => NONE)
      val faSin   = fn fl => (case fl of Mil.Prims.FaSin => SOME () | _ => NONE)
      val faSqrt  = fn fl => (case fl of Mil.Prims.FaSqrt => SOME () | _ => NONE)
      val faTan   = fn fl => (case fl of Mil.Prims.FaTan => SOME () | _ => NONE)
      val faTrunc = fn fl => (case fl of Mil.Prims.FaTrunc => SOME () | _ => NONE)
      val faPow   = fn fl => (case fl of Mil.Prims.FaPow => SOME () | _ => NONE)
    end (* structure Dec *)
  end (* structure FloatOp *)

  structure BitwiseOp = 
  struct
    type t = Mil.Prims.bitwiseOp
    val compare = Compare.bitwiseOp
    val eq      = Eq.bitwiseOp
    val hash    = Hash.bitwiseOp
    structure Dec = 
    struct
      val bNot    = fn bi => (case bi of Mil.Prims.BNot => SOME () | _ => NONE)
      val bAnd    = fn bi => (case bi of Mil.Prims.BAnd => SOME () | _ => NONE)
      val bOr     = fn bi => (case bi of Mil.Prims.BOr => SOME () | _ => NONE)
      val bRotL   = fn bi => (case bi of Mil.Prims.BRotL => SOME () | _ => NONE)
      val bRotR   = fn bi => (case bi of Mil.Prims.BRotR => SOME () | _ => NONE)
      val bShiftL = fn bi => (case bi of Mil.Prims.BShiftL => SOME () | _ => NONE)
      val bShiftR = fn bi => (case bi of Mil.Prims.BShiftR => SOME () | _ => NONE)
      val bXor    = fn bi => (case bi of Mil.Prims.BXor => SOME () | _ => NONE)
    end (* structure Dec *)
  end (* structure BitwiseOp *)

  structure LogicOp = 
  struct
    type t = Mil.Prims.logicOp
    val compare = Compare.logicOp
    val eq      = Eq.logicOp
    val hash    = Hash.logicOp
    structure Dec = 
    struct
      val lNot = fn lo => (case lo of Mil.Prims.LNot => SOME () | _ => NONE)
      val lAnd = fn lo => (case lo of Mil.Prims.LAnd => SOME () | _ => NONE)
      val lOr  = fn lo => (case lo of Mil.Prims.LOr => SOME () | _ => NONE)
      val lXor = fn lo => (case lo of Mil.Prims.LXor => SOME () | _ => NONE)
      val lEq  = fn lo => (case lo of Mil.Prims.LEq => SOME () | _ => NONE)
    end (* structure Dec *)
  end (* structure LogicOp *)

  structure CompareOp = 
  struct
    type t = Mil.Prims.compareOp
    val compare = Compare.compareOp
    val eq      = Eq.compareOp
    val hash    = Hash.compareOp
    structure Dec = 
    struct
      val cEq = fn co => (case co of Mil.Prims.CEq => SOME () | _ => NONE)
      val cNe = fn co => (case co of Mil.Prims.CNe => SOME () | _ => NONE)
      val cLt = fn co => (case co of Mil.Prims.CLt => SOME () | _ => NONE)
      val cLe = fn co => (case co of Mil.Prims.CLe => SOME () | _ => NONE)
    end (* structure Dec *)
  end (* structure CompareOp *)

  structure StringOp = 
  struct
    type t = Mil.Prims.stringOp
    val compare = Compare.stringOp
    val eq      = Eq.stringOp
    val hash    = Hash.stringOp
    structure Dec = 
    struct
      val sAllocate   = fn st => (case st of Mil.Prims.SAllocate => SOME () | _ => NONE)
      val sDeallocate = fn st => (case st of Mil.Prims.SDeallocate => SOME () | _ => NONE)
      val sGetLen     = fn st => (case st of Mil.Prims.SGetLen => SOME () | _ => NONE)
      val sGetChar    = fn st => (case st of Mil.Prims.SGetChar => SOME () | _ => NONE)
      val sSetChar    = fn st => (case st of Mil.Prims.SSetChar => SOME () | _ => NONE)
      val sEqual      = fn st => (case st of Mil.Prims.SEqual => SOME () | _ => NONE)
    end (* structure Dec *)
  end (* structure StringOp *)

  structure Prim = 
  struct
    type t = Mil.Prims.prim
    val compare = Compare.prim
    val eq      = Eq.prim
    val hash    = Hash.prim
    structure Dec = 
    struct
      val pNumArith   = fn pr => (case pr of Mil.Prims.PNumArith r => SOME r | _ => NONE)
      val pFloatOp    = fn pr => (case pr of Mil.Prims.PFloatOp r => SOME r | _ => NONE)
      val pNumCompare = fn pr => (case pr of Mil.Prims.PNumCompare r => SOME r | _ => NONE)
      val pNumConvert = fn pr => (case pr of Mil.Prims.PNumConvert r => SOME r | _ => NONE)
      val pBitwise    = fn pr => (case pr of Mil.Prims.PBitwise r => SOME r | _ => NONE)
      val pBoolean    = fn pr => (case pr of Mil.Prims.PBoolean r => SOME r | _ => NONE)
      val pCString    = fn pr => (case pr of Mil.Prims.PCString r => SOME r | _ => NONE)
    end (* structure Dec *)
  end (* structure Prim *)

  structure Assoc = 
  struct
    type t = Mil.Prims.assoc
    val compare = Compare.assoc
    val eq      = Eq.assoc
    val hash    = Hash.assoc
    structure Dec = 
    struct
      val aLeft  = fn a => (case a of Mil.Prims.ALeft => SOME () | _ => NONE)
      val aRight = fn a => (case a of Mil.Prims.ARight => SOME () | _ => NONE)
      val aAny   = fn a => (case a of Mil.Prims.AAny => SOME () | _ => NONE)
    end (* structure Dec *)
  end (* structure Assoc *)

  structure DataOp = 
  struct
    type t = Mil.Prims.dataOp
    val compare = Compare.dataOp
    val eq      = Eq.dataOp
    val hash    = Hash.dataOp
    structure Dec = 
    struct
      val dBroadcast = fn da => (case da of Mil.Prims.DBroadcast => SOME () | _ => NONE)
      val dVector    = fn da => (case da of Mil.Prims.DVector => SOME () | _ => NONE)
      val dSub       = fn da => (case da of Mil.Prims.DSub r => SOME r | _ => NONE)
      val dPermute   = fn da => (case da of Mil.Prims.DPermute r => SOME r | _ => NONE)
      val dBlend     = fn da => (case da of Mil.Prims.DBlend => SOME () | _ => NONE)
      val dSplit     = fn da => (case da of Mil.Prims.DSplit => SOME () | _ => NONE)
      val dConcat    = fn da => (case da of Mil.Prims.DConcat => SOME () | _ => NONE)
    end (* structure Dec *)
  end (* structure DataOp *)

  structure Vector = 
  struct
    type t = Mil.Prims.vector
    val compare = Compare.vector
    val eq      = Eq.vector
    val hash    = Hash.vector
    structure Dec = 
    struct
      val viPointwise   = fn ve => (case ve of Mil.Prims.ViPointwise r => SOME r | _ => NONE)
      val viConvert     = fn ve => (case ve of Mil.Prims.ViConvert r => SOME r | _ => NONE)
      val viCompare     = fn ve => (case ve of Mil.Prims.ViCompare r => SOME r | _ => NONE)
      val viReduction   = fn ve => (case ve of Mil.Prims.ViReduction r => SOME r | _ => NONE)
      val viData        = fn ve => (case ve of Mil.Prims.ViData r => SOME r | _ => NONE)
      val viMaskData    = fn ve => (case ve of Mil.Prims.ViMaskData r => SOME r | _ => NONE)
      val viMaskBoolean = fn ve => (case ve of Mil.Prims.ViMaskBoolean r => SOME r | _ => NONE)
      val viMaskConvert = fn ve => (case ve of Mil.Prims.ViMaskConvert r => SOME r | _ => NONE)
    end (* structure Dec *)
  end (* structure Vector *)

  structure Runtime = 
  struct
    type t = Mil.Prims.runtime
    val compare = Compare.runtime
    val eq      = Eq.runtime
    val hash    = Hash.runtime
    structure Dec = 
    struct
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
    end (* structure Dec *)
  end (* structure Runtime *)

  structure T = 
  struct
    type t = Mil.Prims.t
    val compare = Compare.t
    val eq      = Eq.t
    val hash    = Hash.t
    structure Dec = 
    struct
      val prim    = fn t => (case t of Mil.Prims.Prim r => SOME r | _ => NONE)
      val runtime = fn t => (case t of Mil.Prims.Runtime r => SOME r | _ => NONE)
      val vector  = fn t => (case t of Mil.Prims.Vector r => SOME r | _ => NONE)
    end (* structure Dec *)
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
               | Prims.FaMod   => ArAAtoA
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
               | Mil.Prims.PCString s                  => stringOp s))

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
	         | Mil.Prims.RtArrayEval            => ArOther (1, 0)))

    val t                : Mil.Prims.t t =
        (fn t => 
            (case t
              of Mil.Prims.Prim p => prim p
               | Mil.Prims.Vector v => vector v
               | Mil.Prims.Runtime r => runtime r))
  end (* structure Arity *)

end (* structure PrimsUtils *)
