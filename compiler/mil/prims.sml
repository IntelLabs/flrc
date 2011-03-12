(* The Intel P to C/Pillar Compiler *)
(* COPYRIGHT_NOTICE_1 *)

(* The primitives we support *)

functor MilPrimsF(type fieldSize) =
struct

  type fieldSize = fieldSize

  datatype vectorSize = Vs64 | Vs128 | Vs256 | Vs512 | Vs1024 

  datatype vectorDescriptor = Vd of {vectorSize : vectorSize, elementSize : fieldSize}

  datatype floatPrecision = FpSingle | FpDouble

  datatype intPrecision = IpArbitrary | IpFixed of IntArb.typ

  datatype numericTyp = NtRat | NtInteger of intPrecision | NtFloat of floatPrecision

  datatype divKind = DkT | DkF | DkE

  datatype arithOp = 
    (* Unary *)
      AAbs | ANegate | ANegateSat
    (* Binary *)
    | ADivide | ADiv of divKind | AMax | AMin | AMinus | AMinusSat 
    | AMod of divKind | APlus | APlusSat | ATimes | ATimesSat
    (* Other *)
    | ADivMod of divKind

  datatype floatOp = 
    (* Unary *)
      FaACos | FaASin | FaCeil | FaCos | FaFloor | FaRcp | FaSin | FaSqrt | FaTan | FaTrunc 
    (* Binary *)
    | FaPow

  datatype bitwiseOp = 
    (* Unary *)
      BNot
    (* Binary *)
    | BAnd | BOr | BRotL | BRotR | BShiftL | BShiftR | BXor

  datatype logicOp = 
    (* Unary *)
      LNot
    (* Binary *)
    | LAnd | LOr | LXor | LEq

  datatype compareOp = CEq | CNe | CLt | CLe

  datatype nameOp = NGetString | NGetHash

  datatype stringOp = SAllocate | SDeallocate | SGetLen | SGetChar | SSetChar | SEqual

  datatype prim =
      PNumArith       of {typ : numericTyp, operator : arithOp}
    | PFloatOp        of {typ : floatPrecision, operator : floatOp}
    | PNumCompare     of {typ : numericTyp, operator : compareOp}
    | PNumConvert     of {to : numericTyp, from : numericTyp}
    | PBitwise        of {typ : intPrecision, operator : bitwiseOp}
    | PBoolean        of logicOp
    | PName           of nameOp
    | PCString        of stringOp
    | PPtrEq

  datatype assoc = ALeft | ARight | AAny

  datatype dataOp = 
      DBroadcast 
    | DVector
    | DSub     of int
    | DPermute of int Vector.t 
    | DBlend 
    | DSplit   (* Split in half *)
    | DConcat  (* Args should have same vectorWidth, result is 2x vectorWidth *)

  datatype vector = 
    (* Pointwise across operands, makes sense for binary or unary ops *)
    (* Boolean indicates takes a mask, when true *)
      ViPointwise   of {descriptor : vectorDescriptor, masked: bool, operator : prim}
    | ViConvert     of {to :   {descriptor : vectorDescriptor, typ : numericTyp}, 
                        from : {descriptor : vectorDescriptor, typ : numericTyp}}
    | ViCompare     of {descriptor : vectorDescriptor, typ : numericTyp, operator : compareOp}
    (* Reduction across the vector with initial value.  Associativity is specified *)
    (* Only makes sense for binary operations, which isn't captured by the syntax. *)
    | ViReduction   of {descriptor : vectorDescriptor, associativity : assoc, operator : prim}
    | ViData        of {descriptor : vectorDescriptor, operator : dataOp}
    | ViMaskData    of {descriptor : vectorDescriptor, operator : dataOp}
    | ViMaskBoolean of {descriptor : vectorDescriptor, operator : logicOp}
    | ViMaskConvert of {to : vectorDescriptor, from : vectorDescriptor}

  datatype runtime =
      RtFloatMk
    | RtWriteln
    | RtReadln
    | RtAssert
    | RtError
    | RtDebug
    | RtOpenOut
    | RtGetStdout
    | RtOutputByte
    | RtCloseOut
    | RtOpenIn
    | RtGetStdin
    | RtInputByte
    | RtInputString
    | RtInputAll
    | RtIsEOF
    | RtCloseIn
    | RtCommandLine
    | RtStringToNat
    | RtStringToFloat
    | RtFloatToString
    | RtFloatToStringI
    | RtRatNumerator
    | RtRatDenominator 
    | RtEqual
    | RtDom
    | RtNub
    | RtRatToUIntpChecked
    | RtRatToString
    | RtStringToRat
    | RtResetTimer
    | RtGetTimer
    | RtVtuneAttach
    | RtVtuneDetach
    | RtArrayEval
    | RtIntegerHash
      
  datatype t =
      Prim    of prim
    | Runtime of runtime
    | Vector  of vector

end (* functor MilPrimsF *)
