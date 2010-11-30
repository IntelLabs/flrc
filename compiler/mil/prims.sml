(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

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
      
  datatype t =
      Prim    of prim
    | Runtime of runtime
    | Vector  of vector

end (* functor MilPrimsF *)
                               




(*
This table describes how to compute the type of the primitive (it does
not descripe the full checking algorithm).


Base primitives

PNumArith {typ, operator} when operator is unary
  : Mil.TNumeric typ => Mil.TNumeric typ

PNumArith {typ, operator} when operator is binary
  : Mil.TNumeric typ x Mil.TNumeric typ => Mil.TNumeric typ

PNumArith {typ, ADivMod} when operator is 
  : Mil.TNumeric typ x Mil.TNumeric typ => Mil.TNumeric typ x Mil.TNumeric typ 

PFloatOp {typ, operator} when operator is unary
  : Mil.TNumeric (NtFloat typ) => Mil.TNumeric (NtFloat typ)

PFloatOp {typ, operator} when operator is binary
  : Mil.TNumeric (NtFloat typ) x Mil.TNumeric (NtFloat typ) => Mil.TNumeric (NtFloat typ)

PFloatOp {typ, operator} when operator is binary
  : Mil.TNumeric (NtFloat typ) x Mil.TNumeric (NtFloat typ) => Mil.TNumeric (NtFloat typ)

PCompare {typ, operator} 
  : Mil.TPrim typ x Mil.TPrim typ => Mil.TBoolean

PNumConvert {to, from} 
  : Mil.TNumeric from => Mil.TNumeric to

PBitWise {typ, operator} when operator is unary
  : Mil.TNumeric (NtInteger typ) => Mil.TNumeric (NtInteger typ)

PBitWise {typ, operator} when operator is binary
  : Mil.TNumeric (NtInteger typ) x Mil.TNumeric (NtInteger typ) => Mil.TNumeric (NtInteger typ)

PBoolean logic when logic is unary
  : Mil.TBoolean => Mil.TBoolean

PBoolean logic when logic is binary
  : Mil.TBoolean x Mil.TBoolean => Mil.TBoolean

PRatNumerator 
  : Mil.TNumeric NtRat => Mil.TNumeric NtRat

PRatDenominator
  : Mil.TNumeric NtRat => Mil.TNumeric NtRat

PEqual
  : Mil.TAny * Mil.TAny => Mil.TAny

PDom
  : array TAny => TPType TAny

PNub 
  : TPType TAny => TRat

PRatTOUintpChecked 
  : TRat => uintp

PCString SAllocate 
  : uintp => TCstring

PCString SDeallocate
  : TCstring => ()

PCString SGetLen
  : TCstring => uintp

PCString SGetChar
  : TCstring * uintp => uint8

PCString SSetChar
  : TCstring * uintp * int8 => ()

PCString SEqual
  : TCstring * TCString => TBoolean


Vector typing:

ViPointwise {descriptor, masked = false, operator} 
  : vector{size, t1} x vector {size, t2} => vector{size, t3}
  where size is #vectorSize descriptor 
        operator : t1 x t2 => t3
        fieldSizeOf(t1) = fieldSizeOf(t2) = fieldSizeOf(t3) = #vectorSize descriptor

ViPointwise {descriptor, masked = true, operator} 
  : vector{size, t1} x vector {size, t2} x mask {size, fs} => vector{size, t3}
  where size is #vectorSize descriptor 
        operator : t1 x t2 => t3
        fieldSizeOf(t1) = fieldSizeOf(t2) = fieldSizeOf(t3) = fs = #vectorSize descriptor

ViConvert {to, from, operator}
  : vector{size1, t1} => vector{size2, t2}
  where size1 is #vectorSize from
        size2 is #vectorSize to
        operator : t1 => t2
        fieldSizeOf(t1) = #elementSize from
        fieldSizeOf(t2) = #elementSize to

ViCompare {descriptor, typ, operator}
  : vector {size, t1} x vector {size, t1} => mask {size, fs}
  where size is #vectorSize descriptor
        t1 is TNumeric typ
        fs = fieldSizeOf(t1) = #elementSize descriptor

ViReduction {descriptor, associativity, operator}
  : t1 x vector {size, t1} => t1
  where size is #vectorSize descriptor
        operator is t1 x t1 => t1
        fieldSizeOf(t1) is #elementSize descriptor


I use vector/mask to indicate the appropriate constructor based on the 
vector data kind.

vector/mask {size, kind, typ} = vector {size, typ} when kind is VkVector typ
vector/mask {size, kind, typ} = mask {size, fieldSizeFromTyp typ} when kind is VkMask
typeOfVdk (VkMask) = TBoolean
typeOfVdk (VkVector typ) = typ


ViData {descriptor, kind, DBroadcast}
  : typ => vector/mask {size, kind, typ}
  where typ = typeOfVdk kind
        fieldSizeOf(typ) = #elementSize descriptor

ViData {descriptor, kind, DVector}
  : typ^n => vector/mask {size, kind, typ}
  where typ = typeOfVdk kind
        fieldSizeOf(typ) = #elementSize descriptor
        size = n = #vectorSize descriptor/#elementSize descriptor

ViData {descriptor, kind, DSub}
  : vector/mask {size, kind, typ} => typ
  where typ = typeOfVdk kind
        fieldSizeOf(typ) = #elementSize descriptor
        size = #vectorSize descriptor

ViData {descriptor, kind, DPermute is}
  : vector/mask {size, typ} => vector/mask {size, typ}
  where typ = typeOfVdk kind
        fieldSizeOf(typ) = #elementSize descriptor
        size = #vectorSize descriptor
        count = size/#elementSize descriptor
        length(is) = count, is = 0, ..., count-1

ViData {descriptor, kind, DBlend}
  : vector/mask {size, typ} * vector/mask {size, typ} * mask {size, fs} => vector/mask {size, typ}
  where typ = typeOfVdk kind
        fieldSizeOf(typ) = fs = #elementSize descriptor
        size = #vectorSize descriptor

ViData {descriptor, kind, DSplit}
  : vector/mask {size1, typ} => vector/mask {size2, typ} x vector/mask {size2, typ}
  where typ = typeOfVdk kind
        fieldSizeOf(typ) = #elementSize descriptor
        size1 = #vectorSize descriptor
        size2 = size1/2

ViData {descriptor, kind, DConcat}
  : vector/mask {size1, typ} x vector/mask {size1, typ} => vector/mask {size2, typ}
  where typ = typeOfVdk kind
        fieldSizeOf(typ) = #elementSize descriptor
        size1 = #vectorSize descriptor
        size2 = size1*2

ViMaskBoolean {descriptor, operator} when operator is unary
  : mask {size, fs} => mask {size, fs}
  where size = #vectorSize descriptor
        fs = #elementSize descriptor

ViMaskBoolean {descriptor, operator} when operator is binary
  : mask {size, fs} x mask {size, fs} => mask {size, fs}
  where size = #vectorSize descriptor
        fs = #elementSize descriptor

ViMaskConvert {to, from}
  : mask {size1, fs1} => mask {size2, fs2}
  where size1 = #vectorSize from
        fs1 = #elementSize from
        size2 = #vectorSize to
        fs2 = #elementSize to

*)
