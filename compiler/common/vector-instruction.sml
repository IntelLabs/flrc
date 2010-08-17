(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

signature VECTOR_INSTRUCTIONS =
sig

  type size = Config.vectorSize

  datatype elemType =
      ViUInt8   | ViUInt16  | ViUInt32 | ViUInt64
    | ViSInt8   | ViSInt16  | ViSInt32 | ViSInt64
    | ViFloat16 | ViFloat32 | ViFloat64
   
  datatype prim =
         (* bitwise ops per element *)
           ViShiftL of elemType (* shift logical *)
         | ViShiftA of elemType (* shift arithmetic *)  
         | ViRotateL of elemType (* rotate left *)
         | ViRotateR of elemType (* rotate right *)
         | ViBitNot of elemType
         | ViBitAnd of elemType
         | ViBitXor of elemType 
         | ViBitOr of elemType 
         (* logical ops per element *)
         | ViNot of elemType
         | ViAnd of elemType
         | ViOr of elemType
         | ViMaskNot of elemType
         | ViMaskAnd of elemType  
         | ViMaskOr of elemType  
         (* two-source ops*)
         | ViAdd of elemType 
         | ViSub of elemType
         | ViMul of elemType
         | ViDiv of elemType
         | ViMod of elemType
         | ViFma of elemType
         | ViFms of elemType
         | ViMax of elemType
         | ViMin of elemType
         (* one-source ops*)
         | ViNeg of elemType
         | ViSqrt of elemType
         | ViSqrtRcp of elemType
         | ViRcp of elemType
         | ViExp2 of elemType
         | ViExp2m1 of elemType (* exp2 (x-1) *)
         | ViLog2 of elemType
         | ViLog2p1 of elemType (* log2 (x+1) *)
         | ViSin of elemType
         | ViAsin of elemType
         | ViCos of elemType
         | ViAcos of elemType
         | ViTan of elemType
         | ViAtan of elemType
         | ViSign of elemType
         | ViAbs of elemType
         (* compare ops*)
         | ViEq of elemType
         | ViNe of elemType
         | ViGt of elemType
         | ViGe of elemType
         | ViLt of elemType
         | ViLe of elemType
         (* misc *)
         | ViSelect of elemType
         | ViPermute of elemType * int Vector.t
         | ViInit of elemType 

  datatype t = TVector of elemType | TMask of elemType
  val typeOf : prim -> t list * t

  val numElemTypeBits : elemType -> int
  val numElemTypeBytes : elemType -> int

  val numSizeBits : size -> int
  val numSizeBytes : size -> int

  val numElems : size * elemType -> int
  val numMaskBytes : size * elemType -> int

  val stringOfSize : size -> string
  val stringOfElemType : elemType -> string
  val stringOfElemTypeShort : elemType -> string
  val stringOfPrim : prim -> string

  val layoutSize : size -> Layout.t
  val layoutElemType : elemType -> Layout.t
  val layoutElemTypeShort : elemType -> Layout.t
  val layoutPrim : prim -> Layout.t

  structure Compare :
  sig
    val elemType : elemType Compare.t
    val prim : prim Compare.t
  end

  val equalElemTypes : elemType * elemType -> bool

end (* signature VECTOR_INSTRUCTIONS *);

structure VectorInstructions :> VECTOR_INSTRUCTIONS =
struct

  type size = Config.vectorSize

  datatype elemType =
      ViUInt8   | ViUInt16  | ViUInt32 | ViUInt64
    | ViSInt8   | ViSInt16  | ViSInt32 | ViSInt64
    | ViFloat16 | ViFloat32 | ViFloat64
   
  datatype prim =
         (* bitwise ops per element *)
           ViShiftL of elemType (* shift logical *)
         | ViShiftA of elemType (* shift arithmetic *)  
         | ViRotateL of elemType (* rotate left *)
         | ViRotateR of elemType (* rotate right *)
         | ViBitNot of elemType
         | ViBitAnd of elemType
         | ViBitXor of elemType 
         | ViBitOr of elemType 
         (* logical ops per element *)
         | ViNot of elemType
         | ViAnd of elemType
         | ViOr of elemType
         | ViMaskNot of elemType
         | ViMaskAnd of elemType  
         | ViMaskOr of elemType  
         (* two-source ops*)
         | ViAdd of elemType 
         | ViSub of elemType
         | ViMul of elemType
         | ViDiv of elemType
         | ViMod of elemType
         | ViFma of elemType
         | ViFms of elemType
         | ViMax of elemType
         | ViMin of elemType
         (* one-source ops*)
         | ViNeg of elemType
         | ViSqrt of elemType
         | ViSqrtRcp of elemType
         | ViRcp of elemType
         | ViExp2 of elemType
         | ViExp2m1 of elemType (* exp2 (x-1) *)
         | ViLog2 of elemType
         | ViLog2p1 of elemType (* log2 (x+1) *)
         | ViSin of elemType
         | ViAsin of elemType
         | ViCos of elemType
         | ViAcos of elemType
         | ViTan of elemType
         | ViAtan of elemType
         | ViSign of elemType
         | ViAbs of elemType
         (* compare ops*)
         | ViEq of elemType
         | ViNe of elemType
         | ViGt of elemType
         | ViGe of elemType
         | ViLt of elemType
         | ViLe of elemType
         (* misc *)
         | ViSelect of elemType
         | ViPermute of elemType * int Vector.t
         (* init *)
         | ViInit of elemType

  datatype t = TVector of elemType | TMask of elemType

  fun typeOf prim = 
      case prim
       of  ViShiftL et  => ([TVector et, TVector et], TVector et)
         | ViShiftA et  => ([TVector et, TVector et], TVector et)
         | ViRotateL et => ([TVector et, TVector et], TVector et)
         | ViRotateR et => ([TVector et, TVector et], TVector et)
         | ViBitNot et  => ([TVector et, TVector et], TVector et)
         | ViBitAnd et  => ([TVector et, TVector et], TVector et)
         | ViBitXor et  => ([TVector et, TVector et], TVector et)
         | ViBitOr et   => ([TVector et, TVector et], TVector et) 
         | ViNot et     => ([TVector et, TVector et], TVector et)
         | ViAnd et     => ([TVector et, TVector et], TVector et)
         | ViOr et      => ([TVector et, TVector et], TVector et)
         | ViMaskNot et => ([TVector et, TVector et], TVector et)
         | ViMaskAnd et => ([TVector et, TVector et], TVector et)  
         | ViMaskOr et  => ([TVector et, TVector et], TVector et)  
         | ViAdd et     => ([TVector et, TVector et], TVector et) 
         | ViSub et     => ([TVector et, TVector et], TVector et)
         | ViMul et     => ([TVector et, TVector et], TVector et)
         | ViDiv et     => ([TVector et, TVector et], TVector et)
         | ViMod et     => ([TVector et, TVector et], TVector et)
         | ViFma et     => ([TVector et, TVector et], TVector et)
         | ViFms et     => ([TVector et, TVector et], TVector et)
         | ViMax et     => ([TVector et, TVector et], TVector et)
         | ViMin et     => ([TVector et, TVector et], TVector et)
         | ViNeg et     => ([TVector et            ], TVector et)
         | ViSqrt et    => ([TVector et, TVector et], TVector et)
         | ViSqrtRcp et => ([TVector et, TVector et], TVector et)
         | ViRcp et     => ([TVector et, TVector et], TVector et)
         | ViExp2 et    => ([TVector et, TVector et], TVector et)
         | ViExp2m1 et  => ([TVector et, TVector et], TVector et)
         | ViLog2 et    => ([TVector et, TVector et], TVector et)
         | ViLog2p1 et  => ([TVector et, TVector et], TVector et)
         | ViSin et     => ([TVector et, TVector et], TVector et)
         | ViAsin et    => ([TVector et, TVector et], TVector et)
         | ViCos et     => ([TVector et, TVector et], TVector et)
         | ViAcos et    => ([TVector et, TVector et], TVector et)
         | ViTan et     => ([TVector et, TVector et], TVector et)
         | ViAtan et    => ([TVector et, TVector et], TVector et)
         | ViSign et    => ([TVector et, TVector et], TVector et)
         | ViAbs et     => ([TVector et, TVector et], TVector et)
         | ViEq et      => ([TVector et, TVector et], TVector et)
         | ViNe et      => ([TVector et, TVector et], TVector et)
         | ViGt et      => ([TVector et, TVector et], TVector et)
         | ViGe et      => ([TVector et, TVector et], TVector et)
         | ViLt et      => ([TVector et, TVector et], TVector et)
         | ViLe et      => ([TVector et, TVector et], TVector et)
         | ViSelect et  => ([TVector et, TVector et], TVector et)
         | ViPermute (et,  _) => ([TVector et, TVector et], TVector et)
         | ViInit et    => ([TVector et            ], TVector et)

  fun numElemTypeBytes vet =
      case vet
       of ViUInt8   => 1
        | ViUInt16  => 2
        | ViUInt32  => 4
        | ViUInt64  => 8
        | ViSInt8   => 1
        | ViSInt16  => 2
        | ViSInt32  => 4
        | ViSInt64  => 8
        | ViFloat16 => 2
        | ViFloat32 => 4
        | ViFloat64 => 8

  fun numElemTypeBits vet = 8 * numElemTypeBytes vet

  fun numSizeBytes vs =
      case vs
       of Config.Vs128 => 16
        | Config.Vs256 => 32
        | Config.Vs512 => 64

  fun numSizeBits vs = 8 * numSizeBytes vs

  fun numElems (vs, vet) = Int.div (numSizeBytes vs, numElemTypeBytes vet)

  fun numMaskBytes (vs, vet) = Int.div (numElems (vs, vet) + 7, 8)

  fun stringOfSize vs =
      case vs
       of Config.Vs128 => "VS128"
        | Config.Vs256 => "VS256"
        | Config.Vs512 => "VS512"

  fun stringOfElemType t =
      case t
       of ViUInt8   => "ViUInt8"
        | ViUInt16  => "ViUInt16"
        | ViUInt32  => "ViUInt32"
        | ViUInt64  => "ViUInt64"
        | ViSInt8   => "ViSInt8"
        | ViSInt16  => "ViSInt16"
        | ViSInt32  => "ViSInt32"
        | ViSInt64  => "ViSInt64"
        | ViFloat16 => "ViFloat16"
        | ViFloat32 => "ViFloat32"
        | ViFloat64 => "ViFloat64"

  fun stringOfElemTypeShort t =
      case t
       of ViUInt8   => "U8"
        | ViUInt16  => "U16"
        | ViUInt32  => "U32"
        | ViUInt64  => "U64"
        | ViSInt8   => "S8"
        | ViSInt16  => "S16"
        | ViSInt32  => "S32"
        | ViSInt64  => "S64"
        | ViFloat16 => "F16"
        | ViFloat32 => "F32"
        | ViFloat64 => "F64"

  fun stringOfPrim p = 
      case p
       of ViShiftL et    => "ViShiftL" ^ stringOfElemTypeShort et
         | ViShiftA et   => "ViShiftA" ^ stringOfElemTypeShort et
         | ViRotateL et  => "ViRotateL" ^ stringOfElemTypeShort et
         | ViRotateR et  => "ViRotateR" ^ stringOfElemTypeShort et
         | ViBitNot et   => "ViBitNot" ^ stringOfElemTypeShort et
         | ViBitAnd et   => "ViBitAnd" ^ stringOfElemTypeShort et
         | ViBitXor et   => "ViBitXor" ^ stringOfElemTypeShort et
         | ViBitOr et    => "ViBitOr" ^ stringOfElemTypeShort et
         | ViNot et      => "ViNot" ^ stringOfElemTypeShort et
         | ViAnd et      => "ViAnd" ^ stringOfElemTypeShort et
         | ViOr et       => "ViOr" ^ stringOfElemTypeShort et
         | ViMaskNot mt  => "ViMaskNot" ^ stringOfElemTypeShort mt
         | ViMaskAnd mt  => "ViMaskAnd" ^ stringOfElemTypeShort mt
         | ViMaskOr mt   => "ViMaskOr" ^ stringOfElemTypeShort mt
         | ViAdd et      => "ViAdd" ^ stringOfElemTypeShort et
         | ViSub et      => "ViSub" ^ stringOfElemTypeShort et
         | ViMul et      => "ViMul" ^ stringOfElemTypeShort et
         | ViDiv et      => "ViDiv" ^ stringOfElemTypeShort et
         | ViMod et      => "ViMod" ^ stringOfElemTypeShort et
         | ViFma et      => "ViFma" ^ stringOfElemTypeShort et
         | ViFms et      => "ViFms" ^ stringOfElemTypeShort et
         | ViMax et      => "ViMax" ^ stringOfElemTypeShort et
         | ViMin et      => "ViMin" ^ stringOfElemTypeShort et
         | ViNeg et      => "ViNeg" ^ stringOfElemTypeShort et
         | ViSqrt et     => "ViSqrt" ^ stringOfElemTypeShort et
         | ViSqrtRcp et  => "ViSqrtRcp" ^ stringOfElemTypeShort et
         | ViRcp et      => "ViRcp" ^ stringOfElemTypeShort et
         | ViExp2 et     => "ViExp2" ^ stringOfElemTypeShort et
         | ViExp2m1 et   => "ViExp2m1" ^ stringOfElemTypeShort et
         | ViLog2 et     => "ViLog2" ^ stringOfElemTypeShort et
         | ViLog2p1 et   => "ViLog2p1" ^ stringOfElemTypeShort et
         | ViSin et      => "ViSin" ^ stringOfElemTypeShort et
         | ViAsin et     => "ViAsin" ^ stringOfElemTypeShort et
         | ViCos et      => "ViCos" ^ stringOfElemTypeShort et
         | ViAcos et     => "ViAcos" ^ stringOfElemTypeShort et
         | ViTan et      => "ViTan" ^ stringOfElemTypeShort et
         | ViAtan et     => "ViAtan" ^ stringOfElemTypeShort et
         | ViSign et     => "ViSign" ^ stringOfElemTypeShort et
         | ViAbs et      => "ViAbs" ^ stringOfElemTypeShort et
         | ViEq et       => "ViEq" ^ stringOfElemTypeShort et
         | ViNe et       => "ViNe" ^ stringOfElemTypeShort et
         | ViGt et       => "ViGt" ^ stringOfElemTypeShort et
         | ViGe et       => "ViGe" ^ stringOfElemTypeShort et
         | ViLt et       => "ViLt" ^ stringOfElemTypeShort et
         | ViLe et       => "ViLe" ^ stringOfElemTypeShort et
         | ViSelect et   => "ViSelect" ^ stringOfElemTypeShort et
         | ViPermute (et, _)  => "ViPermute" ^ stringOfElemTypeShort et
         | ViInit et     => "ViInit" ^ stringOfElemTypeShort et

  fun layoutSize vs = Layout.str (stringOfSize vs)
  fun layoutElemType vet = Layout.str (stringOfElemType vet)
  fun layoutElemTypeShort vet = Layout.str (stringOfElemTypeShort vet)
  fun layoutPrim p = Layout.str (stringOfPrim p)

  structure Compare =
  struct

    fun elemType (et1, et2) =
        case (et1, et2)
         of (ViUInt8,   ViUInt8  ) => EQUAL
          | (ViUInt8,   _        ) => GREATER
          | (_,         ViUInt8  ) => LESS
          | (ViUInt16,  ViUInt16 ) => EQUAL
          | (ViUInt16,  _        ) => GREATER
          | (_,         ViUInt16 ) => LESS
          | (ViUInt32,  ViUInt32 ) => EQUAL
          | (ViUInt32,  _        ) => GREATER
          | (_,         ViUInt32 ) => LESS
          | (ViUInt64,  ViUInt64 ) => EQUAL
          | (ViUInt64,  _        ) => GREATER
          | (_,         ViUInt64 ) => LESS
          | (ViSInt8,   ViSInt8  ) => EQUAL
          | (ViSInt8,   _        ) => GREATER
          | (_,         ViSInt8  ) => LESS
          | (ViSInt16,  ViSInt16 ) => EQUAL
          | (ViSInt16,  _        ) => GREATER
          | (_,         ViSInt16 ) => LESS
          | (ViSInt32,  ViSInt32 ) => EQUAL
          | (ViSInt32,  _        ) => GREATER
          | (_,         ViSInt32 ) => LESS
          | (ViSInt64,  ViSInt64 ) => EQUAL
          | (ViSInt64,  _        ) => GREATER
          | (_,         ViSInt64 ) => LESS
          | (ViFloat16, ViFloat16) => EQUAL
          | (ViFloat16, _        ) => GREATER
          | (_,         ViFloat16) => LESS
          | (ViFloat32, ViFloat32) => EQUAL
          | (ViFloat32, _        ) => GREATER
          | (_,         ViFloat32) => LESS
          | (ViFloat64, ViFloat64) => EQUAL
    
    fun prim (x1, x2) =
        case (x1, x2)
         of (ViShiftL x1,  ViShiftL x2 ) => elemType (x1, x2)
          | (ViShiftL _,   _           ) => GREATER
          | (_,            ViShiftL _  ) => LESS
          | (ViShiftA x1,  ViShiftA x2 ) => elemType (x1, x2)
          | (ViShiftA _,   _           ) => GREATER
          | (_,            ViShiftA _  ) => LESS
          | (ViRotateL x1, ViRotateL x2) => elemType (x1, x2)
          | (ViRotateL _,  _           ) => GREATER
          | (_,            ViRotateL _ ) => LESS
          | (ViRotateR x1, ViRotateR x2) => elemType (x1, x2)
          | (ViRotateR _,  _           ) => GREATER
          | (_,            ViRotateR _ ) => LESS
          | (ViBitNot x1,  ViBitNot x2 ) => elemType (x1, x2)
          | (ViBitNot _,   _           ) => GREATER
          | (_,            ViBitNot _  ) => LESS
          | (ViBitAnd x1,  ViBitAnd x2 ) => elemType (x1, x2)
          | (ViBitAnd _,   _           ) => GREATER
          | (_,            ViBitAnd _  ) => LESS
          | (ViBitXor x1,  ViBitXor x2 ) => elemType (x1, x2)
          | (ViBitXor _,   _           ) => GREATER
          | (_,            ViBitXor _  ) => LESS
          | (ViBitOr x1,   ViBitOr x2  ) => elemType (x1, x2)
          | (ViBitOr _,    _           ) => GREATER
          | (_,            ViBitOr _   ) => LESS
          | (ViNot x1,     ViNot x2    ) => elemType (x1, x2)
          | (ViNot _,      _           ) => GREATER
          | (_,            ViNot _     ) => LESS
          | (ViAnd x1,     ViAnd x2    ) => elemType (x1, x2)
          | (ViAnd _,      _           ) => GREATER
          | (_,            ViAnd _     ) => LESS
          | (ViOr x1,      ViOr x2     ) => elemType (x1, x2)
          | (ViOr _,       _           ) => GREATER
          | (_,            ViOr _      ) => LESS
          | (ViMaskNot x1, ViMaskNot x2) => elemType (x1, x2)
          | (ViMaskNot _,  _           ) => GREATER
          | (_,            ViMaskNot _ ) => LESS
          | (ViMaskAnd x1, ViMaskAnd x2) => elemType (x1, x2)
          | (ViMaskAnd _,  _           ) => GREATER
          | (_,            ViMaskAnd _ ) => LESS
          | (ViMaskOr x1,  ViMaskOr x2 ) => elemType (x1, x2)
          | (ViMaskOr _,   _           ) => GREATER
          | (_,            ViMaskOr _  ) => LESS
          | (ViAdd x1,     ViAdd x2    ) => elemType (x1, x2)
          | (ViAdd _,      _           ) => GREATER
          | (_,            ViAdd _     ) => LESS
          | (ViSub x1,     ViSub x2    ) => elemType (x1, x2)
          | (ViSub _,      _           ) => GREATER
          | (_,            ViSub _     ) => LESS
          | (ViMul x1,     ViMul x2    ) => elemType (x1, x2)
          | (ViMul _,      _           ) => GREATER
          | (_,            ViMul _     ) => LESS
          | (ViDiv x1,     ViDiv x2    ) => elemType (x1, x2)
          | (ViDiv _,      _           ) => GREATER
          | (_,            ViDiv _     ) => LESS
          | (ViMod x1,     ViMod x2    ) => elemType (x1, x2)
          | (ViMod _,      _           ) => GREATER
          | (_,            ViMod _     ) => LESS
          | (ViFma x1,     ViFma x2    ) => elemType (x1, x2)
          | (ViFma _,      _           ) => GREATER
          | (_,            ViFma _     ) => LESS
          | (ViFms x1,     ViFms x2    ) => elemType (x1, x2)
          | (ViFms _,      _           ) => GREATER
          | (_,            ViFms _     ) => LESS
          | (ViMax x1,     ViMax x2    ) => elemType (x1, x2)
          | (ViMax _,      _           ) => GREATER
          | (_,            ViMax _     ) => LESS
          | (ViMin x1,     ViMin x2    ) => elemType (x1, x2)
          | (ViMin _,      _           ) => GREATER
          | (_,            ViMin _     ) => LESS
          | (ViNeg x1,     ViNeg x2    ) => elemType (x1, x2)
          | (ViNeg _,      _           ) => GREATER
          | (_,            ViNeg _     ) => LESS
          | (ViSqrt x1,    ViSqrt x2   ) => elemType (x1, x2)
          | (ViSqrt _,     _           ) => GREATER
          | (_,            ViSqrt _    ) => LESS
          | (ViSqrtRcp x1, ViSqrtRcp x2) => elemType (x1, x2)
          | (ViSqrtRcp _,  _           ) => GREATER
          | (_,            ViSqrtRcp _ ) => LESS
          | (ViRcp x1,     ViRcp x2    ) => elemType (x1, x2)
          | (ViRcp _,      _           ) => GREATER
          | (_,            ViRcp _     ) => LESS
          | (ViExp2 x1,    ViExp2 x2   ) => elemType (x1, x2)
          | (ViExp2 _,     _           ) => GREATER
          | (_,            ViExp2 _    ) => LESS
          | (ViExp2m1 x1,  ViExp2m1 x2 ) => elemType (x1, x2)
          | (ViExp2m1 _,   _           ) => GREATER
          | (_,            ViExp2m1 _  ) => LESS
          | (ViLog2 x1,    ViLog2 x2   ) => elemType (x1, x2)
          | (ViLog2 _,     _           ) => GREATER
          | (_,            ViLog2 _    ) => LESS
          | (ViLog2p1 x1,  ViLog2p1 x2 ) => elemType (x1, x2)
          | (ViLog2p1 _,   _           ) => GREATER
          | (_,            ViLog2p1 _  ) => LESS
          | (ViSin x1,     ViSin x2    ) => elemType (x1, x2)
          | (ViSin _,      _           ) => GREATER
          | (_,            ViSin _     ) => LESS
          | (ViAsin x1,    ViAsin x2   ) => elemType (x1, x2)
          | (ViAsin _,     _           ) => GREATER
          | (_,            ViAsin _    ) => LESS
          | (ViCos x1,     ViCos x2    ) => elemType (x1, x2)
          | (ViCos _,      _           ) => GREATER
          | (_,            ViCos _     ) => LESS
          | (ViAcos x1,    ViAcos x2   ) => elemType (x1, x2)
          | (ViAcos _,     _           ) => GREATER
          | (_,            ViAcos _    ) => LESS
          | (ViTan x1,     ViTan x2    ) => elemType (x1, x2)
          | (ViTan _,      _           ) => GREATER
          | (_,            ViTan _     ) => LESS
          | (ViAtan x1,    ViAtan x2   ) => elemType (x1, x2)
          | (ViAtan _,     _           ) => GREATER
          | (_,            ViAtan _    ) => LESS
          | (ViSign x1,    ViSign x2   ) => elemType (x1, x2)
          | (ViSign _,     _           ) => GREATER
          | (_,            ViSign _    ) => LESS
          | (ViAbs x1,     ViAbs x2    ) => elemType (x1, x2)
          | (ViAbs _,      _           ) => GREATER
          | (_,            ViAbs _     ) => LESS
          | (ViEq x1,      ViEq x2     ) => elemType (x1, x2)
          | (ViEq _,       _           ) => GREATER
          | (_,            ViEq _      ) => LESS
          | (ViNe x1,      ViNe x2     ) => elemType (x1, x2)
          | (ViNe _,       _           ) => GREATER
          | (_,            ViNe _      ) => LESS
          | (ViGt x1,      ViGt x2     ) => elemType (x1, x2)
          | (ViGt _,       _           ) => GREATER
          | (_,            ViGt _      ) => LESS
          | (ViGe x1,      ViGe x2     ) => elemType (x1, x2)
          | (ViGe _,       _           ) => GREATER
          | (_,            ViGe _      ) => LESS
          | (ViLt x1,      ViLt x2     ) => elemType (x1, x2)
          | (ViLt _,       _           ) => GREATER
          | (_,            ViLt _      ) => LESS
          | (ViLe x1,      ViLe x2     ) => elemType (x1, x2)
          | (ViLe _,       _           ) => GREATER
          | (_,            ViLe _      ) => LESS
          | (ViSelect x1,  ViSelect x2 ) => elemType (x1, x2)
          | (ViSelect _,   _           ) => GREATER
          | (_,            ViSelect _  ) => LESS
          | (ViPermute (x1, _), ViPermute (x2, _)) => elemType (x1, x2)
          | (ViInit x1,    ViInit x2   ) => elemType (x1, x2)
          | (ViInit _,     _           ) => GREATER
          | (_,            ViInit _    ) => LESS

  end

  val equalElemTypes = fn (t1, t2) => Compare.elemType (t1, t2) = EQUAL
end;
