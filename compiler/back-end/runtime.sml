(* The Intel P to C/Pillar Compiler *)
(* Copyright (C) Intel Corporation, October 2006 *)

(* The Compiler/Runtime Contract/Interface *)

functor RuntimeF(structure Pil : PIL) =
struct

  structure M = Mil

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

  structure VT =
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
             | M.PokOArray    => "VArrayTag"
             | M.PokIArray    => "VArrayIdxTag"
             | M.PokSum       => "VSumTag"
             | M.PokOptionSet => "VSetTag"
             | M.PokType      => "VTypeTag"
             | M.PokThunk     => "VThunkTag"
             | M.PokRef       => "VRefTag")
        
    fun pObjKindVTable pok =
        Pil.identifier
          (case pok
            of M.PokNone      => "pLsrVTableNone"
             | M.PokRat       => "pLsrPRatVTable"
             | M.PokFloat     => "pLsrPFloatVTable"
             | M.PokDouble    => "pLsrPDoubleVTable"
             | M.PokName      => "pLsrPNameVTable"
             | M.PokFunction  => "pLsrClosureVTable"
             | M.PokOArray    => "pLsrPArrayOVTable"
             | M.PokIArray    => "pLsrPArrayIVTable"
             | M.PokSum       => "pLsrPSumVTable"
             | M.PokOptionSet => "pLsrPSetVTable"
             | M.PokType      => "pLsrPTypeVTable"
             | M.PokThunk     => Fail.fail ("Runtime", "pObjKindVtable", "Must use Thunk.vTable")
             | M.PokRef       => "pLsrPRefVTable")

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

    val optMax = IntInf.- (IntInf.<< (IntInf.one, 0w30), IntInf.one)
    val optMin = IntInf.<< (IntInf.one, 0w30)

    fun checkOpt i =
        if IntInf.< (i, optMin) orelse IntInf.> (i, optMax) then
          Fail.fail ("Runtime", "checkOpt", "bad optimised rational")
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

    open Prims

    fun thnk t = if t then "T" else "D"

    fun getNumTypName nt =
        case nt
         of NtRat        => "Rational"
          | NtInteger    => "Integer"
          | NtIntegral t => IntArb.stringOfTyp t
          | NtFloat      => "Float"
          | NtDouble     => "Double"

    fun getArithName a =
        case a
         of APlus   => "Plus"
          | ANegate => "Negate"
          | AMinus  => "Minus"
          | ATimes  => "Times"
          | ADivide => "Divide"
          | ADiv    => "Div"
          | ARem    => "Rem"

    fun getCompareName c =
        case c
         of CEq => "EQ"
          | CNe => "NE"
          | CLt => "LT"
          | CLe => "LE"

    fun getNumArithName (nt, a) =
        "pLsr" ^ (getNumTypName nt) ^ (getArithName a)

    fun getNumCompareName (nt, c) =
        "pLsr" ^ (getNumTypName nt) ^ (getCompareName c)

    fun getNumConvertName (nt1, nt2) =
        "pLsr" ^ (getNumTypName nt1) ^ "From" ^ (getNumTypName nt2)

    fun getPrimName (p, t) =
        case p
         of PNumArith (nt, a)      => getNumArithName (nt, a)
          | PNumCompare (nt, c)    => getNumCompareName (nt, c)
          | PNumConvert (nt1, nt2) => getNumConvertName (nt1, nt2)
          | PRatNumerator          => "pLsrRatNumerator"
          | PRatDenominator        => "pLsrRatDenominator"
          | PEqual                 => "pLsrPEq"
          | PDom                   => "pLsrPDom"
          | PNub                   => "pLsrPNub"
          | PPtrType               => "pLsrPPtrType"
          | PPtrRead               => "pLsrPPtrRead"
          | PPtrWrite              => "pLsrPPtrWrite"
          | PPtrNew                => "pLsrPPtreNew"
          | PRatToUInt32Checked    => "pLsrRationalToUInt32Checked"

    fun getRuntimeName (rt, t) =
        case rt
         of RtIntDivMod          => "pLsrPIntDivMod" ^ thnk t
          | RtIntDiv             => "pLsrPIntDiv"
          | RtIntMod             => "pLsrPIntMod"
          | RtFloatMk            => "pLsrFloatMk"
          | RtFloatToInt         => "pLsrFloatToInt"
          | RtFloatFromInt       => "pLsrFloatFromInt"
          | RtFloatACos          => "pLsrFloatACos" 
          | RtFloatASin          => "pLsrFloatASin" 
          | RtFloatCos           => "pLsrFloatCos"  
          | RtFloatSin           => "pLsrFloatSin"  
          | RtFloatTan           => "pLsrFloatTan"  
          | RtFloatSqrt          => "pLsrFloatSqrt" 
          | RtFloatFMod          => "pLsrFloatFMod" 
          | RtFloatPow           => "pLsrFloatPow"  
          | RtWriteln            => "pLsrPWriteln"
          | RtReadln             => "pLsrPReadln" ^ thnk t
          | RtAssert             => "pLsrPAssert"
          | RtError              => "pLsrPError"
          | RtDebug              => "pLsrPDebug"
          | RtOpenOut            => "pLsrPOpenOut"
          | RtGetStdout          => "pLsrPGetStdout"
          | RtOutputByte         => "pLsrPOutputByte"
          | RtCloseOut           => "pLsrPCloseOut"
          | RtOpenIn             => "pLsrPOpenIn"
          | RtGetStdin           => "pLsrPGetStdin"
          | RtInputByte          => "pLsrPInputByte"
          | RtInputString        => "pLsrPInputString" ^ thnk t
          | RtInputAll           => "pLsrPInputAll" ^ thnk t
          | RtIsEOF              => "pLsrPIsEOF"
          | RtCloseIn            => "pLsrPCloseIn"
          | RtCommandLine        => "pLsrPCommandLine" ^ thnk t
          | RtStringToNat        => "pLsrPString2Nat"
          | RtStringToFloat      => "pLsrPString2Float"
          | RtFloatToString      => "pLsrPFloat2String" ^ thnk t
          | RtResetTimer         => "pLsrPResetTimer"
          | RtGetTimer           => "pLsrPGetTimer"
          | RtVtuneAttach        => "pLsrPVTuneAttach"
          | RtVtuneDetach        => "pLsrPVTuneDetach"
          | RtArrayEval          => "pLsrPArrayEval"

    local
      open VectorInstructions
    in

    fun genElemTypSuffix et =
        case et
         of ViUInt8   => "UI8"
          | ViUInt16  => "UI16"
          | ViUInt32  => "UI32"
          | ViUInt64  => "UI64"
          | ViSInt8   => "SI8"
          | ViSInt16  => "SI16"
          | ViSInt32  => "SI32"
          | ViSInt64  => "SI64"
          | ViFloat16 => "F16"
          | ViFloat32 => "F32"
          | ViFloat64 => "F64"

    fun getViName (p, t) = 
        case p 
         of ViShiftL et        => "pLsrViShiftL" ^ genElemTypSuffix et
          | ViShiftA et        => "pLsrViShiftA" ^ genElemTypSuffix et
          | ViRotateL et       => "pLsrViRotateL" ^ genElemTypSuffix et
          | ViRotateR et       => "pLsrViRotateR" ^ genElemTypSuffix et
          | ViBitNot et        => "pLsrViBitNot" ^ genElemTypSuffix et
          | ViBitAnd et        => "pLsrViBitAnd" ^ genElemTypSuffix et
          | ViBitXor et        => "pLsrViBitXor" ^ genElemTypSuffix et
          | ViBitOr et         => "pLsrViBitOr" ^ genElemTypSuffix et
          | ViNot et           => "pLsrViNot" ^ genElemTypSuffix et
          | ViAnd et           => "pLsrViAnd" ^ genElemTypSuffix et
          | ViOr et            => "pLsrViOr" ^ genElemTypSuffix et
          | ViMaskNot et       => "pLsrViMaskNot" ^ genElemTypSuffix et
          | ViMaskAnd et       => "pLsrViMaskAnd" ^ genElemTypSuffix et
          | ViMaskOr  et       => "pLsrViMaskOr" ^ genElemTypSuffix et
          | ViAdd et           => "pLsrViAdd" ^ genElemTypSuffix et
          | ViSub et           => "pLsrViSub" ^ genElemTypSuffix et
          | ViMul et           => "pLsrViMul" ^ genElemTypSuffix et
          | ViDiv et           => "pLsrViDiv" ^ genElemTypSuffix et
          | ViMod et           => "pLsrViMod" ^ genElemTypSuffix et
          | ViFma et           => "pLsrViFma" ^ genElemTypSuffix et
          | ViFms et           => "pLsrViFms" ^ genElemTypSuffix et
          | ViMax et           => "pLsrViMax" ^ genElemTypSuffix et
          | ViMin et           => "pLsrViMin" ^ genElemTypSuffix et
          | ViNeg et           => "pLsrViNeg" ^ genElemTypSuffix et
          | ViSqrt et          => "pLsrViSqrt" ^ genElemTypSuffix et
          | ViSqrtRcp et       => "pLsrViSqrtRcp" ^ genElemTypSuffix et
          | ViRcp et           => "pLsrViRcp" ^ genElemTypSuffix et
          | ViExp2 et          => "pLsrViExp2" ^ genElemTypSuffix et
          | ViExp2m1 et        => "pLsrViExp2m1" ^ genElemTypSuffix et
          | ViLog2 et          => "pLsrViLog2" ^ genElemTypSuffix et
          | ViLog2p1 et        => "pLsrViLog2p1" ^ genElemTypSuffix et
          | ViSin et           => "pLsrViSin" ^ genElemTypSuffix et
          | ViAsin et          => "pLsrViAsin" ^ genElemTypSuffix et
          | ViCos et           => "pLsrViCos" ^ genElemTypSuffix et
          | ViAcos et          => "pLsrViAcos" ^ genElemTypSuffix et
          | ViTan et           => "pLsrViTan" ^ genElemTypSuffix et
          | ViAtan et          => "pLsrViAtan" ^ genElemTypSuffix et
          | ViSign et          => "pLsrViSign" ^ genElemTypSuffix et
          | ViAbs et           => "pLsrViAbs" ^ genElemTypSuffix et
          | ViEq et            => "pLsrViEq" ^ genElemTypSuffix et
          | ViNe et            => "pLsrViNe" ^ genElemTypSuffix et
          | ViGt et            => "pLsrViGt" ^ genElemTypSuffix et
          | ViGe et            => "pLsrViGe" ^ genElemTypSuffix et
          | ViLt et            => "pLsrViLt" ^ genElemTypSuffix et
          | ViLe et            => "pLsrViLe" ^ genElemTypSuffix et
          | ViSelect et        => "pLsrViSelect" ^ genElemTypSuffix et
          | ViPermute (et, _)  => "pLsrViPermute" ^ genElemTypSuffix et

    end
    
    fun getName (p, t) =
        let
          val s =
              case p
               of Prim p     => getPrimName (p, t)
                | Runtime rt => getRuntimeName (rt, t)
                | Vi p       => getViName (p, t)
        in Pil.identifier s
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

  val pmain = Pil.identifier "__pmain"
  val gErrorVal = Pil.identifier "pLsrErrorVal"

end;
