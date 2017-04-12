(*
 * Redistribution and use in source and binary forms, with or without modification, are permitted 
 * provided that the following conditions are met:
 * 1.   Redistributions of source code must retain the above copyright notice, this list of 
 * conditions and the following disclaimer.
 * 2.   Redistributions in binary form must reproduce the above copyright notice, this list of
 * conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 * BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

(*
 * Definitions of primitive operators and their types from GHC.Prim.
 *)

signature GHC_PRIM_OP = 
sig
  datatype primOp
      (* char *)
    = GtCharzh
    | GeCharzh
    | EqCharzh
    | NeCharzh
    | LtCharzh
    | LeCharzh
    | Ordzh
     (* integer *)
    | Integerzmzh
    | Integerzpzh
    | Integerztzh
    | NegateIntegerzh
    | QuotIntegerzh
    | RemIntegerzh
    | QuotRemIntegerzh
    | Integerzezezh
    | Integerzszezh
    | Integerzgzezh
    | Integerzgzh
    | Integerzlzezh
    | Integerzlzh
    | Int2Integerzh
    | Int64ToIntegerzh
    | Word2Integerzh
    | Word64ToIntegerzh
    | Integer2Intzh
    | Integer2Int64zh
    | Integer2Wordzh
    | Integer2Word64zh
    | Integer2Floatzh
    | Integer2Doublezh
    | IntegerAndzh
    | IntegerOrzh
    | IntegerXorzh
    | IntegerIShiftLzh
    | IntegerIShiftRzh
    | IntegerEncodeFloatzh
    | IntegerEncodeDoublezh
     (* int *)
    | Zmzh
    | Zpzh
    | Ztzh
    | NegateIntzh
    | QuotIntzh
    | RemIntzh
    | QuotRemIntzh
    | Zezezh
    | Zszezh
    | Zgzezh
    | Zgzh
    | Zlzezh
    | Zlzh
    | Chrzh
    | Int2Wordzh
    | Int2Word64zh
    | Int2Floatzh
    | Int2Doublezh
    | UncheckedIShiftLzh
    | UncheckedIShiftRAzh
    | UncheckedIShiftRLzh
    | AddIntCzh
    | SubIntCzh
    | MulIntMayOflozh
     (* word *)
    | MinusWordzh
    | PlusWordzh
    | TimesWordzh
    | QuotWordzh
    | RemWordzh
    | QuotRemWordzh
    | Andzh
    | Orzh
    | Xorzh
    | Notzh
    | Word2Intzh
    | GtWordzh
    | GeWordzh
    | EqWordzh
    | NeWordzh
    | LtWordzh
    | LeWordzh
    | UncheckedShiftLzh
    | UncheckedShiftRLzh
     (* narrowing *)
    | Narrow8Intzh
    | Narrow16Intzh
    | Narrow32Intzh
    | Narrow8Wordzh
    | Narrow16Wordzh
    | Narrow32Wordzh
     (* double *)
    | Zezezhzh
    | Zszezhzh
    | Zgzezhzh
    | Zgzhzh
    | Zlzezhzh
    | Zlzhzh
    | Zmzhzh
    | Zpzhzh
    | Ztzhzh
    | Zszhzh
    | NegateDoublezh
    | Double2Intzh
    | Double2Floatzh
    | TanhDoublezh
    | CoshDoublezh
    | SinhDoublezh
    | AtanDoublezh
    | LogDoublezh
    | ExpDoublezh
    | AcosDoublezh
    | AsinDoublezh
    | TanDoublezh
    | CosDoublezh
    | SinDoublezh
    | SqrtDoublezh
    | Ztztzhzh
    | DecodeDoublezu2Intzh
     (* float *)
    | EqFloatzh
    | NeFloatzh
    | GeFloatzh
    | GtFloatzh
    | LeFloatzh
    | LtFloatzh
    | MinusFloatzh
    | PlusFloatzh
    | TimesFloatzh
    | DivideFloatzh
    | NegateFloatzh
    | Float2Intzh
    | Float2Doublezh
    | TanhFloatzh
    | CoshFloatzh
    | SinhFloatzh
    | AtanFloatzh
    | LogFloatzh
    | ExpFloatzh
    | AcosFloatzh
    | AsinFloatzh
    | TanFloatzh
    | CosFloatzh
    | SinFloatzh
    | SqrtFloatzh
    | PowerFloatzh
    | DecodeFloatzuIntzh
     (* bit operations *)
    | PopCnt8zh
    | PopCnt16zh
    | PopCnt32zh
    | PopCnt64zh
    | PopCntzh
     (* array *)
    | NewArrayzh
    | ReadArrayzh
    | WriteArrayzh
    | SameMutableArrayzh
    | IndexArrayzh
    | SizeofArrayzh
    | SizeofMutableArrayzh
    | UnsafeThawArrayzh
    | UnsafeFreezzeArrayzh
    | CopyArrayzh
    | CopyMutableArrayzh
    | CopyByteArrayzh
    | CopyMutableByteArrayzh
     (* Immutable Arrays *)
    | NewImmutableArrayzh
    | NewStrictImmutableArrayzh
    | InitImmutableArrayzh
    | InitStrictImmutableArrayzh
    | ImmutableArrayInitedzh
    | StrictImmutableArrayInitedzh
    | SizzeofImmutableArrayzh
    | SizzeofStrictImmutableArrayzh
    | IndexImmutableArrayzh
    | IndexStrictImmutableArrayzh
     (* Unboxed Arrays *)
    | NewUnboxedWordArrayzh
    | NewUnboxedWord8Arrayzh
    | NewUnboxedWord16Arrayzh
    | NewUnboxedWord32Arrayzh
    | NewUnboxedWord64Arrayzh
    | NewUnboxedIntArrayzh
    | NewUnboxedInt8Arrayzh
    | NewUnboxedInt16Arrayzh
    | NewUnboxedInt32Arrayzh
    | NewUnboxedInt64Arrayzh
    | NewUnboxedFloatArrayzh
    | NewUnboxedDoubleArrayzh
    | NewUnboxedCharArrayzh
    | NewUnboxedAddrArrayzh
    | InitUnboxedWordArrayzh
    | InitUnboxedWord8Arrayzh
    | InitUnboxedWord16Arrayzh
    | InitUnboxedWord32Arrayzh
    | InitUnboxedWord64Arrayzh
    | InitUnboxedIntArrayzh
    | InitUnboxedInt8Arrayzh
    | InitUnboxedInt16Arrayzh
    | InitUnboxedInt32Arrayzh
    | InitUnboxedInt64Arrayzh
    | InitUnboxedFloatArrayzh
    | InitUnboxedDoubleArrayzh
    | InitUnboxedCharArrayzh
    | InitUnboxedAddrArrayzh
    | UnboxedWordArrayInitedzh
    | UnboxedWord8ArrayInitedzh
    | UnboxedWord16ArrayInitedzh
    | UnboxedWord32ArrayInitedzh
    | UnboxedWord64ArrayInitedzh
    | UnboxedIntArrayInitedzh
    | UnboxedInt8ArrayInitedzh
    | UnboxedInt16ArrayInitedzh
    | UnboxedInt32ArrayInitedzh
    | UnboxedInt64ArrayInitedzh
    | UnboxedFloatArrayInitedzh
    | UnboxedDoubleArrayInitedzh
    | UnboxedCharArrayInitedzh
    | UnboxedAddrArrayInitedzh
    | SizeofUnboxedWordArrayzh
    | SizeofUnboxedWord8Arrayzh
    | SizeofUnboxedWord16Arrayzh
    | SizeofUnboxedWord32Arrayzh
    | SizeofUnboxedWord64Arrayzh
    | SizeofUnboxedIntArrayzh
    | SizeofUnboxedInt8Arrayzh
    | SizeofUnboxedInt16Arrayzh
    | SizeofUnboxedInt32Arrayzh
    | SizeofUnboxedInt64Arrayzh
    | SizeofUnboxedFloatArrayzh
    | SizeofUnboxedDoubleArrayzh
    | SizeofUnboxedCharArrayzh
    | SizeofUnboxedAddrArrayzh
    | IndexUnboxedWordArrayzh
    | IndexUnboxedWord8Arrayzh
    | IndexUnboxedWord16Arrayzh
    | IndexUnboxedWord32Arrayzh
    | IndexUnboxedWord64Arrayzh
    | IndexUnboxedIntArrayzh
    | IndexUnboxedInt8Arrayzh
    | IndexUnboxedInt16Arrayzh
    | IndexUnboxedInt32Arrayzh
    | IndexUnboxedInt64Arrayzh
    | IndexUnboxedFloatArrayzh
    | IndexUnboxedDoubleArrayzh
    | IndexUnboxedCharArrayzh
    | IndexUnboxedAddrArrayzh
     (* byte arrays *)
    | NewByteArrayzh
    | NewPinnedByteArrayzh
    | NewAlignedPinnedByteArrayzh
    | ByteArrayContentszh
    | SameMutableByteArrayzh
    | UnsafeFreezzeByteArrayzh
    | SizzeofByteArrayzh
    | SizzeofMutableByteArrayzh
     (* typed unboxed array as alternative to creating byte arrays *)
    | NewUnboxedArrayzh
    | NewPinnedUnboxedArrayzh
    | NewAlignedPinnedUnboxedArrayzh
     (* the following operation is only defined on typed unboxed arrays, never use them on byte arrays! *)
    | IndexWord64Arrayzh
    | IndexWord32Arrayzh
    | IndexWord16Arrayzh
    | IndexWord8Arrayzh
    | IndexInt64Arrayzh
    | IndexInt32Arrayzh
    | IndexInt16Arrayzh
    | IndexInt8Arrayzh
    | IndexWordArrayzh
    | IndexIntArrayzh
    | IndexWideCharArrayzh
    | IndexCharArrayzh
    | IndexStablePtrArrayzh
    | IndexDoubleArrayzh
    | IndexFloatArrayzh
    | IndexAddrArrayzh
     (* the following operation is only defined on typed unboxed arrays, never use them on byte arrays! *)
    | ReadWord64Arrayzh
    | ReadWord32Arrayzh
    | ReadWord16Arrayzh
    | ReadWord8Arrayzh
    | ReadInt64Arrayzh
    | ReadInt32Arrayzh
    | ReadInt16Arrayzh
    | ReadInt8Arrayzh
    | ReadStablePtrArrayzh
    | ReadDoubleArrayzh
    | ReadFloatArrayzh
    | ReadAddrArrayzh
    | ReadWordArrayzh
    | ReadIntArrayzh
    | ReadWideCharArrayzh
    | ReadCharArrayzh
     (* the following operation is only defined on typed unboxed arrays, never use them on byte arrays! *)
    | WriteWord64Arrayzh
    | WriteWord32Arrayzh
    | WriteWord16Arrayzh
    | WriteWord8Arrayzh
    | WriteInt64Arrayzh
    | WriteInt32Arrayzh
    | WriteInt16Arrayzh
    | WriteInt8Arrayzh
    | WriteStablePtrArrayzh
    | WriteDoubleArrayzh
    | WriteFloatArrayzh
    | WriteAddrArrayzh
    | WriteWordArrayzh
    | WriteIntArrayzh
    | WriteWideCharArrayzh
    | WriteCharArrayzh
     (* addr *)
    | NullAddrzh
    | PlusAddrzh
    | MinusAddrzh
    | RemAddrzh
    | Addr2Intzh
    | Int2Addrzh
    | GtAddrzh
    | GeAddrzh
    | LtAddrzh
    | LeAddrzh
    | EqAddrzh
    | CastToAddrzh
    | CastFromAddrzh
    | IndexWord64OffAddrzh
    | IndexWord32OffAddrzh
    | IndexWord16OffAddrzh
    | IndexWord8OffAddrzh
    | IndexInt64OffAddrzh
    | IndexInt32OffAddrzh
    | IndexInt16OffAddrzh
    | IndexInt8OffAddrzh
    | IndexWordOffAddrzh
    | IndexIntOffAddrzh
    | IndexWideCharOffAddrzh
    | IndexStablePtrOffAddrzh
    | IndexDoubleOffAddrzh
    | IndexFloatOffAddrzh
    | IndexAddrOffAddrzh
    | IndexCharOffAddrzh
    | ReadWord64OffAddrzh
    | ReadWord32OffAddrzh
    | ReadWord16OffAddrzh
    | ReadWord8OffAddrzh
    | ReadInt64OffAddrzh
    | ReadInt32OffAddrzh
    | ReadInt16OffAddrzh
    | ReadInt8OffAddrzh
    | ReadWordOffAddrzh
    | ReadIntOffAddrzh
    | ReadWideCharOffAddrzh
    | ReadStablePtrOffAddrzh
    | ReadDoubleOffAddrzh
    | ReadFloatOffAddrzh
    | ReadAddrOffAddrzh
    | ReadCharOffAddrzh
    | WriteWord64OffAddrzh
    | WriteWord32OffAddrzh
    | WriteWord16OffAddrzh
    | WriteWord8OffAddrzh
    | WriteInt64OffAddrzh
    | WriteInt32OffAddrzh
    | WriteInt16OffAddrzh
    | WriteInt8OffAddrzh
    | WriteWordOffAddrzh
    | WriteIntOffAddrzh
    | WriteWideCharOffAddrzh
    | WriteStablePtrOffAddrzh
    | WriteDoubleOffAddrzh
    | WriteFloatOffAddrzh
    | WriteAddrOffAddrzh
    | WriteCharOffAddrzh
     (* Mutable Variable *)
    | NewMutVarzh
    | ReadMutVarzh
    | WriteMutVarzh
    | SameMutVarzh
    | CasMutVarzh
    | AtomicModifyMutVarzh
     (* STM *)
    | NewTVarzh
    | ReadTVarzh
    | ReadTVarIOzh
    | WriteTVarzh
    | SameTVarzh
    | Atomicallyzh
    (* TODO
    | Retryzh
    | CatchRetryzh
    | Checkzh
    | CatchSTMzh
    *)
     (* exception *)
    | Catchzh
    | Raisezh
    | RaiseIOzh
    | MaskAsyncExceptionszh
    | MaskUninterruptiblezh
    | UnmaskAsyncExceptionszh
    | GetMaskingStatezh
     (* MVar *)
    | NewMVarzh
    | TakeMVarzh
    | TryTakeMVarzh
    | PutMVarzh
    | TryPutMVarzh
    | SameMVarzh
    | IsEmptyMVarzh
     (* delay/wait *)
    | Delayzh
    | WaitReadzh
    | WaitWritezh
    | AsyncReadzh
    | AsyncWritezh
    | AsyncDoProczh
     (* concurrency *)
    | Forkzh
    | ForkOnzh
    | KillThreadzh
    | Yieldzh
    | MyThreadIdzh
    | LabelThreadzh
    | IsCurrentThreadBoundzh
    | NoDuplicatezh
    | ThreadStatuszh
     (* weak pointer *)
    | MkWeakzh
    | MkWeakForeignEnvzh
    | DeRefWeakzh
    | FinalizzeWeakzh
    | Touchzh
     (* stable pointer *)
    | MakeStablePtrzh
    | DeRefStablePtrzh
    | FreeStablePtrzh
    | EqStablePtrzh
     (* XXX: not implemented
    | MakeStableNamezh
    | EqStableNamezh
    | StableNameToIntzh
     *)
     (* unsafe pointer *)
    | ReallyUnsafePtrEqualityzh
     (* parallelism *)
     (* XXX: need to add these: parzh, sparkzh, getSparkzh, numSparkszh, parGlobalzh, parLocalzh, parAtzh,
      *      parAtAbszh, parAtRelzh, parAtForNowzh, copyablezh, noFollowzh
      *)
    | Seqzh
     (* tag to enum *)
    | DataToTagzh
    | TagToEnumzh
     (* Misc *)
     (* XXX: traceCcszh and traceEventzh *)
    | TraceEventzh

  val effects : primOp -> Effect.set

end

structure GHCPrimOp =
struct

  datatype primOp
      (* char *)
    = GtCharzh
    | GeCharzh
    | EqCharzh
    | NeCharzh
    | LtCharzh
    | LeCharzh
    | Ordzh
     (* integer *)
    | Integerzmzh
    | Integerzpzh
    | Integerztzh
    | NegateIntegerzh
    | QuotIntegerzh
    | RemIntegerzh
    | QuotRemIntegerzh
    | Integerzezezh
    | Integerzszezh
    | Integerzgzezh
    | Integerzgzh
    | Integerzlzezh
    | Integerzlzh
    | Int2Integerzh
    | Int64ToIntegerzh
    | Word2Integerzh
    | Word64ToIntegerzh
    | Integer2Intzh
    | Integer2Int64zh
    | Integer2Wordzh
    | Integer2Word64zh
    | Integer2Floatzh
    | Integer2Doublezh
    | IntegerAndzh
    | IntegerOrzh
    | IntegerXorzh
    | IntegerIShiftLzh
    | IntegerIShiftRzh
    | IntegerEncodeFloatzh
    | IntegerEncodeDoublezh
     (* int *)
    | Zmzh
    | Zpzh
    | Ztzh
    | NegateIntzh
    | QuotIntzh
    | RemIntzh
    | QuotRemIntzh
    | Zezezh
    | Zszezh
    | Zgzezh
    | Zgzh
    | Zlzezh
    | Zlzh
    | Chrzh
    | Int2Wordzh
    | Int2Word64zh
    | Int2Floatzh
    | Int2Doublezh
    | UncheckedIShiftLzh
    | UncheckedIShiftRAzh
    | UncheckedIShiftRLzh
    | AddIntCzh
    | SubIntCzh
    | MulIntMayOflozh
     (* word *)
    | MinusWordzh
    | PlusWordzh
    | TimesWordzh
    | QuotWordzh
    | RemWordzh
    | QuotRemWordzh
    | Andzh
    | Orzh
    | Xorzh
    | Notzh
    | Word2Intzh
    | GtWordzh
    | GeWordzh
    | EqWordzh
    | NeWordzh
    | LtWordzh
    | LeWordzh
    | UncheckedShiftLzh
    | UncheckedShiftRLzh
     (* narrowing *)
    | Narrow8Intzh
    | Narrow16Intzh
    | Narrow32Intzh
    | Narrow8Wordzh
    | Narrow16Wordzh
    | Narrow32Wordzh
     (* double *)
    | Zezezhzh
    | Zszezhzh
    | Zgzezhzh
    | Zgzhzh
    | Zlzezhzh
    | Zlzhzh
    | Zmzhzh
    | Zpzhzh
    | Ztzhzh
    | Zszhzh
    | NegateDoublezh
    | Double2Intzh
    | Double2Floatzh
    | TanhDoublezh
    | CoshDoublezh
    | SinhDoublezh
    | AtanDoublezh
    | LogDoublezh
    | ExpDoublezh
    | AcosDoublezh
    | AsinDoublezh
    | TanDoublezh
    | CosDoublezh
    | SinDoublezh
    | SqrtDoublezh
    | Ztztzhzh
    | DecodeDoublezu2Intzh
     (* float *)
    | EqFloatzh
    | NeFloatzh
    | GeFloatzh
    | GtFloatzh
    | LeFloatzh
    | LtFloatzh
    | MinusFloatzh
    | PlusFloatzh
    | TimesFloatzh
    | DivideFloatzh
    | NegateFloatzh
    | Float2Intzh
    | Float2Doublezh
    | TanhFloatzh
    | CoshFloatzh
    | SinhFloatzh
    | AtanFloatzh
    | LogFloatzh
    | ExpFloatzh
    | AcosFloatzh
    | AsinFloatzh
    | TanFloatzh
    | CosFloatzh
    | SinFloatzh
    | SqrtFloatzh
    | PowerFloatzh
    | DecodeFloatzuIntzh
     (* bit operations *)
    | PopCnt8zh
    | PopCnt16zh
    | PopCnt32zh
    | PopCnt64zh
    | PopCntzh
     (* array *)
    | NewArrayzh
    | ReadArrayzh
    | WriteArrayzh
    | SameMutableArrayzh
    | IndexArrayzh
    | SizeofArrayzh
    | SizeofMutableArrayzh
    | UnsafeThawArrayzh
    | UnsafeFreezzeArrayzh
    | CopyArrayzh
    | CopyMutableArrayzh
    | CopyByteArrayzh
    | CopyMutableByteArrayzh
     (* Immutable Arrays *)
    | NewImmutableArrayzh
    | NewStrictImmutableArrayzh
    | InitImmutableArrayzh
    | InitStrictImmutableArrayzh
    | ImmutableArrayInitedzh
    | StrictImmutableArrayInitedzh
    | SizzeofImmutableArrayzh
    | SizzeofStrictImmutableArrayzh
    | IndexImmutableArrayzh
    | IndexStrictImmutableArrayzh
     (* Unboxed Arrays *)
    | NewUnboxedWordArrayzh
    | NewUnboxedWord8Arrayzh
    | NewUnboxedWord16Arrayzh
    | NewUnboxedWord32Arrayzh
    | NewUnboxedWord64Arrayzh
    | NewUnboxedIntArrayzh
    | NewUnboxedInt8Arrayzh
    | NewUnboxedInt16Arrayzh
    | NewUnboxedInt32Arrayzh
    | NewUnboxedInt64Arrayzh
    | NewUnboxedFloatArrayzh
    | NewUnboxedDoubleArrayzh
    | NewUnboxedCharArrayzh
    | NewUnboxedAddrArrayzh
    | InitUnboxedWordArrayzh
    | InitUnboxedWord8Arrayzh
    | InitUnboxedWord16Arrayzh
    | InitUnboxedWord32Arrayzh
    | InitUnboxedWord64Arrayzh
    | InitUnboxedIntArrayzh
    | InitUnboxedInt8Arrayzh
    | InitUnboxedInt16Arrayzh
    | InitUnboxedInt32Arrayzh
    | InitUnboxedInt64Arrayzh
    | InitUnboxedFloatArrayzh
    | InitUnboxedDoubleArrayzh
    | InitUnboxedCharArrayzh
    | InitUnboxedAddrArrayzh
    | UnboxedWordArrayInitedzh
    | UnboxedWord8ArrayInitedzh
    | UnboxedWord16ArrayInitedzh
    | UnboxedWord32ArrayInitedzh
    | UnboxedWord64ArrayInitedzh
    | UnboxedIntArrayInitedzh
    | UnboxedInt8ArrayInitedzh
    | UnboxedInt16ArrayInitedzh
    | UnboxedInt32ArrayInitedzh
    | UnboxedInt64ArrayInitedzh
    | UnboxedFloatArrayInitedzh
    | UnboxedDoubleArrayInitedzh
    | UnboxedCharArrayInitedzh
    | UnboxedAddrArrayInitedzh
    | SizeofUnboxedWordArrayzh
    | SizeofUnboxedWord8Arrayzh
    | SizeofUnboxedWord16Arrayzh
    | SizeofUnboxedWord32Arrayzh
    | SizeofUnboxedWord64Arrayzh
    | SizeofUnboxedIntArrayzh
    | SizeofUnboxedInt8Arrayzh
    | SizeofUnboxedInt16Arrayzh
    | SizeofUnboxedInt32Arrayzh
    | SizeofUnboxedInt64Arrayzh
    | SizeofUnboxedFloatArrayzh
    | SizeofUnboxedDoubleArrayzh
    | SizeofUnboxedCharArrayzh
    | SizeofUnboxedAddrArrayzh
    | IndexUnboxedWordArrayzh
    | IndexUnboxedWord8Arrayzh
    | IndexUnboxedWord16Arrayzh
    | IndexUnboxedWord32Arrayzh
    | IndexUnboxedWord64Arrayzh
    | IndexUnboxedIntArrayzh
    | IndexUnboxedInt8Arrayzh
    | IndexUnboxedInt16Arrayzh
    | IndexUnboxedInt32Arrayzh
    | IndexUnboxedInt64Arrayzh
    | IndexUnboxedFloatArrayzh
    | IndexUnboxedDoubleArrayzh
    | IndexUnboxedCharArrayzh
    | IndexUnboxedAddrArrayzh
     (* byte arrays *)
    | NewByteArrayzh
    | NewPinnedByteArrayzh
    | NewAlignedPinnedByteArrayzh
    | ByteArrayContentszh
    | SameMutableByteArrayzh
    | UnsafeFreezzeByteArrayzh
    | SizzeofByteArrayzh
    | SizzeofMutableByteArrayzh
     (* typed unboxed array as alternative to creating byte arrays *)
    | NewUnboxedArrayzh
    | NewPinnedUnboxedArrayzh
    | NewAlignedPinnedUnboxedArrayzh
     (* the following operation is only defined on typed unboxed arrays, never use them on byte arrays! *)
    | IndexWord64Arrayzh
    | IndexWord32Arrayzh
    | IndexWord16Arrayzh
    | IndexWord8Arrayzh
    | IndexInt64Arrayzh
    | IndexInt32Arrayzh
    | IndexInt16Arrayzh
    | IndexInt8Arrayzh
    | IndexWordArrayzh
    | IndexIntArrayzh
    | IndexWideCharArrayzh
    | IndexCharArrayzh
    | IndexStablePtrArrayzh
    | IndexDoubleArrayzh
    | IndexFloatArrayzh
    | IndexAddrArrayzh
     (* the following operation is only defined on typed unboxed arrays, never use them on byte arrays! *)
    | ReadWord64Arrayzh
    | ReadWord32Arrayzh
    | ReadWord16Arrayzh
    | ReadWord8Arrayzh
    | ReadInt64Arrayzh
    | ReadInt32Arrayzh
    | ReadInt16Arrayzh
    | ReadInt8Arrayzh
    | ReadStablePtrArrayzh
    | ReadDoubleArrayzh
    | ReadFloatArrayzh
    | ReadAddrArrayzh
    | ReadWordArrayzh
    | ReadIntArrayzh
    | ReadWideCharArrayzh
    | ReadCharArrayzh
     (* the following operation is only defined on typed unboxed arrays, never use them on byte arrays! *)
    | WriteWord64Arrayzh
    | WriteWord32Arrayzh
    | WriteWord16Arrayzh
    | WriteWord8Arrayzh
    | WriteInt64Arrayzh
    | WriteInt32Arrayzh
    | WriteInt16Arrayzh
    | WriteInt8Arrayzh
    | WriteStablePtrArrayzh
    | WriteDoubleArrayzh
    | WriteFloatArrayzh
    | WriteAddrArrayzh
    | WriteWordArrayzh
    | WriteIntArrayzh
    | WriteWideCharArrayzh
    | WriteCharArrayzh
     (* addr *)
    | NullAddrzh
    | PlusAddrzh
    | MinusAddrzh
    | RemAddrzh
    | Addr2Intzh
    | Int2Addrzh
    | GtAddrzh
    | GeAddrzh
    | LtAddrzh
    | LeAddrzh
    | EqAddrzh
    | CastToAddrzh
    | CastFromAddrzh
    | IndexWord64OffAddrzh
    | IndexWord32OffAddrzh
    | IndexWord16OffAddrzh
    | IndexWord8OffAddrzh
    | IndexInt64OffAddrzh
    | IndexInt32OffAddrzh
    | IndexInt16OffAddrzh
    | IndexInt8OffAddrzh
    | IndexWordOffAddrzh
    | IndexIntOffAddrzh
    | IndexWideCharOffAddrzh
    | IndexStablePtrOffAddrzh
    | IndexDoubleOffAddrzh
    | IndexFloatOffAddrzh
    | IndexAddrOffAddrzh
    | IndexCharOffAddrzh
    | ReadWord64OffAddrzh
    | ReadWord32OffAddrzh
    | ReadWord16OffAddrzh
    | ReadWord8OffAddrzh
    | ReadInt64OffAddrzh
    | ReadInt32OffAddrzh
    | ReadInt16OffAddrzh
    | ReadInt8OffAddrzh
    | ReadWordOffAddrzh
    | ReadIntOffAddrzh
    | ReadWideCharOffAddrzh
    | ReadStablePtrOffAddrzh
    | ReadDoubleOffAddrzh
    | ReadFloatOffAddrzh
    | ReadAddrOffAddrzh
    | ReadCharOffAddrzh
    | WriteWord64OffAddrzh
    | WriteWord32OffAddrzh
    | WriteWord16OffAddrzh
    | WriteWord8OffAddrzh
    | WriteInt64OffAddrzh
    | WriteInt32OffAddrzh
    | WriteInt16OffAddrzh
    | WriteInt8OffAddrzh
    | WriteWordOffAddrzh
    | WriteIntOffAddrzh
    | WriteWideCharOffAddrzh
    | WriteStablePtrOffAddrzh
    | WriteDoubleOffAddrzh
    | WriteFloatOffAddrzh
    | WriteAddrOffAddrzh
    | WriteCharOffAddrzh
     (* Mutable Variable *)
    | NewMutVarzh
    | ReadMutVarzh
    | WriteMutVarzh
    | SameMutVarzh
    | CasMutVarzh
    | AtomicModifyMutVarzh
     (* STM *)
    | NewTVarzh
    | ReadTVarzh
    | ReadTVarIOzh
    | WriteTVarzh
    | SameTVarzh
    | Atomicallyzh
    (* TODO
    | Retryzh
    | CatchRetryzh
    | Checkzh
    | CatchSTMzh
    *)
     (* exception *)
    | Catchzh
    | Raisezh
    | RaiseIOzh
    | MaskAsyncExceptionszh
    | MaskUninterruptiblezh
    | UnmaskAsyncExceptionszh
    | GetMaskingStatezh
     (* MVar *)
    | NewMVarzh
    | TakeMVarzh
    | TryTakeMVarzh
    | PutMVarzh
    | TryPutMVarzh
    | SameMVarzh
    | IsEmptyMVarzh
     (* delay/wait *)
    | Delayzh
    | WaitReadzh
    | WaitWritezh
    | AsyncReadzh
    | AsyncWritezh
    | AsyncDoProczh
     (* concurrency *)
    | Forkzh
    | ForkOnzh
    | KillThreadzh
    | Yieldzh
    | MyThreadIdzh
    | LabelThreadzh
    | IsCurrentThreadBoundzh
    | NoDuplicatezh
    | ThreadStatuszh
     (* weak pointer *)
    | MkWeakzh
    | MkWeakForeignEnvzh
    | DeRefWeakzh
    | FinalizzeWeakzh
    | Touchzh
     (* stable pointer *)
    | MakeStablePtrzh
    | DeRefStablePtrzh
    | FreeStablePtrzh
    | EqStablePtrzh
     (* XXX: not implemented
    | MakeStableNamezh
    | EqStableNamezh
    | StableNameToIntzh
     *)
     (* unsafe pointer *)
    | ReallyUnsafePtrEqualityzh
     (* parallelism *)
     (* XXX: need to add these: parzh, sparkzh, getSparkzh, numSparkszh, parGlobalzh, parLocalzh, parAtzh,
      *      parAtAbszh, parAtRelzh, parAtForNowzh, copyablezh, noFollowzh
      *)
    | Seqzh
     (* tag to enum *)
    | DataToTagzh
    | TagToEnumzh
     (* Misc *)
     (* XXX: traceCcszh and traceEventzh *)
    | TraceEventzh

  val strTable = StringDict.fromList
      [ ("gtCharzh", GtCharzh)
      , ("geCharzh", GeCharzh)
      , ("eqCharzh", EqCharzh)
      , ("neCharzh", NeCharzh)
      , ("ltCharzh", LtCharzh)
      , ("leCharzh", LeCharzh)
      , ("ordzh", Ordzh)
      , ("integerzmzh", Integerzmzh)
      , ("integerzpzh", Integerzpzh)
      , ("integerztzh", Integerztzh)
      , ("negateIntegerzh", NegateIntegerzh)
      , ("quotIntegerzh", QuotIntegerzh)
      , ("remIntegerzh", RemIntegerzh)
      , ("quotRemIntegerzh", QuotRemIntegerzh)
      , ("integerzezezh", Integerzezezh)
      , ("integerzszezh", Integerzszezh)
      , ("integerzgzezh", Integerzgzezh)
      , ("integerzgzh", Integerzgzh)
      , ("integerzlzezh", Integerzlzezh)
      , ("integerzlzh", Integerzlzh)
      , ("int2Integerzh", Int2Integerzh)
      , ("int64ToIntegerzh", Int64ToIntegerzh)
      , ("word2Integerzh", Word2Integerzh)
      , ("word64ToIntegerzh", Word64ToIntegerzh)
      , ("integer2Intzh", Integer2Intzh)
      , ("integer2Int64zh", Integer2Int64zh)
      , ("integer2Wordzh", Integer2Wordzh)
      , ("integer2Word64zh", Integer2Word64zh)
      , ("integer2Floatzh", Integer2Floatzh)
      , ("integer2Doublezh", Integer2Doublezh)
      , ("integerAndzh", IntegerAndzh)
      , ("integerOrzh", IntegerOrzh)
      , ("integerXorzh", IntegerXorzh)
      , ("integerIShiftLzh", IntegerIShiftLzh)
      , ("integerIShiftRzh", IntegerIShiftRzh)
      , ("integerEncodeFloatzh", IntegerEncodeFloatzh)
      , ("integerEncodeDoublezh", IntegerEncodeDoublezh)
      , ("zmzh", Zmzh)
      , ("zpzh", Zpzh)
      , ("ztzh", Ztzh)
      , ("negateIntzh", NegateIntzh)
      , ("quotIntzh", QuotIntzh)
      , ("remIntzh", RemIntzh)
      , ("quotRemIntzh", QuotRemIntzh)
      , ("zezezh", Zezezh)
      , ("zszezh", Zszezh)
      , ("zgzezh", Zgzezh)
      , ("zgzh", Zgzh)
      , ("zlzezh", Zlzezh)
      , ("zlzh", Zlzh)
      , ("chrzh", Chrzh)
      , ("int2Wordzh", Int2Wordzh)
      , ("int2Word64zh", Int2Word64zh)
      , ("int2Floatzh", Int2Floatzh)
      , ("int2Doublezh", Int2Doublezh)
      , ("uncheckedIShiftLzh", UncheckedIShiftLzh)
      , ("uncheckedIShiftRAzh", UncheckedIShiftRAzh)
      , ("uncheckedIShiftRLzh", UncheckedIShiftRLzh)
      , ("addIntCzh", AddIntCzh)
      , ("subIntCzh", SubIntCzh)
      , ("mulIntMayOflozh", MulIntMayOflozh)
      , ("minusWordzh", MinusWordzh)
      , ("plusWordzh", PlusWordzh)
      , ("timesWordzh", TimesWordzh)
      , ("quotWordzh", QuotWordzh)
      , ("remWordzh", RemWordzh)
      , ("quotRemWordzh", QuotRemWordzh)
      , ("andzh", Andzh)
      , ("orzh", Orzh)
      , ("xorzh", Xorzh)
      , ("notzh", Notzh)
      , ("word2Intzh", Word2Intzh)
      , ("gtWordzh", GtWordzh)
      , ("geWordzh", GeWordzh)
      , ("eqWordzh", EqWordzh)
      , ("neWordzh", NeWordzh)
      , ("ltWordzh", LtWordzh)
      , ("leWordzh", LeWordzh)
      , ("uncheckedShiftLzh", UncheckedShiftLzh)
      , ("uncheckedShiftRLzh", UncheckedShiftRLzh)
      , ("narrow8Intzh", Narrow8Intzh)
      , ("narrow16Intzh", Narrow16Intzh)
      , ("narrow32Intzh", Narrow32Intzh)
      , ("narrow8Wordzh", Narrow8Wordzh)
      , ("narrow16Wordzh", Narrow16Wordzh)
      , ("narrow32Wordzh", Narrow32Wordzh)
      , ("zezezhzh", Zezezhzh)
      , ("zszezhzh", Zszezhzh)
      , ("zgzezhzh", Zgzezhzh)
      , ("zgzhzh", Zgzhzh)
      , ("zlzezhzh", Zlzezhzh)
      , ("zlzhzh", Zlzhzh)
      , ("zmzhzh", Zmzhzh)
      , ("zpzhzh", Zpzhzh)
      , ("ztzhzh", Ztzhzh)
      , ("zszhzh", Zszhzh)
      , ("negateDoublezh", NegateDoublezh)
      , ("double2Intzh", Double2Intzh)
      , ("double2Floatzh", Double2Floatzh)
      , ("tanhDoublezh", TanhDoublezh)
      , ("coshDoublezh", CoshDoublezh)
      , ("sinhDoublezh", SinhDoublezh)
      , ("atanDoublezh", AtanDoublezh)
      , ("logDoublezh", LogDoublezh)
      , ("expDoublezh", ExpDoublezh)
      , ("acosDoublezh", AcosDoublezh)
      , ("asinDoublezh", AsinDoublezh)
      , ("tanDoublezh", TanDoublezh)
      , ("cosDoublezh", CosDoublezh)
      , ("sinDoublezh", SinDoublezh)
      , ("sqrtDoublezh", SqrtDoublezh)
      , ("ztztzhzh", Ztztzhzh)
      , ("decodeDoublezu2Intzh", DecodeDoublezu2Intzh)
      , ("eqFloatzh", EqFloatzh)
      , ("neFloatzh", NeFloatzh)
      , ("geFloatzh", GeFloatzh)
      , ("gtFloatzh", GtFloatzh)
      , ("leFloatzh", LeFloatzh)
      , ("ltFloatzh", LtFloatzh)
      , ("minusFloatzh", MinusFloatzh)
      , ("plusFloatzh", PlusFloatzh)
      , ("timesFloatzh", TimesFloatzh)
      , ("divideFloatzh", DivideFloatzh)
      , ("negateFloatzh", NegateFloatzh)
      , ("float2Intzh", Float2Intzh)
      , ("float2Doublezh", Float2Doublezh)
      , ("tanhFloatzh", TanhFloatzh)
      , ("coshFloatzh", CoshFloatzh)
      , ("sinhFloatzh", SinhFloatzh)
      , ("atanFloatzh", AtanFloatzh)
      , ("logFloatzh", LogFloatzh)
      , ("expFloatzh", ExpFloatzh)
      , ("acosFloatzh", AcosFloatzh)
      , ("asinFloatzh", AsinFloatzh)
      , ("tanFloatzh", TanFloatzh)
      , ("cosFloatzh", CosFloatzh)
      , ("sinFloatzh", SinFloatzh)
      , ("sqrtFloatzh", SqrtFloatzh)
      , ("powerFloatzh", PowerFloatzh)
      , ("decodeFloatzuIntzh", DecodeFloatzuIntzh)
      , ("popCnt8zh", PopCnt8zh)
      , ("popCnt16zh", PopCnt16zh)
      , ("popCnt32zh", PopCnt32zh)
      , ("popCnt64zh", PopCnt64zh)
      , ("popCntzh", PopCntzh)
      , ("newArrayzh", NewArrayzh)
      , ("readArrayzh", ReadArrayzh)
      , ("writeArrayzh", WriteArrayzh)
      , ("sameMutableArrayzh", SameMutableArrayzh)
      , ("indexArrayzh", IndexArrayzh)
      , ("sizeofArrayzh", SizeofArrayzh)
      , ("sizeofMutableArrayzh", SizeofMutableArrayzh)
      , ("unsafeThawArrayzh", UnsafeThawArrayzh)
      , ("unsafeFreezzeArrayzh", UnsafeFreezzeArrayzh)
      , ("copyArrayzh", CopyArrayzh)
      , ("copyMutableArrayzh", CopyMutableArrayzh)
      , ("copyByteArrayzh", CopyByteArrayzh)
      , ("copyMutableByteArrayzh", CopyMutableByteArrayzh)
      , ("newImmutableArrayzh", NewImmutableArrayzh)
      , ("newStrictImmutableArrayzh", NewStrictImmutableArrayzh)
      , ("initImmutableArrayzh", InitImmutableArrayzh)
      , ("initStrictImmutableArrayzh", InitStrictImmutableArrayzh)
      , ("immutableArrayInitedzh", ImmutableArrayInitedzh)
      , ("strictImmutableArrayInitedzh", StrictImmutableArrayInitedzh)
      , ("sizzeofImmutableArrayzh", SizzeofImmutableArrayzh)
      , ("sizzeofStrictImmutableArrayzh", SizzeofStrictImmutableArrayzh)
      , ("indexImmutableArrayzh", IndexImmutableArrayzh)
      , ("indexStrictImmutableArrayzh", IndexStrictImmutableArrayzh)
      , ("newUnboxedWordArrayzh", NewUnboxedWordArrayzh)
      , ("newUnboxedWord8Arrayzh", NewUnboxedWord8Arrayzh)
      , ("newUnboxedWord16Arrayzh", NewUnboxedWord16Arrayzh)
      , ("newUnboxedWord32Arrayzh", NewUnboxedWord32Arrayzh)
      , ("newUnboxedWord64Arrayzh", NewUnboxedWord64Arrayzh)
      , ("newUnboxedIntArrayzh", NewUnboxedIntArrayzh)
      , ("newUnboxedInt8Arrayzh", NewUnboxedInt8Arrayzh)
      , ("newUnboxedInt16Arrayzh", NewUnboxedInt16Arrayzh)
      , ("newUnboxedInt32Arrayzh", NewUnboxedInt32Arrayzh)
      , ("newUnboxedInt64Arrayzh", NewUnboxedInt64Arrayzh)
      , ("newUnboxedFloatArrayzh", NewUnboxedFloatArrayzh)
      , ("newUnboxedDoubleArrayzh", NewUnboxedDoubleArrayzh)
      , ("newUnboxedCharArrayzh", NewUnboxedCharArrayzh)
      , ("newUnboxedAddrArrayzh", NewUnboxedAddrArrayzh)
      , ("initUnboxedWordArrayzh", InitUnboxedWordArrayzh)
      , ("initUnboxedWord8Arrayzh", InitUnboxedWord8Arrayzh)
      , ("initUnboxedWord16Arrayzh", InitUnboxedWord16Arrayzh)
      , ("initUnboxedWord32Arrayzh", InitUnboxedWord32Arrayzh)
      , ("initUnboxedWord64Arrayzh", InitUnboxedWord64Arrayzh)
      , ("initUnboxedIntArrayzh", InitUnboxedIntArrayzh)
      , ("initUnboxedInt8Arrayzh", InitUnboxedInt8Arrayzh)
      , ("initUnboxedInt16Arrayzh", InitUnboxedInt16Arrayzh)
      , ("initUnboxedInt32Arrayzh", InitUnboxedInt32Arrayzh)
      , ("initUnboxedInt64Arrayzh", InitUnboxedInt64Arrayzh)
      , ("initUnboxedFloatArrayzh", InitUnboxedFloatArrayzh)
      , ("initUnboxedDoubleArrayzh", InitUnboxedDoubleArrayzh)
      , ("initUnboxedCharArrayzh", InitUnboxedCharArrayzh)
      , ("initUnboxedAddrArrayzh", InitUnboxedAddrArrayzh)
      , ("unboxedWordArrayInitedzh", UnboxedWordArrayInitedzh)
      , ("unboxedWord8ArrayInitedzh", UnboxedWord8ArrayInitedzh)
      , ("unboxedWord16ArrayInitedzh", UnboxedWord16ArrayInitedzh)
      , ("unboxedWord32ArrayInitedzh", UnboxedWord32ArrayInitedzh)
      , ("unboxedWord64ArrayInitedzh", UnboxedWord64ArrayInitedzh)
      , ("unboxedIntArrayInitedzh", UnboxedIntArrayInitedzh)
      , ("unboxedInt8ArrayInitedzh", UnboxedInt8ArrayInitedzh)
      , ("unboxedInt16ArrayInitedzh", UnboxedInt16ArrayInitedzh)
      , ("unboxedInt32ArrayInitedzh", UnboxedInt32ArrayInitedzh)
      , ("unboxedInt64ArrayInitedzh", UnboxedInt64ArrayInitedzh)
      , ("unboxedFloatArrayInitedzh", UnboxedFloatArrayInitedzh)
      , ("unboxedDoubleArrayInitedzh", UnboxedDoubleArrayInitedzh)
      , ("unboxedCharArrayInitedzh", UnboxedCharArrayInitedzh)
      , ("unboxedAddrArrayInitedzh", UnboxedAddrArrayInitedzh)
      , ("sizeofUnboxedWordArrayzh", SizeofUnboxedWordArrayzh)
      , ("sizeofUnboxedWord8Arrayzh", SizeofUnboxedWord8Arrayzh)
      , ("sizeofUnboxedWord16Arrayzh", SizeofUnboxedWord16Arrayzh)
      , ("sizeofUnboxedWord32Arrayzh", SizeofUnboxedWord32Arrayzh)
      , ("sizeofUnboxedWord64Arrayzh", SizeofUnboxedWord64Arrayzh)
      , ("sizeofUnboxedIntArrayzh", SizeofUnboxedIntArrayzh)
      , ("sizeofUnboxedInt8Arrayzh", SizeofUnboxedInt8Arrayzh)
      , ("sizeofUnboxedInt16Arrayzh", SizeofUnboxedInt16Arrayzh)
      , ("sizeofUnboxedInt32Arrayzh", SizeofUnboxedInt32Arrayzh)
      , ("sizeofUnboxedInt64Arrayzh", SizeofUnboxedInt64Arrayzh)
      , ("sizeofUnboxedFloatArrayzh", SizeofUnboxedFloatArrayzh)
      , ("sizeofUnboxedDoubleArrayzh", SizeofUnboxedDoubleArrayzh)
      , ("sizeofUnboxedCharArrayzh", SizeofUnboxedCharArrayzh)
      , ("sizeofUnboxedAddrArrayzh", SizeofUnboxedAddrArrayzh)
      , ("indexUnboxedWordArrayzh", IndexUnboxedWordArrayzh)
      , ("indexUnboxedWord8Arrayzh", IndexUnboxedWord8Arrayzh)
      , ("indexUnboxedWord16Arrayzh", IndexUnboxedWord16Arrayzh)
      , ("indexUnboxedWord32Arrayzh", IndexUnboxedWord32Arrayzh)
      , ("indexUnboxedWord64Arrayzh", IndexUnboxedWord64Arrayzh)
      , ("indexUnboxedIntArrayzh", IndexUnboxedIntArrayzh)
      , ("indexUnboxedInt8Arrayzh", IndexUnboxedInt8Arrayzh)
      , ("indexUnboxedInt16Arrayzh", IndexUnboxedInt16Arrayzh)
      , ("indexUnboxedInt32Arrayzh", IndexUnboxedInt32Arrayzh)
      , ("indexUnboxedInt64Arrayzh", IndexUnboxedInt64Arrayzh)
      , ("indexUnboxedFloatArrayzh", IndexUnboxedFloatArrayzh)
      , ("indexUnboxedDoubleArrayzh", IndexUnboxedDoubleArrayzh)
      , ("indexUnboxedCharArrayzh", IndexUnboxedCharArrayzh)
      , ("indexUnboxedAddrArrayzh", IndexUnboxedAddrArrayzh)
      , ("newByteArrayzh", NewByteArrayzh)
      , ("newPinnedByteArrayzh", NewPinnedByteArrayzh)
      , ("newAlignedPinnedByteArrayzh", NewAlignedPinnedByteArrayzh)
      , ("byteArrayContentszh", ByteArrayContentszh)
      , ("sameMutableByteArrayzh", SameMutableByteArrayzh)
      , ("unsafeFreezzeByteArrayzh", UnsafeFreezzeByteArrayzh)
      , ("sizzeofByteArrayzh", SizzeofByteArrayzh)
      , ("sizzeofMutableByteArrayzh", SizzeofMutableByteArrayzh)
      , ("newUnboxedArrayzh", NewUnboxedArrayzh)
      , ("newPinnedUnboxedArrayzh", NewPinnedUnboxedArrayzh)
      , ("newAlignedPinnedUnboxedArrayzh", NewAlignedPinnedUnboxedArrayzh)
      , ("indexWord64Arrayzh", IndexWord64Arrayzh)
      , ("indexWord32Arrayzh", IndexWord32Arrayzh)
      , ("indexWord16Arrayzh", IndexWord16Arrayzh)
      , ("indexWord8Arrayzh", IndexWord8Arrayzh)
      , ("indexInt64Arrayzh", IndexInt64Arrayzh)
      , ("indexInt32Arrayzh", IndexInt32Arrayzh)
      , ("indexInt16Arrayzh", IndexInt16Arrayzh)
      , ("indexInt8Arrayzh", IndexInt8Arrayzh)
      , ("indexWordArrayzh", IndexWordArrayzh)
      , ("indexIntArrayzh", IndexIntArrayzh)
      , ("indexWideCharArrayzh", IndexWideCharArrayzh)
      , ("indexCharArrayzh", IndexCharArrayzh)
      , ("indexStablePtrArrayzh", IndexStablePtrArrayzh)
      , ("indexDoubleArrayzh", IndexDoubleArrayzh)
      , ("indexFloatArrayzh", IndexFloatArrayzh)
      , ("indexAddrArrayzh", IndexAddrArrayzh)
      , ("readWord64Arrayzh", ReadWord64Arrayzh)
      , ("readWord32Arrayzh", ReadWord32Arrayzh)
      , ("readWord16Arrayzh", ReadWord16Arrayzh)
      , ("readWord8Arrayzh", ReadWord8Arrayzh)
      , ("readInt64Arrayzh", ReadInt64Arrayzh)
      , ("readInt32Arrayzh", ReadInt32Arrayzh)
      , ("readInt16Arrayzh", ReadInt16Arrayzh)
      , ("readInt8Arrayzh", ReadInt8Arrayzh)
      , ("readStablePtrArrayzh", ReadStablePtrArrayzh)
      , ("readDoubleArrayzh", ReadDoubleArrayzh)
      , ("readFloatArrayzh", ReadFloatArrayzh)
      , ("readAddrArrayzh", ReadAddrArrayzh)
      , ("readWordArrayzh", ReadWordArrayzh)
      , ("readIntArrayzh", ReadIntArrayzh)
      , ("readWideCharArrayzh", ReadWideCharArrayzh)
      , ("readCharArrayzh", ReadCharArrayzh)
      , ("writeWord64Arrayzh", WriteWord64Arrayzh)
      , ("writeWord32Arrayzh", WriteWord32Arrayzh)
      , ("writeWord16Arrayzh", WriteWord16Arrayzh)
      , ("writeWord8Arrayzh", WriteWord8Arrayzh)
      , ("writeInt64Arrayzh", WriteInt64Arrayzh)
      , ("writeInt32Arrayzh", WriteInt32Arrayzh)
      , ("writeInt16Arrayzh", WriteInt16Arrayzh)
      , ("writeInt8Arrayzh", WriteInt8Arrayzh)
      , ("writeStablePtrArrayzh", WriteStablePtrArrayzh)
      , ("writeDoubleArrayzh", WriteDoubleArrayzh)
      , ("writeFloatArrayzh", WriteFloatArrayzh)
      , ("writeAddrArrayzh", WriteAddrArrayzh)
      , ("writeWordArrayzh", WriteWordArrayzh)
      , ("writeIntArrayzh", WriteIntArrayzh)
      , ("writeWideCharArrayzh", WriteWideCharArrayzh)
      , ("writeCharArrayzh", WriteCharArrayzh)
      , ("nullAddrzh", NullAddrzh)
      , ("plusAddrzh", PlusAddrzh)
      , ("minusAddrzh", MinusAddrzh)
      , ("remAddrzh", RemAddrzh)
      , ("addr2Intzh", Addr2Intzh)
      , ("int2Addrzh", Int2Addrzh)
      , ("gtAddrzh", GtAddrzh)
      , ("geAddrzh", GeAddrzh)
      , ("ltAddrzh", LtAddrzh)
      , ("leAddrzh", LeAddrzh)
      , ("eqAddrzh", EqAddrzh)
      , ("castToAddrzh", CastToAddrzh)
      , ("castFromAddrzh", CastFromAddrzh)
      , ("indexWord64OffAddrzh", IndexWord64OffAddrzh)
      , ("indexWord32OffAddrzh", IndexWord32OffAddrzh)
      , ("indexWord16OffAddrzh", IndexWord16OffAddrzh)
      , ("indexWord8OffAddrzh", IndexWord8OffAddrzh)
      , ("indexInt64OffAddrzh", IndexInt64OffAddrzh)
      , ("indexInt32OffAddrzh", IndexInt32OffAddrzh)
      , ("indexInt16OffAddrzh", IndexInt16OffAddrzh)
      , ("indexInt8OffAddrzh", IndexInt8OffAddrzh)
      , ("indexWordOffAddrzh", IndexWordOffAddrzh)
      , ("indexIntOffAddrzh", IndexIntOffAddrzh)
      , ("indexWideCharOffAddrzh", IndexWideCharOffAddrzh)
      , ("indexStablePtrOffAddrzh", IndexStablePtrOffAddrzh)
      , ("indexDoubleOffAddrzh", IndexDoubleOffAddrzh)
      , ("indexFloatOffAddrzh", IndexFloatOffAddrzh)
      , ("indexAddrOffAddrzh", IndexAddrOffAddrzh)
      , ("indexCharOffAddrzh", IndexCharOffAddrzh)
      , ("readWord64OffAddrzh", ReadWord64OffAddrzh)
      , ("readWord32OffAddrzh", ReadWord32OffAddrzh)
      , ("readWord16OffAddrzh", ReadWord16OffAddrzh)
      , ("readWord8OffAddrzh", ReadWord8OffAddrzh)
      , ("readInt64OffAddrzh", ReadInt64OffAddrzh)
      , ("readInt32OffAddrzh", ReadInt32OffAddrzh)
      , ("readInt16OffAddrzh", ReadInt16OffAddrzh)
      , ("readInt8OffAddrzh", ReadInt8OffAddrzh)
      , ("readWordOffAddrzh", ReadWordOffAddrzh)
      , ("readIntOffAddrzh", ReadIntOffAddrzh)
      , ("readWideCharOffAddrzh", ReadWideCharOffAddrzh)
      , ("readStablePtrOffAddrzh", ReadStablePtrOffAddrzh)
      , ("readDoubleOffAddrzh", ReadDoubleOffAddrzh)
      , ("readFloatOffAddrzh", ReadFloatOffAddrzh)
      , ("readAddrOffAddrzh", ReadAddrOffAddrzh)
      , ("readCharOffAddrzh", ReadCharOffAddrzh)
      , ("writeWord64OffAddrzh", WriteWord64OffAddrzh)
      , ("writeWord32OffAddrzh", WriteWord32OffAddrzh)
      , ("writeWord16OffAddrzh", WriteWord16OffAddrzh)
      , ("writeWord8OffAddrzh", WriteWord8OffAddrzh)
      , ("writeInt64OffAddrzh", WriteInt64OffAddrzh)
      , ("writeInt32OffAddrzh", WriteInt32OffAddrzh)
      , ("writeInt16OffAddrzh", WriteInt16OffAddrzh)
      , ("writeInt8OffAddrzh", WriteInt8OffAddrzh)
      , ("writeWordOffAddrzh", WriteWordOffAddrzh)
      , ("writeIntOffAddrzh", WriteIntOffAddrzh)
      , ("writeWideCharOffAddrzh", WriteWideCharOffAddrzh)
      , ("writeStablePtrOffAddrzh", WriteStablePtrOffAddrzh)
      , ("writeDoubleOffAddrzh", WriteDoubleOffAddrzh)
      , ("writeFloatOffAddrzh", WriteFloatOffAddrzh)
      , ("writeAddrOffAddrzh", WriteAddrOffAddrzh)
      , ("writeCharOffAddrzh", WriteCharOffAddrzh)
      , ("newMutVarzh", NewMutVarzh)
      , ("readMutVarzh", ReadMutVarzh)
      , ("writeMutVarzh", WriteMutVarzh)
      , ("sameMutVarzh", SameMutVarzh)
      , ("casMutVarzh", CasMutVarzh)
      , ("atomicModifyMutVarzh", AtomicModifyMutVarzh)
      , ("newTVarzh", NewTVarzh)
      , ("readTVarzh", ReadTVarzh)
      , ("readTVarIOzh", ReadTVarIOzh)
      , ("writeTVarzh", WriteTVarzh)
      , ("sameTVarzh", SameTVarzh)
      , ("atomicallyzh", Atomicallyzh)
      (*
      , ("retryzh", Retryzh)
      , ("catchRetryzh", CatchRetryzh)
      , ("checkzh", Checkzh)
      , ("catchSTMzh", CatchSTMzh)
      *)
      , ("catchzh", Catchzh)
      , ("raisezh", Raisezh)
      , ("raiseIOzh", RaiseIOzh)
      , ("maskAsyncExceptionszh", MaskAsyncExceptionszh)
      , ("maskUninterruptiblezh", MaskUninterruptiblezh)
      , ("unmaskAsyncExceptionszh", UnmaskAsyncExceptionszh)
      , ("getMaskingStatezh", GetMaskingStatezh)
      , ("newMVarzh", NewMVarzh)
      , ("takeMVarzh", TakeMVarzh)
      , ("tryTakeMVarzh", TryTakeMVarzh)
      , ("putMVarzh", PutMVarzh)
      , ("tryPutMVarzh", TryPutMVarzh)
      , ("sameMVarzh", SameMVarzh)
      , ("isEmptyMVarzh", IsEmptyMVarzh)
      , ("delayzh", Delayzh)
      , ("waitReadzh", WaitReadzh)
      , ("waitWritezh", WaitWritezh)
      , ("asyncReadzh", AsyncReadzh)
      , ("asyncWritezh", AsyncWritezh)
      , ("asyncDoProczh", AsyncDoProczh)
      , ("forkzh", Forkzh)
      , ("forkOnzh", ForkOnzh)
      , ("killThreadzh", KillThreadzh)
      , ("yieldzh", Yieldzh)
      , ("myThreadIdzh", MyThreadIdzh)
      , ("labelThreadzh", LabelThreadzh)
      , ("isCurrentThreadBoundzh", IsCurrentThreadBoundzh)
      , ("noDuplicatezh", NoDuplicatezh)
      , ("threadStatuszh", ThreadStatuszh)
      , ("mkWeakzh", MkWeakzh)
      , ("mkWeakForeignEnvzh", MkWeakForeignEnvzh)
      , ("deRefWeakzh", DeRefWeakzh)
      , ("finalizzeWeakzh", FinalizzeWeakzh)
      , ("touchzh", Touchzh)
      , ("makeStablePtrzh", MakeStablePtrzh)
      , ("deRefStablePtrzh", DeRefStablePtrzh)
      , ("freeStablePtrzh", FreeStablePtrzh)
      , ("eqStablePtrzh", EqStablePtrzh)
      (* 
      , ("makeStableNamezh", MakeStableNamezh)
      , ("eqStableNamezh", EqStableNamezh)
      , ("stableNameToIntzh", StableNameToIntzh)
      *)
      , ("reallyUnsafePtrEqualityzh", ReallyUnsafePtrEqualityzh)
      , ("seqzh", Seqzh)
      , ("dataToTagzh", DataToTagzh)
      , ("tagToEnumzh", TagToEnumzh)
      , ("traceEventzh", TraceEventzh)
      ]

  val fromString : string -> primOp option = fn v => StringDict.lookup (strTable, v)

  val toString : primOp -> string = 
     fn GtCharzh => "gtCharzh"
      | GeCharzh => "geCharzh"
      | EqCharzh => "eqCharzh"
      | NeCharzh => "neCharzh"
      | LtCharzh => "ltCharzh"
      | LeCharzh => "leCharzh"
      | Ordzh => "ordzh"
      | Integerzmzh => "integerzmzh"
      | Integerzpzh => "integerzpzh"
      | Integerztzh => "integerztzh"
      | NegateIntegerzh => "negateIntegerzh"
      | QuotIntegerzh => "quotIntegerzh"
      | RemIntegerzh => "remIntegerzh"
      | QuotRemIntegerzh => "quotRemIntegerzh"
      | Integerzezezh => "integerzezezh"
      | Integerzszezh => "integerzszezh"
      | Integerzgzezh => "integerzgzezh"
      | Integerzgzh => "integerzgzh"
      | Integerzlzezh => "integerzlzezh"
      | Integerzlzh => "integerzlzh"
      | Int2Integerzh => "int2Integerzh"
      | Int64ToIntegerzh => "int64ToIntegerzh"
      | Word2Integerzh => "word2Integerzh"
      | Word64ToIntegerzh => "word64ToIntegerzh"
      | Integer2Intzh => "integer2Intzh"
      | Integer2Int64zh => "integer2Int64zh"
      | Integer2Wordzh => "integer2Wordzh"
      | Integer2Word64zh => "integer2Word64zh"
      | Integer2Floatzh => "integer2Floatzh"
      | Integer2Doublezh => "integer2Doublezh"
      | IntegerAndzh => "integerAndzh"
      | IntegerOrzh => "integerOrzh"
      | IntegerXorzh => "integerXorzh"
      | IntegerIShiftLzh => "integerIShiftLzh"
      | IntegerIShiftRzh => "integerIShiftRzh"
      | IntegerEncodeFloatzh => "integerEncodeFloatzh"
      | IntegerEncodeDoublezh => "integerEncodeDoublezh"
      | Zmzh => "zmzh"
      | Zpzh => "zpzh"
      | Ztzh => "ztzh"
      | NegateIntzh => "negateIntzh"
      | QuotIntzh => "quotIntzh"
      | RemIntzh => "remIntzh"
      | QuotRemIntzh => "quotRemIntzh"
      | Zezezh => "zezezh"
      | Zszezh => "zszezh"
      | Zgzezh => "zgzezh"
      | Zgzh => "zgzh"
      | Zlzezh => "zlzezh"
      | Zlzh => "zlzh"
      | Chrzh => "chrzh"
      | Int2Wordzh => "int2Wordzh"
      | Int2Word64zh => "int2Word64zh"
      | Int2Floatzh => "int2Floatzh"
      | Int2Doublezh => "int2Doublezh"
      | UncheckedIShiftLzh => "uncheckedIShiftLzh"
      | UncheckedIShiftRAzh => "uncheckedIShiftRAzh"
      | UncheckedIShiftRLzh => "uncheckedIShiftRLzh"
      | AddIntCzh => "addIntCzh"
      | SubIntCzh => "subIntCzh"
      | MulIntMayOflozh => "mulIntMayOflozh"
      | MinusWordzh => "minusWordzh"
      | PlusWordzh => "plusWordzh"
      | TimesWordzh => "timesWordzh"
      | QuotWordzh => "quotWordzh"
      | RemWordzh => "remWordzh"
      | QuotRemWordzh => "quotRemWordzh"
      | Andzh => "andzh"
      | Orzh => "orzh"
      | Xorzh => "xorzh"
      | Notzh => "notzh"
      | Word2Intzh => "word2Intzh"
      | GtWordzh => "gtWordzh"
      | GeWordzh => "geWordzh"
      | EqWordzh => "eqWordzh"
      | NeWordzh => "neWordzh"
      | LtWordzh => "ltWordzh"
      | LeWordzh => "leWordzh"
      | UncheckedShiftLzh => "uncheckedShiftLzh"
      | UncheckedShiftRLzh => "uncheckedShiftRLzh"
      | Narrow8Intzh => "narrow8Intzh"
      | Narrow16Intzh => "narrow16Intzh"
      | Narrow32Intzh => "narrow32Intzh"
      | Narrow8Wordzh => "narrow8Wordzh"
      | Narrow16Wordzh => "narrow16Wordzh"
      | Narrow32Wordzh => "narrow32Wordzh"
      | Zezezhzh => "zezezhzh"
      | Zszezhzh => "zszezhzh"
      | Zgzezhzh => "zgzezhzh"
      | Zgzhzh => "zgzhzh"
      | Zlzezhzh => "zlzezhzh"
      | Zlzhzh => "zlzhzh"
      | Zmzhzh => "zmzhzh"
      | Zpzhzh => "zpzhzh"
      | Ztzhzh => "ztzhzh"
      | Zszhzh => "zszhzh"
      | NegateDoublezh => "negateDoublezh"
      | Double2Intzh => "double2Intzh"
      | Double2Floatzh => "double2Floatzh"
      | TanhDoublezh => "tanhDoublezh"
      | CoshDoublezh => "coshDoublezh"
      | SinhDoublezh => "sinhDoublezh"
      | AtanDoublezh => "atanDoublezh"
      | LogDoublezh => "logDoublezh"
      | ExpDoublezh => "expDoublezh"
      | AcosDoublezh => "acosDoublezh"
      | AsinDoublezh => "asinDoublezh"
      | TanDoublezh => "tanDoublezh"
      | CosDoublezh => "cosDoublezh"
      | SinDoublezh => "sinDoublezh"
      | SqrtDoublezh => "sqrtDoublezh"
      | Ztztzhzh => "ztztzhzh"
      | DecodeDoublezu2Intzh => "decodeDoublezu2Intzh"
      | EqFloatzh => "eqFloatzh"
      | NeFloatzh => "neFloatzh"
      | GeFloatzh => "geFloatzh"
      | GtFloatzh => "gtFloatzh"
      | LeFloatzh => "leFloatzh"
      | LtFloatzh => "ltFloatzh"
      | MinusFloatzh => "minusFloatzh"
      | PlusFloatzh => "plusFloatzh"
      | TimesFloatzh => "timesFloatzh"
      | DivideFloatzh => "divideFloatzh"
      | NegateFloatzh => "negateFloatzh"
      | Float2Intzh => "float2Intzh"
      | Float2Doublezh => "float2Doublezh"
      | TanhFloatzh => "tanhFloatzh"
      | CoshFloatzh => "coshFloatzh"
      | SinhFloatzh => "sinhFloatzh"
      | AtanFloatzh => "atanFloatzh"
      | LogFloatzh => "logFloatzh"
      | ExpFloatzh => "expFloatzh"
      | AcosFloatzh => "acosFloatzh"
      | AsinFloatzh => "asinFloatzh"
      | TanFloatzh => "tanFloatzh"
      | CosFloatzh => "cosFloatzh"
      | SinFloatzh => "sinFloatzh"
      | SqrtFloatzh => "sqrtFloatzh"
      | PowerFloatzh => "powerFloatzh"
      | DecodeFloatzuIntzh => "decodeFloatzuIntzh"
      | PopCnt8zh => "popCnt8zh"
      | PopCnt16zh => "popCnt16zh"
      | PopCnt32zh => "popCnt32zh"
      | PopCnt64zh => "popCnt64zh"
      | PopCntzh => "popCntzh"
      | NewArrayzh => "newArrayzh"
      | ReadArrayzh => "readArrayzh"
      | WriteArrayzh => "writeArrayzh"
      | SameMutableArrayzh => "sameMutableArrayzh"
      | IndexArrayzh => "indexArrayzh"
      | SizeofArrayzh => "sizeofArrayzh"
      | SizeofMutableArrayzh => "sizeofMutableArrayzh"
      | UnsafeThawArrayzh => "unsafeThawArrayzh"
      | UnsafeFreezzeArrayzh => "unsafeFreezzeArrayzh"
      | CopyArrayzh => "copyArrayzh"
      | CopyMutableArrayzh => "copyMutableArrayzh"
      | CopyByteArrayzh => "copyByteArrayzh"
      | CopyMutableByteArrayzh => "copyMutableByteArrayzh"
      | NewImmutableArrayzh => "newImmutableArrayzh"
      | NewStrictImmutableArrayzh => "newStrictImmutableArrayzh"
      | InitImmutableArrayzh => "initImmutableArrayzh"
      | InitStrictImmutableArrayzh => "initStrictImmutableArrayzh"
      | ImmutableArrayInitedzh => "immutableArrayInitedzh"
      | StrictImmutableArrayInitedzh => "strictImmutableArrayInitedzh"
      | SizzeofImmutableArrayzh => "sizzeofImmutableArrayzh"
      | SizzeofStrictImmutableArrayzh => "sizzeofStrictImmutableArrayzh"
      | IndexImmutableArrayzh => "indexImmutableArrayzh"
      | IndexStrictImmutableArrayzh => "indexStrictImmutableArrayzh"
      | NewUnboxedWordArrayzh => "newUnboxedWordArrayzh"
      | NewUnboxedWord8Arrayzh => "newUnboxedWord8Arrayzh"
      | NewUnboxedWord16Arrayzh => "newUnboxedWord16Arrayzh"
      | NewUnboxedWord32Arrayzh => "newUnboxedWord32Arrayzh"
      | NewUnboxedWord64Arrayzh => "newUnboxedWord64Arrayzh"
      | NewUnboxedIntArrayzh => "newUnboxedIntArrayzh"
      | NewUnboxedInt8Arrayzh => "newUnboxedInt8Arrayzh"
      | NewUnboxedInt16Arrayzh => "newUnboxedInt16Arrayzh"
      | NewUnboxedInt32Arrayzh => "newUnboxedInt32Arrayzh"
      | NewUnboxedInt64Arrayzh => "newUnboxedInt64Arrayzh"
      | NewUnboxedFloatArrayzh => "newUnboxedFloatArrayzh"
      | NewUnboxedDoubleArrayzh => "newUnboxedDoubleArrayzh"
      | NewUnboxedCharArrayzh => "newUnboxedCharArrayzh"
      | NewUnboxedAddrArrayzh => "newUnboxedAddrArrayzh"
      | InitUnboxedWordArrayzh => "initUnboxedWordArrayzh"
      | InitUnboxedWord8Arrayzh => "initUnboxedWord8Arrayzh"
      | InitUnboxedWord16Arrayzh => "initUnboxedWord16Arrayzh"
      | InitUnboxedWord32Arrayzh => "initUnboxedWord32Arrayzh"
      | InitUnboxedWord64Arrayzh => "initUnboxedWord64Arrayzh"
      | InitUnboxedIntArrayzh => "initUnboxedIntArrayzh"
      | InitUnboxedInt8Arrayzh => "initUnboxedInt8Arrayzh"
      | InitUnboxedInt16Arrayzh => "initUnboxedInt16Arrayzh"
      | InitUnboxedInt32Arrayzh => "initUnboxedInt32Arrayzh"
      | InitUnboxedInt64Arrayzh => "initUnboxedInt64Arrayzh"
      | InitUnboxedFloatArrayzh => "initUnboxedFloatArrayzh"
      | InitUnboxedDoubleArrayzh => "initUnboxedDoubleArrayzh"
      | InitUnboxedCharArrayzh => "initUnboxedCharArrayzh"
      | InitUnboxedAddrArrayzh => "initUnboxedAddrArrayzh"
      | UnboxedWordArrayInitedzh => "unboxedWordArrayInitedzh"
      | UnboxedWord8ArrayInitedzh => "unboxedWord8ArrayInitedzh"
      | UnboxedWord16ArrayInitedzh => "unboxedWord16ArrayInitedzh"
      | UnboxedWord32ArrayInitedzh => "unboxedWord32ArrayInitedzh"
      | UnboxedWord64ArrayInitedzh => "unboxedWord64ArrayInitedzh"
      | UnboxedIntArrayInitedzh => "unboxedIntArrayInitedzh"
      | UnboxedInt8ArrayInitedzh => "unboxedInt8ArrayInitedzh"
      | UnboxedInt16ArrayInitedzh => "unboxedInt16ArrayInitedzh"
      | UnboxedInt32ArrayInitedzh => "unboxedInt32ArrayInitedzh"
      | UnboxedInt64ArrayInitedzh => "unboxedInt64ArrayInitedzh"
      | UnboxedFloatArrayInitedzh => "unboxedFloatArrayInitedzh"
      | UnboxedDoubleArrayInitedzh => "unboxedDoubleArrayInitedzh"
      | UnboxedCharArrayInitedzh => "unboxedCharArrayInitedzh"
      | UnboxedAddrArrayInitedzh => "unboxedAddrArrayInitedzh"
      | SizeofUnboxedWordArrayzh => "sizeofUnboxedWordArrayzh"
      | SizeofUnboxedWord8Arrayzh => "sizeofUnboxedWord8Arrayzh"
      | SizeofUnboxedWord16Arrayzh => "sizeofUnboxedWord16Arrayzh"
      | SizeofUnboxedWord32Arrayzh => "sizeofUnboxedWord32Arrayzh"
      | SizeofUnboxedWord64Arrayzh => "sizeofUnboxedWord64Arrayzh"
      | SizeofUnboxedIntArrayzh => "sizeofUnboxedIntArrayzh"
      | SizeofUnboxedInt8Arrayzh => "sizeofUnboxedInt8Arrayzh"
      | SizeofUnboxedInt16Arrayzh => "sizeofUnboxedInt16Arrayzh"
      | SizeofUnboxedInt32Arrayzh => "sizeofUnboxedInt32Arrayzh"
      | SizeofUnboxedInt64Arrayzh => "sizeofUnboxedInt64Arrayzh"
      | SizeofUnboxedFloatArrayzh => "sizeofUnboxedFloatArrayzh"
      | SizeofUnboxedDoubleArrayzh => "sizeofUnboxedDoubleArrayzh"
      | SizeofUnboxedCharArrayzh => "sizeofUnboxedCharArrayzh"
      | SizeofUnboxedAddrArrayzh => "sizeofUnboxedAddrArrayzh"
      | IndexUnboxedWordArrayzh => "indexUnboxedWordArrayzh"
      | IndexUnboxedWord8Arrayzh => "indexUnboxedWord8Arrayzh"
      | IndexUnboxedWord16Arrayzh => "indexUnboxedWord16Arrayzh"
      | IndexUnboxedWord32Arrayzh => "indexUnboxedWord32Arrayzh"
      | IndexUnboxedWord64Arrayzh => "indexUnboxedWord64Arrayzh"
      | IndexUnboxedIntArrayzh => "indexUnboxedIntArrayzh"
      | IndexUnboxedInt8Arrayzh => "indexUnboxedInt8Arrayzh"
      | IndexUnboxedInt16Arrayzh => "indexUnboxedInt16Arrayzh"
      | IndexUnboxedInt32Arrayzh => "indexUnboxedInt32Arrayzh"
      | IndexUnboxedInt64Arrayzh => "indexUnboxedInt64Arrayzh"
      | IndexUnboxedFloatArrayzh => "indexUnboxedFloatArrayzh"
      | IndexUnboxedDoubleArrayzh => "indexUnboxedDoubleArrayzh"
      | IndexUnboxedCharArrayzh => "indexUnboxedCharArrayzh"
      | IndexUnboxedAddrArrayzh => "indexUnboxedAddrArrayzh"
      | NewByteArrayzh => "newByteArrayzh"
      | NewPinnedByteArrayzh => "newPinnedByteArrayzh"
      | NewAlignedPinnedByteArrayzh => "newAlignedPinnedByteArrayzh"
      | ByteArrayContentszh => "byteArrayContentszh"
      | SameMutableByteArrayzh => "sameMutableByteArrayzh"
      | UnsafeFreezzeByteArrayzh => "unsafeFreezzeByteArrayzh"
      | SizzeofByteArrayzh => "sizzeofByteArrayzh"
      | SizzeofMutableByteArrayzh => "sizzeofMutableByteArrayzh"
      | NewUnboxedArrayzh => "newUnboxedArrayzh"
      | NewPinnedUnboxedArrayzh => "newPinnedUnboxedArrayzh"
      | NewAlignedPinnedUnboxedArrayzh => "newAlignedPinnedUnboxedArrayzh"
      | IndexWord64Arrayzh => "indexWord64Arrayzh"
      | IndexWord32Arrayzh => "indexWord32Arrayzh"
      | IndexWord16Arrayzh => "indexWord16Arrayzh"
      | IndexWord8Arrayzh => "indexWord8Arrayzh"
      | IndexInt64Arrayzh => "indexInt64Arrayzh"
      | IndexInt32Arrayzh => "indexInt32Arrayzh"
      | IndexInt16Arrayzh => "indexInt16Arrayzh"
      | IndexInt8Arrayzh => "indexInt8Arrayzh"
      | IndexWordArrayzh => "indexWordArrayzh"
      | IndexIntArrayzh => "indexIntArrayzh"
      | IndexWideCharArrayzh => "indexWideCharArrayzh"
      | IndexCharArrayzh => "indexCharArrayzh"
      | IndexStablePtrArrayzh => "indexStablePtrArrayzh"
      | IndexDoubleArrayzh => "indexDoubleArrayzh"
      | IndexFloatArrayzh => "indexFloatArrayzh"
      | IndexAddrArrayzh => "indexAddrArrayzh"
      | ReadWord64Arrayzh => "readWord64Arrayzh"
      | ReadWord32Arrayzh => "readWord32Arrayzh"
      | ReadWord16Arrayzh => "readWord16Arrayzh"
      | ReadWord8Arrayzh => "readWord8Arrayzh"
      | ReadInt64Arrayzh => "readInt64Arrayzh"
      | ReadInt32Arrayzh => "readInt32Arrayzh"
      | ReadInt16Arrayzh => "readInt16Arrayzh"
      | ReadInt8Arrayzh => "readInt8Arrayzh"
      | ReadStablePtrArrayzh => "readStablePtrArrayzh"
      | ReadDoubleArrayzh => "readDoubleArrayzh"
      | ReadFloatArrayzh => "readFloatArrayzh"
      | ReadAddrArrayzh => "readAddrArrayzh"
      | ReadWordArrayzh => "readWordArrayzh"
      | ReadIntArrayzh => "readIntArrayzh"
      | ReadWideCharArrayzh => "readWideCharArrayzh"
      | ReadCharArrayzh => "readCharArrayzh"
      | WriteWord64Arrayzh => "writeWord64Arrayzh"
      | WriteWord32Arrayzh => "writeWord32Arrayzh"
      | WriteWord16Arrayzh => "writeWord16Arrayzh"
      | WriteWord8Arrayzh => "writeWord8Arrayzh"
      | WriteInt64Arrayzh => "writeInt64Arrayzh"
      | WriteInt32Arrayzh => "writeInt32Arrayzh"
      | WriteInt16Arrayzh => "writeInt16Arrayzh"
      | WriteInt8Arrayzh => "writeInt8Arrayzh"
      | WriteStablePtrArrayzh => "writeStablePtrArrayzh"
      | WriteDoubleArrayzh => "writeDoubleArrayzh"
      | WriteFloatArrayzh => "writeFloatArrayzh"
      | WriteAddrArrayzh => "writeAddrArrayzh"
      | WriteWordArrayzh => "writeWordArrayzh"
      | WriteIntArrayzh => "writeIntArrayzh"
      | WriteWideCharArrayzh => "writeWideCharArrayzh"
      | WriteCharArrayzh => "writeCharArrayzh"
      | NullAddrzh => "nullAddrzh"
      | PlusAddrzh => "plusAddrzh"
      | MinusAddrzh => "minusAddrzh"
      | RemAddrzh => "remAddrzh"
      | Addr2Intzh => "addr2Intzh"
      | Int2Addrzh => "int2Addrzh"
      | GtAddrzh => "gtAddrzh"
      | GeAddrzh => "geAddrzh"
      | LtAddrzh => "ltAddrzh"
      | LeAddrzh => "leAddrzh"
      | EqAddrzh => "eqAddrzh"
      | CastToAddrzh => "castToAddrzh"
      | CastFromAddrzh => "castFromAddrzh"
      | IndexWord64OffAddrzh => "indexWord64OffAddrzh"
      | IndexWord32OffAddrzh => "indexWord32OffAddrzh"
      | IndexWord16OffAddrzh => "indexWord16OffAddrzh"
      | IndexWord8OffAddrzh => "indexWord8OffAddrzh"
      | IndexInt64OffAddrzh => "indexInt64OffAddrzh"
      | IndexInt32OffAddrzh => "indexInt32OffAddrzh"
      | IndexInt16OffAddrzh => "indexInt16OffAddrzh"
      | IndexInt8OffAddrzh => "indexInt8OffAddrzh"
      | IndexWordOffAddrzh => "indexWordOffAddrzh"
      | IndexIntOffAddrzh => "indexIntOffAddrzh"
      | IndexWideCharOffAddrzh => "indexWideCharOffAddrzh"
      | IndexStablePtrOffAddrzh => "indexStablePtrOffAddrzh"
      | IndexDoubleOffAddrzh => "indexDoubleOffAddrzh"
      | IndexFloatOffAddrzh => "indexFloatOffAddrzh"
      | IndexAddrOffAddrzh => "indexAddrOffAddrzh"
      | IndexCharOffAddrzh => "indexCharOffAddrzh"
      | ReadWord64OffAddrzh => "readWord64OffAddrzh"
      | ReadWord32OffAddrzh => "readWord32OffAddrzh"
      | ReadWord16OffAddrzh => "readWord16OffAddrzh"
      | ReadWord8OffAddrzh => "readWord8OffAddrzh"
      | ReadInt64OffAddrzh => "readInt64OffAddrzh"
      | ReadInt32OffAddrzh => "readInt32OffAddrzh"
      | ReadInt16OffAddrzh => "readInt16OffAddrzh"
      | ReadInt8OffAddrzh => "readInt8OffAddrzh"
      | ReadWordOffAddrzh => "readWordOffAddrzh"
      | ReadIntOffAddrzh => "readIntOffAddrzh"
      | ReadWideCharOffAddrzh => "readWideCharOffAddrzh"
      | ReadStablePtrOffAddrzh => "readStablePtrOffAddrzh"
      | ReadDoubleOffAddrzh => "readDoubleOffAddrzh"
      | ReadFloatOffAddrzh => "readFloatOffAddrzh"
      | ReadAddrOffAddrzh => "readAddrOffAddrzh"
      | ReadCharOffAddrzh => "readCharOffAddrzh"
      | WriteWord64OffAddrzh => "writeWord64OffAddrzh"
      | WriteWord32OffAddrzh => "writeWord32OffAddrzh"
      | WriteWord16OffAddrzh => "writeWord16OffAddrzh"
      | WriteWord8OffAddrzh => "writeWord8OffAddrzh"
      | WriteInt64OffAddrzh => "writeInt64OffAddrzh"
      | WriteInt32OffAddrzh => "writeInt32OffAddrzh"
      | WriteInt16OffAddrzh => "writeInt16OffAddrzh"
      | WriteInt8OffAddrzh => "writeInt8OffAddrzh"
      | WriteWordOffAddrzh => "writeWordOffAddrzh"
      | WriteIntOffAddrzh => "writeIntOffAddrzh"
      | WriteWideCharOffAddrzh => "writeWideCharOffAddrzh"
      | WriteStablePtrOffAddrzh => "writeStablePtrOffAddrzh"
      | WriteDoubleOffAddrzh => "writeDoubleOffAddrzh"
      | WriteFloatOffAddrzh => "writeFloatOffAddrzh"
      | WriteAddrOffAddrzh => "writeAddrOffAddrzh"
      | WriteCharOffAddrzh => "writeCharOffAddrzh"
      | NewMutVarzh => "newMutVarzh"
      | ReadMutVarzh => "readMutVarzh"
      | WriteMutVarzh => "writeMutVarzh"
      | SameMutVarzh => "sameMutVarzh"
      | CasMutVarzh => "casMutVarzh"
      | AtomicModifyMutVarzh => "atomicModifyMutVarzh"
      | NewTVarzh => "newTVarzh"
      | ReadTVarzh => "readTVarzh"
      | ReadTVarIOzh => "readTVarIOzh"
      | WriteTVarzh => "writeTVarzh"
      | SameTVarzh => "sameTVarzh"
      | Atomicallyzh => "atomicallyzh"
      (*
      | Retryzh => "retryzh"
      | CatchRetryzh => "catchRetryzh"
      | Checkzh => "checkzh"
      | CatchSTMzh => "catchSTMzh"
      *)
      | Catchzh => "catchzh"
      | Raisezh => "raisezh"
      | RaiseIOzh => "raiseIOzh"
      | MaskAsyncExceptionszh => "maskAsyncExceptionszh"
      | MaskUninterruptiblezh => "maskUninterruptiblezh"
      | UnmaskAsyncExceptionszh => "unmaskAsyncExceptionszh"
      | GetMaskingStatezh => "getMaskingStatezh"
      | NewMVarzh => "newMVarzh"
      | TakeMVarzh => "takeMVarzh"
      | TryTakeMVarzh => "tryTakeMVarzh"
      | PutMVarzh => "putMVarzh"
      | TryPutMVarzh => "tryPutMVarzh"
      | SameMVarzh => "sameMVarzh"
      | IsEmptyMVarzh => "isEmptyMVarzh"
      | Delayzh => "delayzh"
      | WaitReadzh => "waitReadzh"
      | WaitWritezh => "waitWritezh"
      | AsyncReadzh => "asyncReadzh"
      | AsyncWritezh => "asyncWritezh"
      | AsyncDoProczh => "asyncDoProczh"
      | Forkzh => "forkzh"
      | ForkOnzh => "forkOnzh"
      | KillThreadzh => "killThreadzh"
      | Yieldzh => "yieldzh"
      | MyThreadIdzh => "myThreadIdzh"
      | LabelThreadzh => "labelThreadzh"
      | IsCurrentThreadBoundzh => "isCurrentThreadBoundzh"
      | NoDuplicatezh => "noDuplicatezh"
      | ThreadStatuszh => "threadStatuszh"
      | MkWeakzh => "mkWeakzh"
      | MkWeakForeignEnvzh => "mkWeakForeignEnvzh"
      | DeRefWeakzh => "deRefWeakzh"
      | FinalizzeWeakzh => "finalizzeWeakzh"
      | Touchzh => "touchzh"
      | MakeStablePtrzh => "makeStablePtrzh"
      | DeRefStablePtrzh => "deRefStablePtrzh"
      | FreeStablePtrzh => "freeStablePtrzh"
      | EqStablePtrzh => "eqStablePtrzh"
      (* 
      | MakeStableNamezh => "makeStableNamezh"
      | EqStableNamezh => "eqStableNamezh"
      | StableNameToIntzh => "stableNameToIntzh"
      *)
      | ReallyUnsafePtrEqualityzh => "reallyUnsafePtrEqualityzh"
      | Seqzh => "seqzh"
      | DataToTagzh => "dataToTagzh"
      | TagToEnumzh => "tagToEnumzh"
      | TraceEventzh => "traceEventzh"

  local
    structure CH = CoreHs
    structure CHU = CoreHsUtils
    structure CHP = CoreHsPrims

    val c = CHP.tCharzh
    val b = CHP.tBool
    val j = CHP.tIntegerzh
    val i = CHP.tIntzh
    val ii = CHP.tInt64zh
    val f = CHP.tFloatzh
    val d = CHP.tDoublezh
    val a = CHP.tAddrzh
    val w = CHP.tWordzh
    val ww = CHP.tWord64zh
    val da = CH.Tvar "a"
    val db = CH.Tvar "b"
    val dc = CH.Tvar "c"
    val ds = CH.Tvar "s"
    val s = CHP.tStatezh ds
    val rw = CH.Tcon CHP.tcRealWorld
    val srw = CHP.tStatezh rw
    val ba = CHP.tByteArrayzh
    val mba = CHP.tMutableByteArrayzh ds
    val ar = CHP.tArrayzh da
    val ia = CHP.tImmutableArrayzh da
    val sia = CHP.tStrictImmutableArrayzh da
    val uwa = CHP.tUnboxedWordArrayzh
    val uw8a = CHP.tUnboxedWord8Arrayzh
    val uw16a = CHP.tUnboxedWord16Arrayzh
    val uw32a = CHP.tUnboxedWord32Arrayzh
    val uw64a = CHP.tUnboxedWord64Arrayzh
    val uia = CHP.tUnboxedIntArrayzh
    val ui8a = CHP.tUnboxedInt8Arrayzh
    val ui16a = CHP.tUnboxedInt16Arrayzh
    val ui32a = CHP.tUnboxedInt32Arrayzh
    val ui64a = CHP.tUnboxedInt64Arrayzh
    val ufa = CHP.tUnboxedFloatArrayzh
    val uda = CHP.tUnboxedDoubleArrayzh
    val uca = CHP.tUnboxedCharArrayzh
    val uaa = CHP.tUnboxedAddrArrayzh
    val ma = CHP.tMutableArrayzh ds da
    val mv = CHP.tMVarzh ds da
    val muv = CHP.tMutVarzh ds da
    val sp = CHP.tStablePtrzh da
    val wp = CHP.tWeakPtrzh 
    val ti = CHP.tThreadIdzh
    val z0t = CHU.z0t
    fun z1h u = CHU.tUtuple [u]
    fun z2h (u, v) = CHU.tUtuple [u, v]
    fun z3h (u, v, w) = CHU.tUtuple [u, v, w]
    fun z4h (u, v, w, x) = CHU.tUtuple [u, v, w, x]
    fun ts a = z2h (s, a)
    fun tsrw a = z2h (srw, a)
    val arr = CHU.tArrow
    val tsr = z4h (srw,i,i,i)
    val toTy =
     fn GtCharzh => [c,c,b]
      | GeCharzh => [c,c,b]
      | EqCharzh => [c,c,b]
      | NeCharzh => [c,c,b]
      | LtCharzh => [c,c,b]
      | LeCharzh => [c,c,b]
      | Ordzh => [c,i]
      | Integerzmzh => [j,j,j]
      | Integerzpzh => [j,j,j]
      | Integerztzh => [j,j,j]
      | NegateIntegerzh => [j,j]
      | QuotIntegerzh => [j,j,j]
      | RemIntegerzh => [j,j,j]
      | QuotRemIntegerzh => [j,j,z2h(j,j)]
      | Integerzezezh => [j,j,b]
      | Integerzszezh => [j,j,b]
      | Integerzgzezh => [j,j,b]
      | Integerzgzh => [j,j,b]
      | Integerzlzezh => [j,j,b]
      | Integerzlzh => [j,j,b]
      | Int2Integerzh => [i,j]
      | Int64ToIntegerzh => [ii,j]
      | Word2Integerzh => [w,j]
      | Word64ToIntegerzh => [ww,j]
      | Integer2Intzh => [j,i]
      | Integer2Int64zh => [j,i]
      | Integer2Wordzh => [j,w]
      | Integer2Word64zh => [j,ww]
      | Integer2Floatzh => [j,f]
      | Integer2Doublezh => [j,d]
      | IntegerAndzh => [j,j,j]
      | IntegerOrzh => [j,j,j]
      | IntegerXorzh => [j,j,j]
      | IntegerIShiftLzh => [j,j,j]
      | IntegerIShiftRzh => [j,j,j]
      | IntegerEncodeFloatzh => [j,i,f]
      | IntegerEncodeDoublezh => [j,i,d]
      | Zmzh => [i,i,i]
      | Zpzh => [i,i,i]
      | Ztzh => [i,i,i]
      | NegateIntzh => [i,i]
      | QuotIntzh => [i,i,i]
      | RemIntzh => [i,i,i]
      | QuotRemIntzh => [i,i,z2h(i,i)]
      | Zezezh => [i,i,b]
      | Zszezh => [i,i,b]
      | Zgzezh => [i,i,b]
      | Zgzh => [i,i,b]
      | Zlzezh => [i,i,b]
      | Zlzh => [i,i,b]
      | Chrzh => [i,c]
      | Int2Wordzh => [i,w]
      | Int2Word64zh => [i,ww]
      | Int2Floatzh => [i,f]
      | Int2Doublezh => [i,d]
      | UncheckedIShiftLzh => [i,i,i]
      | UncheckedIShiftRAzh => [i,i,i]
      | UncheckedIShiftRLzh => [i,i,i]
      | AddIntCzh => [i,i,z2h(i,i)]
      | SubIntCzh => [i,i,z2h(i,i)]
      | MulIntMayOflozh => [i,i,i]
      | MinusWordzh => [w,w,w]
      | PlusWordzh => [w,w,w]
      | TimesWordzh => [w,w,w]
      | QuotWordzh => [w,w,w]
      | RemWordzh => [w,w,w]
      | QuotRemWordzh => [w,w,z2h(w,w)]
      | Andzh => [w,w,w]
      | Orzh => [w,w,w]
      | Xorzh => [w,w,w]
      | Notzh => [w,w]
      | Word2Intzh => [w,i]
      | GtWordzh => [w,w,b]
      | GeWordzh => [w,w,b]
      | EqWordzh => [w,w,b]
      | NeWordzh => [w,w,b]
      | LtWordzh => [w,w,b]
      | LeWordzh => [w,w,b]
      | UncheckedShiftLzh => [w,w,w]
      | UncheckedShiftRLzh => [w,w,w]
      | Narrow8Intzh => [i,i]
      | Narrow16Intzh => [i,i]
      | Narrow32Intzh => [i,i]
      | Narrow8Wordzh => [w,w]
      | Narrow16Wordzh => [w,w]
      | Narrow32Wordzh => [w,w]
      | Zezezhzh => [d,d,b]
      | Zszezhzh => [d,d,b]
      | Zgzezhzh => [d,d,b]
      | Zgzhzh => [d,d,b]
      | Zlzezhzh => [d,d,b]
      | Zlzhzh => [d,d,b]
      | Zmzhzh => [d,d,d]
      | Zpzhzh => [d,d,d]
      | Ztzhzh => [d,d,d]
      | Zszhzh => [d,d,d]
      | NegateDoublezh => [d,d]
      | Double2Intzh => [d,i]
      | Double2Floatzh => [d,f]
      | TanhDoublezh => [d,d]
      | CoshDoublezh => [d,d]
      | SinhDoublezh => [d,d]
      | AtanDoublezh => [d,d]
      | LogDoublezh => [d,d]
      | ExpDoublezh => [d,d]
      | AcosDoublezh => [d,d]
      | AsinDoublezh => [d,d]
      | TanDoublezh => [d,d]
      | CosDoublezh => [d,d]
      | SinDoublezh => [d,d]
      | SqrtDoublezh => [d,d]
      | Ztztzhzh => [d,d,d]
      | DecodeDoublezu2Intzh => [d,z4h (i,w,w,i)]
      | EqFloatzh => [f,f,b]
      | NeFloatzh => [f,f,b]
      | GeFloatzh => [f,f,b]
      | GtFloatzh => [f,f,b]
      | LeFloatzh => [f,f,b]
      | LtFloatzh => [f,f,b]
      | MinusFloatzh => [f,f,f]
      | PlusFloatzh => [f,f,f]
      | TimesFloatzh => [f,f,f]
      | DivideFloatzh => [f,f,f]
      | NegateFloatzh => [f,f]
      | Float2Intzh => [f,i]
      | Float2Doublezh => [f,d]
      | TanhFloatzh => [f,f]
      | CoshFloatzh => [f,f]
      | SinhFloatzh => [f,f]
      | AtanFloatzh => [f,f]
      | LogFloatzh => [f,f]
      | ExpFloatzh => [f,f]
      | AcosFloatzh => [f,f]
      | AsinFloatzh => [f,f]
      | TanFloatzh => [f,f]
      | CosFloatzh => [f,f]
      | SinFloatzh => [f,f]
      | SqrtFloatzh => [f,f]
      | PowerFloatzh => [f,f,f]
      | DecodeFloatzuIntzh => [d,z2h (i,i)]
      | PopCnt8zh => [w,w]
      | PopCnt16zh => [w,w]
      | PopCnt32zh => [w,w]
      | PopCnt64zh => [ww,w]
      | PopCntzh => [w,w]
      | NewArrayzh => [i,da,s,ts ma]
      | ReadArrayzh => [ma,i,s,ts da]
      | WriteArrayzh => [ma,i,da,s,s]
      | SameMutableArrayzh => [ma,ma,b]
      | IndexArrayzh => [ar,i,z1h da]
      | SizeofArrayzh => [ar,i]
      | SizeofMutableArrayzh => [ma,i]
      | UnsafeThawArrayzh => [ar,s,ts ma]
      | UnsafeFreezzeArrayzh => [ma,s,ts ar]
      | CopyArrayzh => [ar,i,ma,i,i,s,s]
      | CopyMutableArrayzh => [ma,i,ma,i,i,s,s]
      | CopyByteArrayzh => [ba,i,mba,i,i,s,s]
      | CopyMutableByteArrayzh => [mba,i,mba,i,i,s,s]
      | NewImmutableArrayzh => [i,s,ts ia]
      | NewStrictImmutableArrayzh => [i,s,ts sia]
      | InitImmutableArrayzh => [ia,i,da,s,s]
      | InitStrictImmutableArrayzh => [sia,i,da,s,s]
      | ImmutableArrayInitedzh => [ia,s,ts ia]
      | StrictImmutableArrayInitedzh => [sia,s,ts sia]
      | SizzeofImmutableArrayzh => [ia,i]
      | SizzeofStrictImmutableArrayzh => [sia,i]
      | IndexImmutableArrayzh => [ia,i,z1h da]
      | IndexStrictImmutableArrayzh => [sia,i,z1h da]
      | NewUnboxedWordArrayzh => [i,s,ts uwa]
      | NewUnboxedWord8Arrayzh => [i,s,ts uw8a]
      | NewUnboxedWord16Arrayzh => [i,s,ts uw16a]
      | NewUnboxedWord32Arrayzh => [i,s,ts uw32a]
      | NewUnboxedWord64Arrayzh => [i,s,ts uw64a]
      | NewUnboxedIntArrayzh => [i,s,ts uia]
      | NewUnboxedInt8Arrayzh => [i,s,ts ui8a]
      | NewUnboxedInt16Arrayzh => [i,s,ts ui16a]
      | NewUnboxedInt32Arrayzh => [i,s,ts ui32a]
      | NewUnboxedInt64Arrayzh => [i,s,ts ui64a]
      | NewUnboxedFloatArrayzh => [i,s,ts ufa]
      | NewUnboxedDoubleArrayzh => [i,s,ts uda]
      | NewUnboxedCharArrayzh => [i,s,ts uca]
      | NewUnboxedAddrArrayzh => [i,s,ts uaa]
      | InitUnboxedWordArrayzh => [uwa,i,w,s,s]
      | InitUnboxedWord8Arrayzh => [uw8a,i,w,s,s]
      | InitUnboxedWord16Arrayzh => [uw16a,i,w,s,s]
      | InitUnboxedWord32Arrayzh => [uw32a,i,w,s,s]
      | InitUnboxedWord64Arrayzh => [uw64a,i,ww,s,s]
      | InitUnboxedIntArrayzh => [uia,i,i,s,s]
      | InitUnboxedInt8Arrayzh => [ui8a,i,i,s,s]
      | InitUnboxedInt16Arrayzh => [ui16a,i,i,s,s]
      | InitUnboxedInt32Arrayzh => [ui32a,i,i,s,s]
      | InitUnboxedInt64Arrayzh => [ui64a,i,ii,s,s]
      | InitUnboxedFloatArrayzh => [ufa,i,f,s,s]
      | InitUnboxedDoubleArrayzh => [uda,i,d,s,s]
      | InitUnboxedCharArrayzh => [uca,i,c,s,s]
      | InitUnboxedAddrArrayzh => [uaa,i,a,s,s]
      | UnboxedWordArrayInitedzh => [uwa,s,ts uwa]
      | UnboxedWord8ArrayInitedzh => [uw8a,s,ts uw8a]
      | UnboxedWord16ArrayInitedzh => [uw16a,s,ts uw16a]
      | UnboxedWord32ArrayInitedzh => [uw32a,s,ts uw32a]
      | UnboxedWord64ArrayInitedzh => [uw64a,s,ts uw64a]
      | UnboxedIntArrayInitedzh => [uia,s,ts uia]
      | UnboxedInt8ArrayInitedzh => [ui8a,s,ts ui8a]
      | UnboxedInt16ArrayInitedzh => [ui16a,s,ts ui16a]
      | UnboxedInt32ArrayInitedzh => [ui32a,s,ts ui32a]
      | UnboxedInt64ArrayInitedzh => [ui64a,s,ts ui64a]
      | UnboxedFloatArrayInitedzh => [ufa,s,ts ufa]
      | UnboxedDoubleArrayInitedzh => [uda,s,ts uda]
      | UnboxedCharArrayInitedzh => [uca,s,ts uca]
      | UnboxedAddrArrayInitedzh => [uaa,s,ts uaa]
      | SizeofUnboxedWordArrayzh => [uwa,i]
      | SizeofUnboxedWord8Arrayzh => [uw8a,i]
      | SizeofUnboxedWord16Arrayzh => [uw16a,i]
      | SizeofUnboxedWord32Arrayzh => [uw32a,i]
      | SizeofUnboxedWord64Arrayzh => [uw64a,i]
      | SizeofUnboxedIntArrayzh => [uia,i]
      | SizeofUnboxedInt8Arrayzh => [ui8a,i]
      | SizeofUnboxedInt16Arrayzh => [ui16a,i]
      | SizeofUnboxedInt32Arrayzh => [ui32a,i]
      | SizeofUnboxedInt64Arrayzh => [ui64a,i]
      | SizeofUnboxedFloatArrayzh => [ufa,i]
      | SizeofUnboxedDoubleArrayzh => [uda,i]
      | SizeofUnboxedCharArrayzh => [uca,i]
      | SizeofUnboxedAddrArrayzh => [uaa,i]
      | IndexUnboxedWordArrayzh => [uwa,i,w]
      | IndexUnboxedWord8Arrayzh => [uw8a,i,w]
      | IndexUnboxedWord16Arrayzh => [uw16a,i,w]
      | IndexUnboxedWord32Arrayzh => [uw32a,i,w]
      | IndexUnboxedWord64Arrayzh => [uw64a,i,ww]
      | IndexUnboxedIntArrayzh => [uia,i,i]
      | IndexUnboxedInt8Arrayzh => [ui8a,i,i]
      | IndexUnboxedInt16Arrayzh => [ui16a,i,i]
      | IndexUnboxedInt32Arrayzh => [ui32a,i,i]
      | IndexUnboxedInt64Arrayzh => [ui64a,i,ii]
      | IndexUnboxedFloatArrayzh => [ufa,i,f]
      | IndexUnboxedDoubleArrayzh => [uda,i,d]
      | IndexUnboxedCharArrayzh => [uca,i,c]
      | IndexUnboxedAddrArrayzh => [uaa,i,a]
      | NewByteArrayzh => [i,s,ts mba]
      | NewPinnedByteArrayzh => [i,s,ts mba]
      | NewAlignedPinnedByteArrayzh => [i,i,s,ts mba]
      | ByteArrayContentszh => [ba,a]
      | SameMutableByteArrayzh => [mba,mba,b]
      | UnsafeFreezzeByteArrayzh => [mba,s,ts ba]
      | SizzeofByteArrayzh => [ba,i]
      | SizzeofMutableByteArrayzh => [mba,i]
      | NewUnboxedArrayzh => [i,da,s,ts mba]
      | NewPinnedUnboxedArrayzh => [i,da,s,ts mba]
      | NewAlignedPinnedUnboxedArrayzh => [i,da,i,s,ts mba]
      | IndexWord64Arrayzh => [ba,i,ww]
      | IndexWord32Arrayzh => [ba,i,w]
      | IndexWord16Arrayzh => [ba,i,w]
      | IndexWord8Arrayzh => [ba,i,w]
      | IndexInt64Arrayzh => [ba,i,ii]
      | IndexInt32Arrayzh => [ba,i,i]
      | IndexInt16Arrayzh => [ba,i,i]
      | IndexInt8Arrayzh => [ba,i,i]
      | IndexWordArrayzh => [ba,i,w]
      | IndexIntArrayzh => [ba,i,i]
      | IndexWideCharArrayzh => [ba,i,c]
      | IndexCharArrayzh => [ba,i,c]
      | IndexStablePtrArrayzh => [ba,i,sp]
      | IndexDoubleArrayzh => [ba,i,d]
      | IndexFloatArrayzh => [ba,i,f]
      | IndexAddrArrayzh => [ba,i,a]
      | ReadWord64Arrayzh => [mba,i,s,ts ww]
      | ReadWord32Arrayzh => [mba,i,s,ts w]
      | ReadWord16Arrayzh => [mba,i,s,ts w]
      | ReadWord8Arrayzh => [mba,i,s,ts w]
      | ReadInt64Arrayzh => [mba,i,s,ts ii]
      | ReadInt32Arrayzh => [mba,i,s,ts i]
      | ReadInt16Arrayzh => [mba,i,s,ts i]
      | ReadInt8Arrayzh => [mba,i,s,ts i]
      | ReadStablePtrArrayzh => [mba,i,s,ts sp]
      | ReadDoubleArrayzh => [mba,i,s,ts d]
      | ReadFloatArrayzh => [mba,i,s,ts f]
      | ReadAddrArrayzh => [mba,i,s,ts a]
      | ReadWordArrayzh => [mba,i,s,ts w]
      | ReadIntArrayzh => [mba,i,s,ts i]
      | ReadWideCharArrayzh => [mba,i,s,ts c]
      | ReadCharArrayzh => [mba,i,s,ts c]
      | WriteWord64Arrayzh => [mba,i,ww,s,s]
      | WriteWord32Arrayzh => [mba,i,w,s,s]
      | WriteWord16Arrayzh => [mba,i,w,s,s]
      | WriteWord8Arrayzh => [mba,i,w,s,s]
      | WriteInt64Arrayzh => [mba,i,ii,s,s]
      | WriteInt32Arrayzh => [mba,i,i,s,s]
      | WriteInt16Arrayzh => [mba,i,i,s,s]
      | WriteInt8Arrayzh => [mba,i,i,s,s]
      | WriteStablePtrArrayzh => [mba,i,sp,s,s]
      | WriteDoubleArrayzh => [mba,i,d,s,s]
      | WriteFloatArrayzh => [mba,i,f,s,s]
      | WriteAddrArrayzh => [mba,i,a,s,s]
      | WriteWordArrayzh => [mba,i,w,s,s]
      | WriteIntArrayzh => [mba,i,i,s,s]
      | WriteWideCharArrayzh => [mba,i,c,s,s]
      | WriteCharArrayzh => [mba,i,c,s,s]
      | NullAddrzh => [a]
      | PlusAddrzh => [a,i,a]
      | MinusAddrzh => [a,a,i]
      | RemAddrzh => [a,i,i]
      | Addr2Intzh => [a,i]
      | Int2Addrzh => [i,a]
      | GtAddrzh => [a,a,b]
      | GeAddrzh => [a,a,b]
      | LtAddrzh => [a,a,b]
      | LeAddrzh => [a,a,b]
      | EqAddrzh => [a,a,b]
      | CastToAddrzh => [da,a]
      | CastFromAddrzh => [a,da]
      | IndexWord64OffAddrzh => [a,i,ww]
      | IndexWord32OffAddrzh => [a,i,w]
      | IndexWord16OffAddrzh => [a,i,w]
      | IndexWord8OffAddrzh => [a,i,w]
      | IndexInt64OffAddrzh => [a,i,ii]
      | IndexInt32OffAddrzh => [a,i,i]
      | IndexInt16OffAddrzh => [a,i,i]
      | IndexInt8OffAddrzh => [a,i,i]
      | IndexWordOffAddrzh => [a,i,w]
      | IndexIntOffAddrzh => [a,i,i]
      | IndexWideCharOffAddrzh => [a,i,c]
      | IndexStablePtrOffAddrzh => [a,i,sp]
      | IndexDoubleOffAddrzh => [a,i,d]
      | IndexFloatOffAddrzh => [a,i,f]
      | IndexAddrOffAddrzh => [a,i,a]
      | IndexCharOffAddrzh => [a,i,c]
      | ReadWord64OffAddrzh => [a,i,s,ts ww]
      | ReadWord32OffAddrzh => [a,i,s,ts w]
      | ReadWord16OffAddrzh => [a,i,s,ts w]
      | ReadWord8OffAddrzh => [a,i,s,ts w]
      | ReadInt64OffAddrzh => [a,i,s,ts ii]
      | ReadInt32OffAddrzh => [a,i,s,ts i]
      | ReadInt16OffAddrzh => [a,i,s,ts i]
      | ReadInt8OffAddrzh => [a,i,s,ts i]
      | ReadWordOffAddrzh => [a,i,s,ts w]
      | ReadIntOffAddrzh => [a,i,s,ts i]
      | ReadWideCharOffAddrzh => [a,i,s,ts c]
      | ReadStablePtrOffAddrzh => [a,i,s,ts sp]
      | ReadDoubleOffAddrzh => [a,i,s,ts d]
      | ReadFloatOffAddrzh => [a,i,s,ts f]
      | ReadAddrOffAddrzh => [a,i,s,ts a]
      | ReadCharOffAddrzh => [a,i,s,ts c]
      | WriteWord64OffAddrzh => [a,i,ww,s,s]
      | WriteWord32OffAddrzh => [a,i,w,s,s]
      | WriteWord16OffAddrzh => [a,i,w,s,s]
      | WriteWord8OffAddrzh => [a,i,w,s,s]
      | WriteInt64OffAddrzh => [a,i,ii,s,s]
      | WriteInt32OffAddrzh => [a,i,i,s,s]
      | WriteInt16OffAddrzh => [a,i,i,s,s]
      | WriteInt8OffAddrzh => [a,i,i,s,s]
      | WriteWordOffAddrzh => [a,i,w,s,s]
      | WriteIntOffAddrzh => [a,i,i,s,s]
      | WriteWideCharOffAddrzh => [a,i,c,s,s]
      | WriteStablePtrOffAddrzh => [a,i,sp,s,s]
      | WriteDoubleOffAddrzh => [a,i,d,s,s]
      | WriteFloatOffAddrzh => [a,i,f,s,s]
      | WriteAddrOffAddrzh => [a,i,a,s,s]
      | WriteCharOffAddrzh => [a,i,c,s,s]
      | NewMutVarzh => [da,s,ts muv]
      | ReadMutVarzh => [muv,s,ts da]
      | WriteMutVarzh => [muv,da,s,s]
      | SameMutVarzh => [muv,muv,b]
      | CasMutVarzh => [muv,da,da,da,s,z3h (s,i,da)]
      | AtomicModifyMutVarzh => [muv,arr(da,db),s,ts dc]
      | NewTVarzh => [da,s,ts muv]
      | ReadTVarzh => [muv,s,ts da]
      | ReadTVarIOzh => [muv,s,ts da]
      | WriteTVarzh => [muv,da,s,s]
      | SameTVarzh => [muv,muv,b]
      | Atomicallyzh => [arr(srw,tsrw da),srw,tsrw da]
      (*
      | Retryzh => [srw,tsrw da]
      | CatchRetryzh => [arr(srw,tsrw da),arr(srw,tsrw da),srw,tsrw da]
      | Checkzh => [arr(srw,tsrw da),srw,tsrw,tsrw]
      | CatchSTMzh => [arr(srw,tsrw da),arr(db,arr(srw,tsrw da)),srw,tsrw da]
      *)
      | Catchzh => [arr(srw,tsrw da),arr(db,arr(srw,tsrw da)),srw,tsrw da]
      | Raisezh => [da,db]
      | RaiseIOzh => [da,srw,tsrw db]
      | MaskAsyncExceptionszh => [arr(srw,tsrw da),srw,tsrw da]
      | MaskUninterruptiblezh => [arr(srw,tsrw da),srw,tsrw da]
      | UnmaskAsyncExceptionszh => [arr(srw,tsrw da),srw,tsrw da]
      | GetMaskingStatezh => [srw,tsrw i]
      | NewMVarzh => [s,ts mv]
      | TakeMVarzh => [mv,s,ts da]
      | TryTakeMVarzh => [mv,s,z3h (s,i,da)]
      | PutMVarzh => [mv,da,s,s]
      | TryPutMVarzh => [mv,da,s,ts i]
      | SameMVarzh => [mv,mv,b]
      | IsEmptyMVarzh => [mv,s,ts i]
      | Delayzh => [i,s,s]
      | WaitReadzh => [i,s,s]
      | WaitWritezh => [i,s,s]
      | AsyncReadzh => [i,i,i,a,srw,z3h (srw,i,i)]
      | AsyncWritezh => [i,i,i,a,srw,z3h (srw,i,i)]
      | AsyncDoProczh => [a,a,srw,z3h (srw,i,i)]
      | Forkzh => [da,srw,tsrw ti]
      | ForkOnzh => [i,da,srw,tsrw ti]
      | KillThreadzh => [ti,da,srw,srw]
      | Yieldzh => [srw,srw]
      | MyThreadIdzh => [srw,tsrw ti]
      | LabelThreadzh => [ti,a,srw,srw]
      | IsCurrentThreadBoundzh => [srw,tsrw i]
      | NoDuplicatezh => [srw,srw]
      | ThreadStatuszh => [ti,srw,tsr]
      | MkWeakzh => [da,db,dc,srw,tsrw (wp db)]
      | MkWeakForeignEnvzh => [da,db,a,a,i,a,srw,tsrw w]
      | DeRefWeakzh => [wp da,srw,z3h (srw,i,da)]
      | FinalizzeWeakzh => [wp da,srw,z3h (srw,i,arr(srw,tsrw z0t))]
      | Touchzh => [da,srw,srw]
      | MakeStablePtrzh => [da,srw,tsrw sp]
      | DeRefStablePtrzh => [sp,srw,tsrw da]
      | FreeStablePtrzh => [sp,srw,srw]
      | EqStablePtrzh => [sp,sp,b]
      (* 
      | MakeStableNamezh => [da,s,ts sn]
      | EqStableNamezh => [sn,sn,b]
      | StableNameToIntzh => [sn,i]
      *)
      | ReallyUnsafePtrEqualityzh => [da,da,b]
      | Seqzh => [da,s,ts da]
      | DataToTagzh => [da,i]
      | TagToEnumzh => [i,da]
      | TraceEventzh => [a, s, s]

    fun mkTy tys =
      let
        fun getTvar (tvs, CH.Tvar v) = if List.exists (tvs, fn u => u = v) then tvs else (v :: tvs)
          | getTvar (tvs, CH.Tapp (t1, t2)) = getTvar (getTvar (tvs, t2), t1)
          | getTvar (tvs, c) = tvs
        fun mkFun [t] = t
          | mkFun (t::ts) = arr (t, mkFun ts)
          | mkFun []  = Fail.fail ("GHCPrimOp", "mkFun", "impossible: empty type list")
        val ty = mkFun tys
        val tvs = getTvar ([], ty)
      in
        List.fold (tvs, ty, fn (v, t) => CH.Tforall ((v, CH.Klifted), t))
      end
  in
    val getType : primOp -> CoreHs.ty = mkTy o toTy
  end

  fun effects p =
    let
      val xc  = Effect.Control
      val xt  = Effect.Total
      val xr  = Effect.ReadOnly
      val xw  = Effect.Heap
      val xrw = Effect.Heap
      val xf  = Effect.Control
      val xh  = Effect.Heap
      val xa  = Effect.Any
      val xig = Effect.InitGenS
      val xir = Effect.InitReadS
      val xiw = Effect.InitWriteS
      val xio = Effect.IoS
    in
      case p
       of GtCharzh => xt
        | GeCharzh => xt
        | EqCharzh => xt
        | NeCharzh => xt
        | LtCharzh => xt
        | LeCharzh => xt
        | Ordzh => xt
        | Integerzmzh => xt
        | Integerzpzh => xt
        | Integerztzh => xt
        | NegateIntegerzh => xt
        | QuotIntegerzh => xt
        | RemIntegerzh => xt
        | QuotRemIntegerzh => xt
        | Integerzezezh => xt
        | Integerzszezh => xt
        | Integerzgzezh => xt
        | Integerzgzh => xt
        | Integerzlzezh => xt
        | Integerzlzh => xt
        | Int2Integerzh => xt
        | Int64ToIntegerzh => xt
        | Word2Integerzh => xt
        | Word64ToIntegerzh => xt
        | Integer2Intzh => xt
        | Integer2Int64zh => xt
        | Integer2Wordzh => xt
        | Integer2Word64zh => xt
        | Integer2Floatzh => xt
        | Integer2Doublezh => xt
        | IntegerAndzh => xt
        | IntegerOrzh => xt
        | IntegerXorzh => xt
        | IntegerIShiftLzh => xt
        | IntegerIShiftRzh => xt
        | IntegerEncodeFloatzh => xt
        | IntegerEncodeDoublezh => xt
        | Zmzh => xt
        | Zpzh => xt
        | Ztzh => xt
        | NegateIntzh => xt
        | QuotIntzh => xt
        | RemIntzh => xt
        | QuotRemIntzh => xt
        | Zezezh => xt
        | Zszezh => xt
        | Zgzezh => xt
        | Zgzh => xt
        | Zlzezh => xt
        | Zlzh => xt
        | Chrzh => xt
        | Int2Wordzh => xt
        | Int2Word64zh => xt
        | Int2Floatzh => xt
        | Int2Doublezh => xt
        | UncheckedIShiftLzh => xt
        | UncheckedIShiftRAzh => xt
        | UncheckedIShiftRLzh => xt
        | AddIntCzh => xt
        | SubIntCzh => xt
        | MulIntMayOflozh => xt
        | MinusWordzh => xt
        | PlusWordzh => xt
        | TimesWordzh => xt
        | QuotWordzh => xt
        | RemWordzh => xt
        | QuotRemWordzh => xt
        | Andzh => xt
        | Orzh => xt
        | Xorzh => xt
        | Notzh => xt
        | Word2Intzh => xt
        | GtWordzh => xt
        | GeWordzh => xt
        | EqWordzh => xt
        | NeWordzh => xt
        | LtWordzh => xt
        | LeWordzh => xt
        | UncheckedShiftLzh => xt
        | UncheckedShiftRLzh => xt
        | Narrow8Intzh => xt
        | Narrow16Intzh => xt
        | Narrow32Intzh => xt
        | Narrow8Wordzh => xt
        | Narrow16Wordzh => xt
        | Narrow32Wordzh => xt
        | Zezezhzh => xt
        | Zszezhzh => xt
        | Zgzezhzh => xt
        | Zgzhzh => xt
        | Zlzezhzh => xt
        | Zlzhzh => xt
        | Zmzhzh => xt
        | Zpzhzh => xt
        | Ztzhzh => xt
        | Zszhzh => xt
        | NegateDoublezh => xt
        | Double2Intzh => xt
        | Double2Floatzh => xt
        | TanhDoublezh => xt
        | CoshDoublezh => xt
        | SinhDoublezh => xt
        | AtanDoublezh => xt
        | LogDoublezh => xt
        | ExpDoublezh => xt
        | AcosDoublezh => xt
        | AsinDoublezh => xt
        | TanDoublezh => xt
        | CosDoublezh => xt
        | SinDoublezh => xt
        | SqrtDoublezh => xt
        | Ztztzhzh => xt
        | DecodeDoublezu2Intzh => xr
        | EqFloatzh => xt
        | NeFloatzh => xt
        | GeFloatzh => xt
        | GtFloatzh => xt
        | LeFloatzh => xt
        | LtFloatzh => xt
        | MinusFloatzh => xt
        | PlusFloatzh => xt
        | TimesFloatzh => xt
        | DivideFloatzh => xt
        | NegateFloatzh => xt
        | Float2Intzh => xt
        | Float2Doublezh => xt
        | TanhFloatzh => xt
        | CoshFloatzh => xt
        | SinhFloatzh => xt
        | AtanFloatzh => xt
        | LogFloatzh => xt
        | ExpFloatzh => xt
        | AcosFloatzh => xt
        | AsinFloatzh => xt
        | TanFloatzh => xt
        | CosFloatzh => xt
        | SinFloatzh => xt
        | SqrtFloatzh => xt
        | PowerFloatzh => xt
        | DecodeFloatzuIntzh => xr
        | PopCnt8zh => xt
        | PopCnt16zh => xt
        | PopCnt32zh => xt
        | PopCnt64zh => xt
        | PopCntzh => xt
        | NewArrayzh => xh
        | ReadArrayzh => xh
        | WriteArrayzh => xw
        | SameMutableArrayzh => xt
        | IndexArrayzh => xt
        | SizeofArrayzh => xt
        | SizeofMutableArrayzh => xr
        | UnsafeThawArrayzh => xt
        | UnsafeFreezzeArrayzh => xt
        | UnsafeFreezzeByteArrayzh => xt
        | CopyArrayzh => xw
        | CopyMutableArrayzh => xw
        | CopyByteArrayzh => xw
        | CopyMutableByteArrayzh => xw
        | NewImmutableArrayzh => xig
        | NewStrictImmutableArrayzh => xig
        | InitImmutableArrayzh => xiw
        | InitStrictImmutableArrayzh => xiw
        | ImmutableArrayInitedzh => xiw
        | StrictImmutableArrayInitedzh => xiw
        | SizzeofImmutableArrayzh => xt
        | SizzeofStrictImmutableArrayzh => xt
        | IndexImmutableArrayzh => xir
        | IndexStrictImmutableArrayzh => xir
        | NewUnboxedWordArrayzh => xig
        | NewUnboxedWord8Arrayzh => xig
        | NewUnboxedWord16Arrayzh => xig
        | NewUnboxedWord32Arrayzh => xig
        | NewUnboxedWord64Arrayzh => xig
        | NewUnboxedIntArrayzh => xig
        | NewUnboxedInt8Arrayzh => xig
        | NewUnboxedInt16Arrayzh => xig
        | NewUnboxedInt32Arrayzh => xig
        | NewUnboxedInt64Arrayzh => xig
        | NewUnboxedFloatArrayzh => xig
        | NewUnboxedDoubleArrayzh => xig
        | NewUnboxedCharArrayzh => xig
        | NewUnboxedAddrArrayzh => xig
        | InitUnboxedWordArrayzh => xiw
        | InitUnboxedWord8Arrayzh => xiw
        | InitUnboxedWord16Arrayzh => xiw
        | InitUnboxedWord32Arrayzh => xiw
        | InitUnboxedWord64Arrayzh => xiw
        | InitUnboxedIntArrayzh => xiw
        | InitUnboxedInt8Arrayzh => xiw
        | InitUnboxedInt16Arrayzh => xiw
        | InitUnboxedInt32Arrayzh => xiw
        | InitUnboxedInt64Arrayzh => xiw
        | InitUnboxedFloatArrayzh => xiw
        | InitUnboxedDoubleArrayzh => xiw
        | InitUnboxedCharArrayzh => xiw
        | InitUnboxedAddrArrayzh => xiw
        | UnboxedWordArrayInitedzh => xiw
        | UnboxedWord8ArrayInitedzh => xiw
        | UnboxedWord16ArrayInitedzh => xiw
        | UnboxedWord32ArrayInitedzh => xiw
        | UnboxedWord64ArrayInitedzh => xiw
        | UnboxedIntArrayInitedzh => xiw
        | UnboxedInt8ArrayInitedzh => xiw
        | UnboxedInt16ArrayInitedzh => xiw
        | UnboxedInt32ArrayInitedzh => xiw
        | UnboxedInt64ArrayInitedzh => xiw
        | UnboxedFloatArrayInitedzh => xiw
        | UnboxedDoubleArrayInitedzh => xiw
        | UnboxedCharArrayInitedzh => xiw
        | UnboxedAddrArrayInitedzh => xiw
        | SizeofUnboxedWordArrayzh => xt
        | SizeofUnboxedWord8Arrayzh => xt
        | SizeofUnboxedWord16Arrayzh => xt
        | SizeofUnboxedWord32Arrayzh => xt
        | SizeofUnboxedWord64Arrayzh => xt
        | SizeofUnboxedIntArrayzh => xt
        | SizeofUnboxedInt8Arrayzh => xt
        | SizeofUnboxedInt16Arrayzh => xt
        | SizeofUnboxedInt32Arrayzh => xt
        | SizeofUnboxedInt64Arrayzh => xt
        | SizeofUnboxedFloatArrayzh => xt
        | SizeofUnboxedDoubleArrayzh => xt
        | SizeofUnboxedCharArrayzh => xt
        | SizeofUnboxedAddrArrayzh => xt
        | IndexUnboxedWordArrayzh => xir
        | IndexUnboxedWord8Arrayzh => xir
        | IndexUnboxedWord16Arrayzh => xir
        | IndexUnboxedWord32Arrayzh => xir
        | IndexUnboxedWord64Arrayzh => xir
        | IndexUnboxedIntArrayzh => xir
        | IndexUnboxedInt8Arrayzh => xir
        | IndexUnboxedInt16Arrayzh => xir
        | IndexUnboxedInt32Arrayzh => xir
        | IndexUnboxedInt64Arrayzh => xir
        | IndexUnboxedFloatArrayzh => xir
        | IndexUnboxedDoubleArrayzh => xir
        | IndexUnboxedCharArrayzh => xir
        | IndexUnboxedAddrArrayzh => xir
        | NewByteArrayzh => xh
        | NewPinnedByteArrayzh => xh
        | NewAlignedPinnedByteArrayzh => xh
        | ByteArrayContentszh => xt
        | SameMutableByteArrayzh => xt
        | SizzeofByteArrayzh => xt
        | SizzeofMutableByteArrayzh => xt
        | NewUnboxedArrayzh => xh
        | NewPinnedUnboxedArrayzh => xh
        | NewAlignedPinnedUnboxedArrayzh => xh
        | IndexWord64Arrayzh => xt
        | IndexWord32Arrayzh => xt
        | IndexWord16Arrayzh => xt
        | IndexWord8Arrayzh => xt
        | IndexInt64Arrayzh => xt
        | IndexInt32Arrayzh => xt
        | IndexInt16Arrayzh => xt
        | IndexInt8Arrayzh => xt
        | IndexWordArrayzh => xt
        | IndexIntArrayzh => xt
        | IndexWideCharArrayzh => xt
        | IndexCharArrayzh => xt
        | IndexStablePtrArrayzh => xt
        | IndexDoubleArrayzh => xt
        | IndexFloatArrayzh => xt
        | IndexAddrArrayzh => xt
        | ReadWord64Arrayzh => xr
        | ReadWord32Arrayzh => xr
        | ReadWord16Arrayzh => xr
        | ReadWord8Arrayzh => xr
        | ReadInt64Arrayzh => xr
        | ReadInt32Arrayzh => xr
        | ReadInt16Arrayzh => xr
        | ReadInt8Arrayzh => xr
        | ReadStablePtrArrayzh => xr
        | ReadDoubleArrayzh => xr
        | ReadFloatArrayzh => xr
        | ReadAddrArrayzh => xr
        | ReadWordArrayzh => xr
        | ReadIntArrayzh => xr
        | ReadWideCharArrayzh => xr
        | ReadCharArrayzh => xr
        | WriteWord64Arrayzh => xw
        | WriteWord32Arrayzh => xw
        | WriteWord16Arrayzh => xw
        | WriteWord8Arrayzh => xw
        | WriteInt64Arrayzh => xw
        | WriteInt32Arrayzh => xw
        | WriteInt16Arrayzh => xw
        | WriteInt8Arrayzh => xw
        | WriteStablePtrArrayzh => xw
        | WriteDoubleArrayzh => xw
        | WriteFloatArrayzh => xw
        | WriteAddrArrayzh => xw
        | WriteWordArrayzh => xw
        | WriteIntArrayzh => xw
        | WriteWideCharArrayzh => xw
        | WriteCharArrayzh => xw
        | NullAddrzh => xt
        | PlusAddrzh => xt
        | MinusAddrzh => xt
        | RemAddrzh => xt
        | Addr2Intzh => xt
        | Int2Addrzh => xt
        | GtAddrzh => xt
        | GeAddrzh => xt
        | LtAddrzh => xt
        | LeAddrzh => xt
        | EqAddrzh => xt
        | CastToAddrzh => xt
        | CastFromAddrzh => xt
        | IndexWord64OffAddrzh => xt
        | IndexWord32OffAddrzh => xt
        | IndexWord16OffAddrzh => xt
        | IndexWord8OffAddrzh => xt
        | IndexInt64OffAddrzh => xt
        | IndexInt32OffAddrzh => xt
        | IndexInt16OffAddrzh => xt
        | IndexInt8OffAddrzh => xt
        | IndexWordOffAddrzh => xt
        | IndexIntOffAddrzh => xt
        | IndexWideCharOffAddrzh => xt
        | IndexStablePtrOffAddrzh => xt
        | IndexDoubleOffAddrzh => xt
        | IndexFloatOffAddrzh => xt
        | IndexAddrOffAddrzh => xt
        | IndexCharOffAddrzh => xt
        | ReadWord64OffAddrzh => xr
        | ReadWord32OffAddrzh => xr
        | ReadWord16OffAddrzh => xr
        | ReadWord8OffAddrzh => xr
        | ReadInt64OffAddrzh => xr
        | ReadInt32OffAddrzh => xr
        | ReadInt16OffAddrzh => xr
        | ReadInt8OffAddrzh => xr
        | ReadWordOffAddrzh => xr
        | ReadIntOffAddrzh => xr
        | ReadWideCharOffAddrzh => xr
        | ReadStablePtrOffAddrzh => xr
        | ReadDoubleOffAddrzh => xr
        | ReadFloatOffAddrzh => xr
        | ReadAddrOffAddrzh => xr
        | ReadCharOffAddrzh => xr
        | WriteWord64OffAddrzh => xw
        | WriteWord32OffAddrzh => xw
        | WriteWord16OffAddrzh => xw
        | WriteWord8OffAddrzh => xw
        | WriteInt64OffAddrzh => xw
        | WriteInt32OffAddrzh => xw
        | WriteInt16OffAddrzh => xw
        | WriteInt8OffAddrzh => xw
        | WriteWordOffAddrzh => xw
        | WriteIntOffAddrzh => xw
        | WriteWideCharOffAddrzh => xw
        | WriteStablePtrOffAddrzh => xw
        | WriteDoubleOffAddrzh => xw
        | WriteFloatOffAddrzh => xw
        | WriteAddrOffAddrzh => xw
        | WriteCharOffAddrzh => xw
        | NewMutVarzh => xh
        | ReadMutVarzh => xr
        | WriteMutVarzh => xw
        | SameMutVarzh => xt
        | CasMutVarzh => xrw
        | AtomicModifyMutVarzh => xrw
        | NewTVarzh => xh
        | ReadTVarzh => xr
        | ReadTVarIOzh => xr
        | WriteTVarzh => xw
        | SameTVarzh => xt
        | Atomicallyzh => xa
        | Catchzh => xa
        | Raisezh => xf
        | RaiseIOzh => xf
        | MaskAsyncExceptionszh => xa
        | MaskUninterruptiblezh => xa
        | UnmaskAsyncExceptionszh => xa
        | GetMaskingStatezh => xt
        | NewMVarzh => xh
        | TakeMVarzh => xrw
        | TryTakeMVarzh => xrw
        | PutMVarzh => xrw
        | TryPutMVarzh => xrw
        | SameMVarzh => xt
        | IsEmptyMVarzh => xr
        | Delayzh => xio
        | WaitReadzh => xio
        | WaitWritezh => xio
        | AsyncReadzh => xio
        | AsyncWritezh => xio
        | AsyncDoProczh => xio
        | Forkzh => xio
        | ForkOnzh => xio
        | KillThreadzh => xio
        | Yieldzh => xio
        | MyThreadIdzh => xio
        | LabelThreadzh => xio
        | IsCurrentThreadBoundzh => xio
        | NoDuplicatezh => xio
        | ThreadStatuszh => xio
        | MkWeakzh => xh
        | MkWeakForeignEnvzh => xh
        | DeRefWeakzh => xr
        | FinalizzeWeakzh => xa
        | Touchzh =>     xt
        | MakeStablePtrzh => xh
        | DeRefStablePtrzh => xt
        | FreeStablePtrzh => xh
        | EqStablePtrzh => xt
        | ReallyUnsafePtrEqualityzh => xt
        | Seqzh => xf
        | DataToTagzh => xc
        | TagToEnumzh => xr
        | TraceEventzh => xt
  end

  val keepStrictArgs : primOp * 'a list -> 'a list =
    fn (NewArrayzh, [i,da,s]) => [i,s]
     | (WriteArrayzh, [ma,i,da,s]) => [ma,i,s]
     | (InitImmutableArrayzh, [ia,i,da,s]) => [ia,i,s]
     | (NewMutVarzh, [da,s]) => [s]
     | (WriteMutVarzh, [muv,da,s]) => [muv,s]
     | (CasMutVarzh, [muv,da0,da1,da2,s]) => [muv,s]
     | (NewTVarzh, [da,s]) => [s]
     | (WriteTVarzh, [muv,da,s]) => [muv,s]
     | (Catchzh, [arr,arr',srw]) => [arr,srw]
     | (Raisezh, [da]) => []
     | (RaiseIOzh, [da,srw]) => [srw]
     | (PutMVarzh, [mv,da,s]) => [mv,s]
     | (TryPutMVarzh, [mv,da,s]) => [mv,s]
     | (Forkzh, [da,srw]) => [srw]
     | (ForkOnzh, [i,da,srw]) => [i,srw]
     | (KillThreadzh, [ti,da,srw]) => [ti,srw]
     | (MkWeakzh, [da,db,dc,srw]) => [srw]
     | (MkWeakForeignEnvzh, [da,db,a0,a1,i,a2,srw]) => [a0,a1,i,a2,srw]
     | (Touchzh, [da,srw]) => [srw]
     | (MakeStablePtrzh, [da,srw]) => [srw]
     (*
     | (MakeStableNamezh, [da,s]) => [s]
     *)
     | (_, vs) => vs

end
