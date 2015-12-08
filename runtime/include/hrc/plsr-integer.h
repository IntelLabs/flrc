/* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

#ifndef _PLSR_INTEGER_H_
#define _PLSR_INTEGER_H_

/**********************************************************************
 * Arbitrary precision integers
 */

#include "hrc/plsr-ap-integer.h"

/* GC functions */

#define pLsrIntegerIsRef 1
#define pLsrIntegerRegisterVTables pLsrAPIntRegisterVTables
#define pLsrIntegerRegisterGlobals pLsrAPIntRegisterGlobals

#ifdef P_USE_TAGGED_INTEGERS

#include "hrc/plsr-tagged-int32.h"

/* Types */

typedef PlsrAPInt PlsrInteger;
typedef PlsrAPIntS PlsrIntegerU;

/* Basic constructors */
#define pLsrIntegerIsTagged(a) (pLsrTaggedInt32TaggedIntIsTagged(a))

/* Constants */

#define pLsrSmallIntegerMax pLsrTaggedInt32Max
#define pLsrSmallIntegerMin pLsrTaggedInt32Min

#define pLsrIntegerZero  ((PlsrInteger) pLsrTaggedInt32Zero)
#define pLsrIntegerOne  ((PlsrInteger) pLsrTaggedInt32One)
#define pLsrIntegerMinusOne  ((PlsrInteger) pLsrTaggedInt32MinusOne)
#define pLsrIntegerMinusUInt32Max  pLsrAPIntMinusUInt32Max


#ifdef PLSR_AP_INT_BACKPATCH_GLOBALS

#define pLsrIntegerStaticUnboxed pLsrAPIntStaticGlobalNew
#define pLsrIntegerStaticGlobalInit pLsrAPIntStaticGlobalInitFromCString
#define pLsrIntegerStaticRef(uvar)  ((PlsrAPInt) & (uvar))

#else

#define pLsrIntegerSignNeg PlsrAPNeg
#define pLsrIntegerSignPos PlsrAPPos
#define pLsrIntegerSignZero PlsrAPZero
#define pLsrIntegerDigitListStaticEmpty pLsrAPIntDigitListStaticEmpty
#define pLsrIntegerDigitListStaticConsUnboxedDef pLsrAPIntDigitListStaticConsUnboxedDef
#define pLsrIntegerDigitListStaticConsRef pLsrAPIntDigitListStaticConsRef
#define pLsrIntegerStaticUnboxedDef pLsrAPIntStaticUnboxedDef
#define pLsrIntegerStaticRef(uvar)  ((PlsrAPInt) & uvar)

#endif


/* Conversions */

#define pLsrSmallIntegerFromSInt32(a) (pLsrTaggedInt32TaggedIntFromSmallInt32(PlsrInteger, a))

#define pLsrIntegerFromSInt8 pLsrIntegerFromSInt32
#define pLsrSInt8FromInteger pLsrSInt32FromInteger

#define pLsrIntegerFromSInt16 pLsrIntegerFromSInt32
#define pLsrSInt16FromInteger pLsrSInt32FromInteger


#define pLsrIntegerFromSInt32(dest, a)                              \
    pLsrTaggedInt32NumConvFastTaggedIntFromSIntX(PlsrInteger, sint32, pLsrAPIntFromSInt32, dest, a)
#define pLsrSInt32FromInteger(dest, a)                             \
    pLsrTaggedInt32NumConvFastSIntXFromTaggedInt(sint32, PlsrInteger, PlsrAPInt, pLsrSInt32FromAPInt, dest, a)

#define pLsrIntegerFromSIntp(dest, a)                              \
    pLsrTaggedInt32NumConvFastTaggedIntFromSIntX(PlsrInteger, sintp, pLsrAPIntFromSIntp, dest, a)
#define pLsrSIntpFromInteger(dest, a)                             \
    pLsrTaggedInt32NumConvFastSIntXFromTaggedInt(sintp, PlsrInteger, PlsrAPInt, pLsrSIntpFromAPInt, dest, a)

#define pLsrIntegerFromSInt64(dest, a)                              \
    pLsrTaggedInt32NumConvFastTaggedIntFromSIntX(PlsrInteger, sint64, pLsrAPIntFromSInt64, dest, a)
#define pLsrSInt64FromInteger(dest, a)                             \
    pLsrTaggedInt32NumConvFastSIntXFromTaggedInt(sint64, PlsrInteger, PlsrAPInt, pLsrSInt64FromAPInt, dest, a)

#define pLsrIntegerFromUInt8 pLsrIntegerFromUInt32
#define pLsrUInt8FromInteger pLsrUInt32FromInteger

#define pLsrIntegerFromUInt16 pLsrIntegerFromUInt32
#define pLsrUInt16FromInteger pLsrUInt32FromInteger

#define pLsrIntegerFromUInt32(dest, a)                              \
    pLsrTaggedInt32NumConvFastTaggedIntFromUIntX(PlsrInteger, uint32, pLsrAPIntFromUInt32, dest, a)
#define pLsrUInt32FromInteger(dest, a)                             \
    pLsrTaggedInt32NumConvFastUIntXFromTaggedInt(uint32, PlsrInteger, PlsrAPInt, pLsrUInt32FromAPInt, dest, a)

#define pLsrIntegerFromUIntp(dest, a)                              \
    pLsrTaggedInt32NumConvFastTaggedIntFromUIntX(PlsrInteger, uintp, pLsrAPIntFromUIntp, dest, a)
#define pLsrUIntpFromInteger(dest, a)                             \
    pLsrTaggedInt32NumConvFastUIntXFromTaggedInt(uintp, PlsrInteger, PlsrAPInt, pLsrUIntpFromAPInt, dest, a)

#define pLsrIntegerFromUInt64(dest, a)                              \
    pLsrTaggedInt32NumConvFastTaggedIntFromUIntX(PlsrInteger, uint64, pLsrAPIntFromUInt64, dest, a)
#define pLsrUInt64FromInteger(dest, a)                             \
    pLsrTaggedInt32NumConvFastUIntXFromTaggedInt(uint64, PlsrInteger, PlsrAPInt, pLsrUInt64FromAPInt, dest, a)

#define pLsrIntegerFromFloat32(dest, a)                                 \
    pLsrTaggedInt32NumConvFastTaggedIntFromFloat32(PlsrInteger, pLsrAPIntFromFloat32, dest, a)
#define pLsrFloat32FromInteger(dest, a)                                 \
    pLsrTaggedInt32NumConvFastFloatXFromTaggedInt(float32, PlsrInteger, PlsrAPInt, pLsrFloat32FromAPInt, dest, a)

#define pLsrIntegerFromFloat64(dest, a)                                 \
    pLsrTaggedInt32NumConvFastTaggedIntFromFloat64(PlsrInteger, pLsrAPIntFromFloat64, dest, a)
#define pLsrFloat64FromInteger(dest, a)                                 \
    pLsrTaggedInt32NumConvFastFloatXFromTaggedInt(float64, PlsrInteger, PlsrAPInt, pLsrFloat64FromAPInt, dest, a)


/* pLsrCStringFromInteger */
pLsrTaggedInt32MkCStringFrom(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrCStringFromAPInt);

/* pLsrIntegerFromCString */
pLsrTaggedInt32MkFromCString(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, \
                             pLsrSInt32FromAPInt, pLsrAPIntLessOrEqual, pLsrAPIntFromCString);

/* Casts */

/* NG: Note that we are assuming that casting from sint32 to sintX and uintX takes the bottom X bits
 *     This is not portable.
 */

#define pLsrIntegerCastToSInt8(dest, a)                                 \
    pLsrTaggedInt32NumConvFastSIntXFromTaggedInt(sint8, PlsrInteger, PlsrAPInt, pLsrAPIntCastToSInt8, dest, a)

#define pLsrIntegerCastToSInt16(dest, a)                                \
    pLsrTaggedInt32NumConvFastSIntXFromTaggedInt(sint16, PlsrInteger, PlsrAPInt, pLsrAPIntCastToSInt16, dest, a)

#define pLsrIntegerCastToSInt32(dest, a)                                \
    pLsrTaggedInt32NumConvFastSIntXFromTaggedInt(sint32, PlsrInteger, PlsrAPInt, pLsrAPIntCastToSInt32, dest, a)

#define pLsrIntegerCastToSInt64(dest, a)                                \
    pLsrTaggedInt32NumConvFastSIntXFromTaggedInt(sint64, PlsrInteger, PlsrAPInt, pLsrAPIntCastToSInt64, dest, a)

#define pLsrIntegerCastToUInt8(dest, a)                                 \
    pLsrTaggedInt32NumConvFastSIntXFromTaggedInt(uint8, PlsrInteger, PlsrAPInt, pLsrAPIntCastToUInt8, dest, a)

#define pLsrIntegerCastToUInt16(dest, a)                                \
    pLsrTaggedInt32NumConvFastSIntXFromTaggedInt(uint16, PlsrInteger, PlsrAPInt, pLsrAPIntCastToUInt16, dest, a)

#define pLsrIntegerCastToUInt32(dest, a)                                \
    pLsrTaggedInt32NumConvFastSIntXFromTaggedInt(uint32, PlsrInteger, PlsrAPInt, pLsrAPIntCastToUInt32, dest, a)

#define pLsrIntegerCastToUInt64(dest, a)                                \
    pLsrTaggedInt32NumConvFastSIntXFromTaggedInt(uint64, PlsrInteger, PlsrAPInt, pLsrAPIntCastToUInt64, dest, a)

#define pLsrIntegerCastToFloat32(dest, a)                               \
    pLsrTaggedInt32NumConvFastSIntXFromTaggedInt(float32, PlsrInteger, PlsrAPInt, pLsrAPIntCastToFloat32, dest, a)

#define pLsrIntegerCastToFloat64(dest, a)                               \
    pLsrTaggedInt32NumConvFastSIntXFromTaggedInt(float64, PlsrInteger, PlsrAPInt, pLsrAPIntCastToFloat64, dest, a)

/* Arithmetic */

#ifdef PLSR_TAGGED_INTEGER_RECOVER

static PlsrInteger pLsrIntegerAPIntBNot(PlsrAPInt a)
{
    PlsrAPInt c = pLsrAPIntBNot(a);
    sint32 ci = pLsrAPIntCheckRangeSInt32 (c, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    if (ci > SINT32_MIN) {
        return pLsrSmallIntegerFromSInt32(ci);
    } else {
        return c;
    }
}

static PlsrInteger pLsrIntegerAPIntBAnd(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt c = pLsrAPIntBAnd(a, b);
    sint32 ci = pLsrAPIntCheckRangeSInt32 (c, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    if (ci > SINT32_MIN) {
        return pLsrSmallIntegerFromSInt32(ci);
    } else {
        return c;
    }
}

static PlsrInteger pLsrIntegerAPIntBOr(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt c = pLsrAPIntBOr(a, b);
    sint32 ci = pLsrAPIntCheckRangeSInt32 (c, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    if (ci > SINT32_MIN) {
        return pLsrSmallIntegerFromSInt32(ci);
    } else {
        return c;
    }
}

static PlsrInteger pLsrIntegerAPIntBShiftL(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt c = pLsrAPIntBShiftL(a, b);
    sint32 ci = pLsrAPIntCheckRangeSInt32 (c, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    if (ci > SINT32_MIN) {
        return pLsrSmallIntegerFromSInt32(ci);
    } else {
        return c;
    }
}

static PlsrInteger pLsrIntegerAPIntBShiftR(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt c = pLsrAPIntBShiftR(a, b);
    sint32 ci = pLsrAPIntCheckRangeSInt32 (c, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    if (ci > SINT32_MIN) {
        return pLsrSmallIntegerFromSInt32(ci);
    } else {
        return c;
    }
}

static PlsrInteger pLsrIntegerAPIntBXor(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt c = pLsrAPIntBXor(a, b);
    sint32 ci = pLsrAPIntCheckRangeSInt32 (c, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    if (ci > SINT32_MIN) {
        return pLsrSmallIntegerFromSInt32(ci);
    } else {
        return c;
    }
}

static PlsrInteger pLsrIntegerAPIntAdd(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt c = pLsrAPIntAdd(a, b);
    sint32 ci = pLsrAPIntCheckRangeSInt32 (c, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    if (ci > SINT32_MIN) {
        return pLsrSmallIntegerFromSInt32(ci);
    } else {
        return c;
    }
}

static PlsrInteger pLsrIntegerAPIntSub(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt c = pLsrAPIntSub(a, b);
    sint32 ci = pLsrAPIntCheckRangeSInt32 (c, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    if (ci > SINT32_MIN) {
        return pLsrSmallIntegerFromSInt32(ci);
    } else {
        return c;
    }
}

/* Multiplication can only make things worse */
#define pLsrIntegerAPIntMul pLsrAPIntMul

static void pLsrIntegerAPIntDivModE(PlsrInteger* q, PlsrInteger* r, PlsrAPInt a, PlsrAPInt b)
{
    pLsrAPIntDivModE(q, r, a, b);
    sint32 qi = pLsrAPIntCheckRangeSInt32 (*q, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    sint32 ri = pLsrAPIntCheckRangeSInt32 (*r, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    if (qi > SINT32_MIN) {
        *q = pLsrSmallIntegerFromSInt32(qi);
    }
    if (ri > SINT32_MIN) {
        *r = pLsrSmallIntegerFromSInt32(ri);
    }
}

static void pLsrIntegerAPIntDivModF(PlsrInteger* q, PlsrInteger* r, PlsrAPInt a, PlsrAPInt b)
{
    pLsrAPIntDivModF(q, r, a, b);
    sint32 qi = pLsrAPIntCheckRangeSInt32 (*q, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    sint32 ri = pLsrAPIntCheckRangeSInt32 (*r, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    if (qi > SINT32_MIN) {
        *q = pLsrSmallIntegerFromSInt32(qi);
    }
    if (ri > SINT32_MIN) {
        *r = pLsrSmallIntegerFromSInt32(ri);
    }
}

static void pLsrIntegerAPIntDivModT(PlsrInteger* q, PlsrInteger* r, PlsrAPInt a, PlsrAPInt b)
{
    pLsrAPIntDivModT(q, r, a, b);
    sint32 qi = pLsrAPIntCheckRangeSInt32 (*q, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    sint32 ri = pLsrAPIntCheckRangeSInt32 (*r, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    if (qi > SINT32_MIN) {
        *q = pLsrSmallIntegerFromSInt32(qi);
    }
    if (ri > SINT32_MIN) {
        *r = pLsrSmallIntegerFromSInt32(ri);
    }
}

static PlsrInteger pLsrIntegerAPIntDivE(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt c = pLsrAPIntDivE(a, b);
    sint32 ci = pLsrAPIntCheckRangeSInt32 (c, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    if (ci > SINT32_MIN) {
        return pLsrSmallIntegerFromSInt32(ci);
    } else {
        return c;
    }
}

static PlsrInteger pLsrIntegerAPIntDivF(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt c = pLsrAPIntDivF(a, b);
    sint32 ci = pLsrAPIntCheckRangeSInt32 (c, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    if (ci > SINT32_MIN) {
        return pLsrSmallIntegerFromSInt32(ci);
    } else {
        return c;
    }
}

static PlsrInteger pLsrIntegerAPIntDivT(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt c = pLsrAPIntDivT(a, b);
    sint32 ci = pLsrAPIntCheckRangeSInt32 (c, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    if (ci > SINT32_MIN) {
        return pLsrSmallIntegerFromSInt32(ci);
    } else {
        return c;
    }
}

static PlsrInteger pLsrIntegerAPIntModE(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt c = pLsrAPIntModE(a, b);
    sint32 ci = pLsrAPIntCheckRangeSInt32 (c, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    if (ci > SINT32_MIN) {
        return pLsrSmallIntegerFromSInt32(ci);
    } else {
        return c;
    }
}

static PlsrInteger pLsrIntegerAPIntModF(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt c = pLsrAPIntModF(a, b);
    sint32 ci = pLsrAPIntCheckRangeSInt32 (c, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    if (ci > SINT32_MIN) {
        return pLsrSmallIntegerFromSInt32(ci);
    } else {
        return c;
    }
}

static PlsrInteger pLsrIntegerAPIntModT(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt c = pLsrAPIntModT(a, b);
    sint32 ci = pLsrAPIntCheckRangeSInt32 (c, pLsrTaggedInt32Max, pLsrTaggedInt32Min);
    if (ci > SINT32_MIN) {
        return pLsrSmallIntegerFromSInt32(ci);
    } else {
        return c;
    }
}

#else
#define pLsrIntegerAPIntBNot pLsrAPIntBNot
#define pLsrIntegerAPIntBAnd pLsrAPIntBAnd
#define pLsrIntegerAPIntBOr pLsrAPIntBOr
#define pLsrIntegerAPIntBShiftL pLsrAPIntBShiftL
#define pLsrIntegerAPIntBShiftR pLsrAPIntBShiftR
#define pLsrIntegerAPIntBXor pLsrAPIntBXor
#define pLsrIntegerAPIntAdd pLsrAPIntAdd
#define pLsrIntegerAPIntSub pLsrAPIntSub
#define pLsrIntegerAPIntMul pLsrAPIntMul
#define pLsrIntegerAPIntDivModE pLsrAPIntDivModE
#define pLsrIntegerAPIntDivModF pLsrAPIntDivModF
#define pLsrIntegerAPIntDivModT pLsrAPIntDivModT
#define pLsrIntegerAPIntDivE pLsrAPIntDivE
#define pLsrIntegerAPIntDivF pLsrAPIntDivF
#define pLsrIntegerAPIntDivT pLsrAPIntDivT
#define pLsrIntegerAPIntModE pLsrAPIntModE
#define pLsrIntegerAPIntModF pLsrAPIntModF
#define pLsrIntegerAPIntModT pLsrAPIntModT
#endif


/**************************** Bitwise *********************************/

/* pLsrTaggedInt32IntegerBNotSlow*/
#define pLsrIntegerBNot(dest, a)                                        \
    pLsrTaggedInt32TaggedIntBNot(PlsrInteger, PlsrAPInt, pLsrTaggedInt32IntegerBNotSlow, dest, a)

/* pLsrTaggedInt32IntegerBAndSlow*/
pLsrTaggedInt32MkTaggedIntBAndSlow(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrIntegerAPIntBAnd);
#define pLsrIntegerBAnd(dest, a, b) \
    pLsrTaggedInt32TaggedIntBAnd(PlsrInteger, PlsrAPInt, pLsrTaggedInt32IntegerBAndSlow, dest, a, b)

/* pLsrTaggedInt32IntegerBOrSlow*/
pLsrTaggedInt32MkTaggedIntBOrSlow(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrIntegerAPIntBOr);
#define pLsrIntegerBOr(dest, a, b) \
    pLsrTaggedInt32TaggedIntBOr(PlsrInteger, PlsrAPInt, pLsrTaggedInt32IntegerBOrSlow, dest, a, b)

/* pLsrTaggedInt32IntegerBShiftLSlow*/
pLsrTaggedInt32MkTaggedIntBShiftLSlow(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrIntegerAPIntBShiftL);
#define pLsrIntegerBShiftL(dest, a, b) \
    pLsrTaggedInt32TaggedIntBShiftL(PlsrInteger, PlsrAPInt, pLsrTaggedInt32IntegerBShiftLSlow, dest, a, b)

/* pLsrTaggedInt32IntegerBShiftRSlow*/
pLsrTaggedInt32MkTaggedIntBShiftRSlow(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrIntegerAPIntBShiftR);
#define pLsrIntegerBShiftR(dest, a, b)                                  \
    pLsrTaggedInt32TaggedIntBShiftR(PlsrInteger, PlsrAPInt, pLsrTaggedInt32IntegerBShiftRSlow, dest, a, b)

/* pLsrTaggedInt32IntegerBXorSlow*/
pLsrTaggedInt32MkTaggedIntBXorSlow(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrIntegerAPIntBXor);
#define pLsrIntegerBXor(dest, a, b) \
    pLsrTaggedInt32TaggedIntBXor(PlsrInteger, PlsrAPInt, pLsrTaggedInt32IntegerBXorSlow, dest, a, b)


/**************************** Arithmetic *********************************/

#define pLsrIntegerNegate(dest, r) \
    pLsrTaggedInt32TaggedIntNeg(PlsrInteger, PlsrAPInt, pLsrAPIntNegate, pLsrAPIntFromSInt32, dest, r)

/* pLsrTaggedInt32IntegerAddSlow*/
pLsrTaggedInt32MkTaggedIntAddSlow(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrIntegerAPIntAdd);
#define pLsrIntegerPlus(dest, a, b) \
    pLsrTaggedInt32TaggedIntAdd(PlsrInteger, PlsrAPInt, pLsrTaggedInt32IntegerAddSlow, dest, a, b)

/* pLsrTaggedInt32IntegerSubSlow*/
pLsrTaggedInt32MkTaggedIntSubSlow(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrIntegerAPIntSub);
#define pLsrIntegerMinus(dest, a, b)                               \
    pLsrTaggedInt32TaggedIntSub(PlsrInteger, PlsrAPInt, pLsrTaggedInt32IntegerSubSlow, dest, a, b)

/* pLsrTaggedInt32IntegerMulSlow*/
pLsrTaggedInt32MkTaggedIntMulSlow(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrAPIntFromSInt64,
                                  pLsrIntegerAPIntMul);
#define pLsrIntegerTimes(dest, a, b)                                \
    pLsrTaggedInt32TaggedIntMul(PlsrInteger, PlsrAPInt, pLsrTaggedInt32IntegerMulSlow, dest, a, b)

/* pLsrTaggedInt32IntegerDivModESlow */
pLsrTaggedInt32MkTaggedIntDivModESlow(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrIntegerAPIntDivModE);
#define pLsrIntegerDivModE(q, r, a, b)                                  \
    pLsrTaggedInt32TaggedIntDivModE(PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrTaggedInt32IntegerDivModESlow, q, r, a, b)
#define pLsrIntegerDivE(dest, a, b)                                     \
    pLsrTaggedInt32TaggedIntDivE(PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrTaggedInt32IntegerDivModESlow, dest, a, b)
#define pLsrIntegerModE(dest, a, b)                                     \
    pLsrTaggedInt32TaggedIntModE(PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrTaggedInt32IntegerDivModESlow, dest, a, b)

/* pLsrTaggedInt32IntegerDivModFSlow */
pLsrTaggedInt32MkTaggedIntDivModFSlow(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrIntegerAPIntDivModF);
#define pLsrIntegerDivModF(q, r, a, b)                                  \
    pLsrTaggedInt32TaggedIntDivModF(PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrTaggedInt32IntegerDivModFSlow, q, r, a, b)
#define pLsrIntegerDivF(dest, a, b)                                     \
    pLsrTaggedInt32TaggedIntDivF(PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrTaggedInt32IntegerDivModFSlow, dest, a, b)
#define pLsrIntegerModF(dest, a, b)                                     \
    pLsrTaggedInt32TaggedIntModF(PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrTaggedInt32IntegerDivModFSlow, dest, a, b)

/* pLsrTaggedInt32IntegerDivModTSlow */
pLsrTaggedInt32MkTaggedIntDivModTSlow(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrIntegerAPIntDivModT);
#define pLsrIntegerDivModT(q, r, a, b)                                  \
    pLsrTaggedInt32TaggedIntDivModT(PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrTaggedInt32IntegerDivModTSlow, q, r, a, b)
#define pLsrIntegerDivT(dest, a, b)                                     \
    pLsrTaggedInt32TaggedIntDivT(PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrTaggedInt32IntegerDivModTSlow, dest, a, b)
#define pLsrIntegerModT(dest, a, b)                                     \
    pLsrTaggedInt32TaggedIntModT(PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrTaggedInt32IntegerDivModTSlow, dest, a, b)

/* pLsrTaggedInt32IntegerGcd */
pLsrTaggedInt32MkTaggedIntGcd(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, \
                              pLsrAPIntLessOrEqual, pLsrSInt32FromAPInt, pLsrAPIntGcd);
#define pLsrIntegerGcd pLsrTaggedInt32IntegerGcd

/* Comparisons */
pLsrTaggedInt32MkTaggedIntEqSlow(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrAPIntEqual);
pLsrTaggedInt32MkTaggedIntNeSlow(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrAPIntNotEqual);
pLsrTaggedInt32MkTaggedIntLtSlow(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrAPIntLess);
pLsrTaggedInt32MkTaggedIntGtSlow(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrAPIntGreater);
pLsrTaggedInt32MkTaggedIntLeSlow(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrAPIntLessOrEqual);
pLsrTaggedInt32MkTaggedIntGeSlow(Integer, PlsrInteger, PlsrAPInt, pLsrAPIntFromSInt32, pLsrAPIntGreaterOrEqual);

#define pLsrIntegerEQ(dest, a, b) pLsrTaggedInt32TaggedIntEq(pLsrTaggedInt32IntegerEqSlow, dest, a, b)
#define pLsrIntegerNE(dest, a, b) pLsrTaggedInt32TaggedIntNe(pLsrTaggedInt32IntegerNeSlow, dest, a, b)
#define pLsrIntegerLT(dest, a, b) pLsrTaggedInt32TaggedIntLt(pLsrTaggedInt32IntegerLtSlow, dest, a, b)
#define pLsrIntegerGT(dest, a, b) pLsrTaggedInt32TaggedIntGt(pLsrTaggedInt32IntegerGtSlow, dest, a, b)
#define pLsrIntegerLE(dest, a, b) pLsrTaggedInt32TaggedIntLe(pLsrTaggedInt32IntegerLeSlow, dest, a, b)
#define pLsrIntegerGE(dest, a, b) pLsrTaggedInt32TaggedIntGe(pLsrTaggedInt32IntegerGeSlow, dest, a, b)

/* Miscellaneous */
#define pLsrIntegerFitsInSInt32(a) ((pLsrIntegerIsTagged(a)) || (pLsrAPIntFitsInSInt32(a)))

static sint32 pLsrIntegerCheckRangeSInt32(PlsrInteger a, sint32 upper, sint32 lower)
{
    if (pLsrIntegerIsTagged(a)) {
        sint32 ai;
        pLsrSInt32FromInteger(ai, a);
        return ((ai <= upper) && (ai >= lower)) ? ai : SINT32_MIN;
    } else {
        return pLsrAPIntCheckRangeSInt32(a, upper, lower);
    }
}

#define pLsrIntegerHash(i) pLsrTaggedInt32TaggedIntHash(PlsrInteger, PlsrAPInt, pLsrAPIntHash, i)

#else /* !P_USE_TAGGED_INTEGERS */

#define pLsrIntegerIsTagged(a) (0)

/* Types */

typedef PlsrAPInt PlsrInteger;
typedef PlsrAPIntS PlsrIntegerU;

/* Constants */

#define pLsrSmallIntegerMax pLsrTaggedInt32Max
#define pLsrSmallIntegerMin pLsrTaggedInt32Min

#define pLsrIntegerZero  pLsrAPIntZero
#define pLsrIntegerOne  pLsrAPIntOne
#define pLsrIntegerMinusOne  pLsrAPIntMinusOne
#define pLsrIntegerMinusUInt32Max  pLsrAPIntMinusUInt32Max

#define pLsrIntegerSignNeg PlsrAPNeg
#define pLsrIntegerSignPos PlsrAPPos
#define pLsrIntegerSignZero PlsrAPZero

#ifdef PLSR_AP_INT_BACKPATCH_GLOBALS

#define pLsrIntegerStaticUnboxed pLsrAPIntStaticGlobalNew
#define pLsrIntegerStaticGlobalInit pLsrAPIntStaticGlobalInitFromCString
#define pLsrIntegerStaticRef(uvar)  ((PlsrAPInt) & (uvar))

#else

#define pLsrIntegerSignNeg PlsrAPNeg
#define pLsrIntegerSignPos PlsrAPPos
#define pLsrIntegerSignZero PlsrAPZero
#define pLsrIntegerDigitListStaticEmpty pLsrAPIntDigitListStaticEmpty
#define pLsrIntegerDigitListStaticConsUnboxedDef pLsrAPIntDigitListStaticConsUnboxedDef
#define pLsrIntegerDigitListStaticConsRef pLsrAPIntDigitListStaticConsRef
#define pLsrIntegerStaticUnboxedDef pLsrAPIntStaticUnboxedDef
#define pLsrIntegerStaticRef(uvar)  ((PlsrAPInt) & uvar)

#endif


/* Conversions */

#define pLsrSmallIntegerFromSInt32 pLsrAPIntFromSInt32

#define pLsrIntegerFromSInt8 pLsrIntegerFromSInt32
#define pLsrSInt8FromInteger pLsrSInt32FromInteger

#define pLsrIntegerFromSInt16 pLsrIntegerFromSInt32
#define pLsrSInt16FromInteger pLsrSInt32FromInteger

#define pLsrIntegerFromSInt32(dest, i) ((dest) = (pLsrAPIntFromSInt32(i)))
#define pLsrSInt32FromInteger(dest, i) ((dest) = (pLsrSInt32FromAPInt(i)))

#define pLsrIntegerFromSInt64(dest, i) ((dest) = (pLsrAPIntFromSInt64(i)))
#define pLsrSInt64FromInteger(dest, i) ((dest) = (pLsrSInt64FromAPInt(i)))

#define pLsrIntegerFromSIntp(dest, i) ((dest) = (pLsrAPIntFromSIntp(i)))
#define pLsrSIntpFromInteger(dest, i) ((dest) = (pLsrSIntpFromAPInt(i)))


#define pLsrIntegerFromUInt8 pLsrIntegerFromUInt32
#define pLsrUInt8FromInteger pLsrUInt32FromInteger

#define pLsrIntegerFromUInt16 pLsrIntegerFromUInt32
#define pLsrUInt16FromInteger pLsrUInt32FromInteger

#define pLsrIntegerFromUInt32(dest, i) ((dest) = (pLsrAPIntFromUInt32(i)))
#define pLsrUInt32FromInteger(dest, i) ((dest) = (pLsrUInt32FromAPInt(i)))

#define pLsrIntegerFromUInt64(dest, i) ((dest) = (pLsrAPIntFromUInt64(i)))
#define pLsrUInt64FromInteger(dest, i) ((dest) = (pLsrUInt64FromAPInt(i)))

#define pLsrIntegerFromUIntp(dest, i) ((dest) = (pLsrAPIntFromUIntp(i)))
#define pLsrUIntpFromInteger(dest, i) ((dest) = (pLsrUIntpFromAPInt(i)))

#define pLsrCStringFromInteger pLsrCStringFromAPInt
#define pLsrIntegerFromCString pLsrAPIntFromCString

/* Bitwise */

#define pLsrIntegerBNot(dest, i) ((dest) = (pLsrAPIntBNot(i)))
#define pLsrIntegerBAnd(dest, a, b) ((dest) = (pLsrAPIntBAnd(a, b)))
#define pLsrIntegerBOr(dest, a, b) ((dest) = (pLsrAPIntBOr(a, b)))
#define pLsrIntegerBShiftL(dest, a, b) ((dest) = (pLsrAPIntBShiftL(a, b)))
#define pLsrIntegerBShiftR(dest, a, b) ((dest) = (pLsrAPIntBShiftR(a, b)))
#define pLsrIntegerBXor(dest, a, b) ((dest) = (pLsrAPIntBXor(a, b)))

/* Arithmetic */

#define pLsrIntegerNegate(dest, i) ((dest) = (pLsrAPIntNegate(i)))
#define pLsrIntegerPlus(dest, a, b) ((dest) = (pLsrAPIntAdd(a, b)))
#define pLsrIntegerMinus(dest, a, b) ((dest) = (pLsrAPIntSub(a, b)))
#define pLsrIntegerTimes(dest, a, b) ((dest) = (pLsrAPIntMul(a, b)))

#define pLsrIntegerDivModE(q, r, a, b)                              \
    do {                                                                \
        PlsrAPInt pLsrIntegerDivModE_q = NULL;                          \
        PlsrAPInt pLsrIntegerDivModE_r = NULL;                          \
        pLsrAPIntDivModE(&pLsrIntegerDivModE_q, &pLsrIntegerDivModE_r, a, b); \
        q = pLsrIntegerDivModE_q;                                       \
        r = pLsrIntegerDivModE_r;                                       \
    } while (0)
#define pLsrIntegerDivModF(q, r, a, b)                              \
    do {                                                                \
        PlsrAPInt pLsrIntegerDivModF_q = NULL;                          \
        PlsrAPInt pLsrIntegerDivModF_r = NULL;                          \
        pLsrAPIntDivModF(&pLsrIntegerDivModF_q, &pLsrIntegerDivModF_r, a, b); \
        q = pLsrIntegerDivModF_q;                                       \
        r = pLsrIntegerDivModF_r;                                       \
    } while (0)
#define pLsrIntegerDivModT(q, r, a, b)                               \
    do {                                                                \
        PlsrAPInt pLsrIntegerDivModT_q = NULL;                          \
        PlsrAPInt pLsrIntegerDivModT_r = NULL;                          \
        pLsrAPIntDivModT(&pLsrIntegerDivModT_q, &pLsrIntegerDivModT_r, a, b); \
        q = pLsrIntegerDivModT_q;                                       \
        r = pLsrIntegerDivModT_r;                                       \
    } while (0)


#define pLsrIntegerDivE(dest, a, b) ((dest) = (pLsrAPIntDivE(a, b)))
#define pLsrIntegerModE(dest, a, b) ((dest) = (pLsrAPIntModE(a, b)))

#define pLsrIntegerDivF(dest, a, b) ((dest) = (pLsrAPIntDivF(a, b)))
#define pLsrIntegerModF(dest, a, b) ((dest) = (pLsrAPIntModF(a, b)))

#define pLsrIntegerDivT(dest, a, b) ((dest) = (pLsrAPIntDivT(a, b)))
#define pLsrIntegerModT(dest, a, b) ((dest) = (pLsrAPIntModT(a, b)))

#define pLsrIntegerGcd pLsrAPIntGcd

/* Comparisons */

#define pLsrIntegerEQ(dest, a, b) ((dest) = (pLsrAPIntEqual(a, b)))
#define pLsrIntegerNE(dest, a, b) ((dest) = (pLsrAPIntNotEqual(a, b)))
#define pLsrIntegerLT(dest, a, b) ((dest) = (pLsrAPIntLess(a, b)))
#define pLsrIntegerGT(dest, a, b) ((dest) = (pLsrAPIntGreater(a, b)))
#define pLsrIntegerLE(dest, a, b) ((dest) = (pLsrAPIntLessOrEqual(a, b)))
#define pLsrIntegerGE(dest, a, b) ((dest) = (pLsrAPIntGreaterOrEqual(a, b)))

/* Miscellaneous */
#define pLsrIntegerFitsInSInt32 pLsrAPIntFitsInSInt32
#define pLsrIntegerCheckRangeSInt32 pLsrAPIntCheckRangeSInt32

#define pLsrIntegerHash(i) (pLsrAPInthash(i))

#endif /* P_USE_TAGGED_INTEGERS */
#endif /*_PLSR_INTEGER_H_ */
