/* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

/* Arbitrary precision numbers */

#ifndef _PLSR_RATIONAL_H_
#define _PLSR_RATIONAL_H_

/**********************************************************************
 * This file implements the Rational type in terms of
 * an arbitrary precision rational package APRat, with associated
 * operations.
 */

#include "hrc/plsr-ap-rational.h"

static void pLsrRationalRegisterVTables()
{
    pLsrAPRatRegisterVTables();
}

static void pLsrRationalRegisterGlobals() {
    pLsrAPRatRegisterGlobals();
}


#ifdef P_USE_TAGGED_RATIONALS

#include "hrc/plsr-tagged-int32.h"

/* Basic constructors */

typedef PlsrAPRat PlsrRational;
typedef PlsrAPRatS PlsrRationalU;

#define pLsrRationalIsTagged(a) (pLsrTaggedInt32TaggedIntIsTagged(a))

#define pLsrRationalUnTag(a) (pLsrTaggedInt32TaggedIntUnTag(a))

/* Basic queries and destructors */

#define pLsrRationalIsIntegral(a)                                       \
    (pLsrRationalIsTagged(a) || pLsrAPRatIsIntegral((PlsrAPRat) a))

#define pLsrRationalStaticUnboxedDef pLsrAPRatStaticUnboxedDef
#define pLsrRationalStaticInit pLsrAPRatStaticInit

#define pLsrSmallRationalFromSInt32(a) (pLsrTaggedInt32TaggedIntFromSmallInt32(PlsrRational, a))

#define pLsrSmallRationalMax pLsrTaggedInt32Max
#define pLsrSmallRationalMin pLsrTaggedInt32Min


/* Conversions */
/* Helpers */
static void pLsrIntegersFromRational(PlsrInteger* numO, PlsrInteger* denO, PlsrRational r) {
    if (pLsrRationalIsTagged(r)) {
        /* This conversion could be optimized */
        pLsrIntegerFromSInt32(*numO, pLsrRationalUnTag(r));
        *denO = pLsrIntegerOne;
    } else {
        pLsrIntegersFromAPRat(numO, denO, r);
    }
}

static PlsrInteger pLsrRatNumeratorSlow(PlsrRational r) {
    if (pLsrRationalIsTagged(r)) {
        PlsrInteger i;
        /* This conversion could be optimized */
        pLsrIntegerFromSInt32(i, pLsrRationalUnTag(r));
        return i;
    } else {
        return pLsrAPRatNumerator(r);
    }
}

#define pLsrRatNumerator pLsrRatNumeratorSlow

static PlsrInteger pLsrRatDenominatorSlow(PlsrRational r) {
    if (pLsrRationalIsTagged(r)) {
        return pLsrIntegerOne;
    } else {
        return pLsrAPRatDenominator(r);
    }
}

#define pLsrRatDenominator pLsrRatDenominatorSlow

/************* Runtime/Compiler API ***************/


#define pLsrRationalFromSInt8 pLsrRationalFromSInt32
#define pLsrSInt8FromRational pLsrSInt32FromRational

#define pLsrRationalFromSInt16 pLsrRationalFromSInt32
#define pLsrSInt16FromRational pLsrSInt32FromRational

#define pLsrRationalFromSInt32(dest, a)                              \
    pLsrTaggedInt32NumConvFastTaggedIntFromSIntX(PlsrRational, sint32, pLsrAPRatFromSInt32, dest, a)
#define pLsrSInt32FromRational(dest, a)                             \
    pLsrTaggedInt32NumConvFastSIntXFromTaggedInt(sint32, PlsrRational, PlsrAPRat, pLsrSInt32FromAPRat, dest, a)

#define pLsrRationalFromSIntp(dest, a)                              \
    pLsrTaggedInt32NumConvFastTaggedIntFromSIntX(PlsrRational, sintp, pLsrAPRatFromSIntp, dest, a)
#define pLsrSIntpFromRational(dest, a)                             \
    pLsrTaggedInt32NumConvFastSIntXFromTaggedInt(sintp, PlsrRational, PlsrAPRat, pLsrSIntpFromAPRat, dest, a)

#define pLsrRationalFromSInt64(dest, a)                              \
    pLsrTaggedInt64NumConvFastTaggedIntFromSIntX(PlsrRational, sint64, pLsrAPRatFromSInt64, dest, a)
#define pLsrSInt64FromRational(dest, a)                             \
    pLsrTaggedInt64NumConvFastSIntXFromTaggedInt(sint64, PlsrRational, PlsrAPRat, pLsrSInt64FromAPRat, dest, a)

#define pLsrRationalFromUInt8 pLsrRationalFromUInt32
#define pLsrUInt8FromRational pLsrUInt32FromRational

#define pLsrRationalFromUInt16 pLsrRationalFromUInt32
#define pLsrUInt16FromRational pLsrUInt32FromRational

#define pLsrRationalFromUInt32(dest, a)                              \
    pLsrTaggedInt32NumConvFastTaggedIntFromUIntX(PlsrRational, uint32, pLsrAPRatFromUInt32, dest, a)
#define pLsrUInt32FromRational(dest, a)                             \
    pLsrTaggedInt32NumConvFastUIntXFromTaggedInt(uint32, PlsrRational, PlsrAPRat, pLsrUInt32FromAPRat, dest, a)

#define pLsrRationalFromUIntp(dest, a)                              \
    pLsrTaggedInt32NumConvFastTaggedIntFromUIntX(PlsrRational, uintp, pLsrAPRatFromUIntp, dest, a)
#define pLsrUIntpFromRational(dest, a)                             \
    pLsrTaggedInt32NumConvFastUIntXFromTaggedInt(uintp, PlsrRational, PlsrAPRat, pLsrUIntpFromAPRat, dest, a)

#define pLsrRationalFromUInt64(dest, a)                              \
    pLsrTaggedInt64NumConvFastTaggedIntFromUIntX(PlsrRational, uint64, pLsrAPRatFromUInt64, dest, a)
#define pLsrUInt64FromRational(dest, a)                             \
    pLsrTaggedInt64NumConvFastUIntXFromTaggedInt(uint64, PlsrRational, PlsrAPRat, pLsrUInt64FromAPRat, dest, a)

#define pLsrRationalFromFloat32(dest, a)                                \
    pLsrTaggedInt32NumConvFastTaggedIntFromFloat32(PlsrRational, pLsrAPRatFromFloat32, dest, a)
#define pLsrFloat32FromRational(dest, a)                                 \
    pLsrTaggedInt32NumConvFastFloatXFromTaggedInt(float32, PlsrRational, PlsrAPRat, pLsrFloat32FromAPRat, dest, a)

#define pLsrRationalFromFloat64(dest, a)                                \
    pLsrTaggedInt64NumConvFastTaggedIntFromFloat64(PlsrRational, pLsrAPRatFromFloat64, dest, a)
#define pLsrFloat64FromRational(dest, a)                                 \
    pLsrTaggedInt64NumConvFastFloatXFromTaggedInt(float64, PlsrRational, PlsrAPRat, pLsrFloat64FromAPRat, dest, a)

static PlsrRational pLsrRationalFromIntegerSlow(PlsrInteger i)
{
    if (pLsrIntegerFitsInSInt32(i)) {
        sint32 si;
        PlsrRational r;
        pLsrSInt32FromInteger(si, i);
        pLsrRationalFromSInt32(r, si);
        return r;
    } else {
        return (PlsrRational) pLsrAPRatFromInteger(i);
    }
}

#define pLsrRationalFromInteger(dest, i) ((dest) = (pLsrRationalFromIntegerSlow(i)))

static PlsrInteger pLsrIntegerFromRationalSlow(PlsrRational r)  {
    if (pLsrRationalIsTagged(r)) {
        PlsrInteger i;
        /* This conversion could be optimized */
        pLsrIntegerFromSInt32(i, pLsrRationalUnTag(r));
        return i;
    } else {
        return pLsrIntegerFromAPRat(r);
    }
}

#define pLsrIntegerFromRational(dest, r) ((dest) = (pLsrIntegerFromRationalSlow(r)))

#define pLsrRationalToUInt32Checked(dest, a)     \
    do {                                             \
        if (pLsrRationalIsTagged(a)) {               \
            sint32 aa = pLsrRationalUnTag(a);        \
            if (aa < 0) {(dest) = 0xffffffff;}       \
            else {(dest) = ((uint32) aa);}           \
        } else {                                     \
            ((dest) = pLsrAPRatToUInt32Checked(a));  \
        }                                            \
    } while(0)

/* pLsrCStringFromRational */
pLsrTaggedInt32MkCStringFrom(Rational, PlsrRational, PlsrAPRat, pLsrAPRatFromSInt32, pLsrCStringFromAPRat);

/* pLsrRationalFromCString */
pLsrTaggedInt32MkFromCString(Rational, PlsrRational, PlsrAPRat, pLsrAPRatFromSInt32, \
                             pLsrSInt32FromAPRat, pLsrAPRatLe, pLsrAPRatFromCString);

/* Arithmetic */

#define pLsrRationalNegate(dest, r) \
    pLsrTaggedInt32TaggedIntNeg(PlsrRational, PlsrAPRat, pLsrAPRatNeg, pLsrAPRatFromSInt32, dest, r)

/* pLsrTaggedInt32RationalAddSlow*/
pLsrTaggedInt32MkTaggedIntAddSlow(Rational, PlsrRational, PlsrAPRat, pLsrAPRatFromSInt32, pLsrAPRatPlus);
#define pLsrRationalPlus(dest, a, b) \
    pLsrTaggedInt32TaggedIntAdd(PlsrRational, PlsrAPRat, pLsrTaggedInt32RationalAddSlow, dest, a, b)

/* pLsrTaggedInt32RationalSubSlow*/
pLsrTaggedInt32MkTaggedIntSubSlow(Rational, PlsrRational, PlsrAPRat, pLsrAPRatFromSInt32, pLsrAPRatMinus);
#define pLsrRationalMinus(dest, a, b)                               \
    pLsrTaggedInt32TaggedIntSub(PlsrRational, PlsrAPRat, pLsrTaggedInt32RationalSubSlow, dest, a, b)

/* pLsrTaggedInt32RationalMulSlow*/
pLsrTaggedInt32MkTaggedIntMulSlow(Rational, PlsrRational, PlsrAPRat, pLsrAPRatFromSInt32, pLsrAPRatFromSInt64, pLsrAPRatMult);
#define pLsrRationalTimes(dest, a, b) \
    pLsrTaggedInt32TaggedIntMul(PlsrRational, PlsrAPRat, pLsrTaggedInt32RationalMulSlow, dest, a, b)

/* Comparisons */
pLsrTaggedInt32MkTaggedIntEqSlow(Rational, PlsrRational, PlsrAPRat, pLsrAPRatFromSInt32, pLsrAPRatEq);
pLsrTaggedInt32MkTaggedIntNeSlow(Rational, PlsrRational, PlsrAPRat, pLsrAPRatFromSInt32, pLsrAPRatNe);
pLsrTaggedInt32MkTaggedIntLtSlow(Rational, PlsrRational, PlsrAPRat, pLsrAPRatFromSInt32, pLsrAPRatLt);
pLsrTaggedInt32MkTaggedIntGtSlow(Rational, PlsrRational, PlsrAPRat, pLsrAPRatFromSInt32, pLsrAPRatGt);
pLsrTaggedInt32MkTaggedIntLeSlow(Rational, PlsrRational, PlsrAPRat, pLsrAPRatFromSInt32, pLsrAPRatLe);
pLsrTaggedInt32MkTaggedIntGeSlow(Rational, PlsrRational, PlsrAPRat, pLsrAPRatFromSInt32, pLsrAPRatGe);

#define pLsrRationalEQ(dest, a, b) pLsrTaggedInt32TaggedIntEq(pLsrTaggedInt32RationalEqSlow, dest, a, b)
#define pLsrRationalNE(dest, a, b) pLsrTaggedInt32TaggedIntNe(pLsrTaggedInt32RationalNeSlow, dest, a, b)
#define pLsrRationalLT(dest, a, b) pLsrTaggedInt32TaggedIntLt(pLsrTaggedInt32RationalLtSlow, dest, a, b)
#define pLsrRationalGT(dest, a, b) pLsrTaggedInt32TaggedIntGt(pLsrTaggedInt32RationalGtSlow, dest, a, b)
#define pLsrRationalLE(dest, a, b) pLsrTaggedInt32TaggedIntLe(pLsrTaggedInt32RationalLeSlow, dest, a, b)
#define pLsrRationalGE(dest, a, b) pLsrTaggedInt32TaggedIntGe(pLsrTaggedInt32RationalGeSlow, dest, a, b)

/* Constants */
#define pLsrRationalOne pLsrTaggedInt32One
#define pLsrRationalZero pLsrTaggedInt32Zero

#define pLsrRationalCheckTaggedInt32Assertions() pLsrTaggedInt32Check(PlsrRational, PlsrAPRat)

static void pLsrRationalCheckAssertions() {
    assert(pLsrTaggedInt32Max == pLsrSmallRationalMax);
    assert(pLsrTaggedInt32Min == pLsrSmallRationalMin);
    pLsrRationalCheckTaggedInt32Assertions();
}

#else /* P_USE_TAGGED_RATIONALS*/

/**********************************************************************
 * This file implements the Rational type in terms of
 * an arbitrary precision rational package APRat, with associated
 * operations.
 */

/* Basic constructors */
typedef PlsrAPRat PlsrRational;
typedef PlsrAPRatS PlsrRationalU;

/* Basic queries and destructors */

#define pLsrRationalIsIntegral pLsrAPRatIsIntegral
#define pLsrRationalStaticInit pLsrAPRatStaticInit
#define pLsrRationalStaticUnboxedDef pLsrAPRatStaticUnboxedDef

/* Conversions */

#define pLsrRationalFromIntegers pLsrAPRatFromIntegers
#define pLsrIntegersFromRational pLsrIntegersFromAPRat

#define pLsrRatNumerator pLsrAPRatNumerator
#define pLsrRatDenominator pLsrAPRatDenominator

#define pLsrRationalFromInteger(dest, a) ((dest) = pLsrAPRatFromInteger(a))


#define pLsrIntegerFromRational(dest, a) ((dest) = pLsrIntegerFromAPRat(a))

#define pLsrRationalFromSIntp(dest, a) ((dest) = pLsrAPRatFromSIntp(a))
#define pLsrSIntpFromRational(dest, a) ((dest) = pLsrSIntpFromAPRat(a))

#define pLsrRationalFromUIntp(dest, a) ((dest) = pLsrAPRatFromUIntp(a))
#define pLsrUIntpFromRational(dest, a) ((dest) = pLsrUIntpFromAPRat(a))

#define pLsrRationalFromSInt32(dest, a) ((dest) = pLsrAPRatFromSInt32(a))
#define pLsrSInt32FromRational(dest, a) ((dest) = pLsrSInt32FromAPRat(a))

#define pLsrSmallRationalFromSInt32 pLsrRationalFromSInt32

#define pLsrRationalFromUInt32(dest, a) ((dest) = pLsrAPRatFromUInt32(a))
#define pLsrUInt32FromRational(dest, a) ((dest) = pLsrUInt32FromAPRat(a))

#define pLsrRationalToUInt32Checked(dest, a) ((dest) = pLsrAPRatToUInt32Checked(a))

#define pLsrCStringFromRational pLsrCStringFromAPRat
#define pLsrRationalFromCString pLsrAPRatFromCString

/* Arithmetic */

#define pLsrRationalNegate(dest, a) ((dest) = pLsrAPRatNeg(a))
#define pLsrRationalPlus(dest, a, b) ((dest) = pLsrAPRatPlus(a, b))
#define pLsrRationalMinus(dest, a, b) ((dest) = pLsrAPRatMinus(a, b))
#define pLsrRationalTimes(dest, a, b) ((dest) = pLsrAPRatMult(a, b))

/* Comparisons */
#define pLsrRationalEQ(dest, a, b) ((dest) = pLsrAPRatEq(a, b))
#define pLsrRationalNE(dest, a, b) ((dest) = pLsrAPRatNe(a, b))
#define pLsrRationalLT(dest, a, b) ((dest) = pLsrAPRatLt(a, b))
#define pLsrRationalGT(dest, a, b) ((dest) = pLsrAPRatGt(a, b))
#define pLsrRationalLE(dest, a, b) ((dest) = pLsrAPRatLe(a, b))
#define pLsrRationalGE(dest, a, b) ((dest) = pLsrAPRatGe(a, b))

/* Constants */
#define pLsrRationalOne pLsrAPRatOne
#define pLsrRationalZero pLsrAPRatZero

static void pLsrRationalCheckAssertions() {
}

#endif /* P_USE_TAGGED_RATIONALS*/

#endif /* !_PLSR_RATIONAL_H_ */
