/* The Haskell Research Compiler */
/*
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
 */

/* Arbitrary precision numbers */

#ifndef _PLSR_AP_RATIONAL_H_
#define _PLSR_AP_RATIONAL_H_

/**********************************************************************
 * Arbitrary precision rationals
 */

/* Represents the rational x such that c0+c1*x=0
 * Invariants: gcd(c0,c1)=1 and c1>0
 */

typedef struct PlsrAPRatS_ {
    PlsrVTable vtable;
    PlsrInteger c0;
    PlsrInteger c1;
} PlsrAPRatS;

#ifdef P_USE_PILLAR
typedef PlsrRef PlsrAPRat;
#else /* !P_USE_PILLAR */
typedef PlsrAPRatS* PlsrAPRat;
#endif /* !P_USE_PILLAR */

#define pLsrAPRatRep(l) ((PlsrAPRatS*)(l))
#define pLsrAPRatC0(l) pLsrAPRatRep(l)->c0
#define pLsrAPRatC1(l) pLsrAPRatRep(l)->c1


#define pLsrAPRatPadding                                        \
    (sizeof(PlsrAPRatS) - sizeof(PlsrVTable) - sizeof(PlsrInteger) - sizeof(PlsrInteger))
pLsrVTableStatic(pLsrAPRatVTable_, "*rational*", pLsrAPRatPadding);
#define pLsrAPRatVTable &pLsrAPRatVTable_

/* Note: the parameter names are significant.  Using c0 and c1 breaks,
 * since apparently the gnu and Intel C compilers confuse the parameter
 * with the field name.  Uggh.
 */
#define pLsrAPRatStaticUnboxedDef(uvar)                         \
    static PlsrAPRatS uvar = { .vtable = pLsrAPRatVTable, }

#define pLsrAPRatStaticInitFromIntegers(var, c00, c10)                  \
    do {                                                                \
        if (pLsrIntegerIsRef) {                                         \
            pLsrWriteBarrierRefBase(var, pLsrAPRatC0(var), c00);        \
            pLsrWriteBarrierRefBase(var, pLsrAPRatC1(var), c10);        \
        } else {                                                        \
            pLsrAPRatC0(var) = c00;                                     \
            pLsrAPRatC1(var) = c10;                                     \
        }                                                               \
    } while (0)

#define pLsrAPRatStaticInit(var, c00, c10) \
    pLsrAPRatStaticInitFromIntegers(var, pLsrIntegerFromCString(c00), pLsrIntegerFromCString(c10))

#define pLsrAPRatStaticRef(uvar)                \
    ((PlsrAPRat) &uvar)

/* Constants */

static PlsrAPRatS pLsrAPRatZero_ = { .vtable = pLsrAPRatVTable,
                                     .c0 = NULL,
                                     .c1 = NULL };
#define pLsrAPRatZero ((PlsrAPRat) &pLsrAPRatZero_)

static PlsrAPRatS pLsrAPRatOne_ = { .vtable = pLsrAPRatVTable,
                                    .c0 = NULL,
                                    .c1 = NULL };
#define pLsrAPRatOne ((PlsrAPRat) &pLsrAPRatOne_)

static PlsrAPRatS pLsrAPRatUInt32Max_ = { .vtable = pLsrAPRatVTable,
                                          .c0 = NULL,
                                          .c1 = NULL };
#define pLsrAPRatUInt32Max ((PlsrAPRat) &pLsrAPRatUInt32Max_)

/* Constructors */

/* Assumes that c0 and c1 are already in canonical form */
static PlsrAPRat pLsrAPRatMk(PlsrInteger c0, PlsrInteger c1) {
    uintp size = sizeof(PlsrAPRatS);
    PlsrAPRat a;
    noyield {
        pLsrAlloc(PlsrAPRat, a, pLsrAPRatVTable, size);
        if (pLsrIntegerIsRef) {
            pLsrWriteBarrierRefOptBase(a, pLsrAPRatC0(a), c0);
            pLsrWriteBarrierRefOptBase(a, pLsrAPRatC1(a), c1);
        } else {
            pLsrAPRatC0(a) = c0;
            pLsrAPRatC1(a) = c1;
        }
    }
    return a;
}

/* Create a rational number r from two integers c0, c1 such
 * that r = -c0/c1, where gcd(c0, c1) may not be 1 */
static PlsrAPRat pLsrAPRatFromIntegersNorm(PlsrInteger num, PlsrInteger den)
{
    PlsrInteger g = pLsrIntegerGcd(num, den);
    PlsrBoolean b;
    pLsrIntegerLT(b, den, pLsrIntegerZero);
    if (b) {
        pLsrIntegerNegate(num, num);
        pLsrIntegerNegate(den, den);
    }
    pLsrIntegerDivT(num, num, g);
    pLsrIntegerDivT(den, den, g);
    return pLsrAPRatMk(num, den);
}

static PlsrAPRat pLsrAPRatFromSIntps(sintp a, sintp b) {
    PlsrInteger i1;
    pLsrIntegerFromSIntp(i1, -a);
    PlsrInteger i2;
    pLsrIntegerFromSIntp(i2, b);
    return pLsrAPRatFromIntegersNorm(i1, i2);
}


/* Basic queries and destructors */

static PlsrBoolean pLsrAPRatIsIntegral(PlsrAPRat r)
{
    PlsrBoolean b;
    pLsrIntegerEQ(b, pLsrAPRatC1(r), pLsrIntegerOne);
    return b;
}

static PlsrInteger pLsrAPRatNumerator(PlsrAPRat r) {
    PlsrInteger i = pLsrAPRatC0(r);
    pLsrIntegerNegate(i, i);
    return i;
}

static PlsrInteger pLsrAPRatDenominator(PlsrAPRat r) {
    return pLsrAPRatC1(r);
}

/* Conversions */

static PlsrAPRat pLsrAPRatFromIntegers(PlsrInteger a, PlsrInteger b) {
    PlsrInteger n;
    pLsrIntegerNegate(n, a);
    return pLsrAPRatFromIntegersNorm(n, b);
}

static void pLsrIntegersFromAPRat(PlsrInteger* numO, PlsrInteger* denO, PlsrAPRat r) {
    pLsrIntegerNegate(*numO, pLsrAPRatC0(r));
    *denO = pLsrAPRatC1(r);
    return;
}

static PlsrAPRat pLsrAPRatFromInteger(PlsrInteger i)
{
    return pLsrAPRatFromIntegers(i, pLsrIntegerOne);
}

static PlsrInteger pLsrIntegerFromAPRat(PlsrAPRat r)
{
    assert(pLsrAPRatIsIntegral(r));
    PlsrInteger n;
    pLsrIntegerNegate(n, pLsrAPRatC0(r));
    return n;
}

static PlsrAPRat pLsrAPRatFromSInt32(sint32 i)
{
    PlsrInteger ai;
    pLsrIntegerFromSInt32(ai, i);
    return pLsrAPRatFromInteger(ai);
}

static sint32 pLsrSInt32FromAPRat(PlsrAPRat r)
{
    sint32 i;
    pLsrSInt32FromInteger(i, pLsrIntegerFromAPRat(r));
    return i;
}

static PlsrAPRat pLsrAPRatFromSInt64(sint64 i)
{
    PlsrInteger ai;
    pLsrIntegerFromSInt64(ai, i);
    return pLsrAPRatFromInteger(ai);
}

static sint32 pLsrSInt64FromAPRat(PlsrAPRat r)
{
    sint64 i;
    pLsrSInt64FromInteger(i, pLsrIntegerFromAPRat(r));
    return i;
}

static PlsrAPRat pLsrAPRatFromUInt32(uint32 i)
{
    PlsrInteger ai;
    pLsrIntegerFromUInt32(ai, i);
    return pLsrAPRatFromInteger(ai);
}

static uint32 pLsrUInt32FromAPRat(PlsrAPRat r)
{
    uint32 i;
    pLsrUInt32FromInteger(i, pLsrIntegerFromAPRat(r));
    return i;
}

static PlsrAPRat pLsrAPRatFromSIntp(sintp i)
{
    PlsrInteger ai;
    pLsrIntegerFromSIntp(ai, i);
    return pLsrAPRatFromInteger(ai);
}

static sintp pLsrSIntpFromAPRat(PlsrAPRat r)
{
    sintp i;
    pLsrSIntpFromInteger(i, pLsrIntegerFromAPRat(r));
    return i;
}

static PlsrAPRat pLsrAPRatFromUIntp(uintp i)
{
    PlsrInteger ai;
    pLsrIntegerFromUIntp(ai, i);
    return pLsrAPRatFromInteger(ai);
}

static uintp pLsrUIntpFromAPRat(PlsrAPRat r)
{
    uintp i;
    pLsrUIntpFromInteger(i, pLsrIntegerFromAPRat(r));
    return i;
}

static PlsrAPRat pLsrAPRatFromFloat32(float32 f) {
    pLsrRuntimeError("pLsrAPRatFromFloat32 not implemented");
    return 0;
}

static PlsrAPRat pLsrAPRatFromFloat64(float64 f) {
    pLsrRuntimeError("pLsrAPRatFromFloat64 not implemented");
    return 0;
}

static float32 pLsrFloat32FromAPRat(PlsrAPRat a) {
    pLsrRuntimeError("pLsrFloat32FromAPRat not implemented");
    return 0;
}

static float64 pLsrFloat64FromAPRat(PlsrAPRat a) {
    pLsrRuntimeError("pLsrFloat64FromAPRat not implemented");
    return 0;
}

static PlsrBoolean pLsrAPRatLt(PlsrAPRat a, PlsrAPRat b);
static PlsrBoolean pLsrAPRatGt(PlsrAPRat a, PlsrAPRat b);

static uintp pLsrAPRatToUInt32Checked(PlsrAPRat r)
{
    if (!pLsrAPRatIsIntegral(r)
        || pLsrAPRatLt(r, pLsrAPRatZero)
        || pLsrAPRatGt(r, pLsrAPRatUInt32Max))
        return UINTP_MAX;
    return pLsrUInt32FromAPRat(r);
};

static char* pLsrCStringFromAPRat(PlsrAPRat r) {
    PlsrInteger numI = pLsrAPRatNumerator(r);
    PlsrInteger denI = pLsrAPRatDenominator(r);
    char *num = pLsrCStringFromInteger(numI);
    char *w;
    PlsrBoolean b;
    pLsrIntegerEQ(b, denI, pLsrIntegerOne);
    if (b) {
        w = (char*)pLsrAllocC(sizeof(char) * (strlen(num) + 1));
        sprintf(w, "%s", num);
    } else {
        char *den = pLsrCStringFromInteger(denI);
        w = (char*)pLsrAllocC(sizeof(char) * (strlen(num) + strlen(den) + 1 + 1));
        sprintf(w, "%s/%s", num, den);
        pLsrFreeC(den);
    }
    pLsrFreeC(num);

    return w;
}

static PlsrAPRat pLsrAPRatFromCString(char* s) {
    char *num;
    char *den;
    PlsrInteger numI;
    PlsrInteger denI;
    int i;
    for(i=0;(s[i] != '/') && (s[i] != '\0');i++);
    num = (char*) pLsrAllocC(sizeof(char) * (i + 1));
    strncpy(num, s, i);
    numI = pLsrIntegerFromCString(num);
    pLsrFreeC(num);
    if (s[i] == '\0') {
        denI = pLsrIntegerOne;
    } else {
        den = s + i + 1;
        denI = pLsrIntegerFromCString(den);
    }
    return pLsrAPRatFromIntegers(numI, denI);
 }

/* Arithmetic */

static PlsrAPRat pLsrAPRatNeg(PlsrAPRat r)
{
    PlsrInteger n;
    pLsrIntegerNegate(n, pLsrAPRatC0(r));
    return pLsrAPRatMk(n, pLsrAPRatC1(r));
    /* Normalisation should not be necessary */
};

static PlsrAPRat pLsrAPRatPlus(PlsrAPRat r1,
                               PlsrAPRat r2)
{
    PlsrInteger r1c0 = pLsrAPRatC0(r1);
    PlsrInteger r1c1 = pLsrAPRatC1(r1);
    PlsrInteger r2c0 = pLsrAPRatC0(r2);
    PlsrInteger r2c1 = pLsrAPRatC1(r2);
    PlsrInteger p1;
    PlsrInteger p2;
    PlsrInteger p3;
    PlsrInteger c0;
    PlsrInteger c1;
    pLsrIntegerTimes(p1, r1c0, r2c1);
    pLsrIntegerTimes(p2, r1c1, r2c0);
    pLsrIntegerPlus(c0, p1, p2);
    pLsrIntegerTimes(c1, r1c1, r2c1);
    return pLsrAPRatFromIntegersNorm(c0, c1);
}

static PlsrAPRat pLsrAPRatMinus(PlsrAPRat r1,
                                PlsrAPRat r2)
{
    PlsrInteger r1c0 = pLsrAPRatC0(r1);
    PlsrInteger r1c1 = pLsrAPRatC1(r1);
    PlsrInteger r2c0 = pLsrAPRatC0(r2);
    PlsrInteger r2c1 = pLsrAPRatC1(r2);
    PlsrInteger p1;
    PlsrInteger p2;
    PlsrInteger p3;
    PlsrInteger c0;
    PlsrInteger c1;
    pLsrIntegerTimes(p1, r1c0, r2c1);
    pLsrIntegerTimes(p2, r1c1, r2c0);
    pLsrIntegerMinus(c0, p1, p2);
    pLsrIntegerTimes(c1, r1c1, r2c1);
    return pLsrAPRatFromIntegersNorm(c0, c1);
}

static PlsrAPRat pLsrAPRatMult(PlsrAPRat r1,
                               PlsrAPRat r2)
{
    PlsrInteger r1c0 = pLsrAPRatC0(r1);
    PlsrInteger r1c1 = pLsrAPRatC1(r1);
    PlsrInteger r2c0 = pLsrAPRatC0(r2);
    PlsrInteger r2c1 = pLsrAPRatC1(r2);
    PlsrInteger c0;
    PlsrInteger c1;
    pLsrIntegerNegate(r1c0, r1c0);
    pLsrIntegerTimes(c0, r1c0, r2c0);
    pLsrIntegerTimes(c1, r1c1, r2c1);
    return pLsrAPRatFromIntegersNorm(c0, c1);
}

/* Comparisons */

static PlsrBoolean pLsrAPRatEq(PlsrAPRat r1, PlsrAPRat r2)
{
    PlsrInteger r1c0 = pLsrAPRatC0(r1);
    PlsrInteger r1c1 = pLsrAPRatC1(r1);
    PlsrInteger r2c0 = pLsrAPRatC0(r2);
    PlsrInteger r2c1 = pLsrAPRatC1(r2);
    PlsrBoolean b1, b2;
    pLsrIntegerEQ(b1, r1c0, r2c0);
    if (!b1) return 0;
    pLsrIntegerEQ(b2, r1c1, r2c1);
    return b2;
}

static PlsrBoolean pLsrAPRatNe(PlsrAPRat r1, PlsrAPRat r2)
{
    return !pLsrAPRatEq(r1, r2);
}

static PlsrBoolean pLsrAPRatLt(PlsrAPRat r1, PlsrAPRat r2)
{
    PlsrInteger r1c0 = pLsrAPRatC0(r1);
    PlsrInteger r1c1 = pLsrAPRatC1(r1);
    PlsrInteger r2c0 = pLsrAPRatC0(r2);
    PlsrInteger r2c1 = pLsrAPRatC1(r2);
    PlsrInteger p1;
    PlsrInteger p2;
    PlsrBoolean b;
    pLsrIntegerTimes(p1, r1c1, r2c0);
    pLsrIntegerTimes(p2, r1c0, r2c1);
    pLsrIntegerLT(b, p1, p2);
    return b;
}

static PlsrBoolean pLsrAPRatGt(PlsrAPRat r1, PlsrAPRat r2)
{
    PlsrInteger r1c0 = pLsrAPRatC0(r1);
    PlsrInteger r1c1 = pLsrAPRatC1(r1);
    PlsrInteger r2c0 = pLsrAPRatC0(r2);
    PlsrInteger r2c1 = pLsrAPRatC1(r2);
    PlsrInteger p1;
    PlsrInteger p2;
    PlsrBoolean b;
    pLsrIntegerTimes(p1, r1c1, r2c0);
    pLsrIntegerTimes(p2, r1c0, r2c1);
    pLsrIntegerGT(b, p1, p2);
    return b;
}

static PlsrBoolean pLsrAPRatLe(PlsrAPRat r1, PlsrAPRat r2)
{
    PlsrInteger r1c0 = pLsrAPRatC0(r1);
    PlsrInteger r1c1 = pLsrAPRatC1(r1);
    PlsrInteger r2c0 = pLsrAPRatC0(r2);
    PlsrInteger r2c1 = pLsrAPRatC1(r2);
    PlsrInteger p1;
    PlsrInteger p2;
    PlsrBoolean b;
    pLsrIntegerTimes(p1, r1c1, r2c0);
    pLsrIntegerTimes(p2, r1c0, r2c1);
    pLsrIntegerLE(b, p1, p2);
    return b;
}

static PlsrBoolean pLsrAPRatGe(PlsrAPRat r1, PlsrAPRat r2)
{
    PlsrInteger r1c0 = pLsrAPRatC0(r1);
    PlsrInteger r1c1 = pLsrAPRatC1(r1);
    PlsrInteger r2c0 = pLsrAPRatC0(r2);
    PlsrInteger r2c1 = pLsrAPRatC1(r2);
    PlsrInteger p1;
    PlsrInteger p2;
    PlsrBoolean b;
    pLsrIntegerTimes(p1, r1c1, r2c0);
    pLsrIntegerTimes(p2, r1c0, r2c1);
    pLsrIntegerGE(b, p1, p2);
    return b;
}

static sint32 pLsrAPRatCheckRangeSInt32(PlsrAPRat a, sint32 upper, sint32 lower) {
    if (pLsrAPRatIsIntegral(a)) {
        PlsrInteger im = pLsrAPRatNumerator(a);
        PlsrInteger i;
        pLsrIntegerNegate(i, im);
        return pLsrIntegerCheckRangeSInt32(i, upper, lower);
    }
    return SINT32_MIN;
}

#define pLsrAPRatSize (sizeof(PlsrAPRatS))
#define pLsrAPRatAlignment 4

static void pLsrAPRatRegisterVTables()
{
    static PgcIsRef pLsrAPRatRefs[pLsrAPRatSize/P_WORD_SIZE] = { 0, pLsrIntegerIsRef, pLsrIntegerIsRef };

    assert(pLsrAPRatSize/P_WORD_SIZE == 3);

    pLsrVTableRegister(pLsrAPRatVTable, pLsrAPRatAlignment, pLsrAPRatSize, pLsrAPRatRefs, 0, 0, 0,
                       PGC_ALWAYS_IMMUTABLE, 0);
}

#define pLsrAPRatGlobalsCount 3

static PlsrObjectB pLsrAPRatGlobals[] =
    {
        (PlsrObjectB) pLsrAPRatZero,
        (PlsrObjectB) pLsrAPRatOne,
        (PlsrObjectB) pLsrAPRatUInt32Max,
        (PlsrObjectB) NULL /* This must be last */
    };

static void pLsrAPRatRegisterGlobals() {
    assert(pLsrAPRatGlobals[pLsrAPRatGlobalsCount] == NULL);
    pLsrGcRegisterGlobals(pLsrAPRatGlobals, pLsrAPRatGlobalsCount);
}

static void pLsrAPRatInitialize() {
    pLsrAPRatStaticInitFromIntegers(pLsrAPRatZero, pLsrIntegerZero, pLsrIntegerOne);
    pLsrAPRatStaticInitFromIntegers(pLsrAPRatOne, pLsrIntegerMinusOne, pLsrIntegerOne);
    pLsrAPRatStaticInitFromIntegers(pLsrAPRatUInt32Max, pLsrIntegerMinusUInt32Max, pLsrIntegerOne);
}

#endif /* !_PLSR_AP_RATIONAL_H_ */
