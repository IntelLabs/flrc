/* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

#ifndef _PLSR_AP_INTEGER_H_
#define _PLSR_AP_INTEGER_H_

#ifdef PLSR_GMP_USE_MALLOC
#define pLsrAPIntIsPinned 0
#else /* PLSR_GMP_USE_MALLOC */

#ifdef PLSR_GMP_USE_PCDECL

#define pLsrAPIntIsPinned 0

#ifndef __pillar2c__
#error "pcdecl only supported on pillar2c"
#endif /* __pillar2c__ */

#else /* PLSR_GMP_USE_PCDECL*/

#ifdef PLSR_GMP_USE_PINNING
#define pLsrAPIntIsPinned 1

#else /* !PLSR_GMP_USE_PINNING */

#ifdef PLSR_GMP_USE_GCMALLOC

#define pLsrAPIntIsPinned 1
#define PLSR_GMP_REPLACE_MALLOC

#else /* !PLSR_GMP_USE_GCMALLOC */

#ifdef PLSR_GMP_USE_FORCE_GC
#define pLsrAPIntIsPinned 0
#define PLSR_GMP_USE_PCDECL
#define PLSR_GMP_FORCE_GC
#define PLSR_GMP_REPLACE_MALLOC
#else /* !PLSR_GMP_USE_FORCE_GC */
#error "No GMP implementation selected"
#endif /* !PLSR_GMP_USE_FORCE_GC */

#endif /* !PLSR_GMP_USE_GCMALLOC */

#endif /* PLSR_GMP_USE_PINNING */

#endif /* !PLSR_GMP_USE_PCDECL*/

#endif /* !PLSR_GMP_USE_MALLOC*/


#ifdef PLSR_GMP_USE_PCDECL

#  pragma pillar_push_cc(__pcdecl)
#include <gmp.h>
#  pragma pillar_pop_cc

#else /* PLSR_GMP_USE_PCDECL */
#ifdef P_USE_PILLAR
#  pragma pillar_managed(off)
#  define to __to__
#endif /* P_USE_PILLAR */

#include <gmp.h>

#ifdef P_USE_PILLAR
#  undef to
#  pragma pillar_managed(on)
#endif
#endif /* !PLSR_GMP_USE_PCDECL */
/* Types */

#ifdef PLSR_GMP_USE_MALLOC

typedef struct PlsrAPIntS_ {
    PlsrVTable vtable;
    mpz_t *z;
} PlsrAPIntS;

#define pLsrAPIntPadding (sizeof(PlsrAPIntS) - sizeof(PlsrVTable) - sizeof(mpz_t*))

#else /* PLSR_GMP_USE_MALLOC */

typedef struct PlsrAPIntS_ {
    PlsrVTable vtable;
    mpz_t z;
} PlsrAPIntS;

#define pLsrAPIntPadding (sizeof(PlsrAPIntS) - sizeof(PlsrVTable) - sizeof(mpz_t))

#endif /* !PLSR_GMP_USE_MALLOC */

/* This must be pinned, and we must add finalizers for it */
pLsrVTableStatic(pLsrAPIntVTable_, VNoneTag, "*ap integer*", pLsrAPIntPadding);
#define pLsrAPIntVTable (&pLsrAPIntVTable_)


#ifdef P_USE_PILLAR
typedef PlsrRef PlsrAPInt;
#else /* !P_USE_PILLAR */
typedef PlsrAPIntS* PlsrAPInt;
#endif /* !P_USE_PILLAR */

#define pLsrAPIntAlignment 4
#define pLsrAPIntSize (sizeof(PlsrAPIntS))

#ifdef PLSR_GMP_USE_MALLOC
#define pLsrAPIntGetZPtr(i) (((PlsrAPIntS*) i)->z)
#define pLsrAPIntGetZ(i) (*(((PlsrAPIntS*) i)->z))
#else
#define pLsrAPIntGetZ(i) (((PlsrAPIntS*) i)->z)
#endif

#define pLsrAPIntKeepLive1(v)                   \
    do {                                        \
        volatile PlsrAPInt pLsrAPIntKeepLive1A; \
        pLsrAPIntKeepLive1A = v;                \
    } while(0);

#define pLsrAPIntKeepLive2(v0, v1)              \
    do {                                        \
        volatile PlsrAPInt pLsrAPIntKeepLive2A; \
        pLsrAPIntKeepLive2A = v0;               \
        pLsrAPIntKeepLive2A = v1;               \
    } while(0);

#ifdef PLSR_AP_INT_TRACE
static void pLsrAPIntShow1(char * s, PlsrAPInt i)
{
    char* res = pLsrAllocC(mpz_sizeinbase (pLsrAPIntGetZ(i), 10) + 2);
    mpz_get_str(res, 10, pLsrAPIntGetZ(i));
    printf("APInt: %s, %s\n", s, res);
    pLsrFreeC(res);
}
static void pLsrAPIntShow2(char * s, PlsrAPInt i1, PlsrAPInt i2)
{
    char* res1 = pLsrAllocC(mpz_sizeinbase (pLsrAPIntGetZ(i1), 10) + 2);
    mpz_get_str(res1, 10, pLsrAPIntGetZ(i1));
    char* res2 = pLsrAllocC(mpz_sizeinbase (pLsrAPIntGetZ(i2), 10) + 2);
    mpz_get_str(res2, 10, pLsrAPIntGetZ(i2));
    printf("APInt: %s, (%s, %s)\n", s, res1, res2);
    pLsrFreeC(res1);
    pLsrFreeC(res2);
}
static void pLsrAPIntShow3(char * s, PlsrAPInt i1, PlsrAPInt i2, PlsrAPInt i3)
{
    char* res1 = pLsrAllocC(mpz_sizeinbase (pLsrAPIntGetZ(i1), 10) + 2);
    mpz_get_str(res1, 10, pLsrAPIntGetZ(i1));
    char* res2 = pLsrAllocC(mpz_sizeinbase (pLsrAPIntGetZ(i2), 10) + 2);
    mpz_get_str(res2, 10, pLsrAPIntGetZ(i2));
    char* res3 = pLsrAllocC(mpz_sizeinbase (pLsrAPIntGetZ(i3), 10) + 2);
    mpz_get_str(res3, 10, pLsrAPIntGetZ(i3));
    printf("APInt: %s, (%s, %s, %s)\n", s, res1, res2, res3);
    pLsrFreeC(res1);
    pLsrFreeC(res2);
    pLsrFreeC(res3);
}
static void pLsrAPIntShow4(char * s, PlsrAPInt i1, PlsrAPInt i2, PlsrAPInt i3, PlsrAPInt i4)
{
    char* res1 = pLsrAllocC(mpz_sizeinbase (pLsrAPIntGetZ(i1), 10) + 2);
    mpz_get_str(res1, 10, pLsrAPIntGetZ(i1));
    char* res2 = pLsrAllocC(mpz_sizeinbase (pLsrAPIntGetZ(i2), 10) + 2);
    mpz_get_str(res2, 10, pLsrAPIntGetZ(i2));
    char* res3 = pLsrAllocC(mpz_sizeinbase (pLsrAPIntGetZ(i3), 10) + 2);
    mpz_get_str(res3, 10, pLsrAPIntGetZ(i3));
    char* res4 = pLsrAllocC(mpz_sizeinbase (pLsrAPIntGetZ(i4), 10) + 2);
    mpz_get_str(res4, 10, pLsrAPIntGetZ(i4));
    printf("APInt: %s, (%s, %s, %s, %s)\n", s, res1, res2, res3, res4);
    pLsrFreeC(res1);
    pLsrFreeC(res2);
    pLsrFreeC(res3);
    pLsrFreeC(res4);
}

#define pLsrAPIntTrace0(s) printf("APInt: %s\n", s)
#define pLsrAPIntTrace1(s, i1)  pLsrAPIntShow1(s, i1)
#define pLsrAPIntTrace2(s, i1, i2) pLsrAPIntShow2(s, i1, i2)
#define pLsrAPIntTrace3(s, i1, i2, i3) pLsrAPIntShow3(s, i1, i2, i3)
#define pLsrAPIntTrace4(s, i1, i2, i3, i4) pLsrAPIntShow4(s, i1, i2, i3, i4)
#define pLsrAPIntTraceFmt1(s, i1)  printf(s, i1)
#else
#define pLsrAPIntTrace0(s)
#define pLsrAPIntTrace1(s, i1)
#define pLsrAPIntTrace2(s, i1, i2)
#define pLsrAPIntTrace3(s, i1, i2, i3)
#define pLsrAPIntTrace4(s, i1, i2, i3, i4)
#define pLsrAPIntTraceFmt1(s, i1)
#endif

/* Allocation */

#ifdef P_USE_PILLAR
#  pragma pillar_managed(off)
#  define to __to__
static void pLsrAPIntFinalize(PlsrAPInt i)
{
    mpz_clear(pLsrAPIntGetZ(i));

#ifdef PLSR_GMP_USE_MALLOC
    pLsrFreeC(pLsrAPIntGetZPtr(i));
#endif

    pLsrAPIntKeepLive1(i);
}

#  undef to
#pragma pillar_managed(on)
#endif

#ifdef PLSR_GMP_FORCE_GC
/* At some point, this should be made thread safe */
static int pLsrGmpAllocated=0;
static int pLsrGmpMemLimit=100*1000*1000;
#define pLsrAPIntGCCheck()                                      \
    do {                                                        \
        if (pLsrGmpAllocated > pLsrGmpMemLimit) {               \
            pgc_force_gc();                                     \
        }                                                       \
    } while (0)
#else /* PLSR_GMP_FORCE_GC */

#define pLsrAPIntGCCheck()

#endif /* PLSR_GMP_FORCE_GC */

/* NB: This does not initialize the mpz_t structure */
static PlsrAPInt pLsrAPIntNewUninit()
{
    PlsrAPInt res;
    pLsrAPIntGCCheck();
#ifdef PLSR_GMP_USE_PINNING
    pLsrAllocPinnedFinalizable(PlsrAPInt, res, pLsrAPIntVTable, sizeof(PlsrAPIntS));
#else
    pLsrAllocFinalizable(PlsrAPInt, res, pLsrAPIntVTable, sizeof(PlsrAPIntS));
#endif
#ifdef PLSR_GMP_USE_MALLOC
    pLsrAPIntGetZPtr(res) = (mpz_t*) pLsrAllocC(sizeof(mpz_t));
#endif
    return res;
}

static PlsrAPInt pLsrAPIntNew()
{
    PlsrAPInt res;
    pLsrAPIntGCCheck();
#ifdef PLSR_GMP_USE_PINNING
    pLsrAllocPinnedFinalizable(PlsrAPInt, res, pLsrAPIntVTable, sizeof(PlsrAPIntS));
#else
    pLsrAllocFinalizable(PlsrAPInt, res, pLsrAPIntVTable, sizeof(PlsrAPIntS));
#endif
#ifdef PLSR_GMP_USE_MALLOC
    pLsrAPIntGetZPtr(res) = (mpz_t*) pLsrAllocC(sizeof(mpz_t));
#endif
    mpz_init(pLsrAPIntGetZ(res));
    return res;
}

static PlsrAPInt pLsrAPIntNewFromCString(char *s)
{
    PlsrAPInt res = pLsrAPIntNewUninit();
    if (mpz_init_set_str(pLsrAPIntGetZ(res), s, 0) != 0) {
        pLsrRuntimeError("Failed to initialize ap integer");
    }
    pLsrAPIntTrace1("New from string", res);
    return res;
}

static PlsrAPInt pLsrAPIntNewFromSignedLong(long int si)
{
    PlsrAPInt res = pLsrAPIntNewUninit();
    mpz_init_set_si(pLsrAPIntGetZ(res), si);
    return res;
}

static PlsrAPInt pLsrAPIntNewFromUnsignedLong(unsigned long int ui)
{
    PlsrAPInt res = pLsrAPIntNewUninit();
    mpz_init_set_ui(pLsrAPIntGetZ(res), ui);
    return res;
}


static PlsrAPInt pLsrAPIntNewFromFloat32(float32 f)
{
    PlsrAPInt res = pLsrAPIntNewUninit();
    mpz_init_set_d(pLsrAPIntGetZ(res), (float64) f);
    pLsrAPIntTrace1("New from float32", res);
    return res;
}

static PlsrAPInt pLsrAPIntNewFromFloat64(float64 f)
{
    PlsrAPInt res = pLsrAPIntNewUninit();
    mpz_init_set_d(pLsrAPIntGetZ(res), f);
    pLsrAPIntTrace1("New from float64", res);
    return res;
}


static PlsrAPInt pLsrAPIntNewFromAPInt(PlsrAPInt a)
{
    PlsrAPInt res = pLsrAPIntNewUninit();
    mpz_init_set(pLsrAPIntGetZ(res), pLsrAPIntGetZ(a));
    pLsrAPIntKeepLive1(a);
    return res;
}

static void pLsrAPIntDestroy(PlsrAPInt i)
{
    mpz_clear(pLsrAPIntGetZ(i));
#ifdef PLSR_GMP_USE_MALLOC
    pLsrFreeC(pLsrAPIntGetZPtr(i));
#endif
    pLsrAPIntKeepLive1(i);
}

/* Constants */

/* Globals */

#define PLSR_AP_INT_BACKPATCH_GLOBALS 1

#define pLsrAPIntStaticGlobalNew(dv, bytes)                     \
    static PlsrAPIntS dv = {.vtable = pLsrAPIntVTable, }

/* These are global, and will never be freed, */
static void pLsrAPIntStaticGlobalInitFromCString(PlsrAPInt i, char* s)
{
#ifdef PLSR_GMP_USE_MALLOC
    pLsrAPIntGetZPtr(i) = (mpz_t*) pLsrAllocC(sizeof(mpz_t));
#endif
    /* Use leading characters of string to determine base*/
    if (mpz_init_set_str(pLsrAPIntGetZ(i), s, 0) != 0) {
        pLsrRuntimeError("Failed to initialize global ap integer");
    }
}

static void pLsrAPIntStaticGlobalInitFromSInt32(PlsrAPInt z, sint32 si)
{
#ifdef PLSR_GMP_USE_MALLOC
    pLsrAPIntGetZPtr(z) = (mpz_t*) pLsrAllocC(sizeof(mpz_t));
#endif
    mpz_init_set_si(pLsrAPIntGetZ(z), si);
}

static void pLsrAPIntStaticGlobalInitFromUInt32(PlsrAPInt z, uint32 ui)
{
#ifdef PLSR_GMP_USE_MALLOC
    pLsrAPIntGetZPtr(z) = (mpz_t*) pLsrAllocC(sizeof(mpz_t));
#endif
    mpz_init_set_ui(pLsrAPIntGetZ(z), ui);
}

pLsrAPIntStaticGlobalNew(pLsrAPIntZero_, 0);
#define pLsrAPIntZero ((PlsrAPInt) &pLsrAPIntZero_)
pLsrAPIntStaticGlobalNew(pLsrAPIntOne_, 0);
#define pLsrAPIntOne ((PlsrAPInt) &pLsrAPIntOne_)
pLsrAPIntStaticGlobalNew(pLsrAPIntMinusOne_, 0);
#define pLsrAPIntMinusOne ((PlsrAPInt) &pLsrAPIntMinusOne_)
pLsrAPIntStaticGlobalNew(pLsrAPIntMinusUInt32Max_, 0);
#define pLsrAPIntMinusUInt32Max ((PlsrAPInt) &pLsrAPIntMinusUInt32Max_)

/* Conversions */

/* Unsigned Integer Conversions */

static PlsrAPInt pLsrAPIntFromUInt32(uint32 i)
{
    PlsrAPInt res = pLsrAPIntNewFromUnsignedLong(i);
    pLsrAPIntTrace1("New from uint32", res);
    return res;
}

static PlsrAPInt pLsrAPIntFromUInt64(uint64 i)
{
    PlsrAPInt res;
    if (sizeof(uint64) <= sizeof(unsigned long int))
        res = pLsrAPIntNewFromUnsignedLong(i);
    else if (i <= UINT32_MAX) {
        res = pLsrAPIntFromUInt32(i);
    } else {
        uint64 ui = i;
        uint32 upper = (uint32) (ui >> 32);
        uint32 lower = (uint32) ui;
        PlsrAPInt z = pLsrAPIntFromUInt32(upper);
        mpz_mul_2exp(pLsrAPIntGetZ(z), pLsrAPIntGetZ(z), 32);
        mpz_add_ui(pLsrAPIntGetZ(z), pLsrAPIntGetZ(z), lower);
        res = z;
    }
    pLsrAPIntTrace1("New from uint64", res);
    return res;
}

static PlsrAPInt pLsrAPIntFromUIntp(uintp i)
{
    PlsrAPInt res;
    if (sizeof(uintp) <= sizeof(uint32))
        res = pLsrAPIntFromUInt32(i);
    else if (sizeof(uintp) <= sizeof(uint64))
        res = pLsrAPIntFromUInt64(i);
    else {
        pLsrRuntimeError("UIntp too large");
        res = 0;
    }
    pLsrAPIntTrace1("New from uintp", res);
    return res;
}

static uint32 pLsrUInt32FromAPInt(PlsrAPInt a)
{
    uint32 res = (uint32) mpz_get_ui(pLsrAPIntGetZ(a));
    pLsrAPIntKeepLive1(a);
    pLsrAPIntTrace1("To uint32", a);
    return res;
}

static uint64 pLsrUInt64FromAPInt(PlsrAPInt a)
{
    uint64 res = 0;
    if (sizeof(uint64) <= sizeof(unsigned long int) || mpz_fits_ulong_p(pLsrAPIntGetZ(a))) {
        res = (uint64) mpz_get_ui(pLsrAPIntGetZ(a));
    } else {
        mpz_t tmp1;
        mpz_t tmp2;
        mpz_init_set(tmp1, pLsrAPIntGetZ(a));
        mpz_init_set(tmp2, tmp1);
        mpz_fdiv_q_2exp(tmp1, tmp1, 32);
        mpz_fdiv_r_2exp(tmp2, tmp2, 32);
        res = (uint64) mpz_get_ui(tmp1);
        res = res << 32;
        res += (uint64) mpz_get_ui(tmp2);
        mpz_clear(tmp1);
        mpz_clear(tmp2);
    }
    pLsrAPIntKeepLive1(a);
    pLsrAPIntTrace1("To uint64", a);
    return res;
}

static uintp pLsrUIntpFromAPInt(PlsrAPInt a)
{
    uintp res;
    if (sizeof(uintp) <= sizeof(uint32))
        res = pLsrUInt32FromAPInt(a);
    else if (sizeof(uintp) <= sizeof(uint64))
        res = pLsrUInt64FromAPInt(a);
    else {
        pLsrRuntimeError("UIntp too large");
        res = 0;
    }
    pLsrAPIntTrace1("To uintp", a);
    return res;
}

/* Signed Integer Conversions */

static PlsrAPInt pLsrAPIntFromSInt32(sint32 i)
{
    PlsrAPInt res = pLsrAPIntNewFromSignedLong(i);
    pLsrAPIntTrace1("From sint32", res);
    return res;
}

static PlsrAPInt pLsrAPIntFromSInt64(sint64 i)
{
    PlsrAPInt res;
    if (sizeof(sint64) <= sizeof(long int))
        res = pLsrAPIntNewFromSignedLong(i);
    else if (i <= ((sint64) SINT32_MAX) && i >= ((sint64) SINT32_MIN)) {
        res = pLsrAPIntFromSInt32((sint32) i);
    } else {
        uint64 ui = pLsrAbs64(i);
        uint32 upper = (uint32) (ui >> 32);
        uint32 lower = (uint32) ui;
        PlsrAPInt z = pLsrAPIntFromUInt32(upper);
        mpz_mul_2exp(pLsrAPIntGetZ(z), pLsrAPIntGetZ(z), 32);
        mpz_add_ui(pLsrAPIntGetZ(z), pLsrAPIntGetZ(z), lower);
        if (i < 0) {
            mpz_neg(pLsrAPIntGetZ(z), pLsrAPIntGetZ(z));
        }
        res = z;
    }
    pLsrAPIntTrace1("From sint64", res);
    return res;
}

static PlsrAPInt pLsrAPIntFromSIntp(sintp i)
{
    PlsrAPInt res;
    if (sizeof(sintp) <= sizeof(sint32))
        res = pLsrAPIntFromSInt32(i);
    else if (sizeof(sintp) <= sizeof(sint64))
        res = pLsrAPIntFromSInt64(i);
    else {
        pLsrRuntimeError("SIntp too large");
        res = 0;
    }
    pLsrAPIntTrace1("From sint64", res);
    return res;
}

static sint32 pLsrSInt32FromAPInt(PlsrAPInt a)
{
    sint32 res = (sint32) mpz_get_si(pLsrAPIntGetZ(a));
    pLsrAPIntTraceFmt1("To sint32 %d", res);
    return res;
}

static sint64 pLsrSInt64FromAPInt(PlsrAPInt a)
{
    sint64 res = 0;
    pLsrAPIntTrace1("To sint64", a);
    if (sizeof(sint64) <= sizeof(long int) || mpz_fits_slong_p(pLsrAPIntGetZ(a))) {
        res = (sint64) mpz_get_si(pLsrAPIntGetZ(a));
    } else {
        int negate = 0;
        mpz_t tmp1;
        mpz_t tmp2;
        mpz_init_set(tmp1, pLsrAPIntGetZ(a));
        if (mpz_sgn(tmp1) == -1) {
            negate = 1;
            mpz_neg(tmp1, tmp1);
        }
        mpz_init_set(tmp2, tmp1);
        mpz_fdiv_q_2exp(tmp1, tmp1, 32);
        mpz_fdiv_r_2exp(tmp2, tmp2, 32);
        res = (sint64) mpz_get_ui(tmp1);
        res = res << 32;
        res += (sint64) mpz_get_ui(tmp2);
        if (negate) {
            res = -res;
        }
        mpz_clear(tmp1);
        mpz_clear(tmp2);
    }
    pLsrAPIntKeepLive1(a);
    pLsrAPIntTraceFmt1("To sint64 %lld\n", res);
    return res;
}

static sintp pLsrSIntpFromAPInt(PlsrAPInt a)
{
    pLsrAPIntTrace1("To sintp", a);
    if (sizeof(sintp) <= sizeof(sint32))
        return pLsrSInt32FromAPInt(a);
    else if (sizeof(sintp) <= sizeof(sint64))
        return pLsrSInt64FromAPInt(a);
    else {
        pLsrRuntimeError("SIntp too large");
        return 0;
    }
}

/* String Conversions */
static char* pLsrCStringFromAPInt(PlsrAPInt a)
{
    char* res = pLsrAllocC(mpz_sizeinbase (pLsrAPIntGetZ(a), 10) + 2);
    mpz_get_str(res, 10, pLsrAPIntGetZ(a));
    pLsrAPIntKeepLive1(a);
    pLsrAPIntTrace1("To cstring", a);
    return res;
}

static PlsrAPInt pLsrAPIntFromCString(char* s)
{
    PlsrAPInt res = pLsrAPIntNewFromCString(s);
    pLsrAPIntTrace1("From cstring", res);
    return res;
}

#define pLsrAPIntBinary(a, b, operator)                                 \
    do {                                                                \
        PlsrAPInt res = pLsrAPIntNew();                                 \
        operator(pLsrAPIntGetZ(res), pLsrAPIntGetZ(a), pLsrAPIntGetZ(b)); \
        pLsrAPIntKeepLive2(a, b);                                       \
        pLsrAPIntTrace3(#operator, a, b, res);                         \
        return res;                                                     \
    } while(0)

/* Casts */

static sint8 pLsrAPIntCastToSInt8(PlsrAPInt a)
{
    sint8 res = (sint8)(sint32) mpz_get_si(pLsrAPIntGetZ(a));
    pLsrAPIntTraceFmt1("Cast to sint8 %d", res);
    return res;
}

static sint16 pLsrAPIntCastToSInt16(PlsrAPInt a)
{
    sint16 res = (sint16)(sint32) mpz_get_si(pLsrAPIntGetZ(a));
    pLsrAPIntTraceFmt1("Cast to sint16 %d", res);
    return res;
}

static sint64 pLsrAPIntCastToSInt64(PlsrAPInt a)
{
    sint64 res = 0;
    pLsrAPIntTrace1("Cast to sint64", a);
    if (sizeof(sint64) <= sizeof(long int) || mpz_fits_slong_p(pLsrAPIntGetZ(a))) {
        res = (sint64) mpz_get_si(pLsrAPIntGetZ(a));
    } else {
        int negate = 0;
        mpz_t tmp1;
        mpz_t tmp2;
        mpz_init_set(tmp1, pLsrAPIntGetZ(a));
        if (mpz_sgn(tmp1) == -1) {
            negate = 1;
            mpz_neg(tmp1, tmp1);
        }
        mpz_init_set(tmp2, tmp1);
        mpz_fdiv_q_2exp(tmp1, tmp1, 32);
        mpz_fdiv_r_2exp(tmp2, tmp2, 32);
        res = (sint64) mpz_get_ui(tmp1);
        res = res << 32;
        res += (sint64) mpz_get_ui(tmp2);
        if (negate) {
            res = -res;
        }
        mpz_clear(tmp1);
        mpz_clear(tmp2);
    }
    pLsrAPIntKeepLive1(a);
    pLsrAPIntTraceFmt1("To sint64 %lld\n", res);
    return res;
}

static uint8 pLsrAPIntCastToUInt8(PlsrAPInt a)
{
    uint8 res = (uint8)(sint32) mpz_get_si(pLsrAPIntGetZ(a));
    pLsrAPIntTraceFmt1("Cast to uint8 %d", res);
    return res;
}

static uint16 pLsrAPIntCastToUInt16(PlsrAPInt a)
{
    uint16 res = (uint16)(sint32) mpz_get_si(pLsrAPIntGetZ(a));
    pLsrAPIntTraceFmt1("Cast to uint16 %d", res);
    return res;
}

static uint32 pLsrAPIntCastToUInt32(PlsrAPInt a)
{
    uint32 res = (uint32) mpz_get_ui(pLsrAPIntGetZ(a));
    if (mpz_sgn(pLsrAPIntGetZ(a)) == -1) {
        res = -res;
    }
    pLsrAPIntTraceFmt1("Cast to uint32 %d", res);
    return res;
}

static sint32 pLsrAPIntCastToSInt32(PlsrAPInt a)
{
    return (sint32)pLsrAPIntCastToUInt32(a);
}

static uint64 pLsrAPIntCastToUInt64(PlsrAPInt a)
{
    sint64 res = 0;
    pLsrAPIntTrace1("Cast to uint64", a);
    if (sizeof(sint64) <= sizeof(long int) || mpz_fits_slong_p(pLsrAPIntGetZ(a))) {
        res = (sint64) mpz_get_si(pLsrAPIntGetZ(a));
    } else {
        int negate = 0;
        mpz_t tmp1;
        mpz_t tmp2;
        mpz_init_set(tmp1, pLsrAPIntGetZ(a));
        if (mpz_sgn(tmp1) == -1) {
            negate = 1;
            mpz_neg(tmp1, tmp1);
        }
        mpz_init_set(tmp2, tmp1);
        mpz_fdiv_q_2exp(tmp1, tmp1, 32);
        mpz_fdiv_r_2exp(tmp2, tmp2, 32);
        res = (sint64) mpz_get_ui(tmp1);
        res = res << 32;
        res += (sint64) mpz_get_ui(tmp2);
        if (negate) {
            res = -res;
        }
        mpz_clear(tmp1);
        mpz_clear(tmp2);
    }
    pLsrAPIntKeepLive1(a);
    pLsrAPIntTraceFmt1("To uint64 %lld\n", res);
    return (uint64)res;
}

static float32 pLsrAPIntCastToFloat32(PlsrAPInt a)
{
    float32 res = (float32) mpz_get_d(pLsrAPIntGetZ(a));
    pLsrAPIntKeepLive1(a);
    pLsrAPIntTraceFmt1("Cast to float32 %f\n", res);
    return res;
}

static float64 pLsrAPIntCastToFloat64(PlsrAPInt a)
{
    float64 res = (float64) mpz_get_d(pLsrAPIntGetZ(a));
    pLsrAPIntKeepLive1(a);
    pLsrAPIntTraceFmt1("Cast to float64 %lf\n", res);
    return res;
}

/* Bitwise */

static PlsrAPInt pLsrAPIntBNot(PlsrAPInt a)
{
    PlsrAPInt res = pLsrAPIntNew();
    mpz_com(pLsrAPIntGetZ(res), pLsrAPIntGetZ(a));
    pLsrAPIntKeepLive1(a);
    pLsrAPIntTrace2("BNot", a, res);
    return res;
}

static PlsrAPInt pLsrAPIntBAnd(PlsrAPInt a, PlsrAPInt b)
{
    pLsrAPIntBinary(a, b, mpz_and);
}

static PlsrAPInt pLsrAPIntBOr(PlsrAPInt a, PlsrAPInt b)
{
    pLsrAPIntBinary(a, b, mpz_ior);
}

static PlsrAPInt pLsrAPIntShiftLUIntL(PlsrAPInt a, unsigned long b)
{
    PlsrAPInt res = pLsrAPIntNew();
    mpz_mul_2exp(pLsrAPIntGetZ(res), pLsrAPIntGetZ(a), b);
    pLsrAPIntKeepLive1(a);
    return res;
}

static PlsrAPInt pLsrAPIntShiftRUIntL(PlsrAPInt a, unsigned long b)
{
    PlsrAPInt res = pLsrAPIntNew();
    mpz_fdiv_q_2exp(pLsrAPIntGetZ(res), pLsrAPIntGetZ(a), b);
    pLsrAPIntKeepLive1(a);
    return res;
}

static PlsrAPInt pLsrAPIntBShiftL(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt res = 0;
    if (mpz_fits_slong_p(pLsrAPIntGetZ(b))) {
        long int count = mpz_get_si(pLsrAPIntGetZ(b));
        if (count < 0) {
            res = pLsrAPIntShiftRUIntL(a, -count);
        } else {
            res = pLsrAPIntShiftLUIntL(a, count);
        }
    } else {
        pLsrRuntimeError("GMP AP Int only supports shifts up to 2^32 bits");
    }
    return res;
}

static PlsrAPInt pLsrAPIntBShiftR(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt res = 0;
    if (mpz_fits_slong_p(pLsrAPIntGetZ(b))) {
        long int count = mpz_get_si(pLsrAPIntGetZ(b));
        if (count < 0) {
            res = pLsrAPIntShiftLUIntL(a, -count);
        } else {
            res = pLsrAPIntShiftRUIntL(a, count);
        }
    } else {
        pLsrRuntimeError("GMP AP Int only supports shifts up to 2^32 bits");
    }
    return res;
}

static PlsrAPInt pLsrAPIntBXor(PlsrAPInt a, PlsrAPInt b)
{
    pLsrAPIntBinary(a, b, mpz_xor);
}



/* Arithmetic */

static PlsrAPInt pLsrAPIntNegate(PlsrAPInt a)
{
    PlsrAPInt res = pLsrAPIntNew();
    mpz_neg(pLsrAPIntGetZ(res), pLsrAPIntGetZ(a));
    pLsrAPIntKeepLive1(a);
    pLsrAPIntTrace2("Negate", a, res);
    return res;
}

static PlsrAPInt pLsrAPIntAdd(PlsrAPInt a, PlsrAPInt b)
{
    pLsrAPIntBinary(a, b, mpz_add);
}

static PlsrAPInt pLsrAPIntSub(PlsrAPInt a, PlsrAPInt b)
{
    pLsrAPIntBinary(a, b, mpz_sub);
}

static PlsrAPInt pLsrAPIntMul(PlsrAPInt a, PlsrAPInt b)
{
    pLsrAPIntBinary(a, b, mpz_mul);
}


static void pLsrAPIntDivModE(PlsrAPInt* quotO, PlsrAPInt* remO, PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt q = pLsrAPIntNew();
    PlsrAPInt r = pLsrAPIntNew();
    mpz_tdiv_qr(pLsrAPIntGetZ(q), pLsrAPIntGetZ(r), pLsrAPIntGetZ(a), pLsrAPIntGetZ(b));
    if (mpz_sgn(pLsrAPIntGetZ(r)) < 0) {
        if (mpz_sgn(pLsrAPIntGetZ(b)) > 0) {
            mpz_sub_ui(pLsrAPIntGetZ(q), pLsrAPIntGetZ(q),1);
            mpz_add(pLsrAPIntGetZ(r), pLsrAPIntGetZ(r), pLsrAPIntGetZ(b));
        } else {
            mpz_add_ui(pLsrAPIntGetZ(q), pLsrAPIntGetZ(q), 1);
            mpz_sub(pLsrAPIntGetZ(r), pLsrAPIntGetZ(r), pLsrAPIntGetZ(b));
        }
    }
    *quotO = q;
    *remO = r;
    pLsrAPIntKeepLive2(a, b);
    pLsrAPIntTrace4("DivModE", a, b, q, r);
    return;
}

static void pLsrAPIntDivModF(PlsrAPInt* quotO, PlsrAPInt* remO, PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt q = pLsrAPIntNew();
    PlsrAPInt r = pLsrAPIntNew();
    mpz_fdiv_qr(pLsrAPIntGetZ(q), pLsrAPIntGetZ(r), pLsrAPIntGetZ(a), pLsrAPIntGetZ(b));
    *quotO = q;
    *remO = r;
    pLsrAPIntKeepLive2(a, b);
    pLsrAPIntTrace4("DivModF", a, b, q, r);
    return;
}

static void pLsrAPIntDivModT(PlsrAPInt* quotO, PlsrAPInt* remO, PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt q = pLsrAPIntNew();
    PlsrAPInt r = pLsrAPIntNew();
    mpz_tdiv_qr(pLsrAPIntGetZ(q), pLsrAPIntGetZ(r), pLsrAPIntGetZ(a), pLsrAPIntGetZ(b));
    *quotO = q;
    *remO = r;
    pLsrAPIntKeepLive2(a, b);
    pLsrAPIntTrace4("DivModT", a, b, q, r);
    return;
}

static PlsrAPInt pLsrAPIntDivE(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt q = pLsrAPIntNew();
    mpz_t r;
    mpz_init(r);
    mpz_tdiv_qr(pLsrAPIntGetZ(q), r, pLsrAPIntGetZ(a), pLsrAPIntGetZ(b));
    if (mpz_sgn(r) < 0) {
        if (mpz_sgn(pLsrAPIntGetZ(b)) > 0) {
            mpz_sub_ui(pLsrAPIntGetZ(q), pLsrAPIntGetZ(q), 1);
        } else {
            mpz_add_ui(pLsrAPIntGetZ(q), pLsrAPIntGetZ(q), 1);
        }
    }
    pLsrAPIntTrace4("DivE", a, b, q, r);
    mpz_clear(r);
    pLsrAPIntKeepLive2(a, b);
    pLsrAPIntTrace3("DivE", a, b, q);
    return q;

}

static PlsrAPInt pLsrAPIntDivF(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt q = pLsrAPIntNew();
    mpz_fdiv_q(pLsrAPIntGetZ(q), pLsrAPIntGetZ(a), pLsrAPIntGetZ(b));
    pLsrAPIntKeepLive2(a, b);
    pLsrAPIntTrace3("DivF", a, b, q);
    return q;
}

static PlsrAPInt pLsrAPIntDivT(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt q = pLsrAPIntNew();
    mpz_tdiv_q(pLsrAPIntGetZ(q), pLsrAPIntGetZ(a), pLsrAPIntGetZ(b));
    pLsrAPIntKeepLive2(a, b);
    pLsrAPIntTrace3("DivT", a, b, q);
    return q;
}

static PlsrAPInt pLsrAPIntModE(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt r = pLsrAPIntNew();
    mpz_tdiv_r(pLsrAPIntGetZ(r), pLsrAPIntGetZ(a), pLsrAPIntGetZ(b));
    if (mpz_sgn(pLsrAPIntGetZ(r)) < 0) {
        if (mpz_sgn(pLsrAPIntGetZ(b)) > 0) {
            mpz_add(pLsrAPIntGetZ(r), pLsrAPIntGetZ(r), pLsrAPIntGetZ(b));
        } else {
            mpz_sub(pLsrAPIntGetZ(r), pLsrAPIntGetZ(r), pLsrAPIntGetZ(b));
        }
    }
    pLsrAPIntKeepLive2(a, b);
    pLsrAPIntTrace3("ModE", a, b, r);
    return r;
}

static PlsrAPInt pLsrAPIntModF(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt r = pLsrAPIntNew();
    mpz_fdiv_r(pLsrAPIntGetZ(r), pLsrAPIntGetZ(a), pLsrAPIntGetZ(b));
    pLsrAPIntKeepLive2(a, b);
    pLsrAPIntTrace3("ModF", a, b, r);
    return r;
}

static PlsrAPInt pLsrAPIntModT(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt r = pLsrAPIntNew();
    mpz_tdiv_r(pLsrAPIntGetZ(r), pLsrAPIntGetZ(a), pLsrAPIntGetZ(b));
    pLsrAPIntKeepLive2(a, b);
    return r;
}

static PlsrAPInt pLsrAPIntGcd(PlsrAPInt a, PlsrAPInt b)
{
    PlsrAPInt res = pLsrAPIntNew();
    mpz_gcd(pLsrAPIntGetZ(res), pLsrAPIntGetZ(a), pLsrAPIntGetZ(b));
    pLsrAPIntKeepLive2(a, b);
    pLsrAPIntTrace3("Gcd", a, b, res);
    return res;
}


/* Comparisons */

static sintp pLsrAPIntCompare(PlsrAPInt a, PlsrAPInt b)
{
    sintp res = mpz_cmp(pLsrAPIntGetZ(a), pLsrAPIntGetZ(b));
    pLsrAPIntKeepLive2(a, b);
    pLsrAPIntTrace2("Compare", a, b);
    return res;
}


static PlsrBoolean pLsrAPIntLess(PlsrAPInt a, PlsrAPInt b) {
    return pLsrAPIntCompare(a, b) < 0;
}

static PlsrBoolean pLsrAPIntGreater(PlsrAPInt a, PlsrAPInt b) {
    return pLsrAPIntCompare(a, b) > 0;
}

static PlsrBoolean pLsrAPIntEqual(PlsrAPInt a, PlsrAPInt b) {
    return pLsrAPIntCompare(a, b) == 0;
}

static PlsrBoolean pLsrAPIntNotEqual(PlsrAPInt a, PlsrAPInt b) {
    return pLsrAPIntCompare(a, b) != 0;
}

static PlsrBoolean pLsrAPIntLessOrEqual(PlsrAPInt a, PlsrAPInt b) {
    return pLsrAPIntCompare(a, b) <= 0;
}

static PlsrBoolean pLsrAPIntGreaterOrEqual(PlsrAPInt a, PlsrAPInt b) {
    return pLsrAPIntCompare(a, b) >= 0;
}


/* Miscellaneous */

static uint32 pLsrAPIntFitsInSInt32(PlsrAPInt a)
{
    uint32 res = mpz_fits_slong_p(pLsrAPIntGetZ(a));

    if (sizeof(sint32) < sizeof(long int)) {
        /* If it fits in a long int and the long int is small enough,
         * it fits in an sint32 */
        if (res) {
            long int ai = mpz_get_si(pLsrAPIntGetZ(a));
            res = ai >=SINT32_MIN && ai <= SINT32_MAX;
        }
    } else if (sizeof(sint32) > sizeof(long int)) {
        if (!res) {
            res = (pLsrAPIntGreaterOrEqual(a, pLsrAPIntFromSInt32(SINT32_MIN)) &&
                   pLsrAPIntLessOrEqual(a, pLsrAPIntFromSInt32(SINT32_MAX)));
        }
    }
    /* If they're equal, we're good. */
    pLsrAPIntKeepLive1(a);
    return res;
}

/* Returns SINT32_MIN if (a >= upper) or (a <= lower)
 * Otherwise returns a.
 * To be useful, (lower > SINT32_MIN) should be true.
 */
static sint32 pLsrAPIntCheckRangeSInt32(PlsrAPInt a, sint32 upper, sint32 lower)
{
    uint32 fitsInLong = mpz_fits_slong_p(pLsrAPIntGetZ(a));
    sint32 res = SINT32_MIN;
    if (sizeof(sint32) < sizeof(long int)) {
        /* If it fits in a long int and the long int is small enough,
         * it fits in an sint32 */
        if (fitsInLong) {
            long int ai = mpz_get_si(pLsrAPIntGetZ(a));
            if (ai >= ((long int) lower) && ai <= ((long int) upper)) {
                res = (sint32) ai;
            }
        }
    } else if (sizeof(sint32) > sizeof(long int)) {
        if (fitsInLong) {
            sint32 ai = (sint32) mpz_get_si(pLsrAPIntGetZ(a));
            if (ai >= lower && ai <= upper) {
                res = ai;
            }
        } else if (pLsrAPIntGreaterOrEqual(a, pLsrAPIntFromSInt32(lower)) &&
                   pLsrAPIntLessOrEqual(a, pLsrAPIntFromSInt32(upper))) {
            res = pLsrSInt32FromAPInt(a);
        }
    } else if (fitsInLong) {
        sint32 ai = (sint32) mpz_get_si(pLsrAPIntGetZ(a));
        if (ai >= lower && ai <= upper) {
            res = ai;
        }
    }
    pLsrAPIntKeepLive1(a);
    return res;
}


#define pLsrAPIntFromFloat32 pLsrAPIntNewFromFloat32
#define pLsrAPIntFromFloat64 pLsrAPIntNewFromFloat64

static float32 pLsrFloat32FromAPInt(PlsrAPInt a) {
    float32 res = (float32) mpz_get_d(pLsrAPIntGetZ(a));
    pLsrAPIntKeepLive1(a);
    pLsrAPIntTrace1("To float32", a);
    return res;
}

static float64 pLsrFloat64FromAPInt(PlsrAPInt a) {
    float64 res = (float64) mpz_get_d(pLsrAPIntGetZ(a));
    pLsrAPIntKeepLive1(a);
    pLsrAPIntTrace1("To float64", a);
    return res;
}

#define hashPair(h1, h2) ((h1)+((h2)<<5)+(h2)+720)

static uintp pLsrAPIntMPZHash(mpz_t z)
{
    uintp h;
    switch mpz_sgn(z) {
    case -1: h = 987; break;
    case 1:  h = 0;   break;
    case 0:  h = 0;   break;
    }
    for(size_t l = mpz_size(z); l > 0; l--) {
        h = hashPair(h, mpz_getlimbn(z, l-1));
    }
    return h;
}
static uintp pLsrAPIntHash(PlsrAPInt i)
{
    uintp h = pLsrAPIntMPZHash(pLsrAPIntGetZ(i));
    pLsrAPIntKeepLive1(i);
    return h;
}

/* This function must match the previous one */
static uintp pLsrSInt32Hash(sint32 i)
{
    uintp h;
    mpz_t tmp;
    mpz_init_set_si(tmp, i);
    h = pLsrAPIntMPZHash(tmp);
    mpz_clear(tmp);
    return h;
}

/* Initialization and Registration */

static void pLsrAPIntRegisterVTables()
{
    static PgcIsRef pLsrAPIntRefs[pLsrAPIntSize/P_WORD_SIZE] = { 0, };

    pLsrVTableRegisterFinalizable(pLsrAPIntVTable, pLsrAPIntAlignment, pLsrAPIntSize, pLsrAPIntRefs, 0, 0, 0,
                                  PGC_ALWAYS_MUTABLE, pLsrAPIntIsPinned, pLsrAPIntFinalize);

}

#define pLsrAPIntGlobalsCount 4

static PlsrObjectB pLsrAPIntGlobals[] =
    {
        (PlsrObjectB) &pLsrAPIntZero_,
        (PlsrObjectB) &pLsrAPIntOne_,
        (PlsrObjectB) &pLsrAPIntMinusOne_,
        (PlsrObjectB) &pLsrAPIntMinusUInt32Max_,
        (PlsrObjectB) NULL /* This must be last */
    };

static void pLsrAPIntRegisterGlobals() {
    assert(pLsrAPIntGlobals[pLsrAPIntGlobalsCount] == NULL);
    pLsrGcRegisterGlobals (pLsrAPIntGlobals, pLsrAPIntGlobalsCount);
};

#ifdef PLSR_GMP_USE_GCMALLOC
#ifdef P_USE_PILLAR
#  pragma pillar_managed(off)
#  define to __to__

static void* pLsrGMPMalloc(size_t size)
{
    return pLsrGCHeapMalloc((uintp) size);
}
static void pLsrGMPFree(void* object, size_t size)
{
    pLsrGCHeapFree(object);
}
static void* pLsrGMPReAlloc(void* object, size_t osize, size_t nsize)
{
    return pLsrGCHeapReAlloc(object, (uintp) osize, (uintp) nsize);
}

#  undef to
#pragma pillar_managed(on)
#endif
#else
#ifdef PLSR_GMP_FORCE_GC

#ifdef P_USE_PILLAR
#  pragma pillar_managed(off)
#  define to __to__

static void* pLsrGMPMalloc(size_t size)
{
    pLsrGmpAllocated += size;
    return malloc((uintp) size);
}
static void pLsrGMPFree(void* object, size_t size)
{
    pLsrGmpAllocated -= size;
    free(object);
}
static void* pLsrGMPReAlloc(void* object, size_t osize, size_t nsize)
{
    if (osize > nsize) {
        pLsrGmpAllocated -= (osize-nsize);
    } else {
        pLsrGmpAllocated += (nsize-osize);
    }
    return realloc(object, (uintp) nsize);
}

#  undef to
#pragma pillar_managed(on)
#endif

#endif /* PLSR_GMP_FORCE_GC */
#endif /* PLSR_GMP_USE_GCMALLOC*/

static void pLsrAPIntInitialize(uintp memLimit) {
#ifdef PLSR_GMP_FORCE_GC
    if (memLimit != 0) pLsrGmpMemLimit = memLimit;
#endif
#ifdef PLSR_GMP_REPLACE_MALLOC
    mp_set_memory_functions(pLsrGMPMalloc, pLsrGMPReAlloc, pLsrGMPFree);
#endif
    pLsrAPIntStaticGlobalInitFromSInt32(pLsrAPIntZero, 0);
    pLsrAPIntStaticGlobalInitFromSInt32(pLsrAPIntOne, 1);
    pLsrAPIntStaticGlobalInitFromSInt32(pLsrAPIntMinusOne, -1);
    pLsrAPIntStaticGlobalInitFromUInt32(pLsrAPIntMinusUInt32Max, UINT32_MAX);
    mpz_neg(pLsrAPIntGetZ(pLsrAPIntMinusUInt32Max), pLsrAPIntGetZ(pLsrAPIntMinusUInt32Max));
};

#endif /*_PLSR_AP_INTEGER_H_ */
