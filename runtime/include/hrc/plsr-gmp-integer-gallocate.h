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

#ifndef _PLSR_AP_INTEGER_H_
#define _PLSR_AP_INTEGER_H_

#define PLSR_GMP_REPLACE_MALLOC
#define PLSR_GMP_USE_PCDECL

#ifndef __pillar2c__
#error "GMP integer guaranteed allocation only supported on ipc"

#endif /* __pillar2c__ */

#  pragma pillar_push_cc(__pcdecl)
#include <gmp.h>
#  pragma pillar_pop_cc

/* Types */

typedef struct PlsrAPIntS_ {
    PlsrVTable vtable;
    int alloc;
    int size;
    mp_limb_t d[];
} PlsrAPIntS;

#define pLsrAPIntPadding (sizeof(PlsrAPIntS) - sizeof(PlsrVTable) - sizeof(int) - sizeof(int))
pLsrVTableStatic(pLsrAPIntVTable_, "*ap integer*", pLsrAPIntPadding);
#define pLsrAPIntVTable (&pLsrAPIntVTable_)

#ifdef P_USE_PILLAR
typedef PlsrRef PlsrAPInt;
#else /* !P_USE_PILLAR */
typedef PlsrAPIntS* PlsrAPInt;
#endif /* !P_USE_PILLAR */

#define pLsrAPIntAlignment 4
#define pLsrAPIntSize (sizeof(PlsrAPIntS))

#define pLsrMPZ_mp_alloc(i) ((i)[0]._mp_alloc)
#define pLsrMPZ_mp_size(i) ((i)[0]._mp_size)
#define pLsrMPZ_mp_d(i) ((i)[0]._mp_d)

#define pLsrAPInt_mp_alloc(i) (((PlsrAPIntS*) (i))->alloc)
#define pLsrAPInt_mp_size(i) (((PlsrAPIntS*) (i))->size)
#define pLsrAPInt_mp_d(i) (&(((PlsrAPIntS*) (i))->d[0]))

#define pLsrAPInt_mp_d_Offset ((uintp)(pLsrAPIntSize))
#define pLsrAPIntMPZMemPtrToAPInt(mem) (((char*) mem) - (pLsrAPInt_mp_d_Offset))

#define pLsrAPIntToMPZ(dest, src)                                       \
    do {                                                                \
        pLsrMPZ_mp_alloc(dest) = pLsrAPInt_mp_alloc(src);               \
        pLsrMPZ_mp_size(dest) = pLsrAPInt_mp_size(src);                 \
        pLsrMPZ_mp_d(dest) = pLsrAPInt_mp_d(src);                       \
    } while (0)

#define pLsrAPIntFromMPZ(dest, src)                                     \
    do {                                                                \
        (dest) = pLsrAPIntMPZMemPtrToAPInt(pLsrMPZ_mp_d(src));          \
        assert(pLsrAPInt_mp_alloc(dest) == pLsrMPZ_mp_alloc(src));      \
        pLsrAPInt_mp_size(dest) = pLsrMPZ_mp_size(src);                 \
    } while (0)

#define pLsrAPIntInitMPZFromAPInt(src)                                  \
    {{pLsrAPInt_mp_alloc(src), pLsrAPInt_mp_size(src), pLsrAPInt_mp_d(src)}};


static uintp pLsrGmpAllocationReserve = 300;

// Ask if the GC can definitely allocate size number of bytes without needing a collection.
EXTERN(PgcBool) __pcdecl pgc_can_allocate_without_collection_th(unsigned size, PrtTaskHandle task);
// Do whatever is necessary to make sure that size bytes can be allocated after this call without a collection.
// This call itself may or may not cause a collection.
EXTERN(void) PRT_CDECL   pgc_require_allocate_without_collection_th(unsigned size, PrtTaskHandle task);
/* Prepare to allocate up to count gmp integers */
#define pLsrGmpPrepareForAllocation(count)                              \
    do {                                                                \
        if (!pgc_can_allocate_without_collection_th(count*pLsrGmpAllocationReserve,prtGetTaskHandle())) { \
            pgc_require_allocate_without_collection_th(count*pLsrGmpAllocationReserve,prtGetTaskHandle()); \
        }                                                               \
    } while(0)

/* For documentation purposes only*/
#define pLsrAPIntBeginNoGC

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
static void pLsrAPIntShow1(char * s, PlsrAPInt i1)
{
    mpz_t t1 = pLsrAPIntInitMPZFromAPInt(i1);
    char* res = pLsrAllocC(mpz_sizeinbase (t1, 10) + 2);
    mpz_get_str(res, 10, t1);
    printf("APInt: %s, %s\n", s, res);
    pLsrFreeC(res);
}

static void pLsrAPIntShow3(char * s, PlsrAPInt i1, PlsrAPInt i2, PlsrAPInt i3)
{
    mpz_t t1 = pLsrAPIntInitMPZFromAPInt(i1);
    mpz_t t2 = pLsrAPIntInitMPZFromAPInt(i2);
    mpz_t t3 = pLsrAPIntInitMPZFromAPInt(i3);
    char* res1 = pLsrAllocC(mpz_sizeinbase (t1, 10) + 2);
    mpz_get_str(res1, 10, t1);
    char* res2 = pLsrAllocC(mpz_sizeinbase (t2, 10) + 2);
    mpz_get_str(res2, 10, t2);
    char* res3 = pLsrAllocC(mpz_sizeinbase (t3, 10) + 2);
    mpz_get_str(res3, 10, t3);
    printf("APInt: %s, (%s, %s, %s)\n", s, res1, res2, res3);
    pLsrFreeC(res1);
    pLsrFreeC(res2);
    pLsrFreeC(res3);
}
#define pLsrAPIntTrace0(s)
#define pLsrAPIntTrace1(s, i1)  pLsrAPIntShow1(s, i1)
#define pLsrAPIntTrace2(s, i1, i2)
#define pLsrAPIntTrace3(s, i1, i2, i3) pLsrAPIntShow3(s, i1, i2, i3)
#define pLsrAPIntTrace4(s, i1, i2, i3, i4)
#define pLsrAPIntTraceFmt1(s, i1)
#else
#define pLsrAPIntTrace0(s)
#define pLsrAPIntTrace1(s, i1)
#define pLsrAPIntTrace2(s, i1, i2)
#define pLsrAPIntTrace3(s, i1, i2, i3)
#define pLsrAPIntTrace4(s, i1, i2, i3, i4)
#define pLsrAPIntTraceFmt1(s, i1)
#endif

static PlsrAPInt pLsrAPIntNewFromCString(char *s)
{
    pLsrGmpPrepareForAllocation(1);
    pLsrAPIntBeginNoGC {
        mpz_t t1;
        if (mpz_init_set_str(t1, s, 0) != 0) {
            pLsrRuntimeError("Failed to initialize ap integer");
        }
        PlsrAPInt res; pLsrAPIntFromMPZ(res, t1);
        return res;
    }
}

/* operation must be an init_set_* */
#define pLsrAPIntNewFromXBody(operation, src)           \
    do {                                                \
        mpz_t t1;                                       \
        pLsrGmpPrepareForAllocation(1);                 \
        pLsrAPIntBeginNoGC {                            \
            operation(t1, src);                         \
            PlsrAPInt res; pLsrAPIntFromMPZ(res, t1);    \
            return res;                                 \
        }                                               \
    } while (0)

static PlsrAPInt pLsrAPIntNewFromSignedLong(long int si)
{
    pLsrAPIntNewFromXBody(mpz_init_set_si, si);
}

static PlsrAPInt pLsrAPIntNewFromUnsignedLong(unsigned long int ui)
{
    pLsrAPIntNewFromXBody(mpz_init_set_ui, ui);
}

static PlsrAPInt pLsrAPIntNewFromFloat32(float32 f)
{
    pLsrAPIntNewFromXBody(mpz_init_set_d, (float64) f);
}

static PlsrAPInt pLsrAPIntNewFromFloat64(float64 f)
{
    pLsrAPIntNewFromXBody(mpz_init_set_d, f);
}


static PlsrAPInt pLsrAPIntNewFromAPInt(PlsrAPInt a)
{
    pLsrGmpPrepareForAllocation(1);
    pLsrAPIntBeginNoGC {
        mpz_t t1;
        mpz_t t2 = pLsrAPIntInitMPZFromAPInt(a);
        mpz_init_set(t1, t2);
        PlsrAPInt res; pLsrAPIntFromMPZ(res, t1);
        return res;
    }
}

/* Constants */

/* Globals */

#define PLSR_AP_INT_BACKPATCH_GLOBALS 1

/* This must match the definition of PlsrAPIntS */
#define PlsrAPIntSGlobal(bytes)                 \
    struct {                                    \
        PlsrVTable vtable;                      \
        int alloc;                              \
        int size;                               \
        mp_limb_t d[bytes];                     \
    }

#define pLsrAPIntStaticGlobalNew(dv, bytes)                             \
    static PlsrAPIntSGlobal(bytes) dv = {.vtable = pLsrAPIntVTable,        \
                                         .alloc = (bytes + (sizeof(mp_limb_t) - 1))/sizeof(mp_limb_t), \
                                         .size = 0,                     \
    }

void pLsrAPIntStaticGlobalInitFromAPInt(PlsrAPInt dest, PlsrAPInt src)
{
    int size = pLsrAbs32(pLsrAPInt_mp_size(src));
    assert(pLsrAPInt_mp_alloc(dest) >= size);
    pLsrAPInt_mp_size(dest) = pLsrAPInt_mp_size(src);
    memcpy(pLsrAPInt_mp_d(dest), pLsrAPInt_mp_d(src), size * sizeof(mp_limb_t));
}

/* These are global, and will never be freed, */
#define pLsrAPIntStaticGlobalInitFromCString(dest, s) (pLsrAPIntStaticGlobalInitFromAPInt(dest, pLsrAPIntNewFromCString(s)))

#define pLsrAPIntStaticGlobalInitFromSInt32(dest, si) (pLsrAPIntStaticGlobalInitFromAPInt(dest, pLsrAPIntNewFromSignedLong(si)))

#define pLsrAPIntStaticGlobalInitFromUInt32(dest, ui) (pLsrAPIntStaticGlobalInitFromAPInt(dest, pLsrAPIntNewFromUnsignedLong(ui)))

pLsrAPIntStaticGlobalNew(pLsrAPIntZero_, 0);
#define pLsrAPIntZero ((PlsrAPInt) &pLsrAPIntZero_)
pLsrAPIntStaticGlobalNew(pLsrAPIntOne_, 1);
#define pLsrAPIntOne ((PlsrAPInt) &pLsrAPIntOne_)
pLsrAPIntStaticGlobalNew(pLsrAPIntMinusOne_, 1);
#define pLsrAPIntMinusOne ((PlsrAPInt) &pLsrAPIntMinusOne_)
pLsrAPIntStaticGlobalNew(pLsrAPIntMinusUInt32Max_, sizeof(uint32));
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
        pLsrGmpPrepareForAllocation(2);
        pLsrAPIntBeginNoGC {
            mpz_t t1 = pLsrAPIntInitMPZFromAPInt(z);
            mpz_mul_2exp(t1, t1, 32);
            mpz_add_ui(t1, t1, lower);
            pLsrAPIntFromMPZ(res, t1);
        }
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
    mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
    uint32 res = (uint32) mpz_get_ui(t1);
    pLsrAPIntTrace1("To uint32", a);
    return res;
}

static uint64 pLsrUInt64FromAPInt(PlsrAPInt a)
{
    uint64 res = 0;
    pLsrGmpPrepareForAllocation(4);
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        if (sizeof(uint64) <= sizeof(unsigned long int) || mpz_fits_ulong_p(t1)) {
            res = (uint64) mpz_get_ui(t1);
        } else {
            mpz_t tmp1;
            mpz_init_set(tmp1, t1);
            mpz_fdiv_q_2exp(tmp1, tmp1, 32);
            res = (uint64) mpz_get_ui(tmp1);
            res = res << 32;
            mpz_set(tmp1, t1);
            mpz_fdiv_r_2exp(tmp1, tmp1, 32);
            res += (uint64) mpz_get_ui(tmp1);
        }
    }
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
        pLsrGmpPrepareForAllocation(2);
        pLsrAPIntBeginNoGC {
            mpz_t t1 = pLsrAPIntInitMPZFromAPInt(z);
            mpz_mul_2exp(t1, t1, 32);
            mpz_add_ui(t1, t1, lower);
            if (i < 0) {
                mpz_neg(t1, t1);
            }
            pLsrAPIntFromMPZ(res, t1);
        }
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
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        sint32 res = (sint32) mpz_get_si(t1);
        pLsrAPIntTraceFmt1("To sint32 %d", res);
        return res;
    }
}

static sint64 pLsrSInt64FromAPInt(PlsrAPInt a)
{
    sint64 res = 0;
    pLsrAPIntTrace1("To sint64", a);
    pLsrGmpPrepareForAllocation(3);
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        if (sizeof(sint64) <= sizeof(long int) || mpz_fits_slong_p(t1)) {
            res = (sint64) mpz_get_si(t1);
        } else {
            int negate = 0;
            mpz_t tmp1;
            mpz_init_set(tmp1, t1);
            if (mpz_sgn(tmp1) == -1) {
                negate = 1;
                mpz_neg(tmp1, tmp1);
            }
            mpz_fdiv_q_2exp(tmp1, tmp1, 32);
            res = (sint64) mpz_get_ui(tmp1);
            res = res << 32;
            if (negate) {
                mpz_neg(tmp1, t1);
            } else {
                mpz_set(tmp1, t1);
            }
            mpz_fdiv_r_2exp(tmp1, tmp1, 32);
            res += (sint64) mpz_get_ui(tmp1);
            if (negate) {
                res = -res;
            }
        }
    }
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
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        char* res = pLsrAllocC(mpz_sizeinbase (t1, 10) + 2);
        mpz_get_str(res, 10, t1);
        pLsrAPIntTrace1("To cstring", a);
        return res;
    }
}

static PlsrAPInt pLsrAPIntFromCString(char* s)
{
    PlsrAPInt res = pLsrAPIntNewFromCString(s);
    pLsrAPIntTrace1("From cstring", res);
    return res;
}

#define pLsrAPIntBinary(a, b, operator)                                 \
    do {                                                                \
        pLsrGmpPrepareForAllocation(1);                                 \
        pLsrAPIntBeginNoGC {                                            \
            mpz_t t1;                                                   \
            mpz_init(t1);                                               \
            mpz_t t2 = pLsrAPIntInitMPZFromAPInt(a);                    \
            mpz_t t3 = pLsrAPIntInitMPZFromAPInt(b);                    \
            operator(t1, t2, t3);                                       \
            PlsrAPInt res; pLsrAPIntFromMPZ(res, t1);                   \
            pLsrAPIntTrace3(#operator, a, b, res);                      \
            return res;                                                 \
        }                                                               \
    } while(0)

/* Casts */

static sint8 pLsrAPIntCastToSInt8(PlsrAPInt a)
{
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        sint8 res = (sint8)(sint32) mpz_get_si(t1);
        pLsrAPIntTraceFmt1("Cast to sint8 %d", res);
        return res;
    }
}

static sint16 pLsrAPIntCastToSInt16(PlsrAPInt a)
{
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        sint16 res = (sint16)(sint32) mpz_get_si(t1);
        pLsrAPIntTraceFmt1("Cast to sint16 %d", res);
        return res;
    }
}

static sint64 pLsrAPIntCastToSInt64(PlsrAPInt a)
{
    sint64 res = 0;
    pLsrAPIntTrace1("Cast to sint64", a);
    pLsrGmpPrepareForAllocation(3);
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        if (sizeof(sint64) <= sizeof(long int) || mpz_fits_slong_p(t1)) {
            res = (sint64) mpz_get_si(t1);
        } else {
            int negate = 0;
            mpz_t tmp1;
            mpz_init_set(tmp1, t1);
            if (mpz_sgn(tmp1) == -1) {
                negate = 1;
                mpz_neg(tmp1, tmp1);
            }
            mpz_fdiv_q_2exp(tmp1, tmp1, 32);
            res = (sint64) mpz_get_ui(tmp1);
            res = res << 32;
            if (negate) {
                mpz_neg(tmp1, t1);
            } else {
                mpz_set(tmp1, t1);
            }
            mpz_fdiv_r_2exp(tmp1, tmp1, 32);
            res += (sint64) mpz_get_ui(tmp1);
            if (negate) {
                res = -res;
            }
        }
    }
    pLsrAPIntTraceFmt1("To sint64 %lld\n", res);
    return res;
}

static uint8 pLsrAPIntCastToUInt8(PlsrAPInt a)
{
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        uint8 res = (uint8)(sint32) mpz_get_si(t1);
        pLsrAPIntTraceFmt1("Cast to uint8 %d", res);
        return res;
    }
}

static uint16 pLsrAPIntCastToUInt16(PlsrAPInt a)
{
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        uint16 res = (uint16)(sint32) mpz_get_si(t1);
        pLsrAPIntTraceFmt1("Cast to uint16 %d", res);
        return res;
    }
}

static uint32 pLsrAPIntCastToUInt32(PlsrAPInt a)
{
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        uint32 res = (uint32) mpz_get_ui(t1);
        if (mpz_sgn(t1) == -1) {
            res = -res;
        }
        pLsrAPIntTraceFmt1("Cast to uint32 %d", res);
        return res;
    }
}

static sint32 pLsrAPIntCastToSInt32(PlsrAPInt a)
{
    return (sint32)pLsrAPIntCastToUInt32(a);
}

static uint64 pLsrAPIntCastToUInt64(PlsrAPInt a)
{
    sint64 res = 0;
    pLsrAPIntTrace1("Cast to uint64", a);
    pLsrGmpPrepareForAllocation(3);
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        if (sizeof(sint64) <= sizeof(long int) || mpz_fits_slong_p(t1)) {
            res = (sint64) mpz_get_si(t1);
        } else {
            int negate = 0;
            mpz_t tmp1;
            mpz_init_set(tmp1, t1);
            if (mpz_sgn(tmp1) == -1) {
                negate = 1;
                mpz_neg(tmp1, tmp1);
            }
            mpz_fdiv_q_2exp(tmp1, tmp1, 32);
            res = (sint64) mpz_get_ui(tmp1);
            res = res << 32;
            if (negate) {
                mpz_neg(tmp1, t1);
            } else {
                mpz_set(tmp1, t1);
            }
            mpz_fdiv_r_2exp(tmp1, tmp1, 32);
            res += (sint64) mpz_get_ui(tmp1);
            if (negate) {
                res = -res;
            }
        }
    }
    pLsrAPIntTraceFmt1("To uint64 %lld\n", res);
    return (uint64)res;
}

static float32 pLsrAPIntCastToFloat32(PlsrAPInt a)
{
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        float32 res = (float32) mpz_get_d(t1);
        pLsrAPIntTraceFmt1("Cast to float32 %f\n", res);
        return res;
    }
}

static float64 pLsrAPIntCastToFloat64(PlsrAPInt a)
{
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        float64 res = (float64) mpz_get_d(t1);
        pLsrAPIntTraceFmt1("Cast to float64 %lf\n", res);
        return res;
    }
}

/* Bitwise */

static PlsrAPInt pLsrAPIntBNot(PlsrAPInt a)
{
    pLsrGmpPrepareForAllocation(2);
    pLsrAPIntBeginNoGC {
        mpz_t t1;
        mpz_init(t1);
        mpz_t t2 = pLsrAPIntInitMPZFromAPInt(a);
        mpz_com(t1, t2);
        PlsrAPInt res; pLsrAPIntFromMPZ(res, t1);
        pLsrAPIntTrace2("BNot", a, res);
        return res;
    }
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
    pLsrGmpPrepareForAllocation(1);
    pLsrAPIntBeginNoGC {
        mpz_t t1;
        mpz_init(t1);
        mpz_t t2 = pLsrAPIntInitMPZFromAPInt(a);
        mpz_mul_2exp(t1, t2, b);
        PlsrAPInt res; pLsrAPIntFromMPZ(res, t1);
        return res;
    }
}

static PlsrAPInt pLsrAPIntShiftRUIntL(PlsrAPInt a, unsigned long b)
{
    pLsrGmpPrepareForAllocation(2);
    pLsrAPIntBeginNoGC {
        mpz_t t1;
        mpz_init(t1);
        mpz_t t2 = pLsrAPIntInitMPZFromAPInt(a);
        mpz_fdiv_q_2exp(t1, t2, b);
        PlsrAPInt res; pLsrAPIntFromMPZ(res, t1);
        return res;
    }
}

static PlsrAPInt pLsrAPIntBShiftL(PlsrAPInt a, PlsrAPInt b)
{
    long int count;
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(b);
        if (mpz_fits_slong_p(t1)) {
            count = mpz_get_si(t1);
        } else {
            pLsrRuntimeError("GMP AP Int only supports shifts up to 2^32 bits");
            count = 0;
        }
    }
    PlsrAPInt res = 0;
    if (count < 0) {
        res = pLsrAPIntShiftRUIntL(a, -count);
    } else {
        res = pLsrAPIntShiftLUIntL(a, count);
    }
    return res;
}

static PlsrAPInt pLsrAPIntBShiftR(PlsrAPInt a, PlsrAPInt b)
{
    long int count;
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(b);
        if (mpz_fits_slong_p(t1)) {
            count = mpz_get_si(t1);
        } else {
            pLsrRuntimeError("GMP AP Int only supports shifts up to 2^32 bits");
            count = 0;
        }
    }
    PlsrAPInt res = 0;
    if (count < 0) {
        res = pLsrAPIntShiftLUIntL(a, -count);
    } else {
        res = pLsrAPIntShiftRUIntL(a, count);
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
    pLsrGmpPrepareForAllocation(2);
    pLsrAPIntBeginNoGC {
        mpz_t t1;
        mpz_init(t1);
        mpz_t t2 = pLsrAPIntInitMPZFromAPInt(a);
        mpz_neg(t1, t2);
        PlsrAPInt res; pLsrAPIntFromMPZ(res, t1);
        pLsrAPIntTrace2("Negate", a, res);
        return res;
    }
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
    pLsrGmpPrepareForAllocation(4);
    pLsrAPIntBeginNoGC {
        mpz_t tq, tr;
        mpz_init(tq);
        mpz_init(tr);
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        mpz_t t2 = pLsrAPIntInitMPZFromAPInt(b);
        mpz_tdiv_qr(tq, tr, t1, t2);
        if (mpz_sgn(tr) < 0) {
            if (mpz_sgn(t2) > 0) {
                mpz_sub_ui(tq, tq,1);
                mpz_add(tr, tr, t2);
            } else {
                mpz_add_ui(tq, tq, 1);
                mpz_sub(tr, tr, t2);
            }
        }
        pLsrAPIntFromMPZ(*quotO, tq);
        pLsrAPIntFromMPZ(*remO, tr);
        pLsrAPIntTrace4("DivModE", a, b, q, r);
        return;
    }
}

static void pLsrAPIntDivModF(PlsrAPInt* quotO, PlsrAPInt* remO, PlsrAPInt a, PlsrAPInt b)
{
    pLsrGmpPrepareForAllocation(4);
    pLsrAPIntBeginNoGC {
        mpz_t tq, tr;
        mpz_init(tq);
        mpz_init(tr);
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        mpz_t t2 = pLsrAPIntInitMPZFromAPInt(b);
        mpz_fdiv_qr(tq, tr, t1, t2);
        pLsrAPIntFromMPZ(*quotO, tq);
        pLsrAPIntFromMPZ(*remO, tr);
        pLsrAPIntTrace4("DivModF", a, b, q, r);
        return;
    }
}

static void pLsrAPIntDivModT(PlsrAPInt* quotO, PlsrAPInt* remO, PlsrAPInt a, PlsrAPInt b)
{
    pLsrGmpPrepareForAllocation(4);
    pLsrAPIntBeginNoGC {
        mpz_t tq, tr;
        mpz_init(tq);
        mpz_init(tr);
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        mpz_t t2 = pLsrAPIntInitMPZFromAPInt(b);
        mpz_tdiv_qr(tq, tr, t1, t2);
        pLsrAPIntFromMPZ(*quotO, tq);
        pLsrAPIntFromMPZ(*remO, tr);
        pLsrAPIntTrace4("DivModT", a, b, q, r);
        return;
    }
}

static PlsrAPInt pLsrAPIntDivE(PlsrAPInt a, PlsrAPInt b)
{
    pLsrGmpPrepareForAllocation(4);
    pLsrAPIntBeginNoGC {
        mpz_t tq, tr;
        mpz_init(tq);
        mpz_init(tr);
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        mpz_t t2 = pLsrAPIntInitMPZFromAPInt(b);
        mpz_tdiv_qr(tq, tr, t1, t2);
        if (mpz_sgn(tr) < 0) {
            if (mpz_sgn(t2) > 0) {
                mpz_sub_ui(tq, tq, 1);
            } else {
                mpz_add_ui(tq, tq, 1);
            }
        }
        pLsrAPIntTrace4("DivE", a, b, q, r);
        PlsrAPInt res; pLsrAPIntFromMPZ(res, tq);
        return res;
    }
}

static PlsrAPInt pLsrAPIntDivF(PlsrAPInt a, PlsrAPInt b)
{
    pLsrGmpPrepareForAllocation(3);
    pLsrAPIntBeginNoGC {
        mpz_t tq;
        mpz_init(tq);
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        mpz_t t2 = pLsrAPIntInitMPZFromAPInt(b);
        mpz_fdiv_q(tq, t1, t2);
        PlsrAPInt res; pLsrAPIntFromMPZ(res, tq);
        pLsrAPIntTrace3("DivF", a, b, res);
        return res;
    };
}

static PlsrAPInt pLsrAPIntDivT(PlsrAPInt a, PlsrAPInt b)
{
    pLsrGmpPrepareForAllocation(3);
    pLsrAPIntBeginNoGC {
        mpz_t tq;
        mpz_init(tq);
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        mpz_t t2 = pLsrAPIntInitMPZFromAPInt(b);
        mpz_tdiv_q(tq, t1, t2);
        PlsrAPInt res; pLsrAPIntFromMPZ(res, tq);
        pLsrAPIntTrace3("DivT", a, b, res);
        return res;
    }
}

static PlsrAPInt pLsrAPIntModE(PlsrAPInt a, PlsrAPInt b)
{
    pLsrGmpPrepareForAllocation(3);
    pLsrAPIntBeginNoGC {
        mpz_t tr;
        mpz_init(tr);
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        mpz_t t2 = pLsrAPIntInitMPZFromAPInt(b);
        mpz_tdiv_r(tr, t1, t2);
        if (mpz_sgn(tr) < 0) {
            if (mpz_sgn(t2) > 0) {
                mpz_add(tr, tr, t2);
            } else {
                mpz_sub(tr, tr, t2);
            }
        }
        PlsrAPInt res; pLsrAPIntFromMPZ(res, tr);
        pLsrAPIntTrace3("ModE", a, b, res);
        return res;
    }
}

static PlsrAPInt pLsrAPIntModF(PlsrAPInt a, PlsrAPInt b)
{
    pLsrGmpPrepareForAllocation(2);
    pLsrAPIntBeginNoGC {
        mpz_t tr;
        mpz_init(tr);
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        mpz_t t2 = pLsrAPIntInitMPZFromAPInt(b);
        mpz_fdiv_r(tr, t1, t2);
        PlsrAPInt res; pLsrAPIntFromMPZ(res, tr);
        pLsrAPIntTrace3("ModF", a, b, res);
        return res;
    }
}

static PlsrAPInt pLsrAPIntModT(PlsrAPInt a, PlsrAPInt b)
{
    pLsrGmpPrepareForAllocation(2);
    pLsrAPIntBeginNoGC {
        mpz_t tr;
        mpz_init(tr);
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        mpz_t t2 = pLsrAPIntInitMPZFromAPInt(b);
        mpz_tdiv_r(tr, t1, t2);
        PlsrAPInt res; pLsrAPIntFromMPZ(res, tr);
        return res;
    }
}

static PlsrAPInt pLsrAPIntGcd(PlsrAPInt a, PlsrAPInt b)
{
    pLsrGmpPrepareForAllocation(2);
    pLsrAPIntBeginNoGC {
        mpz_t tres;
        mpz_init(tres);
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        mpz_t t2 = pLsrAPIntInitMPZFromAPInt(b);
        mpz_gcd(tres, t1, t2);
        PlsrAPInt res; pLsrAPIntFromMPZ(res, tres);
        pLsrAPIntTrace3("Gcd", a, b, res);
        return res;
    }
}


/* Comparisons */
/* calls below assume that this does not incur a gc */
static sintp pLsrAPIntCompare(PlsrAPInt a, PlsrAPInt b)
{
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        mpz_t t2 = pLsrAPIntInitMPZFromAPInt(b);
        sintp res = mpz_cmp(t1, t2);
        pLsrAPIntTrace2("Compare", a, b);
        return res;
    }
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
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        uint32 res = mpz_fits_slong_p(t1);
        if (sizeof(sint32) < sizeof(long int)) {
            /* If it fits in a long int and the long int is small enough,
             * it fits in an sint32 */
            if (res) {
                long int ai = mpz_get_si(t1);
                res = ai >=SINT32_MIN && ai <= SINT32_MAX;
            }
        } else if (sizeof(sint32) > sizeof(long int)) {
            if (!res) { /* gc ok */
                res = (pLsrAPIntGreaterOrEqual(a, pLsrAPIntFromSInt32(SINT32_MIN)) &&
                       pLsrAPIntLessOrEqual(a, pLsrAPIntFromSInt32(SINT32_MAX)));
            }
        }
        /* If they're equal, we're good. */
        return res;
    }
}

/* Returns SINT32_MIN if (a >= upper) or (a <= lower)
 * Otherwise returns a.
 * To be useful, (lower > SINT32_MIN) should be true.
 */
static sint32 pLsrAPIntCheckRangeSInt32(PlsrAPInt a, sint32 upper, sint32 lower)
{
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        uint32 fitsInLong = mpz_fits_slong_p(t1);
        sint32 res = SINT32_MIN;
        if (sizeof(sint32) < sizeof(long int)) {
            /* If it fits in a long int and the long int is small enough,
             * it fits in an sint32 */
            if (fitsInLong) {
                long int ai = mpz_get_si(t1);
                if (ai >= ((long int) lower) && ai <= ((long int) upper)) {
                    res = (sint32) ai;
                }
            }
        } else if (sizeof(sint32) > sizeof(long int)) {
            if (fitsInLong) {
                sint32 ai = (sint32) mpz_get_si(t1);
                if (ai >= lower && ai <= upper) {
                    res = ai;
                }
            } else if (pLsrAPIntGreaterOrEqual(a, pLsrAPIntFromSInt32(lower)) &&
                       pLsrAPIntLessOrEqual(a, pLsrAPIntFromSInt32(upper))) {
                res = (sint32) mpz_get_si(t1);
            }
        } else if (fitsInLong) {
            sint32 ai = (sint32) mpz_get_si(t1);
            if (ai >= lower && ai <= upper) {
                res = ai;
            }
        }
        return res;
    }
}

#define pLsrAPIntFromFloat32 pLsrAPIntNewFromFloat32
#define pLsrAPIntFromFloat64 pLsrAPIntNewFromFloat64

static float32 pLsrFloat32FromAPInt(PlsrAPInt a) {
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        float32 res = (float32) mpz_get_d(t1);
        pLsrAPIntTrace1("To float32", a);
        return res;
    }
}

static float64 pLsrFloat64FromAPInt(PlsrAPInt a) {
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        float64 res = (float64) mpz_get_d(t1);
        pLsrAPIntTrace1("To float64", a);
        return res;
    }
}

#define hashPair(h1, h2) ((h1)+((h2)<<5)+(h2)+720)

/* no gc */
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

static uintp pLsrAPIntHash(PlsrAPInt a)
{
    pLsrAPIntBeginNoGC {
        mpz_t t1 = pLsrAPIntInitMPZFromAPInt(a);
        uintp h = pLsrAPIntMPZHash(t1);
        return h;
    }
}

/* This function must match the previous one */
static uintp pLsrSInt32Hash(sint32 i)
{
    pLsrAPIntBeginNoGC {
        uintp h;
        mpz_t tmp;
        mpz_init_set_si(tmp, i);
        h = pLsrAPIntMPZHash(tmp);
        return h;
    }
}

/* Initialization and Registration */

static void pLsrAPIntRegisterVTables()
{
    static PgcIsRef pLsrAPIntRefs[pLsrAPIntSize/P_WORD_SIZE] = { 0, };

    pLsrVTableRegister(pLsrAPIntVTable, pLsrAPIntAlignment, pLsrAPIntSize, pLsrAPIntRefs, sizeof(mp_limb_t), P_WORD_SIZE, 0,
                       PGC_ALWAYS_MUTABLE, 0);

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

#ifdef P_USE_PILLAR
#  pragma pillar_managed(off)
#  define to __to__

static void* pLsrGMPMalloc(size_t size)
{
    PlsrAPIntS* mem;
    pLsrAllocNoGC(PlsrAPIntS*, mem, pLsrAPIntVTable, sizeof(PlsrAPIntS) + size);
    mem->alloc = size/sizeof(mp_limb_t);
    assert(size % sizeof(mp_limb_t) == 0);
    if (size > pLsrGmpAllocationReserve/2) pLsrGmpAllocationReserve = size*2;
    return pLsrAPInt_mp_d(mem);
}

static void pLsrGMPFree(void* object, size_t size)
{
    PlsrAPInt a;
    assert((a = pLsrAPIntMPZMemPtrToAPInt(object), pLsrAPInt_mp_alloc(a)*sizeof(mp_limb_t) ==  size));
}

static void* pLsrGMPReAlloc(void* object, size_t osize, size_t nsize)
{
    PlsrAPIntS* mem;
    void* s;
    pLsrAllocNoGC(PlsrAPIntS*, mem, pLsrAPIntVTable, sizeof(PlsrAPIntS) + nsize);
    s = pLsrAPInt_mp_d(mem);
    memcpy(s, object, osize);
    mem->alloc = nsize/sizeof(mp_limb_t);
    assert(nsize % sizeof(mp_limb_t) == 0);
    if (nsize > pLsrGmpAllocationReserve/2) pLsrGmpAllocationReserve = nsize*2;
    return s;
}

#  undef to
#pragma pillar_managed(on)
#endif


static void pLsrAPIntInitialize(uintp memLimit) {
    mp_set_memory_functions(pLsrGMPMalloc, pLsrGMPReAlloc, pLsrGMPFree);
    pLsrAPIntStaticGlobalInitFromSInt32(pLsrAPIntZero, 0);
    pLsrAPIntStaticGlobalInitFromSInt32(pLsrAPIntOne, 1);
    pLsrAPIntStaticGlobalInitFromSInt32(pLsrAPIntMinusOne, -1);
    PlsrAPInt tmp = pLsrAPIntFromUInt32(UINT32_MAX);
    tmp = pLsrAPIntNegate(tmp);
    pLsrAPIntStaticGlobalInitFromAPInt(pLsrAPIntMinusUInt32Max, tmp);
};


#endif /*_PLSR_AP_INTEGER_H_ */
