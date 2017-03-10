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

/* Utilities */

#ifndef _PLSR_UTIL_H_
#define _PLSR_UTIL_H_

#include <assert.h>
#include <stdlib.h>
#include "hrc/pil.h"

#ifdef P_USE_PILLAR
#  pragma pillar_managed(off)
#  define to __to__
#endif /* P_USE_PILLAR */

#ifdef PLSR_LINUX
#include <sys/time.h>
#endif

#ifdef P_USE_PILLAR
#  undef to
#  pragma pillar_managed(on)
#endif

/* This is the type of Mil booleans.  Mil booleans
 * are integers in {0, 1}.  Use toPlsrBoolean to
 * convert C booleans (i.e. zero/non-zero values)
 * to P booleans.
 */
typedef uintp PlsrBoolean;
#define toPlsrBoolean(e) ((PlsrBoolean)(!!(e)))

/* Absolute value.  According the c99 spec, converting from a signed to an
 * unsigned type of the same size:
 * 1) preserves the value if the value is representable (positive)
 * 2) is the result of repeatedly adding one more than the maximum value of
 *    the new type to the old value
 * So, assuming a is a negative sint32, then
 * ((uint32) a) = (UINT32_MAX + 1) + a (using non-modular arithmetic).  This is
 * guaranteed to be representable, since |SINT32_MIN| < UINT32_MAX
 * So using true arithmetic, we have:
 * UINT32_MAX - ((uint32) a) + 1 = UINT32_MAX - (UINT32_MAX + 1 + a)  + 1
 *                               = UINT32_MAX - UINT32_MAX -1 -a + 1
 *                               = -a
 * But note that since SINT32_MIN <= a < 0,
 * 0 < ((uint32) a) = (UINT32_MAX + 1) + a <= UINT32_MAX must be true, so
 * UINT32_MAX - ((uint32) a) cannot underflow, and must be less than
 * UINT32_MAX (since ((uint32) a) must be positive.
 * Therefore UINT32_MAX - ((uint32) a) + 1 can be computed precisely in
 * 32 bit modular arithmetic.
 */

#define pLsrUInt32FromNegativeSInt32Abs(a) (UINT32_MAX - ((uint32) a) + 1)
#define pLsrUInt64FromNegativeSInt64Abs(a) (UINT64_MAX - ((uint64) a) + 1)
#define pLsrUIntpFromNegativeSIntpAbs(a) (UINTP_MAX - ((uintp) a) + 1)

static uint32 pLsrAbs32(sint32 a) {
    if (a < 0) {
        return UINT32_MAX - ((uint32) a) + 1;
    } else {
        return a;
    }
}

static uint64 pLsrAbs64(sint64 a) {
    if (a < 0) {
        return UINT64_MAX - ((uint64) a) + 1;
    } else {
        return a;
    }
}

static uintp pLsrAbsP(sintp a) {
    if (a < 0) {
        return UINTP_MAX - ((uintp) a) + 1;
    } else {
        return a;
    }
}

#ifndef PRT_NORETURN
#ifdef __GNUC__
#define PRT_NORETURN __attribute__((noreturn))
#else
#define PRT_NORETURN __declspec(noreturn)
#endif
#endif

static void pLsrExit(int status)
{
#ifdef P_USE_PARALLEL_FUTURES
    ptkFutureSystemShutdown();
#endif
#ifdef P_USE_AGC
    pgc_kill();
#endif /* P_USE_AGC */
#ifdef P_USE_PILLAR
    prtExit(status);
#else /* !P_USE_AGC */
    exit(status);
#endif /* !P_USE_AGC */
}

#define pLsrHalt(status)                        \
    do {                                        \
        pLsrExit(status);                       \
        return 0;                               \
    } while (0)

#define pLsrHaltV(status)                        \
    do {                                         \
        pLsrExit(status);                        \
        return;                                  \
    } while (0)

static PRT_NORETURN void pLsrHaltNegOne()
{
    fflush(stdout);
    assert(0);
    pLsrExit(-1);
}

#define pLsrRuntimeError(msg) \
    (printf("Runtime error: %s at line %d in file %s\n", msg, __LINE__, __FILE__), \
     pLsrHaltNegOne(),                                                        \
     0)

static PRT_NORETURN void pLsrRuntimeError_(char * msg) {
    pLsrRuntimeError(msg);
}

#ifdef P_USE_PILLAR
#  pragma pillar_managed(off)
#  define to __to__

static PRT_NORETURN void pLsrRuntimeErrorUnmanaged(char * msg) {
    printf("Runtime error: %s at line %d in file %s\n", msg, __LINE__, __FILE__), \
    fflush(stdout);
    assert(0);
    exit(-1);
}

#  undef to
#pragma pillar_managed(on)
#endif

void pLsrDisableErrorBox();
uint64 pLsrEventsTimeStamp();

#if (defined(PLSR_ENABLE_EVENTS)) && (defined(_WIN32))

static uint64 pLsrEventsStartTime;
static FILE* pLsrEventsLogFile;

void pLsrEventsInit()
{
    pLsrEventsStartTime = pLsrEventsTimeStamp();
    pLsrEventsLogFile = fopen("events.log", "wt");
}

#define pLsrEventsTransition(action, state)           \
    fprintf(pLsrEventsLogFile, "%d,%p,%s,%s\n",                         \
            (uint32) (pLsrEventsTimeStamp() - pLsrEventsStartTime),     \
            prtGetTaskHandle(),                                         \
            action, state);                                             \

void pLsrEventsShutdown()
{
    fclose(pLsrEventsLogFile);
}

#else /* PLSR_ENABLE_EVENTS */

#define pLsrEventsInit()
#define pLsrEventsTransition(action, state)
#define pLsrEventsShutdown()

#endif /* PLSR_ENABLE_EVENTS */

#endif /* !_PLSR_UTIL_H_ */
