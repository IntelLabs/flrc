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

/* This file defines the Pil abstractions to make C and Pillar look as close
 * as possible.
 */

#ifndef _PIL_H_
#define _PIL_H_

#ifdef P_USE_PILLAR
#  pragma pillar_managed(off)
#  define to __to__
#endif /* P_USE_PILLAR */

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#ifdef P_USE_PILLAR
#  undef to
#  pragma pillar_managed(on)
#  include "prt/prt.h"
#endif /* P_USE_PILLAR */

/**********************************************************************/
/* Define definite sized integral types                               */

#ifdef WIN32  /* MS VC++ compiler */

typedef __int64 sint64;
typedef __int32 sint32;
typedef __int8 sint8;
typedef __int16 sint16;
typedef unsigned __int64 uint64;
typedef unsigned __int32 uint32;
typedef unsigned __int8 uint8;
typedef unsigned __int16 uint16;
typedef float float32;
typedef double float64;

#define SINT32_MAX INT_MAX
#define SINT32_MIN INT_MIN
#define UINT32_MAX ULONG_MAX

#define SINT64_MAX _I64_MAX
#define SINT64_MIN _I64_MIN
#define UINT64_MAX _UI64_MAX

#define INFINITY32 ((float)(1.0/0.0))
#define NAN32 ((float)0.0/0.0)
#define INFINITY64 ((double)(1.0/0.0))
#define NAN64 ((double)0.0/0.0)

#define SINT64_C(c) c ## L
#define UINT64_C(c) c ## UL

#else /* !WIN32 */
#ifdef __INTEL_COMPILER  /* Intel C Compiler */

typedef long long sint64;
typedef unsigned long long uint64;
typedef int sint32;
typedef unsigned uint32;
typedef short sint16;
typedef unsigned short uint16;
typedef char sint8;
typedef unsigned char uint8;
typedef float float32;
typedef double float64;

#define SINT32_MAX INT_MAX
#define SINT32_MIN INT_MIN
#define UINT32_MAX UINT_MAX

#define SINT64_MAX _I64_MAX
#define SINT64_MIN _I64_MIN
#define UINT64_MAX _UI64_MAX

#define INFINITY32 ((float)(1.0/0.0))
#define NAN32 ((float)0.0/0.0)
#define INFINITY64 ((double)(1.0/0.0))
#define NAN64 ((double)0.0/0.0)

#else /* !WIN32 && !__INTEL_COMPILER */
#ifdef __GNUC__ /* gcc */

#ifndef P_USE_PILLAR
#ifndef __cdecl
#define __cdecl __attribute__((cdecl))
#endif /* __cdecl */
#endif /* P_USE_PILLAR */

#include <stdint.h>

typedef long long int sint64;
typedef unsigned long long int uint64;
typedef int sint32;
typedef unsigned uint32;
typedef short sint16;
typedef unsigned short uint16;
typedef char sint8;
typedef unsigned char uint8;
typedef float float32;
typedef double float64;

#define SINT32_MAX INT32_MAX
#define SINT32_MIN INT32_MIN

#define SINT64_MAX INT64_MAX
#define SINT64_MIN INT64_MIN

#define INFINITY32 ((float)(1.0/0.0))
#define NAN32 ((float)0.0/0.0)
#define INFINITY64 ((double)(1.0/0.0))
#define NAN64 ((double)0.0/0.0)

#ifdef INTEL64
#define SINT64_C(c)    c ## L
#define UINT64_C(c)   c ## UL
#else
#define SINT64_C INT64_C
#endif

#else

#error Unknown compiler

#endif /* !WIN32 && !__INTEL_COMPILER && !__GNUC__ */
#endif /* !WIN32 && !__INTEL_COMPILER */
#endif /* !WIN32 */

#if P_WORD_SIZE == 4

typedef sint32 sintp;
typedef uint32 uintp;
#define SINTP_MAX SINT32_MAX
#define SINTP_MIN SINT32_MIN
#define UINTP_MAX UINT32_MAX

#elif P_WORD_SIZE == 8

typedef sint64 sintp;
typedef uint64 uintp;
#define SINTP_MAX SINT64_MAX
#define SINTP_MIN SINT64_MIN
#define UINTP_MAX UINT64_MAX

#else /* P_WORD_SIZE notin {4, 8} */

#error P_WORD_SIZE not defined or not a supported size

#endif /* P_WORD_SIZE notin {4, 8} */

#ifdef  __GNUC__
#define pil_aligned(a) __attribute__((aligned(a)))
#else
#define pil_aligned(a) __declspec(align(a))
#endif

typedef uintp bool;

static void pilCheck()
{
    if (sizeof(sint32)!=4) { fprintf(stderr, "pil: bad sint32!\n"); exit(-1); }
    if (sizeof(uint32)!=4) { fprintf(stderr, "pil: bad uint32!\n"); exit(-1); }
    if (sizeof(sint64)!=8) { fprintf(stderr, "pil: bad sint64!\n"); exit(-1); }
    if (sizeof(uint64)!=8) { fprintf(stderr, "pil: bad uint64!\n"); exit(-1); }
    if (sizeof(sintp)!=P_WORD_SIZE)
        { fprintf(stderr, "pil: bad sintp!\n"); exit(-1); }
    if (sizeof(uintp)!=P_WORD_SIZE)
        { fprintf(stderr, "pil: bad uint!\n"); exit(-1); }
    if (sizeof(void*)!=P_WORD_SIZE)
        { fprintf(stderr, "pil: bad word size!\n"); exit(-1); }
    if (sizeof(float32)!=4) { fprintf(stderr, "pil: bad float32!\n"); exit(-1); }
    if (sizeof(float64)!=8) { fprintf(stderr, "pil: bad float64!\n"); exit(-1); }
}

/**********************************************************************/
/* Control stuff                                                      */

#ifdef P_USE_PILLAR

#ifdef __pillar2c__
# define PilContinuation0 continuation_type
# define PilContinuation(...) continuation_type< __VA_ARGS__ >
#else
# define PilContinuation0 g4
# define PilContinuation(...) g4
#endif
#define pilContinuationLocal(cv)
#define pilContinuationMake(v, cl, cv) (v) = (cv)
#define pilContinuation0(cl, cv) continuation cv():
#define pilContinuation(cl, cv, ...) continuation cv(__VA_ARGS__):
#define pilCutTo0(c) cut to c
#ifdef __pillar2c__
  #define pilCutToA(c, ...) cut to c with (__VA_ARGS__)
  #define pilCutToC(...) also cuts to (__VA_ARGS__)
#else /* !__pillar2c__ */
  #define pilCutToA(c, ...) cut to c(__VA_ARGS__)
  #define pilCutToC(...) also cuts to __VA_ARGS__
#endif /* !__pillar2c__ */

#if !PLSR_DISABLE_TAILCALL
  #define TAILCALL(e) return tailcall e
  #define TAILCALLV(e) tailcall e; return
#else
  #define TAILCALL(e) return e
  #define TAILCALLV(e) e;return
#endif

#ifdef USE_PTHREADS
#define YIELDCHECK() (*(unsigned volatile*)(((char *)prtGetTaskHandle()) + (3*P_WORD_SIZE)))
#else  // USE_PTHREADS
#ifdef NO_PRSCALL
#define YIELDCHECK() (*((*((short volatile**)(((char *)prtGetTaskHandle()) +(2*P_WORD_SIZE)))) + 0x76))
#else  // NO_PRSCALL
#define YIELDCHECK() (*(short volatile*)(**(char volatile* volatile* volatile*)((char *)prtGetTaskHandle()+(2*P_WORD_SIZE))+0x76))
#endif // NO_PRSCALL
#endif // USE_PTHREADS

#define pilYieldDec uintp spin=0
#define pilYield()                                              \
    do {                                                        \
        if (++spin&15==0 && YIELDCHECK()) prtYield();           \
    } while (0)

#else /* !P_USE_PILLAR */

#include <setjmp.h>

#define PilContinuation0 jmp_buf*
#define PilContinuation(...) jmp_buf*
#define pilContinuationLocal(cv) jmp_buf cv
#define pilContinuationMake(v, cl, cv)                          \
    do { (v) = &(cv); if (setjmp((cv))) goto cl; } while (0)
#define pilContinuation0(cl, cv)
#define pilContinuation(cl, cv, ...)
#define pilCutTo0(c) longjmp(*c, 1)
#define pilCutToA(c, ...) longjmp(*c, 1)
#define pilCutToC(...)

#define TAILCALL(e) return (e)
#define TAILCALLV(e) do { e; return; } while (0)
#define noyield
#define pilYieldDec
#define pilYield()

#endif /* !P_USE_PILLAR */

#endif /* !_PIL_H */
