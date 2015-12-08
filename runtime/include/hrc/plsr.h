/* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

/* The P language-specific runtime */

#ifndef _PLSR_H_
#define _PLSR_H_

/**********************************************************************
 * Control
 */

/* User Control macros:
 *   P_WORD_SIZE               - number of bytes in a pointer
 *   P_USE_PILLAR              - compiled for pillar
 *   P_USE_CGC                 - use BDW conservative garbage collector
 *   P_USE_AGC                 - use a PSL accurate garabage collector
 *                             - value specifies kind (see PlsrAgcKind )
 *   P_AGC_LOCK_PARAM          - the lock parameter to pass to pgc_init
 *   PLSR_ENABLE_EVENTS        - Turn on the event logging code.
 *   P_USE_FAST_ALLOC          - use an inlined fast path for allocation
 *   PLSR_INSTRUMENT_ALLOCATION - collect and report allocation stats
 *   PLSR_INSTRUMENT_GCS       - collect and report per gc allocation stats
 *   PLSR_INSTRUMENT_VTB_ALC   - collect and report per vtable allocation stats
 *   P_USE_GC_WRITE_BARRIERS   - use write barriers for the GC
 *   P_ALL_BARRIERS            - do all barriers for barrier validation
 *   P_USE_PARALLEL_FUTURES    - use the parallel futures system
 *   P_DO_VTABLE_CHANGE        - do vtable changing for immutability etc.
 *   P_PRAT_USE_SINTP          - use sintp to represent P Rationals (unsafe)
 *   P_USE_TAGGED_INTEGERS     - use low bit tagging on small ap integers
 *   P_USE_TAGGED_RATIONALS    - use low bit tagging on small ap rationals
 *   P_TAGGED_INT32_PORTABLE   - see commentary in plsr-tagged-int32.h
 *   P_TAGGED_INT32_ASSERT_SMALL - use 32 bit integers with runtime checks
 *   P_TAGGED_INT32_ASSUME_SMALL - use 32 bit untegers with no checks
 *   PLSR_DISABLE_TAILCALL     - disable tailcalls
 *   PLSR_GMP_USE_GCMALLOC     - use gc malloc for gmp wrappers
 *   PLSR_GMP_USE_PCDECL       - use pcdecl to avoid pinning gmp wrappers
 *   PLSR_GMP_USE_MALLOC       - use malloc to avoid pinning gmp wrappers
 *   PLSR_GMP_FORCE_GC         - force malloc heap limits for gmp wrappers
 *   PLSR_GMP_USE_PINNING      - use pinning in gmp wrappers
 *   PLSR_GMP_USE_GALLOCATE    - use guaranteed allocation in gmp wrappers
 *   PLSR_GMP_USE_DEFAULT      - use a backend default gmp implementation
 *   PLSR_TAGGED_INT32_TEST    - generate test functions
 *   PLSR_TAGGED_INTEGER_RECOVER - Check AP int output for taggable integers
 *   PLSR_THUNK_INTERCEPT_CUTS - Intercept cuts out of thunks
 *   PLSR_THUNK_SYNCHRONIZE    - use synchronized thunks
 *   PLSR_NO_GMP_INTEGERS      - don't use the GMP
 *   PLSR_STACK_SIZE_WORKER    - default stack size (mb) for ipc worker threads
 *   PLSR_STACK_SIZE_MAIN      - default stack size (mb) for ipc main threads
 *   PLSR_SINGLE_THREADED      - no runtime concurrency allowed
 * Derived Control macros:
 *   P_USE_MCRT                - compiled for C and use mcrt
 *   P_USE_PTHREADS            - use pthreads for threading
 *   PLSR_GNU_ASM              - use gnu style asm
 *   PLSR_MS_ASM               - use ms style asm
 */

/*#define PLSR_ENABLE_EVENTS*/

/* Derive control macros */

#if defined(__GNUC__) || __INTEL_COMPILER >= 1000
  #define PLSR_GNU_ASM
#else
  #define PLSR_MS_ASM
#endif

#if defined(PLSR_SINGLE_THREADED) && defined(P_USE_PARALLEL_FUTURES)
#error "Can't use parallel futures with the single threaded runtime"
#endif

#if defined(P_USE_PARALLEL_FUTURES) && !defined(PLSR_THUNK_SYNCHRONIZE)
#error "Must use synchronized thunks with parallel futures"
#endif

#ifdef USE_PTHREADS
#define P_USE_PTHREADS
#else
#if !defined(P_USE_PILLAR) && defined(P_USE_PARALLEL_FUTURES)
#define P_USE_MCRT
#endif /* !P_USE_PILLAR && P_USE_PARALLEL_FUTURES */
#endif /* USE_PTHREADS */

/* Memory allocation */
#ifdef P_USE_CGC
#    define GC_NOT_DLL
#  ifdef DEBUG
#    define GC_DEBUG
#  endif /* DEBUG */
#endif /* P_USE_CGC */

/**********************************************************************
 * C header files
 */

#ifdef P_USE_PILLAR
#  pragma pillar_managed(off)
#  define to __to__
#endif /* P_USE_PILLAR */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <sys/timeb.h>
#ifndef PLSR_LINUX
#include <windows.h>
#endif
#include <limits.h>

/* Memory allocation and collection utilities */

#define PgcIsRef bool

enum PlsrAgcKind {
    PlsrAKMf  = 1,
    PlsrAKCgc = 2,
    PlsrAKTgc = 3
};

#ifdef P_USE_CGC
#  ifdef P_USE_PILLAR
#     error "CGC and Pillar combination not supported"
#  endif /* P_USE_PILLAR */
#  include "toolkit/gc.h"
#  define P_VTABLE_RESERVE P_WORD_SIZE
#else /* !P_USE_CGC */
#ifdef P_USE_AGC
#  ifdef P_USE_PILLAR
#     undef PgcIsRef
#     include "pgc/pgc.h"
#     define P_VTABLE_RESERVE PGC_VTABLE_RESERVE
#  else /* !P_USR_PILLAR */
#     error "AGC and non-Pillar combination not support"
#  endif /* !P_USE_PILLAR */
#else /* !P_USE_AGC */
#  include <malloc.h>
#  define P_VTABLE_RESERVE P_WORD_SIZE
#endif /* !P_USE_AGC */
#endif /* !P_USE_CGC */

#if !defined(P_USE_PTHREADS) && (defined(P_USE_MCRT) || defined(P_USE_PARALLEL_FUTURES))
#include <mcrt.h>
#endif /* defined(P_USE_MCRT) || defined(P_USE_PARALLEL_FUTURES) */

#ifdef P_USE_PILLAR
#  undef to
#  pragma pillar_managed(on)
#  include "prt/prt.h"
#  include "prt/prtcodegenerator.h"
#endif /* P_USE_PILLAR */

/* Toolkit headers - these work in both C and Pillar */

#if defined(P_USE_PARALLEL_FUTURES) || defined(PLSR_THUNK_SYNCHRONIZE)
#define CONCURRENCY
#else /* !P_USE_PARALLEL_FUTURES */
#undef CONCURRENCY
#endif /* !P_USE_PARALLEL_FUTURES */
#include "toolkit/ptkfuture.h"
typedef ref PtkRef;

/**********************************************************************
 * Runtime proper
 */

#include "hrc/plsr-util.h"
#include "hrc/plsr-objects.h"
#include "hrc/plsr-synchronization.h"
#include "hrc/plsr-gc.h"
#include "hrc/plsr-finalizer.h"
#include "hrc/plsr-numeric.h"
#include "hrc/plsr-thunk.h"
#include "hrc/plsr-value.h"
#include "hrc/plsr-wpo.h"
#include "hrc/plsr-prims.h"
#include "hrc/plsr-params.h"
#include "hrc/plsr-main.h"

#endif /* !_PLSR_H_ */
