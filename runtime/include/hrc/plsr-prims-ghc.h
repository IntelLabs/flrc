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

#ifndef _PLSR_PRIMS_GHC_H_
#define _PLSR_PRIMS_GHC_H_

/* XXX NG: This stuff doesn't work under C */
#ifdef P_USE_PILLAR

/* get type definitions from pil.h */
#include "hrc/pil.h"
#include "hrc/plsr-value.h"
#include "hrc/plsr-gc.h"
#include "hrc/plsr-util.h"

/* C functions used for Runtime */
#ifdef P_USE_PILLAR
#  pragma pillar_managed(off)
#  define to __to__
#endif /* P_USE_PILLAR */

#ifndef PLSR_LINUX
#include <io.h>
#include <errno.h>
#endif // !PLSR_LINUX

#include "hrc/ghc/float.h"
#include "hrc/ghc/Globals.h"
#include "hrc/ghc/TTY.h"
#include "hrc/ghc/thread.h"

void pLsrGetProgArgv (void* argc, void* argv)
{
    if (argc) { *((sintp*)argc) = pargc; }
    if (argv) { *((void**)argv) = pargv; }
}

#ifdef P_USE_PILLAR
#  undef to
#  pragma pillar_managed(on)
#endif /* P_USE_PILLAR */

/*** Address Stuff ***/

/* pointer read */
#define pLsrPrimGHCIndexOffAddrzh(p,i,r,t)   ((r)*(((t*)(p))+(i)))
#define pLsrPrimGHCIndexFloatOffAddrzh(p,i)  pLsrPrimGHCIndexOffAddrzh(p,i,float,float)
#define pLsrPrimGHCIndexDoubleOffAddrzh(p,i) pLsrPrimGHCIndexOffAddrzh(p,i,double,double)
#define pLsrPrimGHCIndexAddrOffAddrzh(p,i)   pLsrPrimGHCIndexOffAddrzh(p,i,void*,void*)
#define pLsrPrimGHCIndexIntOffAddrzh(p,i)    pLsrPrimGHCIndexOffAddrzh(p,i,sintp,sintp)
#define pLsrPrimGHCIndexUIntOffAddrzh(p,i)   pLsrPrimGHCIndexOffAddrzh(p,i,uintp,uintp)
#define pLsrPrimGHCIndexUInt8OffAddrzh(p,i)  pLsrPrimGHCIndexOffAddrzh(p,i,uintp,uint8)
#define pLsrPrimGHCIndexUInt16OffAddrzh(p,i) pLsrPrimGHCIndexOffAddrzh(p,i,uintp,uint16)
#define pLsrPrimGHCIndexUInt32OffAddrzh(p,i) pLsrPrimGHCIndexOffAddrzh(p,i,uintp,uint32)
#define pLsrPrimGHCIndexUInt64OffAddrzh(p,i) pLsrPrimGHCIndexOffAddrzh(p,i,uint64,uint64)
#define pLsrPrimGHCIndexInt8OffAddrzh(p,i)   pLsrPrimGHCIndexOffAddrzh(p,i,sintp,sint8)
#define pLsrPrimGHCIndexInt16OffAddrzh(p,i)  pLsrPrimGHCIndexOffAddrzh(p,i,sintp,sint16)
#define pLsrPrimGHCIndexInt32OffAddrzh(p,i)  pLsrPrimGHCIndexOffAddrzh(p,i,sintp,sint32)
#define pLsrPrimGHCIndexInt64OffAddrzh(p,i)  pLsrPrimGHCIndexOffAddrzh(p,i,sint64,sint64)
/* pointer write */
#define pLsrPrimGHCWriteOffAddrzh(p,i,v,t)     { *(((t*)(p))+(i))=(t)(v); }
#define pLsrPrimGHCWriteFloatOffAddrzh(p,i,v)  pLsrPrimGHCWriteOffAddrzh(p,i,v,float)
#define pLsrPrimGHCWriteDoubleOffAddrzh(p,i,v) pLsrPrimGHCWriteOffAddrzh(p,i,v,double)
#define pLsrPrimGHCWriteAddrOffAddrzh(p,i,v)   pLsrPrimGHCWriteOffAddrzh(p,i,v,void*)
#define pLsrPrimGHCWriteIntOffAddrzh(p,i,v)    pLsrPrimGHCWriteOffAddrzh(p,i,v,sintp)
#define pLsrPrimGHCWriteUIntOffAddrzh(p,i,v)   pLsrPrimGHCWriteOffAddrzh(p,i,v,uintp)
#define pLsrPrimGHCWriteUInt8OffAddrzh(p,i,v)  pLsrPrimGHCWriteOffAddrzh(p,i,v,uint8)
#define pLsrPrimGHCWriteUInt16OffAddrzh(p,i,v) pLsrPrimGHCWriteOffAddrzh(p,i,v,uint16)
#define pLsrPrimGHCWriteUInt32OffAddrzh(p,i,v) pLsrPrimGHCWriteOffAddrzh(p,i,v,uint32)
#define pLsrPrimGHCWriteUInt64OffAddrzh(p,i,v) pLsrPrimGHCWriteOffAddrzh(p,i,v,uint64)
#define pLsrPrimGHCWriteInt8OffAddrzh(p,i,v)   pLsrPrimGHCWriteOffAddrzh(p,i,v,sint8)
#define pLsrPrimGHCWriteInt16OffAddrzh(p,i,v)  pLsrPrimGHCWriteOffAddrzh(p,i,v,sint16)
#define pLsrPrimGHCWriteInt32OffAddrzh(p,i,v)  pLsrPrimGHCWriteOffAddrzh(p,i,v,sint32)
#define pLsrPrimGHCWriteInt64OffAddrzh(p,i,v)  pLsrPrimGHCWriteOffAddrzh(p,i,v,sint64)
/* pointer arithmetic */
#define pLsrPrimGHCNullAddrzh() ((void*)0)
#define pLsrPrimGHCPlusAddrzh(p,i) ((void*)((char*)(p)+(i)))
#define pLsrPrimGHCMinusAddrzh(p,q) ((sintp)((char*)(p)-(char*)(q)))
#define pLsrPrimGHCRemAddrzh(p,i) (((sintp)(p)%(i)))
#define pLsrPrimGHCAddr2Intzh(p) ((sintp)(p))
#define pLsrPrimGHCInt2Addrzh(i) ((void*)(i))
#define pLsrPrimGHCGtAddrzh(p,q) ((p)>(q))
#define pLsrPrimGHCGeAddrzh(p,q) ((p)>=(q))
#define pLsrPrimGHCLtAddrzh(p,q) ((p)<(q))
#define pLsrPrimGHCLeAddrzh(p,q) ((p)<=(q))
#define pLsrPrimCastToAddrzh(p) ((void*)(p))
#define pLsrPrimCastFromAddrzh(p) ((PlsrPAny*)(p))
/* bit operations */
#define pLsrPrimGHCPopCntzh(w) ((uintp)(__builtin_popcount((unsigned)(w))))
#define pLsrPrimGHCPopCnt64zh(w) ((uintp)(__builtin_popcountll((uint64)(w))))
/* byte array */
#define pLsrPrimGHCByteArrayContentszh(p) ((void*)(&pLsrPArrayOElt((PlsrObjectB)(p), 0)))

#ifdef P_USE_PILLAR
static void performMajorGC () { pgc_force_gc(); }
#endif // P_USE_PILLAR

/*** Thread stuff ***/

typedef enum {IhrTsRunning, IhrTsCompleted, IhrTsKilled} IhrThreadStatus;

typedef struct IhrThreadS {
    PlsrVTable vtable;
    PrtTaskHandle prtThread;
    IhrThreadStatus status;
    bool maskAsyncExn, uninteruptable;
} IhrThreadU;

#define ihrThreadSize sizeof(IhrThreadU)
#define ihrThreadPadding \
    (sizeof(IhrThreadU) - sizeof(PlsrVTable) - sizeof(PrtTaskHandle) - sizeof(IhrThreadStatus) - 2*sizeof(bool))
pLsrVTableStatic (ihrVTableThread, "*haskell thread*", ihrThreadPadding);

#ifdef P_USE_PILLAR
typedef ref IhrThreadId;
#define ihrThreadB(t) ((IhrThreadU*)(t))
#else
typedef IhrThreadU* IhrThreadId;
#define ihrThreadB(t) (t)
#endif

typedef struct IhrTlsS {
    IhrThreadId thread;
    //PilContinuation(PlsrObjectB) exnHandler;
    PilContinuation0 exnHandler;
    PlsrObjectB exn;
} IhrTlsU;

typedef IhrTlsU* IhrTlsP;

unsigned ihrTlsOffset;

#define ihrPrtTlsToTls(tls) (*((IhrTlsP*)(((char*)(tls))+ihrTlsOffset)))

static void __cdecl ihrTlsRse(PrtProvidedTlsHandle tls_, struct PrtRseInfo* rse)
{
    IhrTlsP tls = ihrPrtTlsToTls(tls_);
    if (tls) {
        rse->callback(rse->env, &tls->thread, PrtGcTagDefault, NULL);
        rse->callback(rse->env, &tls->exn, PrtGcTagDefault, NULL);
    }
}

static void ihrTlsInit()
{
    ihrTlsOffset = ptkGetNextTlsOffset(sizeof(IhrTlsP));
    prtRegisterTlsEnumerator(ihrTlsRse);
}

static IhrTlsP ihrTlsGet()
{
    IhrTlsP tls = ihrPrtTlsToTls(prtGetTls());
    return tls;
}

static void ihrTlsSet(IhrTlsP tls)
{
    ihrPrtTlsToTls(prtGetTls()) = tls;
}

/*** Exceptions ***/

//static inline PilContinuation(PlsrObjectB) ihrExceptionHandlerGet()
static inline PilContinuation0 ihrExceptionHandlerGet()
{
    return ihrTlsGet()->exnHandler;
}

static inline PlsrObjectB ihrExceptionExnGet()
{
    PlsrObjectB exn = ihrTlsGet()->exn;
    assert(exn);
    return exn;
}

//static inline void ihrExceptionHandlerSet(PilContinuation(PlsrObjectB) c)
static inline void ihrExceptionHandlerSet(PilContinuation0 c)
{
    ihrTlsGet()->exnHandler = c;
}

static inline void ihrExceptionExnSet(PlsrObjectB c)
{
    ihrTlsGet()->exn = c;
}

/*** Delay/wait operations ***/

static void ihrDelay(sintp d)
{
    uintp s;
    if (d<0) return;
    s = (d+999)/1000;
    prtSleep(s);
}

static void ihrWaitRead(sintp f)
{
    /* NG: The GHC runtime blocks the given thread until the given file descriptor
     *     is ready to be read from.  Ultimately it uses select to test this.
     *     Since we don't multiplex Haskell level threads onto OS threads, it seems
     *     fine to just do nothing.  The thread will then go ahead and try to read.
     * XXX: If we start multiplexing Haskell threads, then we need to implement this
     *      properly.
     */
}

static void ihrWaitWrite(sintp f)
{
    /* NG: The GHC runtime blocks the given thread until the given file descriptor
     *     is ready to be written to.  Ultimately it uses select to test this.
     *     Since we don't multiplex Haskell level threads onto OS threads, it seems
     *     fine to just do nothing.  The thread will then go ahead and try to write.
     * XXX: If we start multiplexing Haskell threads, then we need to implement this
     *      properly.
     */
}

typedef struct IhrPairS {
    PlsrVTable vtable;
    sintp fst;
    sintp snd;
}* IhrPair;

#define ihrPairPadding \
    (sizeof(struct IhrPairS) - sizeof(PlsrVTable) - 2*sizeof(sintp))
pLsrVTableStatic (ihrVTablePair, "*IHR pair*", ihrPairPadding);
#define ihrPairSize (sizeof(struct IhrPairS))

#ifndef PLSR_LINUX

/* NG: The GHC runtime blocks the given thread until the request read/write/action
 *     is done.  Since we don't multiplex Haskell level threads onto OS threads, it
 *     seems fine to just do the operation and the return.
 * XXX: If we start multiplexing Haskell threads, then we need to implement this
 *      properly.
 */

/* XXX NG: These are non-threaded implementations.  Threaded ones should barf. */

static IhrPair ihrAsyncRead(sintp f, sintp isSock, sintp num, void* buf)
{
    IhrPair res;
    int len;
    DWORD ec = 0;

    if (isSock) {
        len = recv(f, buf, num, 0);
        if (len==SOCKET_ERROR)
            ec = WSAGetLastError();
    } else {
        len = read(f, buf, num);
        /* XXX NG: GHC does some special processing in certain circumstances,
         *         but I'm not sure we can do the same, so I'm ignoring that.
         */
        if (len==-1) ec = errno;
    }

    pLsrAlloc(IhrPair, res, &ihrVTablePair, ihrPairSize);
    res->fst = len;
    res->snd = ec;
    return res;
}

static IhrPair ihrAsyncWrite(sintp f, sintp isSock, sintp num, void* buf)
{
    IhrPair res;
    int len;
    DWORD ec = 0;

    if (isSock) {
        len = send(f, buf, num,0);
        if (len==SOCKET_ERROR)
            ec = WSAGetLastError();
    } else {
        len = write(f, buf, num);
        /* XXX NG: GHC does some special processing in certain circumstances,
         *         but I'm not sure we can do the same, so I'm ignoring that.
         */
        if (len==-1) {
            ec = errno;
            if (ec==EINVAL && GetLastError()==ERROR_NO_DATA) ec = EPIPE;
        }
    }

    pLsrAlloc(IhrPair, res, &ihrVTablePair, ihrPairSize);
    res->fst = len;
    res->snd = ec;
    return res;
}

typedef sintp (*IhrProc)(void*);

static sintp ihrAsyncDoProc(void* f_, void* a)
{
    IhrProc f = (IhrProc)f_;
    if (f)
        return f(a);
    else
        return 1;
}

#endif // !PLSR_LINUX

/*** Concurency ***/

#define ihrThreadMask(t) (ihrThreadB(t)->maskAsyncExn=1, ihrThreadB(t)->uninteruptable=1)
#define ihrThreadUnmask(t) (ihrThreadB(t)->maskAsyncExn=0, ihrThreadB(t)->uninteruptable=0)

#define ihrThreadAllocInit(t, pt)                                       \
    do {                                                                \
        pLsrAlloc(IhrThreadId, (t), &ihrVTableThread, ihrThreadSize);   \
        ihrThreadB(t)->prtThread = (pt);                                \
        ihrThreadB(t)->status = IhrTsRunning;                           \
        ihrThreadMask(t);                                               \
    } while(0)

static void ihrThreadWrapper0(void (*f)())
{
    IhrTlsU tls;
    PlsrObjectB exn;
    PilContinuation0 cv;
    //PilContinuation(PlsrObjectB) cv;
    pilContinuationLocal(cvl);
    ihrThreadAllocInit(tls.thread, prtGetTaskHandle());
    pilContinuationMake(cv, cvl, c);
    tls.exnHandler = c;
    tls.exn = NULL;
    ihrTlsSet(&tls);
    ihrThreadUnmask(tls.thread);
    f() pilCutToC(c);
    ihrThreadMask(tls.thread);
    ihrThreadB(tls.thread)->status = IhrTsCompleted;
    return;
    pilContinuation0(cvl, c)
        //pilContinuation(cvl, c, exn)
        ihrThreadB(tls.thread)->status = IhrTsKilled;
        return;
}

static void ihrThreadWrapper1(void (*f)(PlsrObjectB), PlsrObjectB a)
{
    IhrTlsU tls;
    PlsrObjectB exn;
    PilContinuation0 cv;
    //PilContinuation(PlsrObjectB) cv;
    pilContinuationLocal(cvl);
    ihrThreadAllocInit(tls.thread, prtGetTaskHandle());
    pilContinuationMake(cv, cvl, c);
    tls.exnHandler = c;
    tls.exn = NULL;
    ihrTlsSet(&tls);
    ihrThreadUnmask(tls.thread);
    f(a) pilCutToC(c);
    ihrThreadMask(tls.thread);
    ihrThreadB(tls.thread)->status = IhrTsCompleted;
    return;
    pilContinuation0(cvl, c)
        //pilContinuation(cvl, c, exn)
        ihrThreadB(tls.thread)->status = IhrTsKilled;
        return;
}

/* We make this a separate function because some of the thunk evals are macros,
 * and we need to annotate the cuts below in ihrForkedThreadMain
 */
static void ihrForkedThreadRun(PlsrObjectB thnk)
{
    pLsrThunkEvalRef(thnk);
}

static void ihrForkedThreadMain(IhrThreadId thrd, PlsrObjectB thnk)
{
    IhrTlsU tls;
    PlsrObjectB exn;
    PilContinuation0 cv;
    //PilContinuation(PlsrObjectB) cv;
    pilContinuationLocal(cvl);
    ihrThreadB(thrd)->prtThread = prtGetTaskHandle();
    tls.thread = thrd;
    pilContinuationMake(cv, cvl, c);
    tls.exnHandler = c;
    tls.exn = NULL;
    ihrTlsSet(&tls);
    ihrThreadUnmask(tls.thread);
    ihrForkedThreadRun(thnk) pilCutToC(c);
    ihrThreadMask(tls.thread);
    ihrThreadB(tls.thread)->status = IhrTsCompleted;
    return;
    pilContinuation0(cvl, c)
        //pilContinuation(cvl, c, exn)
        ihrThreadB(tls.thread)->status = IhrTsKilled;
        return;
}

static void __cdecl ihrForkOnRse(PrtCodeAddress func,
                                 void *arguments,
                                 struct PrtRseInfo *rse,
                                 PrtCimSpecificDataType opaque) {
    rse->callback(rse->env,((void**)arguments)+1,PrtGcTagDefault,0);
}

static IhrThreadId ihrForkOn(sintp p, PlsrObjectB thnk)
{
    IhrThreadId thrd;
    ihrThreadAllocInit(thrd, NULL);
    noyield {
        void* args[2] = {thrd, NULL};
        // This write barrier promotes thnk to the public space.
        pLsrWriteBarrierRef(args[1], thnk);
        // This statement keeps thnk alive past the write barrier so that
        // any GC caused by the write barrier doesn't eliminate it.
        args[1] = thnk;
        prtPcall((PrtCodeAddress)ihrForkedThreadMain, args, 2, p, ihrForkOnRse);
    }
    return thrd;
}

static IhrThreadId ihrFork(PlsrObjectB t)
{
    return ihrForkOn(PRT_NO_PROC_AFFINITY, t);
}

static void ihrKillThread(IhrThreadId t, PlsrObjectB exn)
{
    pLsrRuntimeError("ihrKillThread unimplemented");
}

static void ihrYield()
{
    prtYield();
}

static IhrThreadId ihrMyThreadId()
{
    return ihrTlsGet()->thread;
}

static void ihrLabelThread(IhrThreadId t, void* l)
{
    pLsrRuntimeError("ihrLabelThread unimplemented");
}

static sintp ihrIsCurrentThreadBound()
{
    return 0;
}

static void ihrNoDuplicate()
{
    /* This GHC primitive ensures that no two threads are evaluating the same thunk at the same time.
     * Since we currently ensure this all the time, this is a no operation.
     * If this changes, then we need to implement this operation.
     */
}

static sintp ihrThreadStatus(IhrThreadId t)
{
    //pLsrRuntimeError("ihrThreadStatus unimplemented");
    return 0;
}

/*** Weak Pointers ***/

/* a no-op just to keep object alive */
#define pLsrPrimGHCTouchzh(p) do {volatile ref pLsrPrimGHCTouchzh_tmp = (ref)(uintp)(p);} while (0)

/* Run the Haskell finaliser - needs to wrap the thread for exceptions, etc. */
void ihrRunHaskellFinaliser(PlsrObjectB f)
{
    ihrThreadWrapper1(pLsrWpoRunFinalizer, f);
}

/*** Float and Double Encoding/Decoding ***/

typedef struct {
    PlsrVTable vtable;
    uintp tag;
    sintp man_sign;
    uintp man_high;
    uintp man_low;
    sintp exp;
} PlsrDecodedDoubleU;
typedef PlsrDecodedDoubleU *PlsrDecodedDoubleB;
#define pLsrDecodedDoublePadding                \
    (sizeof(PlsrDecodedDoubleU) - sizeof(PlsrVTable) - sizeof(uintp) - sizeof(sintp) - 2*sizeof(uintp) - sizeof(sintp))
pLsrVTableStatic (pLsrVTableDecodedDouble, "*decoded double*", pLsrDecodedDoublePadding);
PlsrRef pLsrPrimGHCDecodeDouble2Intzh (double v)
{
    PlsrDecodedDoubleB d;
    uintp h, l;
    sintp s, e;
    __decodeDouble_2Int (&s, &h, &l, &e, v);
    pLsrAlloc (PlsrDecodedDoubleB, d, &pLsrVTableDecodedDouble, sizeof (PlsrDecodedDoubleU));
    d->tag = 0;
    d->man_sign = s;
    d->man_high = h;
    d->man_low  = l;
    d->exp = e;
    return (PlsrRef)d;
}


typedef struct {
    PlsrVTable vtable;
    uintp tag;
    sintp man;
    sintp exp;
} PlsrDecodedFloatU;
#define pLsrDecodedFloatPadding                \
    (sizeof(PlsrDecodedFloatU) - sizeof(PlsrVTable) - sizeof(uintp) - 2*sizeof(sintp))
typedef PlsrDecodedFloatU *PlsrDecodedFloatB;
pLsrVTableStatic (pLsrVTableDecodedFloat, "*decoded float*", pLsrDecodedFloatPadding);
PlsrRef pLsrPrimGHCDecodeFloatzh (float v)
{
    PlsrDecodedFloatB f;
    sintp m, e;
    __decodeFloat_Int (&m, &e, v);
    pLsrAlloc (PlsrDecodedFloatB, f, &pLsrVTableDecodedFloat, sizeof (PlsrDecodedFloatU));
    f->tag = 0;
    f->man = m;
    f->exp = e;
    return (PlsrRef)f;
}
#define pLsrDecodedDoubleSize (sizeof(PlsrDecodedDoubleU))
#define pLsrDecodedFloatSize (sizeof(PlsrDecodedFloatU))

/*** Initialisation ***/

static void pLsrPrimGHCRegisterVTables()
{
    static PgcIsRef ihrThreadRefs[ihrThreadSize/P_WORD_SIZE] = { 0, };
    static PgcIsRef ihrPairRefs[ihrPairSize/P_WORD_SIZE] = { 0, };
    static PgcIsRef pLsrDecodedDoubleRefs[pLsrDecodedDoubleSize/P_WORD_SIZE] = { 0, };
    static PgcIsRef pLsrDecodedFloatRefs[pLsrDecodedFloatSize/P_WORD_SIZE] = { 0, };

    pLsrVTableRegister(&ihrVTableThread, 4, ihrThreadSize, ihrThreadRefs, 0, 0, 0, PGC_ALWAYS_MUTABLE, 0);
    pLsrVTableRegister(&ihrVTablePair, 4, ihrPairSize, ihrPairRefs, 0, 0, 0, PGC_ALWAYS_IMMUTABLE, 0);
    pLsrVTableRegister(&pLsrVTableDecodedDouble, 4, pLsrDecodedDoubleSize, pLsrDecodedDoubleRefs,
                       0, 0, 0, PGC_ALWAYS_IMMUTABLE, 0);
    pLsrVTableRegister(&pLsrVTableDecodedFloat, 4, pLsrDecodedFloatSize, pLsrDecodedFloatRefs,
                       0, 0, 0, PGC_ALWAYS_IMMUTABLE, 0);
}

static void ihrInit()
{
    pLsrPrimGHCRegisterVTables();
    ihrTlsInit();
    ihrGlobalInit();
}

#endif // P_USE_PILLAR

#endif // !_PLSR_PRIMS_GHC_H_
