/* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

/* Finalizers */

#ifndef _PLSR_FINALIZER_H_
#define _PLSR_FINALIZER_H_

typedef void (*PlsrFinalizerCode)(PlsrRef);

#ifdef P_USE_PILLAR

#define pLsrFinalizerInitialSize 500
typedef struct PlsrFinalizerS {PlsrFinalizerCode f;volatile PlsrRef object;} PlsrFinalizerU, *PlsrFinalizer;

typedef struct PlsrFinalizerChunkS {
    volatile uintp count;
    uintp length;
    volatile struct PlsrFinalizerChunkS* next;
    volatile PlsrFinalizerU entries[];
} PlsrFinalizerChunkU, *PlsrFinalizerChunk;

typedef struct PlsrFinalizerGlobalsS {
    uintp busy;
    uintp shutdown;
    uintp done;
    uintp signaled;
    struct prtMutex *lock;
    struct prtCondition *notify;
    volatile PlsrFinalizerChunk inbox; 
    volatile PlsrFinalizerChunk todo; 
} PlsrFinalizerGlobalsU, *PlsrFinalizerGlobals;
    
static PlsrFinalizerGlobalsU pLsrFinalizerGlobals;

/* Finalizer addition */
#pragma pillar_managed(off)

/* Notify the finalizer thread of activity
* (shutdown or new work).  
*/
static void pLsrFinalizerNotify() 
{
    if (!pLsrFinalizerGlobals.signaled) {
        fflush(stdout);
        prtMutexLock(pLsrFinalizerGlobals.lock);
        fflush(stdout);
        pLsrFinalizerGlobals.signaled = 1;
        prtConditionSignal(pLsrFinalizerGlobals.notify);
        fflush(stdout);
        prtMutexUnlock(pLsrFinalizerGlobals.lock);
        fflush(stdout);
    }
}

/* Wait for notification of new work */
static void pLsrFinalizerWaitForNotification() 
{
    prtMutexLock(pLsrFinalizerGlobals.lock);
    if (!pLsrFinalizerGlobals.signaled) {
        prtConditionWait(pLsrFinalizerGlobals.notify, pLsrFinalizerGlobals.lock);
    }
    pLsrFinalizerGlobals.signaled = 0;
    prtMutexUnlock(pLsrFinalizerGlobals.lock);
}

static PlsrFinalizerChunk pLsrFinalizerTakeInbox() 
{
    return (PlsrFinalizerChunk) pLsrSynchAtomicTakeUIntp((volatile uintp*) &pLsrFinalizerGlobals.inbox, 1);
}

static void pLsrFinalizerPutInbox(PlsrFinalizerChunk inbox) 
{
    pLsrSynchAtomicPutUIntp((volatile uintp*) &pLsrFinalizerGlobals.inbox, (uintp) inbox);
    pLsrFinalizerNotify();
}

static PlsrFinalizerChunk pLsrFinalizerNewChunkUnmanaged(uintp length) 
{
    PlsrFinalizerChunk chunk = 
        (PlsrFinalizerChunk) pLsrAllocCUnmanaged(sizeof(PlsrFinalizerChunkU) + length*sizeof(PlsrFinalizerU));
    chunk->count=0;
    chunk->length=length;
    chunk->next=NULL;
    return chunk;
}

static void pLsrFinalizerAdd(PlsrFinalizerCode f, volatile PlsrRef object) 
{
#if (defined(PLSR_FINALIZER_TRACE) && (PLSR_FINALIZER_TRACE>0))
    printf("Adding finalizer object %p\n", object);
    fflush(stdout);
#endif
#ifdef PLSR_SINGLE_THREADED
    printf("Can't use finalizers with the single threaded runtime\n");
    fflush(stdout);
    assert(0);
    exit(-1);
#else
    PlsrFinalizerChunk inbox = pLsrFinalizerTakeInbox();
    if (inbox->count >= inbox->length) {
        uintp length = inbox->length;
        PlsrFinalizerChunk next = inbox;
        inbox = pLsrFinalizerNewChunkUnmanaged(length * 2);
        inbox->next = next;
    } 
    uintp count=inbox->count;
    inbox->entries[count].f = f;
    inbox->entries[count].object = object;
    inbox->count = count + 1;
    pLsrFinalizerPutInbox(inbox);
#endif
}

#pragma pillar_managed(on)

/* Root set enumeration */
#pragma pillar_managed(off)
/* These run on a pillar task, can yield */

static void pLsrFinalizerEnumerateChunk(PrtRseCallback rse, void* env, PlsrFinalizerChunk chunk)
{
    while (chunk != NULL) {
        for(int i=0; i<chunk->count;i++) {
#if (defined(PLSR_FINALIZER_TRACE) && (PLSR_FINALIZER_TRACE>0))
    printf("Enumerating finalizer object %p\n", chunk->entries[i].object);
    fflush(stdout);
#endif

            rse(env, (void**) &(chunk->entries[i].object), PrtGcTagDefault, 0);
        }
        chunk=(PlsrFinalizerChunk) chunk->next;
    }
}

static void pLsrFinalizerReportRoots(PrtRseCallback rse, void* env)
{
#ifdef PLSR_FINALIZER_TRACE
    printf("Finalizer enumeration starting\n");
    fflush(stdout);
#endif
    pLsrFinalizerEnumerateChunk(rse, env, pLsrFinalizerGlobals.inbox);
    pLsrFinalizerEnumerateChunk(rse, env, pLsrFinalizerGlobals.todo);  /* Finalizer thread is paused */
#ifdef PLSR_FINALIZER_TRACE
    printf("Finalizer enumeration finished\n");
    fflush(stdout);
#endif
}
#pragma pillar_managed(on)

static PlsrFinalizerChunk pLsrFinalizerNewChunk(uintp length) {
    PlsrFinalizerChunk chunk = 
        (PlsrFinalizerChunk) pLsrAllocC(sizeof(PlsrFinalizerChunkU) + length*sizeof(PlsrFinalizerU));
    chunk->count=0;
    chunk->length=length;
    chunk->next=NULL;
    return chunk;
}

static void pLsrFinalizerDeleteChunk(PlsrFinalizerChunk chunk) {
    pLsrFreeC(chunk);
}


/* We walk through the todo set, removing an element one
 * at a time.  All but the last (removed) element, remain
 * in the set to be enumerated by the root set enumerator.
 * The last (removed) element, is on the stack of this managed thread,
 * and hence will be enumerated by pillar
 */
static void pLsrFinalizerFinalizeTodo() 
{
#ifdef PLSR_FINALIZER_TRACE
    int processed = 0;
    printf("Finalizer finalize loop starting\n");
    fflush(stdout);
#endif
    pLsrFinalizerGlobals.busy = 1;
    /* Atomically */
    noyield {
        pLsrFinalizerGlobals.todo = pLsrFinalizerGlobals.inbox;
        int length = 
            (pLsrFinalizerInitialSize < pLsrFinalizerGlobals.inbox->length/2)
            ? pLsrFinalizerGlobals.inbox->length/2
            : pLsrFinalizerInitialSize;
        pLsrFinalizerGlobals.inbox = pLsrFinalizerNewChunk(length);
    }

    /* We keep a copy here.  GC may move the contents
    * of the fields, but nothing else modifies todo itself */
    PlsrFinalizerChunk todo = pLsrFinalizerGlobals.todo;
    while (todo != NULL) {
        while (todo->count > 0) {
            PlsrFinalizerCode f;
            PlsrRef object;
            noyield {
                int i = todo->count-1;
                f = todo->entries[i].f;
                object = todo->entries[i].object;
                todo->count = todo->count - 1;
            }

#if (defined(PLSR_FINALIZER_TRACE) && (PLSR_FINALIZER_TRACE>0))
            printf("Finalizer finalizing object %p\n", object);
            fflush(stdout);
#endif
#ifdef PLSR_FINALIZER_TRACE
            processed++;
#endif
            f(object);
        }
        PlsrFinalizerChunk next = (PlsrFinalizerChunk) todo->next;
        pLsrFinalizerGlobals.todo = next;
        pLsrFinalizerDeleteChunk(todo);
        todo=next;
    }
    pLsrFinalizerGlobals.busy = 0;
#ifdef PLSR_FINALIZER_TRACE
    printf("Finalizer finalize loop finished finalizing %d objects\n", processed);
    fflush(stdout);
#endif

}

static void pLsrFinalizerRun()
{
    pilYieldDec;

#ifdef PLSR_FINALIZER_TRACE
    printf("Starting finalizer thread\n");
    fflush(stdout);
#endif

    while (!pLsrFinalizerGlobals.shutdown) {
        assert(pLsrFinalizerGlobals.todo == NULL);
        if (pLsrFinalizerGlobals.inbox->count > 0) {
            pLsrFinalizerFinalizeTodo();
        } else {
            pLsrFinalizerWaitForNotification();
        }
        pilYield();
    }
#ifdef PLSR_FINALIZER_TRACE
    printf("Starting finalizer thread shutdown sequence\n");
    fflush(stdout);
#endif

#ifdef PLSR_FINALIZER_SHUTDOWN_LOOP
    pgc_force_gc();
    while (pLsrFinalizerGlobals.inbox->count > 0) {
#ifdef PLSR_FINALIZER_TRACE
    printf("Iterating finalizer thread shutdown sequence\n");
    fflush(stdout);
#endif
        pLsrFinalizerFinalizeTodo();
        pgc_force_gc();
        pilYield();
    } 
#endif

#ifdef PLSR_FINALIZER_TRACE
    printf("Completed finalizer thread shutdown sequence\n");
    fflush(stdout);
#endif

    pLsrFinalizerGlobals.done = 1;
}

static void pLsrFinalizerStart()
{
    pLsrFinalizerGlobals.shutdown = 0;
    pLsrFinalizerGlobals.done = 0;
    pLsrFinalizerGlobals.busy = 0;
    pLsrFinalizerGlobals.lock = prtMutexCreate(NULL);
    pLsrFinalizerGlobals.notify = prtConditionInit(NULL);
    pLsrFinalizerGlobals.signaled = 0;
    pLsrFinalizerGlobals.inbox = pLsrFinalizerNewChunk(pLsrFinalizerInitialSize);
    pLsrFinalizerGlobals.todo = NULL;

#ifndef PLSR_SINGLE_THREADED
    pcall pLsrFinalizerRun();
#endif
}

/* Shutdown the system.  If cleanup, then complete finalization
 * before exiting, otherwise just exit.
 */
static void pLsrFinalizerShutdown(PlsrBoolean cleanup)
{
#ifndef PLSR_SINGLE_THREADED
    if (cleanup) {
        pLsrFinalizerGlobals.shutdown = 1;
        pLsrFinalizerNotify();
        pLsrSynchYieldUntilEqualUIntp(&pLsrFinalizerGlobals.done, 1);
    }
#endif
}

static void pLsrFinalizerYieldToFinalizer() 
{
#ifndef PLSR_SINGLE_THREADED
    if (pLsrFinalizerGlobals.signaled) {
        pLsrSynchYieldUntilEqualUIntp(&pLsrFinalizerGlobals.signaled, 0);
    }
    if (pLsrFinalizerGlobals.busy) {
        pLsrSynchYieldUntilEqualUIntp(&pLsrFinalizerGlobals.busy, 0);
    }
#else
    if (pLsrFinalizerGlobals.inbox->count > 0) {
        pLsrFinalizerFinalizeTodo();
    }
#endif
}

static void pLsrFinalizerRegisterVTables()
{
}

static void pLsrFinalizerRegisterGlobals()
{
}

static void pLsrFinalizerCheckAssertions()
{
}
#else /* !P_USE_PILLAR */

static void pLsrFinalizerStart()
{
}

static void pLsrFinalizerShutdown(PlsrBoolean cleanup)
{
}

static void pLsrFinalizerRegisterVTables()
{
}

static void pLsrFinalizerRegisterGlobals()
{
}

static void pLsrFinalizerCheckAssertions()
{
}

#endif /* P_USE_PILLAR */

#endif /* !_PLSR_FINALIZER_H_ */
