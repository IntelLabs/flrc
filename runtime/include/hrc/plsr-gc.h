 /* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

/* Memory Allocation and Garbage Collection */

#ifndef _PLSR_GC_H_
#define _PLSR_GC_H_

#ifdef PLSR_LINUX
#define min(x,y) ((x)<(y)?(x):(y))
#endif

/**********************************************************************/
/*** Allocation instrumentation ***/

static uint64 pLsrNumObjectsAllocated = 0;
static uint64 pLsrNumBytesAllocated = 0;
static uint64 pLsrNumPaddingBytesAllocated = 0;

static uint64 pLsrNumObjectsAllocatedSinceLast = 0;
static uint64 pLsrNumBytesAllocatedSinceLast = 0;

static uint64 pLsrNumObjectsAllocatedUnmanaged = 0;
static uint64 pLsrNumBytesAllocatedUnmanaged = 0;

static uint64 pLsrNumObjectsFreedUnmanaged = 0;

static uint64 pLsrNumObjectsAllocatedUnmanagedSinceLast = 0;
static uint64 pLsrNumBytesAllocatedUnmanagedSinceLast = 0;

static uint64 pLsrNumObjectsFreedUnmanagedSinceLast = 0;

#ifdef PLSR_INSTRUMENT_ALLOCATION
#define pLsrAllocInstrument1(vtable, size)               \
    do {                                                 \
        pLsrNumObjectsAllocated++;                       \
        pLsrNumBytesAllocated += (size);                 \
        pLsrNumPaddingBytesAllocated += (vtable)->padding;      \
    } while (0)
#define pLsrAllocCInstrument1(size)                                     \
    do {                                                                \
        pLsrNumObjectsAllocatedUnmanaged++;                             \
        pLsrNumBytesAllocatedUnmanaged += (size);                       \
    } while (0);
#define pLsrFreeCInstrument1()                                          \
    do {                                                                \
        pLsrNumObjectsFreedUnmanaged++;                                 \
        pLsrNumObjectsFreedUnmanagedSinceLast++;                        \
    } while (0);
#else /* !PLSR_INSTRUMENT_ALLOCATION */
#define pLsrAllocInstrument1(vtable, size)
#define pLsrAllocCInstrument1(size)
#define pLsrFreeCInstrument1()
#endif /* !PLSR_INSTRUMENT_ALLOCATION */

#ifdef PLSR_INSTRUMENT_VTB_ALC
#define pLsrAllocInstrument2(vtable, size) \
    do {                                   \
        (vtable)->numObjects++;            \
        (vtable)->numBytes += (size);      \
    } while (0)
#else /* !PLSR_INSTRUMENT_VTB_ALC */
#define pLsrAllocInstrument2(vtable, size)
#endif /* !PLSR_INSTRUMENT_VTB_ALC */

#ifdef PLSR_INSTRUMENT_GCS
#define pLsrAllocInstrument3(size)                       \
    do {                                                 \
        pLsrNumObjectsAllocatedSinceLast++;              \
        pLsrNumBytesAllocatedSinceLast += (size);        \
    } while (0)
#define pLsrAllocCInstrument2(size)                               \
    do {                                                          \
        pLsrNumObjectsAllocatedUnmanagedSinceLast++;              \
        pLsrNumBytesAllocatedUnmanagedSinceLast += (size);        \
    } while (0);
#else /* !PLSR_INSTRUMENT_GCS */
#define pLsrAllocInstrument3(size)
#define pLsrAllocCInstrument2(size)
#endif /* !PLSR_INSTRUMENT_GCS */

#ifdef PLSR_FORCE_REGC
uint32 pLsrAllocREGC_signal = 0;
#define pLsrAllocREGC() do {if (pLsrAllocREGC_signal) {pgc_force_gc(); pLsrAllocREGC_signal = 0;}} while (0)
#define pLsrAllocREGCSet() (pLsrAllocREGC_signal = 1)
#else
#define pLsrAllocREGC()
#define pLsrAllocREGCSet()
#endif


/**********************************************************************/
/*** Heap Allocation ***/

#define pLsrDefaultAlignment 4

#define pLsrAllocStart() pLsrAllocREGC()

#define pLsrAllocFinish(r, vt, size)                    \
    do {                                                \
        pLsrAllocInstrument1(vt, size);                  \
        pLsrAllocInstrument2(vt, size);                 \
        pLsrAllocInstrument3(size);                     \
        ((PlsrObjectU*)(r))->vtable = vt;               \
    } while(0)

#define pLsrAllocInstrumentWrap(t, dest, vtable, size, pLsrAllocator)      \
    do {                                                                \
        pLsrAllocStart();                                               \
        (dest) = (t) pLsrAllocator(size, vtable);                       \
        pLsrAllocFinish(dest, vtable, size);                            \
    } while(0)


#ifdef P_USE_AGC
/* Wrap the unmanaged call in a managed function, and inline
 * the managed function call. This keeps the code size smaller.
 * alignment is based on the vtable properties */
PlsrObjectB pLsrAllocSlowCapacity(uintp size, PlsrVTable vtable) {
#ifdef __pillar2c__
    PlsrObjectB res;
    if (!(res = pgc_allocate_or_null(size, vtable))) {
        return pgc_allocate(size, vtable);
    }
    return res;
#else
    return pgc_allocate(size, vtable);
#endif
}
PlsrObjectB pLsrAllocSlow(uintp size, PlsrVTable vtable) {
    return pgc_allocate(size, vtable);
}

PlsrObjectB pLsrAllocSlowOrNull(uintp size, PlsrVTable vtable) {
    PlsrObjectB res;
    if (!(res = pgc_allocate_or_null(size, vtable))) {
        pLsrRuntimeError_ ("Allocate or null returned null");
    }
    return res;
}

#define BITS_PER_BYTE 8
#define NEXT_TO_HIGH_BIT_SET_MASK (1<<((sizeof(uintp) * BITS_PER_BYTE)-2))

/* Wrap the unmanaged call in a managed function, and inline
 * the managed function call. This keeps the code size smaller.
 * alignment is based on the vtable properties */
PlsrObjectB pLsrAllocSlowPinned(uintp size, PlsrVTable vtable) {
    if ((P_USE_AGC == PlsrAKMf))
        return pgc_allocate(size | NEXT_TO_HIGH_BIT_SET_MASK, vtable);
    else
        return pgc_allocate(size, vtable);
}

/* Accurate GC, pinned */
#define pLsrAllocPinned(t, dest, vtable, size) \
    pLsrAllocInstrumentWrap(t, dest, vtable, size, pLsrAllocSlowPinned)

#define pLsrAllocAlignedPinned(t, dest, vtable, size, alignment) \
    pLsrAllocPinned(t, dest, vtable, size)

#define pLsrAllocPinnedFinalizable pLsrAllocPinned

#define pLsrAllocFinalizable(t, dest, vtable, size)                 \
    pLsrAllocInstrumentWrap(t, dest, vtable, size, pLsrAllocSlow)

#ifdef P_USE_FAST_ALLOC

/* Accurate GC, fast path */
typedef struct {
    char *tls_current_free;
    char *tls_current_ceiling;
    void *chunk;
    void *curr_alloc_block;
} GC_Nursery_Info;

#ifdef __pillar2c__

extern unsigned g_tls_offset_bytes;
#define orp_local_to_gc_local(handle) (void *)((char*)(handle) + g_tls_offset_bytes)
#ifdef TLS0
#define pLsrGetAllocNursery() (GC_Nursery_Info *)orp_local_to_gc_local(*((void**)prtGetTaskHandle()))
#define pLsrGetAllocNurseryTh(task) (GC_Nursery_Info *)orp_local_to_gc_local(*((void**)task))
#else // TLS0
#define pLsrGetAllocNursery() (GC_Nursery_Info *)orp_local_to_gc_local(*((void**)((char*)prtGetTaskHandle() + P_WORD_SIZE)))
#define pLsrGetAllocNurseryTh(task) (GC_Nursery_Info *)orp_local_to_gc_local(*((void**)((char*)task + P_WORD_SIZE)))
#endif

#else // __pillar2c__
#define pLsrGetAllocNursery() (GC_Nursery_Info *) prtGetTls()
#endif

/* alignment is power of two */
#define pLsrAllocAligned(t, dest, vtable, size, alignment)              \
    do {                                                                \
        uintp pLsrAllocSize = ((size) + 3) & 0xFFffFFfc;                \
        uintp pLsrAllocAlignment = (alignment <=4) ? 0 : (alignment - 1); \
        GC_Nursery_Info *pLsrAllocNursery = pLsrGetAllocNursery();      \
        pLsrAllocStart();                                               \
        noyield {                                                       \
            char * pLsrAllocFrontier = pLsrAllocNursery->tls_current_free; \
            char * pLsrAllocLimit = pLsrAllocNursery->tls_current_ceiling; \
            pLsrAllocFrontier = (char *) ((((uintp) pLsrAllocFrontier) + pLsrAllocAlignment) & (~pLsrAllocAlignment)); \
            if (pLsrAllocFrontier + pLsrAllocSize <= pLsrAllocLimit) {  \
                (dest) = (t)pLsrAllocFrontier;                          \
                pLsrAllocNursery->tls_current_free = pLsrAllocFrontier + pLsrAllocSize; \
            } else {                                                    \
                (dest) = (t)pLsrAllocSlowCapacity(pLsrAllocSize, vtable);       \
            }                                                           \
        }                                                               \
        pLsrAllocFinish(dest, vtable, pLsrAllocSize);                   \
    } while(0)

#define pLsrAlloc(t, dest, vtable, size)  pLsrAllocAligned(t, dest, vtable, size, 1)

/* Accurate GC, skip local nursery */
#define pLsrAlloc_(t, dest, vtable, size)                           \
    pLsrAllocInstrumentWrap(t, dest, vtable, size, pLsrAllocSlow)

/* alignment is power of two */
#define pLsrAllocNoGC(t, dest, vtable, size)                            \
    do {                                                                \
        uintp pLsrAllocSize = ((size) + 3) & 0xFFffFFfc;                \
        GC_Nursery_Info *pLsrAllocNursery = pLsrGetAllocNursery();      \
        pLsrAllocStart();                                               \
        char * pLsrAllocFrontier = pLsrAllocNursery->tls_current_free;  \
        char * pLsrAllocLimit = pLsrAllocNursery->tls_current_ceiling;  \
        if (pLsrAllocFrontier + pLsrAllocSize <= pLsrAllocLimit) {      \
            (dest) = (t)pLsrAllocFrontier;                              \
            pLsrAllocNursery->tls_current_free = pLsrAllocFrontier + pLsrAllocSize; \
        } else { /* could fall back to allocate or null here */         \
            pLsrRuntimeErrorUnmanaged("Insufficient space for guaranteed allocation"); \
            (dest) = (t)(0);                                            \
        }                                                               \
        pLsrAllocFinish(dest, vtable, pLsrAllocSize);                   \
    } while(0)

#else /* !P_USE_FAST_ALLOC */

/* Accurate GC, slow path */
#define pLsrAllocAligned(t, dest, vtable, size, alignment)      \
    pLsrAllocInstrumentWrap(t, dest, vtable, size, pLsrAllocSlow)

#define pLsrAlloc(t, dest, vtable, size) pLsrAllocAligned(t, dest, vtable, size, 1)

#define pLsrAlloc_(t, dest, vtable, size) pLsrAlloc(t, dest, vtable, size)

#define pLsrAllocNoGC(t, dest, vtable, size)    \
    pLsrAllocInstrumentWrap(t, dest, vtable, size, pLsrAllocSlowOrNull)

#endif /* P_USE_FAST_ALLOC */
#else /* !P_USE_AGC */

/* Conservative GC*/
#ifdef P_USE_CGC
#define pLsrAlloc(t, dest, vtable, size)                 \
    do {                                                \
        pLsrAllocStart();                               \
        (dest) = (t) GC_MALLOC(size);                   \
        if (!(dest)) pLsrRuntimeError_("Out of memory"); \
        pLsrAllocFinish(dest, vtable, size);            \
    } while(0)

#define pLsrAllocPinned pLsrAlloc
#define pLsrAllocAligned(t, dest, vtable, size, algn) pLsrAlloc(t, dest, vtable, size)
#define pLsrAlloc_ pLsrAlloc
#define pLsrAllocPinnedFinalizable pLsrAlloc
#define pLsrAllocFinalizable pLsrAlloc

#else /* !P_USE_CGC */

/* NO GC */
#define pLsrAlloc(t, dest, vtable, size)                 \
    do {                                                \
        pLsrAllocStart();                               \
        dest = (t) malloc(size);                        \
        if (!(dest)) pLsrRuntimeError_("Out of memory"); \
        pLsrAllocFinish(dest, vtable, size);            \
    } while(0)

#define pLsrAllocPinned pLsrAlloc
#define pLsrAllocAligned(t, dest, vtable, size, algn) pLsrAlloc(t, dest, vtable, size)
#define pLsrAlloc_ pLsrAlloc
#define pLsrAllocPinnedFinalizable pLsrAlloc
#define pLsrAllocFinalizable pLsrAlloc

#endif /*P_USE_CGC */
#endif /*P_USE_AGC */

/**********************************************************************/
/*** C Allocation ***/


#ifdef P_USE_PILLAR
#  pragma pillar_managed(off)
#  define to __to__
#endif /* P_USE_PILLAR */

static void* pLsrAllocCUnmanaged(uintp size)
{
    void* res = malloc(size);
    pLsrAllocCInstrument1(size);
    pLsrAllocCInstrument2(size);
    if (res) return res;
    printf("Out of memory (C unmanaged alloc)");
    exit(-1);
    return 0;
}

#ifdef P_USE_PILLAR
#  undef to
#  pragma pillar_managed(on)
#endif


static void* pLsrAllocC(uintp size)
{
    void* res = malloc(size);
    pLsrAllocCInstrument1(size);
    pLsrAllocCInstrument2(size);
    if (res) return res;
    pLsrRuntimeError_("Out of memory (C alloc)");
    return 0;
}

static void* pLsrReAllocC(void * obj, uintp osize, uintp nsize)
{
    void* res = realloc(obj, nsize);
    pLsrFreeCInstrument1();
    pLsrAllocCInstrument1(nsize);
    pLsrAllocCInstrument2(nsize);
    if (res) return res;
    pLsrRuntimeError_("Out of memory (C realloc)");
    return 0;
}

static void pLsrFreeC(void* obj)
{
    pLsrFreeCInstrument1();
    free(obj);
}

/**********************************************************************/
/*** Options and Initialisation ***/

#ifdef P_USE_AGC
#pragma pillar_managed(off)
static void pLsrRuntimeReportRoots(PrtRseCallback, void*);
static void pLsrPPilerReportRoots(PrtRseCallback, void*);
static void pLsrGcReportRoots(PrtRseCallback rse, void* env)
{
#ifdef PLSR_INSTRUMENT_GCS
    printf("Total allocation: %I64u bytes (%I64u objects) managed, %I64u bytes (%I64u objects) unmanaged, %I64u objects freed\n",
           pLsrNumBytesAllocated, pLsrNumObjectsAllocated,
           pLsrNumBytesAllocatedUnmanaged, pLsrNumObjectsAllocatedUnmanaged,
           pLsrNumObjectsFreedUnmanaged);
    printf("Since last: %I64u bytes (%I64u objects) managed, %I64u bytes (%I64u objects) unmanaged, %I64u objects freed\n",
           pLsrNumBytesAllocatedSinceLast, pLsrNumObjectsAllocatedSinceLast,
           pLsrNumBytesAllocatedUnmanagedSinceLast, pLsrNumObjectsAllocatedUnmanagedSinceLast,
           pLsrNumObjectsFreedUnmanagedSinceLast);
    pLsrNumBytesAllocatedSinceLast = 0;
    pLsrNumObjectsAllocatedSinceLast = 0;
    pLsrNumBytesAllocatedUnmanagedSinceLast = 0;
    pLsrNumObjectsAllocatedUnmanagedSinceLast = 0;
    pLsrNumObjectsFreedUnmanagedSinceLast = 0;
#endif
    pLsrRuntimeReportRoots(rse, env);
    pLsrPPilerReportRoots(rse, env);
    pLsrAllocREGCSet();
}
#pragma pillar_managed(on)
#endif /* P_USE_AGC */

static void pLsrGcOption(const char* name, const char* arg)
{
#ifdef P_USE_AGC
    pgc_next_command_line_argument(name, arg);
#else /* !P_USE_AGC */
    pLsrRuntimeError("GC option not implemented");
#endif /* !P_USE_AGC */
}

static void pLsrGcInit(sintp initHeap, uintp maxHeap)
{
    char buf[100];
#ifdef P_USE_CGC
    if (initHeap<0) initHeap = 50;
    initHeap *= 1024 * 1024;
    /* GC Bug.  The GC tends to run out of memory when initHeap is set to maxHeap */
    initHeap -= initHeap / 10;
    maxHeap *= 1024 * 1024;
    GC_init();
    GC_expand_hp(initHeap);
    GC_set_max_heap_size (maxHeap);
#else /* !P_USE_CGC */
#ifdef P_USE_AGC
    if (P_USE_AGC == PlsrAKCgc || P_USE_AGC == PlsrAKTgc) {
        /* If only maxHeap set, prefer maxHeap */
        if (initHeap<=0 && maxHeap>0) {initHeap = maxHeap;}
        else if (initHeap>0 && maxHeap==0) {maxHeap = initHeap;}
        else if (initHeap >= 0 && maxHeap > 0 && initHeap != maxHeap)
            { pLsrRuntimeError("GCInit: initHeap <> maxHeap not supported on this GC");}
    }
    if (initHeap>0) {
        sprintf(buf, "-Xms%dm", initHeap);
        pgc_next_command_line_argument("-Xms", buf);
    }
    if (maxHeap>0) {
        sprintf(buf, "-Xmx%dm", maxHeap);
        pgc_next_command_line_argument("-Xmx", buf);
    }
    // lockParm = false for MF, true for V4
    pgc_init(pLsrGcReportRoots, P_AGC_LOCK_PARAM);
#endif /* P_USR_AGC */
#endif /* !P_USE_CGC */
}

/**********************************************************************/
/*** Vtable registration and Enumeration stuff ***/

static PlsrVTable pLsrAllVTables = NULL;

#ifdef P_USE_AGC

#define pLsrGcRegisterGlobals pgc_register_global_objects
#define pLsrGcRegisterGlobalRefs pgc_register_global_refs

/* vtable, object size (bytes), offset of the indirection slot (bytes) */
static void pLsrIndirectionVTableRegister(PlsrVTable vt, uintp size, uintp offset)
{
#ifdef PLSR_INSTRUMENT_VTB_ALC
    vt->next = pLsrAllVTables;
    pLsrAllVTables = vt;
#endif
    pgc_new_indirection_object((struct VTable*)vt, size, offset);
 };

static void pLsrVTableRegisterV(PlsrVTable vt, uintp alignment,
                                uintp fs, PgcIsRef frefs[],
                                uintp vs, uintp vlo,
                                PgcIsRef vrefs[],
                                enum PGC_MUTABILITY mutability,
                                uintp pinned,
                                PgcIsRef wrefs[],
                                void (* __cdecl finalizer)(Managed_Object_Handle))
{
    /* alignment is a power of two, so just count shifts, offset by two */
    uintp powerOfTwoBaseFour = 0;
    uintp adjusted = alignment >> 2;
    while ((adjusted >>=1) > 0) { powerOfTwoBaseFour++; }
    /* alignment requirement is 2^(2+powerOfTwoBaseFour).*/
    struct AlignmentInfo ai = {.alignArray = 0, .powerOfTwoBaseFour = powerOfTwoBaseFour};

#ifdef PLSR_INSTRUMENT_VTB_ALC
    vt->next = pLsrAllVTables;
    pLsrAllVTables = vt;
#endif
    pgc_new_object_format((struct VTable*)vt,
                          fs,
                          frefs,
                          vs,
                          vlo,
                          vrefs,
                          ai,
                          mutability,
                          pinned,
                          wrefs,
                          finalizer
                          );
}

/*
 * vt is the vtable
 * alignment is the required object alignment in bytes (must be power of two)
 * fs is the fixed size in bytes
 * frefs indicates for each word offset starting at zero (i.e. starting with the vtable) is it a ref
 * vs is the element size for the variable portion (in bytes)
 *   If vs=0, then there is no variable portion, and vlo and vref are ignored.
 * vlo is the offset from base of object of the length field (if applicable).
 * vref indicates the traceability of the first word of the elements in the variable portion
 * mutability indicates the mutability of objects allocated with this vtable
 * finalizer is an unmanged code pointer to be run on finalization (NULL if none)
 */
static void pLsrVTableRegisterFinalizable(PlsrVTable vt, uintp alignment,
                                          uintp fs, PgcIsRef frefs[],
                                          uintp vs, uintp vlo, PgcIsRef vref,
                                          enum PGC_MUTABILITY mutability,
                                          uintp pinned,
                                          void (* __cdecl finalizer)(Managed_Object_Handle)
                                          )
{
    PgcIsRef vrefs[2];
    vrefs[0] = vref;
    vrefs[1] = 0;
    PgcIsRef wrefs[fs/P_WORD_SIZE];
    for(int i = 0;i < fs/P_WORD_SIZE;i++) {
        wrefs[i]=0;
    }
    pLsrVTableRegisterV(vt, alignment, fs, frefs, vs, vlo, vrefs, mutability, pinned, wrefs, finalizer);
}

#define pLsrVTableRegister(vt, alignment, fcs, frefs, vs, vlo, vref, mutability, pinned) \
    pLsrVTableRegisterFinalizable(vt, alignment, fcs, frefs, vs, vlo, vref, mutability, pinned, NULL)

#else /* !P_USE_AGC */

enum PGC_MUTABILITY {
	PGC_ALWAYS_MUTABLE = 0,
	PGC_CREATED_MUTABLE = 1,
	PGC_ALWAYS_IMMUTABLE = 2
};

#define pLsrGcRegisterGlobals(gs, num)
#define pLsrGcRegisterGlobalRefs(gs, num)
#define pLsrIndirectionVTableRegister(vt, size, offset)
#define pLsrVTableRegister(vt, alignment, fs, frefs, vs, vlo, vref, m, pinned)
#define pLsrVTableRegisterFinalizable(vt, alignment, fs, frefs, vs, vlo, vref, m, pinned, finalizer)
#define pLsrVTableRegisterV(vt, alignment, fs, frefs, vs, vlo, vrefs, m, pinned, wrefs, finalizer)

#endif /* !P_USE_AGC */

/**********************************************************************/
/*** Barriers ***/

#ifdef P_USE_GC_WRITE_BARRIERS
#ifdef P_USE_AGC
#define pLsrWriteBarrierRef(t, s) \
    (pgc_write_ref_slot((struct Object**)&(t), (struct Object*)(s)))
#define pLsrWriteBarrierRefBase(b, t, s)                  \
    (pgc_write_ref_slot_with_base((struct Object*)(b),    \
                                  (struct Object**)&(t),  \
                                  (struct Object*)(s)))
#ifdef P_ALL_BARRIERS
#define pLsrWriteBarrierRefOpt(t, s) \
    (pgc_write_ref_slot((struct Object**)&(t), (struct Object*)(s)))
#define pLsrWriteBarrierRefOptBase(b, t, s)      \
    (pgc_write_ref_slot_with_base((struct Object*)(b),  \
                                  (struct Object**)&(t),        \
                                  (struct Object*)(s)))
#else /* !P_ALL_BARRIERS */
#define pLsrWriteBarrierRefOpt(t, s)                    \
    ((*((struct Object**)&(t)))=((struct Object*)(s)))
#define pLsrWriteBarrierRefOptBase(b, t, s)             \
    ((*((struct Object**)&(t)))=((struct Object*)(s)))
#endif /* !P_ALL_BARRIERS */
#else /* !P_USE_AGC */
#error "write barriers only work with agc"
#endif /* !P_USE_AGC */
#else /* !P_USE_GC_WRITE_BARRIERS */
#define pLsrWriteBarrierRef(t, s)                       \
    ((*((struct Object**)&(t)))=((struct Object*)(s)))
#define pLsrWriteBarrierRefBase(b, t, s)                \
    ((*((struct Object**)&(t)))=((struct Object*)(s)))
#define pLsrWriteBarrierRefOpt(t, s)                    \
    ((*((struct Object**)&(t)))=((struct Object*)(s)))
#define pLsrWriteBarrierRefOptBase(b, t, s)             \
    ((*((struct Object**)&(t)))=((struct Object*)(s)))
#endif /* !P_USE_GC_WRITE_BARRIERS */

/**********************************************************************/
/*** Changing VTables ***/

#ifdef P_USE_AGC

#define pLsrObjectChangeVTableMandatory(obj, vtb) \
    pgc_modify_object_vtable(((struct Object*)(obj)), ((struct VTable*)(vtb)))

/* XXX Extract this out to pgc.h  -leaf */
#define pLsrObjectCmpAndSetVTableMandatory(obj, vtbOld, vtbNew)        \
    pLsrSynchCmpAndSetUIntp(((volatile uintp*)(obj)), (uintp) vtbOld, (uintp) vtbNew)

#else /* !P_USE_AGC */

#define pLsrObjectChangeVTableMandatory(obj, vtb) (obj)->vtable = (vtb)

#define pLsrObjectCmpAndSetVTableMandatory(obj, vtbOld, vtbNew)        \
    pLsrSynchCmpAndSetUIntp(((struct Object*)(obj)), (uintp) vtbOld, (uintp) vtbNew)

#endif /* !P_USE_AGC */

#ifdef P_DO_VTABLE_CHANGE

#define pLsrObjectChangeVTable(obj, vtb) pLsrObjectChangeVTableMandatory(obj, vtb)

#else /* !P_DO_VTABLE_CHANGE */

#define pLsrObjectChangeVTable(obj, vtb)

#endif /* !P_DO_VTABLE_CHANGE */


/**********************************************************************/
/*** Malloc in the GC heap ***/

#ifdef P_USE_AGC

typedef struct PlsrGCHeapMallocObjS_ {
    PlsrVTable vtable;
    struct PlsrGCHeapMallocObjS_* prev;
    struct PlsrGCHeapMallocObjS_* next;
    uintp length;
    char bytes[];
} PlsrGCHeapMallocObjS;

#define pLsrGCHeapMallocObjPadding \
    (sizeof(PlsrGCHeapMallocObjS) - sizeof(PlsrVTable) - 2*sizeof(struct PlsrGCHeapMallocObjS_*) - sizeof(uintp))
pLsrVTableStatic(pLsrGCHeapMallocObjVTable_, "*heap malloc*", pLsrGCHeapMallocObjPadding);
#define pLsrGCHeapMallocObjVTable (&pLsrGCHeapMallocObjVTable_)

#define pLsrGCHeapMallocObjSize (sizeof(PlsrGCHeapMallocObjS))

static PlsrGCHeapMallocObjS pLsrGCHeapMallocObjectList_ =
    {.vtable = pLsrGCHeapMallocObjVTable,
     .prev = NULL,
     .next = NULL,
     .length = 0};
static PlsrGCHeapMallocObjS* pLsrGCHeapMallocObjList = &pLsrGCHeapMallocObjectList_;

static void pLsrGCHeapMallocObjListInsert(PlsrGCHeapMallocObjS* list, PlsrGCHeapMallocObjS* node)
{
    node->prev = list->prev;
    node->prev->next = node;
    node->next = list;
    list->prev = node;
}

static void pLsrGCHeapMallocObjListRemove(PlsrGCHeapMallocObjS* node)
{
    node->next->prev = node->prev;
    node->prev->next = node->next;
    node->prev = NULL;
    node->next = NULL;
}

#define pLsrGCHeapPtrToObject(ptr) ((PlsrGCHeapMallocObjS *) (((char*) ptr) - pLsrGCHeapMallocObjSize))

static void* pLsrGCHeapMalloc(uintp size)
{
    PlsrGCHeapMallocObjS *node;
    assert(pLsrGCHeapMallocObjSize == (uintp) &((PlsrGCHeapMallocObjS *)0)->bytes);
    pLsrAllocPinned(PlsrGCHeapMallocObjS*, node, pLsrGCHeapMallocObjVTable, pLsrGCHeapMallocObjSize + size);
    PlsrGCHeapMallocObjS *list = (PlsrGCHeapMallocObjS *) pLsrSynchAtomicTakeUIntp((volatile uintp*) &pLsrGCHeapMallocObjList, 1);
    pLsrGCHeapMallocObjListInsert(list, node);
    pLsrSynchAtomicPutUIntp((volatile uintp*) &pLsrGCHeapMallocObjList, (uintp) list);
    node->length=size;
    return &node->bytes;
}

static void pLsrGCHeapFree(void *ptr)
{

    PlsrGCHeapMallocObjS *node = pLsrGCHeapPtrToObject(ptr);
    PlsrGCHeapMallocObjS *list = (PlsrGCHeapMallocObjS *) pLsrSynchAtomicTakeUIntp((volatile uintp*) &pLsrGCHeapMallocObjList, 1);
    pLsrGCHeapMallocObjListRemove(node);
    pLsrSynchAtomicPutUIntp((volatile uintp*) &pLsrGCHeapMallocObjList, (uintp) list);
    return;
}

static void* pLsrGCHeapReAlloc(void * ptr, uintp osize, uintp nsize)
{
    PlsrGCHeapMallocObjS *old = pLsrGCHeapPtrToObject(ptr);
    assert(old->length >= osize);
    if (old->length >= nsize) {
        return ptr;
    } else {
        void *newPtr = pLsrGCHeapMalloc(nsize);
        uintp csize = min(nsize, osize);
        memcpy(newPtr, ptr, csize);
        pLsrGCHeapFree(ptr);
        return newPtr;
    }
}

static void pLsrGCHeapMallocRegisterVTables()
{
    static PgcIsRef refs[pLsrGCHeapMallocObjSize/P_WORD_SIZE] = { 0, 1, 1, 0};
    uintp pinned = 1;
    assert(pLsrGCHeapMallocObjSize/P_WORD_SIZE == 4);
    assert((uintp)&(((PlsrGCHeapMallocObjS*)(0))->length) == 3*P_WORD_SIZE);
    pLsrVTableRegister(pLsrGCHeapMallocObjVTable, pLsrDefaultAlignment, pLsrGCHeapMallocObjSize,
                       refs, 1, 3*P_WORD_SIZE, 0, PGC_ALWAYS_MUTABLE, pinned);
}

#define pLsrGCHeapMallocGlobalsCount 1

static PlsrObjectB pLsrGCHeapMallocGlobals[] =
    {
        (PlsrObjectB) (&pLsrGCHeapMallocObjectList_),
        (PlsrObjectB) NULL /* This must be last */
    };

static void pLsrGCHeapMallocRegisterGlobals()
{
    assert(pLsrGCHeapMallocGlobals[pLsrGCHeapMallocGlobalsCount] == NULL);
    pLsrGcRegisterGlobals (pLsrGCHeapMallocGlobals, pLsrGCHeapMallocGlobalsCount);
};


static void pLsrGCHeapMallocInitialize()
{
    pLsrGCHeapMallocObjList->prev=pLsrGCHeapMallocObjList;
    pLsrGCHeapMallocObjList->next=pLsrGCHeapMallocObjList;
}

#else /* !P_USE_AGC */

#define pLsrGCHeapMalloc pLsrAllocC
#define pLsrGCHeapReAlloc pLsrReAllocC
#define pLsrGCHeapFree pLsrFreeC

static void pLsrGCHeapMallocRegisterVTables() {}
static void pLsrGCHeapMallocRegisterGlobals() {}
static void pLsrGCHeapMallocInitialize() {}

#endif /* P_USE_AGC */

static void pLsrGCRegisterVTables()
{
    pLsrGCHeapMallocRegisterVTables();
}
static void pLsrGCRegisterGlobals()
{
    pLsrGCHeapMallocRegisterGlobals();
}
static void pLsrGCInitialize()
{
    pLsrGCHeapMallocInitialize();
}

#endif /* !_PLSR_GC_H_ */
