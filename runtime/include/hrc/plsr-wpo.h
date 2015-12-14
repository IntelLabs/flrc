/* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

/* Weak Pointer Objects */

#ifndef _PLSR_WPO_H_
#define _PLSR_WPO_H_

/***********************************************************************************
 ************* Weak Pointer Object Interface ***************************************
 **********************************************************************************/

typedef PlsrRef PlsrWpo;

/* Create a new weak pointer object from key, value and optional finalizer
*  finalizer may be NULL.  Finalizer is assumed to be a PlsrThunkBRef
*/
static PlsrWpo pLsrWpoNew(PlsrRef key, PlsrRef value, PlsrRef finalizer);

/* Create a new weak pointer object from key, value and optional finalizer
*  finalizer may be NULL.  Finalizer is run using the provided code pointer.
*/
static PlsrWpo pLsrWpoNewWithRun(PlsrRef key, PlsrRef value, PlsrRef finalizer, PlsrFinalizerCode run);

/* Read the value (if present) of the weak pointer object.  Returns NULL
*  if not present.
*/
static PlsrRef pLsrWpoRead(PlsrWpo wpo);

/* Tombstone the weak pointer object and immediately run its finalizer.
*  Returns when the finalizer has completed.
*/
static void pLsrWpoFinalize(PlsrWpo wpo);

/* Registration code */
static void pLsrWpoRegisterVTables();
static void pLsrWpoRegisterGlobals();
static void pLsrWpoCheckAssertions();

/***********************************************************************************
 ************* Weak Pointer Object Implementation **********************************
 **********************************************************************************/


typedef struct PlsrWpoS {
    PlsrVTable vtable;
    PlsrRef key;
    volatile PlsrRef value;
    PlsrRef finalizer;
    PlsrFinalizerCode run;
} PlsrWpoU;

#define pLsrWpoPadding \
    (sizeof(PlsrWpoU) - sizeof(PlsrVTable) - 3*sizeof(PlsrRef) - sizeof(PlsrFinalizerCode))
pLsrVTableStatic(pLsrWpoVTable_, "*weak pointer object*", pLsrWpoPadding);
#define pLsrWpoVTable (&pLsrWpoVTable_)



#ifdef P_USE_PILLAR


static PlsrWpo pLsrWpoNewWithRun(PlsrRef key, PlsrRef value, PlsrRef finalizer, PlsrFinalizerCode run)
{
    PlsrRef res;
    pLsrAlloc_(PlsrRef, res, pLsrWpoVTable, sizeof(PlsrWpoU));
    ((PlsrWpoU*)res)->key=key;
    ((PlsrWpoU*)res)->value=value;
    ((PlsrWpoU*)res)->finalizer=finalizer;
    ((PlsrWpoU*)res)->run=run;
#ifdef PLSR_WPO_TRACE
    printf("Creating WPO object %p with finalizer object %p and value %p\n", res, finalizer, value);
    fflush(stdout);
#endif
    return res;
}

void pLsrWpoRunFinalizer(PlsrRef finalizer)
{
    pLsrThunkEvalRef(finalizer);
}

static PlsrWpo pLsrWpoNew(PlsrRef key, PlsrRef value, PlsrRef finalizer)
{
    return pLsrWpoNewWithRun(key, value, finalizer, pLsrWpoRunFinalizer);
}

static PlsrRef pLsrWpoRead(PlsrWpo wpo)
{
#ifdef PLSR_WPO_TRACE
    printf("Reading WPO object %p with value %p\n", wpo, ((PlsrWpoU*)wpo)->value);
    fflush(stdout);
#endif

    return ((PlsrWpoU*)wpo)->value;
}

static void pLsrWpoFinalize(PlsrWpo wpo)
{
    ((PlsrWpoU*)wpo)->value = NULL;
    if (((PlsrWpoU*)wpo)->finalizer) {
        ((PlsrWpoU*)wpo)->run(((PlsrWpoU*)wpo)->finalizer);
    }
}


#pragma pillar_managed(off)

static void pLsrWpoTombstone(void * wpo)
{
#ifdef PLSR_WPO_TRACE
    printf("Tombstoning WPO object %p with value %p\n", wpo, ((PlsrWpoU*)wpo)->value);
    fflush(stdout);
#endif
    assert(((PlsrWpoU*)wpo)->value);
    ((PlsrWpoU*)wpo)->value = NULL;
    if (((PlsrWpoU*)wpo)->finalizer) {
        pLsrFinalizerAdd(((PlsrWpoU*)wpo)->run, ((PlsrWpoU*)wpo)->finalizer);
    }
}

#pragma pillar_managed(on)

#define pLsrWpoSize (sizeof(PlsrWpoU))

static void pLsrWpoRegisterVTables()
{
    /* XXX Temporarily disable weak pointer semantics pending GC fixes. -leaf */
    static PgcIsRef pLsrWpoRefs[] = { 0, 1, 1, 1, 0 };
    static PgcIsRef pLsrWpoWRefs[] = { 0, 0, 0, 0, 0 };
    pLsrVTableRegisterV(pLsrWpoVTable, pLsrDefaultAlignment, pLsrWpoSize, pLsrWpoRefs,
                        0, 0, 0,
                        PGC_ALWAYS_MUTABLE, 0, pLsrWpoWRefs, NULL);
    pgc_set_wpo_vtable((struct VTable *) pLsrWpoVTable);
    pgc_set_wpo_finalizer(pLsrWpoTombstone);
}

static void pLsrWpoRegisterGlobals()
{
}

static void pLsrWpoCheckAssertions()
{
}

#else /* ! P_USE_PILLAR */

static PlsrWpo pLsrWpoNew(PlsrRef key, PlsrRef value, PlsrRef finalizer)
{
    pLsrRuntimeError("Weak pointer objects not supported");
    return 0;
}

static void pLsrWpoRegisterVTables()
{
}

static void pLsrWpoRegisterGlobals()
{
}

static void pLsrWpoCheckAssertions()
{
}

#endif /* P_USE_PILLAR */

#endif /* !_PLSR_WPO_H_ */
