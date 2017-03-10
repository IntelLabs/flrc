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
