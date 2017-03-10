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

#ifndef _PLSR_MAIN_H_
#define _PLSR_MAIN_H_

#ifdef P_USE_PILLAR
# define MAIN pillar_main
#else
# define MAIN main
#endif

/* C functions used for Runtime */
#ifdef P_USE_PILLAR
#  pragma pillar_managed(off)
#  define to __to__
#endif /* P_USE_PILLAR */

#include <locale.h>

#ifdef P_USE_PILLAR
#  undef to
#  pragma pillar_managed(on)
#endif /* P_USE_PILLAR */



static void pLsrRuntimeInitialize() 
{
    pLsrGCInitialize();
    pLsrNumericInitialize(pLsrGmpMemLimitParam);
}

static void pLsrRegisterRuntimeGlobals()
{
    pLsrGCRegisterGlobals();
    pLsrFinalizerRegisterGlobals();
    pLsrWpoRegisterGlobals();
    pLsrNumericRegisterGlobals();
    pLsrValueRegisterGlobals();
}

static void pLsrRegisterRuntimeVTables()
{
    pLsrGCRegisterVTables();
    pLsrFinalizerRegisterVTables();
    pLsrWpoRegisterVTables();
    pLsrNumericRegisterVTables();
    pLsrValueRegisterVTables();
}

static void pLsrCheckRuntimeAssertions()
{
    pLsrFinalizerCheckAssertions();
    pLsrWpoCheckAssertions();
    pLsrNumericCheckAssertions();
    pLsrObjectCheckModel();
    pLsrThunkCheck();
    pLsrValueCheck();
}

#ifdef P_USE_PILLAR
#pragma pillar_managed(off)
static void pLsrRuntimeReportRoots(PrtRseCallback rse, void* env) 
{
    pLsrFinalizerReportRoots(rse, env);
}
#pragma pillar_managed(on)
#endif

static void pLsrFuturesStart () 
{
    uintp sizeInBytes = pLsrStackSizeWorker * 1024 * 1024;
    uintp digitsB10 = (uintp) log10(sizeInBytes);
    char *fmt = "stacksize=%u";
    char *buf = pLsrAllocC(strlen(fmt) + digitsB10 + 1);
    if (!sprintf(buf, fmt, sizeInBytes)) {
        pLsrRuntimeError("Unable to set stack size");
    }
#ifdef P_USE_PARALLEL_FUTURES
    ptkFutureSystemSetOption(buf);
    ptkFutureSystemStart(0);
#endif
    pLsrFreeC(buf);
}

static void pLsrIHRInitialize() {
    if (pLsrIHRThreadCountParam > 0) ihrSetNCapabilities(pLsrIHRThreadCountParam);
}

static void __pmain();


static void pLsrRun()
{
    pLsrEventsTransition("Enter", "Main");
    __pmain();
    pLsrFinalizerShutdown (1);
    pLsrEventsTransition("Exit", "Main");
    fflush(stdout);

#ifdef PLSR_INSTRUMENT_ALLOCATION
    printf("plsr: Number objects       allocated: %I64u\n", pLsrNumObjectsAllocated);
    printf("plsr: Number bytes         allocated: %I64u\n", pLsrNumBytesAllocated);
    printf("plsr: Number padding bytes allocated: %I64u\n", pLsrNumPaddingBytesAllocated);
    printf("plsr: Number unmanaged objects allocated: %I64u\n", pLsrNumObjectsAllocatedUnmanaged);
    printf("plsr: Number unmanaged bytes   allocated: %I64u\n", pLsrNumBytesAllocatedUnmanaged);
    printf("plsr: Number unmanaged objects freed: %I64u\n", pLsrNumObjectsFreedUnmanaged);
#endif /* PLSR_INSTRUMENT_ALLOCATION */
#ifdef PLSR_INSTRUMENT_VTB_ALC
    {
        PlsrVTable cur = pLsrAllVTables;
        printf("plsr: vtable allocation stats:\n");
        while(cur) {
            printf("  %s (%p): Number objects       allocated: %I64u\n",
                   cur->name, cur, cur->numObjects);
            printf("  %s (%p): Number bytes         allocated: %I64u\n",
                   cur->name, cur, cur->numBytes);
            printf("  %s (%p): Number padding bytes allocated: %I64u\n",
                   cur->name, cur, cur->padding*cur->numObjects);
            cur = cur->next;
        }
    }
#endif /* PLSR_INSTRUMENT_VTB_ALC */

    pLsrEventsShutdown();
    pLsrExit(0);
    
    return;
}

int MAIN(int _argc, const char** _argv)
{
    int argc;
    const char** argv;

#ifdef P_USE_MCRT
    mcrtStart(main, _argc, _argv);
#endif
    setlocale(LC_CTYPE, "");
    pLsrEventsInit();
    pLsrEventsTransition("Enter", "Startup");
    pLsrDisableErrorBox();
    pilCheck();
    pLsrCheckRuntimeAssertions();
    pLsrParseOptions(_argc, _argv, &argc, &argv);
    pLsrGcInit(pLsrInitHeapParam, pLsrMaxHeapParam);
    pLsrRegisterRuntimeVTables();
    pLsrRegisterRuntimeGlobals();
    pLsrRuntimeInitialize();
    pLsrIHRInitialize();
    pLsrFuturesStart ();
    pLsrFinalizerStart ();
    pLsrEventsTransition("Exit", "Startup");
#ifdef __pillar2c__
    prtSetPcallStackSize(pLsrStackSizeMain * 1024 * 1024);
    pcall pLsrRun();
#else
    pLsrRun();
#endif
    return 0;
}

#endif /* !_PLSR_MAIN_H_ */
