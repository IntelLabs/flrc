/* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

/* Parse Commandline Parameters */

#ifndef _PLSR_PARAMS_H_
#define _PLSR_PARAMS_H_

typedef enum {
    PPBool,
    PPInt,
    PPGc,
    PPRT,
    PFutures,
#ifdef P_USE_PILLAR
    PPPGc,
#endif // P_USE_PILLAR
    PPEnd,
} PlsrPParamType;

typedef struct pLsrPParamEntryS {
    PlsrPParamType t;
    char* name; 
    int len;
    int* loc; 
    char* usage;
} PlsrPParamEntry;

static char pLsrPPStart[] = "@PPiler";
static uintp pLsrPPStartLen = 7;
static char pLsrPPEnd[] = "--";
static uintp pLsrPPEndLen = 2;

static uintp pLsrIHRThreadCountParam = 0;
static sintp pLsrInitHeapParam = -1;   /* MB */
static uintp pLsrMaxHeapParam = 0;     /* MB, 0 => unlimited */
static uintp pLsrStackSizeMain = PLSR_STACK_SIZE_MAIN;
static uintp pLsrStackSizeWorker = PLSR_STACK_SIZE_WORKER;
static uintp pLsrGmpMemLimitParam = 0;

static PlsrPParamEntry pLsroptions[] = {
    {PPInt,    "gmpMaxMem",       0, &pLsrGmpMemLimitParam,    "Memory limit for gmp integer allocation"},
    {PPInt,    "ihrThreads",      0, &pLsrIHRThreadCountParam, "How many haskell thread capabilities?"},
    {PPInt,    "initHeap",        0, &pLsrInitHeapParam,       "Initial heap size (megabytes)"},
    {PPInt,    "maxHeap",         0, &pLsrMaxHeapParam,        "Max heap size (megabytes), 0 => unlimited"},
    {PPInt,    "stackMain",       0, &pLsrStackSizeMain,       "Stack size for main threads (megabytes)"},
    {PPInt,    "stackWorker",     0, &pLsrStackSizeWorker,     "Stack size for worker threads (megabytes)"},
    {PPGc,     "gc",              0, NULL,                     "Pass option to gc"},
    {PPRT,     "prt",             0, NULL,                     "Pass option to prt"},
    {PFutures, "futures",         0, NULL,                     "Pass option to the futures package"},
#ifdef P_USE_PILLAR
    {PPPGc,    "pgc",             0, NULL,                     "Pass option to pgc"},
#endif // P_USR_PILLAR
    {PPEnd,    NULL,              0, NULL,                     NULL} /*Signal the end */
};

static void pLsrinitPParamTable()
{
    uintp i = 0;
    while(pLsroptions[i].name != NULL) {
        pLsroptions[i].len = strlen(pLsroptions[i].name);
        i++;
    }
    return;
}

static void pLsrdoPParamUsage(const char* argv[])
{
    uintp i = 0;
    fprintf(stderr, "usage: %s @PPiler [opt]* -- \n", argv[0]);
    fprintf(stderr, "  where opt is one of\n");
    
    while(pLsroptions[i].t != PPEnd) {
        switch(pLsroptions[i].t) {
        case PPInt:
            fprintf(stderr, 
                    "    %s n (default=%d)\n", 
                    pLsroptions[i].name,
                    *(pLsroptions[i].loc));
            break;
        case PPBool:
            fprintf(stderr, 
                    "    %s\n", 
                    pLsroptions[i].name);
            break;
        case PPGc:
            fprintf(stderr,
                    "    %s <string>\n",
                    pLsroptions[i].name);
            break;
        case PPRT:
            fprintf(stderr,
                    "    %s <string>\n",
                    pLsroptions[i].name);
            break;
        case PFutures:
            fprintf(stderr,
                    "    %s <string>\n",
                    pLsroptions[i].name);
            break;
#ifdef P_USE_PILLAR
        case PPPGc:
            fprintf(stderr,
                    "    %s <string>\n",
                    pLsroptions[i].name);
            break;
#endif // P_USR_PILLAR
        case PPEnd:
            assert(0);
            break;
        }
        i++;
    }
    pLsrRuntimeError("Bad @PPiler option usage");
}

static int pLsrdoPParamInt(int* res, int idx, int argc, const char* argv[])
{
    if (idx >= argc) 
        pLsrdoPParamUsage(argv);
    if (sscanf(argv[idx], "%i", res) <= 0)
        pLsrdoPParamUsage(argv);
    return idx+1;
}
    
static int pLsrdoPParamGc(int idx, int argc, const char* argv[])
{
    if (idx >= argc) 
        pLsrdoPParamUsage(argv);
    if (strcmp(argv[idx], "verbose") == 0)
        pLsrGcOption("-verbosegc", "");
    else
        pLsrGcOption("-gc", argv[idx]);
    return idx+1;
}

static int pLsrdoPParam(int idx, int argc, const char* argv[])
{
    void __cdecl prtSetOption(const char *optionString);
    uintp j;
    for(j=0; pLsroptions[j].t != PPEnd; j++) {
        /* XXX NG: this is not strictly correct */
        if (0 == strncmp(argv[idx], pLsroptions[j].name, pLsroptions[j].len)) {
            idx++;
            switch (pLsroptions[j].t) {
            case PPInt:
                idx = pLsrdoPParamInt(pLsroptions[j].loc, idx, argc, argv);
                break;
            case PPBool:
                *(pLsroptions[j].loc) = 1;
                break;
            case PPGc:
                idx = pLsrdoPParamGc(idx, argc, argv);
                break;
            case PPRT:
                if (idx >= argc)
                    pLsrdoPParamUsage(argv);
#ifdef P_USE_PILLAR
                prtSetOption(argv[idx]);
#else /* !P_USE_PILLAR */
                fprintf(stderr, 
                        "prt option: %s ignored (not using pillar)\n",
                        argv[idx]);
#endif
                idx++;
                break;
            case PFutures:
                if (idx >= argc)
                    pLsrdoPParamUsage(argv);
                ptkFutureSystemSetOption(argv[idx]);
                idx++;
                break;
#ifdef P_USE_PILLAR
            case PPPGc:
                if (idx >= argc)
                    pLsrdoPParamUsage(argv);
                pgcSetOption(argv[idx]);
                idx++;
                break;
#endif // P_USR_PILLAR
            case PPEnd:
                assert(0);
                /* Impossible */
                break;
            }
            return idx;
        }
    }
    /* If we get here, it was an unknown param */
    pLsrdoPParamUsage(argv);
    return 0;    
}

static int pLsrdoPParamList(int idx, int argc, const char* argv[])
{
    while(idx < argc) {
        if (0 == strncmp(argv[idx], pLsrPPEnd, pLsrPPEndLen)) {
            return idx+1;
        }
        idx = pLsrdoPParam(idx, argc, argv);
    }
    /* If we get here, then there was no PPEnd symbol */
    pLsrdoPParamUsage(argv);
    return 0;
}

static int pLsrdoPParamSegment(int idx, int argc, const char* argv[])
{
    if (idx < argc) {
        if (0 == strncmp(argv[idx], pLsrPPStart, pLsrPPStartLen)) {
            return pLsrdoPParamList(idx+1, argc, argv);
        }
        else {
            return idx;
        }
    }
    return idx;
}

static void pLsrParseOptions(int _argc, const char* _argv[],
                             int* argc, const char*** argv)
{
    /* For now, assume that all the PPiler opts are first.  
     * Later we can generalize 
     */
    int idx;
    int count;
    pLsrinitPParamTable();
    idx = pLsrdoPParamSegment(1, _argc, _argv);
    count = idx -1;
    *argc = pargc = _argc - count;  /* consumed args */
    *argv = pargv = _argv + count;  /* Advance to last consumed arg */
    *argv[0] = _argv[0];    /* Keep command name around in place of last consumed */
}

#endif /* !_PLSR_PARAMS_H_ */
