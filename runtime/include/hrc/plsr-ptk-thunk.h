/* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

/* Unified Futures Thunk Implementation */

#ifndef _PLSR_THUNK_H_
#define _PLSR_THUNK_H_

#ifdef PLSR_THUNK_INTERCEPT_CUTS
  #ifndef P_USE_PILLAR
    #error "Cut interception only supported on Pillar"
  #endif
#endif

/*** Types ***/

typedef enum {
    PlsrThunkIndir = PtkFutureStatusUserDefined,
    PlsrThunkCut
} PlsrFutureStatus;

#ifdef PLSR_THUNK_INTERCEPT_CUTS
#define PlsrTypesMk(name, retType)                                      \
    typedef PlsrFutureStatus PlsrThunkReturnType##name;                 \
    typedef PlsrThunkReturnType##name (*PlsrThunkCode##name)(PlsrThunkB##name); \
    /* PlsrObjectU must be a prefix of this structure */                \
    typedef struct PlsrThunkS##name {                                   \
        PlsrVTable vtable;                                              \
        PtkFutureBuffer future;                                         \
        PilContinuation0 cutCont;                                       \
        retType result;                                                 \
        char fvs[];                                                     \
    } PlsrThunkU##name
#else /* !PLSR_THUNK_INTERCEPT_CUTS */
#define PlsrTypesMk(name, retType)                                      \
    typedef PlsrFutureStatus PlsrThunkReturnType##name;                 \
    typedef PlsrThunkReturnType##name (*PlsrThunkCode##name)(PlsrThunkB##name); \
    /* PlsrObjectU must be a prefix of this structure */                \
    typedef struct PlsrThunkS##name {                                   \
        PlsrVTable vtable;                                              \
        PtkFutureBuffer future;                                         \
        retType result;                                                 \
        char fvs[];                                                     \
    } PlsrThunkU##name
#endif /* !PLSR_THUNK_INTERCEPT_CUTS */

/*
 * PlsrThunkCodeRef, pLsrThunkSRef, pLsrThunkURef
 * PlsrThunkCode32, pLsrThunkS32, pLsrThunkU32
 * PlsrThunkCode64, pLsrThunkS64, pLsrThunkU64
 * PlsrThunkCodeFloat, pLsrThunkSFloat, pLsrThunkUFloat
 * PlsrThunkCodeDouble, pLsrThunkSDouble, pLsrThunkUDouble
 */
PlsrTypesMk(Ref, PlsrRef);
PlsrTypesMk(32, uint32);
PlsrTypesMk(64, uint64);
PlsrTypesMk(Float, float32);
PlsrTypesMk(Double, float64);

#define pLsrThunkRef(t) ((PlsrThunkURef*)(t))
#define pLsrThunk32(t)  ((PlsrThunkU32*)(t))
#define pLsrThunk64(t)  ((PlsrThunkU64*)(t))
#define pLsrThunkFloat(t)  ((PlsrThunkUFloat*)(t))
#define pLsrThunkDouble(t)  ((PlsrThunkUDouble*)(t))

#define pLsrThunkResultFieldRef(thnk)     (pLsrThunkRef(thnk)->result)
#define pLsrThunkResultField32(thnk)      (pLsrThunk32(thnk)->result)
#define pLsrThunkResultField64(thnk)      (pLsrThunk64(thnk)->result)
#define pLsrThunkResultFieldFloat(thnk)   (pLsrThunkFloat(thnk)->result)
#define pLsrThunkResultFieldDouble(thnk)  (pLsrThunkDouble(thnk)->result)

#ifdef PLSR_THUNK_SUBSUMPTION
#define pLsrThunkCastToObjectRef(thunk)     ((PlsrRef) thunk)
#else /* !PLSR_THUNK_SUBSUMPTION */
#define pLsrThunkCastToObjectRef(thunk)     ((PlsrRef) 0)
#endif /* !PLSR_THUNK_SUBSUMPTION */
#define pLsrThunkCastToObject32(thunk)      ((uint32) 0)
#define pLsrThunkCastToObject64(thunk)      ((uint64) 0)
#define pLsrThunkCastToObjectFloat(thunk)   ((float32) 0)
#define pLsrThunkCastToObjectDouble(thunk)  ((float64) 0)

/* Global Thunks */

  #define pLsrThunkStaticValueMk(name, v, vt, retType, val)               \
    static PlsrThunkU##name v =                                         \
        { .vtable = (vt),                                               \
          .future = ptkFutureStatic(PlsrThunkIndir, 0),                 \
          .result = (retType) (val) }

/* These are marked ALWAYS_IMMUTABLE, so must be initialized atomically wrt the gc */
#define pLsrThunkStaticValueRef(v, val)    pLsrThunkStaticValueMk(Ref,    v, pLsrThunkValVTableRef, PlsrRef, val)
#define pLsrThunkStaticValue32(v, val)     pLsrThunkStaticValueMk(32,     v, pLsrThunkValVTable32, uint32, val)
#define pLsrThunkStaticValue64(v, val)     pLsrThunkStaticValueMk(64,     v, pLsrThunkValVTable64, uint64, val)
#define pLsrThunkStaticValueFloat(v, val)  pLsrThunkStaticValueMk(Float,  v, pLsrThunkValVTableFloat, float32, val)
#define pLsrThunkStaticValueDouble(v, val) pLsrThunkStaticValueMk(Double, v, pLsrThunkValVTableDouble, float64, val)

/* Creation */

/* This cannot engender a yield */
#define pLsrThunkNewMk(name, dv, vt, sz, algn)                               \
    do {                                                                \
        pLsrAllocAligned(PlsrThunkB##name, (dv), (vt), (sz), (algn));   \
        assert((vt)->tag==VThunkTag);                                   \
    } while (0)
#ifdef PLSR_ZERO_REFS                                                   
#define pLsrThunkNewRef(dv, vt, sz, algn)      \
    do {                                       \
        pLsrThunkNewMk(Ref, dv, vt, sz, algn); \
        pLsrThunkResultFieldRef(dv) = NULL;    \
    } while (0) 
#else
#define pLsrThunkNewRef(dv, vt, sz, algn)    pLsrThunkNewMk(Ref, dv, vt, sz, algn)
#endif
#define pLsrThunkNew32(dv, vt, sz, algn)     pLsrThunkNewMk(32, dv, vt, sz, algn)
#define pLsrThunkNew64(dv, vt, sz, algn)     pLsrThunkNewMk(64, dv, vt, sz, algn)
#define pLsrThunkNewFloat(dv, vt, sz, algn)  pLsrThunkNewMk(Float, dv, vt, sz, algn)
#define pLsrThunkNewDouble(dv, vt, sz, algn) pLsrThunkNewMk(Double, dv, vt, sz, algn)

/* Initialisation */

#define pLsrThunkSetInitMk(name, thnk, code)                            \
    (ptkFutureInit((PtkRef)(thnk),                                      \
                   (unsigned)&pLsrThunk##name(0)->future,               \
                   (PtkFutureCodePointer)code))

#define pLsrThunkSetInitRef(thnk, code)    pLsrThunkSetInitMk(Ref, thnk, code)
#define pLsrThunkSetInit32(thnk, code)     pLsrThunkSetInitMk(32, thnk, code)
#define pLsrThunkSetInit64(thnk, code)     pLsrThunkSetInitMk(64, thnk, code)
#define pLsrThunkSetInitFloat(thnk, code)  pLsrThunkSetInitMk(Float, thnk, code)
#define pLsrThunkSetInitDouble(thnk, code) pLsrThunkSetInitMk(Double, thnk, code)

#define pLsrThunkSetValueRef(thnk, v)                                   \
    (pLsrWriteBarrierRefBase((PlsrThunkBRef)(thnk), pLsrThunkRef(thnk)->result, (v)), \
     ptkFutureSetStatus((PtkRef)(thnk),                          \
                        (unsigned)&pLsrThunkRef(0)->future,             \
                        (PtkFutureStatus)PlsrThunkIndir))
/* This should not engender a yield */
#define pLsrThunkSetValueNonWbMk(name, thnk, v)                         \
    (pLsrThunk##name(thnk)->result = (v),                               \
     ptkFutureSetStatus((PlsrThunkB##name)(thnk),                                    \
                        (unsigned)&pLsrThunk##name(0)->future,          \
                        (PtkFutureStatus)PlsrThunkIndir))

#define pLsrThunkSetValue32(thnk, v)     pLsrThunkSetValueNonWbMk(32, thnk, v)
#define pLsrThunkSetValue64(thnk, v)     pLsrThunkSetValueNonWbMk(64, thnk, v)
#define pLsrThunkSetValueFloat(thnk, v)  pLsrThunkSetValueNonWbMk(Float, thnk, v)
#define pLsrThunkSetValueDouble(thnk, v) pLsrThunkSetValueNonWbMk(Double, thnk, v)

#define pLsrThunkValueInitRef(thnk, v)                                  \
    do {                                                                \
        pLsrThunkSetValueRef(thnk, v);                                  \
        pLsrObjectChangeVTableMandatory(thnk, pLsrThunkValVTableRef);   \
    } while (0)

#define pLsrThunkValueInit32(thnk, v)                                  \
    do {                                                                \
        pLsrThunkSetValue32(thnk, v);                                  \
        pLsrObjectChangeVTableMandatory(thnk, pLsrThunkValVTable32);   \
    } while (0)

#define pLsrThunkValueInit64(thnk, v)                                  \
    do {                                                                \
        pLsrThunkSetValue64(thnk, v);                                  \
        pLsrObjectChangeVTableMandatory(thnk, pLsrThunkValVTable64);   \
    } while (0)

#define pLsrThunkValueInitFloat(thnk, v)                                  \
    do {                                                                \
        pLsrThunkSetValueFloat(thnk, v);                                  \
        pLsrObjectChangeVTableMandatory(thnk, pLsrThunkValVTableFloat); \
    } while (0)

#define pLsrThunkValueInitDouble(thnk, v)                                  \
    do {                                                                \
        pLsrThunkSetValueDouble(thnk, v);                                  \
        pLsrObjectChangeVTableMandatory(thnk, pLsrThunkValVTableDouble); \
    } while (0)

#define pLsrThunkNewValueMk(name, dv, vt, sz, algn, v)                        \
    do {                                                                \
        noyield {                                                       \
            pLsrThunkNew##name(dv, vt, sz, algn);                             \
            pLsrThunkSetValueNonWbMk(name, dv, v);                      \
        }                                                               \
    } while(0)

#define pLsrThunkNewValueRef(dv, vt, sz, algn, v)    pLsrThunkNewValueMk(Ref, dv, vt, sz, algn, ((PlsrRef) v))
#define pLsrThunkNewValue32(dv, vt, sz, algn, v)     pLsrThunkNewValueMk(32, dv, vt, sz, algn, v)
#define pLsrThunkNewValue64(dv, vt, sz, algn, v)     pLsrThunkNewValueMk(64, dv, vt, sz, algn, v)
#define pLsrThunkNewValueFloat(dv, vt, sz, algn, v)  pLsrThunkNewValueMk(Float, dv, vt, sz, algn, v)
#define pLsrThunkNewValueDouble(dv, vt, sz, algn, v) pLsrThunkNewValueMk(Double, dv, vt, sz, algn, v)

static PlsrThunkBRef pLsrThunkNewValRef(PlsrRef v)
{
    noyield {
        PlsrThunkBRef res;
        pLsrAllocAligned(PlsrThunkBRef, res, pLsrThunkValVTableRef, sizeof(PlsrThunkURef), sizeof(PlsrRef));
        pLsrThunkSetValueRef(res, v);
        return res;
    }
}

/* Projection */
#define pLsrThunkIsUnboxedRef(thnk)    (0)
#define pLsrThunkIsUnboxed32(thnk)     (0)
#define pLsrThunkIsUnboxed64(thnk)     (0)
#define pLsrThunkIsUnboxedFloat(thnk)  (0)
#define pLsrThunkIsUnboxedDouble(thnk) (0)

#define pLsrThunkStatusMk(name, thnk)                                    \
    (ptkFutureGetStatus((PlsrThunkB##name)(thnk), (unsigned)&pLsrThunk##name(0)->future))
#define pLsrThunkStatusRef(thnk)    pLsrThunkStatusMk(Ref, thnk)
#define pLsrThunkStatus32(thnk)     pLsrThunkStatusMk(32, thnk)
#define pLsrThunkStatus64(thnk)     pLsrThunkStatusMk(64, thnk)
#define pLsrThunkStatusFloat(thnk)  pLsrThunkStatusMk(Float, thnk)
#define pLsrThunkStatusDouble(thnk) pLsrThunkStatusMk(Double, thnk)

#define pLsrThunkIsEvaledRef(thnk)     (pLsrThunkStatusRef(thnk) == PlsrThunkIndir)
#define pLsrThunkIsEvaled32(thnk)      (pLsrThunkStatus32 (thnk) == PlsrThunkIndir)
#define pLsrThunkIsEvaled64(thnk)      (pLsrThunkStatus64 (thnk) == PlsrThunkIndir)
#define pLsrThunkIsEvaledFloat(thnk)   (pLsrThunkStatusFloat (thnk) == PlsrThunkIndir)
#define pLsrThunkIsEvaledDouble(thnk)  (pLsrThunkStatusDouble (thnk) == PlsrThunkIndir)

#define pLsrThunkGetValRef(thnk)     pLsrThunkResultFieldRef(thnk)
#define pLsrThunkGetVal32(thnk)      pLsrThunkResultField32(thnk)
#define pLsrThunkGetVal64(thnk)      pLsrThunkResultField64(thnk)
#define pLsrThunkGetValFloat(thnk)   pLsrThunkResultFieldFloat(thnk)
#define pLsrThunkGetValDouble(thnk)  pLsrThunkResultFieldDouble(thnk)


/* Evaluation */

static PlsrRef pLsrThunkEvalStatusErrorRef(PlsrFutureStatus status) 
{
    char msg[80];
    sprintf(msg, "Bad thunk status in Eval: %d", status);
    pLsrRuntimeError(msg);
    return 0;
}

static PlsrRef pLsrThunkEvalSlowRef(PlsrThunkBRef thunk)
{
    for(;;) {
        switch (pLsrThunkStatusRef(thunk)) {
        case PtkFutureStatusUninit:
            pLsrRuntimeError("Uninitialised thunk in Eval");
            return 0;
#ifdef P_USE_PARALLEL_FUTURES
        case PtkFutureStatusSpawning:
        case PtkFutureStatusSpawned:
#endif /* P_USE_PARALLEL_FUTURES */
        case PtkFutureStatusInit:
        case PtkFutureStatusStarted:
            ptkFutureWait((PtkRef)thunk,
                          (unsigned)&pLsrThunkRef(0)->future,
                          1);
#ifdef PLSR_THUNK_INTERCEPT_CUTS
            assert(pLsrThunkStatusRef(thunk) == PlsrThunkIndir ||
                   pLsrThunkStatusRef(thunk) == PlsrThunkCut);
#else /* !PLSR_THUNK_INTERCEPT_CUTS */
            assert(pLsrThunkStatusRef(thunk) == PlsrThunkIndir);
            return pLsrThunkGetValRef(thunk);
#endif /* !PLSR_THUNK_INTERCEPT_CUTS */
            break;
        case PlsrThunkIndir:
            return pLsrThunkGetValRef(thunk);
#ifdef PLSR_THUNK_INTERCEPT_CUTS
        case PlsrThunkCut:
            pilCutTo0(pLsrThunkRef(thunk)->cutCont);
            return 0;
#endif /* PLSR_THUNK_INTERCEPT_CUTS */
        default:
            return pLsrThunkEvalStatusErrorRef(pLsrThunkStatusRef(thunk));
        }
    }
}


#ifdef P_USE_PARALLEL_FUTURES
#ifdef PLSR_THUNK_INTERCEPT_CUTS
#define pLsrThunkEvalSlowNonRefMk(retType, name)                        \
                                                                        \
    static retType pLsrThunkEvalStatusError##name(PlsrFutureStatus status) \
    {                                                                   \
        char msg[80];                                                   \
        sprintf(msg, "Bad thunk status in Eval: %d", status);           \
        pLsrRuntimeError(msg);                                          \
        return 0;                                                       \
    }                                                                   \
                                                                        \
    static retType pLsrThunkEvalSlow##name(PlsrThunkB##name thunk)      \
    {                                                                   \
        for(;;) {                                                       \
            switch (pLsrThunkStatus##name(thunk)) {                     \
            case PtkFutureStatusUninit:                                 \
                pLsrRuntimeError("Uninitialised thunk in Eval");        \
                return 0;                                               \
            case PtkFutureStatusSpawning:                               \
            case PtkFutureStatusSpawned:                                \
            case PtkFutureStatusInit:                                   \
            case PtkFutureStatusStarted:                                \
                ptkFutureWait((PtkRef)thunk,                            \
                              (unsigned)&pLsrThunk##name(0)->future,    \
                              1);                                       \
                assert(pLsrThunkStatus##name(thunk) == PlsrThunkIndir || \
                       pLsrThunkStatus##name(thunk) == PlsrThunkCut);   \
                break;                                                  \
            case PlsrThunkIndir:                                        \
                return pLsrThunkGetVal##name(thunk);                    \
            case PlsrThunkCut:                                          \
                pilCutTo0(pLsrThunk##name(thunk)->cutCont);             \
                return 0;                                               \
            default:                                                    \
                return pLsrThunkEvalStatusError##name(pLsrThunkStatus##name(thunk)); \
            }                                                           \
        }                                                               \
    }
#else /* !PLSR_THUNK_INTERCEPT_CUTS */
#define pLsrThunkEvalSlowNonRefMk(retType, name)                            \
                                                                        \
    static retType pLsrThunkEvalStatusError##name(PlsrFutureStatus status) \
    {                                                                   \
        char msg[80];                                                   \
        sprintf(msg, "Bad thunk status in Eval: %d", status);           \
        pLsrRuntimeError(msg);                                          \
        return 0;                                                       \
    }                                                                   \
                                                                        \
    static retType pLsrThunkEvalSlow##name(PlsrThunkB##name thunk)          \
    {                                                                   \
        for(;;) {                                                       \
            switch (pLsrThunkStatus##name(thunk)) {                     \
            case PtkFutureStatusUninit:                                 \
                pLsrRuntimeError("Uninitialised thunk in Eval");        \
                return 0;                                               \
            case PtkFutureStatusSpawning:                               \
            case PtkFutureStatusSpawned:                                \
            case PtkFutureStatusInit:                                   \
            case PtkFutureStatusStarted:                                \
                ptkFutureWait((PtkRef)thunk,                               \
                              (unsigned)&pLsrThunk##name(0)->future,    \
                              1);                                       \
                assert(pLsrThunkStatus##name(thunk) == PlsrThunkIndir); \
                break;                                                  \
            case PlsrThunkIndir:                                        \
                return pLsrThunkGetVal##name(thunk);                    \
            default:                                                    \
                return pLsrThunkEvalStatusError##name(pLsrThunkStatus##name(thunk)); \
            }                                                           \
        }                                                               \
    }
#endif /* PLSR_THUNK_INTERCEPT_CUTS */
#else /* !P_USE_PARALLEL_FUTURES */
#define pLsrThunkEvalSlowNonRefMk(retType, name)                            \
                                                                        \
    static retType pLsrThunkEvalStatusError##name(PlsrFutureStatus status) \
    {                                                                   \
        char msg[80];                                                   \
        sprintf(msg, "Bad thunk status in Eval: %d", status);           \
        pLsrRuntimeError(msg);                                          \
        return 0;                                                       \
    }                                                                   \
                                                                        \
    static retType pLsrThunkEvalSlow##name(PlsrThunkB##name thunk)          \
    {                                                                   \
        for(;;) {                                                       \
            switch (pLsrThunkStatus##name(thunk)) {                     \
            case PtkFutureStatusUninit:                                 \
                pLsrRuntimeError("Uninitialised thunk in Eval");        \
                return 0;                                               \
            case PtkFutureStatusInit:                                   \
            case PtkFutureStatusStarted:                                \
                ptkFutureWait((PtkRef)thunk,                            \
                              (unsigned)&pLsrThunk##name(0)->future,    \
                              1);                                       \
                assert(pLsrThunkStatus##name(thunk) == PlsrThunkIndir); \
                return pLsrThunkGetVal##name(thunk);                    \
            case PlsrThunkIndir:                                        \
                return pLsrThunkGetVal##name(thunk);                    \
            default:                                                    \
                return pLsrThunkEvalStatusError##name(pLsrThunkStatus##name(thunk)); \
            }                                                           \
        }                                                               \
    }

#endif /* P_USE_PARALLEL_FUTURES */

/*
 * pLsrThunkEval32
 * pLsrThunkEval64
 * pLsrThunkEvalFloat
 * pLsrThunkEvalDouble
 */
pLsrThunkEvalSlowNonRefMk(uint32, 32);
pLsrThunkEvalSlowNonRefMk(uint64, 64);
pLsrThunkEvalSlowNonRefMk(float32, Float);
pLsrThunkEvalSlowNonRefMk(float64, Double);

#define pLsrThunkEvalRef(thunk)    pLsrThunkEvalSlowRef (thunk)
#define pLsrThunkEval32(thunk)     pLsrThunkEvalSlow32 (thunk)
#define pLsrThunkEval64(thunk)     pLsrThunkEvalSlow64 (thunk)
#define pLsrThunkEvalFloat(thunk)  pLsrThunkEvalSlowFloat (thunk)
#define pLsrThunkEvalDouble(thunk) pLsrThunkEvalSlowDouble (thunk)

#define pLsrThunkCallRef(thunk)    pLsrThunkEvalSlowRef (thunk)
#define pLsrThunkCall32(thunk)     pLsrThunkEvalSlow32 (thunk)
#define pLsrThunkCall64(thunk)     pLsrThunkEvalSlow64 (thunk)
#define pLsrThunkCallFloat(thunk)  pLsrThunkEvalSlowFloat (thunk)
#define pLsrThunkCallDouble(thunk) pLsrThunkEvalSlowDouble (thunk)

#define pLsrThunkEvalDirectRef(code, thunk)    pLsrThunkEvalSlowRef (thunk)
#define pLsrThunkEvalDirect32(code, thunk)     pLsrThunkEvalSlow32 (thunk)
#define pLsrThunkEvalDirect64(code, thunk)     pLsrThunkEvalSlow64 (thunk)
#define pLsrThunkEvalDirectFloat(code, thunk)  pLsrThunkEvalSlowFloat (thunk)
#define pLsrThunkEvalDirectDouble(code, thunk) pLsrThunkEvalSlowDouble (thunk)

#define pLsrThunkCallDirectRef(code, thunk)    pLsrThunkEvalDirectRef(code, thunk)
#define pLsrThunkCallDirect32(code, thunk)     pLsrThunkEvalDirect32 (code, thunk)
#define pLsrThunkCallDirect64(code, thunk)     pLsrThunkEvalDirect64 (code, thunk)
#define pLsrThunkCallDirectFloat(code, thunk)  pLsrThunkEvalDirectFloat (code, thunk)
#define pLsrThunkCallDirectDouble(code, thunk) pLsrThunkEvalDirectDouble (code, thunk)

#define pLsrThunkTailEvalRef(code, thunk)    TAILCALL(pLsrThunkEvalSlowRef (thunk))
#define pLsrThunkTailEval32(code, thunk)     TAILCALL(pLsrThunkEvalSlow32 (thunk))
#define pLsrThunkTailEval64(code, thunk)     TAILCALL(pLsrThunkEvalSlow64 (thunk))
#define pLsrThunkTailEvalFloat(code, thunk)  TAILCALL(pLsrThunkEvalSlowFloat (thunk))
#define pLsrThunkTailEvalDouble(code, thunk) TAILCALL(pLsrThunkEvalSlowDouble (thunk))

#define pLsrThunkTailCallRef(thunk)    TAILCALL(pLsrThunkEvalSlowRef (thunk))
#define pLsrThunkTailCall32(thunk)     TAILCALL(pLsrThunkEvalSlow32 (thunk))
#define pLsrThunkTailCall64(thunk)     TAILCALL(pLsrThunkEvalSlow64 (thunk))
#define pLsrThunkTailCallFloat(thunk)  TAILCALL(pLsrThunkEvalSlowFloat (thunk))
#define pLsrThunkTailCallDouble(thunk) TAILCALL(pLsrThunkEvalSlowDouble (thunk))

#define pLsrThunkTailEvalDirectRef(code, thunk)    TAILCALL(pLsrThunkEvalSlowRef (thunk))
#define pLsrThunkTailEvalDirect32(code, thunk)     TAILCALL(pLsrThunkEvalSlow32 (thunk))
#define pLsrThunkTailEvalDirect64(code, thunk)     TAILCALL(pLsrThunkEvalSlow64 (thunk))
#define pLsrThunkTailEvalDirectFloat(code, thunk)  TAILCALL(pLsrThunkEvalSlowFloat (thunk))
#define pLsrThunkTailEvalDirectDouble(code, thunk) TAILCALL(pLsrThunkEvalSlowDouble (thunk))

#define pLsrThunkTailCallDirectRef(code, thunk)    TAILCALL(pLsrThunkEvalDirectRef(code, thunk))
#define pLsrThunkTailCallDirect32(code, thunk)     TAILCALL(pLsrThunkEvalDirect32 (code, thunk))
#define pLsrThunkTailCallDirect64(code, thunk)     TAILCALL(pLsrThunkEvalDirect64 (code, thunk))
#define pLsrThunkTailCallDirectFloat(code, thunk)  TAILCALL(pLsrThunkEvalDirectFloat (code, thunk))
#define pLsrThunkTailCallDirectDouble(code, thunk) TAILCALL(pLsrThunkEvalDirectDouble (code, thunk))

#define pLsrThunkReturnRef(thnk, val)                                   \
    do {                                                                \
        pLsrWriteBarrierRefBase((thnk), pLsrThunkResultFieldRef(thnk), (val)); \
        return PlsrThunkIndir;                                          \
    } while (0)

#define pLsrThunkReturnNonRefMk(name, thnk, val)                        \
    do {                                                                \
        pLsrThunkResultField##name(thnk) = (val);                       \
        return PlsrThunkIndir;                                          \
    } while (0)

#define pLsrThunkReturn32(thnk, val)     pLsrThunkReturnNonRefMk(32, thnk, val)
#define pLsrThunkReturn64(thnk, val)     pLsrThunkReturnNonRefMk(64, thnk, val)
#define pLsrThunkReturnFloat(thnk, val)  pLsrThunkReturnNonRefMk(Float, thnk, val)
#define pLsrThunkReturnDouble(thnk, val) pLsrThunkReturnNonRefMk(Double, thnk, val)

#ifdef PLSR_THUNK_INTERCEPT_CUTS

#define pLsrThunkCutMk(name, thnk, cont)                                \
    do {                                                                \
        pLsrThunk##name(thnk)->cutCont = (cont);                        \
        return PlsrThunkCut;                                            \
        cut to cont;                                                    \
    } while (0)
#define pLsrThunkCutRef(thnk, cont)    pLsrThunkCutMk(Ref, thnk, cont)
#define pLsrThunkCut32(thnk, cont)     pLsrThunkCutMk(32, thnk, cont)
#define pLsrThunkCut64(thnk, cont)     pLsrThunkCutMk(64, thnk, cont)
#define pLsrThunkCutFloat(thnk, cont)  pLsrThunkCutMk(Float, thnk, cont)
#define pLsrThunkCutDouble(thnk, cont) pLsrThunkCutMk(Double, thnk, cont)

#endif /* PLSR_THUNK_INTERCEPT_CUTS */

/* Printing */

static void pLsrThunkPrintRef(PlsrThunkBRef t)
{
    pLsrValuePrint((PlsrObjectB)pLsrThunkEvalRef(t));
}

/* Spawning */

#ifdef P_USE_PARALLEL_FUTURES

#define pLsrThunkSpawnMk(name)                                          \
    static void pLsrThunkSpawn##name(PlsrThunkB##name thunk)            \
    {                                                                   \
        ptkFutureSpawn((PtkRef)thunk, (unsigned)&pLsrThunk##name(0)->future); \
    }

/*
 * pLsrThunkSpawnRef
 * pLsrThunkSpawn32
 * pLsrThunkSpawn64
 * pLsrThunkSpawnFloat
 * pLsrThunkSpawnDouble
 */
pLsrThunkSpawnMk(Ref);
pLsrThunkSpawnMk(32);
pLsrThunkSpawnMk(64);
pLsrThunkSpawnMk(Float);
pLsrThunkSpawnMk(Double);

#endif /* P_USE_PARALLEL_FUTURES */

#define pLsrThunkBlackHoleRef(thunk)
#define pLsrThunkBlackHole32(thunk)
#define pLsrThunkBlackHole64(thunk)
#define pLsrThunkBlackHoleFloat(thunk)
#define pLsrThunkBlackHoleDouble(thunk)

#define pLsrThunkClaimRef(thunk)
#define pLsrThunkClaim32(thunk)
#define pLsrThunkClaim64(thunk)
#define pLsrThunkClaimFloat(thunk)
#define pLsrThunkClaimDouble(thunk)


#define pLsrThunkZeroFV(zero) zero

/*** Check Object Model ***/

/* Generated code defines:
 *   pLsrThunkFixedSizeRef
 *   pLsrThunkFixedSize64
 *   pLsrThunkFixedSize32
 *   pLsrThunkFixedSizeFloat
 *   pLsrThunkFixedSizeDouble
 *   pLsrThunkResultOffsetRef
 *   pLsrThunkResultOffset64
 *   pLsrThunkResultOffset32
 *   pLsrThunkResultOffsetFloat
 *   pLsrThunkResultOffsetDouble
 */

static void pLsrThunkCheck()
{
    /*printf("Thunk check: %d/%d, %d/%d, %d/%d\n", pLsrThunkFixedSizeRef, 
           sizeof(PlsrThunkURef), pLsrThunkFixedSize32, sizeof(PlsrThunkU32),
           pLsrThunkFixedSize64, sizeof(PlsrThunkU64));*/
    if (pLsrThunkFixedSizeRef != sizeof(PlsrThunkURef))
        pLsrRuntimeError("Bad thunk object model!\n");
    if (pLsrThunkFixedSize32  != sizeof(PlsrThunkU32))
        pLsrRuntimeError("Bad thunk object model!\n");
    if (pLsrThunkFixedSize64  != sizeof(PlsrThunkU64))
        pLsrRuntimeError("Bad thunk object model!\n");
    if (pLsrThunkFixedSizeFloat  != sizeof(PlsrThunkUFloat))
        pLsrRuntimeError("Bad thunk object model!\n");
    if (pLsrThunkFixedSizeDouble  != sizeof(PlsrThunkUDouble))
        pLsrRuntimeError("Bad thunk object model!\n"); 

    if (pLsrThunkResultOffsetRef != ((unsigned)(&pLsrThunkResultFieldRef(0))))
        pLsrRuntimeError("Bad thunk object model!\n");
    if (pLsrThunkResultOffset32  != ((unsigned)(&pLsrThunkResultField32(0))))
        pLsrRuntimeError("Bad thunk object model!\n");
    if (pLsrThunkResultOffset64  != ((unsigned)(&pLsrThunkResultField64(0))))
        pLsrRuntimeError("Bad thunk object model!\n");
    if (pLsrThunkResultOffsetFloat  != ((unsigned)(&pLsrThunkResultFieldFloat(0))))
        pLsrRuntimeError("Bad thunk object model!\n");
    if (pLsrThunkResultOffsetDouble  != ((unsigned)(&pLsrThunkResultFieldDouble(0))))
        pLsrRuntimeError("Bad thunk object model!\n"); 
}

#endif /* !_PLSR_THUNK_H_ */
