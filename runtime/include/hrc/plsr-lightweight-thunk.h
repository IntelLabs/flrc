/* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

/* Unified Futures Thunk Implementation */

#ifndef _PLSR_THUNK_H_
#define _PLSR_THUNK_H_

#ifndef PLSR_THUNK_NO_SUBSUMPTION
#define PLSR_THUNK_SUBSUMPTION
#endif

#ifdef P_USE_PARALLEL_FUTURES
#error "Parallel futures not supported with lightweight thunks"
#endif

#ifdef PLSR_THUNK_INTERCEPT_CUTS
#error "Cut interception not supported on lightweight thunks"
  #ifndef P_USE_PILLAR
    #error "Cut interception only supported on Pillar"
  #endif
#endif

/*** Types ***/

#define PlsrTypesMk(name, retType)                                      \
    typedef retType PlsrThunkReturnType##name;                          \
    typedef PlsrThunkReturnType##name (*PlsrThunkCode##name)(PlsrThunkB##name); \
    /* PlsrObjectU must be a prefix of this structure */                \
    typedef struct PlsrThunkSVS##name {                                 \
        PlsrVTable vtable;                                              \
        retType result;                                                 \
    } PlsrThunkSV##name;                                                \
    typedef struct PlsrThunkSDS##name {                                 \
        PlsrVTable vtable;                                              \
        char fvs[];                                                     \
    } PlsrThunkSD##name;                                                \
    typedef struct PlsrThunkSES##name {                                  \
        PlsrVTable vtable;                                              \
        PlsrRef exn;                                                    \
    } PlsrThunkSE##name;                                                \
    typedef union PlsrThunkUU##name {                                   \
        PlsrThunkSD##name delay;                                        \
        PlsrThunkSV##name value;                                        \
        PlsrThunkSE##name exn;                                          \
    } PlsrThunkU##name

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

#define pLsrThunkResultFieldRef(thunk)     (pLsrThunkRef(thunk)->value.result)
#define pLsrThunkResultField32(thunk)      (pLsrThunk32(thunk)->value.result)
#define pLsrThunkResultField64(thunk)      (pLsrThunk64(thunk)->value.result)
#define pLsrThunkResultFieldFloat(thunk)   (pLsrThunkFloat(thunk)->value.result)
#define pLsrThunkResultFieldDouble(thunk)  (pLsrThunkDouble(thunk)->value.result)

#define pLsrThunkExnFieldRef(thunk)     (pLsrThunkRef(thunk)->exn.exn)
#define pLsrThunkExnField32(thunk)      (pLsrThunk32(thunk)->exn.exn)
#define pLsrThunkExnField64(thunk)      (pLsrThunk64(thunk)->exn.exn)
#define pLsrThunkExnFieldFloat(thunk)   (pLsrThunkFloat(thunk)->exn.exn)
#define pLsrThunkExnFieldDouble(thunk)  (pLsrThunkDouble(thunk)->exn.exn)

#define pLsrThunkVTableFieldRef(thunk)     (pLsrThunkRef(thunk)->value.vtable)
#define pLsrThunkVTableField32(thunk)      (pLsrThunk32(thunk)->value.vtable)
#define pLsrThunkVTableField64(thunk)      (pLsrThunk64(thunk)->value.vtable)
#define pLsrThunkVTableFieldFloat(thunk)   (pLsrThunkFloat(thunk)->value.vtable)
#define pLsrThunkVTableFieldDouble(thunk)  (pLsrThunkDouble(thunk)->value.vtable)

#define pLsrThunkGetCodeRef(thunk)     ((PlsrThunkCodeRef)pLsrVTableGetCustom(pLsrThunkRef(thunk)->delay.vtable))
#define pLsrThunkGetCode32(thunk)      ((PlsrThunkCode32)pLsrVTableGetCustom(pLsrThunk32(thunk)->delay.vtable))
#define pLsrThunkGetCode64(thunk)      ((PlsrThunkCode64)pLsrVTableGetCustom(pLsrThunk64(thunk)->delay.vtable))
#define pLsrThunkGetCodeFloat(thunk)   ((PlsrThunkCodeFloat)pLsrVTableGetCustom(pLsrThunkFloat(thunk)->delay.vtable))
#define pLsrThunkGetCodeDouble(thunk)  ((PlsrThunkCodeDouble)pLsrVTableGetCustom(pLsrThunkDouble(thunk)->delay.vtable))

#ifdef PLSR_THUNK_SUBSUMPTION
#define pLsrThunkCastToObjectRef(thunk)     ((PlsrRef) thunk)
#else /* !PLSR_THUNK_SUBSUMPTION */
#define pLsrThunkCastToObjectRef(thunk)     ((PlsrRef) 0)
#endif /* !PLSR_THUNK_SUBSUMPTION */
#define pLsrThunkCastToObject32(thunk)      (assert(0), (uint32) 0)
#define pLsrThunkCastToObject64(thunk)      (assert(0), (uint64) 0)
#define pLsrThunkCastToObjectFloat(thunk)   (assert(0), (float32) 0)
#define pLsrThunkCastToObjectDouble(thunk)  (assert(0), (float64) 0)

/* Global Thunks */

#define pLsrThunkStaticValueMk(name, v, vt, retType, val)               \
    static PlsrThunkU##name v =                                         \
        { .value = {.vtable = (vt),                                     \
                    .result = (retType) (val) }                         \
        }

#ifdef PLSR_THUNK_SUBSUMPTION
/* For now leave globals boxed, since there are some mil-to-pil complications
* with doing otherwise -leaf */
  #define pLsrThunkSubsumptiveStaticValueMk pLsrThunkStaticValueMk
#else /* ! PLSR_THUNK_SUBSUMPTION */
  #define pLsrThunkSubsumptiveStaticValueMk pLsrThunkStaticValueMk
#endif /* ! PLSR_THUNK_SUBSUMPTION */

/* These are marked ALWAYS_IMMUTABLE, so must be initialized atomically wrt the gc */
#define pLsrThunkStaticValueRef(v, val)    \
    pLsrThunkSubsumptiveStaticValueMk(Ref,    v, pLsrThunkValVTableRef, PlsrRef, val)
#define pLsrThunkStaticValue32(v, val)     pLsrThunkStaticValueMk(32,     v, pLsrThunkValVTable32, uint32, val)
#define pLsrThunkStaticValue64(v, val)     pLsrThunkStaticValueMk(64,     v, pLsrThunkValVTable64, uint64, val)
#define pLsrThunkStaticValueFloat(v, val)  pLsrThunkStaticValueMk(Float,  v, pLsrThunkValVTableFloat, float32, val)
#define pLsrThunkStaticValueDouble(v, val) pLsrThunkStaticValueMk(Double, v, pLsrThunkValVTableDouble, float64, val)

/* Creation */
#define pLsrThunkUninitCodeMk(name)                                     \
    static PlsrThunkReturnType##name pLsrThunkUninitCode##name(PlsrThunkB##name thunk) \
    {                                                                   \
        pLsrRuntimeError_("Tried to evaluate uninitialized thunk");      \
        return (PlsrThunkReturnType##name) 0;                           \
    }
/* pLsrThunkUninitCodeRef, pLsrThunkUninitCode32, pLsrThunkUninitCode64
 * pLsrThunkUninitCodeFloat, pLsrThunkUninitCodeDouble
 */
pLsrThunkUninitCodeMk(Ref);
pLsrThunkUninitCodeMk(32);
pLsrThunkUninitCodeMk(64);
pLsrThunkUninitCodeMk(Float);
pLsrThunkUninitCodeMk(Double);

/* This cannot engender a yield */
#define pLsrThunkNewMk(name, dv, vt, sz, algn)                          \
    do {                                                                \
        pLsrAllocAligned(PlsrThunkB##name, (dv), (vt), (sz), (algn));   \
        assert((vt)->tag==VThunkTag);                                   \
    } while (0)

#ifdef PLSR_ZERO_REFS
#define pLsrThunkNewRef(dv, vt, sz, algn)                               \
    do {                                                                \
        pLsrThunkNewMk(Ref, dv, vt, sz, algn);                          \
        pLsrThunkResultFieldRef(dv) = NULL;                             \
        pLsrThunkExnFieldRef(dv) = NULL;                                \
    } while (0)
#define pLsrThunkNew32(dv, vt, sz, algn)                                \
    do {                                                                \
        pLsrThunkNewMk(32, dv, vt, sz, algn);                           \
        pLsrThunkExnField32(dv) = NULL;                                 \
    } while (0)
#define pLsrThunkNew64(dv, vt, sz, algn)                                \
    do {                                                                \
        pLsrThunkNewMk(64, dv, vt, sz, algn);                           \
        pLsrThunkExnField64(dv) = NULL;                                 \
    } while (0)
#define pLsrThunkNewFloat(dv, vt, sz, algn)                             \
    do {                                                                \
        pLsrThunkNewMk(Float, dv, vt, sz, algn);                        \
        pLsrThunkExnFieldFloat(dv) = NULL;                              \
    } while (0)
#define pLsrThunkNewDouble(dv, vt, sz, algn)                            \
    do {                                                                \
        pLsrThunkNewMk(Double, dv, vt, sz, algn);                       \
        pLsrThunkExnFieldDouble(dv) = NULL;                             \
    } while (0)
#else
#define pLsrThunkNewRef(dv, vt, sz, algn)    pLsrThunkNewMk(Ref, dv, vt, sz, algn)
#define pLsrThunkNew32(dv, vt, sz, algn)     pLsrThunkNewMk(32, dv, vt, sz, algn)
#define pLsrThunkNew64(dv, vt, sz, algn)     pLsrThunkNewMk(64, dv, vt, sz, algn)
#define pLsrThunkNewFloat(dv, vt, sz, algn)  pLsrThunkNewMk(Float, dv, vt, sz, algn)
#define pLsrThunkNewDouble(dv, vt, sz, algn) pLsrThunkNewMk(Double, dv, vt, sz, algn)
#endif

/* Initialisation */

#define pLsrThunkSetInitMk(name, thunk, vtable)    \
    pLsrObjectChangeVTableMandatory(thunk, vtable)

#define pLsrThunkSetInitRef(thunk, vtable)    pLsrThunkSetInitMk(Ref, thunk, vtable)
#define pLsrThunkSetInit32(thunk, vtable)     pLsrThunkSetInitMk(32, thunk, vtable)
#define pLsrThunkSetInit64(thunk, vtable)     pLsrThunkSetInitMk(64, thunk, vtable)
#define pLsrThunkSetInitFloat(thunk, vtable)  pLsrThunkSetInitMk(Float, thunk, vtable)
#define pLsrThunkSetInitDouble(thunk, vtable) pLsrThunkSetInitMk(Double, thunk, vtable)

#define pLsrThunkSetValueRef(thunk, v)                                   \
    (pLsrWriteBarrierRefBase((PlsrThunkBRef)(thunk), pLsrThunkResultFieldRef(thunk), (v)))
/* This should not engender a yield */
#define pLsrThunkSetValueNonWbMk(name, thunk, v)                         \
    (pLsrThunkResultField##name(thunk) = (v))

#define pLsrThunkSetValue32(thunk, v)     pLsrThunkSetValueNonWbMk(32, thunk, v)
#define pLsrThunkSetValue64(thunk, v)     pLsrThunkSetValueNonWbMk(64, thunk, v)
#define pLsrThunkSetValueFloat(thunk, v)  pLsrThunkSetValueNonWbMk(Float, thunk, v)
#define pLsrThunkSetValueDouble(thunk, v) pLsrThunkSetValueNonWbMk(Double, thunk, v)

#define pLsrThunkValueInitRef(thunk, v)                                 \
    do {                                                                \
        pLsrThunkSetValueRef(thunk, v);                                 \
        pLsrObjectChangeVTableMandatory(thunk, pLsrThunkValVTableRef);  \
    } while (0)

#define pLsrThunkValueInit32(thunk, v)                                  \
    do {                                                                \
        pLsrThunkSetValue32(thunk, v);                                  \
        pLsrObjectChangeVTableMandatory(thunk, pLsrThunkValVTable32);   \
    } while (0)

#define pLsrThunkValueInit64(thunk, v)                                  \
    do {                                                                \
        pLsrThunkSetValue64(thunk, v);                                  \
        pLsrObjectChangeVTableMandatory(thunk, pLsrThunkValVTable64);   \
    } while (0)

#define pLsrThunkValueInitFloat(thunk, v)                                  \
    do {                                                                \
        pLsrThunkSetValueFloat(thunk, v);                                  \
        pLsrObjectChangeVTableMandatory(thunk, pLsrThunkValVTableFloat); \
    } while (0)

#define pLsrThunkValueInitDouble(thunk, v)                                  \
    do {                                                                \
        pLsrThunkSetValueDouble(thunk, v);                                  \
        pLsrObjectChangeVTableMandatory(thunk, pLsrThunkValVTableDouble); \
    } while (0)

#define pLsrThunkNewValueMk(name, dv, vt, sz, algn, v)                   \
    do {                                                                \
        noyield {                                                       \
            pLsrThunkNew##name(dv, vt, sz, algn);                       \
            pLsrThunkSetValueNonWbMk(name, dv, v);                      \
        }                                                               \
    } while(0)

#ifdef PLSR_THUNK_SUBSUMPTION

#define pLsrThunkSubsumptiveNewValueMk(name, dv, vt, sz, algn, v) ((dv) = ((PlsrThunkBRef) (v)))

#else /* ! PLSR_THUNK_SUBSUMPTION */

#define pLsrThunkSubsumptiveNewValueMk pLsrThunkNewValueMk

#endif /* ! PLSR_THUNK_SUBSUMPTION */
#define pLsrThunkNewValueRef(dv, vt, sz, algn, v)    pLsrThunkSubsumptiveNewValueMk(Ref, dv, vt, sz, algn, ((PlsrRef) v))
#define pLsrThunkNewValue32(dv, vt, sz, algn, v)     pLsrThunkNewValueMk(32, dv, vt, sz, algn, v)
#define pLsrThunkNewValue64(dv, vt, sz, algn, v)     pLsrThunkNewValueMk(64, dv, vt, sz, algn, v)
#define pLsrThunkNewValueFloat(dv, vt, sz, algn, v)  pLsrThunkNewValueMk(Float, dv, vt, sz, algn, v)
#define pLsrThunkNewValueDouble(dv, vt, sz, algn, v) pLsrThunkNewValueMk(Double, dv, vt, sz, algn, v)

static PlsrThunkBRef pLsrThunkNewValRef(PlsrRef v)
{
#ifdef PLSR_THUNK_SUBSUMPTION
    return (PlsrThunkBRef) v;
#else
    noyield {
        PlsrThunkBRef res;
        pLsrAlloc(PlsrThunkBRef, res, pLsrThunkValVTableRef, sizeof(PlsrThunkURef));
        pLsrThunkSetValueRef(res, v);
        return res;
    }
#endif
}

/* Projection */
#ifdef PLSR_THUNK_SUBSUMPTION
  #define pLsrThunkIsUnboxedRef(thunk)                                    \
    (pLsrIntegerIsTagged(thunk) || (pLsrThunkVTableFieldRef(thunk)->tag != VThunkTag))
#else
  #define pLsrThunkIsUnboxedRef(thunk)     (assert(pLsrThunkVTableFieldRef(thunk)->tag == VThunkTag), 0)
#endif
#define pLsrThunkIsUnboxed32(thunk)      (assert(pLsrThunkVTableField32(thunk)->tag == VThunkTag), 0)
#define pLsrThunkIsUnboxed64(thunk)      (assert(pLsrThunkVTableField64(thunk)->tag == VThunkTag), 0)
#define pLsrThunkIsUnboxedFloat(thunk)   (assert(pLsrThunkVTableFieldFloat(thunk)->tag == VThunkTag), 0)
#define pLsrThunkIsUnboxedDouble(thunk)  (assert(pLsrThunkVTableFieldDouble(thunk)->tag == VThunkTag), 0)

#define pLsrThunkIsEvaledRef(thunk)     (pLsrThunkVTableFieldRef(thunk) == pLsrThunkValVTableRef)
#define pLsrThunkIsEvaled32(thunk)      (pLsrThunkVTableField32(thunk) == pLsrThunkValVTable32)
#define pLsrThunkIsEvaled64(thunk)      (pLsrThunkVTableField64(thunk) == pLsrThunkValVTable64)
#define pLsrThunkIsEvaledFloat(thunk)   (pLsrThunkVTableFieldFloat(thunk) == pLsrThunkValVTableFloat)
#define pLsrThunkIsEvaledDouble(thunk)  (pLsrThunkVTableFieldDouble(thunk) == pLsrThunkValVTableDouble)

#define pLsrThunkGetValRef(thunk)     pLsrThunkResultFieldRef(thunk)
#define pLsrThunkGetVal32(thunk)      pLsrThunkResultField32(thunk)
#define pLsrThunkGetVal64(thunk)      pLsrThunkResultField64(thunk)
#define pLsrThunkGetValFloat(thunk)   pLsrThunkResultFieldFloat(thunk)
#define pLsrThunkGetValDouble(thunk)  pLsrThunkResultFieldDouble(thunk)


/* Evaluation */

#define pLsrThunkCallMk(name, thunk) pLsrThunkGetCode##name(thunk)(thunk)
#define pLsrThunkTailCallMk(name, thunk) TAILCALL((pLsrThunkGetCode##name(thunk))(thunk))

#define pLsrThunkCallDirectMk(name, code, thunk)  code(thunk)
#define pLsrThunkTailCallDirectMk(name, code, thunk)  TAILCALL(code(thunk))

#define pLsrThunkCallDirectNoCutsMk(name, code, thunk)  code(thunk)
#define pLsrThunkTailCallDirectNoCutsMk(name, code, thunk)  TAILCALL(code(thunk))

#define pLsrThunkEvalMk(name, thunk)                                    \
    (pLsrThunkIsEvaled##name(thunk)) ? pLsrThunkGetVal##name(thunk) : pLsrThunkCall##name(thunk)
#define pLsrThunkEvalDirectMk(name, code, thunk)                        \
    (pLsrThunkIsEvaled##name(thunk)) ? pLsrThunkGetVal##name(thunk) : pLsrThunkCallDirect##name(code, thunk)
#define pLsrThunkTailEvalMk(name, thunk)                                \
    do {                                                                \
        if (pLsrThunkIsEvaled##name(thunk)) {                           \
            return pLsrThunkGetVal##name(thunk);                        \
        } else {                                                        \
            pLsrThunkTailCall##name(thunk);                             \
        }                                                               \
    } while (0)
#define pLsrThunkTailEvalDirectMk(name, code, thunk)                    \
    do {                                                                \
        if (pLsrThunkIsEvaled##name(thunk)) {                           \
            return pLsrThunkGetVal##name(thunk);                        \
        } else {                                                        \
            pLsrThunkTailCallDirect##name(code, thunk);                 \
        }                                                               \
    } while (0)

#ifdef PLSR_THUNK_SUBSUMPTION
#define pLsrThunkSubsumptiveEvalMk(name, thunk)                         \
    (pLsrThunkIsUnboxed##name(thunk)) ? pLsrThunkCastToObject##name(thunk) : pLsrThunkEvalMk(name, thunk)
#define pLsrThunkSubsumptiveEvalDirectMk(name, code, thunk)             \
    (pLsrThunkIsUnboxed##name(thunk)) ? pLsrThunkCastToObject##name(thunk) : pLsrThunkEvalDirectMk(name, code, thunk)
#define pLsrThunkSubsumptiveTailEvalMk(name, thunk)                     \
    do {                                                                \
        if (pLsrThunkIsUnboxed##name(thunk)) {                          \
            return pLsrThunkCastToObject##name(thunk);                  \
        } else {                                                        \
            pLsrThunkTailEvalMk(name, thunk);                            \
        }                                                               \
    } while (0)
#define pLsrThunkSubsumptiveTailEvalDirectMk(name, code, thunk)         \
    do {                                                                \
        if (pLsrThunkIsUnboxed##name(thunk)) {                          \
            return pLsrThunkCastToObject##name(thunk);                  \
        } else {                                                        \
            pLsrThunkTailEvalDirectMk(name, code, thunk);                \
        }                                                               \
    } while (0)
#else /* ! PLSR_THUNK_SUBSUMPTION */

#define pLsrThunkSubsumptiveEvalMk pLsrThunkEvalMk
#define pLsrThunkSubsumptiveEvalDirectMk pLsrThunkEvalDirectMk
#define pLsrThunkSubsumptiveTailEvalMk pLsrThunkTailEvalMk
#define pLsrThunkTailSubsumptiveEvalDirectMk pLsrThunkTailEvalDirectMk

#endif /* ! PLSR_THUNK_SUBSUMPTION */

#define pLsrThunkCallRef(thunk)    pLsrThunkCallMk (Ref,thunk)
#define pLsrThunkCall32(thunk)     pLsrThunkCallMk (32, thunk)
#define pLsrThunkCall64(thunk)     pLsrThunkCallMk (64, thunk)
#define pLsrThunkCallFloat(thunk)  pLsrThunkCallMk (Float, thunk)
#define pLsrThunkCallDouble(thunk) pLsrThunkCallMk (Double, thunk)

#define pLsrThunkCallDirectRef(code, thunk)    pLsrThunkCallDirectMk (Ref, code, thunk)
#define pLsrThunkCallDirect32(code, thunk)     pLsrThunkCallDirectMk (32, code, thunk)
#define pLsrThunkCallDirect64(code, thunk)     pLsrThunkCallDirectMk (64, code, thunk)
#define pLsrThunkCallDirectFloat(code, thunk)  pLsrThunkCallDirectMk (Float, code, thunk)
#define pLsrThunkCallDirectDouble(code, thunk) pLsrThunkCallDirectMk (Double, code, thunk)

#define pLsrThunkTailCallRef(thunk)    pLsrThunkTailCallMk (Ref,thunk)
#define pLsrThunkTailCall32(thunk)     pLsrThunkTailCallMk (32, thunk)
#define pLsrThunkTailCall64(thunk)     pLsrThunkTailCallMk (64, thunk)
#define pLsrThunkTailCallFloat(thunk)  pLsrThunkTailCallMk (Float, thunk)
#define pLsrThunkTailCallDouble(thunk) pLsrThunkTailCallMk (Double, thunk)

#define pLsrThunkTailCallDirectRef(code, thunk)    pLsrThunkTailCallDirectMk (Ref, code, thunk)
#define pLsrThunkTailCallDirect32(code, thunk)     pLsrThunkTailCallDirectMk (32, code, thunk)
#define pLsrThunkTailCallDirect64(code, thunk)     pLsrThunkTailCallDirectMk (64, code, thunk)
#define pLsrThunkTailCallDirectFloat(code, thunk)  pLsrThunkTailCallDirectMk (Float, code, thunk)
#define pLsrThunkTailCallDirectDouble(code, thunk) pLsrThunkTailCallDirectMk (Double, code, thunk)

#define pLsrThunkEvalRef(thunk)    pLsrThunkSubsumptiveEvalMk (Ref, thunk)
#define pLsrThunkEval32(thunk)     pLsrThunkEvalMk (32, thunk)
#define pLsrThunkEval64(thunk)     pLsrThunkEvalMk (64, thunk)
#define pLsrThunkEvalFloat(thunk)  pLsrThunkEvalMk (Float, thunk)
#define pLsrThunkEvalDouble(thunk) pLsrThunkEvalMk (Double, thunk)

#define pLsrThunkEvalDirectRef(code, thunk)    pLsrThunkSubsumptiveEvalDirectMk (Ref, code, thunk)
#define pLsrThunkEvalDirect32(code, thunk)     pLsrThunkEvalDirectMk (32, code, thunk)
#define pLsrThunkEvalDirect64(code, thunk)     pLsrThunkEvalDirectMk (64, code, thunk)
#define pLsrThunkEvalDirectFloat(code, thunk)  pLsrThunkEvalDirectMk (Float, code, thunk)
#define pLsrThunkEvalDirectDouble(code, thunk) pLsrThunkEvalDirectMk (Double, code, thunk)

#define pLsrThunkTailEvalRef(thunk)    pLsrThunkSubsumptiveTailEvalMk (Ref, thunk)
#define pLsrThunkTailEval32(thunk)     pLsrThunkTailEvalMk (32, thunk)
#define pLsrThunkTailEval64(thunk)     pLsrThunkTailEvalMk (64, thunk)
#define pLsrThunkTailEvalFloat(thunk)  pLsrThunkTailEvalMk (Float, thunk)
#define pLsrThunkTailEvalDouble(thunk) pLsrThunkTailEvalMk (Double, thunk)

#define pLsrThunkTailEvalDirectRef(code, thunk)    pLsrThunkSubsumptiveTailEvalDirectMk (Ref, code, thunk)
#define pLsrThunkTailEvalDirect32(code, thunk)     pLsrThunkTailEvalDirectMk (32, code, thunk)
#define pLsrThunkTailEvalDirect64(code, thunk)     pLsrThunkTailEvalDirectMk (64, code, thunk)
#define pLsrThunkTailEvalDirectFloat(code, thunk)  pLsrThunkTailEvalDirectMk (Float, code, thunk)
#define pLsrThunkTailEvalDirectDouble(code, thunk) pLsrThunkTailEvalDirectMk (Double, code, thunk)

#define pLsrThunkReturnRef(thnk, val)                                   \
     do {                                                               \
         pLsrThunkSetValueRef(thnk, val);                               \
         pLsrObjectChangeVTableMandatory(thnk, pLsrThunkValVTableRef); \
         return val;                                                    \
    } while (0)

#define pLsrThunkReturnNonRefMk(name, thnk, val)                        \
    do {                                                                \
        pLsrThunkSetValue##name(thnk, val);                             \
        pLsrObjectChangeVTableMandatory(thnk, pLsrThunkValVTable##name); \
        return val;                                                     \
    } while (0)

#define pLsrThunkReturn32(thunk, val)     pLsrThunkReturnNonRefMk(32, thunk, val)
#define pLsrThunkReturn64(thunk, val)     pLsrThunkReturnNonRefMk(64, thunk, val)
#define pLsrThunkReturnFloat(thunk, val)  pLsrThunkReturnNonRefMk(Float, thunk, val)
#define pLsrThunkReturnDouble(thunk, val) pLsrThunkReturnNonRefMk(Double, thunk, val)

PlsrThunkReturnTypeRef pLsrThunkDoCut(PlsrThunkBRef t) {
    pLsrRuntimeError_("Thunk recuts not supported with lightweight thunks");
    return 0;
#if 0
    PilContinuation0 c = (PilContinuation0) pLsrThunkRef(t)->result;
    pilCutTo0(c);
    return 0;
#endif
}

pLsrVTableStaticWithCustom(pLsrThunkCutVTable_, "*thunk value (cut)*", 0, pLsrThunkDoCut);
#define pLsrThunkCutVTable (&pLsrThunkCutVTable_)

#define pLsrThunkCutMk(name, thunk, cont)                                \
    do {                                                                \
        assert(sizeof(PlsrThunkU##name) >= sizeof(PlsrThunkURef));      \
        pLsrObjectChangeVTableMandatory(thunk, pLsrThunkCutVTable);      \
        /*        pLsrThunkRef(thunk)->exn = (PlsrRef) (exn);*/      \
        cut to cont;                                                    \
    } while (0)

#define pLsrThunkCutRef(thunk, cont)    pLsrThunkCutMk(Ref, thunk, cont)
#define pLsrThunkCut32(thunk, cont)     pLsrThunkCutMk(32, thunk, cont)
#define pLsrThunkCut64(thunk, cont)     pLsrThunkCutMk(64, thunk, cont)
#define pLsrThunkCutFloat(thunk, cont)  pLsrThunkCutMk(Float, thunk, cont)
#define pLsrThunkCutDouble(thunk, cont) pLsrThunkCutMk(Double, thunk, cont)

/* Printing */

static void pLsrThunkPrintRef(PlsrThunkBRef t)
{
    pLsrValuePrint((PlsrObjectB)pLsrThunkEvalRef(t));
}

/* VTables for black holed thunks */

pLsrVTableStatic(pLsrThunkEvalVTableRef_, "*evaled thunk (ref)*", pLsrThunkPaddingRef);
#define pLsrThunkEvalVTableRef (&pLsrThunkEvalVTableRef_)

pLsrVTableStatic(pLsrThunkEvalVTable32_, "*evaled thunk (32)*", pLsrThunkPadding32);
#define pLsrThunkEvalVTable32 (&pLsrThunkEvalVTable32_)

pLsrVTableStatic(pLsrThunkEvalVTable64_, "*evaled thunk (64)*", pLsrThunkPadding64);
#define pLsrThunkEvalVTable64 (&pLsrThunkEvalVTable64_)

pLsrVTableStatic(pLsrThunkEvalVTableFloat_, "*evaled thunk (float)*", pLsrThunkPaddingFloat);
#define pLsrThunkEvalVTableFloat (&pLsrThunkEvalVTableFloat_)

pLsrVTableStatic(pLsrThunkEvalVTableDouble_, "*evaled thunk (double)*", pLsrThunkPaddingDouble);
#define pLsrThunkEvalVTableDouble (&pLsrThunkEvalVTableDouble_)

#ifdef PLSR_THUNK_SYNCHRONIZE
#define pLsrThunkClaimDo(name, thunk)                                   \
    do {                                                                \
        PlsrVTable pLsrThunkClaimDoVT = pLsrThunkVTableField##name(thunk); \
        if (!(pLsrVTableGetCustom(pLsrThunkClaimDoVT) &&                \
              pLsrObjectCmpAndSetVTableMandatory(thunk, pLsrThunkClaimDoVT, pLsrThunkEvalVTable##name))) { \
            iFlcSynchWaitEqualVoidS(thunk, 0, (void*) pLsrThunkValVTable##name); \
            return pLsrThunkGetVal##name(thunk);                        \
        }                                                               \
    } while (0)
#else
#define pLsrThunkClaimDo(name, thunk)                                   \
    do {                                                                \
        if (pLsrThunkVTableField##name(thunk) == pLsrThunkEvalVTable##name) { \
            pLsrRuntimeError_("Black Hole");                            \
        }                                                               \
        pLsrObjectChangeVTableMandatory(thunk, pLsrThunkEvalVTable##name); \
    } while (0)
#endif

#define pLsrThunkClaimRef(thunk)    pLsrThunkClaimDo(Ref, thunk)
#define pLsrThunkClaim32(thunk)     pLsrThunkClaimDo(32, thunk)
#define pLsrThunkClaim64(thunk)     pLsrThunkClaimDo(64, thunk)
#define pLsrThunkClaimFloat(thunk)  pLsrThunkClaimDo(Float, thunk)
#define pLsrThunkClaimDouble(thunk) pLsrThunkClaimDo(Double, thunk)

#ifdef PLSR_THUNK_SYNCHRONIZE
#define pLsrThunkBlackHoleDo(name, thunk)                               \
do {                                                                    \
    iFlcSynchWaitEqualUIntp(thunk, 0, pLsrThunkValVTable##name);        \
    return pLsrThunkGetVal##name(thunk);                                \
 } while (0)
#else
#define pLsrThunkBlackHoleDo(name, thunk)                               \
    do {                                                                \
        pLsrRuntimeError_("Black Hole");                                \
    } while (0)
#endif
#define pLsrThunkBlackHoleRef(thunk)    pLsrThunkBlackHoleDo(Ref, thunk)
#define pLsrThunkBlackHole32(thunk)     pLsrThunkBlackHoleDo(32, thunk)
#define pLsrThunkBlackHole64(thunk)     pLsrThunkBlackHoleDo(64, thunk)
#define pLsrThunkBlackHoleFloat(thunk)  pLsrThunkBlackHoleDo(Float, thunk)
#define pLsrThunkBlackHoleDouble(thunk) pLsrThunkBlackHoleDo(Double, thunk)

#define pLsrThunkZeroFV(zero)


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
