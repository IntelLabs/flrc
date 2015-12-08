/* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

/* Various P Values */

#ifndef _PLSR_VALUE_H_
#define _PLSR_VALUE_H_


/**********************************************************************
 * Option Sets and Types
 */

/* Option sets based on this vtable must always be initialised
 * immediately after allocation without an intervening GC.  If this is
 * not the case, the mutability property of the vtable must be
 * changed.
 */
pLsrVTableStatic(pLsrPSetVTable_, VSetTag, "*option set*", pLsrPSetPadding);
#define pLsrPSetVTable (&pLsrPSetVTable_)

/* Generated code defines:
 *   pLsrPSetSize
 *   pLsrPSetOffset
 *   pLsrPTypeSize
 */

static PlsrPAny pLsrPSetNew(PlsrPAny v)
{
    PlsrPAny s;
    noyield {
        pLsrAlloc(PlsrPAny, s, pLsrPSetVTable, pLsrPSetSize);
        pLsrWriteBarrierRefOptBase
            (s, pLsrObjectField(s, pLsrPSetOffset, PlsrPAny*), v);
    }
    return s;
}

#define pLsrPSetGet(s) (pLsrObjectField(s, pLsrPSetOffset, PlsrPAny*))
#define pLsrPSetIsEmpty(s) ((PlsrBoolean)(!pLsrPSetGet(s)))

/* Types based on this vtable must always be initialised
 * immediately after allocation without an intervening GC.  If this is
 * not the case, the mutability property of the vtable must be
 * changed.
 */
pLsrVTableStatic(pLsrPTypeVTable_, VTypeTag, "*type*", pLsrPTypePadding);
#define pLsrPTypeVTable (&pLsrPTypeVTable_)

/**********************************************************************
 * Rationals
 */

/* Boxed rationals based on this vtable must always be initialised
 * immediately after allocation without an intervening GC.  If this is
 * not the case, the mutability property of the vtable must be
 * changed.
 */
pLsrVTableStatic(pLsrPRatVTable_, VRatTag, "*rat*", pLsrPRatPadding);
#define pLsrPRatVTable (&pLsrPRatVTable_)

/* Generated code defines:
 *   pLsrPRatSize
 *   pLsrPRatOffset
 */

#ifdef P_PRAT_IS_SINTP 

#define pLsrPRatContainsRef 0

#define pLsrPRatUnimp(s, dest)                                          \
    do {                                                                \
        pLsrRuntimeError("PRat unimplemented");                         \
        (dest) = 0;                                                     \
    } while (0)

#define pLsrSIntpFromPRat(pLsrSIntpFromPRatDest, pLsrSIntpFromPRatArg1) \
    do {                                                                \
        (pLsrSIntpFromPRatDest) =                                       \
            ((sintp)pLsrObjectField(pLsrSIntpFromPRatArg1, pLsrPRatOffset, sintp*)); \
    } while (0)

#define pLsrPRatFromRational(pLsrPRatFromRationalDest, pLsrPRatFromRationalArg1) \
    pLsrPRatUnimp("pLsrPRatFromRational", pLsrPRatFromRationalDest)

#define pLsrRationalFromPRat(pLsrRationalFromPRatDest, pLsrRationalFromPRatArg1)\
    do {                                                                \
        sintp pLsrRationalFromPRatI;                                    \
        pLsrSIntpFromPRat(pLsrRationalFromPRatI, pLsrRationalFromPRatArg1); \
        pLsrRationalFromSIntp(pLsrRationalFromPRatDest, pLsrRationalFromPRatI); \
    } while (0)

/* Conversions */

#define pLsrPRatFromUInt32(pLsrPRatFromUInt32Dest, pLsrPRatFromUInt32Arg1) \
    do {                                                                \
        noyield {                                                       \
            pLsrAlloc(PlsrPAny, (pLsrPRatFromUInt32Dest), pLsrPRatVTable, pLsrPRatSize); \
            pLsrObjectField(pLsrPRatFromUInt32Dest, pLsrPRatOffset, uintp*) = ((uintp) pLsrPRatFromUInt32Arg1); \
        }                                                               \
    } while (0)

#define pLsrPRatFromInteger(pLsrPRatFromIntegerDest, pLsrPRatFromIntegerArg1) \
    pLsrPRatUnimp("pLsrPRatFromInteger", pLsrPRatFromIntegerDest)

#define pLsrUInt32FromPRat(pLsrUInt32FromPRatDest, pLsrUInt32FromPRatArg1) \
    do {                                                                \
        sintp pLsrUInt32FromPRatI;                                      \
        pLsrSIntpFromPRat(pLsrUInt32FromPRatI, pLsrUInt32FromPRatArg1); \
        if (pLsrUInt32FromPRatI < 0 || pLsrUInt32FromPRatI > UINT32_MAX) { \
            pLsrRuntimeError_("pLsrUInt32FromPRat: not in range");       \
        }                                                               \
        pLsrUInt32FromPRatDest = (uint32) pLsrUInt32FromPRatI;          \
    } while (0)

#define pLsrUIntpFromPRat(pLsrUIntpFromPRatDest, pLsrUIntpFromPRatArg1) \
    pLsrPRatUnimp("pLsrUIntpFromPRat", pLsrUIntpFromPRatDest)


#else /* !P_PRAT_IS_SINTP */

#define pLsrPRatContainsRef 1

#define pLsrPRatFromRational(pLsrPRatFromRationalDest, pLsrPRatFromRationalArg1) \
    do {                                                                \
        noyield {                                                       \
            pLsrAlloc(PlsrPAny, (pLsrPRatFromRationalDest), pLsrPRatVTable, pLsrPRatSize); \
            pLsrWriteBarrierRefOptBase                                  \
                (pLsrPRatFromRationalDest,                              \
                 pLsrObjectField(pLsrPRatFromRationalDest, pLsrPRatOffset, PlsrRational*), \
                 pLsrPRatFromRationalArg1);                             \
        }                                                               \
    } while (0)

#define pLsrRationalFromPRat(pLsrRationalFromPRatDest, pLsrRationalFromPRatArg1)\
    do {                                                                \
        (pLsrRationalFromPRatDest) = \
            ((PlsrRational)pLsrObjectField(pLsrRationalFromPRatArg1, pLsrPRatOffset, PlsrRational*)); \
    } while (0)

/* Conversions */

#define pLsrPRatFromUInt32(pLsrPRatFromUInt32Dest, pLsrPRatFromUInt32Arg1) \
    do {                                                                \
        PlsrRational pLsrPRatFromUInt32R;                               \
        pLsrRationalFromUInt32(pLsrPRatFromUInt32R, pLsrPRatFromUInt32Arg1); \
        pLsrPRatFromRational(pLsrPRatFromUInt32Dest, pLsrPRatFromUInt32R); \
    } while (0)

#define pLsrPRatFromInteger(pLsrPRatFromIntegerDest, pLsrPRatFromIntegerArg1) \
    do {                                                                \
        PlsrRational pLsrPRatFromIntegerR;                              \
        pLsrRationalFromInteger(pLsrPRatFromIntegerR, pLsrPRatFromIntegerArg1); \
        pLsrPRatFromRational(pLsrPRatFromIntegerDest, pLsrPRatFromIntegerR); \
    } while (0)

#define pLsrUInt32FromPRat(pLsrUInt32FromPRatDest, pLsrUInt32FromPRatArg1)\
    do {                                                                \
        PlsrRational pLsrUInt32FromPRatR;                               \
        pLsrRationalFromPRat(pLsrUInt32FromPRatR, pLsrUInt32FromPRatArg1); \
        pLsrUInt32FromRational(pLsrUInt32FromPRatDest, pLsrUInt32FromPRatR); \
    } while (0)

#define pLsrUIntpFromPRat(pLsrUIntpFromPRatDest, pLsrUIntpFromPRatArg1) \
    do {                                                                \
        PlsrRational pLsrUIntpFromPRatR;                                \
        pLsrRationalFromPRat(pLsrUIntpFromPRatR, pLsrUIntpFromPRatArg1); \
        pLsrUIntpFromRational(pLsrUIntpFromPRatDest, pLsrUIntpFromPRatR); \
    } while (0)

#endif /* P_PRAT_IS_SINTP */
/**********************************************************************
 * Names
 */

pLsrVTableStatic(pLsrPNameVTable_, VNameTag, "*name*", 0);
#define pLsrPNameVTable (&pLsrPNameVTable_)

/* PlsrObjectU must be a prefix of this structure */
/* This structure must always be initialised immediately after allocation
 * without an intervening GC.  If this is not the case, the mutability
 * property of the vtable must be changed.
 */
typedef struct {
    PlsrVTable vtable;
    uintp tag;
    uintp hash;
    uintp strLen;
    char str[];
} PlsrPNameU;

#define pLsrPNameStatic(v, n, h, sl, ...)        \
    static PlsrPNameU v = {                      \
        .vtable = pLsrPNameVTable,               \
        .tag = (n),                              \
        .hash = (h),                             \
        .strLen = (sl),                          \
        .str = { __VA_ARGS__ }                   \
    }

#define pLsrPNameGetTag(n) (((PlsrPNameU*)(n))->tag)
#define pLsrPNameGetHash(n) (((PlsrPNameU*)(n))->hash)
#define pLsrPNameGetStringLen(n) (((PlsrPNameU*)(n))->strLen)
#define pLsrPNameGetString(n) (((PlsrPNameU*)(n))->str)

/**********************************************************************
 * Floating Point
 */

/* Generated code defines:
 *  pLsrPFloatOffset
 *  pLsrPFloatSize
 *  pLsrPDoubleOffset
 *  pLsrPDoubleSize
 */

/* Boxed floats based on this vtable must always be initialised
 * immediately after allocation without an intervening GC.  If this is
 * not the case, the mutability property of the vtable must be
 * changed.
 */
pLsrVTableStatic(pLsrPFloatVTable_, VFloatTag, "*float*", pLsrPFloatPadding);
#define pLsrPFloatVTable (&pLsrPFloatVTable_)

/* Boxed doubles based on this vtable must always be initialised
 * immediately after allocation without an intervening GC.  If this is
 * not the case, the mutability property of the vtable must be
 * changed.
 */
pLsrVTableStatic(pLsrPDoubleVTable_, VDoubleTag, "*double*", pLsrPDoublePadding);
#define pLsrPDoubleVTable (&pLsrPDoubleVTable_)

#define pLsrPFloatGet(v) (pLsrObjectField((v), pLsrPFloatOffset, float*))
#define pLsrPDoubleGet(v) (pLsrObjectField((v), pLsrPDoubleOffset, double*))


/**********************************************************************
 * Indexes
 */

typedef struct {
    PlsrPAny name;
    uintp offset;
} PlsrIdxE;

/* PlsrObjectU must be a prefix of this structure */
typedef struct {
    PlsrVTable vtable;
    uintp len;
    PlsrIdxE elts[];
} PlsrIdxU;

#define pLsrIdxPadding (sizeof(PlsrIdxU) - (sizeof(PlsrVTable) + sizeof(uintp)))
pLsrVTableStatic(pLsrIdxVTable_, VNoneTag, "*index*", pLsrIdxPadding);
#define pLsrIdxVTable (&pLsrIdxVTable_)

typedef PlsrIdxU* PlsrIdxB;

#define pLsrIdxEltStatic {0, 0}

#define pLsrIdxEmpty(v) \
    static PlsrIdxU v = \
        { .vtable = pLsrIdxVTable, .len = 0, .elts = { pLsrIdxEltStatic } }

#define pLsrIdxStatic(v, dlen, ... ) \
    static PlsrIdxU v = \
        { .vtable = pLsrIdxVTable, .len = (dlen), .elts = { __VA_ARGS__ } }

static PlsrIdxB pLsrIdxNew(uintp len)
{
    uintp dlen = 1;
    uintp i;
    PlsrIdxB dct;
    uintp size;

    /* Choose a power of two at least 1.5 times as large as len*/
    while(2*dlen < 3*len) dlen *= 2;
    size = sizeof(PlsrIdxU) + dlen*sizeof(PlsrIdxE);
    pLsrAlloc(PlsrIdxB, dct, pLsrIdxVTable, size);
    dct->len = dlen;
    for(i=0;i<dlen;i++) dct->elts[i].name=0;
    return dct;
}

static void pLsrIdxSet(PlsrIdxB idx, PlsrPAny n, uintp e)
{
    uintp mask = idx->len-1;
    PlsrIdxE* elts = idx->elts;
    uintp j;
    uintp hash;
    if (!n) pLsrRuntimeError("Name out of range");
    hash  = pLsrPNameGetHash(n);
    for(j = hash & mask; elts[j].name; j = (j+1) & mask);
    pLsrWriteBarrierRefBase(idx, elts[j].name, n);
    elts[j].offset = e;
}

static uintp pLsrIdxGet(PlsrIdxB idx, PlsrPAny n)
{
    uintp mask = idx->len-1;
    PlsrIdxE* elts = idx->elts;
    uintp hash = pLsrPNameGetHash(n);
    uintp j;
    for(j = hash & mask; elts[j].name != n; j = (j+1) & mask);
    return elts[j].offset;
}

/**********************************************************************
 * Tuples
 */

/* Generated code defines:
 *   pLsrPArrayOLenOffset
 *   pLsrPArrayOEltOffset
 *   pLsrPArrayOBaseSize
 *   pLsrPArrayILenOffset
 *   pLsrPArrayIEltOffset
 *   pLsrPArrayIIdxOffset
 *   pLsrPArrayIBaseSize
 */

/* Base tuples */

#define pLsrTupleStatic(v, t, ...) static t v = { __VA_ARGS__ }

#define pLsrTupleStaticV(v, t, vtb, sz, ...)                            \
    static t v = { .vtable = vtb, .f_0 = sz, .extras = { __VA_ARGS__ } }

#define pLsrTupleNewFixed(dest, vt, sz, algn)                   \
    pLsrAllocAligned(PlsrObjectB, (dest), (vt), (sz), algn)
#define pLsrTupleNewVariable(dest, vt, fsz, esz, c, algn)               \
    pLsrAllocAligned(PlsrObjectB, (dest), (vt), (fsz)+(esz)*(c), algn)

#define pLsrTupleNewPinnedFixed(dest, vt, sz, algn)                     \
    pLsrAllocAlignedPinned(PlsrObjectB, (dest), (vt), (sz), algn)
#define pLsrTupleNewPinnedVariable(dest, vt, fsz, esz, c, algn)         \
    pLsrAllocAlignedPinned(PlsrObjectB, (dest), (vt), (fsz)+(esz)*(c), algn)

/* Ordinal arrays */

/* This vtable should only be used with arrays of refs
 */
pLsrVTableStatic(pLsrPArrayOVTable_, VArrayTag, "*ordinal array*", pLsrPArrayOPadding);
#define pLsrPArrayOVTable (&pLsrPArrayOVTable_)

/* This function should only be used for arrays of references */
static PlsrPAny pLsrPArrayONew(uintp c)
{
    PlsrPAny res;
    pLsrTupleNewVariable(res, pLsrPArrayOVTable, pLsrPArrayOBaseSize,
                         sizeof(PlsrObjectB), c, sizeof(PlsrObjectB));
    pLsrObjectField(res, pLsrPArrayOLenOffset, uintp*) = c;
    return res;
}

#define pLsrPArrayOGetLen(arr) \
    (pLsrObjectField((arr), pLsrPArrayOLenOffset, uintp*))

/* This macro is for arrays of references */
#define pLsrPArrayOElt(arr, i) \
    (pLsrObjectField((arr), pLsrPArrayOEltOffset+sizeof(PlsrObjectB)*(i), \
                     PlsrObjectB*))

static void pLsrPArrayOSet(PlsrPAny arr, PlsrPAny i, PlsrObjectB e)
{
    uintp ui;
    pLsrUIntpFromPRat(ui, i);
    pLsrWriteBarrierRefBase(arr, pLsrPArrayOElt(arr, ui), e);
}

static PlsrObjectB pLsrPArrayOGet(PlsrPAny arr, PlsrPAny i)
{
    uintp ui;
    pLsrUIntpFromPRat(ui, i);
    return pLsrPArrayOElt(arr, ui);
}

#define pLsrPArrayOEltEval(dest, a, i) pLsrObjectEval((dest), pLsrPArrayOElt((a), (i)))

/* Indexed arrays */

/* This vtable should not be used with accurate GC, as it does not
 * determine its GC info unambiguously.  However, the runtime uses
 * it for arrays with all reference elements.
 */
pLsrVTableStatic(pLsrPArrayIVTable_, VArrayIdxTag, "*indexed array*", pLsrPArrayIPadding);
#define pLsrPArrayIVTable (&pLsrPArrayIVTable_)

/* This function should only be used for arrays of references */
static PlsrPAny pLsrPArrayINew(uintp c, PlsrIdxB idx)
{
    PlsrPAny res;
    noyield {
        pLsrTupleNewVariable(res, 
                             pLsrPArrayIVTable,
                             pLsrPArrayIBaseSize,
                             sizeof(PlsrObjectB),
                             c, 
                             sizeof(PlsrObjectB));
        pLsrObjectField(res, pLsrPArrayILenOffset, uintp*) = c;
        pLsrWriteBarrierRefOptBase(res,
                                   pLsrObjectField(res, pLsrPArrayIIdxOffset,
                                                   PlsrIdxB*),
                                   idx);
    }
    return res;
}

/* This macro is for arrays of references */
#define pLsrPArrayIElt(arr, i)                                          \
    (pLsrObjectField((arr), pLsrPArrayIEltOffset+sizeof(PlsrObjectB)*(i), \
                     PlsrObjectB*))

#define pLsrPArrayIGetLen(arr)                                  \
    (pLsrObjectField((arr), pLsrPArrayILenOffset, uintp*))

#define pLsrPArrayIGetIdx(arr)                                  \
    (pLsrObjectField((arr), pLsrPArrayIIdxOffset, PlsrIdxB*))

static PlsrObjectB pLsrPArrayIGet(PlsrPAny arr, PlsrPAny n)
{
    uintp i = pLsrIdxGet(pLsrPArrayIGetIdx(arr), n);
    return pLsrPArrayIElt(arr, i);
}

/* Utilities */

static PlsrBoolean pLsrPArrayHasIdx(PlsrPAny arr)
{
    return pLsrVTableGetTag(pLsrObjectGetVTable(arr)) == VArrayIdxTag;
}


/**********************************************************************
 * P Functions
 */

/* When using accurate GC, this vtable should be used only with closures
 * with no free variables.
 */
pLsrVTableStatic(pLsrClosureVTable_, VFunctionTag, "*function*", pLsrPFunctionPadding);
#define pLsrClosureVTable (&pLsrClosureVTable_)

/* Generated constants:
 *   pLsrPFunctionCodeOffset
 *   pLsrPFunctionSize  (* for no free variables *)
 */

typedef PlsrObjectB (*PlsrPFunctionCodeRef)(PlsrObjectB);

#define pLsrPFunctionApplyRef(clos, arg)                                \
    (pLsrObjectField(clos, pLsrPFunctionCodeOffset, PlsrPFunctionCodeRef*) \
     (arg))

/**********************************************************************
 * Sums
 */

/* All this code is for sums over references only */

/* Objects created from this vtable must always be initialised immediately
 * after allocation
 * without an intervening GC.  If this is not the case, the mutability
 * property of the vtable must be changed.
 */
pLsrVTableStatic(pLsrPSumVTable_, VSumTag, "*sum*", pLsrPSumPadding);
#define pLsrPSumVTable (&pLsrPSumVTable_)

/* Generated constants:
 *   pLsrPSumTagOffset
 *   pLsrPSumValOffset
 *   pLsrPSumSize
 */

static PlsrPAny pLsrPSumNew(PlsrPAny tag, PlsrObjectB value)
{
    PlsrPAny v;
    noyield {
        pLsrAlloc(PlsrPAny, v, pLsrPSumVTable, pLsrPSumSize);
        pLsrWriteBarrierRefOptBase
            (v, pLsrObjectField(v, pLsrPSumTagOffset, PlsrPAny*), tag);
        pLsrWriteBarrierRefOptBase
            (v, pLsrObjectField(v, pLsrPSumValOffset, PlsrObjectB*), value);
    }
    return v;
}

static PlsrPAny pLsrPSumNewT(PlsrPAny tag, PlsrPAny value)
{
    PlsrThunkBRef valthnk = pLsrThunkNewValRef((PlsrRef) value);
    return pLsrPSumNew(tag, (PlsrObjectB)valthnk);
}

#define pLsrPSumGetTag(s) (pLsrObjectField(s, pLsrPSumTagOffset, PlsrPAny*))
#define pLsrPSumGetVal(s) (pLsrObjectField(s, pLsrPSumValOffset, PlsrObjectB*))
#define pLsrPSumGetValEval(dest, s) pLsrObjectEval((dest), pLsrPSumGetVal(s))


/**********************************************************************
 * Strings
 */

static PlsrPAny pLsrCoreCharOrd = 0;

static void pLsrRegisterCoreCharOrd(PlsrPAny n)
{
    assert(!pLsrCoreCharOrd); // Should only be registered once
    assert(n);
    pLsrCoreCharOrd = n;
}

static PlsrBoolean pLsrIsNiceChar(int c)
{
    return c>=32 && c<=126;
}

static PlsrBoolean pLsrValueToChar(char* c, PlsrPAny v)
{
    assert(pLsrCoreCharOrd);
    if (pLsrVTableGetTag(pLsrObjectGetVTable(v)) == VSumTag) {
        if(pLsrPSumGetTag(v) == pLsrCoreCharOrd) {
            PlsrPAny cv;
            pLsrPSumGetValEval(cv, v);
            if (cv) {
                char i;
                pLsrUInt32FromPRat(i, cv);
                if (pLsrIsNiceChar(i)) {
                    *c = i;
                    return 1;
                }
            }
        }
    }
    return 0;
}

static char* pLsrValueToCStringMaybe(PlsrPAny v)
{
    if (!pLsrCoreCharOrd ||
        pLsrVTableGetTag(pLsrObjectGetVTable(v)) != VArrayTag ||
        pLsrPArrayHasIdx(v) ||
        pLsrPArrayOGetLen(v) == 0)
        return 0;
    {
        uintp c = pLsrPArrayOGetLen(v);
        char* s = (char*)pLsrAllocC((c+1) * sizeof(char));
        uintp i;
        for(i=0; i<c; i++) {
            PlsrPAny elt;
            pLsrPArrayOEltEval(elt, v, i);
            if (!(elt && pLsrValueToChar(&s[i], elt))) {
                pLsrFreeC(s);
                return 0;
            }
        }
        s[i]=0;
        return s;
    }
}

static char* pLsrPStringToCString(PlsrPAny s)
{
    uintp len = pLsrPArrayOGetLen(s);
    char* buf = (char*)pLsrAllocC((len+1)*sizeof(char));
    uintp i;
    assert(pLsrCoreCharOrd);
    assert(pLsrVTableGetTag(pLsrObjectGetVTable(s)) == VArrayTag &&
           !pLsrPArrayHasIdx(s));
    buf[len] = '\0';
    for(i=0; i<len; i++) {
        PlsrPAny c;
        pLsrPArrayOEltEval(c, s, i);
        PlsrPAny cord;
        char cval;
        assert(pLsrVTableGetTag(pLsrObjectGetVTable(c)) == VSumTag &&
               pLsrPSumGetTag(c) == pLsrCoreCharOrd);
        pLsrPSumGetValEval(cord, c);
        assert(pLsrVTableGetTag(pLsrObjectGetVTable(cord)) == VRatTag);
        pLsrUInt32FromPRat(cval, cord);
        buf[i] = cval;
    }
    return buf;
}

static PlsrPAny pLsrCStringToPStringT(const char* str)
{
    uintp len = strlen(str);
    PlsrPAny res = pLsrPArrayONew(len);
    uintp i;
    assert(pLsrCoreCharOrd);
    for(i=0; i<len; i++) {
        PlsrPAny cval;
        PlsrPAny csum;
        pLsrPRatFromUInt32(cval, str[i]);
        csum = pLsrPSumNewT(pLsrCoreCharOrd, cval);
        PlsrThunkBRef thnk2 = pLsrThunkNewValRef((PlsrRef)csum);
        pLsrWriteBarrierRefBase(res, pLsrPArrayOElt(res, i),
                                (PlsrObjectB)thnk2);
    }
    return res;
}

static PlsrPAny pLsrCStringToPString(const char* str)
{
    uintp len = strlen(str);
    PlsrPAny res = pLsrPArrayONew(len);
    uintp i;
    assert(pLsrCoreCharOrd);
    for(i=0; i<len; i++) {
        PlsrPAny cval;
        PlsrPAny csum;
        pLsrPRatFromUInt32(cval, str[i]);
        csum = pLsrPSumNew(pLsrCoreCharOrd, cval);
        pLsrWriteBarrierRefBase(res, pLsrPArrayOElt(res, i), csum);
    }
    return res;
}

/**********************************************************************
 * Unreachable error values
 */

#define pLsrErrorVal(t) ((t)0)

/**********************************************************************
 * Printing
 */

static void pLsrNamePrint(PlsrPAny n)
{
    printf("%s", pLsrPNameGetString(n));
}

static void pLsrValuePrint(PlsrObjectB v)
{
    switch (pLsrVTableGetTag(pLsrObjectGetVTable(v))) {
    case VRatTag: {
        PlsrRational r;
        char *s;
        pLsrRationalFromPRat(r, v);
        s = pLsrCStringFromRational(r);
        printf("%s", s);
        pLsrFreeC(s);
        break;
    }
    case VNameTag:
        pLsrNamePrint(v);
        break;
    case VFloatTag:{
        float f = pLsrPFloatGet(v);
        printf("%f", pLsrPFloatGet(v));
        break;}
    case VDoubleTag:
        printf("%f", pLsrPDoubleGet(v));
        break;
    case VArrayTag: {
        char* s =  pLsrValueToCStringMaybe(v);
        if (s) {
             printf("%s", s);
             pLsrFreeC(s);
        } else {
            uintp i;
            printf("{");
            for(i=0; i<pLsrPArrayOGetLen(v); i++) {
                if(i>0)  printf(", ");
                pLsrValuePrint(pLsrPArrayOElt(v, i));
            }
            printf("}");
        }
        break;
    }
    case VArrayIdxTag: {
        uintp alen = pLsrPArrayIGetLen(v);
        /* Don't stack allocate until Pillar compiler alloca bug is fixed. */
        PlsrPAny *names = (PlsrPAny*) pLsrAllocC(alen*sizeof(PlsrPAny));
        {
            uintp i;
            PlsrIdxB idx = pLsrPArrayIGetIdx(v);
            uintp dlen = idx->len;

            for(i=0; i < dlen; i++) {
                if (idx->elts[i].name) {
                    names[idx->elts[i].offset] = idx->elts[i].name;
                }
            }
        }
        {
            uintp i;
            bool first = 1;
            
            printf("{");
            for(i=0; i < alen; i++) {
                if (!first) printf(", ");
                else first=0;
                pLsrNamePrint(names[i]);
                printf(" => ");
                pLsrValuePrint(pLsrPArrayIElt(v, i));
            }
        }
        printf("}");
        pLsrFreeC(names);
        break;
    }
    case VSetTag:
        printf("SET{");
        if (!pLsrPSetIsEmpty(v)) 
            pLsrValuePrint(pLsrPSetGet(v));
        printf("}");
        break;
    case VTypeTag:
        printf("TypePH");
        break;
    case VSumTag:
        printf("sum(");
        pLsrNamePrint(pLsrPSumGetTag(v));
        printf(") ");
        pLsrValuePrint(pLsrPSumGetVal(v));
        break;
    case VFunctionTag:
        printf("A Closure");
        break;
    case VThunkTag:
        pLsrThunkPrintRef((PlsrThunkBRef)v);
        break;
    default:
        printf("Something else: possibly missed a case in ValuePrint");
        break;
    }
};

/**********************************************************************
 * Register Runtime VTables
 */

#define pLsrPNameSize (sizeof(PlsrPNameU)+1) // plus one for zero byte
#define pLsrPNameLenOff ((unsigned)(&((PlsrPNameU*)0)->strLen))
#define pLsrIdxSize (sizeof(PlsrIdxU))
#define pLsrIdxLenOff ((unsigned)(&((PlsrIdxU*)0)->len))
#define pLsrIdxEltSize (sizeof(PlsrIdxE))
#define pLsrThunkCutSize ()

/* Note: for PgcIsRef arrays which are all zeroes, we rely on 
 * c99 static initialization semantics to initialize the fields to zero.
 * By convention, we write this as {0, } .
 */
static void pLsrValueRegisterVTables()
{
    static PgcIsRef pLsrPRatRefs[pLsrPRatSize/P_WORD_SIZE] = { 0, pLsrPRatContainsRef };
    static PgcIsRef pLsrPNameRefs[pLsrPNameSize/P_WORD_SIZE] = { 0, };
    static PgcIsRef pLsrPFloatRefs[pLsrPFloatSize/P_WORD_SIZE] = { 0, };
    static PgcIsRef pLsrPDoubleRefs[pLsrPDoubleSize/P_WORD_SIZE] = { 0, };
    static PgcIsRef pLsrPSumRefs[] = { 0, 1, 1 };
    static PgcIsRef pLsrPArrayORefs[pLsrPArrayOBaseSize/P_WORD_SIZE] = { 0, };
    static PgcIsRef pLsrPArrayIRefs[] = { 0, 0, 1 };
    static PgcIsRef pLsrPFunctionRefs[pLsrPFunctionSize/P_WORD_SIZE] = { 0, };
    static PgcIsRef pLsrPSetRefs[] = { 0, 1 };
    static PgcIsRef pLsrPTypeRefs[pLsrPTypeSize/P_WORD_SIZE] = { 0, };
    static PgcIsRef pLsrIdxRefs[pLsrIdxSize/P_WORD_SIZE] = { 0, };
    static PgcIsRef pLsrIdxWRefs[pLsrIdxSize/P_WORD_SIZE] = {0, };
    static PgcIsRef pLsrIdxEltRefs[] = { 1, 0 };
#ifdef PLSR_LIGHTWEIGHT_THUNKS
    assert(sizeof(PlsrThunkURef)/P_WORD_SIZE == 2);
    static PgcIsRef pLsrThunkValRefsRef[] = {0, 1};
    static PgcIsRef pLsrThunkCutRefs[sizeof(PlsrThunkURef)/P_WORD_SIZE] = {0, 1};
#else /* !PLSR_LIGHTWEIGHT_THUNKS */
#ifdef PLSR_THUNK_SYNCHRONIZE
#ifdef PLSR_THUNK_INTERCEPT_CUTS
    assert(sizeof(PlsrThunkURef)/P_WORD_SIZE == 6);
    static PgcIsRef pLsrThunkValRefsRef[] = {0, 0, 0, 0, 0, 1};
#else  /*  PLSR_THUNK_INTERCEPT_CUTS*/
    assert(sizeof(PlsrThunkURef)/P_WORD_SIZE == 5);
    static PgcIsRef pLsrThunkValRefsRef[] = {0, 0, 0, 0, 1};
#endif /*  PLSR_THUNK_INTERCEPT_CUTS */
#else /* !PLSR_THUNK_SYNCHRONIZE */
#ifdef PLSR_THUNK_INTERCEPT_CUTS
    assert(sizeof(PlsrThunkURef)/P_WORD_SIZE == 5);
    static PgcIsRef pLsrThunkValRefsRef[] = {0, 0, 0, 0, 1};
#else  /*  PLSR_THUNK_INTERCEPT_CUTS*/
    assert(sizeof(PlsrThunkURef)/P_WORD_SIZE == 4);
    static PgcIsRef pLsrThunkValRefsRef[] = {0, 0, 0, 1};
#endif /*  PLSR_THUNK_INTERCEPT_CUTS */
#endif /* !PLSR_THUNK_SYNCHRONIZE */
#endif /* !PLSR_LIGHTWEIGHT_THUNKS */
    static PgcIsRef pLsrThunkValRefs32[sizeof(PlsrThunkU32)/P_WORD_SIZE]         = {0, };
    static PgcIsRef pLsrThunkValRefs64[sizeof(PlsrThunkU64)/P_WORD_SIZE]         = {0, };
    static PgcIsRef pLsrThunkValRefsFloat[sizeof(PlsrThunkUFloat)/P_WORD_SIZE]   = {0, };
    static PgcIsRef pLsrThunkValRefsDouble[sizeof(PlsrThunkUDouble)/P_WORD_SIZE] = {0, };

#ifdef PLSR_LIGHTWEIGHT_THUNKS
    static PgcIsRef pLsrThunkEvalRefsRef[sizeof(PlsrThunkURef)/P_WORD_SIZE]       = {0, };
    static PgcIsRef pLsrThunkEvalRefs32[sizeof(PlsrThunkU32)/P_WORD_SIZE]         = {0, };
    static PgcIsRef pLsrThunkEvalRefs64[sizeof(PlsrThunkU64)/P_WORD_SIZE]         = {0, };
    static PgcIsRef pLsrThunkEvalRefsFloat[sizeof(PlsrThunkUFloat)/P_WORD_SIZE]   = {0, };
    static PgcIsRef pLsrThunkEvalRefsDouble[sizeof(PlsrThunkUDouble)/P_WORD_SIZE] = {0, };
#endif /* PLSR_LIGHTWEIGHT_THUNKS */

    assert(pLsrPRatSize/P_WORD_SIZE == 2);
    assert(pLsrPSumSize/P_WORD_SIZE == 3);
    assert(pLsrPArrayIBaseSize/P_WORD_SIZE == 3);
    assert(pLsrPSetSize/P_WORD_SIZE == 2);
    assert(pLsrIdxEltSize/P_WORD_SIZE == 2);

    pLsrVTableRegister(pLsrPRatVTable, pLsrDefaultAlignment, pLsrPRatSize, pLsrPRatRefs, 0, 0, 0,
                       PGC_ALWAYS_IMMUTABLE, 0);
    pLsrVTableRegister(pLsrPNameVTable, pLsrDefaultAlignment, pLsrPNameSize, pLsrPNameRefs, 1,
                       pLsrPNameLenOff, 0, PGC_ALWAYS_IMMUTABLE, 0);
    pLsrVTableRegister(pLsrPFloatVTable, pLsrDefaultAlignment, pLsrPFloatSize, pLsrPFloatRefs,
                       0, 0, 0, PGC_ALWAYS_IMMUTABLE, 0);
    pLsrVTableRegister(pLsrPDoubleVTable, pLsrDefaultAlignment, pLsrPDoubleSize, pLsrPDoubleRefs,
                       0, 0, 0, PGC_ALWAYS_IMMUTABLE, 0);
    pLsrVTableRegister(pLsrPSumVTable, pLsrDefaultAlignment, pLsrPSumSize, pLsrPSumRefs, 0, 0, 0,
                       PGC_ALWAYS_IMMUTABLE, 0);
    pLsrVTableRegister(pLsrPArrayOVTable, pLsrDefaultAlignment, pLsrPArrayOBaseSize, pLsrPArrayORefs,
                       P_WORD_SIZE, pLsrPArrayOLenOffset, 1,
                       PGC_CREATED_MUTABLE, 0);
    pLsrVTableRegister(pLsrPArrayIVTable, pLsrDefaultAlignment, pLsrPArrayIBaseSize, pLsrPArrayIRefs,
                       P_WORD_SIZE, pLsrPArrayILenOffset, 1,
                       PGC_CREATED_MUTABLE, 0);
    pLsrVTableRegister(pLsrClosureVTable, pLsrDefaultAlignment, pLsrPFunctionSize, pLsrPFunctionRefs,
                       0, 0, 0, PGC_CREATED_MUTABLE, 0);
    pLsrVTableRegister(pLsrPSetVTable, pLsrDefaultAlignment, pLsrPSetSize, pLsrPSetRefs, 0, 0, 0,
                       PGC_ALWAYS_IMMUTABLE, 0);
    pLsrVTableRegister(pLsrPTypeVTable, pLsrDefaultAlignment, pLsrPTypeSize, pLsrPTypeRefs, 0, 0,
                       0, PGC_ALWAYS_IMMUTABLE, 0);
    pLsrVTableRegisterV(pLsrIdxVTable, pLsrDefaultAlignment, pLsrIdxSize, pLsrIdxRefs,
                        pLsrIdxEltSize, pLsrIdxLenOff, pLsrIdxEltRefs,
                        PGC_CREATED_MUTABLE, 0, pLsrIdxWRefs, NULL);
#ifdef PLSR_GC_INDIRECTIONS
    pLsrIndirectionVTableRegister(pLsrThunkValVTableRef, sizeof(PlsrThunkURef), pLsrThunkResultOffsetRef);
#else
    pLsrVTableRegister(pLsrThunkValVTableRef, pLsrDefaultAlignment, sizeof(PlsrThunkURef),
                       pLsrThunkValRefsRef, 0, 0, 0, PGC_ALWAYS_IMMUTABLE, 0);
#endif
    pLsrVTableRegister(pLsrThunkValVTable32, pLsrDefaultAlignment, sizeof(PlsrThunkU32),
                       pLsrThunkValRefs32, 0, 0, 0, PGC_ALWAYS_IMMUTABLE, 0);
    pLsrVTableRegister(pLsrThunkValVTable64, pLsrDefaultAlignment, sizeof(PlsrThunkU64),
                       pLsrThunkValRefs64, 0, 0, 0, PGC_ALWAYS_IMMUTABLE, 0);
    pLsrVTableRegister(pLsrThunkValVTableFloat, pLsrDefaultAlignment, sizeof(PlsrThunkUFloat),
                       pLsrThunkValRefsFloat, 0, 0, 0, PGC_ALWAYS_IMMUTABLE, 0);
    pLsrVTableRegister(pLsrThunkValVTableDouble, pLsrDefaultAlignment, sizeof(PlsrThunkUDouble),
                       pLsrThunkValRefsDouble, 0, 0, 0, PGC_ALWAYS_IMMUTABLE, 0);

#ifdef PLSR_LIGHTWEIGHT_THUNKS
    pLsrVTableRegister(pLsrThunkCutVTable, pLsrDefaultAlignment, sizeof(PlsrThunkURef),
                       pLsrThunkCutRefs, 0, 0, 0, PGC_ALWAYS_IMMUTABLE, 0);

    pLsrVTableRegister(pLsrThunkEvalVTableRef, pLsrDefaultAlignment, sizeof(PlsrThunkURef),
                       pLsrThunkEvalRefsRef, 0, 0, 0, PGC_CREATED_MUTABLE, 0);
    pLsrVTableRegister(pLsrThunkEvalVTable32, pLsrDefaultAlignment, sizeof(PlsrThunkU32),
                       pLsrThunkEvalRefs32, 0, 0, 0, PGC_CREATED_MUTABLE, 0);
    pLsrVTableRegister(pLsrThunkEvalVTable64, pLsrDefaultAlignment, sizeof(PlsrThunkU64),
                       pLsrThunkEvalRefs64, 0, 0, 0, PGC_CREATED_MUTABLE, 0);
    pLsrVTableRegister(pLsrThunkEvalVTableFloat, pLsrDefaultAlignment, sizeof(PlsrThunkUFloat),
                       pLsrThunkEvalRefsFloat, 0, 0, 0, PGC_CREATED_MUTABLE, 0);
    pLsrVTableRegister(pLsrThunkEvalVTableDouble, pLsrDefaultAlignment, sizeof(PlsrThunkUDouble),
                       pLsrThunkEvalRefsDouble, 0, 0, 0, PGC_CREATED_MUTABLE, 0);
#endif
}

/**********************************************************************
 * Register Runtime Globals
 */

#define pLsrValueGlobalRefsCount 1

static PlsrRef* pLsrValueGlobalRefs[] = 
    {
        (PlsrRef *) &pLsrCoreCharOrd,
        (PlsrRef *) NULL /* This must be last */
    };

static void pLsrValueRegisterGlobalRefs() {
    assert(pLsrValueGlobalRefs[pLsrValueGlobalRefsCount] == NULL);
    pLsrGcRegisterGlobalRefs ((void **)pLsrValueGlobalRefs, pLsrValueGlobalRefsCount);
};

static void pLsrValueRegisterGlobals() {
    pLsrValueRegisterGlobalRefs();
};

/**********************************************************************
 * Runtime Assertions
 */

static void pLsrValueCheck()
{
    if (pLsrObjectFieldsBase > pLsrPSetOffset)
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrPSetSize < pLsrPSetOffset+sizeof(PlsrPAny))
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrObjectFieldsBase > pLsrPTypeSize)
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrObjectFieldsBase > pLsrPRatOffset)
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrPRatSize < pLsrPRatOffset+sizeof(PlsrRational))
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrObjectFieldsBase > pLsrPFloatOffset)
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrPFloatSize < pLsrPFloatOffset+sizeof(float))
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrObjectFieldsBase > pLsrPDoubleOffset)
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrPDoubleSize < pLsrPDoubleOffset+sizeof(double))
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrObjectFieldsBase > pLsrPArrayOLenOffset)
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrPArrayOEltOffset < pLsrPArrayOLenOffset+sizeof(uintp))
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrPArrayOBaseSize < pLsrPArrayOEltOffset)
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrObjectFieldsBase > pLsrPArrayILenOffset)
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrPArrayIIdxOffset < pLsrPArrayILenOffset+sizeof(uintp))
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrPArrayIEltOffset < pLsrPArrayIIdxOffset+sizeof(void*))
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrPArrayIBaseSize < pLsrPArrayIEltOffset)
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrObjectFieldsBase > pLsrPFunctionCodeOffset)
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrObjectFieldsBase > pLsrPSumTagOffset)
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrPSumSize < pLsrPSumTagOffset+sizeof(PlsrPAny))
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrPSumSize < pLsrPSumValOffset+sizeof(PlsrObjectB))
        pLsrRuntimeError("Bad object model!\n");
#ifdef P_USE_TAGGED_RATIONALS
    if (pLsrPSmallRationalMax != pLsrSmallRationalMax) 
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrPSmallRationalMin != pLsrSmallRationalMin)
        pLsrRuntimeError("Bad object model!\n");
#endif
#ifdef P_USE_TAGGED_INTEGERS
    if (pLsrPSmallIntegerMax != pLsrSmallIntegerMax) 
        pLsrRuntimeError("Bad object model!\n");
    if (pLsrPSmallIntegerMin != pLsrSmallIntegerMin)
        pLsrRuntimeError("Bad object model!\n");
#endif
}

#endif /* !_PLSR_VALUE_H_ */
