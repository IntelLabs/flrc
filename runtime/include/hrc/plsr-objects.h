/* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

/* Heap Objects and the Object Model */

#ifndef _PLSR_OBJECTS_H_
#define _PLSR_OBJECTS_H_

/**********************************************************************
 * Refs
 */

#ifdef P_USE_PILLAR
typedef ref PlsrRef;
#else /* !P_USE_PILLAR */
typedef void* PlsrRef;
#endif /* !P_USE_PILLAR */

/**********************************************************************
 * VTables
 */

enum PlsrValueTagE {
    VNoneTag,
    VRatTag,
    VNameTag,
    VFloatTag,
    VDoubleTag,
    VArrayTag,
    VArrayIdxTag,
    VSumTag,
    VFunctionTag,
    VSetTag,
    VTypeTag,
    VPtrTag,
    VThunkTag
};

struct PlsrVTableS {
    char reserve[P_VTABLE_RESERVE];
    const enum PlsrValueTagE tag;
    uintp custom;
#if (defined(PLSR_INSTRUMENT_VTB_ALC) || defined(DEBUG))
    char* name;
#endif
#ifdef PLSR_INSTRUMENT_VTB_ALC
    struct PlsrVTableS* next;
    uint64 numObjects;
    uint64 numBytes;
#endif /* PLSR_INSTRUMENT_VTB_ALC */
#if (defined(PLSR_INSTRUMENT_ALLOCATION) || defined(PLSR_INSTRUMENT_VTB_ALC))
    uint64 padding; /* How much padding does this object contain? */
#endif

};

typedef struct PlsrVTableS* PlsrVTable;

#define pLsrVTableStaticTagField(tg) .tag = (tg),
#define pLsrVTableStaticCustomField(c) .custom = ((uintp) (c)),
#if (defined(PLSR_INSTRUMENT_VTB_ALC) || defined(DEBUG))
#define pLsrVTableStaticNameField(nm) .name  = (nm),
#else
#define pLsrVTableStaticNameField(nm)
#endif
#ifdef PLSR_INSTRUMENT_VTB_ALC
#define pLsrVTableStaticNextField(n) .next = (n),
#define pLsrVTableStaticNumObjectsField(n) .numObjects = (n),
#define pLsrVTableStaticNumBytesField(n) .numBytes = (n),
#else
#define pLsrVTableStaticNextField(n) 
#define pLsrVTableStaticNumObjectsField(n) 
#define pLsrVTableStaticNumBytesField(n) 
#endif 
#if (defined(PLSR_INSTRUMENT_ALLOCATION) || defined(PLSR_INSTRUMENT_VTB_ALC))
#define pLsrVTableStaticPaddingField(n) .padding = (n),
#else
#define pLsrVTableStaticPaddingField(n)
#endif

#define pLsrVTableStaticWithCustom(vt, tg, nm, p, c)        \
    pil_aligned(16) static struct PlsrVTableS vt =                  \
        { pLsrVTableStaticTagField(tg)                      \
          pLsrVTableStaticCustomField(c)                    \
          pLsrVTableStaticNameField(nm)                     \
          pLsrVTableStaticNextField(0)                      \
          pLsrVTableStaticNumObjectsField(0)                \
          pLsrVTableStaticNumBytesField(0)                  \
          pLsrVTableStaticPaddingField(p)}

#define pLsrVTableStatic(vt, tg, nm, p)                   \
    pLsrVTableStaticWithCustom(vt, tg, nm, p, 0)

#define pLsrVTableGetTag(vt) ((vt)->tag)

#define pLsrVTableGetCustom(vt) ((vt)->custom)

/* This vtable should not be used with accurate GC, as it does not
 * determine its GC info unambiguously.
 */
pLsrVTableStatic(pLsrVTableNone_, VNoneTag, "*none*", 0);
#define pLsrVTableNone (&pLsrVTableNone_)

/* VTables for thunks */

/* When using accurate GC, these vtable is only for thunks with no
 * free variables, initialized atomically at allocation time.
 */
pLsrVTableStatic(pLsrThunkValVTableRef_, VThunkTag, "*thunk value (ref)*", pLsrThunkPaddingRef);
#define pLsrThunkValVTableRef (&pLsrThunkValVTableRef_)

pLsrVTableStatic(pLsrThunkValVTable32_, VThunkTag, "*thunk value (32)*", pLsrThunkPadding32);
#define pLsrThunkValVTable32 (&pLsrThunkValVTable32_)

pLsrVTableStatic(pLsrThunkValVTable64_, VThunkTag, "*thunk value (64)*", pLsrThunkPadding64);
#define pLsrThunkValVTable64 (&pLsrThunkValVTable64_)

pLsrVTableStatic(pLsrThunkValVTableFloat_, VThunkTag, "*thunk value (float)*", pLsrThunkPaddingFloat);
#define pLsrThunkValVTableFloat (&pLsrThunkValVTableFloat_)

pLsrVTableStatic(pLsrThunkValVTableDouble_, VThunkTag, "*thunk value (double)*", pLsrThunkPaddingDouble);
#define pLsrThunkValVTableDouble (&pLsrThunkValVTableDouble_)



/**********************************************************************
 * Objects
 */

typedef struct {
    PlsrVTable vtable;
} PlsrObjectU;

#ifdef P_USE_PILLAR
typedef PlsrRef PlsrObjectB;
#else /* !P_USE_PILLAR */
typedef PlsrObjectU* PlsrObjectB;
#endif /* !P_USE_PILLAR */

#define pLsrObjectGetVTable(obj)                                        \
    ((PlsrVTable)((long)(((PlsrObjectU*)(obj))->vtable) & (0xFFffFFfc)))

/* Generated code defines:
 *   pLsrObjectFieldsBase
 */

#define pLsrObjectGetKind(obj) (pLsrVTableGetTag(pLsrObjectGetVTable(obj)))
/* Note that both of these need the pointer type for t */
#define pLsrObjectField(obj, off, t) (*(t)((char*)(obj) + (off)))
#define pLsrObjectExtra(obj, off, t, es, i) (*(t)((char*)(obj) + (off) + (es * i)))

#ifdef P_USE_PILLAR
typedef PlsrRef PlsrThunkBRef;
typedef PlsrRef PlsrThunkB32;
typedef PlsrRef PlsrThunkB64;
typedef PlsrRef PlsrThunkBFloat;
typedef PlsrRef PlsrThunkBDouble;
typedef PlsrRef PlsrPAny;
#else /* !P_USE_PILLAR */
typedef struct PlsrThunkSRef* PlsrThunkBRef;
typedef struct PlsrThunkS32* PlsrThunkB32;
typedef struct PlsrThunkS64* PlsrThunkB64;
typedef struct PlsrThunkSFloat* PlsrThunkBFloat;
typedef struct PlsrThunkSDouble* PlsrThunkBDouble;
typedef PlsrObjectU* PlsrPAny;
#endif /* !P_USE_PILLAR */

static void pLsrValuePrint(PlsrObjectB);
static PlsrRef pLsrThunkEvalRef(PlsrThunkBRef);

#define pLsrObjectEval(dest, obj)                                       \
    do {                                                                \
        if (pLsrVTableGetTag(pLsrObjectGetVTable(obj)) == VThunkTag) {  \
            (dest) = (PlsrPAny) pLsrThunkEvalRef((PlsrThunkBRef)obj);    \
        } else {                                                        \
            (dest) = (PlsrPAny)obj;                                     \
        }                                                               \
    } while (0)

static void pLsrObjectCheckModel()
{
    if (pLsrObjectFieldsBase < sizeof(PlsrObjectU))
        pLsrRuntimeError("Bad object model!\n");
}

#define pLsrMemzeroArray(dest, fsz, esz, c)  \
    memset(((char*)dest) + (fsz),0,(esz)*(c))

#endif /* !_PLSR_OBJECTS_H_ */
