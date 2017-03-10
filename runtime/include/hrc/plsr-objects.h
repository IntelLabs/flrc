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

#define pLsrVTableStaticWithCustom(vt, nm, p, c)        \
    pil_aligned(16) static struct PlsrVTableS vt =                  \
        { pLsrVTableStaticCustomField(c)                    \
          pLsrVTableStaticNameField(nm)                     \
          pLsrVTableStaticNextField(0)                      \
          pLsrVTableStaticNumObjectsField(0)                \
          pLsrVTableStaticNumBytesField(0)                  \
          pLsrVTableStaticPaddingField(p)}

#define pLsrVTableStatic(vt, nm, p)                   \
    pLsrVTableStaticWithCustom(vt, nm, p, 0)

#define pLsrVTableGetTag(vt) ((vt)->tag)

#define pLsrVTableGetCustom(vt) ((vt)->custom)

/* This vtable should not be used with accurate GC, as it does not
 * determine its GC info unambiguously.
 */
pLsrVTableStatic(pLsrVTableNone_, "*none*", 0);
#define pLsrVTableNone (&pLsrVTableNone_)

/* VTables for thunks */

/* When using accurate GC, these vtable is only for thunks with no
 * free variables, initialized atomically at allocation time.
 */
pLsrVTableStatic(pLsrThunkValVTableRef_, "*thunk value (ref)*", pLsrThunkPaddingRef);
#define pLsrThunkValVTableRef (&pLsrThunkValVTableRef_)

pLsrVTableStatic(pLsrThunkValVTable32_, "*thunk value (32)*", pLsrThunkPadding32);
#define pLsrThunkValVTable32 (&pLsrThunkValVTable32_)

pLsrVTableStatic(pLsrThunkValVTable64_, "*thunk value (64)*", pLsrThunkPadding64);
#define pLsrThunkValVTable64 (&pLsrThunkValVTable64_)

pLsrVTableStatic(pLsrThunkValVTableFloat_, "*thunk value (float)*", pLsrThunkPaddingFloat);
#define pLsrThunkValVTableFloat (&pLsrThunkValVTableFloat_)

pLsrVTableStatic(pLsrThunkValVTableDouble_, "*thunk value (double)*", pLsrThunkPaddingDouble);
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
