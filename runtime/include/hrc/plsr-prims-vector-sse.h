/* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

#ifndef _PLSR_PRIMS_VECTOR_SSE_H_
#define _PLSR_PRIMS_VECTOR_SSE_H_

#ifdef P_USE_PILLAR
#  pragma pillar_managed(off)
#  define to __to__
#endif /* P_USE_PILLAR */


#ifdef P_USE_PILLAR
#  undef to
#  pragma pillar_managed(on)
#endif /* P_USE_PILLAR */

#ifdef PLSR_VECTOR_USE_ASM
// define the vector types
/* typedef float __m64 __attribute__ ((__vector_size__ (8)));
typedef int  __m128i  __attribute__ ((__vector_size__ (16)));
typedef double __m128d __attribute__ ((__vector_size__ (16)));
typedef float __m128 __attribute__ ((__vector_size__ (16))); */

/*
struct floatVec {
  float elems[4];
};
typedef struct floatVec __m128;

struct floatVecShort {
  float elems[2];
};
typedef struct floatVecShort __m64;

struct doubleVec {
  double elems[2];
};
typedef struct doubleVec __m128d; */

typedef union __m128i {
    __int8              m128i_i8[16];
    __int16             m128i_i16[8];
    __int32             m128i_i32[4];
    __int64             m128i_i64[2];
    unsigned __int8     m128i_u8[16];
    unsigned __int16    m128i_u16[8];
    unsigned __int32    m128i_u32[4];
    unsigned __int64    m128i_u64[2];
} __m128i;

typedef struct __m128r {
    ref    m128i_u32[4];
} __m128r;

typedef struct __m128d {
    double              m128d_f64[2];
} __m128d;

typedef union __m128 {
     float               m128_f32[4];
     unsigned __int64    m128_u64[2];
     __int8              m128_i8[16];
     __int16             m128_i16[8];
     __int32             m128_i32[4];
     __int64             m128_i64[2];
     unsigned __int8     m128_u8[16];
     unsigned __int16    m128_u16[8];
     unsigned __int32    m128_u32[4];
 } __m128;

typedef union __m64
{
    unsigned __int64    m64_u64;
    float               m64_f32[2];
    __int8              m64_i8[8];
    __int16             m64_i16[4];
    __int32             m64_i32[2];
    __int64             m64_i64;
    unsigned __int8     m64_u8[8];
    unsigned __int16    m64_u16[4];
    unsigned __int32    m64_u32[2];
    } __m64;

#else
#ifdef P_USE_PILLAR
#define __ICL_INTRINCC __pcdecl
#endif /* P_USE_PILLAR */
#include <nmmintrin.h>
#endif

typedef __m64 PlsrVector64F32;

typedef __m128 PlsrVector128F32;
typedef __m128i PlsrVector128B32;
typedef __m128d PlsrVector128F64;
typedef __m128i PlsrVector128Ref;

typedef __m128 PlsrVectorMask128Fs32;
typedef __m128 PlsrVectorMask128Fs64;

// *****************************************************************************
// Comparison
// *****************************************************************************
#if 0
#define pLsrPrimV128PointwiseSInt32LT(dest, a, b) ((dest) = _mm_cmplt_epi32((a),(b)))
#define pLsrPrimV128PointwiseSInt32EQ(dest, a, b) ((dest) = _mm_cmpeq_epi32((a),(b)))
#define pLsrPrimV128PointwiseSInt32GT(dest, a, b) ((dest) = _mm_cmpgt_epi32((a),(b)))
#else
#define pLsrPrimV128CompareSInt32LT(dest, a, b) ((dest) = _mm_cmplt_epi32((a),(b)))
#define pLsrPrimV128CompareSInt32EQ(dest, a, b) ((dest) = _mm_cmpeq_epi32((a),(b)))
#define pLsrPrimV128CompareSInt32GT(dest, a, b) ((dest) = _mm_cmpgt_epi32((a),(b)))
#endif

// *****************************************************************************
// Conversions & Casts
// *****************************************************************************

#ifdef PLSR_VECTOR_USE_ASM

#define pLsrPrimV128128ConvertFloat32FromUInt32(dest, a) vector128128ConvertFloat32FromUInt32(dest, a)
#define pLsrPrimV128128ConvertUInt32FromFloat32(dest, a) vector128128ConvertUInt32FromFloat32(dest, a)

#define vector128128ConvertFloat32FromUInt32(dest, a)    \
    do {                                                 \
        __asm__ ("cvtdq2ps %1, %0;"                      \
                 : "=x"(dest)                            \
                 : "xm"(a));                             \
    } while (0)

#define vector128128ConvertUInt32FromFloat32(dest, a)   \
    do {                                                \
        __asm__ ("cvtps2dq %1, %0;"                     \
                 : "=x"(dest)                           \
                 : "xm"(a));                            \
    } while (0)

#else

#define pLsrPrimV128128ConvertFloat32FromUInt32(dest, a) ((dest) = _mm_cvtepi32_ps(a))
#define pLsrPrimV128128ConvertUInt32FromFloat32(dest, a) ((dest) = _mm_cvtps_epi32(a))
// vec64 -> vec128
#define pLsrPrimV128128ConvertFloat64FromUInt32(dest, a) ((dest) = _mm_cvtepi32_pd (_mm_movpi64_epi64(a)))
#define pLsrPrimV128128ConvertUInt32FromFloat64(dest, a) ((dest) = _mm_movepi64_pi64 (_mm_cvtpd_epi32(a)))

#define pLsrPrimV128128CastSInt32ToFloat32(dest, a) ((dest) = _mm_cvtepi32_ps(a))

#endif

#define pLsrPrimV128128ConvertUInt32FromSInt32(dest, a) ((dest) = a)
#define pLsrPrimV128128ConvertSInt32FromUInt32(dest, a) ((dest) = a)

// *****************************************************************************
// Data operations on the more abstract B32/F32/F64 types
// *****************************************************************************

#define pLsrPrimV128DataVectorB32(dest, c0, c1, c2, c3) pLsrPrimV128UInt32Const(dest, c0, c1, c2, c3)
#define pLsrPrimV128DataVectorF32(dest, c0, c1, c2, c3) pLsrPrimV128Float32Const(dest, c0, c1, c2, c3)
#define pLsrPrimV128DataVectorF64(dest, c0, c1)         pLsrPrimV128Float64Const(dest, c0, c1)

#define pLsrPrimV128DataBroadcastB32(dest, c0) pLsrPrimV128UInt32Lift(dest, c0)
#define pLsrPrimV128DataBroadcastF32(dest, c0) pLsrPrimV128Float32Lift(dest, c0)
#define pLsrPrimV128DataBroadcastF64(dest, c0) pLsrPrimV128Float64Lift(dest, c0)

// Subscripting operations

#define pLsrPrimV128DataSub0B32(dest, a) pLsrPrimV128DataSubB32(dest, 0, a)
#define pLsrPrimV128DataSub1B32(dest, a) pLsrPrimV128DataSubB32(dest, 1, a)
#define pLsrPrimV128DataSub2B32(dest, a) pLsrPrimV128DataSubB32(dest, 2, a)
#define pLsrPrimV128DataSub3B32(dest, a) pLsrPrimV128DataSubB32(dest, 3, a)

/*
static inline int pLsrPrimV128DataSubB32(int index, __m128i a) {
  // _mm_extract_epi32 (SSE >= 4.1)
} */

#define pLsrPrimV128DataSubB32(dest, n, a)     \
  do {                                          \
    int pLsrPrimV128DataSubB32_x;              \
    __asm__ ("pshufd %1, %2, %2;\n\t"           \
             "movd %2, %0;\n\t"                 \
             : "=r"(pLsrPrimV128DataSubB32_x)  \
             : "i"(n), "x"(a));                 \
    (dest) = pLsrPrimV128DataSubB32_x;         \
  } while (0)


#define pLsrPrimV128DataSub0F32(dest, a) pLsrPrimV128DataSubF32(dest, 0, a)
#define pLsrPrimV128DataSub1F32(dest, a) pLsrPrimV128DataSubF32(dest, 1, a)
#define pLsrPrimV128DataSub2F32(dest, a) pLsrPrimV128DataSubF32(dest, 2, a)
#define pLsrPrimV128DataSub3F32(dest, a) pLsrPrimV128DataSubF32(dest, 3, a)

#define pLsrPrimV128DataSubF32(dest, sub, a) ((dest) = ((float*)&a)[sub])

#define pLsrPrimV128DataSub0F64(dest, a) pLsrPrimV128DataSubF64(dest, 0, a)
#define pLsrPrimV128DataSub1F64(dest, a) pLsrPrimV128DataSubF64(dest, 1, a)

#define pLsrPrimV128DataSubF64(dest, sub, a) ((dest) = ((double*)&a)[sub])

// loads and stores

#define pLsrPrimV128B32StoreF(arr, offset, v) pLsrPrimV128UInt32StoreF(arr, offset, v)
#define pLsrPrimV128B64StoreF(arr, offset, v) pLsrPrimV128UInt64StoreF(arr, offset, v)
#define pLsrPrimV128F32StoreF(arr, offset, v) pLsrPrimV128Float32StoreF(arr, offset, v)
#define pLsrPrimV128F64StoreF(arr, offset, v) pLsrPrimV128Float64StoreF(arr, offset, v)
#define pLsrPrimV128RefStoreF(arr, offset, v) pLsrPrimV128RefStoreF(arr, offset, v)

#define pLsrPrimV128B32StoreVS(arr, offset, idx, stride, v)            \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV128UInt32Store(arr, offset, idx, v);              \
        } else {                                                        \
            pLsrPrimV128UInt32StoreStrided(arr, offset, idx, stride, v); \
        }                                                               \
    } while (0)

#define pLsrPrimV128B64StoreVS(arr, offset, idx, stride, v)            \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV128UInt64Store(arr, offset, idx, v);              \
        } else {                                                        \
            pLsrPrimV128UInt64StoreStrided(arr, offset, idx, stride, v); \
        }                                                               \
    } while (0)
#define pLsrPrimV128F32StoreVS(arr, offset, idx, stride, v)            \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV128Float32Store(arr, offset, idx, v);             \
        } else {                                                        \
            pLsrPrimV128Float32StoreStrided(arr, offset, idx, stride, v); \
        }                                                               \
    } while (0)
#define pLsrPrimV128F64StoreVS(arr, offset, idx, stride, v)            \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV128Float64Store(arr, offset, idx, v);             \
        } else {                                                        \
            pLsrPrimV128Float64StoreStrided(arr, offset, idx, stride, v); \
        }                                                               \
    } while (0)
#define pLsrPrimV128RefStoreVS(arr, offset, idx, stride, v)            \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV128RefStore(arr, offset, idx, v);                 \
        } else {                                                        \
            pLsrPrimV128RefStoreStrided(arr, offset, idx, stride, v);  \
        }                                                               \
    } while (0)

#define pLsrPrimV128B32StoreVI(arr, offset, vi, v) pLsrPrimV128UInt32StoreIndexed(arr, offset, vi, v)
#define pLsrPrimV128B64StoreVI(arr, offset, vi, v) pLsrPrimV128UInt64StoreIndexed(arr, offset, vi, v)
#define pLsrPrimV128F32StoreVI(arr, offset, vi, v) pLsrPrimV128Float32StoreIndexed(arr, offset, vi, v)
#define pLsrPrimV128F64StoreVI(arr, offset, vi, v) pLsrPrimV128Float64StoreIndexed(arr, offset, vi, v)
#define pLsrPrimV128RefStoreVI(arr, offset, vi, v) pLsrPrimV128RefStoreIndexed(arr, offset, vi, v)

#define pLsrPrimV128B32StoreVVS(arr, offset, idx, stride, v)           \
    pLsrPrimV128UInt32StoreVectorStrided(arr, offset, idx, stride, v)
#define pLsrPrimV128B64StoreVVS(arr, offset, idx, stride, v)           \
    pLsrPrimV128UInt64StoreVectorStrided(arr, offset, idx, stride, v)
#define pLsrPrimV128F32StoreVVS(arr, offset, idx, stride, v)           \
    pLsrPrimV128Float32StoreVectorStrided(arr, offset, idx, stride, v)
#define pLsrPrimV128F64StoreVVS(arr, offset, idx, stride, v)           \
    pLsrPrimV128Float64StoreVectorStrided(arr, offset, idx, stride, v)
#define pLsrPrimV128RefStoreVVS(arr, offset, idx, stride, v)           \
    pLsrPrimV128RefStoreVectorStrided(arr, offset, idx, stride, v)

#define pLsrPrimV128B32StoreVVI(arr, offset, vi, v) pLsrPrimV128UInt32StoreVectorIndexed(arr, offset, vi, v)
#define pLsrPrimV128B64StoreVVI(arr, offset, vi, v) pLsrPrimV128UInt64StoreVectorIndexed(arr, offset, vi, v)
#define pLsrPrimV128F32StoreVVI(arr, offset, vi, v) pLsrPrimV128Float32StoreVectorIndexed(arr, offset, vi, v)
#define pLsrPrimV128F64StoreVVI(arr, offset, vi, v) pLsrPrimV128Float64StoreVectorIndexed(arr, offset, vi, v)
#define pLsrPrimV128RefStoreVVI(arr, offset, vi, v) pLsrPrimV128RefStoreVectorIndexed(arr, offset, vi, v)

#define pLsrPrimV128B32LoadF(dest, arr, offset) pLsrPrimV128UInt32LoadF(dest, arr, offset)
#define pLsrPrimV128B64LoadF(dest, arr, offset) pLsrPrimV128UInt64LoadF(dest, arr, offset)
#define pLsrPrimV128F32LoadF(dest, arr, offset) pLsrPrimV128Float32LoadF(dest, arr, offset)
#define pLsrPrimV128F64LoadF(dest, arr, offset) pLsrPrimV128Float64LoadF(dest, arr, offset)
#define pLsrPrimV128RefLoadF(dest, arr, offset) pLsrPrimV128RefLoadF(dest, arr, offset)

#define pLsrPrimV128B32LoadVS(dest, arr, offset, idx, stride)          \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV128UInt32Load(dest, arr, offset, idx);            \
        } else {                                                        \
            pLsrPrimV128UInt32LoadStrided(dest, arr, offset, idx, stride); \
        }                                                               \
    } while (0)
#define pLsrPrimV128B64LoadVS(dest, arr, offset, idx, stride)          \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV128UInt64Load(dest, arr, offset, idx);            \
        } else {                                                        \
            pLsrPrimV128UInt64LoadStrided(dest, arr, offset, idx, stride); \
        }                                                               \
    } while (0)
#define pLsrPrimV128F32LoadVS(dest, arr, offset, idx, stride)        \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV128Float32Load(dest, arr, offset, idx);           \
        } else {                                                        \
            pLsrPrimV128Float32LoadStrided(dest, arr, offset, idx, stride); \
        }                                                               \
    } while (0)
#define pLsrPrimV128F64LoadVS(dest, arr, offset, idx, stride)          \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV128Float64Load(dest, arr, offset, idx);           \
        } else {                                                        \
            pLsrPrimV128Float64LoadStrided(dest, arr, offset, idx, stride); \
        }                                                               \
    } while (0)
#define pLsrPrimV128RefLoadVS(dest, arr, offset, idx, stride)          \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV128RefLoad(dest, arr, offset, idx);               \
        } else {                                                        \
            pLsrPrimV128RefLoadStrided(dest, arr, offset, idx, stride); \
        }                                                               \
    } while (0)

#define pLsrPrimV128B32LoadVI(dest, arr, offset, vi) pLsrPrimV128UInt32LoadIndexed(dest, arr, offset, vi)
#define pLsrPrimV128B64LoadVI(dest, arr, offset, vi) pLsrPrimV128UInt64LoadIndexed(dest, arr, offset, vi)
#define pLsrPrimV128F32LoadVI(dest, arr, offset, vi) pLsrPrimV128Float32LoadIndexed(dest, arr, offset, vi)
#define pLsrPrimV128F32LoadVI64(dest, arr, offset, vi) pLsrPrimV128Float32LoadIndexed64(dest, arr, offset, vi)
#define pLsrPrimV128F64LoadVI(dest, arr, offset, vi) pLsrPrimV128Float64LoadIndexed(dest, arr, offset, vi)
#define pLsrPrimV128RefLoadVI(dest, arr, offset, vi) pLsrPrimV128RefLoadIndexed(dest, arr, offset, vi)

#define pLsrPrimV128B32LoadVectorStrided(dest, arr, offset, idx, stride)           \
    pLsrPrimV128UInt32LoadVectorStrided(dest, arr, offset, idx, stride)
#define pLsrPrimV128B64LoadVectorStrided(dest, arr, offset, idx, stride)           \
    pLsrPrimV128UInt64LoadVectorStrided(dest, arr, offset, idx, stride)
#define pLsrPrimV128F32LoadVectorStrided(dest, arr, offset, idx, stride) \
    do {                                                                \
        if (stride == 0) {                                              \
            pLsrPrimV128Float32LoadVector(dest, arr, offset, idx);      \
        } else {                                                        \
            pLsrPrimV128Float32LoadVectorStrided(dest, arr, offset, idx, stride); \
        }                                                               \
    } while (0)                                                         \

#define pLsrPrimV128F64LoadVectorStrided(dest, arr, offset, idx, stride)           \
    pLsrPrimV128Float64LoadVectorStrided(dest, arr, offset, idx, stride)
#define pLsrPrimV128RefLoadVectorStrided(dest, arr, offset, idx, stride)           \
    pLsrPrimV128RefLoadVectorStrided(dest, arr, offset, idx, stride)

#define pLsrPrimV128B32LoadVVS(dest, arr, offset, idx, stride)           \
    pLsrPrimV128UInt32LoadVectorStrided(dest, arr, offset, idx, stride)
#define pLsrPrimV128B64LoadVVS(dest, arr, offset, idx, stride)           \
    pLsrPrimV128UInt64LoadVectorStrided(dest, arr, offset, idx, stride)
#define pLsrPrimV128F32LoadVVS(dest, arr, offset, idx, stride)           \
    pLsrPrimV128F32LoadVectorStrided(dest, arr, offset, idx, stride)
#define pLsrPrimV128F64LoadVVS(dest, arr, offset, idx, stride)           \
    pLsrPrimV128F64LoadVectorStrided(dest, arr, offset, idx, stride)
#define pLsrPrimV128RefLoadVVS(dest, arr, offset, idx, stride)           \
    pLsrPrimV128RefLoadVectorStrided(dest, arr, offset, idx, stride)

#define pLsrPrimV128B32LoadVVI(dest, arr, offset, vi) pLsrPrimV128UInt32LoadVectorIndexed(dest, arr, offset, vi)
#define pLsrPrimV128B64LoadVVI(dest, arr, offset, vi) pLsrPrimV128UInt64LoadVectorIndexed(dest, arr, offset, vi)
#define pLsrPrimV128F32LoadVVI(dest, arr, offset, vi) pLsrPrimV128Float32LoadVectorIndexed(dest, arr, offset, vi)
#define pLsrPrimV128F64LoadVVI(dest, arr, offset, vi) pLsrPrimV128Float64LoadVectorIndexed(dest, arr, offset, vi)
#define pLsrPrimV128RefLoadVVI(dest, arr, offset, vi) pLsrPrimV128RefLoadVectorIndexed(dest, arr, offset, vi)

// *****************************************************************************
// Type-specific arithemetic and memory operations
// *****************************************************************************

// ================
// UInt32 - 128-bit
// ================

// Arithmetic

#ifdef PLSR_VECTOR_USE_ASM

#define pLsrPrimV128PointwiseUInt32Plus(dest, a, b) vector128PointwiseUInt32Plus(dest, a, b)
#define pLsrPrimV128PointwiseUInt32Minus(dest, a, b) vector128PointwiseUInt32Minus(dest, a, b)
#define pLsrPrimV128PointwiseUInt32Times(dest, a, b) vector128PointwiseUInt32Times(dest, a, b)

#define vector128PointwiseUInt32Plus(dest, a, b)                        \
    do {                                                                \
        __m128i vector128PointwiseUInt32Plus_c = b;                     \
        __asm__ ("paddd %2, %0;\n\t"                                    \
                 : "=x"(vector128PointwiseUInt32Plus_c)                 \
                 : "0"(vector128PointwiseUInt32Plus_c), "x"(a));        \
        (dest) = vector128PointwiseUInt32Plus_c;                        \
    } while (0)

#define vector128PointwiseUInt32Minus(dest, a, b)                       \
    do {                                                                \
        __m128i vector128PointwiseUInt32Minus_c = b;                    \
        __asm__ ("psubd %2, %0;\n\t"                                    \
                 : "=x"(vector128PointwiseUInt32Minus_c)                \
                 : "0"(vector128PointwiseUInt32Minus_c), "x"(a));       \
        (dest) = vector128PointwiseUInt32Minus_c;                       \
    } while (0)

#define vector128PointwiseUInt32Times(dest, a, b)               \
    do {                                                        \
        __m128i vector128PointwiseUInt32Times_c;                \
        __asm__ ("movdqu %1, %%xmm0;\n\t"                       \
                 "movdqu %2, %%xmm1;\n\t"                       \
                 "psrldq $0x04, %%xmm0;\n\t" /* c */            \
                 "psrldq $0x04, %%xmm1;\n\t"  /* d */           \
                 "pmuludq %%xmm0, %%xmm1;\n\t" /* xmm1 = c*d */ \
                 "pmuludq %1, %2;\n\t" /* v = a*b */            \
                 "pshufd $0x08, %2, %0;\n\t"                    \
                 "pshufd $0x08, %%xmm1, %%xmm1;\n\t"            \
                 "punpckldq %%xmm1, %0;\n\t"                    \
                 : "=x"(vector128PointwiseUInt32Times_c)        \
                 : "x"(a), "x"(b)                               \
                 : "xmm0", "xmm1");                             \
        (dest) = vector128PointwiseUInt32Times_c;               \
    } while (0)

#else

#define pLsrPrimV128PointwiseUInt32Plus(dest, a, b) ((dest) = _mm_add_epi32(a, b))
#define pLsrPrimV128PointwiseUInt32Minus(dest, a, b) ((dest) = _mm_sub_epi32(a, b))
#define pLsrPrimV128PointwiseUInt32Times(dest, a, b) u32_mul(dest, a, b)  // _mm_mullo_epi32(a, b)) <- SSE 4.1
#define pLsrPrimV128PointwiseUInt32Max(a, b) (_mm_max_epi32(a, b)) //not supported <- SSE 4.1

#define pLsrPrimV128PointwiseSInt32Plus(dest, a, b) ((dest) = _mm_add_epi32(a, b))
#define pLsrPrimV128PointwiseSInt32Minus(dest, a, b) ((dest) = _mm_sub_epi32(a, b))
#define pLsrPrimV128PointwiseSInt32Times(dest, a, b) ((dest) = _mm_mullo_epi32(a, b)) //<- SSE 4.1

#define pLsrPrimV128PointwiseUInt32BAnd(dest, a, b) ((dest) = _mm_and_si128((a), (b)))

/*
#define pLsrPrimV128PointwiseUInt32BShiftR(dest, a, b)  \
    do {						\
	int pLsrPrimV128PointwiseUInt32BShiftR_count = _mm_extract_epi32(b, 0);		\
	(dest) = _mm_srli_epi32((a), pLsrPrimV128PointwiseUInt32BShiftR_count);		\
    } while (0)

#define pLsrPrimV128PointwiseUInt32BShiftL(dest, a, b)  \
    do {						\
	int pLsrPrimV128PointwiseUInt32BShiftL_count = _mm_extract_epi32(b, 0);		\
	(dest) = _mm_slli_epi32((a), pLsrPrimV128PointwiseUInt32BShiftL_count);		\
    } while (0)
*/

#define pLsrPrimV128PointwiseUInt32BShiftR(dest, a, b) \
    do {                                                                \
        uint32 pLsrPrimV128PointwiseUInt32BShiftR_R0 = _mm_extract_epi32(a, 0) >> _mm_extract_epi32(b, 0) ; \
        uint32 pLsrPrimV128PointwiseUInt32BShiftR_R1 = _mm_extract_epi32(a, 1) >> _mm_extract_epi32(b, 1) ; \
        uint32 pLsrPrimV128PointwiseUInt32BShiftR_R2 = _mm_extract_epi32(a, 2) >> _mm_extract_epi32(b, 2) ; \
        uint32 pLsrPrimV128PointwiseUInt32BShiftR_R3 = _mm_extract_epi32(a, 3) >> _mm_extract_epi32(b, 3) ; \
        (dest) = _mm_set_epi32(pLsrPrimV128PointwiseUInt32BShiftR_R3,      \
                               pLsrPrimV128PointwiseUInt32BShiftR_R2,      \
                               pLsrPrimV128PointwiseUInt32BShiftR_R1,      \
                               pLsrPrimV128PointwiseUInt32BShiftR_R0);     \
    } while (0)

#define pLsrPrimV128PointwiseUInt32BShiftL(dest, a, b) \
    do {                                                                \
        uint32 pLsrPrimV128PointwiseUInt32BShiftL_R0 = _mm_extract_epi32(a, 0) << _mm_extract_epi32(b, 0) ; \
        uint32 pLsrPrimV128PointwiseUInt32BShiftL_R1 = _mm_extract_epi32(a, 1) << _mm_extract_epi32(b, 1) ; \
        uint32 pLsrPrimV128PointwiseUInt32BShiftL_R2 = _mm_extract_epi32(a, 2) << _mm_extract_epi32(b, 2) ; \
        uint32 pLsrPrimV128PointwiseUInt32BShiftL_R3 = _mm_extract_epi32(a, 3) << _mm_extract_epi32(b, 3) ; \
        (dest) = _mm_set_epi32(pLsrPrimV128PointwiseUInt32BShiftL_R3,      \
                               pLsrPrimV128PointwiseUInt32BShiftL_R2,      \
                               pLsrPrimV128PointwiseUInt32BShiftL_R1,      \
                               pLsrPrimV128PointwiseUInt32BShiftL_R0);     \
    } while (0)

#define pLsrPrimV128PointwiseSInt32DivT(dest, a, b)                     \
    do {                                                                \
        sint32 pLsrPrimV128PointwiseSInt32DivT_R0 = _mm_extract_epi32(a, 0) / _mm_extract_epi32(b, 0) ; \
        sint32 pLsrPrimV128PointwiseSInt32DivT_R1 = _mm_extract_epi32(a, 1) / _mm_extract_epi32(b, 1) ; \
        sint32 pLsrPrimV128PointwiseSInt32DivT_R2 = _mm_extract_epi32(a, 2) / _mm_extract_epi32(b, 2) ; \
        sint32 pLsrPrimV128PointwiseSInt32DivT_R3 = _mm_extract_epi32(a, 3) / _mm_extract_epi32(b, 3) ; \
        (dest) = _mm_set_epi32(pLsrPrimV128PointwiseSInt32DivT_R3,      \
                               pLsrPrimV128PointwiseSInt32DivT_R2,      \
                               pLsrPrimV128PointwiseSInt32DivT_R1,      \
                               pLsrPrimV128PointwiseSInt32DivT_R0);     \
    } while (0)

#define pLsrPrimV128PointwiseSInt32ModT(dest, a, b)                     \
    do {                                                                \
        sint32 pLsrPrimV128PointwiseSInt32ModT_R0 = _mm_extract_epi32(a, 0) % _mm_extract_epi32(b, 0) ; \
        sint32 pLsrPrimV128PointwiseSInt32ModT_R1 = _mm_extract_epi32(a, 1) % _mm_extract_epi32(b, 1) ; \
        sint32 pLsrPrimV128PointwiseSInt32ModT_R2 = _mm_extract_epi32(a, 2) % _mm_extract_epi32(b, 2) ; \
        sint32 pLsrPrimV128PointwiseSInt32ModT_R3 = _mm_extract_epi32(a, 3) % _mm_extract_epi32(b, 3) ; \
        (dest) = _mm_set_epi32(pLsrPrimV128PointwiseSInt32ModT_R3,      \
                               pLsrPrimV128PointwiseSInt32ModT_R2,      \
                               pLsrPrimV128PointwiseSInt32ModT_R1,      \
                               pLsrPrimV128PointwiseSInt32ModT_R0);     \
    } while (0)


// Multiplication routine for SSE < 4.1
#define u32_mul(u32_mul_dest, u32_mul_a, u32_mul_b)                     \
  do {                                                                  \
    __m128i u32_mul_c = _mm_srli_si128(u32_mul_a, 4);                   \
    __m128i u32_mul_d = _mm_srli_si128(u32_mul_b, 4);                   \
    __m128i u32_mul_u = _mm_mul_epu32(u32_mul_a, u32_mul_b);            \
    __m128i u32_mul_v = _mm_mul_epu32(u32_mul_c, u32_mul_d);            \
    __m128i u32_mul_r1 = _mm_shuffle_epi32(u32_mul_u, 8);               \
    __m128i u32_mul_r2 = _mm_shuffle_epi32(u32_mul_v, 8);               \
    (u32_mul_dest) = _mm_unpacklo_epi32(u32_mul_r1, u32_mul_r2);        \
  } while (0)

#endif

// Reductions (horizontals)

#ifdef PLSR_VECTOR_USE_ASM

#define pLsrPrimV128ReduceAUInt32Plus(dest, init, a) reduceAUInt32Plus(dest, init, a)
#define pLsrPrimV128ReduceASInt32Plus(dest, init, a) reduceAUInt32Plus(dest, init, a)

#define reduceAUInt32Plus(dest, init, a)        \
    do {                                        \
        uint32 reduceAUInt32Plus_c;             \
        __asm__("phaddd %1, %1;\n\t"            \
                "phaddd %1, %1;\n\t"            \
                "movd %1, %0;\n\t"              \
                : "=r"(reduceAUInt32Plus_c)     \
                : "x"(a));                      \
        (dest) = (reduceAUInt32Plus_c + init);  \
    } while (0)

#else

#define pLsrPrimV128ReduceAUInt32Plus(dest, init, a) reduceAUInt32Plus(dest, init, a)
#define pLsrPrimV128ReduceAUInt32Times(dest, init, a) reduceAUInt32Times(dest, init, a)
#define pLsrPrimV128ReduceAUInt32Max(dest, init, a) reduceAUInt32Max(dest, init, a)
#define pLsrPrimV128ReduceAUInt32Min(dest, init, a) reduceAUInt32Min(dest, init, a)

#define pLsrPrimV128ReduceASInt32Plus(dest, init, a) reduceAUInt32Plus(dest, init, a)

#define reduceAUInt32Plus(reduceAUInt32Plus_dest, reduceAUInt32Plus_init, reduceAUInt32Plus_a) \
  do {                                                                  \
    /* probably not a great implementation */                           \
    __m128i reduceAUInt32Plus_tmp = _mm_hadd_epi32(reduceAUInt32Plus_a, reduceAUInt32Plus_a); \
    __m128i reduceAUInt32Plus_tmp2 = _mm_hadd_epi32(reduceAUInt32Plus_tmp, reduceAUInt32Plus_tmp); \
    uint32 reduceAUInt32Plus_tmp3;                                      \
    pLsrPrimV128DataSub0B32(reduceAUInt32Plus_tmp3, reduceAUInt32Plus_tmp2); \
    (reduceAUInt32Plus_dest) = (reduceAUInt32Plus_tmp3 + reduceAUInt32Plus_init); \
  } while (0)

#define reduceAUInt32Times(dest, init, a)                               \
    do {                                                                \
        __m128i reduceAUInt32Times_atmp = _mm_shuffle_epi32(a, _MM_SHUFFLE(0, 0, 3, 2)); \
        __m128i reduceAUInt32Times_b;                                   \
        pLsrPrimV128PointwiseUInt32Times(reduceAUInt32Times_b, reduceAUInt32Times_atmp, a); \
        __m128i reduceAUInt32Times_btmp = _mm_shuffle_epi32(reduceAUInt32Times_b, _MM_SHUFFLE(0, 0, 0, 1)); \
        __m128i reduceAUInt32Times_c;                                   \
        pLsrPrimV128PointwiseUInt32Times(reduceAUInt32Times_c, reduceAUInt32Times_btmp, reduceAUInt32Times_b); \
        uint32 reduceAUInt32Times_r;                                    \
        pLsrPrimV128DataSub0B32(reduceAUInt32Times_r, c);              \
        (dest) = (reduceAUInt32Times_r * (init));                       \
    } while (0)

//inline int max(int a, int b) {
//  return a > b ? a : b;
//}

#define reduceAUInt32Max(dest, init, a)                                 \
    do {                                                                \
        __m128i reduceAUInt32Max_atmp = _mm_shuffle_epi32(a, _MM_SHUFFLE(0, 0, 3, 2)); \
        __m128i reduceAUInt32Max_b = _mm_max_epi32(reduceAUInt32Max_atmp, a); /* note is SSE >= 4.1 */ \
        __m128i reduceAUInt32Max_btmp = _mm_shuffle_epi32(reduceAUInt32Max_b, _MM_SHUFFLE(0, 0, 0, 1)); \
        __m128i reduceAUInt32Max_c = _mm_max_epi32(reduceAUInt32Max_btmp, reduceAUInt32Max_b); \
        uint32 reduceAUInt32Max_r;                                      \
        pLsrPrimV128DataSub0B32(reduceAUInt32Max_r, reduceAUInt32Max_c);                \
        (dest) = (max(reduceAUInt32Max_r, (init)));                     \
    } while (0)                                                         \

#define reduceAUInt32Min(dest, init, a)                                 \
    do {                                                                \
        __m128i reduceAUInt32Min_atmp = _mm_shuffle_epi32(a, _MM_SHUFFLE(0, 0, 3, 2)); \
        __m128i reduceAUInt32Min_b = _mm_min_epi32(reduceAUInt32Min_atmp, a); /* note is SSE >= 4.1 */ \
        __m128i reduceAUInt32Min_btmp = _mm_shuffle_epi32(reduceAUInt32Min_b, _MM_SHUFFLE(0, 0, 0, 1)); \
        __m128i reduceAUInt32Min_c = _mm_min_epi32(reduceAUInt32Min_btmp, reduceAUInt32Min_b); \
        uint32 reduceAUInt32Min_r;                                      \
        pLsrPrimV128DataSub0B32(reduceAUInt32Min_r, c);                \
        (dest) = (min(reduceAUInt32Min_r, (init)));                     \
    } while (0)

#endif

// Data operations

#ifdef PLSR_VECTOR_USE_ASM

#define pLsrPrimV128UInt32Lift(dest, a) vector128UInt32Lift(dest, a)
#define pLsrPrimV128UInt32Const(dest, c0, c1, c2, c3) vector128UInt32Const(dest, c0, c1, c2, c3)
#define pLsrPrimV128UInt32Load(dest, arr, off, idx) vector128UInt32LoadHelp (dest, arr, off, idx)
#define pLsrPrimV128UInt32Store(arr, off, idx, v) vector128UInt32StoreHelp (arr, off, idx, v)

#define vector128UInt32Lift(dest, a)            \
    do {                                        \
        __m128i vector128UInt32Lift_b;          \
        __asm__ ("movd %1, %0;\n\t"             \
                 "pshufd $0x0, %0, %0;\n\t"     \
                 : "=x"(vector128UInt32Lift_b)  \
                 : "r"(a));                     \
        (dest) = vector128UInt32Lift_b;         \
    } while (0)

 // SSE 4.1
  //__m128i dest
  // __asm__ ("pinsrd $0x0, %1, %0;\n\t"
  //         "pinsrd $0x1, %2, %0;\n\t"
  //         "pinsrd $0x2, %3, %0;\n\t"
  //         "pinsrd $0x3, %4, %0;\n\t"
  //         : "=x"(dest)
  //         : "rm"(c0), "rm"(c1), "rm"(c2), "rm"(c3));

#define vector128UInt32Const(dest, c0, c1, c2, c3)              \
    do {                                                        \
        int vector128UInt32Const_c[4];                          \
        __m128i vector128UInt32Const_r;                         \
        vector128UInt32Const_c[0] = c0;                         \
        vector128UInt32Const_c[1] = c1;                         \
        vector128UInt32Const_c[2] = c2;                         \
        vector128UInt32Const_c[3] = c3;                         \
        __asm__("movdqu (%1), %0;"                              \
                : "=x"(vector128UInt32Const_r)                  \
                : "r"(vector128UInt32Const_c));                 \
        dest = vector128UInt32Const_r;                          \
    } while (0)

#define vector128UInt32LoadHelp(dest, arr, off, idx)                    \
  do {                                                                  \
    __m128i* vector128UInt32LoadHelp_marr = (__m128i*)((uint32*)((char*)arr+off) + idx); \
    __m128i vector128UInt32LoadHelp_r;                                  \
    __asm__("movdqu (%1), %0;\n" : "=x"(vector128UInt32LoadHelp_r) : "r"(vector128UInt32LoadHelp_marr)); \
    (dest) = vector128UInt32LoadHelp_r;                                 \
  } while (0)

#define vector128UInt32StoreHelp(arr, off, idx, v)                      \
  do {                                                                  \
    __m128i* vector128UInt32StoreHelp_marr = (__m128i*)((uint32*)((char*)arr+off) + idx); \
    __asm__("movdqu %1, (%0);\n" : : "r"(vector128UInt32StoreHelp_marr), "x"(v)); \
  } while (0)

#define pLsrPrimV128UInt32LoadStrided(dest, arr, off, idx, stride)      \
    do {                                                        \
        pLsrRuntimeError("Strided operations unimplemented");   \
    } while (0)

#define pLsrPrimV128UInt32StoreStrided(arr, off, idx, stride, v)       \
    do {                                                                \
        pLsrRuntimeError("Strided operations unimplemented");           \
    } while (0)

#else

#define pLsrPrimV128UInt32Lift(dest, a) ((dest) = _mm_set1_epi32(a))
#define pLsrPrimV128UInt32Const(dest, c0, c1, c2, c3) ((dest) = _mm_set_epi32(c3, c2, c1, c0))

#define pLsrPrimV128UInt32Load(dest, arr, off, idx) pLsrPrimV128UInt32LoadHelp (dest, arr, off, idx)
#define pLsrPrimV128UInt32Store(arr, off, idx, v) pLsrPrimV128UInt32StoreHelp (arr, off, idx, v)

#define pLsrPrimV128UInt32LoadHelp(dest, arr, off, idx)                 \
    do {                                                                \
        __m128i* pLsrPrimV128UInt32LoadHelp_marr = (__m128i*)((uint32*)((char*)arr+off) + idx); \
        (dest) = _mm_loadu_si128(pLsrPrimV128UInt32LoadHelp_marr);     \
    } while (0)

#define pLsrPrimV128UInt32StoreHelp(arr, off, idx, v)                  \
    do {                                                                \
        __m128i* pLsrPrimV128UInt32StoreHelp_marr = (__m128i*)((uint32*)((char*)arr+off) + idx); \
        _mm_storeu_si128(pLsrPrimV128UInt32StoreHelp_marr, v);         \
    } while (0)

#define pLsrPrimV128UInt32LoadStrided(dest, arr, off, idx, stride)      \
    do {                                                        \
        pLsrRuntimeError("Strided operations unimplemented");   \
    } while (0)

#define pLsrPrimV128UInt32StoreStrided(arr, off, idx, stride, v)       \
    do {                                                                \
        pLsrRuntimeError("Strided operations unimplemented");           \
    } while (0)

#endif

// ================
// UInt32 - 64-bit
// ================

// Arihmetic operations

#ifdef PLSR_VECTOR_USE_ASM


#else

#define pLsrPrimV64PointwiseUInt32Plus(dest, a, b) ((dest) = _mm_add_pi32(a, b))
#define pLsrPrimV64PointwiseUInt32Minus(dest, a, b) ((dest) = _mm_sub_pi32(a, b))
#define pLsrPrimV64PointwiseUInt32Times(dest, a, b) (u32_mul_64(dest, a, b)) //_mm_mullo_pi32(a, b))


// Multiplication route for SSE < 4.1
#define u32_mul_64(dest, a, b)                                          \
    do {                                                                \
        __m64 u32_mul_64_c = _mm_srli_si64(a, 4);                       \
        __m64 u32_mul_64_d = _mm_srli_si64(b, 4);                       \
        __m64 u32_mul_64_u = _mm_mul_su32(a, b);                        \
        __m64 u32_mul_64_v = _mm_mul_su32(u32_mul_64_c, u32_mul_64_d);  \
        (dest) = _mm_unpacklo_pi32(u32_mul_64_u, u32_mul_64_v);         \
    } while (0)

#endif

// Data operations

#ifdef PLSR_VECTOR_USE_ASM



#else

#define pLsrPrimV64UInt32Lift(dest, a) ((dest) = _mm_set1_pi32(a))
#define pLsrPrimV64UInt32Const(dest, c0, c1) ((dest) = _mm_set_pi32(c1, c0))

#endif

// ================
// SInt32 - 128-bit
// ================

#ifdef PLSR_VECTOR_USE_ASM



#else

#define pLsrPrimV128PointwiseSInt32Plus(dest, a, b) ((dest) = _mm_add_epi32(a, b))
#define pLsrPrimV128PointwiseSInt32Minus(dest, a, b) ((dest) = _mm_sub_epi32(a, b))
#define pLsrPrimV128PointwiseSInt32Times(dest, a, b) ((dest) = _mm_mullo_epi32(a, b))
#define pLsrPrimV128PointwiseSInt32Lift(dest, a) ((dest) = _mm_set1_epi32(a))
#define pLsrPrimV128PointwiseSInt32Const(dest, c0, c1, c2, c3) ((dest) = _mm_set_epi32(c3, c2, c1, c0))

#define pLsrPrimV64PointwiseSInt32Plus(dest, a, b) ((dest) = _mm_add_pi32(a, b))
#define pLsrPrimV64PointwiseSInt32Minus(dest, a, b) ((dest) = _mm_sub_pi32(a, b))
#define pLsrPrimV64PointwiseSInt32Times(dest, a, b) ((dest) = _mm_mullo_pi32(a, b))
#define pLsrPrimV64SInt32Lift(dest, a) ((dest) = _mm_set1_pi32(a))
#define pLsrPrimV64SInt32Const(dest, c0, c1) ((dest) = _mm_set_pi32(c1, c0))

#endif

// =====
// Float
// =====

#ifdef PLSR_VECTOR_USE_ASM

static __m128 pLsrPrimV128Float32Zero = {0.0, 0.0, 0.0, 0.0};
#define pLsrPrimV128PointwiseFloat32Plus(dest, a, b)   vector128PointwiseFloat32Plus(dest, a, b)
#define pLsrPrimV128PointwiseFloat32Minus(dest, a, b)  vector128PointwiseFloat32Minus(dest, a, b)
#define pLsrPrimV128PointwiseFloat32Times(dest, a, b)  vector128PointwiseFloat32Times(dest, a, b)
#define pLsrPrimV128PointwiseFloat32Divide(dest, a, b) vector128PointwiseFloat32Divide(dest, a, b)
#define pLsrPrimV128PointwiseFloat32Negate(dest, a) vector128PointwiseFloat32Minus(dest, pLsrPrimV128Float32Zero, a)
#define pLsrPrimV128PointwiseFloat32Max(dest, a, b) vector128PointwiseFloat32Max(dest, a, b)
#define pLsrPrimV128PointwiseFloat32Min(dest, a, b) vector128PointwiseFloat32Min(dest, a, b)

#define vector128PointwiseFloat32Plus(dest, a, b)                       \
    do {                                                                \
        __m128 vector128PointwiseFloat32Plus_a = a;                     \
        __m128 vector128PointwiseFloat32Plus_b = b;                     \
        __asm__ ("addps %1, %0;" : "+x"(vector128PointwiseFloat32Plus_b) : "x"(vector128PointwiseFloat32Plus_a)); \
        (dest) = vector128PointwiseFloat32Plus_b;                       \
    } while (0)

#define vector128PointwiseFloat32Minus(dest, a, b)                      \
    do {                                                                \
        __m128 vector128PointwiseFloat32Minus_a = a;                    \
        __m128 vector128PointwiseFloat32Minus_b = b;                    \
        __asm__ ("subps %1, %0;" : "+x"(vector128PointwiseFloat32Minus_b) : "x"(vector128PointwiseFloat32Minus_a)); \
        (dest) = vector128PointwiseFloat32Minus_b;                      \
    } while (0)

#define vector128PointwiseFloat32Times(dest, a, b)                      \
    do {                                                                \
        __m128 vector128PointwiseFloat32Times_a = a;                    \
        __m128 vector128PointwiseFloat32Times_b = b;                    \
        __asm__ ("mulps %1, %0;" : "+x"(vector128PointwiseFloat32Times_b) : "x"(vector128PointwiseFloat32Times_a)); \
        (dest) = vector128PointwiseFloat32Times_b;                      \
    } while (0)

#define vector128PointwiseFloat32Divide(dest, a, b)                        \
    do {                                                                \
        __m128 vector128PointwiseFloat32Div_a = a;                      \
        __m128 vector128PointwiseFloat32Div_b = b;                      \
        __asm__ ("divps %1, %0;" : "+x"(vector128PointwiseFloat32Div_b) : "x"(vector128PointwiseFloat32Div_a)); \
        (dest) = vector128PointwiseFloat32Div_b;                        \
    } while (0)

#define vector128PointwiseFloat32Max(dest, a, b)                        \
    do {                                                                \
        __m128 vector128PointwiseFloat32Max_a = a;                      \
        __m128 vector128PointwiseFloat32Max_b = b;                      \
        __asm__ ("maxps %1, %0;" : "+x"(vector128PointwiseFloat32Max_b) : "x"(vector128PointwiseFloat32Max_a)); \
        (dest) = vector128PointwiseFloat32Max_b;                        \
    } while (0)

#define vector128PointwiseFloat32Min(dest, a, b)                        \
    do {                                                                \
        __m128 vector128PointwiseFloat32Min_a = a;                      \
        __m128 vector128PointwiseFloat32Min_b = b;                      \
        __asm__ ("maxps %1, %0;" : "+x"(vector128PointwiseFloat32Min_b) : "x"(vector128PointwiseFloat32Min_a)); \
        (dest) = vector128PointwiseFloat32Min_b;                        \
    } while (0)

#else

static __m128 pLsrPrimV128Float32Zero = {0.0, 0.0, 0.0, 0.0};
#define pLsrPrimV128PointwiseFloat32Plus(dest, a, b) ((dest) = _mm_add_ps(a, b))
#define pLsrPrimV128PointwiseFloat32Minus(dest, a, b) ((dest) = _mm_sub_ps(a, b))
#define pLsrPrimV128PointwiseFloat32Times(dest, a, b) ((dest) = _mm_mul_ps(a, b))
#define pLsrPrimV128PointwiseFloat32Divide(dest, a, b) ((dest) = _mm_div_ps(a, b))
#define pLsrPrimV128PointwiseFloat32Negate(dest, a) ((dest) = _mm_sub_ps(pLsrPrimV128Float32Zero, a))
#define pLsrPrimV128PointwiseFloat32Max(a, b) ((dest) = _mm_max_ps(a, b))
#define pLsrPrimV128PointwiseFloat32Min(dest, a, b) ((dest) = _mm_min_ps(a, b))
#define pLsrPrimV128PointwiseFloat32Sqrt(dest, a) ((dest) = _mm_sqrt_ps(a))

#endif

// Reductions (horizontals)

// =====
// Float
// =====

#ifdef PLSR_VECTOR_USE_ASM

#define pLsrPrimV128ReduceAFloat32Plus(dest, init, a) reduceAFloat32Plus(dest, init, a)
#define pLsrPrimV128ReduceAFloat32Max(dest, init, a) reduceAFloat32Max(dest, init, a)

#define reduceAFloat32Plus(dest, init, a)                               \
  do {                                                                  \
  __m128 rAFloat32Plus_a_ = a;                                          \
  float rAFloat32Plus_dest_;                                            \
  __asm__("haddps %1, %1;\n\t"                                          \
          "pextrd $0x0, %1, %0;\n\t"                                    \
          : "=r"(rAFloat32Plus_dest_)                                   \
          : "x"(rAFloat32Plus_a_));                                     \
  (dest) = (rAFloat32Plus_dest_ + (init));                             \
  } while (0)

#define reduceAFloat32Max(dest, init, a)                                \
    do {                                                                \
        __m128 reduceAFloat32Max_a_ = a;                                \
        float reduceAFloat32Max_dest_;                                  \
        __asm__ ("shuhfps $0x0e, %1, %1;\n\t"                           \
                 "max_ps %1, %1;\n\t"                                   \
                 "shufps $0x01, %1, %1;\n\t"                            \
                 "max_ps %1, %1;\n\t"                                   \
                 "pextrd $0x0, %1, %0; \n\t"                            \
                 : "=r"(reduceAFloat32Max_dest_)                        \
                 : "x"(reduceAFloat32Max_a_));                          \
        (dest) = (max(reduceAFloat32Max_dest_, init));                  \
    } while (0)

#else

#define pLsrPrimV128ReduceAFloat32Plus(dest, init, a) reduceAFloat32Plus(dest, init, a)
#define pLsrPrimV128ReduceAFloat32Times(dest, init, a) reduceAFloat32Times(dest, init, a)
#define pLsrPrimV128ReduceAFloat32Max(dest, init, a) reduceAFloat32Max(dest, init, a)
#define pLsrPrimV128ReduceAFloat32Min(dest, init, a) reduceAFloat32Min(dest, init, a)

#define reduceAFloat32Plus(dest, init, a)                               \
    do {                                                                \
        __m128 reduceAFloat32Plus_a_ = a;                               \
        __m128 reduceAFloat32Plus_tmp = _mm_hadd_ps(reduceAFloat32Plus_a_, reduceAFloat32Plus_a_); \
        __m128 reduceAFloat32Plus_tmp2 = _mm_hadd_ps(reduceAFloat32Plus_tmp, reduceAFloat32Plus_tmp); \
        float reduceAFloat32Plus_dest_;                                 \
        pLsrPrimV128DataSubF32(reduceAFloat32Plus_dest_, 0, reduceAFloat32Plus_tmp2); \
        (dest) =  (reduceAFloat32Plus_dest_ + (init));                  \
    } while (0)

#define reduceAFloat32Times(dest, init, a)                      \
    do {                                                        \
        __m128 reduceAFloat32Times_a_ = a;                             \
        __m128 reduceAFloat32Times_atmp = _mm_shuffle_ps(reduceAFloat32Times_a_, reduceAFloat32Times_a_, _MM_SHUFFLE(0, 0, 3, 2)); \
        __m128 reduceAFloat32Times_b;                                   \
        pLsrPrimV128PointwiseFloat32Times(reduceAFloat32Times_b, reduceAFloat32Times_atmp, reduceAFloat32Times_a_); \
        __m128 reduceAFloat32Times_btmp = _mm_shuffle_ps(reduceAFloat32Times_b, reduceAFloat32Times_b, _MM_SHUFFLE(0, 0, 0, 1)); \
        __m128 reduceAFloat32Times_c;                                   \
        pLsrPrimV128PointwiseFloat32Times(reduceAFloat32Times_c, reduceAFloat32Times_btmp, reduceAFloat32Times_b); \
        float reduceAFloat32Times_dest_;                                \
        pLsrPrimV128DataSubF32(reduceAFloat32Times_dest_, 0, reduceAFloat32Times_c); \
        (dest) = (reduceAFloat32Times_dest_ * (init));                  \
    } while (0)

#define reduceAFloat32Max(init, a)                              \
    do {                                                        \
        __m128 reduceAFloat32Max_a_ = a;                                \
        __m128 reduceAFloat32Max_atmp = _mm_shuffle_ps(reduceAFloat32Max_a_, reduceAFloat32Max_a_, _MM_SHUFFLE(0, 0, 3, 2)); \
        __m128 reduceAFloat32Max_b = _mm_max_ps(reduceAFloat32Max_atmp, reduceAFloat32Max_a); \
        __m128 reduceAFloat32Max_btmp = _mm_shuffle_ps(reduceAFloat32Max_b, reduceAFloat32Max_b, _MM_SHUFFLE(0, 0, 0, 1)); \
        __m128 reduceAFloat32Max_c = _mm_max_ps(reduceAFloat32Max_btmp, reduceAFloat32Max_b); \
        float reduceAFloat32Max_dest_;                                  \
        pLsrPrimV128DataSubF32(reduceAFloat32Max_dest_, 0, reduceAFloat32Max_c); \
        (dest) = max(reduceAFloat32Max_, (init));                       \
    } while (0)

#define reduceAFloat32Min(init, a)                              \
    do {                                                        \
        __m128 reduceAFloat32Min_a_ = a;                        \
        __m128 reduceAFloat32Min_atmp = _mm_shuffle_ps(reduceAFloat32Min_a_, reduceAFloat32Min_a_, _MM_SHUFFLE(0, 0, 3, 2)); \
        __m128 reduceAFloat32Min_b = _mm_min_ps(reduceAFloat32Min_atmp, reduceAFloat32Min_a); \
        __m128 reduceAFloat32Min_btmp = _mm_shuffle_ps(reduceAFloat32Min_b, reduceAFloat32Min_b, _MM_SHUFFLE(0, 0, 0, 1)); \
        __m128 reduceAFloat32Min_c = _mm_min_ps(reduceAFloat32Min_btmp, reduceAFloat32Min_b); \
        float reduceAFloat32Min_dest_;                                  \
        pLsrPrimV128DataSubF32(reduceAFloat32Min_dest_, 0, reduceAFloat32Min_c); \
        (dest) = min(reduceAFloat32Min_, (init));                       \
    } while (0)

#endif

// =====
// Double
// =====


#ifdef PLSR_VECTOR_USE_ASM
#else

#define pLsrPrimV128ReduceAFloat64Plus(dest, init, a) reduceAFloat64Plus(dest, init, a)
#define pLsrPrimV128ReduceAFloat64Times(dest, init, a) reduceAFloat64Times(dest, init, a)
#define pLsrPrimV128ReduceAFloat64Max(dest, init, a) reduceAFloat64Max(dest, init, a)
#define pLsrPrimV128ReduceAFloat64Min(dest, init, a) reduceAFloat64Min(dest, init, a)

#define reduceAFloat64Plus(dest, init, a)                               \
    do {                                                                \
        __m128d reduceAFloat64Plus_a_ = a;                               \
        __m128d reduceAFloat64Plus_tmp = _mm_hadd_pd(reduceAFloat64Plus_a_, reduceAFloat64Plus_a_); \
        double reduceAFloat64Plus_dest_;                                 \
        pLsrPrimV128DataSubF64(reduceAFloat64Plus_dest_, 0, reduceAFloat64Plus_tmp); \
        (dest) =  (reduceAFloat64Plus_dest_ + (init));                  \
    } while (0)

#define reduceAFloat64Times(dest, init, a)                             \
    do {                                                               \
        __m128d reduceAFloat64Times_a_ = a;                              \
        double reduceAFloat64Times_dest0_;                              \
        double reduceAFloat64Times_dest1_;                              \
        pLsrPrimV128DataSubF64(reduceAFloat64Times_dest0_, 0, reduceAFloat64Times_a_); \
        pLsrPrimV128DataSubF64(reduceAFloat64Times_dest1_, 1, reduceAFloat64Times_a_); \
        (dest) = reduceAFloat64Times_dest0_ * reduceAFloat64Times_dest0_ * (init); \
    } while (0)

#define reduceAFloat64Max(init, a)                              \
    do {                                                               \
        __m128d reduceAFloat64Max_a_ = a;                              \
        double reduceAFloat64Max_dest0_;                              \
        double reduceAFloat64Max_dest1_;                              \
        pLsrPrimV128DataSubF64(reduceAFloat64Max_dest0_, 0, reduceAFloat64Max_a_); \
        pLsrPrimV128DataSubF64(reduceAFloat64Max_dest1_, 1, reduceAFloat64Max_a_); \
        (dest) = max(max(reduceAFloat64Max_dest0_, reduceAFloat64Max_dest0_), (init)); \
    } while (0)

#define reduceAFloat64Min(init, a)                              \
    do {                                                               \
        __m128 reduceAFloat64Min_a_ = a;                              \
        double reduceAFloat64Min_dest0_;                              \
        double reduceAFloat64Min_dest1_;                              \
        pLsrPrimV128DataSubF64(reduceAFloat64Min_dest0_, 0, reduceAFloat64Min_a_); \
        pLsrPrimV128DataSubF64(reduceAFloat64Min_dest1_, 1, reduceAFloat64Min_a_); \
        (dest) = min(min(reduceAFloat64Min_dest0_, reduceAFloat64Min_dest0_), (init)); \
    } while (0)

#endif


#ifdef PLSR_VECTOR_USE_ASM

#define pLsrPrimV128Float32Const(dest, c0, c1, c2, c3) vector128Float32Const(dest, c0, c1, c2, c3)
#define pLsrPrimV128Float32Lift(dest, a) vector128Float32Lift(dest, a)
#define pLsrPrimV128Float32Load(dest, arr, off, idx) pLsrPrimV128Float32LoadHelp (dest, arr, off, idx)
#define pLsrPrimV128Float32Store(arr, off, idx, v) pLsrPrimV128Float32StoreHelp (arr, off, idx, v)

#define pLsrPrimV128Float32LoadVector(dest, arr, off, idx) pLsrPrimV128Float32LoadVectorHelp (dest, arr, off, idx)

/* Suspicous register constraint XXX -leaf */
#define vector128Float32Lift(dest, a)                    \
  do {                                                   \
    float32 vector128Float32Lift_a_ = a;                 \
    __m128 vector128Float32Lift_dest_;                   \
    __asm__ ("movss %1, %0;\n\t"                         \
             "shufps $0x0, %0, %0;"                      \
             : "=x"(vector128Float32Lift_dest_)          \
             : "xm"(vector128Float32Lift_a_));           \
    (dest) = vector128Float32Lift_dest_;                 \
  } while (0)

#define vector128Float32Const(dest, c0, c1, c2, c3)                     \
  do {                                                                  \
    __m128 vector128Float32Const_dest_;                                 \
    float vector128Float32Const_c[4];                                   \
    vector128Float32Const_c[0] = c0;                                    \
    vector128Float32Const_c[1] = c1;                                    \
    vector128Float32Const_c[2] = c2;                                    \
    vector128Float32Const_c[3] = c3;                                    \
    __asm__("movups (%1), %0;"                                          \
            : "=x"(vector128Float32Const_dest_)                         \
            : "r"(vector128Float32Const_c));                            \
    (dest) = vector128Float32Const_dest_;                               \
  } while(0);

#define pLsrPrimV128Float32LoadVectorHelp(dest, arr, off, idx)          \
    do {                                                                \
        float32* pLsrPrimV128Float32LoadVectorHelp_ptr;                  \
        float32 pLsrPrimV128Float32LoadVectorHelp_marr[4];              \
        pLsrPrimV128DataSubB32(pLsrPrimV128Float32LoadVectorHelp_ptr, 0, arr); \
        pLsrPrimV128Float32LoadVectorHelp_marr[0] =                     \
            *((float32*) (((char*)pLsrPrimV128Float32LoadVectorHelp_ptr + off) + idx)); \
        pLsrPrimV128DataSubB32(pLsrPrimV128Float32LoadVectorHelp_ptr, 1, arr); \
        pLsrPrimV128Float32LoadVectorHelp_marr[1] =                     \
            *((float32*) (((char*)pLsrPrimV128Float32LoadVectorHelp_ptr + off) + idx)); \
        pLsrPrimV128DataSubB32(pLsrPrimV128Float32LoadVectorHelp_ptr, 2, arr); \
        pLsrPrimV128Float32LoadVectorHelp_marr[2] =                     \
            *((float32*) (((char*)pLsrPrimV128Float32LoadVectorHelp_ptr + off) + idx)); \
        pLsrPrimV128DataSubB32(pLsrPrimV128Float32LoadVectorHelp_ptr, 3, arr); \
        pLsrPrimV128Float32LoadVectorHelp_marr[3] =                     \
            *((float32*) (((char*)pLsrPrimV128Float32LoadVectorHelp_ptr + off) + idx)); \
        pLsrPrimV128Float32Load(dest, &pLsrPrimV128Float32LoadVectorHelp_marr, 0, 0); \
    } while (0)


#define pLsrPrimV128Float32LoadHelp(dest, arr, off, idx)       \
    do {                                                                \
        __m128* pLsrPrimV128Float32LoadHelp_marr = (__m128*)((float*)((char*)arr+off) + idx); \
        __m128 pLsrPrimV128Float32LoadHelp_dest_;                      \
        __asm__("movups (%1), %0;"                                      \
                : "=x"(pLsrPrimV128Float32LoadHelp_dest_)              \
                : "r"(pLsrPrimV128Float32LoadHelp_marr));              \
        (dest) = pLsrPrimV128Float32LoadHelp_dest_;                    \
    } while (0)

#define pLsrPrimV128Float32StoreHelp(arr, off, idx, v) \
    do {                                                \
        float* pLsrPrimV128Float32StoreHelp_marr = ((float*)((char*)arr+off) + idx); \
        __asm__("movups %1, (%0);"                                      \
                :                                                       \
                : "r"(pLsrPrimV128Float32StoreHelp_marr), "x"(v));    \
    } while (0)

#else

#define pLsrPrimV128Float32Const(dest, c0, c1, c2, c3) ((dest) = _mm_set_ps (c3, c2, c1, c0))
#define pLsrPrimV128Float32Lift(dest, a) ((dest) = _mm_set1_ps(a))
#define pLsrPrimV128Float32Load(dest, arr, off, idx) pLsrPrimV128Float32LoadHelp (dest, arr, off, idx)
#define pLsrPrimV128Float32LoadVector(dest, arr, off, idx) \
    pLsrPrimV128Float32LoadVectorHelp (dest, arr, off, idx)
#define pLsrPrimV128Float32Store(arr, off, idx, v) pLsrPrimV128Float32StoreHelp (arr, off, idx, v)

#define pLsrPrimV128Float32LoadHelp(dest, arr, off, idx)               \
    do {                                                                \
        float32* pLsrPrimV128Float32LoadHelp_marr = (float32*)((char*)arr+off)+idx; \
        (dest) = _mm_loadu_ps(pLsrPrimV128Float32LoadHelp_marr);       \
    } while (0)

#define pLsrPrimV128Float32StoreHelp(arr, off, idx, v)                 \
    do {                                                                \
        float32* pLsrPrimV128Float32StoreHelp_marr = (float32*)((char*)arr+off) + idx; \
        _mm_storeu_ps(pLsrPrimV128Float32StoreHelp_marr, v);           \
    } while (0)

#define pLsrPrimV128Float32LoadVectorHelp(dest, arr, off, idx)          \
    do {                                                                \
        float32* pLsrPrimV128Float32LoadVectorHelp_ptr0;                \
        float32* pLsrPrimV128Float32LoadVectorHelp_ptr1;                \
        float32* pLsrPrimV128Float32LoadVectorHelp_ptr2;                \
        float32* pLsrPrimV128Float32LoadVectorHelp_ptr3;                \
                                                                        \
        pLsrPrimV128Float32LoadVectorHelp_ptr0 = (float*)_mm_extract_epi32(arr, 0); \
        pLsrPrimV128Float32LoadVectorHelp_ptr1 = (float*)_mm_extract_epi32(arr, 1); \
        pLsrPrimV128Float32LoadVectorHelp_ptr2 = (float*)_mm_extract_epi32(arr, 2); \
        pLsrPrimV128Float32LoadVectorHelp_ptr3 = (float*)_mm_extract_epi32(arr, 3); \
        (dest) =                                                        \
            _mm_set_ps(*(float32*)(((char*)pLsrPrimV128Float32LoadVectorHelp_ptr0 + off) + idx), \
                       *(float32*)(((char*)pLsrPrimV128Float32LoadVectorHelp_ptr1 + off) + idx), \
                       *(float32*)(((char*)pLsrPrimV128Float32LoadVectorHelp_ptr2 + off) + idx), \
                       *(float32*)(((char*)pLsrPrimV128Float32LoadVectorHelp_ptr3 + off) + idx)); \
    } while (0)

#define pLsrPrimV128Float32LoadStrided(dest, arr, off, idx, stride)    \
    do {                                                                \
        pLsrRuntimeError("Strided operations unimplemented");           \
    } while (0)

#define pLsrPrimV128Float32LoadVectorStrided(dest, arr, off, idx, stride)    \
    do {                                                                \
        pLsrRuntimeError("Strided operations unimplemented");           \
    } while (0)

#define pLsrPrimV128Float32StoreStrided(arr, off, idx, stride, v)      \
    do {                                                                \
        pLsrRuntimeError("Strided operations unimplemented");           \
    } while (0)

#endif

// ======
// Double
// ======

#ifdef PLSR_VECTOR_USE_ASM



#else

#define pLsrPrimV128PointwiseFloat64Plus(dest, a, b) ((dest) = _mm_add_pd(a, b))
#define pLsrPrimV128PointwiseFloat64Minus(dest, a, b) ((dest) = _mm_sub_pd(a, b))
#define pLsrPrimV128PointwiseFloat64Times(dest, a, b) ((dest) = _mm_mul_pd(a, b))
#define pLsrPrimV128PointwiseFloat64Divide(dest, a, b) ((dest) = _mm_div_pd(a, b))

#define pLsrPrimV128Float64Const(dest, c0, c1) ((dest) = _mm_set_pd(c1, c0))
#define pLsrPrimV128Float64Lift(dest, a) ((dest) = _mm_set1_pd(a))
#define pLsrPrimV128Float64Load(dest, arr, off, idx) pLsrPrimV128Float64LoadHelp (dest, arr, off, idx)
#define pLsrPrimV128Float64Store(arr, off, idx, v) pLsrPrimV128Float64StoreHelp (arr, off, idx, v)

#define pLsrPrimV128Float64LoadHelp(dest, arr, off, idx)               \
    do {                                                                \
        float64* pLsrPrimV128Float64LoadHelp_marr = (float64*)((char*)arr+off) + idx; \
        (dest) = _mm_loadu_pd(pLsrPrimV128Float64LoadHelp_marr);       \
    } while (0)

#define pLsrPrimV128Float64StoreHelp(arr, off, idx, v)                 \
    do {                                                                \
        float64* pLsrPrimV128Float64StoreHelp_marr = (float64*)((char*)arr+off) + idx; \
        _mm_storeu_pd(pLsrPrimV128Float64StoreHelp_marr, v);           \
    } while (0)

#endif

#define pLsrPrimV128Float64LoadStrided(dest, arr, off, idx, stride)     \
    do {                                                        \
        pLsrRuntimeError("Strided operations unimplemented");   \
    } while (0)

#define pLsrPrimV128Float64StoreStrided(arr, off, idx, stride, v)      \
    do {                                                        \
        pLsrRuntimeError("Strided operations unimplemented");   \
    } while (0)



// ================
// Ref - 128-bit
// ================

#ifdef PLSR_VECTOR_USE_ASM

#define pLsrPrimV128RefLift(dest, a) vector128RefLift(dest, a)
#define pLsrPrimV128RefConst(dest, c0, c1, c2, c3) vector128RefConst(dest, c0, c1, c2, c3)
#define pLsrPrimV128RefLoad(dest, arr, off, idx) vector128RefLoadHelp (dest, arr, off, idx)
#define pLsrPrimV128RefStore(arr, off, idx, v) vector128RefStoreHelp (arr, off, idx, v)

#define vector128RefLift(dest, a)            \
    do {                                        \
        __m128r vector128RefLift_b;          \
        __asm__ ("movd %1, %0;\n\t"             \
                 "pshufd $0x0, %0, %0;\n\t"     \
                 : "=x"(vector128RefLift_b)  \
                 : "r"(a));                     \
        (dest) = vector128RefLift_b;         \
    } while (0)

 // SSE 4.1
  //__m128r dest
  // __asm__ ("pinsrd $0x0, %1, %0;\n\t"
  //         "pinsrd $0x1, %2, %0;\n\t"
  //         "pinsrd $0x2, %3, %0;\n\t"
  //         "pinsrd $0x3, %4, %0;\n\t"
  //         : "=x"(dest)
  //         : "rm"(c0), "rm"(c1), "rm"(c2), "rm"(c3));

#define vector128RefConst(dest, c0, c1, c2, c3)              \
    do {                                                        \
        int vector128RefConst_c[4];                          \
        __m128r vector128RefConst_r;                         \
        vector128RefConst_c[0] = c0;                         \
        vector128RefConst_c[1] = c1;                         \
        vector128RefConst_c[2] = c2;                         \
        vector128RefConst_c[3] = c3;                         \
        __asm__("movdqu (%1), %0;"                              \
                : "=x"(vector128RefConst_r)                  \
                : "r"(vector128RefConst_c));                 \
        dest = vector128RefConst_r;                          \
    } while (0)

#define vector128RefLoadHelp(dest, arr, off, idx)                    \
  do {                                                                  \
    __m128r* vector128RefLoadHelp_marr = (__m128r*)((uint32*)((char*)arr+off) + idx); \
    __m128r vector128RefLoadHelp_r;                                  \
    __asm__("movdqu (%1), %0;\n" : "=x"(vector128RefLoadHelp_r) : "r"(vector128RefLoadHelp_marr)); \
    (dest) = vector128RefLoadHelp_r;                                 \
  } while (0)

#define vector128RefStoreHelp(arr, off, idx, v)                      \
  do {                                                                  \
    __m128r* vector128RefStoreHelp_marr = (__m128r*)((uint32*)((char*)arr+off) + idx); \
    __asm__("movdqu %1, (%0);\n" : : "r"(vector128RefStoreHelp_marr), "x"(v)); \
  } while (0)

#define pLsrPrimV128RefLoadStrided(dest, arr, off, idx, stride)      \
    do {                                                        \
        pLsrRuntimeError("Strided operations unimplemented");   \
    } while (0)

#define pLsrPrimV128RefStoreStrided(arr, off, idx, stride, v)       \
    do {                                                                \
        pLsrRuntimeError("Strided operations unimplemented");           \
    } while (0)

#else

#define pLsrPrimV128RefLoad(dest, arr, off, idx) pLsrPrimV128UInt32Load(dest, arr, off, idx)

#define pLsrPrimV128RefLoadStrided(dest, arr, off, idx, stride)      \
    do {                                                        \
        pLsrRuntimeError("Strided operations unimplemented");   \
    } while (0)

#endif

#endif /* _PLSR_PRIMS_VECTOR_SSE_H_ */
