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

#ifndef _PLSR_PRIMS_VECTOR_AVX_H_
#define _PLSR_PRIMS_VECTOR_AVX_H_

#ifdef P_USE_PILLAR
#  pragma pillar_managed(off)
#  define to __to__
#endif /* P_USE_PILLAR */


#ifdef P_USE_PILLAR
#  undef to
#  pragma pillar_managed(on)
#endif /* P_USE_PILLAR */

//#define __pillar2c__ 1

#ifdef P_USE_PILLAR
#define __ICL_INTRINCC __pcdecl
#endif /* P_USE_PILLAR */
#include <immintrin.h>

typedef __m256 PlsrVector256F32;
typedef __m256d PlsrVector256F64;
typedef __m256i PlsrVector256B32;
typedef __m256i PlsrVector256Ref;

typedef __m256 PlsrVectorMask256Fs32;
typedef __m256 PlsrVectorMask256Fs64;

#ifdef TWOVEC64
typedef struct two__m128i {
  __m128i low,high;
} two__m128i;

typedef two__m128i PlsrVector256B64;
#elif defined ONEVEC64
typedef __m256i PlsrVector256B64;
#else
typedef struct PlsrVector256B64 {
  __int64 values[4];
} PlsrVector256B64;
#endif

#if 0
#define pLsrPrimV256PointwiseFloat32LT(dest, a, b) ((dest) = _mm256_cmp_ps((a),(b),_CMP_LT_OS))
#else
#define pLsrPrimV256CompareFloat32LT(dest, a, b) ((dest) = _mm256_cmp_ps((a),(b),_CMP_LT_OS))
#define pLsrPrimV256CompareFloat64LT(dest, a, b) ((dest) = _mm256_cmp_pd((a),(b),_CMP_LT_OS))
#define pLsrPrimV256CompareFloat32LE(dest, a, b) ((dest) = _mm256_cmp_ps((a),(b),_CMP_LE_OS))
#define pLsrPrimV256CompareFloat64LE(dest, a, b) ((dest) = _mm256_cmp_pd((a),(b),_CMP_LE_OS))
#endif
#define pLsrPrimV256PointwiseFloat32Exp(dest, a) ((dest) = _mm256_exp_ps((a)))
#define pLsrPrimV256PointwiseFloat32Ln(dest, a) ((dest) = _mm256_log_ps((a)))
#define pLsrPrimV256PointwiseFloat64Pow(dest, a, b) ((dest) = _mm256_pow_pd((a), (b)))
#define pLsrPrimV256DataBlendF32(dest, mask, a, b) ((dest) =  _mm256_blendv_ps((b),(a),(mask)))
#define pLsrPrimV256DataBlendB32(dest, mask, a, b) ((dest) =  _mm256_blendv_ps((b),(a),(mask)))
#define pLsrPrimV256DataBlendF64(dest, mask, a, b) ((dest) =  _mm256_blendv_pd((b),(a),(mask)))
#define pLsrPrimV256DataBlendB64(dest, mask, a, b) ((dest) =  _mm256_blendv_pd((b),(a),(mask)))


// *****************************************************************************
// Conversions
// *****************************************************************************

#define pLsrPrimV256256ConvertFloat32FromUInt32(dest, a) ((dest) = _mm256_cvtepi32_ps(a))
#define pLsrPrimV256256ConvertUInt32FromFloat32(dest, a) ((dest) = _mm256_cvtps_epi32(a))

#define pLsrPrimV256128ConvertFloat64FromFloat32(dest, a)  ((dest) = _mm256_cvtps_pd(a))

// vec128 -> vec256
#define pLsrPrimV256U32ToF64(dest, a) ((dest) = _mm256__cvtepi32_pd (_mm_movpi64_epi64(a)))
#define pLsrPrimV256F64ToU32(dest, a) ((dest) = _mm256__movepi64_pi64 (_mm256_cvtpd_epi32(a)))

#define pLsrPrimV256256CastSInt32ToFloat32(dest, a) ((dest) = _mm256_cvtepi32_ps(a))
#define pLsrPrimV256128CastSInt32ToFloat64(dest, a) ((dest) = _mm256_cvtepi32_pd(a))
#define pLsrPrimV256256CastFloat32ToSInt32(dest, a) ((dest) = _mm256_cvttps_epi32(a))
#define pLsrPrimV256256CastSInt32ToUInt32(dest, a) ((dest) = a)
#define pLsrPrimV256256CastUInt32ToSInt32(dest, a) ((dest) = a)

#define pLsrPrimV256256ConvertUInt32FromSInt32(dest, a) ((dest) = a)
#define pLsrPrimV256256ConvertSInt32FromUInt32(dest, a) ((dest) = a)



#ifdef TWOVEC64
#elif defined ONEVEC64
#else
#define pLsrPrimV128256CastSInt64ToFloat32(dest, a) (dest) = _mm_set_ps((a).values[3], (a).values[2], (a).values[1], (a).values[0])
#define pLsrPrimV256128CastFloat32ToSInt64(dest, a) \
  do { \
    float in[4]; \
    _mm_store_ps(in,a); \
    dest.values[0] = in[0]; \
    dest.values[1] = in[1]; \
    dest.values[2] = in[2]; \
    dest.values[3] = in[3]; \
  } while(0)

#endif

// *****************************************************************************
// Data operations on the more abstract B32/F32/F64 types
// *****************************************************************************

#define pLsrPrimV256DataVectorB32(dest, c0, c1, c2, c3, c4, c5, c6, c7) \
  pLsrPrimV256UInt32Const(dest, c0, c1, c2, c3, c4, c5, c6, c7)

#define pLsrPrimV256DataVectorB64(dest, c0, c1, c2, c3) \
  pLsrPrimV256UInt64Const(dest, c0, c1, c2, c3)

#define pLsrPrimV256DataVectorF32(dest, c0, c1, c2, c3, c4, c5, c6, c7) \
  pLsrPrimV256Float32Const(dest, c0, c1, c2, c3, c4, c5, c6, c7)

#define pLsrPrimV256DataVectorF64(dest, c0, c1, c2, c3) \
  pLsrPrimV256Float64Const(dest, c0, c1, c2, c3)

#define pLsrPrimV256DataBroadcastB32(dest, c0) pLsrPrimV256UInt32Lift(dest, c0)
#define pLsrPrimV256DataBroadcastB64(dest, c0) pLsrPrimV256UInt64Lift(dest, c0)
#define pLsrPrimV256DataBroadcastF32(dest, c0) pLsrPrimV256Float32Lift(dest, c0)
#define pLsrPrimV256DataBroadcastF64(dest, c0) pLsrPrimV256Float64Lift(dest, c0)

 // Subscripting operations (chose between the lower or higher part)


#define pLsrPrimV256DataSub0B32(dest, a) pLsrPrimV256DataSubB32L(dest, 0, a)
#define pLsrPrimV256DataSub1B32(dest, a) pLsrPrimV256DataSubB32L(dest, 1, a)
#define pLsrPrimV256DataSub2B32(dest, a) pLsrPrimV256DataSubB32L(dest, 2, a)
#define pLsrPrimV256DataSub3B32(dest, a) pLsrPrimV256DataSubB32L(dest, 3, a)
#define pLsrPrimV256DataSub4B32(dest, a) pLsrPrimV256DataSubB32H(dest, 0, a)
#define pLsrPrimV256DataSub5B32(dest, a) pLsrPrimV256DataSubB32H(dest, 1, a)
#define pLsrPrimV256DataSub6B32(dest, a) pLsrPrimV256DataSubB32H(dest, 2, a)
#define pLsrPrimV256DataSub7B32(dest, a) pLsrPrimV256DataSubB32H(dest, 3, a)

#define pLsrPrimV256DataSub0F32(dest, a) pLsrPrimV256DataSubF32L(dest, 0, a)
#define pLsrPrimV256DataSub1F32(dest, a) pLsrPrimV256DataSubF32L(dest, 1, a)
#define pLsrPrimV256DataSub2F32(dest, a) pLsrPrimV256DataSubF32L(dest, 2, a)
#define pLsrPrimV256DataSub3F32(dest, a) pLsrPrimV256DataSubF32L(dest, 3, a)
#define pLsrPrimV256DataSub4F32(dest, a) pLsrPrimV256DataSubF32H(dest, 0, a)
#define pLsrPrimV256DataSub5F32(dest, a) pLsrPrimV256DataSubF32H(dest, 1, a)
#define pLsrPrimV256DataSub6F32(dest, a) pLsrPrimV256DataSubF32H(dest, 2, a)
#define pLsrPrimV256DataSub7F32(dest, a) pLsrPrimV256DataSubF32H(dest, 3, a)

#define pLsrPrimV256DataSubB32L(dest, sub, a) ((dest) = pLsrPrimV256DataSubB32L_help(sub, a))
static inline int pLsrPrimV256DataSubB32L_help(int sub, __m256i a) {
  __m128i loA = _mm256_extractf128_si256(a, 0);
  return (_mm_extract_epi32(loA, sub));
}
#define pLsrPrimV256DataSubB32H(dest, sub, a) ((dest) = pLsrPrimV256DataSubB32H_help(sub, a))
static inline int pLsrPrimV256DataSubB32H_help(int sub, __m256i a) {
  __m128i hiA = _mm256_extractf128_si256(a, 1);
  return (_mm_extract_epi32(hiA, sub));
}

#define pLsrPrimV256DataSubF32H(dest, sub, a) ((dest) = pLsrPrimV256DataSubF32H_help(sub, a))
static inline float pLsrPrimV256DataSubF32H_help(int sub, __m256 a) {
  //  float* out = (float*)malloc(sizeof(float));
  __m128 hiA = _mm256_extractf128_ps(a, 1);
  //__m128 outA = mm_extract_ps(out, hiA, sub);
  //  _mm_move_ss(out, outA);
  //  return *out;
  float out;
  _MM_EXTRACT_FLOAT(out, hiA, sub);
  return out;

}

#define pLsrPrimV256DataSubF32L(dest, sub, a) ((dest) = pLsrPrimV256DataSubF32L_help(sub, a))
static inline float pLsrPrimV256DataSubF32L_help(int sub, __m256 a) {
  //  float* out = (float*)malloc(sizeof(float));
  __m128 hiA = _mm256_extractf128_ps(a, 0);
  //  __m128 outA = mm_extract_ps(out, hiA, sub);
  //  _mm_move_ss(out, outA);
  float out;
  _MM_EXTRACT_FLOAT(out, hiA, sub);
  return out;
}

#define pLsrPrimV256DataSub0F64(dest, a) pLsrPrimV256DataSubF64(dest, 0, a)
#define pLsrPrimV256DataSub1F64(dest, a) pLsrPrimV256DataSubF64(dest, 1, a)
#define pLsrPrimV256DataSub2F64(dest, a) pLsrPrimV256DataSubF64(dest, 2, a)
#define pLsrPrimV256DataSub3F64(dest, a) pLsrPrimV256DataSubF64(dest, 3, a)

#define pLsrPrimV256DataSubF64(dest, sub, a) ((dest) = ((double*)&a)[sub])

// loads and stores

#define pLsrPrimV256B32StoreVS(arr, offset, idx, stride, v)            \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV256UInt32Store(arr, offset, idx, v);              \
        } else {                                                        \
            pLsrPrimV256UInt32StoreStrided(arr, offset, idx, stride, v); \
        }                                                               \
    } while (0)
#define pLsrPrimV256B64StoreVS(arr, offset, idx, stride, v)            \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV256UInt64Store(arr, offset, idx, v);              \
        } else {                                                        \
            pLsrPrimV256UInt64StoreStrided(arr, offset, idx, stride, v); \
        }                                                               \
    } while (0)
#define pLsrPrimV256F32StoreVS(arr, offset, idx, stride, v)            \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV256Float32Store(arr, offset, idx, v);             \
        } else {                                                        \
            pLsrPrimV256Float32StoreStrided(arr, offset, idx, stride, v); \
        }                                                               \
    } while (0)
#define pLsrPrimV256F64StoreVS(arr, offset, idx, stride, v)            \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV256Float64Store(arr, offset, idx, v);             \
        } else {                                                        \
            pLsrPrimV256Float64StoreStrided(arr, offset, idx, stride, v); \
        }                                                               \
    } while (0)
#define pLsrPrimV256RefStoreVS(arr, offset, idx, stride, v)            \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV256RefStore(arr, offset, idx, v);                 \
        } else {                                                        \
            pLsrPrimV256RefStoreStrided(arr, offset, idx, stride, v);  \
        }                                                               \
    } while (0)


#define pLsrPrimV256B32LoadVS(dest, arr, offset, idx, stride)          \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV256UInt32Load(dest, arr, offset, idx);            \
        } else {                                                        \
            pLsrPrimV256UInt32LoadStrided(dest, arr, offset, idx, stride); \
        }                                                               \
    } while (0)
#define pLsrPrimV256B64LoadVS(dest, arr, offset, idx, stride)          \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV256UInt64Load(dest, arr, offset, idx);            \
        } else {                                                        \
            pLsrPrimV256UInt64LoadStrided(dest, arr, offset, idx, stride); \
        }                                                               \
    } while (0)
#define pLsrPrimV256F32LoadVS(dest, arr, offset, idx, stride)          \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV256Float32Load(dest, arr, offset, idx);           \
        } else {                                                        \
            pLsrPrimV256Float32LoadStrided(dest, arr, offset, idx, stride); \
        }                                                               \
    } while (0)
#define pLsrPrimV256F64LoadVS(dest, arr, offset, idx, stride)          \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV256Float64Load(dest, arr, offset, idx);           \
        } else {                                                        \
            pLsrPrimV256Float64LoadStrided(dest, arr, offset, idx, stride); \
        }                                                               \
    } while (0)
#define pLsrPrimV256RefLoadVS(dest, arr, offset, idx, stride)          \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV256RefLoad(dest, arr, offset, idx);               \
        } else {                                                        \
            pLsrPrimV256RefLoadStrided(dest, arr, offset, idx, stride); \
        }                                                               \
    } while (0)

#define pLsrPrimV128Float32LoadIndexed(dest, arr, off, vi)              \
    do {                                                                \
        uint32 pLsrPrimV256Float32LoadVectorHelp_idx0;                  \
        uint32 pLsrPrimV256Float32LoadVectorHelp_idx1;                  \
        uint32 pLsrPrimV256Float32LoadVectorHelp_idx2;                  \
        uint32 pLsrPrimV256Float32LoadVectorHelp_idx3;                  \
                                                                        \
        pLsrPrimV256Float32LoadVectorHelp_idx0 = (uint32)_mm_extract_epi32(vi, 0); \
        pLsrPrimV256Float32LoadVectorHelp_idx1 = (uint32)_mm_extract_epi32(vi, 1); \
        pLsrPrimV256Float32LoadVectorHelp_idx2 = (uint32)_mm_extract_epi32(vi, 2); \
        pLsrPrimV256Float32LoadVectorHelp_idx3 = (uint32)_mm_extract_epi32(vi, 3); \
        (dest) = _mm_set_ps(*((float32*)((char*)arr + off) + pLsrPrimV256Float32LoadVectorHelp_idx3), \
                            *((float32*)((char*)arr + off) + pLsrPrimV256Float32LoadVectorHelp_idx2), \
                            *((float32*)((char*)arr + off) + pLsrPrimV256Float32LoadVectorHelp_idx1), \
                            *((float32*)((char*)arr + off) + pLsrPrimV256Float32LoadVectorHelp_idx0)); \
    } while (0)

#define pLsrPrimV128Float32LoadIndexed64(dest, arr, offset, vi) \
    do { \
        (dest) = _mm_set_ps( *((float32*)((char*)arr + offset) + vi.values[3]), \
                             *((float32*)((char*)arr + offset) + vi.values[2]), \
                             *((float32*)((char*)arr + offset) + vi.values[1]), \
                             *((float32*)((char*)arr + offset) + vi.values[0])); \
    } while (0)

/* scalar array, scalar offset, vector of indices */
#define pLsrPrimV256F32LoadVI(dest, arr, off, vi)                       \
    do {                                                                \
        __m128i pLsrPrimV256Float32LoadVectorHelp_idxV;                 \
        __m128 pLsrPrimV256Float32LoadVectorHelp_lo;                    \
        __m128 pLsrPrimV256Float32LoadVectorHelp_hi;                    \
        __m256  pLsrPrimV256Float32LoadVectorHelp_res;                  \
        uint32 pLsrPrimV256Float32LoadVectorHelp_idx0;                  \
        uint32 pLsrPrimV256Float32LoadVectorHelp_idx1;                  \
        uint32 pLsrPrimV256Float32LoadVectorHelp_idx2;                  \
        uint32 pLsrPrimV256Float32LoadVectorHelp_idx3;                  \
                                                                        \
        pLsrPrimV256Float32LoadVectorHelp_idxV = _mm256_extractf128_si256(vi, 0); \
        pLsrPrimV256Float32LoadVectorHelp_idx0 = (uint32)_mm_extract_epi32(pLsrPrimV256Float32LoadVectorHelp_idxV, 0); \
        pLsrPrimV256Float32LoadVectorHelp_idx1 = (uint32)_mm_extract_epi32(pLsrPrimV256Float32LoadVectorHelp_idxV, 1); \
        pLsrPrimV256Float32LoadVectorHelp_idx2 = (uint32)_mm_extract_epi32(pLsrPrimV256Float32LoadVectorHelp_idxV, 2); \
        pLsrPrimV256Float32LoadVectorHelp_idx3 = (uint32)_mm_extract_epi32(pLsrPrimV256Float32LoadVectorHelp_idxV, 3); \
        pLsrPrimV256Float32LoadVectorHelp_lo =                          \
            _mm_set_ps(*((float32*)((char*)arr + off) + pLsrPrimV256Float32LoadVectorHelp_idx3), \
                       *((float32*)((char*)arr + off) + pLsrPrimV256Float32LoadVectorHelp_idx2), \
                       *((float32*)((char*)arr + off) + pLsrPrimV256Float32LoadVectorHelp_idx1), \
                       *((float32*)((char*)arr + off) + pLsrPrimV256Float32LoadVectorHelp_idx0)); \
        pLsrPrimV256Float32LoadVectorHelp_idxV = _mm256_extractf128_si256(vi, 1); \
        pLsrPrimV256Float32LoadVectorHelp_idx0 = (uint32)_mm_extract_epi32(pLsrPrimV256Float32LoadVectorHelp_idxV, 0); \
        pLsrPrimV256Float32LoadVectorHelp_idx1 = (uint32)_mm_extract_epi32(pLsrPrimV256Float32LoadVectorHelp_idxV, 1); \
        pLsrPrimV256Float32LoadVectorHelp_idx2 = (uint32)_mm_extract_epi32(pLsrPrimV256Float32LoadVectorHelp_idxV, 2); \
        pLsrPrimV256Float32LoadVectorHelp_idx3 = (uint32)_mm_extract_epi32(pLsrPrimV256Float32LoadVectorHelp_idxV, 3); \
        pLsrPrimV256Float32LoadVectorHelp_hi =                          \
            _mm_set_ps(*((float32*)((char*)arr + off) + pLsrPrimV256Float32LoadVectorHelp_idx3), \
                       *((float32*)((char*)arr + off) + pLsrPrimV256Float32LoadVectorHelp_idx2), \
                       *((float32*)((char*)arr + off) + pLsrPrimV256Float32LoadVectorHelp_idx1), \
                       *((float32*)((char*)arr + off) + pLsrPrimV256Float32LoadVectorHelp_idx0)); \
        pLsrPrimV256Float32LoadVectorHelp_res = _mm256_castps128_ps256(pLsrPrimV256Float32LoadVectorHelp_lo); \
        pLsrPrimV256Float32LoadVectorHelp_res = _mm256_insertf128_ps(pLsrPrimV256Float32LoadVectorHelp_res, \
                                                                     pLsrPrimV256Float32LoadVectorHelp_hi, \
                                                                     1); \
        (dest) = pLsrPrimV256Float32LoadVectorHelp_res;                 \
    } while (0)

/* scalar array, scalar offset, vector of indices */
#define pLsrPrimV256F64LoadVI(dest, arr, off, vi)                       \
    do {                                                                \
        __m128i pLsrPrimV256Float64LoadVectorHelp_idxV;                 \
        __m256d  pLsrPrimV256Float64LoadVectorHelp_res;                 \
        uint32 pLsrPrimV256Float64LoadVectorHelp_idx0;                  \
        uint32 pLsrPrimV256Float64LoadVectorHelp_idx1;                  \
        uint32 pLsrPrimV256Float64LoadVectorHelp_idx2;                  \
        uint32 pLsrPrimV256Float64LoadVectorHelp_idx3;                  \
                                                                        \
        pLsrPrimV256Float64LoadVectorHelp_idxV = vi;                    \
        pLsrPrimV256Float64LoadVectorHelp_idx0 = (uint32)_mm_extract_epi32(pLsrPrimV256Float64LoadVectorHelp_idxV, 0); \
        pLsrPrimV256Float64LoadVectorHelp_idx1 = (uint32)_mm_extract_epi32(pLsrPrimV256Float64LoadVectorHelp_idxV, 1); \
        pLsrPrimV256Float64LoadVectorHelp_idx2 = (uint32)_mm_extract_epi32(pLsrPrimV256Float64LoadVectorHelp_idxV, 2); \
        pLsrPrimV256Float64LoadVectorHelp_idx3 = (uint32)_mm_extract_epi32(pLsrPrimV256Float64LoadVectorHelp_idxV, 3); \
        pLsrPrimV256Float64LoadVectorHelp_res =                         \
            _mm256_set_pd(*((float64*)((char*)arr + off) + pLsrPrimV256Float64LoadVectorHelp_idx3), \
                          *((float64*)((char*)arr + off) + pLsrPrimV256Float64LoadVectorHelp_idx2), \
                          *((float64*)((char*)arr + off) + pLsrPrimV256Float64LoadVectorHelp_idx1), \
                          *((float64*)((char*)arr + off) + pLsrPrimV256Float64LoadVectorHelp_idx0)); \
        (dest) = pLsrPrimV256Float64LoadVectorHelp_res;                 \
    } while (0)


/* scalar array, scalar offset, vector of indices */
/* For some insane reason, the only way to load out a float from an __m128 register
 * is as an integer containing the binary representation of the float. -leaf */
#define pLsrPrimV256F32StoreVI(arr, off, vi, src)                       \
    do {                                                                \
        __m128i pLsrPrimV256Float32StoreVectorHelp_idxV;                 \
        __m128 pLsrPrimV256Float32StoreVectorHelp_srcV;                 \
        uint32 pLsrPrimV256Float32StoreVectorHelp_idx;                  \
                                                                        \
        pLsrPrimV256Float32StoreVectorHelp_idxV =_mm256_extractf128_si256(vi, 0); \
        pLsrPrimV256Float32StoreVectorHelp_srcV =_mm256_extractf128_ps(src, 0); \
        pLsrPrimV256Float32StoreVectorHelp_idx = (uint32)_mm_extract_epi32(pLsrPrimV256Float32StoreVectorHelp_idxV, 0); \
        *((uint32*)((char*)arr + off) + pLsrPrimV256Float32StoreVectorHelp_idx) = \
            (uint32) _mm_extract_ps(pLsrPrimV256Float32StoreVectorHelp_srcV, 0); \
        pLsrPrimV256Float32StoreVectorHelp_idx = (uint32)_mm_extract_epi32(pLsrPrimV256Float32StoreVectorHelp_idxV, 1); \
        *((uint32*)((char*)arr + off) + pLsrPrimV256Float32StoreVectorHelp_idx) = \
            (uint32) _mm_extract_ps(pLsrPrimV256Float32StoreVectorHelp_srcV, 1); \
        pLsrPrimV256Float32StoreVectorHelp_idx = (uint32)_mm_extract_epi32(pLsrPrimV256Float32StoreVectorHelp_idxV, 2); \
        *((uint32*)((char*)arr + off) + pLsrPrimV256Float32StoreVectorHelp_idx) = \
            (uint32) _mm_extract_ps(pLsrPrimV256Float32StoreVectorHelp_srcV, 2); \
        pLsrPrimV256Float32StoreVectorHelp_idx = (uint32)_mm_extract_epi32(pLsrPrimV256Float32StoreVectorHelp_idxV, 3); \
        *((uint32*)((char*)arr + off) + pLsrPrimV256Float32StoreVectorHelp_idx) = \
            (uint32) _mm_extract_ps(pLsrPrimV256Float32StoreVectorHelp_srcV, 3); \
        pLsrPrimV256Float32StoreVectorHelp_idxV =_mm256_extractf128_si256(vi, 1); \
        pLsrPrimV256Float32StoreVectorHelp_srcV =_mm256_extractf128_ps(src, 1); \
        pLsrPrimV256Float32StoreVectorHelp_idx = (uint32)_mm_extract_epi32(pLsrPrimV256Float32StoreVectorHelp_idxV, 0); \
        *((uint32*)((char*)arr + off) + pLsrPrimV256Float32StoreVectorHelp_idx) = \
            (uint32) _mm_extract_ps(pLsrPrimV256Float32StoreVectorHelp_srcV, 0); \
        pLsrPrimV256Float32StoreVectorHelp_idx = (uint32)_mm_extract_epi32(pLsrPrimV256Float32StoreVectorHelp_idxV, 1); \
        *((uint32*)((char*)arr + off) + pLsrPrimV256Float32StoreVectorHelp_idx) = \
            (uint32) _mm_extract_ps(pLsrPrimV256Float32StoreVectorHelp_srcV, 1); \
        pLsrPrimV256Float32StoreVectorHelp_idx = (uint32)_mm_extract_epi32(pLsrPrimV256Float32StoreVectorHelp_idxV, 2); \
        *((uint32*)((char*)arr + off) + pLsrPrimV256Float32StoreVectorHelp_idx) = \
            (uint32) _mm_extract_ps(pLsrPrimV256Float32StoreVectorHelp_srcV, 2); \
        pLsrPrimV256Float32StoreVectorHelp_idx = (uint32)_mm_extract_epi32(pLsrPrimV256Float32StoreVectorHelp_idxV, 3); \
        *((uint32*)((char*)arr + off) + pLsrPrimV256Float32StoreVectorHelp_idx) = \
            (uint32) _mm_extract_ps(pLsrPrimV256Float32StoreVectorHelp_srcV, 3); \
    } while (0)

/* scalar array, scalar offset, vector of indices */
#define pLsrPrimV256F64StoreVI(arr, off, vi, src)                       \
    do {                                                                \
        __m128i pLsrPrimV256Float64StoreVectorHelp_idxV;                 \
        __m128d pLsrPrimV256Float64StoreVectorHelp_srcV;                 \
        uint32 pLsrPrimV256Float64StoreVectorHelp_idx;                  \
                                                                        \
        pLsrPrimV256Float64StoreVectorHelp_idxV = vi;                   \
        pLsrPrimV256Float64StoreVectorHelp_srcV =_mm256_extractf128_pd(src, 0); \
        pLsrPrimV256Float64StoreVectorHelp_idx = (uint32)_mm_extract_epi32(pLsrPrimV256Float64StoreVectorHelp_idxV, 0); \
        _mm_storel_pd(((float64*)((char*)arr + off) + pLsrPrimV256Float64StoreVectorHelp_idx), pLsrPrimV256Float64StoreVectorHelp_srcV); \
        pLsrPrimV256Float64StoreVectorHelp_idx = (uint32)_mm_extract_epi32(pLsrPrimV256Float64StoreVectorHelp_idxV, 1); \
        _mm_storeh_pd(((float64*)((char*)arr + off) + pLsrPrimV256Float64StoreVectorHelp_idx), pLsrPrimV256Float64StoreVectorHelp_srcV); \
                                                                        \
        pLsrPrimV256Float64StoreVectorHelp_srcV =_mm256_extractf128_pd(src, 1); \
        pLsrPrimV256Float64StoreVectorHelp_idx = (uint32)_mm_extract_epi32(pLsrPrimV256Float64StoreVectorHelp_idxV, 2); \
        _mm_storel_pd(((float64*)((char*)arr + off) + pLsrPrimV256Float64StoreVectorHelp_idx), pLsrPrimV256Float64StoreVectorHelp_srcV); \
        pLsrPrimV256Float64StoreVectorHelp_idx = (uint32)_mm_extract_epi32(pLsrPrimV256Float64StoreVectorHelp_idxV, 3); \
        _mm_storeh_pd(((float64*)((char*)arr + off) + pLsrPrimV256Float64StoreVectorHelp_idx), pLsrPrimV256Float64StoreVectorHelp_srcV); \
    } while (0)


#define pLsrPrimV256B32LoadVectorStrided(dest, arr, offset, idx, stride)           \
    pLsrPrimV256UInt32LoadVectorStrided(dest, arr, offset, idx, stride)
#define pLsrPrimV256B64LoadVectorStrided(dest, arr, offset, idx, stride)           \
    pLsrPrimV256UInt64LoadVectorStrided(dest, arr, offset, idx, stride)
#define pLsrPrimV256F32LoadVectorStrided(dest, arr, offset, idx, stride) \
    do {                                                                \
        if (stride == 0) {                                              \
            pLsrPrimV256Float32LoadVector(dest, arr, offset, idx);      \
        } else {                                                        \
            pLsrPrimV256Float32LoadVectorStrided(dest, arr, offset, idx, stride); \
        }                                                               \
    } while (0)                                                         \

#define pLsrPrimV256B32LoadVVS(dest, arr, offset, idx, stride)           \
    pLsrPrimV256UInt32LoadVectorStrided(dest, arr, offset, idx, stride)
#define pLsrPrimV256B64LoadVVS(dest, arr, offset, idx, stride)           \
    pLsrPrimV256UInt64LoadVectorStrided(dest, arr, offset, idx, stride)
#define pLsrPrimV256F32LoadVVS(dest, arr, offset, idx, stride)           \
    pLsrPrimV256F32LoadVectorStrided(dest, arr, offset, idx, stride)
#define pLsrPrimV256F64LoadVVS(dest, arr, offset, idx, stride)           \
    pLsrPrimV256F64LoadVectorStrided(dest, arr, offset, idx, stride)
#define pLsrPrimV256RefLoadVVS(dest, arr, offset, idx, stride)           \
    pLsrPrimV256RefLoadVectorStrided(dest, arr, offset, idx, stride)


// *****************************************************************************
// Type-specific arithemetic and memory operations
// *****************************************************************************

// ================
// UInt32 - 256-bit
// ================

// *****************************************************************************
// Arithmetic
// *****************************************************************************

#define pLsrPrimV256PointwiseUInt32Plus(dest, a, b) vector256PointwiseUInt32Plus(dest, a, b)
#define pLsrPrimV256PointwiseUInt32Minus(dest, a, b) vector256PointwiseUInt32Minus(dest, a, b)
#define pLsrPrimV256PointwiseUInt32Times(dest, a, b) vector256PointwiseUInt32Times(dest, a, b)

#define pLsrPrimV256PointwiseUInt64Plus(dest, a, b) vector256PointwiseUInt64Plus(dest, a, b)
#define pLsrPrimV256PointwiseUInt64Minus(dest, a, b) vector256PointwiseUInt64Minus(dest, a, b)
#define pLsrPrimV256PointwiseUInt64Times(dest, a, b) vector256PointwiseUInt64Times(dest, a, b)

#define pLsrPrimV256PointwiseSInt32Plus(dest, a, b) vector256PointwiseSInt32Plus(dest, a, b)
#define pLsrPrimV256PointwiseSInt32Minus(dest, a, b) vector256PointwiseSInt32Minus(dest, a, b)
#define pLsrPrimV256PointwiseSInt32Times(dest, a, b) vector256PointwiseSInt32Times(dest, a, b)

#define pLsrPrimV256PointwiseSInt64Plus(dest, a, b) vector256PointwiseSInt64Plus(dest, a, b)
#define pLsrPrimV256PointwiseSInt64Minus(dest, a, b) vector256PointwiseSInt64Minus(dest, a, b)
#define pLsrPrimV256PointwiseSInt64Times(dest, a, b) vector256PointwiseSInt64Times(dest, a, b)

#define integer256via128(dest, a, b, op)                                \
    do {                                                                \
      __m256i integer256via128_dest = {0, 0, 0, 0, 0, 0, 0, 0};         \
      __m128i integer256via128_hiA = _mm256_extractf128_si256(a, 1);    \
      __m128i integer256via128_loA = _mm256_extractf128_si256(a, 0);    \
      __m128i integer256via128_hiB  = _mm256_extractf128_si256(b, 1);   \
      __m128i integer256via128_loB  = _mm256_extractf128_si256(b, 0);   \
      __m128i integer256via128_loTemp;                                  \
      __m128i integer256via128_hiTemp;                                  \
      op(integer256via128_loTemp, integer256via128_loA, integer256via128_loB); \
      op(integer256via128_hiTemp, integer256via128_hiA, integer256via128_hiB); \
      integer256via128_dest = _mm256_insertf128_si256(integer256via128_dest, integer256via128_hiTemp, 1); \
      integer256via128_dest = _mm256_insertf128_si256(integer256via128_dest, integer256via128_loTemp, 0); \
      (dest) =  integer256via128_dest;                                  \
} while (0)

#define vector256PointwiseUInt32Plus(dest, a, b) integer256via128(dest, a, b, pLsrPrimV128PointwiseUInt32Plus)
#define vector256PointwiseUInt32Minus(dest, a, b) integer256via128(dest, a, b, pLsrPrimV128PointwiseUInt32Minus)
#define vector256PointwiseUInt32Times(dest, a, b) integer256via128(dest, a, b, pLsrPrimV128PointwiseUInt32Times)

#ifdef TWOVEC64

#define vector256PointwiseSInt64Times(dest, a, b)          \
    do {                                                   \
        __declspec(align(16)) __int64 valsa[4], valsb[8];  \
        _mm_store_si128((void *)&valsa[0],(a).low);        \
        _mm_store_si128((void *)&valsa[2],(a).high);       \
        _mm_store_si128((void *)&valsb[0],(b).low);        \
        _mm_store_si128((void *)&valsb[2],(b).high);       \
        unsigned i;                                        \
        for(i = 0; i < 4; ++i) {                           \
            valsa[i] *= valsb[i];                          \
        }                                                  \
        (dest).low  = _mm_load_si128((void *)&valsa[0]);   \
        (dest).high = _mm_load_si128((void *)&valsa[2]);   \
    } while (0)

#define vector256PointwiseUInt64Plus(dest, a, b) \
    do {                                         \
        (dest).low  = _mm_add_epi64((a).low,  (b).low);  \
        (dest).high = _mm_add_epi64((a).high, (b).high);  \
    } while (0)

#define vector256PointwiseUInt64Minus(dest, a, b) \
    do {                                          \
        (dest).low  = _mm_sub_epi64((a).low,  (b).low);   \
        (dest).high = _mm_sub_epi64((a).high, (b).high);   \
    } while (0)

#define vector256PointwiseSInt64Plus(dest, a, b) \
    do {                                         \
        (dest).low  = _mm_add_epi64((a).low,  (b).low);  \
        (dest).high = _mm_add_epi64((a).high, (b).high);  \
    } while (0)

#define vector256PointwiseSInt64Minus(dest, a, b) \
    do {                                          \
        (dest).low  = _mm_sub_epi64((a).low,  (b).low);   \
        (dest).high = _mm_sub_epi64((a).high, (b).high);   \
    } while (0)
#elif defined ONEVEC64
#else
#define vector256PointwiseSInt64Times(dest, a, b)   \
    do {                                            \
        dest.values[0] = a.values[0] * b.values[0]; \
        dest.values[1] = a.values[1] * b.values[1]; \
        dest.values[2] = a.values[2] * b.values[2]; \
        dest.values[3] = a.values[3] * b.values[3]; \
    } while (0)

#define vector256PointwiseSInt64Plus(dest, a, b)    \
    do {                                            \
        dest.values[0] = a.values[0] + b.values[0]; \
        dest.values[1] = a.values[1] + b.values[1]; \
        dest.values[2] = a.values[2] + b.values[2]; \
        dest.values[3] = a.values[3] + b.values[3]; \
    } while (0)

#define vector256PointwiseSInt64Minus(dest, a, b)   \
    do {                                            \
        dest.values[0] = a.values[0] - b.values[0]; \
        dest.values[1] = a.values[1] - b.values[1]; \
        dest.values[2] = a.values[2] - b.values[2]; \
        dest.values[3] = a.values[3] - b.values[3]; \
    } while (0)

#define pLsrPrimV256PointwiseSInt64DivT(dest, a, b)                    \
    do { \
        dest.values[0] = a.values[0] / b.values[0]; \
        dest.values[1] = a.values[1] / b.values[1]; \
        dest.values[2] = a.values[2] / b.values[2]; \
        dest.values[3] = a.values[3] / b.values[3]; \
    } while (0)

#define pLsrPrimV256PointwiseSInt64ModT(dest, a, b)                    \
    do { \
        dest.values[0] = a.values[0] % b.values[0]; \
        dest.values[1] = a.values[1] % b.values[1]; \
        dest.values[2] = a.values[2] % b.values[2]; \
        dest.values[3] = a.values[3] % b.values[3]; \
    } while (0)


#define vector256PointwiseUInt64Times(dest, a, b)                       \
    do {                                                                \
        dest.values[0] = (uint64_t)a.values[0] * (uint64_t)b.values[0]; \
        dest.values[1] = (uint64_t)a.values[1] * (uint64_t)b.values[1]; \
        dest.values[2] = (uint64_t)a.values[2] * (uint64_t)b.values[2]; \
        dest.values[3] = (uint64_t)a.values[3] * (uint64_t)b.values[3]; \
    } while (0)

#define vectorV256PointwiseUInt64Plus(dest, a, b)                       \
    do {                                                                \
        dest.values[0] = (uint64_t)a.values[0] + (uint64_t)b.values[0]; \
        dest.values[1] = (uint64_t)a.values[1] + (uint64_t)b.values[1]; \
        dest.values[2] = (uint64_t)a.values[2] + (uint64_t)b.values[2]; \
        dest.values[3] = (uint64_t)a.values[3] + (uint64_t)b.values[3]; \
    } while (0)

#define vectorV256PointwiseUInt64Minus(dest, a, b)                      \
    do {                                                                \
        dest.values[0] = (uint64_t)a.values[0] - (uint64_t)b.values[0]; \
        dest.values[1] = (uint64_t)a.values[1] - (uint64_t)b.values[1]; \
        dest.values[2] = (uint64_t)a.values[2] - (uint64_t)b.values[2]; \
        dest.values[3] = (uint64_t)a.values[3] - (uint64_t)b.values[3]; \
    } while (0)

#define pLsrPrimV256PointwiseUInt64DivT(dest, a, b)                    \
    do { \
        dest.values[0] = (uint64_t)a.values[0] / (uint64_t)b.values[0]; \
        dest.values[1] = (uint64_t)a.values[1] / (uint64_t)b.values[1]; \
        dest.values[2] = (uint64_t)a.values[2] / (uint64_t)b.values[2]; \
        dest.values[3] = (uint64_t)a.values[3] / (uint64_t)b.values[3]; \
    } while (0)

#define pLsrPrimV256PointwiseUInt64ModT(dest, a, b)                    \
    do { \
        dest.values[0] = (uint64_t)a.values[0] % (uint64_t)b.values[0]; \
        dest.values[1] = (uint64_t)a.values[1] % (uint64_t)b.values[1]; \
        dest.values[2] = (uint64_t)a.values[2] % (uint64_t)b.values[2]; \
        dest.values[3] = (uint64_t)a.values[3] % (uint64_t)b.values[3]; \
    } while (0)

#endif

#define vector256PointwiseSInt32Plus(dest, a, b) integer256via128(dest, a, b, pLsrPrimV128PointwiseSInt32Plus)
#define vector256PointwiseSInt32Minus(dest, a, b) integer256via128(dest, a, b, pLsrPrimV128PointwiseSInt32Minus)
#define vector256PointwiseSInt32Times(dest, a, b) integer256via128(dest, a, b, pLsrPrimV128PointwiseSInt32Times)


// *****************************************************************************
// Comparison
// *****************************************************************************

#if 0
#define pLsrPrimV256PointwiseSInt32LT(dest, a, b) integer256via128(dest, a, b, pLsrPrimV128PointwiseSInt32LT)
#define pLsrPrimV256PointwiseSInt32EQ(dest, a, b) integer256via128(dest, a, b, pLsrPrimV128PointwiseSInt32EQ)
#define pLsrPrimV256PointwiseSInt32GT(dest, a, b) integer256via128(dest, a, b, pLsrPrimV128PointwiseSInt32GT)
#else
#define pLsrPrimV256CompareSInt32LT(dest, a, b) integer256via128(dest, a, b, pLsrPrimV128CompareSInt32LT)
#define pLsrPrimV256CompareSInt32EQ(dest, a, b) integer256via128(dest, a, b, pLsrPrimV128CompareSInt32EQ)
#define pLsrPrimV256CompareSInt32GT(dest, a, b) integer256via128(dest, a, b, pLsrPrimV128CompareSInt32GT)
#endif

// *****************************************************************************
// Reductions (horizontals)
// *****************************************************************************

#define pLsrPrimV256ReduceAUInt32Plus(dest, init, a)  vector256reduceAUInt32Plus(dest, init, a)
#define pLsrPrimV256ReduceAUInt32Times(dest, init, a) vector256reduceAUInt32Times(dest, init, a)
#define pLsrPrimV256ReduceAUInt32Max(dest, init, a)   vector256reduceAUInt32Max(dest, init, a)
#define pLsrPrimV256ReduceAUInt32Min(dest, init, a)   vector256reduceAUInt32Min(dest, init, a)

#define pLsrPrimV256ReduceASInt32Plus(dest, init, a)  vector256reduceASInt32Plus(dest, init, a)
//#define pLsrPrimV256ReduceASInt32Times(dest, init, a) vector256reduceAUInt32Times(dest, init, a)
//#define pLsrPrimV256ReduceASInt32Max(dest, init, a)   vector256reduceAUInt32Max(dest, init, a)
//#define pLsrPrimV256ReduceASInt32Min(dest, init, a)   vector256reduceAUInt32Min(dest, init, a)

#define vector256reduceAUInt32Plus(dest, init, a)                       \
  do {                                                                  \
    __m128i vector256reduceAUInt32Plus_hiA = _mm256_extractf128_si256(a, 1); \
    __m128i vector256reduceAUInt32Plus_loA = _mm256_extractf128_si256(a, 0); \
    uint32 vector256reduceAUInt32Plus_aRes;                             \
    pLsrPrimV128ReduceAUInt32Plus(vector256reduceAUInt32Plus_aRes, init, vector256reduceAUInt32Plus_hiA); \
    uint32 vector256reduceAUInt32Plus_bRes;                             \
    pLsrPrimV128ReduceAUInt32Plus(vector256reduceAUInt32Plus_bRes, init, vector256reduceAUInt32Plus_loA); \
    (dest) = (vector256reduceAUInt32Plus_aRes + vector256reduceAUInt32Plus_bRes); \
  } while (0)

#define vector256reduceAUInt32Times(dest, init, a)                      \
  do {                                                                  \
    __m128i vector256reduceAUInt32Times_hiA = _mm256_extractf128_si256(a, 1); \
    __m128i vector256reduceAUInt32Times_loA = _mm256_extractf128_si256(a, 0); \
    uint32 vector256reduceAUInt32Times_aRes;                            \
    pLsrPrimV128ReduceAUInt32Times(vector256reduceAUInt32Times_aRes, init, vector256reduceAUInt32Times_hiA); \
    uint32 vector256reduceAUInt32Times_bRes;                            \
    pLsrPrimV128ReduceAUInt32Times(vector256reduceAUInt32Times_bRes, init, vector256reduceAUInt32Times_loA); \
    (dest) = (vector256reduceAUInt32Times_aRes * vector256reduceAUInt32Times_bRes); \
  } while (0)

#define vector256reduceAUInt32Max(dest, init, a)        \
    do {                                                                \
        __m128i vector256reduceAUInt32Max_hiA = _mm256_extractf128_si256(a, 1); \
        __m128i vector256reduceAUInt32Max_loA = _mm256_extractf128_si256(a, 0); \
        uint32 vector256reduceAUInt32Max_aRes; \
        pLsrPrimV128ReduceAUInt32Max(vector256reduceAUInt32Max_aRes, init, vector256reduceAUInt32Max_hiA); \
        uint32 vector256reduceAUInt32Max_bRes; \
        pLsrPrimV128ReduceAUInt32Max(vector256reduceAUInt32Max_bRes, init, vector256reduceAUInt32Max_loA); \
        (dest) = (max(vector256reduceAUInt32Max_aRes, vector256reduceAUInt32Max_bRes)); \
    } while (0)

#define vector256reduceAUInt32Min(dest, init, a)        \
  do {                                                                  \
    __m128i vector256reduceAUInt32Min_hiA = _mm256_extractf128_si256(a, 1); \
    __m128i vector256reduceAUInt32Min_loA = _mm256_extractf128_si256(a, 0); \
    uint32 vector256reduceAUInt32Min_aRes;                              \
    pLsrPrimV128ReduceAUInt32Min(vector256reduceAUInt32Min_aRes, init, vector256reduceAUInt32Min_hiA); \
    uint32 vector256reduceAUInt32Min_bRes;                              \
    pLsrPrimV128ReduceAUInt32Min(vector256reduceAUInt32Min_bRes, init, vector256reduceAUInt32Min_loA); \
    (dest) = (min(vector256reduceAUInt32Min_aRes, vector256reduceAUInt32Min_bRes)); \
  } while (0)

#define vector256reduceASInt32Plus(dest, init, a)                       \
  do {                                                                  \
    __m128i vector256reduceASInt32Plus_hiA = _mm256_extractf128_si256(a, 1); \
    __m128i vector256reduceASInt32Plus_loA = _mm256_extractf128_si256(a, 0); \
    sint32 vector256reduceASInt32Plus_aRes;                             \
    pLsrPrimV128ReduceASInt32Plus(vector256reduceASInt32Plus_aRes, init, vector256reduceASInt32Plus_hiA); \
    sint32 vector256reduceASInt32Plus_bRes;                             \
    pLsrPrimV128ReduceASInt32Plus(vector256reduceASInt32Plus_bRes, init, vector256reduceASInt32Plus_loA); \
    (dest) = (vector256reduceASInt32Plus_aRes + vector256reduceASInt32Plus_bRes); \
  } while (0)


// *****************************************************************************
// Data operations
// *****************************************************************************

#define pLsrPrimV256UInt32Lift(dest, a) vector256UInt32Lift(dest, a)
#define pLsrPrimV256UInt64Lift(dest, a) vector256UInt64Lift(dest, a)
#define pLsrPrimV256UInt32Const(dest, c0, c1, c2, c3, c4, c5, c6, c7)   \
    vector256UInt32Const(dest, c0, c1, c2, c3, c4, c5, c6, c7)
#define pLsrPrimV256UInt64Const(dest, c0, c1, c2, c3)   \
    vector256UInt64Const(dest, c0, c1, c2, c3)

#define vector256UInt32Lift(dest, a)                                    \
  do {                                                                  \
    __m256i vector256UInt32Lift_dest = {0, 0, 0, 0, 0, 0, 0, 0};        \
    __m128i vector256UInt32Lift_lo = _mm_set1_epi32(a);                 \
    vector256UInt32Lift_dest = _mm256_insertf128_si256(vector256UInt32Lift_dest, vector256UInt32Lift_lo, 1); \
    vector256UInt32Lift_dest = _mm256_insertf128_si256(vector256UInt32Lift_dest, vector256UInt32Lift_lo, 0); \
    (dest) = vector256UInt32Lift_dest;                                  \
  } while (0)

#define pLsrPrimV256PointwiseUInt32BAnd(dest, a, b)    integer256via128(dest, a, b, pLsrPrimV128PointwiseUInt32BAnd)
#define pLsrPrimV256PointwiseUInt32BShiftL(dest, a, b) integer256via128(dest, a, b, pLsrPrimV128PointwiseUInt32BShiftL)
#define pLsrPrimV256PointwiseUInt32BShiftR(dest, a, b) integer256via128(dest, a, b, pLsrPrimV128PointwiseUInt32BShiftR)
//#define pLsrPrimV256PointwiseUInt32BShiftR(dest, a, b) (dest) = _mm256_srlv_epi32((a),(b))

#ifdef TWOVEC64

#if 0
#define intToM64(a) __m64{a}
#if 0
inline __m64 intToM64(__int64 a) {
  __m64 ret;
  ret.__m = a;
  return ret;
}
#endif
#else
#define intToM64 _mm_cvtsi64_m64
//#define intToM64 _m_from_int64
#endif

#if 0
#define vector128UInt64Const(dest, c0, c1) \
  do {                                     \
    dest.m128i_u64[0] = c0;                \
    dest.m128i_u64[1] = c1;                \
  } while (0)
#endif

#if 0
#define vector256UInt64Lift(dest, a) vector256UInt64Const(dest, a, a, a, a)
#else
#define vector256UInt64Lift(dest, a)             \
  do {                                           \
    (dest).low  = _mm_set1_epi64(intToM64(a)); \
    (dest).high = _mm_set1_epi64(intToM64(a)); \
  } while (0)
#endif

#if 0
#define vector256UInt64Const(dest, c0, c1, c2, c3) \
    do {                                           \
        vector128UInt64Const((dest).low,  c0, c1); \
        vector128UInt64Const((dest).high, c2, c3); \
    } while (0)
#else
#define vector256UInt64Const(dest, c0, c1, c2, c3) \
    do {                                           \
        (dest).low  = _mm_set_epi64(intToM64(c0), intToM64(c1)); \
        (dest).high = _mm_set_epi64(intToM64(c2), intToM64(c3)); \
    } while (0)
#endif

#elif defined ONEVEC64
#else
#define vector256UInt64Lift(dest, a)             \
  do {                                           \
    (dest).values[0] = a; \
    (dest).values[1] = a; \
    (dest).values[2] = a; \
    (dest).values[3] = a; \
  } while (0)

#define vector256UInt64Const(dest, c0, c1, c2, c3) \
  do {                                           \
    (dest).values[0] = c0; \
    (dest).values[1] = c1; \
    (dest).values[2] = c2; \
    (dest).values[3] = c3; \
  } while (0)
#endif

#if 1
#define vector256UInt32Const(dest, c0, c1, c2, c3, c4, c5, c6, c7)      \
    (dest) = _mm256_set_epi32(c7, c6, c5, c4, c3, c2, c1, c0)
#else
#define vector256UInt32Const(dest, c0, c1, c2, c3, c4, c5, c6, c7)      \
    do {                                                                \
        __m256i vector256UInt32Const_out = {0, 0, 0, 0, 0, 0, 0, 0};    \
        __m128i vector256UInt32Const_lo = _mm_set_epi32(c3, c2, c1, c0); \
        __m128i vector256UInt32Const_hi = _mm_set_epi32(c7, c6, c5, c4); \
        vector256UInt32Const_out = _mm256_insertf128_si256(vector256UInt32Const_out, vector256UInt32Const_hi, 1); \
        vector256UInt32Const_out = _mm256_insertf128_si256(vector256UInt32Const_out, vector256UInt32Const_lo, 0); \
        (dest) = vector256UInt32Const_out;                              \
    } while (0)
#endif

#define pLsrPrimV256UInt32Load(dest, arr, off, idx) pLsrPrimV256UInt32LoadHelp (dest, arr, off, idx)
#define pLsrPrimV256UInt32Store(arr, off, idx, v) pLsrPrimV256UInt32StoreHelp (arr, off, idx, v)
#define pLsrPrimV256RefStore(arr, off, idx, v) pLsrPrimV256UInt32StoreHelp (arr, off, idx, v)

#define pLsrPrimV256UInt32LoadHelp(dest, arr, off, idx)                \
    do {                                                                \
        __m256i* pLsrPrimV256UInt32LoadHelp_marr = (__m256i*)((uint32*)((char*)arr+off) + idx); \
        (dest) = _mm256_loadu_si256(pLsrPrimV256UInt32LoadHelp_marr);  \
    } while (0)

#define pLsrPrimV256UInt32StoreHelp(arr, off, idx, v)                  \
    do {                                                                \
        __m256i* pLsrPrimV256UInt32StoreHelp_marr = (__m256i*)((uint32*)((char*)arr+off) + idx); \
        _mm256_storeu_si256(pLsrPrimV256UInt32StoreHelp_marr, v);                                   \
    } while (0)

/* scalar array, scalar offset, vector of indices */
#define pLsrPrimV256B32LoadVI(dest, arr, off, vi)                   \
    do {                                                            \
        __m128i pLsrPrimV256B32LoadVectorHelp_idxV;                 \
        __m128i pLsrPrimV256B32LoadVectorHelp_lo;                   \
        __m128i pLsrPrimV256B32LoadVectorHelp_hi;                   \
        __m256i pLsrPrimV256B32LoadVectorHelp_res;                  \
        uint32 pLsrPrimV256B32LoadVectorHelp_idx0;                  \
        uint32 pLsrPrimV256B32LoadVectorHelp_idx1;                  \
        uint32 pLsrPrimV256B32LoadVectorHelp_idx2;                  \
        uint32 pLsrPrimV256B32LoadVectorHelp_idx3;                  \
                                                                    \
        pLsrPrimV256B32LoadVectorHelp_idxV = _mm256_extractf128_si256(vi, 0); \
        pLsrPrimV256B32LoadVectorHelp_idx0 = (uint32)_mm_extract_epi32(pLsrPrimV256B32LoadVectorHelp_idxV, 0); \
        pLsrPrimV256B32LoadVectorHelp_idx1 = (uint32)_mm_extract_epi32(pLsrPrimV256B32LoadVectorHelp_idxV, 1); \
        pLsrPrimV256B32LoadVectorHelp_idx2 = (uint32)_mm_extract_epi32(pLsrPrimV256B32LoadVectorHelp_idxV, 2); \
        pLsrPrimV256B32LoadVectorHelp_idx3 = (uint32)_mm_extract_epi32(pLsrPrimV256B32LoadVectorHelp_idxV, 3); \
        pLsrPrimV256B32LoadVectorHelp_lo =                          \
            _mm_set_epi32(*((sint32*)((char*)arr + off) + pLsrPrimV256B32LoadVectorHelp_idx3),  \
                          *((sint32*)((char*)arr + off) + pLsrPrimV256B32LoadVectorHelp_idx2),  \
                          *((sint32*)((char*)arr + off) + pLsrPrimV256B32LoadVectorHelp_idx1),  \
                          *((sint32*)((char*)arr + off) + pLsrPrimV256B32LoadVectorHelp_idx0)); \
        pLsrPrimV256B32LoadVectorHelp_idxV = _mm256_extractf128_si256(vi, 1); \
        pLsrPrimV256B32LoadVectorHelp_idx0 = (uint32)_mm_extract_epi32(pLsrPrimV256B32LoadVectorHelp_idxV, 0); \
        pLsrPrimV256B32LoadVectorHelp_idx1 = (uint32)_mm_extract_epi32(pLsrPrimV256B32LoadVectorHelp_idxV, 1); \
        pLsrPrimV256B32LoadVectorHelp_idx2 = (uint32)_mm_extract_epi32(pLsrPrimV256B32LoadVectorHelp_idxV, 2); \
        pLsrPrimV256B32LoadVectorHelp_idx3 = (uint32)_mm_extract_epi32(pLsrPrimV256B32LoadVectorHelp_idxV, 3); \
        pLsrPrimV256B32LoadVectorHelp_hi =                          \
            _mm_set_epi32(*((sint32*)((char*)arr + off) + pLsrPrimV256B32LoadVectorHelp_idx3),  \
                          *((sint32*)((char*)arr + off) + pLsrPrimV256B32LoadVectorHelp_idx2),  \
                          *((sint32*)((char*)arr + off) + pLsrPrimV256B32LoadVectorHelp_idx1),  \
                          *((sint32*)((char*)arr + off) + pLsrPrimV256B32LoadVectorHelp_idx0)); \
        pLsrPrimV256B32LoadVectorHelp_res =_mm256_insertf128_si256(pLsrPrimV256B32LoadVectorHelp_res, pLsrPrimV256B32LoadVectorHelp_lo, 0); \
        pLsrPrimV256B32LoadVectorHelp_res =_mm256_insertf128_si256(pLsrPrimV256B32LoadVectorHelp_res, pLsrPrimV256B32LoadVectorHelp_hi, 1); \
        (dest) = pLsrPrimV256B32LoadVectorHelp_res;                 \
    } while (0)

// =====
// Float
// =====

// *****************************************************************************
// Arithmetic operations
// *****************************************************************************

static __m256 pLsrPrimV256Float32Zero = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
#define pLsrPrimV256PointwiseFloat32Plus(dest, a, b) ((dest) = _mm256_add_ps(a, b))
#define pLsrPrimV256PointwiseFloat32Minus(dest, a, b) ((dest) = _mm256_sub_ps(a, b))
#define pLsrPrimV256PointwiseFloat32Times(dest, a, b) ((dest) = _mm256_mul_ps(a, b))
#define pLsrPrimV256PointwiseFloat32Divide(dest, a, b) ((dest) = _mm256_div_ps(a, b))
#define pLsrPrimV256PointwiseFloat32Negate(dest, a) ((dest) = _mm256_sub_ps(pLsrPrimV256Float32Zero, a))
#define pLsrPrimV256PointwiseFloat32Max(dest, a, b) ((dest) = _mm256_max_ps(a, b))
#define pLsrPrimV256PointwiseFloat32Sqrt(dest, a) ((dest) = _mm256_sqrt_ps(a))

// *****************************************************************************
// Reductions
// *****************************************************************************

#define pLsrPrimV256ReduceAFloat32Plus(dest, init, a) reduce256AFloat32Plus(dest, init, a)

#define reduce256AFloat32Plus(dest, init, a)                            \
  do {                                                                  \
    __m256 reduceAFloat32Plus_a_ = a;                                   \
    __m128 rf32_hi = _mm256_extractf128_ps(reduceAFloat32Plus_a_, 1);   \
    __m256 rf32_tmp = _mm256_insertf128_ps(reduceAFloat32Plus_a_, rf32_hi, 0); \
    __m256 reduceAFloat32Plus_tmp = _mm256_hadd_ps(reduceAFloat32Plus_a_, rf32_tmp); \
    __m256 reduceAFloat32Plus_tmp2 = _mm256_hadd_ps(reduceAFloat32Plus_tmp, reduceAFloat32Plus_tmp); \
    __m256 reduceAFloat32Plus_tmp3 = _mm256_hadd_ps(reduceAFloat32Plus_tmp2, reduceAFloat32Plus_tmp2); \
    float reduceAFloat32Plus_dest_;                                     \
    pLsrPrimV256DataSubF32L(reduceAFloat32Plus_dest_, 0, reduceAFloat32Plus_tmp3); \
    (dest) =  (reduceAFloat32Plus_dest_ + (init));                      \
  } while (0)

// *****************************************************************************
// Data operations
// *****************************************************************************

#define pLsrPrimV256Float32Load(dest, arr, off, idx)                   \
    do {                                                                \
      float* pLsrPrimV256Float32Load_marr = ((float*)((char*)arr+off) + idx); \
        __m256 float32load = _mm256_loadu_ps(pLsrPrimV256Float32Load_marr); \
        (dest) = float32load;                                           \
    } while (0)

#define pLsrPrimV256Float32LoadVector(dest, arr, off, idx) \
    pLsrPrimV256Float32LoadVectorHelp (dest, arr, off, idx)

#define pLsrPrimV256Float32Store(arr, off, idx, v)                     \
  do {                                                                  \
    float* pLsrPrimV256Float32Store_marr = ((float*)((char*)arr+off) + idx); \
    _mm256_storeu_ps(pLsrPrimV256Float32Store_marr, v);                \
  } while (0)


#define pLsrPrimV256Float32LoadStrided(dest, arr, off, idx, stride)     \
    do {                                                                \
        (dest) = _mm256_set_ps(*((float32*)((char*)arr + off) + idx + 7*stride), \
                               *((float32*)((char*)arr + off) + idx + 6*stride), \
                               *((float32*)((char*)arr + off) + idx + 5*stride), \
                               *((float32*)((char*)arr + off) + idx + 4*stride), \
                               *((float32*)((char*)arr + off) + idx + 3*stride), \
                               *((float32*)((char*)arr + off) + idx + 2*stride), \
                               *((float32*)((char*)arr + off) + idx + stride), \
                               *((float32*)((char*)arr + off) + idx + 0)); \
    } while (0)

#define pLsrPrimV256Float32StoreStrided(arr, off, idx, stride, src)      \
    do {                                                                \
        __m128 pLsrPrimV256Float32StoreStrided_srcV;                   \
                                                                        \
        pLsrPrimV256Float32StoreStrided_srcV =_mm256_extractf128_ps(src, 0); \
        *((uint32*)((char*)arr + off) + idx + 0) =                            \
            (uint32) _mm_extract_ps(pLsrPrimV256Float32StoreStrided_srcV, 0); \
        *((uint32*)((char*)arr + off) + idx + stride) =                       \
            (uint32) _mm_extract_ps(pLsrPrimV256Float32StoreStrided_srcV, 1); \
        *((uint32*)((char*)arr + off) + idx + 2*stride) =                     \
            (uint32) _mm_extract_ps(pLsrPrimV256Float32StoreStrided_srcV, 2); \
        *((uint32*)((char*)arr + off) + idx + 3*stride) =                     \
            (uint32) _mm_extract_ps(pLsrPrimV256Float32StoreStrided_srcV, 3); \
                                                                        \
        pLsrPrimV256Float32StoreStrided_srcV =_mm256_extractf128_ps(src, 1); \
        *((uint32*)((char*)arr + off) + idx + 4*stride) =                     \
            (uint32) _mm_extract_ps(pLsrPrimV256Float32StoreStrided_srcV, 0); \
        *((uint32*)((char*)arr + off) + idx + 5*stride) =                     \
            (uint32) _mm_extract_ps(pLsrPrimV256Float32StoreStrided_srcV, 1); \
        *((uint32*)((char*)arr + off) + idx + 6*stride) =                     \
            (uint32) _mm_extract_ps(pLsrPrimV256Float32StoreStrided_srcV, 2); \
        *((uint32*)((char*)arr + off) + idx + 7*stride) =                     \
            (uint32) _mm_extract_ps(pLsrPrimV256Float32StoreStrided_srcV, 3); \
    } while (0)

#define pLsrPrimV256Float32Const(dest, c0, c1, c2, c3, c4, c5, c6, c7)  \
    ((dest) = _mm256_set_ps(c7, c6, c5, c4, c3, c2, c1, c0))

#define pLsrPrimV256Float32Lift(dest, a)               \
  do {                                                  \
    float temp = a;                                     \
    (dest) = _mm256_broadcast_ss((float *)(&temp));     \
  } while(0)


#define pLsrPrimV256Float32LoadVectorHelp(dest, arr, off, idx)          \
    do {                                                                \
        __m128i pLsrPrimV256Float32LoadVectorHelp_ptrV;                \
        __m128 pLsrPrimV256Float32LoadVectorHelp_lo;                   \
        __m128 pLsrPrimV256Float32LoadVectorHelp_hi;                   \
        __m256  pLsrPrimV256Float32LoadVectorHelp_res;                  \
        float32* pLsrPrimV256Float32LoadVectorHelp_ptr0;                \
        float32* pLsrPrimV256Float32LoadVectorHelp_ptr1;                \
        float32* pLsrPrimV256Float32LoadVectorHelp_ptr2;                \
        float32* pLsrPrimV256Float32LoadVectorHelp_ptr3;                \
                                                                        \
        pLsrPrimV256Float32LoadVectorHelp_ptrV = _mm256_extractf128_si256(arr, 0); \
        pLsrPrimV256Float32LoadVectorHelp_ptr0 = (float*)_mm_extract_epi32(pLsrPrimV256Float32LoadVectorHelp_ptrV, 0); \
        pLsrPrimV256Float32LoadVectorHelp_ptr1 = (float*)_mm_extract_epi32(pLsrPrimV256Float32LoadVectorHelp_ptrV, 1); \
        pLsrPrimV256Float32LoadVectorHelp_ptr2 = (float*)_mm_extract_epi32(pLsrPrimV256Float32LoadVectorHelp_ptrV, 2); \
        pLsrPrimV256Float32LoadVectorHelp_ptr3 = (float*)_mm_extract_epi32(pLsrPrimV256Float32LoadVectorHelp_ptrV, 3); \
        pLsrPrimV256Float32LoadVectorHelp_lo =                          \
            _mm_set_ps(*(float32*)(((char*)pLsrPrimV256Float32LoadVectorHelp_ptr0 + off) + idx), \
                       *(float32*)(((char*)pLsrPrimV256Float32LoadVectorHelp_ptr1 + off) + idx), \
                       *(float32*)(((char*)pLsrPrimV256Float32LoadVectorHelp_ptr2 + off) + idx), \
                       *(float32*)(((char*)pLsrPrimV256Float32LoadVectorHelp_ptr3 + off) + idx)); \
        pLsrPrimV256Float32LoadVectorHelp_ptrV =_mm256_extractf128_si256(arr, 1); \
        pLsrPrimV256Float32LoadVectorHelp_ptr0 = (float*)_mm_extract_epi32(pLsrPrimV256Float32LoadVectorHelp_ptrV, 0); \
        pLsrPrimV256Float32LoadVectorHelp_ptr1 = (float*)_mm_extract_epi32(pLsrPrimV256Float32LoadVectorHelp_ptrV, 1); \
        pLsrPrimV256Float32LoadVectorHelp_ptr2 = (float*)_mm_extract_epi32(pLsrPrimV256Float32LoadVectorHelp_ptrV, 2); \
        pLsrPrimV256Float32LoadVectorHelp_ptr3 = (float*)_mm_extract_epi32(pLsrPrimV256Float32LoadVectorHelp_ptrV, 3); \
        pLsrPrimV256Float32LoadVectorHelp_hi =                          \
            _mm_set_ps(*(float32*)(((char*)pLsrPrimV256Float32LoadVectorHelp_ptr0 + off) + idx), \
                       *(float32*)(((char*)pLsrPrimV256Float32LoadVectorHelp_ptr1 + off) + idx), \
                       *(float32*)(((char*)pLsrPrimV256Float32LoadVectorHelp_ptr2 + off) + idx), \
                       *(float32*)(((char*)pLsrPrimV256Float32LoadVectorHelp_ptr3 + off) + idx)); \
        pLsrPrimV256Float32LoadVectorHelp_res = _mm256_insertf128_ps(pLsrPrimV256Float32LoadVectorHelp_res, \
                                                                     pLsrPrimV256Float32LoadVectorHelp_hi, \
                                                                     1); \
        pLsrPrimV256Float32LoadVectorHelp_res = _mm256_insertf128_ps(pLsrPrimV256Float32LoadVectorHelp_res, \
                                                                     pLsrPrimV256Float32LoadVectorHelp_lo, \
                                                                     0); \
        (dest) = pLsrPrimV256Float32LoadVectorHelp_res;                 \
    } while (0)

// =====
// Double
// =====

// *****************************************************************************
// Arithmetic operations
// *****************************************************************************

static __m256d pLsrPrimV256Float64Zero = {0.0, 0.0, 0.0, 0.0};
#define pLsrPrimV256PointwiseFloat64Plus(dest, a, b) ((dest) = _mm256_add_pd(a, b))
#define pLsrPrimV256PointwiseFloat64Minus(dest, a, b) ((dest) = _mm256_sub_pd(a, b))
#define pLsrPrimV256PointwiseFloat64Times(dest, a, b) ((dest) = _mm256_mul_pd(a, b))
#define pLsrPrimV256PointwiseFloat64Divide(dest, a, b) ((dest) = _mm256_div_pd(a, b))
#define pLsrPrimV256PointwiseFloat64Negate(dest, a) ((dest) = _mm256_sub_pd(pLsrPrimV256Float64Zero, a))
#define pLsrPrimV256PointwiseFloat64Max(dest, a, b) ((dest) = _mm256_max_pd(a, b))
#define pLsrPrimV256PointwiseFloat64Sqrt(dest, a) ((dest) = _mm256_sqrt_pd(a))

#define pLsrPrimV256PointwiseSInt32DivT(dest, a, b)                     \
    do {                                                                \
        __m128i pLsrPrimV256PointwiseSInt32DivT_a;                      \
        __m128i pLsrPrimV256PointwiseSInt32DivT_b;                      \
        __m128i pLsrPrimV256PointwiseSInt32DivT_rlo;                    \
        __m128i pLsrPrimV256PointwiseSInt32DivT_rhi;                    \
        __m256i pLsrPrimV256PointwiseSInt32DivT_res;                    \
                                                                        \
        pLsrPrimV256PointwiseSInt32DivT_a = _mm256_extractf128_si256(a, 0); \
        pLsrPrimV256PointwiseSInt32DivT_b = _mm256_extractf128_si256(b, 0); \
        pLsrPrimV128PointwiseSInt32DivT(pLsrPrimV256PointwiseSInt32DivT_rlo, pLsrPrimV256PointwiseSInt32DivT_a, pLsrPrimV256PointwiseSInt32DivT_b);\
        pLsrPrimV256PointwiseSInt32DivT_a = _mm256_extractf128_si256(a, 1); \
        pLsrPrimV256PointwiseSInt32DivT_b = _mm256_extractf128_si256(b, 1); \
        pLsrPrimV128PointwiseSInt32DivT(pLsrPrimV256PointwiseSInt32DivT_rhi, pLsrPrimV256PointwiseSInt32DivT_a, pLsrPrimV256PointwiseSInt32DivT_b); \
        pLsrPrimV256PointwiseSInt32DivT_res = _mm256_castsi128_si256(pLsrPrimV256PointwiseSInt32DivT_rlo); \
        pLsrPrimV256PointwiseSInt32DivT_res = _mm256_insertf128_si256(pLsrPrimV256PointwiseSInt32DivT_res, \
                                                                      pLsrPrimV256PointwiseSInt32DivT_rhi, \
                                                                      1); \
        (dest) = pLsrPrimV256PointwiseSInt32DivT_res;                 \
    } while (0)

#define pLsrPrimV256PointwiseSInt32ModT(dest, a, b)                     \
    do {                                                                \
        __m128i pLsrPrimV256PointwiseSInt32ModT_a;                      \
        __m128i pLsrPrimV256PointwiseSInt32ModT_b;                      \
        __m128i pLsrPrimV256PointwiseSInt32ModT_rlo;                    \
        __m128i pLsrPrimV256PointwiseSInt32ModT_rhi;                    \
        __m256i pLsrPrimV256PointwiseSInt32ModT_res;                    \
                                                                        \
        pLsrPrimV256PointwiseSInt32ModT_a = _mm256_extractf128_si256(a, 0); \
        pLsrPrimV256PointwiseSInt32ModT_b = _mm256_extractf128_si256(b, 0); \
        pLsrPrimV128PointwiseSInt32ModT(pLsrPrimV256PointwiseSInt32ModT_rlo, pLsrPrimV256PointwiseSInt32ModT_a, pLsrPrimV256PointwiseSInt32ModT_b);\
        pLsrPrimV256PointwiseSInt32ModT_a = _mm256_extractf128_si256(a, 1); \
        pLsrPrimV256PointwiseSInt32ModT_b = _mm256_extractf128_si256(b, 1); \
        pLsrPrimV128PointwiseSInt32ModT(pLsrPrimV256PointwiseSInt32ModT_rhi, pLsrPrimV256PointwiseSInt32ModT_a, pLsrPrimV256PointwiseSInt32ModT_b); \
        pLsrPrimV256PointwiseSInt32ModT_res = _mm256_castsi128_si256(pLsrPrimV256PointwiseSInt32ModT_rlo); \
        pLsrPrimV256PointwiseSInt32ModT_res = _mm256_insertf128_si256(pLsrPrimV256PointwiseSInt32ModT_res, \
                                                                      pLsrPrimV256PointwiseSInt32ModT_rhi, \
                                                                      1); \
        (dest) = pLsrPrimV256PointwiseSInt32ModT_res;                 \
    } while (0)

// *****************************************************************************
// Reductions
// *****************************************************************************

#define pLsrPrimV256ReduceAFloat64Plus(dest, init, a)                   \
    do {                                                                \
        __m256d reduceAFloat64Plus_p_ = a;                               \
        __m128d reduceAFloat64Plus_p_lo   = _mm256_extractf128_pd(reduceAFloat64Plus_p_, 0); \
        __m128d reduceAFloat64Plus_p_hi   = _mm256_extractf128_pd(reduceAFloat64Plus_p_, 1); \
        __m128d reduceAFloat64Plus_p_tmp  = _mm_hadd_pd(reduceAFloat64Plus_p_lo, reduceAFloat64Plus_p_hi); \
        reduceAFloat64Plus_p_tmp  = _mm_hadd_pd(reduceAFloat64Plus_p_tmp, reduceAFloat64Plus_p_tmp); \
        double reduceAFloat64Plus_dest_;                                \
        pLsrPrimV256DataSubF64(reduceAFloat64Plus_dest_, 0, reduceAFloat64Plus_p_tmp); \
        (dest) =  (reduceAFloat64Plus_dest_ + (init));                  \
    } while (0)

// *****************************************************************************
// Data operations
// *****************************************************************************

#define pLsrPrimV256Float64Load(dest, arr, off, idx)                   \
    do {                                                                \
        double* pLsrPrimV256Float64Load_marr = ((double*)((char*)arr+off) + idx); \
        __m256d float64load = _mm256_loadu_pd(pLsrPrimV256Float64Load_marr); \
        (dest) = float64load;                                           \
    } while (0)

#define pLsrPrimV256Float64Store(arr, off, idx, v)                     \
  do {                                                                  \
    double* pLsrPrimV256Float64Store_marr = ((double*)((char*)arr+off) + idx); \
    _mm256_storeu_pd(pLsrPrimV256Float64Store_marr, v);                \
  } while (0)


#define pLsrPrimV256Float64LoadStrided(dest, arr, off, idx, stride)     \
    do {                                                                \
        (dest) = _mm256_set_pd(*((float64*)((char*)arr + off) + idx + 3*stride), \
                               *((float64*)((char*)arr + off) + idx + 2*stride), \
                               *((float64*)((char*)arr + off) + idx + stride), \
                               *((float64*)((char*)arr + off) + idx + 0));    \
    } while (0)

#define pLsrPrimV256Float64StoreStrided(arr, off, idx, stride, src)     \
    do {                                                                \
        __m128d pLsrPrimV256Float64StoreStrided_srcV;                   \
                                                                        \
        pLsrPrimV256Float64StoreStrided_srcV =_mm256_extractf128_pd(src, 0); \
        _mm_storel_pd(((float64*)((char*)arr + off) + idx + 0), pLsrPrimV256Float64StoreStrided_srcV); \
        _mm_storeh_pd(((float64*)((char*)arr + off) + idx + stride), pLsrPrimV256Float64StoreStrided_srcV); \
                                                                        \
        pLsrPrimV256Float64StoreStrided_srcV =_mm256_extractf128_pd(src, 1); \
        _mm_storel_pd(((float64*)((char*)arr + off) + idx + 2*stride), pLsrPrimV256Float64StoreStrided_srcV); \
        _mm_storeh_pd(((float64*)((char*)arr + off) + idx + 3*stride), pLsrPrimV256Float64StoreStrided_srcV); \
    } while (0)

#define pLsrPrimV256Float64Const(dest, c0, c1, c2, c3)  \
    ((dest) = _mm256_set_pd(c3, c2, c1, c0))

#define pLsrPrimV256Float64Lift(dest, a)               \
  do {                                                  \
    double temp = a;                                     \
    (dest) = _mm256_broadcast_sd((double *)(&temp));     \
  } while(0)

// ================
// Ref - 256-bit
// ================

#define pLsrPrimV256RefLoad(dest, arr, off, idx) pLsrPrimV256UInt32Load(dest, arr, off, idx)

#endif _PLSR_PRIMS_VECTOR_AVX_H_
