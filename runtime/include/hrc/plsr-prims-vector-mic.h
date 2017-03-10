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

#ifndef _PLSR_PRIMS_VECTOR_MIC_H_
#define _PLSR_PRIMS_VECTOR_MIC_H_

#ifdef P_USE_PILLAR
#ifdef LINUX
#  pragma pillar_push_cc(__pcdecl)
#  define to __to__
#else
#  pragma pillar_managed(off)
#  define to __to__
#endif
#endif /* P_USE_PILLAR */

#ifdef P_USE_PILLAR
#define __ICL_INTRINCC __pcdecl
#endif /* P_USE_PILLAR */
#include <immintrin.h>
#undef __cdecl  // deep in the include for immintrin.h, there is a "#define __cdecl".  This undef gets rid of the effects of this.

#ifdef P_USE_PILLAR
#ifdef LINUX
#  undef to
#  pragma pillar_pop_cc
#else
#  undef to
#  pragma pillar_managed(on)
#endif
#endif /* P_USE_PILLAR */

//#define __pillar2c__ 1

typedef __m512  PlsrVector512F32;
typedef __m512i PlsrVector512B64;
typedef __m512d PlsrVector512F64;
typedef __m512i PlsrVector512Ref;

typedef __mmask16 PlsrVectorMask512Fs32;
typedef __mmask8  PlsrVectorMask512Fs64;

#define pLsrPrimV512F64StoreVS(arr, offset, idx, stride, v)               \
    do {                                                                  \
        if (stride == 1) {                                                \
            pLsrPrimV512Float64Store(arr, offset, idx, v);                \
        } else {                                                          \
            pLsrPrimV512Float64StoreStrided(arr, offset, idx, stride, v); \
        }                                                                 \
    } while (0)

#define pLsrPrimV512F32StoreVS(arr, offset, idx, stride, v)               \
    do {                                                                  \
        if (stride == 1) {                                                \
            pLsrPrimV512Float32Store(arr, offset, idx, v);                \
        } else {                                                          \
            pLsrPrimV512Float32StoreStrided(arr, offset, idx, stride, v); \
        }                                                                 \
    } while (0)

#define pLsrPrimV512F64LoadVS(dest, arr, offset, idx, stride)               \
    do {                                                                    \
        if (stride == 1) {                                                  \
            pLsrPrimV512Float64Load(dest, arr, offset, idx);                \
        } else {                                                            \
            pLsrPrimV512Float64LoadStrided(dest, arr, offset, idx, stride); \
        }                                                                   \
    } while (0)

#define pLsrPrimV512F32LoadVS(dest, arr, offset, idx, stride)               \
    do {                                                                    \
        if (stride == 1) {                                                  \
            pLsrPrimV512Float32Load(dest, arr, offset, idx);                \
        } else {                                                            \
            pLsrPrimV512Float32LoadStrided(dest, arr, offset, idx, stride); \
        }                                                                   \
    } while (0)

#define pLsrPrimV512B64LoadVS(dest, arr, offset, idx, stride)               \
    do {                                                                    \
        if (stride == 1) {                                                  \
            pLsrPrimV512UInt64Load(dest, arr, offset, idx);                 \
        } else {                                                            \
            pLsrPrimV512UInt64LoadStrided(dest, arr, offset, idx, stride);  \
        }                                                                   \
    } while (0)

#define pLsrPrimV512B64StoreVS(arr, offset, idx, stride, v)                 \
    do {                                                                    \
        if (stride == 1) {                                                  \
            pLsrPrimV512UInt64Store(arr, offset, idx, v);                   \
        } else {                                                            \
            pLsrPrimV512UInt64StoreStrided(arr, offset, idx, stride, v );   \
        }                                                                   \
    } while (0)

#define pLsrPrimV512RefStoreVS(arr, offset, idx, stride, v)            \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV512RefStore(arr, offset, idx, v);                 \
        } else {                                                        \
            pLsrPrimV512RefStoreStrided(arr, offset, idx, stride, v);  \
        }                                                               \
    } while (0)

#define pLsrPrimV512RefLoadVS(dest, arr, offset, idx, stride)           \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV512RefLoad(dest, arr, offset, idx);                \
        } else {                                                        \
            pLsrPrimV512RefLoadStrided(dest, arr, offset, idx, stride); \
        }                                                               \
    } while (0)

#define pLsrPrimV512512ConvertFloat32FromUInt32(dest, a)  ((dest) = _mm512_castsi512_ps(a))
#define pLsrPrimV512512ConvertUInt32FromFloat32(dest, a)  ((dest) = _mm512_castps_si512(a))
#define pLsrPrimV512256ConvertFloat64FromFloat32(dest, a) ((dest) = _mm512_cvtpslo_pd(a))
#define pLsrPrimV512512CastSInt32ToFloat32(dest, a) ((dest) = _mm512_castsi512_ps(a))
#define pLsrPrimV512512ConvertUInt32FromSInt32(dest, a) ((dest) = a)
#define pLsrPrimV512512ConvertSInt32FromUInt32(dest, a) ((dest) = a)
#define pLsrPrimV512512ConvertUInt64FromSInt64(dest, a) ((dest) = a)
#define pLsrPrimV512512ConvertSInt64FromUInt64(dest, a) ((dest) = a)
#define pLsrPrimV512512CastUInt64ToSInt64(dest, a) ((dest) = a)
#define pLsrPrimV512512CastSInt64ToUInt64(dest, a) ((dest) = a)
#define pLsrPrimV512512CastSInt64ToFloat64(dest, a) ((dest) = _mm512_castsi512_pd(a))

#define pLsrPrimV512DataVectorB64(dest, c0, c1, c2, c3, c4, c5, c6, c7) \
  pLsrPrimV512UInt64Const(dest, c0, c1, c2, c3, c4, c5, c6, c7)

#define pLsrPrimV512Float32Const(dest, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15) \
    ((dest) = _mm512_setr_ps(c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15))

#define pLsrPrimV512DataVectorF32(dest, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15) \
  pLsrPrimV512Float32Const(dest, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)

#define pLsrPrimV512DataVectorF64(dest, c0, c1, c2, c3, c4, c5, c6, c7) \
  pLsrPrimV512Float64Const(dest, c0, c1, c2, c3, c4, c5, c6, c7)

#if 1
#define pLsrPrimV512Float32Lift(dest, a) ((dest) = _mm512_set1_ps(a))
#else
#define pLsrPrimV512Float32Lift(dest, a) \
  do { \
    float ftemp = a; \
    (dest) = _mm512_extload_ps((void const *)(&ftemp),_MM_UPCONV_PS_NONE,_MM_BROADCAST_1X16,0); \
  } while (0)
#endif

#define pLsrPrimV512DataBroadcastB64(dest, c0) pLsrPrimV512UInt64Lift(dest, c0)
//#define pLsrPrimV512DataBroadcastB32(dest, c0) pLsrPrimV512UInt32Lift(dest, c0)
#define pLsrPrimV512DataBroadcastF32(dest, c0) pLsrPrimV512Float32Lift(dest, c0)
#define pLsrPrimV512DataBroadcastF64(dest, c0) pLsrPrimV512Float64Lift(dest, c0)

#define pLsrPrimV512F64LoadVI(dest, arr, off, vi)                       \
    (dest) = _mm512_i32logather_pd(vi, ((char*)arr + off), sizeof(float64))

#if 0
    do {                                                                \
        __int64 indices[8];                                             \
        _mm512_extstore_epi64(&indices, vi, _MM_DOWNCONV_PD_NONE, 0);   \
        (dest) = _mm512_set_pd(*((float64*)((char*)arr + off) + indices[7]), \
                               *((float64*)((char*)arr + off) + indices[6]), \
                               *((float64*)((char*)arr + off) + indices[5]), \
                               *((float64*)((char*)arr + off) + indices[4]), \
                               *((float64*)((char*)arr + off) + indices[3]), \
                               *((float64*)((char*)arr + off) + indices[2]), \
                               *((float64*)((char*)arr + off) + indices[1]), \
                               *((float64*)((char*)arr + off) + indices[0])); \
    } while (0)
#endif

#define pLsrPrimV512PointwiseUInt64BAnd(dest, a, b) (dest) = _mm512_and_epi64((a),(b))
#define pLsrPrimV512PointwiseUInt64BOr(dest, a, b) (dest) = _mm512_or_epi64((a),(b))

//#define pLsrPrimV512PointwiseUInt64BShiftR(dest, a, b) (dest) = _mm512_srlv_epi64((a),(b))
#define pLsrPrimV512PointwiseUInt64BShiftR(dest, a, b) \
    do {                                                                \
        __declspec(align(64)) __int64 valsa[8], valsb[8];               \
        _mm512_store_epi64(valsa,(a));                          \
        _mm512_store_epi64(valsb, (b));                        \
        unsigned i;                                                     \
        for(i = 0; i < 8; ++i) {                                        \
            valsa[i] >>= valsb[i];                                      \
        }                                                               \
        (dest) = _mm512_load_epi64(valsa);                     \
    } while (0)

#define pLsrPrimV512PointwiseUInt64BShiftL(dest, a, b) \
    do {                                                                \
        __declspec(align(64)) __int64 valsa[8], valsb[8];               \
        _mm512_store_epi64(valsa,(a));                          \
        _mm512_store_epi64(valsb, (b));                        \
        unsigned i;                                                     \
        for(i = 0; i < 8; ++i) {                                        \
            valsa[i] <<= valsb[i];                                      \
        }                                                               \
        (dest) = _mm512_load_epi64(valsa);                     \
    } while (0)

// This is brain dead.  Unpack the vector.  Multiply each one and repack.
#define pLsrPrimV512PointwiseSInt64Times(dest, a, b)                    \
    do {                                                                \
        __declspec(align(64)) __int64 valsa[8], valsb[8];               \
        _mm512_store_epi64(valsa,(a));                          \
        _mm512_store_epi64(valsb,(b));                          \
        unsigned i;                                                     \
        for(i = 0; i < 8; ++i) {                                        \
            valsa[i] *= valsb[i];                                       \
        }                                                               \
        (dest) = _mm512_load_epi64(valsa);                     \
    } while (0)

#define pLsrPrimV512PointwiseSInt64Plus(dest, a, b)  vector512PointwiseSInt64Plus(dest, a, b)
#define vector512PointwiseSInt64Plus(dest, a, b)  (dest) = _mm512_add_epi64((a),(b))

#define pLsrPrimV512PointwiseSInt64Minus(dest, a, b)  vector512PointwiseSInt64Minus(dest, a, b)
#define vector512PointwiseSInt64Minus(dest, a, b)  (dest) = _mm512_sub_epi64((a),(b))

#if 0
#define pLsrPrimV512PointwiseFloat32LT(dest, a, b) \
    do { \
       __declspec(align(64)) float32 avec[16], bvec[16];       \
       __declspec(align(64)) uint32_t resvec[16];       \
        _mm512_store_ps(avec,(a));                          \
        _mm512_store_ps(bvec,(b));                        \
       unsigned i; \
       for(i=0;i<16;++i) { \
         if(avec[i] < bvec[i]) resvec[i] = 1; else resvec[i] = 0; \
       } \
       (dest) = _mm512_load_epi32(resvec);               \
    } while(0)

#define pLsrPrimV512PointwiseSInt64LT(dest, a, b) \
    do { \
       __declspec(align(64)) __int64 avec[8], bvec[8];       \
        _mm512_store_epi64(avec,(a));                          \
        _mm512_store_epi64(bvec,(b));                        \
       unsigned i; \
       for(i=0;i<8;++i) { \
         if(avec[i] < bvec[i]) avec[i] = 1; else avec[i] = 0; \
       } \
       (dest) = _mm512_load_epi64(avec);               \
    } while(0)

#define pLsrPrimV512PointwiseSInt64EQ(dest, a, b) \
    do { \
       __declspec(align(64)) __int64 avec[8], bvec[8];       \
        _mm512_store_epi64(avec,(a));                          \
        _mm512_store_epi64(bvec,(b));                        \
       unsigned i; \
       for(i=0;i<8;++i) { \
         if(avec[i] == bvec[i]) avec[i] = 1; else avec[i] = 0; \
       } \
       (dest) = _mm512_load_epi64(avec);               \
    } while(0)

#define pLsrPrimV512PointwiseCondMov(dest, mask, a, b) \
    do { \
       __mmask8 realmask=0; \
       __declspec(align(64)) __int64 mvec[8];       \
        _mm512_store_epi64(mvec,(mask));                          \
       unsigned i; \
       for(i=0;i<8;++i) { \
           if(mvec[i]) realmask |= (1 << i); \
       } \
       (dest) = _mm512_mask_blend_epi64((realmask),(a),(b)); \
    } while (0)
#else
#define pLsrPrimV512CompareFloat32LT(dest, a, b) (dest) = _mm512_cmplt_ps_mask((a),(b))
#define pLsrPrimV512DataBlendB32(dest, mask, a, b) (dest) = _mm512_mask_blend_epi32((mask),(a),(b))
#define pLsrPrimV512DataBlendB64(dest, mask, a, b) (dest) = _mm512_mask_blend_epi64((mask),(a),(b))
#define pLsrPrimV512DataBlendF32(dest, mask, a, b) (dest) = _mm512_mask_blend_ps((mask),(a),(b))
#define pLsrPrimV512DataBlendF64(dest, mask, a, b) (dest) = _mm512_mask_blend_pd((mask),(a),(b))

//#define pLsrPrimV512CompareSInt64LT(dest, a, b) (dest) = _mm512_cmplt_epi64_mask((a),(b))
//#define pLsrPrimV512CompareSInt64EQ(dest, a, b) (dest) = _mm512_cmpeq_epi64_mask((a),(b))
#define pLsrPrimV512CompareSInt64LT(dest, a, b) \
    do { \
       __declspec(align(64)) __int64 avec[8], bvec[8];       \
        _mm512_store_epi64(avec,(a));                          \
        _mm512_store_epi64(bvec,(b));                        \
       unsigned i; \
       (dest) = 0; \
       for(i=0;i<8;++i) { \
         if(avec[i] < bvec[i]) (dest) |= (1 << i); \
       } \
    } while(0)

#define pLsrPrimV512CompareSInt64EQ(dest, a, b) \
    do { \
       __declspec(align(64)) __int64 avec[8], bvec[8];       \
        _mm512_store_epi64(avec,(a));                          \
        _mm512_store_epi64(bvec,(b));                        \
       unsigned i; \
       (dest) = 0; \
       for(i=0;i<8;++i) { \
         if(avec[i] == bvec[i]) (dest) |= (1 << i); \
       } \
    } while(0)

#endif

#define pLsrPrimV512UInt64Lift(dest, a) vector512UInt64Lift(dest, a)
#define pLsrPrimV512UInt64Const(dest, c0, c1, c2, c3, c4, c5, c6, c7)   \
    vector512UInt64Const(dest, c0, c1, c2, c3, c4, c5, c6, c7)

#define vector512UInt64Lift(dest, a) \
    do { \
        uint64_t tmp = a; \
        (dest) = _mm512_extload_epi64((void const *)(&tmp),_MM_UPCONV_EPI64_NONE,_MM_BROADCAST_1X8,0); \
    } while(0)

#define vector512UInt64Const(dest, c0, c1, c2, c3, c4, c5, c6, c7)   \
    (dest) = _mm512_setr_epi64(c0, c1, c2, c3, c4, c5, c6, c7)



#define pLsrPrimV512PointwiseFloat64Plus(dest, a, b)  ((dest) = _mm512_add_pd(a, b))
#define pLsrPrimV512PointwiseFloat64Minus(dest, a, b)  ((dest) = _mm512_sub_pd(a, b))
#define pLsrPrimV512PointwiseFloat64Times(dest, a, b) ((dest) = _mm512_mul_pd(a, b))

#define pLsrPrimV512PointwiseFloat32Plus(dest, a, b)  ((dest) = _mm512_add_ps(a, b))
#define pLsrPrimV512PointwiseFloat32Minus(dest, a, b)  ((dest) = _mm512_sub_ps(a, b))
#define pLsrPrimV512PointwiseFloat32Times(dest, a, b) ((dest) = _mm512_mul_ps(a, b))
#define pLsrPrimV512PointwiseFloat32Sqrt(dest, a) ((dest) = _mm512_sqrt_ps(a))
#define pLsrPrimV512PointwiseFloat32Divide(dest, a, b) ((dest) = _mm512_div_ps(a, b))

#define pLsrPrimV512PointwiseSInt64DivT(dest, a, b) \
    (dest) = _mm512_div_epi64((a),(b))

#define pLsrPrimV512PointwiseSInt64ModT(dest, a, b) \
    (dest) = _mm512_rem_epi64((a),(b))

#define pLsrPrimV512ReduceAFloat64Plus(dest, init, a) \
    (dest) = (init) + _mm512_reduce_add_pd(a)

#define pLsrPrimV512ReduceAFloat32Plus(dest, init, a) \
    (dest) = (init) + _mm512_reduce_add_ps(a)

#ifdef HAS_ALIGNMENT
#define pLsrPrimV512Float64Load(dest, arr, off, idx)  \
    (dest) = _mm512_load_pd((void const *)((double*)((char*)arr+off) + idx))
#else
#define pLsrPrimV512Float64Load(dest, arr, off, idx)                   \
    do {                                                               \
        (dest) = _mm512_loadunpacklo_pd((dest),(void *)((double*)((char*)arr+off) + idx)); \
        (dest) = _mm512_loadunpackhi_pd((dest),(char *)((double*)((char*)arr+off) + idx) + 64); \
    } while (0)
#endif

#ifdef HAS_ALIGNMENT
#define pLsrPrimV512Float32Load(dest, arr, off, idx)  \
    (dest) = _mm512_load_ps((void const *)((float*)((char*)arr+off) + idx))
#else
#define pLsrPrimV512Float32Load(dest, arr, off, idx)                   \
    do {                                                               \
        (dest) = _mm512_loadunpacklo_ps((dest),(void *)((float*)((char*)arr+off) + idx)); \
        (dest) = _mm512_loadunpackhi_ps((dest),(char *)((float*)((char*)arr+off) + idx) + 64); \
    } while (0)
#endif

#ifdef HAS_ALIGNMENT
#define pLsrPrimV512Float64Store(arr, off, idx, v)           \
    _mm512_store_pd((void const *)((double*)((char*)arr+off) + idx), v)
#define pLsrPrimV512Float32Store(arr, off, idx, v)           \
    _mm512_store_ps((void const *)((float*)((char*)arr+off) + idx), v)
#define pLsrPrimV512UInt64Store(arr, off, idx, v)           \
    _mm512_store_epi64((void const *)((uint64_t*)((char*)arr+off) + idx), v)
#else
#define pLsrPrimV512Float64Store(arr, off, idx, v)           \
    do {                                                               \
        _mm512_packstorelo_pd((void *)((double*)((char*)arr+off) + idx),(v)); \
        _mm512_packstorehi_pd((char *)((double*)((char*)arr+off) + idx) + 64,(v)); \
    } while (0)
#define pLsrPrimV512Float32Store(arr, off, idx, v)           \
    do {                                                               \
        _mm512_packstorelo_ps((void *)((float*)((char*)arr+off) + idx),(v)); \
        _mm512_packstorehi_ps((char *)((float*)((char*)arr+off) + idx) + 64,(v)); \
    } while (0)
#define pLsrPrimV512UInt64Store(arr, off, idx, v)           \
    do {                                                               \
        _mm512_packstorelo_epi64((void *)((uint64_t*)((char*)arr+off) + idx),(v)); \
        _mm512_packstorehi_epi64((char *)((uint64_t*)((char*)arr+off) + idx) + 64,(v)); \
    } while (0)
#endif

#ifdef HAS_ALIGNMENT
#define pLsrPrimV512UInt64Load(dest, arr, off, idx)  \
    (dest) = _mm512_load_epi64((void const *)((uint64_t*)((char*)arr+off) + idx))
#else
#define pLsrPrimV512UInt64Load(dest, arr, off, idx)                   \
    do {                                                               \
        (dest) = _mm512_loadunpacklo_epi64((dest),(void *)((uint64_t*)((char*)arr+off) + idx)); \
        (dest) = _mm512_loadunpackhi_epi64((dest),(char *)((uint64_t*)((char*)arr+off) + idx) + 64); \
    } while (0)
#endif

#ifdef HAS_ALIGNMENT
#define pLsrPrimV512RefLoad(dest, arr, off, idx)  \
    (dest) = _mm512_load_epi64((void const *)((void**)((char*)arr+off) + idx))
#else
#define pLsrPrimV512RefLoad(dest, arr, off, idx)                   \
    do {                                                               \
        (dest) = _mm512_loadunpacklo_epi64((dest),(void *)((void**)((char*)arr+off) + idx)); \
        (dest) = _mm512_loadunpackhi_epi64((dest),(char *)((void**)((char*)arr+off) + idx) + 64); \
    } while (0)
#endif

#ifdef HAS_ALIGNMENT
#define pLsrPrimV512RefStore(arr, off, idx, v)           \
    _mm512_store_epi64((void const *)((void**)((char*)arr+off) + idx), v)
#else
#define pLsrPrimV512RefStore(arr, off, idx, v)           \
    do {                                                               \
        _mm512_packstorelo_epi64((void *)((void**)((char*)arr+off) + idx),(v)); \
        _mm512_packstorehi_epi64((char *)((void**)((char*)arr+off) + idx) + 64,(v)); \
    } while (0)
#endif



#define pLsrPrimV512Float64LoadStrided(dest, arr, off, idx, stride)     \
    do {                                                                \
        (dest) = _mm512_set_pd(*((float64*)((char*)arr + off) + idx + 7*stride), \
                               *((float64*)((char*)arr + off) + idx + 6*stride), \
                               *((float64*)((char*)arr + off) + idx + 5*stride), \
                               *((float64*)((char*)arr + off) + idx + 4*stride), \
                               *((float64*)((char*)arr + off) + idx + 3*stride), \
                               *((float64*)((char*)arr + off) + idx + 2*stride), \
                               *((float64*)((char*)arr + off) + idx + 1*stride), \
                               *((float64*)((char*)arr + off) + idx + 0*stride));    \
    } while (0)

#define pLsrPrimV512Float32LoadStrided(dest, arr, off, idx, stride)     \
    do {                                                                \
        (dest) = _mm512_set_ps(*((float32*)((char*)arr + off) + idx + 15*stride), \
                               *((float32*)((char*)arr + off) + idx + 14*stride), \
                               *((float32*)((char*)arr + off) + idx + 13*stride), \
                               *((float32*)((char*)arr + off) + idx + 12*stride), \
                               *((float32*)((char*)arr + off) + idx + 11*stride), \
                               *((float32*)((char*)arr + off) + idx + 10*stride), \
                               *((float32*)((char*)arr + off) + idx + 9*stride), \
                               *((float32*)((char*)arr + off) + idx + 8*stride), \
                               *((float32*)((char*)arr + off) + idx + 7*stride), \
                               *((float32*)((char*)arr + off) + idx + 6*stride), \
                               *((float32*)((char*)arr + off) + idx + 5*stride), \
                               *((float32*)((char*)arr + off) + idx + 4*stride), \
                               *((float32*)((char*)arr + off) + idx + 3*stride), \
                               *((float32*)((char*)arr + off) + idx + 2*stride), \
                               *((float32*)((char*)arr + off) + idx + 1*stride), \
                               *((float32*)((char*)arr + off) + idx + 0*stride));    \
    } while (0)

#define pLsrPrimV512Float64StoreStrided(arr, off, idx, stride, src)     \
    do {                                                                \
        __declspec(align(64)) double ftemp[8];                          \
        _mm512_store_pd(ftemp, src);                                   \
        *((float64*)((char*)arr + off) + idx + 0*stride) = ftemp[0];    \
        *((float64*)((char*)arr + off) + idx + 1*stride) = ftemp[1];    \
        *((float64*)((char*)arr + off) + idx + 2*stride) = ftemp[2];    \
        *((float64*)((char*)arr + off) + idx + 3*stride) = ftemp[3];    \
        *((float64*)((char*)arr + off) + idx + 4*stride) = ftemp[4];    \
        *((float64*)((char*)arr + off) + idx + 5*stride) = ftemp[5];    \
        *((float64*)((char*)arr + off) + idx + 6*stride) = ftemp[6];    \
        *((float64*)((char*)arr + off) + idx + 7*stride) = ftemp[7];    \
    } while (0)

#define pLsrPrimV512UInt64StoreStrided(arr, off, idx, stride, src)      \
    do {                                                                \
        __declspec(align(64)) uint64_t ftemp[8];                        \
        _mm512_store_epi64(ftemp, src);                                \
        *((uint64_t*)((char*)arr + off) + idx + 0*stride) = ftemp[0];   \
        *((uint64_t*)((char*)arr + off) + idx + 1*stride) = ftemp[1];   \
        *((uint64_t*)((char*)arr + off) + idx + 2*stride) = ftemp[2];   \
        *((uint64_t*)((char*)arr + off) + idx + 3*stride) = ftemp[3];   \
        *((uint64_t*)((char*)arr + off) + idx + 4*stride) = ftemp[4];   \
        *((uint64_t*)((char*)arr + off) + idx + 5*stride) = ftemp[5];   \
        *((uint64_t*)((char*)arr + off) + idx + 6*stride) = ftemp[6];   \
        *((uint64_t*)((char*)arr + off) + idx + 7*stride) = ftemp[7];   \
    } while (0)

#define pLsrPrimV512Float32StoreStrided(arr, off, idx, stride, src)     \
    do {                                                                \
        __declspec(align(64)) float ftemp[16];                          \
        _mm512_store_ps(ftemp, src);                                   \
        *((float32*)((char*)arr + off) + idx + 0*stride) = ftemp[0];    \
        *((float32*)((char*)arr + off) + idx + 1*stride) = ftemp[1];    \
        *((float32*)((char*)arr + off) + idx + 2*stride) = ftemp[2];    \
        *((float32*)((char*)arr + off) + idx + 3*stride) = ftemp[3];    \
        *((float32*)((char*)arr + off) + idx + 4*stride) = ftemp[4];    \
        *((float32*)((char*)arr + off) + idx + 5*stride) = ftemp[5];    \
        *((float32*)((char*)arr + off) + idx + 6*stride) = ftemp[6];    \
        *((float32*)((char*)arr + off) + idx + 7*stride) = ftemp[7];    \
        *((float32*)((char*)arr + off) + idx + 8*stride) = ftemp[8];    \
        *((float32*)((char*)arr + off) + idx + 9*stride) = ftemp[9];    \
        *((float32*)((char*)arr + off) + idx + 10*stride) = ftemp[10];  \
        *((float32*)((char*)arr + off) + idx + 11*stride) = ftemp[11];  \
        *((float32*)((char*)arr + off) + idx + 12*stride) = ftemp[12];  \
        *((float32*)((char*)arr + off) + idx + 13*stride) = ftemp[13];  \
        *((float32*)((char*)arr + off) + idx + 14*stride) = ftemp[14];  \
        *((float32*)((char*)arr + off) + idx + 15*stride) = ftemp[15];  \
    } while (0)

#define pLsrPrimV512UInt64LoadStrided(dest, arr, off, idx, stride)      \
    do {                                                                \
        (dest) = _mm512_set_epi64(*((uint64_t*)((char*)arr + off) + idx + 7*stride), \
                               *((uint64_t*)((char*)arr + off) + idx + 6*stride), \
                               *((uint64_t*)((char*)arr + off) + idx + 5*stride), \
                               *((uint64_t*)((char*)arr + off) + idx + 4*stride), \
                               *((uint64_t*)((char*)arr + off) + idx + 3*stride), \
                               *((uint64_t*)((char*)arr + off) + idx + 2*stride), \
                               *((uint64_t*)((char*)arr + off) + idx + 1*stride), \
                               *((uint64_t*)((char*)arr + off) + idx + 0*stride));    \
    } while (0)

#define pLsrPrimV512RefLoadStrided(dest, arr, off, idx, stride)     \
    do {                                                                \
        (dest) = _mm512_set_epi64(*((void**)((char*)arr + off) + idx + 7*stride), \
                                  *((void**)((char*)arr + off) + idx + 6*stride), \
                                  *((void**)((char*)arr + off) + idx + 5*stride), \
                                  *((void**)((char*)arr + off) + idx + 4*stride), \
                                  *((void**)((char*)arr + off) + idx + 3*stride), \
                                  *((void**)((char*)arr + off) + idx + 2*stride), \
                                  *((void**)((char*)arr + off) + idx + 1*stride), \
                                  *((void**)((char*)arr + off) + idx + 0*stride));    \
    } while (0)

#define pLsrPrimV512ReftoreStrided(arr, off, idx, stride, src)     \
    do {                                                                \
        __declspec(align(64)) void * ftemp[8];                         \
        _mm512_store_epi64(ftemp, src);                                   \
        *((void**)((char*)arr + off) + idx + 0*stride) = ftemp[0];    \
        *((void**)((char*)arr + off) + idx + 1*stride) = ftemp[1];    \
        *((void**)((char*)arr + off) + idx + 2*stride) = ftemp[2];    \
        *((void**)((char*)arr + off) + idx + 3*stride) = ftemp[3];    \
        *((void**)((char*)arr + off) + idx + 4*stride) = ftemp[4];    \
        *((void**)((char*)arr + off) + idx + 5*stride) = ftemp[5];    \
        *((void**)((char*)arr + off) + idx + 6*stride) = ftemp[6];    \
        *((void**)((char*)arr + off) + idx + 7*stride) = ftemp[7];    \
    } while (0)

#define pLsrPrimV512Float64Const(dest, c0, c1, c2, c3, c4, c5, c6, c7)  \
    ((dest) = _mm512_set_pd(c7, c6, c5, c4, c3, c2, c1, c0))


#define pLsrPrimV256512CastSInt64ToFloat32(dest, a) \
    do {                                            \
        __declspec(align(64)) __int64 the_ints[8]; \
        _mm512_store_epi64(the_ints, a);           \
        (dest) = _mm512_set_ps(0.0,                 \
                               0.0,                 \
                               0.0,                 \
                               0.0,                 \
                               0.0,                 \
                               0.0,                 \
                               0.0,                 \
                               0.0,                 \
                               (float)the_ints[7],  \
                               (float)the_ints[6],  \
                               (float)the_ints[5],  \
                               (float)the_ints[4],  \
                               (float)the_ints[3],  \
                               (float)the_ints[2],  \
                               (float)the_ints[1],  \
                               (float)the_ints[0]); \
    } while (0)

#define pLsrPrimV512ReduceASInt64Plus(dest, init, a) \
    do { \
        __declspec(align(64)) __int64 vi64[8];                         \
        _mm512_store_epi64(vi64, a);                                   \
        (dest) = (init);                                                \
        unsigned i;                                                     \
        for(i=0;i<8;++i) (dest) += vi64[i];                             \
    } while(0)

// ==============================================================================================



#ifndef HAS_256

typedef __m512  PlsrVector256F32;
typedef __m512i PlsrVector256B32;
typedef __m512d PlsrVector256F64;
typedef __m512i PlsrVector256Ref;

#define lower256of512  ((__mmask16)0xff)
#define higher256of512 ((__mmask16)0xff00)
#define lower256of512double  ((__mmask8)0xf)
#define higher256of512double ((__mmask8)0xf0)

#define pLsrPrimV256PointwiseFloat32Ln(dest, a) ((dest) = _mm512_mask_log_ps(dest, lower256of512, a))
#define pLsrPrimV256PointwiseFloat32Exp(dest, a) ((dest) = _mm512_mask_exp_ps(dest, lower256of512, a))
#define pLsrPrimV256PointwiseFloat32Negate(dest, a) ((dest) = _mm512_mask_mul_ps(dest, lower256of512, a, _mm512_set1_ps(-1.0)))

#define pLsrPrimV512256CastFloat32ToSInt64(dest, a) \
    do { \
        __declspec(align(64)) float32 vi32[16];                        \
        __declspec(align(64)) __int64 vi64[8];                        \
        _mm512_store_ps(vi32, a);                                  \
        unsigned i; \
        for(i=0;i<8;i++) vi64[i] = vi32[i]; \
        (dest) = _mm512_load_epi64(vi64); \
    } while (0)

//#define pLsrPrimV512256ConvertSInt64FromSInt32(dest, a)  ((dest) = _mm512_cvtepi32_epi64(a))
#define pLsrPrimV512256ConvertSInt64FromSInt32(dest, a) \
    do { \
        __declspec(align(64)) __int32 vi32[16];                        \
        __declspec(align(64)) __int64 vi64[8];                        \
        _mm512_store_epi32(vi32, a);                                  \
        unsigned i; \
        for(i=0;i<8;i++) vi64[i] = vi32[i]; \
        (dest) = _mm512_load_epi64(vi64); \
    } while (0)

#define pLsrPrimV256DataConcatF32(dest, lower, upper) \
    do { \
        __m512 res = lower;                                             \
        res = _mm512_mask_permute4f128_ps(res, higher256of512, upper, _MM_PERM_BABA);\
        (dest) = res;                                                   \
    } while (0)

/*
#define pLsrPrimV256DataConcatF32(dest, lower, upper) \
    do { \
        __m512 res = lower;                                             \
        res = _mm512_mask_alignr_epi32(res, higher256of512, upper, res, 8);\
        (dest) = res;                                                   \
    } while (0)
#define pLsrPrimV256DataConcatF32(dest, lower, upper) \
    do { \
        __m512 res = lower;                                             \
        __declspec(align(64)) double ftemp[8];                         \
        _mm512_mask_store_pd(&ftemp,lower256of512double,upper);         \
        _mm512_mask_extload_pd(res,higher256of512double,&ftemp,_MM_UPCONV_PD_NONE,_MM_BROADCAST_4X8,0); \
        (dest) = res;                                                   \
    } while (0)
*/

#if 0
#define pLsrPrimV256PointwiseCondMov(dest, mask, a, b) \
    do { \
       __mmask16 realmask=0; \
       __declspec(align(64)) __int64 mvec[8];       \
        _mm512_store_epi64(mvec,(mask));                          \
       unsigned i; \
       for(i=0;i<8;++i) { \
           if(mvec[i]) realmask |= (1 << i); \
       } \
       (dest) = _mm512_mask_blend_ps((realmask),(a),(b)); \
    } while (0)
#else
#define pLsrPrimV256DataBlendB32(dest, mask, a, b) \
    do { \
       __mmask16 realmask=mask; \
       (dest) = _mm512_mask_blend_epi32((realmask),(a),(b)); \
    } while (0)
#define pLsrPrimV256DataBlendF32(dest, mask, a, b) \
    do { \
       __mmask16 realmask=mask; \
       (dest) = _mm512_mask_blend_ps((realmask),(a),(b)); \
    } while (0)
#endif

#define pLsrPrimV256DataVectorF32(dest, c0, c1, c2, c3, c4, c5, c6, c7) \
  pLsrPrimV512Float32Const(dest, c0, c1, c2, c3, c4, c5, c6, c7, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

#define pLsrPrimV256B32LoadVI64(dest, arr, off, vi)                       \
    do {                                                                \
        __declspec(align(64)) __int64 vi64[8];                         \
        __declspec(align(64)) __int32 vi32[16];                        \
        _mm512_store_epi64(vi64, vi);                                  \
        unsigned i;                                                     \
        for(i=0;i<8;++i) vi32[i] = vi64[i];                             \
        __m512i indices32bit;                                           \
        indices32bit = _mm512_load_epi32(vi32);                        \
        _mm512_mask_i32gather_epi32(dest,lower256of512,indices32bit,((char*)arr + off),1); \
    } while (0)

#define pLsrPrimV256F32LoadVI64(dest, arr, off, vi)                       \
    do {                                                                \
        __declspec(align(64)) __int64 vi64[8];                         \
        __declspec(align(64)) __int32 vi32[16];                        \
        _mm512_store_epi64(vi64, vi);                                  \
        unsigned i;                                                     \
        for(i=0;i<8;++i) vi32[i] = vi64[i];                             \
        __m512i indices32bit;                                           \
        indices32bit = _mm512_load_epi32(vi32);                        \
        _mm512_mask_i32gather_ps(dest,lower256of512,indices32bit,((char*)arr + off),1); \
    } while (0)

#define pLsrPrimV256F32LoadVI(dest, arr, off, vi) _mm512_mask_i32gather_ps(dest,lower256of512,vi,((char*)arr + off),1);

#define pLsrPrimV256PointwiseFloat32Plus(dest, a, b)  ((dest) = _mm512_mask_add_ps( (dest), lower256of512, (a), (b)))
#define pLsrPrimV256PointwiseFloat32Minus(dest, a, b) ((dest) = _mm512_mask_sub_ps( (dest), lower256of512, (a), (b)))
#define pLsrPrimV256PointwiseFloat32Times(dest, a, b) ((dest) = _mm512_mask_mul_ps( (dest), lower256of512, (a), (b)))
#define pLsrPrimV256PointwiseFloat32Divide(dest, a, b) ((dest) = _mm512_mask_mul_ps( (dest), lower256of512, (a), _mm512_mask_rcp23_ps( (b), lower256of512, (b))))
#define pLsrPrimV256PointwiseFloat32Sqrt(dest, a) ((dest) = _mm512_mask_sqrt_ps( (dest), lower256of512, (a)))

#define pLsrPrimV256ReduceAFloat32Plus(dest, init, a) reduce256AFloat32Plus(dest, init, a)

#define reduce256AFloat32Plus(dest, init, a)                            \
  (dest) = (_mm512_mask_reduce_add_ps(lower256of512,a) + (init));

#define pLsrPrimV256Float32Lift(dest, a) \
  do { \
    float ftemp = a; \
    (dest) = _mm512_mask_extload_ps((dest), lower256of512, (void const *)(&ftemp),_MM_UPCONV_PS_NONE,_MM_BROADCAST_1X16,0); \
  } while (0)

#define pLsrPrimV256DataBroadcastF32(dest, c0) pLsrPrimV512Float32Lift(dest, c0)

#ifdef HAS_ALIGNMENT
#define pLsrPrimV256Float32Load(dest, arr, off, idx)                 \
    (dest) = _mm512_mask_load_ps((dest), lower256of512, (void const *)((float*)((char*)arr+off) + idx))
#else
#define pLsrPrimV256Float32Load(dest, arr, off, idx)                     \
    do {                                                               \
        (dest) = _mm512_mask_loadunpacklo_ps((dest),lower256of512,(void *)((float*)((char*)arr+off) + idx)); \
        (dest) = _mm512_mask_loadunpackhi_ps((dest),lower256of512,(char *)((float*)((char*)arr+off) + idx) + 64); \
    } while (0)
#endif

#define STRIDE_VIA_GATHER

#ifdef STRIDE_VIA_GATHER
#define pLsrPrimV256Float32StoreStrided(arr, off, idx, stride, src)   \
    do {                                                                        \
        __m512i indices, vstride;                                               \
        indices = _mm512_set_epi32(0,0,0,0,0,0,0,0,7,6,5,4,3,2,1,0);            \
        vstride = _mm512_set1_epi32(stride);                                    \
        indices = _mm512_mask_mullo_epi32(indices,lower256of512,indices,vstride); \
        _mm512_mask_i32scatter_ps((float*)((char*)arr + off) + idx,lower256of512,indices,(src),_MM_SCALE_1); \
    } while (0)
#else
#define pLsrPrimV256Float32StoreStrided(arr, off, idx, stride, src)   \
    do {                                                              \
        __declspec(align(64)) float ftemp[16];                       \
        _mm512_mask_store_ps(ftemp, lower256of512, src);             \
        *((float*)((char*)arr + off) + idx + 0*stride) = ftemp[0];    \
        *((float*)((char*)arr + off) + idx + 1*stride) = ftemp[1];    \
        *((float*)((char*)arr + off) + idx + 2*stride) = ftemp[2];    \
        *((float*)((char*)arr + off) + idx + 3*stride) = ftemp[3];    \
        *((float*)((char*)arr + off) + idx + 4*stride) = ftemp[4];    \
        *((float*)((char*)arr + off) + idx + 5*stride) = ftemp[5];    \
        *((float*)((char*)arr + off) + idx + 6*stride) = ftemp[6];    \
        *((float*)((char*)arr + off) + idx + 7*stride) = ftemp[7];    \
    } while (0)
#endif

#ifdef HAS_ALIGNMENT
#define pLsrPrimV256Float32Store(arr, off, idx, v) \
    _mm512_mask_store_ps((void const *)((float*)((char*)arr+off) + idx), lower256of512, v)
#else
#define pLsrPrimV256Float32Store(arr, off, idx, v)             \
    do {                                                               \
        _mm512_mask_packstorelo_ps((void *)((float*)((char*)arr+off) + idx),lower256of512,v); \
        _mm512_mask_packstorehi_ps((char *)((float*)((char*)arr+off) + idx) + 64,lower256of512,v); \
    } while (0)
#endif

#if 0
        if(stride <= 8 && stride/2%==0) {                                       \
            (dest) = _mm512_mask_i32gather_ps((dest),lower256of512,,(float*)((char*)arr + off) + idx,stride); \
        } else {                                                                \
            (dest) = _mm512_mask_i32gather_ps((dest),lower256of512,,(float*)((char*)arr + off) + idx,_MM_SCALE_1); \
        }
#endif

#ifdef STRIDE_VIA_GATHER
#define pLsrPrimV256Float32LoadStrided(dest, arr, off, idx, stride)             \
    do {                                                                        \
        __m512i indices, vstride;                                               \
        indices = _mm512_set_epi32(0,0,0,0,0,0,0,0,7,6,5,4,3,2,1,0);            \
        vstride = _mm512_set1_epi32(stride);                                    \
        indices = _mm512_mask_mullo_epi32(indices,lower256of512,indices,vstride); \
        (dest) = _mm512_mask_i32gather_ps((dest),lower256of512,indices,(float*)((char*)arr + off) + idx,_MM_SCALE_1); \
    } while (0)
#else
#define pLsrPrimV256Float32LoadStrided(dest, arr, off, idx, stride)             \
    do {                                                                        \
        (dest) = _mm512_set_ps(0.0,                                             \
                               0.0,                                             \
                               0.0,                                             \
                               0.0,                                             \
                               0.0,                                             \
                               0.0,                                             \
                               0.0,                                             \
                               0.0,                                             \
                               *((float*)((char*)arr + off) + idx + 7*stride),  \
                               *((float*)((char*)arr + off) + idx + 6*stride),  \
                               *((float*)((char*)arr + off) + idx + 5*stride),  \
                               *((float*)((char*)arr + off) + idx + 4*stride),  \
                               *((float*)((char*)arr + off) + idx + 3*stride),  \
                               *((float*)((char*)arr + off) + idx + 2*stride),  \
                               *((float*)((char*)arr + off) + idx + 1*stride),  \
                               *((float*)((char*)arr + off) + idx + 0*stride)); \
    } while (0)
#endif

#define pLsrPrimV256F32LoadVS(dest, arr, offset, idx, stride)               \
    do {                                                                    \
        if (stride == 1) {                                                  \
            pLsrPrimV256Float32Load(dest, arr, offset, idx);                \
        } else {                                                            \
            pLsrPrimV256Float32LoadStrided(dest, arr, offset, idx, stride); \
        }                                                                   \
    } while (0)

#define pLsrPrimV256F32StoreVS(arr, offset, idx, stride, v)               \
    do {                                                                  \
        if (stride == 1) {                                                \
            pLsrPrimV256Float32Store(arr, offset, idx, v);                \
        } else {                                                          \
            pLsrPrimV256Float32StoreStrided(arr, offset, idx, stride, v); \
        }                                                                 \
    } while (0)

#ifdef HAS_ALIGNMENT
#define pLsrPrimV256UInt32Load(dest, arr, off, idx)                 \
    (dest) = _mm512_mask_load_epi32((dest), lower256of512, (void const *)((uint32_t*)((char*)arr+off) + idx))
#else
#define pLsrPrimV256UInt32Load(dest, arr, off, idx)                     \
    do {                                                               \
        (dest) = _mm512_mask_loadunpacklo_epi32((dest),lower256of512,(void *)((uint32_t*)((char*)arr+off) + idx)); \
        (dest) = _mm512_mask_loadunpackhi_epi32((dest),lower256of512,(char *)((uint32_t*)((char*)arr+off) + idx) + 64); \
    } while (0)
#endif

#define pLsrPrimV256UInt32LoadStrided(dest, arr, off, idx, stride)             \
    do {                                                                        \
        (dest) = _mm512_set_epi32(0,                                             \
                               0,                                             \
                               0,                                             \
                               0,                                             \
                               0,                                             \
                               0,                                             \
                               0,                                             \
                               0,                                             \
                               *((uint32_t*)((char*)arr + off) + idx + 7*stride),  \
                               *((uint32_t*)((char*)arr + off) + idx + 6*stride),  \
                               *((uint32_t*)((char*)arr + off) + idx + 5*stride),  \
                               *((uint32_t*)((char*)arr + off) + idx + 4*stride),  \
                               *((uint32_t*)((char*)arr + off) + idx + 3*stride),  \
                               *((uint32_t*)((char*)arr + off) + idx + 2*stride),  \
                               *((uint32_t*)((char*)arr + off) + idx + 1*stride),  \
                               *((uint32_t*)((char*)arr + off) + idx + 0*stride)); \
    } while (0)

#define pLsrPrimV256B32LoadVS(dest, arr, offset, idx, stride)          \
    do {                                                                \
        if (stride == 1) {                                              \
            pLsrPrimV256UInt32Load(dest, arr, offset, idx);            \
        } else {                                                        \
            pLsrPrimV256UInt32LoadStrided(dest, arr, offset, idx, stride); \
        }                                                               \
    } while (0)

#define pLsrPrimV256UInt32StoreStrided(arr, off, idx, stride, src)   \
    do {                                                              \
        __declspec(align(64)) uint32_t ftemp[16];                       \
        _mm512_mask_store_epi32(ftemp, lower256of512, src);             \
        *((uint32_t*)((char*)arr + off) + idx + 0*stride) = ftemp[0];    \
        *((uint32_t*)((char*)arr + off) + idx + 1*stride) = ftemp[1];    \
        *((uint32_t*)((char*)arr + off) + idx + 2*stride) = ftemp[2];    \
        *((uint32_t*)((char*)arr + off) + idx + 3*stride) = ftemp[3];    \
        *((uint32_t*)((char*)arr + off) + idx + 4*stride) = ftemp[4];    \
        *((uint32_t*)((char*)arr + off) + idx + 5*stride) = ftemp[5];    \
        *((uint32_t*)((char*)arr + off) + idx + 6*stride) = ftemp[6];    \
        *((uint32_t*)((char*)arr + off) + idx + 7*stride) = ftemp[7];    \
    } while (0)

#ifdef HAS_ALIGNMENT
#define pLsrPrimV256UInt32Store(arr, off, idx, v) \
    _mm512_mask_store_epi32((void const *)((uint32_t*)((char*)arr+off) + idx), lower256of512, v)
#else
#define pLsrPrimV256UInt32Store(arr, off, idx, v)             \
    do {                                                               \
        _mm512_mask_packstorelo_epi32((void *)((uint32_t*)((char*)arr+off) + idx),lower256of512,v); \
        _mm512_mask_packstorehi_epi32((char *)((uint32_t*)((char*)arr+off) + idx) + 64,lower256of512,v); \
    } while (0)
#endif

#define pLsrPrimV256B32StoreVS(arr, offset, idx, stride, v)               \
    do {                                                                  \
        if (stride == 1) {                                                \
            pLsrPrimV256UInt32Store(arr, offset, idx, v);                \
        } else {                                                          \
            pLsrPrimV256UInt32StoreStrided(arr, offset, idx, stride, v); \
        }                                                                 \
    } while (0)

#else // HAS_256

typedef __m256  PlsrVector256F32;
typedef __m256i PlsrVector256B32;
typedef __m256d PlsrVector256F64;
typedef __m256i PlsrVector256Ref;

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

// *****************************************************************************
// Data operations on the more abstract B32/F32/F64 types
// *****************************************************************************

#define pLsrPrimV256DataVectorB32(dest, c0, c1, c2, c3, c4, c5, c6, c7) \
  pLsrPrimV256UInt32Const(dest, c0, c1, c2, c3, c4, c5, c6, c7)

#define pLsrPrimV256DataVectorF32(dest, c0, c1, c2, c3, c4, c5, c6, c7) \
  pLsrPrimV256Float32Const(dest, c0, c1, c2, c3, c4, c5, c6, c7)

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

#define pLsrPrimV256PointwiseUInt32Plus(dest, a, b)  vector256PointwiseUInt32Plus(dest, a, b)
#define pLsrPrimV256PointwiseUInt32Minus(dest, a, b) vector256PointwiseUInt32Minus(dest, a, b)
#define pLsrPrimV256PointwiseUInt32Times(dest, a, b) vector256PointwiseUInt32Times(dest, a, b)

#define pLsrPrimV256PointwiseSInt32Plus(dest, a, b)  vector256PointwiseSInt32Plus(dest, a, b)
#define pLsrPrimV256PointwiseSInt32Minus(dest, a, b) vector256PointwiseSInt32Minus(dest, a, b)
#define pLsrPrimV256PointwiseSInt32Times(dest, a, b) vector256PointwiseSInt32Times(dest, a, b)

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

#define vector256PointwiseUInt32Plus(dest, a, b)  integer256via128(dest, a, b, pLsrPrimV128PointwiseUInt32Plus)
#define vector256PointwiseUInt32Minus(dest, a, b) integer256via128(dest, a, b, pLsrPrimV128PointwiseUInt32Minus)
#define vector256PointwiseUInt32Times(dest, a, b) integer256via128(dest, a, b, pLsrPrimV128PointwiseUInt32Times)

#define vector256PointwiseSInt32Plus(dest, a, b)  integer256via128(dest, a, b, pLsrPrimV128PointwiseSInt32Plus)
#define vector256PointwiseSInt32Minus(dest, a, b) integer256via128(dest, a, b, pLsrPrimV128PointwiseSInt32Minus)
#define vector256PointwiseSInt32Times(dest, a, b) integer256via128(dest, a, b, pLsrPrimV128PointwiseSInt32Times)

// *****************************************************************************
// Reductions (horizontals)
// *****************************************************************************

#define pLsrPrimV256ReduceAUInt32Plus(dest, init, a)  vector256reduceAUInt32Plus(dest, init, a)
#define pLsrPrimV256ReduceAUInt32Times(dest, init, a) vector256reduceAUInt32Times(dest, init, a)
#define pLsrPrimV256ReduceAUInt32Max(dest, init, a)   vector256reduceAUInt32Max(dest, init, a)
#define pLsrPrimV256ReduceAUInt32Min(dest, init, a)   vector256reduceAUInt32Min(dest, init, a)

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

// *****************************************************************************
// Data operations
// *****************************************************************************

#define pLsrPrimV256UInt32Lift(dest, a) vector256UInt32Lift(dest, a)
#define pLsrPrimV256UInt32Const(dest, c0, c1, c2, c3, c4, c5, c6, c7)   \
    vector256UInt32Const(dest, c0, c1, c2, c3, c4, c5, c6, c7)

#define vector256UInt32Lift(dest, a)                                    \
  do {                                                                  \
    __m256i vector256UInt32Lift_dest = {0, 0, 0, 0, 0, 0, 0, 0};        \
    __m128i vector256UInt32Lift_lo = _mm_set1_epi32(a);                 \
    vector256UInt32Lift_dest = _mm256_insertf128_si256(vector256UInt32Lift_dest, vector256UInt32Lift_lo, 1); \
    vector256UInt32Lift_dest = _mm256_insertf128_si256(vector256UInt32Lift_dest, vector256UInt32Lift_lo, 0); \
    (dest) = vector256UInt32Lift_dest;                                  \
  } while (0)

#define vector256UInt32Const(dest, c0, c1, c2, c3, c4, c5, c6, c7)      \
    do {                                                                \
        __m256i vector256UInt32Const_out = {0, 0, 0, 0, 0, 0, 0, 0};    \
        __m128i vector256UInt32Const_lo = _mm_set_epi32(c3, c2, c1, c0); \
        __m128i vector256UInt32Const_hi = _mm_set_epi32(c7, c6, c5, c4); \
        vector256UInt32Const_out = _mm256_insertf128_si256(vector256UInt32Const_out, vector256UInt32Const_hi, 1); \
        vector256UInt32Const_out = _mm256_insertf128_si256(vector256UInt32Const_out, vector256UInt32Const_lo, 0); \
        (dest) = vector256UInt32Const_out;                              \
    } while (0)

#define pLsrPrimV256UInt32Load(dest, arr, off, idx) pLsrPrimV256UInt32LoadHelp (dest, arr, off, idx)
#define pLsrPrimV256UInt32Store(arr, off, idx, v) pLsrPrimV256UInt32StoreHelp (arr, off, idx, v)

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

#define pLsrPrimV256Float64Lift(dest, a)               \
  do {                                                  \
    double temp = a;                                     \
    (dest) = _mm256_broadcast_sd((double *)(&temp));     \
  } while(0)

// ================
// Ref - 256-bit
// ================

#define pLsrPrimV256RefLoad(dest, arr, off, idx) pLsrPrimV256UInt32Load(dest, arr, off, idx)

#endif

#endif _PLSR_PRIMS_VECTOR_MIC_H_
