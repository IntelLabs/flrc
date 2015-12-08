/* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

#ifndef _PLSR_PRIMS_VECTOR_H_
#define _PLSR_PRIMS_VECTOR_H_

#ifdef P_USE_VI_SSE
#define pLsrViWidth 128
#include "hrc/plsr-prims-vector-sse.h"

#elif P_USE_VI_AVX
#define pLsrViWidth 256
#include "hrc/plsr-prims-vector-sse.h"
#include "hrc/plsr-prims-vector-avx.h"

#elif P_USE_VI_MIC
#define pLsrViWidth 512
#include "hrc/plsr-prims-vector-mic.h"

#else
#define pLsrViWidth 128

#endif

#endif /* _PLSR_PRIMS_VECTOR_H_ */
