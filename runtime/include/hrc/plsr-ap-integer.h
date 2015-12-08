 /* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

#ifdef PLSR_GMP_USE_DEFAULT
#ifdef __pillar2c__
#define PLSR_GMP_USE_GALLOCATE
#else /* ! __pillar2c__ */
#define PLSR_GMP_USE_PINNING
#endif /* ! __pillar2c__ */
#endif /* !PLSR_GMP_USE_DEFAULT */

#ifdef PLSR_NO_GMP_INTEGERS
#include "hrc/plsr-iflc-integer.h"
#else /* !PLSR_NO_GMP_INTEGERS */
#ifdef PLSR_GMP_USE_GALLOCATE
#include "hrc/plsr-gmp-integer-gallocate.h"
#else /* !PLSR_GMP_USE_GALLOCATE */
#include "hrc/plsr-gmp-integer.h"
#endif /* !PLSR_GMP_USE_GALLOCATE */
#endif /* !PLSR_NO_GMP_INTEGERS */
