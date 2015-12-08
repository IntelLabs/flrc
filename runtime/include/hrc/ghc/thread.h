/* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

#ifndef _GHC_THREAD_H_
#define _GHC_THREAD_H_

#include "hrc/ghc/float.h"

I_ rts_getThreadId (W_ tid);
I_ cmp_thread (W_ tidA, W_ tidB);
I_ rtsSupportsBoundThreads();

#if defined (__MINGW32__) || defined(WIN32)
HANDLE getIOManagerEvent();
W_ readIOManagerEvent();
void sendIOManagerEvent(W_ e);
#endif

#endif
