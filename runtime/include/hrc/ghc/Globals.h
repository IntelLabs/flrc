/* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

#ifndef _GHC_GLOBALS_H_
#define _GHC_GLOBALS_H_

#include "hrc/ghc/float.h"

typedef enum {
    ISK_SystemEventThreadEventManager, ISK_SystemEventThreadIOManager, ISK_GHCConcWindowsPendingDelays,
    ISK_GHCConcWindowsIOManagerThread, ISK_GHCConcWindowsProdding, ISK_GHCConcSignalSignalHandler, ISK_Num
} IhrStoreKey;

void ihrSetNCapabilities(uint32 n);
void ihrGlobalInit();
void* getOrSetKey(IhrStoreKey k, void* p);
void* getOrSetSystemEventThreadEventManagerStore(void* p);
void* getOrSetSystemEventThreadIOManagerThreadStore(void* p);
void* getOrSetGHCConcWindowsPendingDelaysStore(void* p);
void* getOrSetGHCConcWindowsIOManagerThreadStore(void* p);
void* getOrSetGHCConcWindowsProddingStore(void* p);
void* getOrSetGHCConcSignalSignalHandlerStore(void* p);
void sysErrorBelch(char* s);
void blockUserSignals();
void unblockUserSignals();
void stopTimer();
void startTimer();
void stackOverflow();
// int lockFile(int fd, uint64 dev, uint64 ino, int for_writing);
// int unlockFile(int fd);
uint64 getMonotonicNSec();

#endif
