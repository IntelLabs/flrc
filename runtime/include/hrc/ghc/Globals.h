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
unsigned int *n_capabilities;

#endif
