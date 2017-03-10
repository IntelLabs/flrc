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

#include <prt/prt.h>
#include "hrc/plsr-util.h"
#include "hrc/ghc/Globals.h"

static void* ihrStore[ISK_Num] = {NULL, };
static struct prtMutex* ihrStoreLock;

/* hard code n_capabilities to 1 for now */
unsigned int n_capabilities_ = 1;
unsigned int* n_capabilities = &n_capabilities_;

void ihrSetNCapabilities(uint32 n) {
    *n_capabilities = n;
}

void ihrGlobalInit()
{
    ihrStoreLock = prtMutexCreate(NULL);
    assert(ihrStoreLock);
}

void* getOrSetKey(IhrStoreKey k, void* p)
{
    void* ret = ihrStore[k];
    if (!ret) {
        uint32 status;
        status = prtMutexLock(ihrStoreLock);
        assert(!status);
        ret = ihrStore[k];
        if (!ret) ihrStore[k] = ret = p;
        status = prtMutexUnlock(ihrStoreLock);
        assert(!status);
    }
    return ret;
}

void* getOrSetSystemEventThreadEventManagerStore(void* p)
{
    return getOrSetKey(ISK_SystemEventThreadEventManager, p);
}

void* getOrSetSystemEventThreadIOManagerThreadStore(void* p)
{
    return getOrSetKey(ISK_SystemEventThreadIOManager, p);
}

void* getOrSetGHCConcWindowsPendingDelaysStore(void* p)
{
    return getOrSetKey(ISK_GHCConcWindowsPendingDelays, p);
}

void* getOrSetGHCConcWindowsIOManagerThreadStore(void* p)
{
    return getOrSetKey(ISK_GHCConcWindowsIOManagerThread, p);
}

void* getOrSetGHCConcWindowsProddingStore(void* p)
{
    return getOrSetKey(ISK_GHCConcWindowsProdding, p);
}

void* getOrSetGHCConcSignalSignalHandlerStore(void* p)
{
    return getOrSetKey(ISK_GHCConcSignalSignalHandler, p);
}

void sysErrorBelch(char* s) {}

void blockUserSignals() {}

void unblockUserSignals() {}

void stopTimer() {}

void startTimer() {}

void stackOverflow() {}

int lockFile(int fd, uint64 dev, uint64 ino, int for_writing) { return 0; }

int unlockFile(int fd) { return 0; }

uint64 getMonotonicNSec() { return pLsrEventsTimeStamp() * 100; }
