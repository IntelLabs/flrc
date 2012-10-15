/* The Intel P to C/Pillar Compiler */
/* COPYRIGHT_NOTICE_1 */

typedef enum {
    ISK_SystemEventThreadEventManager, ISK_SystemEventThreadIOManager, ISK_GHCConcWindowsPendingDelays,
    ISK_GHCConcWindowsIOManagerThread, ISK_GHCConcWindowsProdding, ISK_GHCConcSignalSignalHandler, ISK_Num
} IhrStoreKey;

static void* ihrStore[ISK_Num] = {NULL, };
static struct PrtMutex* ihrStoreLock;

/* hard code n_capabilities to 1 for now */
unsigned int n_capabilities = 1;

void ihrGlobalInit()
{
    /* XXX NG: PRT mutexes are implemented yet
    ihrStoreLock = prtMutexCreate(NULL);
    assert(ihrStoreLock);*/
}

void* getOrSetKey(IhrStoreKey k, void* p)
{
    void* ret = ihrStore[k];
    if (!ret) {
        /* XXX NG: PRT mutexes are implemented yet
           assert(!prtMutexLock(ihrStoreLock));*/
        ret = ihrStore[k];
        if (!ret) ihrStore[k] = ret = p;
        /* XXX NG: PRT mutexes are implemented yet
           assert(!prtMutexUnlock(ihrStoreLock));*/
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
