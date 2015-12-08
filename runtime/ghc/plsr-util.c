#include <assert.h>
#include <stdlib.h>
#include "hrc/pil.h"
#include "hrc/plsr-util.h"

/* Time in 100 ns intervals since January 1, 1601 */
uint64 pLsrEventsTimeStamp()
{
#ifdef PLSR_LINUX
    struct timeval tv;
    gettimeofday(&tv,NULL);
    uint64 time;
    time = (((uint64)tv.tv_sec) * 1000000) + ((uint64)tv.tv_usec);
    time *= 10; // convert from micro-seconds to 100-nanosecond intervals
#else // PLSR_LINUX
    FILETIME now;
    uint32 lowTime;
    uint32 highTime;
    uint64 time;

    GetSystemTimeAsFileTime(&now);
    lowTime = now.dwLowDateTime;
    highTime = now.dwHighDateTime;

    time = ((uint64)highTime)<<32 | (uint64)lowTime;
#endif // PLSR_LINUX
    return time;
}

void pLsrDisableErrorBox()
{
#ifndef PLSR_LINUX
    unsigned int mode = SetErrorMode(SEM_NOGPFAULTERRORBOX);
    SetErrorMode(mode | SEM_NOGPFAULTERRORBOX);
#endif // PLSR_LINUX
}
