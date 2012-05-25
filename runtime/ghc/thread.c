/* The Intel P to C/Pillar Compiler */
/* COPYRIGHT_NOTICE_1 */

/* place holders for now */
int rts_getThreadId (uint32 tid)
{
    return (int)tid;
}

int cmp_thread (uint32 tidA, uint32 tidB)
{
    if (tidA > tidB) return 1;
    else if (tidA < tidB) return -1;
    else return 0;
}

int rtsSupportsBoundThreads()
{
    return 0;
}

/*** IO Manager Stuff ***/

HANDLE getIOManagerEvent()
{
    /* XXX NG: this is for a non-threaded runtime. */
    return NULL;
}

uint32 readIOManagerEvent()
{
    /* XXX NG: this is for a non-threaded runtime. */
    return 0;
}

void sendIOManagerEvent(uint32 e)
{
    /* XXX NG: this is for a non-threaded runtime. */
}
