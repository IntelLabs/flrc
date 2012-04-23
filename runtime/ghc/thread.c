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

int rtsSupportsBoundThreads() { return 0; }


