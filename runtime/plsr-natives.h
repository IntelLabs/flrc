/* The Intel P to C/Pillar Compiler */
/* Copyright (C) Intel Corporation, January 2007 */

/* P Natives */

#ifndef _PLSR_NATIVES_H_
#define _PLSR_NATIVES_H_

/**********************************************************************
 * Arithmetic Primitives
 */

static PlsrRational pLsrPIntDiv(PlsrRational a, PlsrRational b)
{
    PlsrInteger ai = pLsrIntegerFromRational(a);
    PlsrInteger bi = pLsrIntegerFromRational(b);
    PlsrInteger q = pLsrIntegerDiv(ai, bi);
    return pLsrRationalFromInteger(q);
}

static PlsrRational pLsrPIntMod(PlsrRational a, PlsrRational b)
{
    PlsrInteger ai = pLsrIntegerFromRational(a);
    PlsrInteger bi = pLsrIntegerFromRational(b);
    PlsrInteger r = pLsrIntegerMod(ai, bi);
    return pLsrRationalFromInteger(r);
}

static PlsrPAny pLsrPIntDivModTD(PlsrPAny (*pair)(PlsrPAny, PlsrPAny),
                                 PlsrRational a, PlsrRational b) {
    PlsrInteger ai = pLsrIntegerFromRational(a);
    PlsrInteger bi = pLsrIntegerFromRational(b);
    PlsrInteger q = NULL;
    PlsrInteger r = NULL;
    pLsrIntegerDivMod(&q, &r, ai, bi);
    PlsrPAny qr = pLsrPRatFromInteger(q);
    PlsrPAny rr = pLsrPRatFromInteger(r);
    return pair(qr, rr);
}

static PlsrPAny pLsrPIntDivModT(PlsrRational a, PlsrRational b)
{
    return pLsrPIntDivModTD(pLsrPPairT, a, b);
}

static PlsrPAny pLsrPIntDivModD(PlsrRational a, PlsrRational b)
{
    return pLsrPIntDivModTD(pLsrPPairD, a, b);
}

/**********************************************************************
 * Pointers
 */

static PlsrPAny pLsrPPtrNewHelper(PlsrPAny a)
{
    pLsrRuntimeError("PPtrNewHelper unimplemented");
    return 0;
}

static PlsrPAny pLsrPPtrNew(PlsrPAny a)
{
    pLsrRuntimeError("PPtrNew unimplemented");
    return 0;
}

static PlsrPAny pLsrPPtrWrite(PlsrPAny a, PlsrPAny b)
{
    pLsrRuntimeError("PPtrWrite unimplemented");
    return 0;
}

static PlsrPAny pLsrPPtrRead(PlsrPAny a)
{
    pLsrRuntimeError("PPtrRead unimplemented");
    return 0;
}

static PlsrPAny pLsrPPtrType(PlsrPAny a)
{
    pLsrRuntimeError("PPtrType unimplemented");
    return 0;
}

/**********************************************************************
 * IO
 */

/* IO Hacks */

#define BUF_SZ (1<<16)

struct PlsrBufList {
    char buf[BUF_SZ];
    struct PlsrBufList* next;
};

static char* pLsrReadAllStdin()
{
    struct PlsrBufList* l = 0;
    size_t lastRead;
    uintp fullBufs = -1, len, cur;
    char* res;
    FILE* f;
    f = stdin;
    assert(f);
    do {
        struct PlsrBufList* p = l;
        fullBufs++;
        l = (struct PlsrBufList*)pLsrAllocC(sizeof(struct PlsrBufList));
        l->next = p;
        lastRead = fread(l->buf, 1, BUF_SZ, f);
    } while (lastRead == BUF_SZ);
    cur = fullBufs * BUF_SZ;
    len = cur + lastRead;
    res = (char*)pLsrAllocC(len+1);
    res[len] = '\0';
    while(l) {
        struct PlsrBufList* n = l->next;
        strncpy(res+cur, l->buf, lastRead);
        pLsrFreeC(l);
        l = n;
        cur -= BUF_SZ;
        lastRead = BUF_SZ;
    }
    return res;
}

static uintp pLsrOpenOut(char* fn)
{
    FILE* f = fopen(fn, "wb");
    assert(f);
    return (uintp)f;
}

static void pLsrOutputByte(uintp fi, char b)
{
    FILE* f = (FILE*)fi;
    fwrite(&b, 1, 1, f);
}

static void pLsrCloseOut(uintp fi)
{
    FILE* f = (FILE*)fi;
    fclose(f);
}

static uintp pLsrOpenIn(char* fn)
{
    FILE* f = fopen(fn, "r");
    assert(f);
    return (uintp)f;
}

static uintp pLsrInputByte(uintp fi)
{
    FILE* f = (FILE*)fi;
    int c = fgetc(f);
    return (uintp)c;
}

static int isDelimiter (char c, char *delimiters) 
{
    int i = 0;
    for (; delimiters[i]; i++) {
      if (c == delimiters[i]) {
        return 1;
      }  
    }
    return 0;
}

static char* pLsrInputString(uintp fi, char *delimiters)
{
    // TODO WL: add max_len argument to the function
    char* res = pLsrAllocC(256);
    FILE* f = (FILE *)fi;
    assert(f);

    char c; 
    // filter out all spaces and returns
    while (!feof(f)) {
      c = fgetc(f);
      if (c == 0x0a || c == 0x0d || isDelimiter(c, delimiters))
        continue;
      else
        break;
    }

    int i = 0;
    while (!feof(f)) {
      if (c == 0x0a || c == 0x0d || isDelimiter(c, delimiters)) {
        res[i] = 0;
        return res;
      } else {
        res[i++] = c;
      } 
      assert(i<256);
      c = fgetc(f);
    }

    res[i] = 0;
    return res;
}

static char* pLsrInputAll(uintp fi)
{
    struct PlsrBufList* l = 0;
    size_t lastRead;
    uintp fullBufs = -1, len, cur;
    char* res;
    FILE* f = (FILE *)fi;
    assert(f);
    do {
        struct PlsrBufList* p = l;
        fullBufs++;
        l = (struct PlsrBufList*)pLsrAllocC(sizeof(struct PlsrBufList));
        l->next = p;
        lastRead = fread(l->buf, 1, BUF_SZ, f);
    } while (lastRead == BUF_SZ);
    cur = fullBufs * BUF_SZ;
    len = cur + lastRead;
    res = (char*)pLsrAllocC(len+1);
    res[len] = '\0';
    while(l) {
        struct PlsrBufList* n = l->next;
        strncpy(res+cur, l->buf, lastRead);
        pLsrFreeC(l);
        l = n;
        cur -= BUF_SZ;
        lastRead = BUF_SZ;
    }
    return res;
}

static uintp pLsrIsEOF(uintp fi)
{
    FILE* f = (FILE*)fi;
    return (uintp)feof(f);
}

static void pLsrCloseIn(uintp fi)
{
    FILE* f = (FILE*)fi;
    fclose(f);
}

static char* pLsrInputLine(uintp fi)
{
    struct PlsrBufList* l = 0;
    FILE* f = (FILE*)fi;
    char* res;
    uintp fullBufs = -1, len, cur, lastAmt, cont;
    assert(f);
    do {
        struct PlsrBufList* p = l;
        char* lastRead;
        fullBufs++;
        l = (struct PlsrBufList*)pLsrAllocC(sizeof(struct PlsrBufList));
        l->next = p;
        lastRead = fgets(l->buf, BUF_SZ, f);
        assert(lastRead);
        lastAmt = strlen(l->buf);
        cont = l->buf[lastAmt-1]!='\n';
    } while (cont);
    len = fullBufs * (BUF_SZ-1) + lastAmt;
    res = (char*)pLsrAllocC(len+1);
    cur = len;
    while(l) {
        struct PlsrBufList* n = l->next;
        lastAmt = strlen(l->buf);
        cur -= lastAmt;
        strncpy(res+cur, l->buf, lastAmt);
        pLsrFreeC(l);
        l = n;
    }
    assert(cur==0);
    return res;
}

/* The Natives */

static PlsrRational pLsrPOpenOut(PlsrPAny a)
{
    char* filename = pLsrPStringToCString(a);
    uintp f = pLsrOpenOut(filename);
    pLsrFreeC(filename);
    return pLsrRationalFromUIntp(f);
}

static PlsrRational pLsrPGetStdout()
{
  return pLsrRationalFromUIntp((uintp)stdout);
}

static void pLsrPOutputByte(PlsrRational a, PlsrRational b)
{
    uintp ai = pLsrUIntpFromRational(a);
    uintp bi = pLsrUIntpFromRational(b);
    pLsrOutputByte(ai, (char)bi);
}

static void pLsrPCloseOut(PlsrRational a)
{
    uintp ai = pLsrUIntpFromRational(a);
    pLsrCloseOut(ai);
}

static PlsrRational pLsrPOpenIn(PlsrPAny a)
{
    char* filename = pLsrPStringToCString(a);
    uintp f = pLsrOpenIn(filename);
    pLsrFreeC(filename);
    return pLsrRationalFromUIntp(f);
}

static PlsrRational pLsrPGetStdin()
{
  return pLsrRationalFromUIntp((uintp)stdin);
}

static PlsrRational pLsrPInputByte(PlsrRational a)
{
    uintp f = pLsrUIntpFromRational(a);
    uintp c = pLsrInputByte(f);
    return pLsrRationalFromUIntp(c);
}

static PlsrPAny pLsrPInputStringT(PlsrRational a, PlsrPAny b)
{
    uintp f = pLsrUIntpFromRational(a);
    char *delimiters = pLsrPStringToCString(b);
    char *s = pLsrInputString(f, delimiters);
    pLsrFreeC(delimiters);
    PlsrPAny res = pLsrCStringToPStringT(s);
    pLsrFreeC(s);
    return res;
}

static PlsrPAny pLsrPInputStringD(PlsrRational a, PlsrPAny b)
{
    uintp f = pLsrUIntpFromRational(a);
    char *delimiters = pLsrPStringToCString(b);
    char *s = pLsrInputString(f, delimiters);
    pLsrFreeC(delimiters);
    PlsrPAny res = pLsrCStringToPStringD(s);
    pLsrFreeC(s);
    return res;
}

static PlsrPAny pLsrPInputAllT(PlsrRational a)
{
    uintp f = pLsrUIntpFromRational(a);
    char *s = pLsrInputAll(f);
    PlsrPAny res = pLsrCStringToPStringT(s);
    pLsrFreeC(s);
    return res;
}

static PlsrPAny pLsrPInputAllD(PlsrRational a)
{
    uintp f = pLsrUIntpFromRational(a);
    char *s = pLsrInputAll(f);
    PlsrPAny res = pLsrCStringToPStringD(s);
    pLsrFreeC(s);
    return res;
}

static PlsrPBool pLsrPIsEOF(PlsrRational a)
{
    uintp f = pLsrUIntpFromRational(a);
    uintp c = pLsrIsEOF(f);
    return toPlsrPBool(c);
}

static void pLsrPCloseIn(PlsrRational a)
{
    uintp f = pLsrUIntpFromRational(a);
    pLsrCloseIn(f);
}

static void pLsrPWriteln(PlsrPAny a)
{
    pLsrValuePrint(a);
    printf("\n");
}

static PlsrPAny pLsrPReadlnT()
{
    char* s = pLsrInputLine((uintp)stdin);
    PlsrPAny res = pLsrCStringToPStringT(s);
    pLsrFreeC(s);
    return res;
}

static PlsrPAny pLsrPReadlnD()
{
    char* s = pLsrInputLine((uintp)stdin);
    PlsrPAny res = pLsrCStringToPStringD(s);
    pLsrFreeC(s);
    return res;
}

static PlsrPAny pLsrPError(PlsrPAny a)
{
    printf("Fatal P error: ");
    pLsrValuePrint(a);
    printf("\nExiting...\n");
    pLsrExit(-1);
    return 0;
}

static PlsrPAny pLsrPAssert(PlsrPAny a)
{
    pLsrRuntimeError("PAssert unimplemented");
    return 0;
}

static void pLsrPDebug(PlsrPAny a)
{
    printf("P Debug: ");
    pLsrValuePrint(a);
    printf("\n");
}

/**********************************************************************
 * Conversions
 */

static uintp pLsrString2Nat(char *str)
{
  return (uintp)atoi(str);
}

static PlsrRational pLsrPString2Nat(PlsrPAny a)
{
    char* str = pLsrPStringToCString(a);
    uintp i = pLsrString2Nat (str);
    pLsrFreeC(str);
    return pLsrRationalFromUIntp(i);
}

static float pLsrString2Float(char *str)
{
  return atof(str);
}

static float pLsrPString2Float(PlsrPAny a)
{
    char* str = pLsrPStringToCString(a);
    float f = pLsrString2Float (str);
    pLsrFreeC(str);
    return f;
}


static PlsrPAny pLsrPFloat2StringT(float f, PlsrRational p)
{
    uintp pi = pLsrUIntpFromRational(p);
    char str[100];
    sprintf(str, "%.*f", pi, f);
    return pLsrCStringToPStringT(str);
}                

static PlsrPAny pLsrPFloat2StringD(float f, PlsrRational p)
{
    uintp pi = pLsrUIntpFromRational(p);
    char str[100];
    sprintf(str, "%.*f", pi, f);
    return pLsrCStringToPStringD(str);
}

/**********************************************************************
 * Domains
 */

/* XXX NG: note that we abuse sets here.  Dom should return a finite set
 * of the indices of the array, and nub should return its size.  Instead
 * dom returns an option set with a rat in that is the length of the array
 * and nub just returns this rat.
 */

static PlsrRational pLsrPNub(PlsrPAny a)
{
    assert(pLsrVTableGetTag(pLsrObjectGetVTable(a)) == VSetTag);
    return (PlsrRational)pLsrPSetGet(a);
}

static PlsrPAny pLsrPDom(PlsrPAny a)
{
    if (pLsrVTableGetTag(pLsrObjectGetVTable(a)) == VArrayTag) {
        return
            pLsrPSetNew
            ((PlsrObjectB)pLsrRationalFromUIntp(pLsrPArrayOGetLen(a)));
    } else {
        pLsrRuntimeError("PDom unimplemented for nonarrays");
        return 0;
    }
}

/**********************************************************************
 * Timers 
 */

/* XXX: WL: WIN32 implementation, not work on Linux */
uint64 t0[16];

static uint64 pLsrGetCurrentTime()
{
    FILETIME ft;
    uint32 lowTime;
    uint32 highTime;
    uint64 time;

    GetSystemTimeAsFileTime(&ft);
    lowTime = ft.dwLowDateTime;
    highTime = ft.dwHighDateTime;

    time = ((uint64)highTime)<<32 | (uint64)lowTime;

    return time;
}

static void pLsrPResetTimer(PlsrRational i)
{
    uintp n = pLsrUIntpFromRational(i);
  
    t0[n] = pLsrGetCurrentTime();
    // printf("init Timer(%d) at %llu\n", n, t0[n]);
}

static float pLsrPGetTimer(PlsrRational i)
{
    uint64 t1;
    float delta;
    uintp n = pLsrUIntpFromRational(i);
    t1 = pLsrGetCurrentTime();
    // printf("get Timer(%d) at %llu\n", n, t1);

    delta = (t1-t0[n])/10000000.0; //interval is 100 ns
    return delta;
}

/**********************************************************************
 * Command Line support
 */
uintp pargc;
const char **pargv;

static PlsrPAny pLsrPCommandLineT()
{
    PlsrPAny res = pLsrPArrayONew(pargc);
    for (uintp i = 0; i < pargc; i++) {
        PlsrPAny ps = pLsrCStringToPStringT(pargv[i]);
        PlsrThunkBRef pst = pLsrThunkNewValRef(ps);
        pLsrWriteBarrierRefBase(res, pLsrPArrayOElt(res, i), (PlsrObjectB)pst);
    }
    return res;
}

static PlsrPAny pLsrPCommandLineD()
{
    PlsrPAny res = pLsrPArrayONew (pargc);
    for (uintp i = 0; i < pargc; i++) {
        PlsrPAny ps = pLsrCStringToPStringD(pargv[i]);
        pLsrWriteBarrierRefBase(res, pLsrPArrayOElt(res, i), ps);
    }
    return res;
}

/**********************************************************************
 * VTune primitives
 */

static void pLsrPVTuneAttach()
{
#ifdef P_USE_PILLAR
    prtVtuneResume(); 
#endif
}

static void pLsrPVTuneDetach()
{
#ifdef P_USE_PILLAR
    prtVtunePause(); 
#endif
}

#endif /* !_PLSR_NATIVES_H_ */
