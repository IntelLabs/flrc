/* The Intel P to C/Pillar Compiler */
/* Copyright (C) Intel Corporation, January 2007 */

/* P Natives */

#ifndef _PLSR_NATIVES_H_
#define _PLSR_NATIVES_H_

/**********************************************************************
 * Arithmetic Primitives
 */

#define pLsrPIntMkDiv(Kind, div)                                        \
    static PlsrRational pLsrPIntDiv##Kind(PlsrRational a, PlsrRational b) \
    {                                                                   \
        PlsrInteger ai;                                                 \
        PlsrInteger bi;                                                 \
        PlsrInteger q;                                                  \
        PlsrRational r;                                                 \
                                                                        \
        pLsrPrimIntegerFromRational(ai, a);                             \
        pLsrPrimIntegerFromRational(bi, b);                             \
        div(q, ai, bi);                                                 \
        pLsrPrimRationalFromInteger(r, q);                              \
        return r;                                                       \
    }                                                                   

pLsrPIntMkDiv(T, pLsrPrimIntegerDivT);
pLsrPIntMkDiv(F, pLsrPrimIntegerDivF);
pLsrPIntMkDiv(E, pLsrPrimIntegerDivE);

#define pLsrPIntMkMod(Kind, mod)                                        \
    static PlsrRational pLsrPIntMod##Kind(PlsrRational a, PlsrRational b) \
    {                                                                   \
        PlsrInteger ai;                                                 \
        PlsrInteger bi;                                                 \
        PlsrInteger rem;                                                \
        PlsrRational r;                                                 \
                                                                        \
        pLsrPrimIntegerFromRational(ai, a);                             \
        pLsrPrimIntegerFromRational(bi, b);                             \
        mod(rem, ai, bi);                                                \
        pLsrPrimRationalFromInteger(r, rem);                            \
        return r;                                                       \
    }                                                                   

pLsrPIntMkMod(T, pLsrPrimIntegerModT);
pLsrPIntMkMod(F, pLsrPrimIntegerModF);
pLsrPIntMkMod(E, pLsrPrimIntegerModE);

#define pLsrPIntMkDivMod(Kind, TorD, divmod)                            \
    static PlsrPAny pLsrPIntDivMod##Kind##TorD(PlsrRational a, PlsrRational b) \
    {                                                                   \
        PlsrInteger ai = NULL;                                          \
        PlsrInteger bi = NULL;                                          \
        pLsrPrimIntegerFromRational(ai, a);                             \
        pLsrPrimIntegerFromRational(bi, b);                             \
                                                                        \
        PlsrInteger q = NULL;                                           \
        PlsrInteger r = NULL;                                           \
        divmod(q, r, ai, bi);                                           \
                                                                        \
        PlsrPAny qr = NULL;                                             \
        PlsrPAny rr = NULL;                                             \
        pLsrPRatFromInteger(qr, q);                                     \
        pLsrPRatFromInteger(rr, r);                                     \
        return pLsrPPair##TorD(qr, rr);                                 \
    }

pLsrPIntMkDivMod(T, T, pLsrPrimIntegerDivModT);
pLsrPIntMkDivMod(F, T, pLsrPrimIntegerDivModF);
pLsrPIntMkDivMod(E, T, pLsrPrimIntegerDivModE);
pLsrPIntMkDivMod(T, D, pLsrPrimIntegerDivModT);
pLsrPIntMkDivMod(F, D, pLsrPrimIntegerDivModF);
pLsrPIntMkDivMod(E, D, pLsrPrimIntegerDivModE);

/**********************************************************************
 * Pointers
 */

static PlsrPAny pLsrPPtrNewHelperSlow(PlsrPAny a)
{
    pLsrRuntimeError("PPtrNewHelper unimplemented");
    return 0;
}

static PlsrPAny pLsrPPtrNewSlow(PlsrPAny a)
{
    pLsrRuntimeError("PPtrNew unimplemented");
    return 0;
}

static PlsrPAny pLsrPPtrWriteSlow(PlsrPAny a, PlsrPAny b)
{
    pLsrRuntimeError("PPtrWrite unimplemented");
    return 0;
}

static PlsrPAny pLsrPPtrReadSlow(PlsrPAny a)
{
    pLsrRuntimeError("PPtrRead unimplemented");
    return 0;
}

static PlsrPAny pLsrPPtrTypeSlow(PlsrPAny a)
{
    pLsrRuntimeError("PPtrType unimplemented");
    return 0;
}

#define pLsrPPtrNewHelper(dest, a) ((dest) = pLsrPPtrNewHelperSlow(a))
#define pLsrPPtrNew(dest, a) ((dest) = pLsrPPtrNewSlow(a))
#define pLsrPPtrWrite(dest, a, b) ((dest) = pLsrPPtrWriteSlow(a, b))
#define pLsrPPtrRead(dest, a) ((dest) = pLsrPPtrReadSlow(a))
#define pLsrPPtrType(dest, a) ((dest) = pLsrPPtrTypeSlow(a))

/**********************************************************************
 * CString
 */

static char* pLsrCStringAllocate(uintp len)
{
    char* res = (char*) pLsrAllocC(len+1);
    res[len] = '\0';
    return res;
}

static void pLsrCStringDeallocate(char* str)
{
    pLsrFreeC(str);
}

static uintp pLsrCStringGetLen(char* str)
{
    return strlen(str);
}

static char pLsrCStringGetChar(char* str, uintp idx)
{
    return str[idx];
}

static void pLsrCStringSetChar(char* str, uintp idx, char c)
{
    str[idx] = c;
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
    PlsrRational r;
    pLsrFreeC(filename);
    pLsrPrimRationalFromUIntp(r, f);
    return r;
}

static PlsrRational pLsrPGetStdout()
{
    PlsrRational r;
    pLsrPrimRationalFromUIntp(r, (uintp)stdout);
    return r;
}

static void pLsrPOutputByte(PlsrRational a, PlsrRational b)
{
    uintp ai;
    uintp bi;

    pLsrPrimUIntpFromRational(ai, a);
    pLsrPrimUIntpFromRational(bi, b);
    pLsrOutputByte(ai, (char)bi);
}

static void pLsrPCloseOut(PlsrRational a)
{
    uintp ai;
    pLsrPrimUIntpFromRational(ai, a);
    pLsrCloseOut(ai);
}

static PlsrRational pLsrPOpenIn(PlsrPAny a)
{
    char* filename = pLsrPStringToCString(a);
    uintp f = pLsrOpenIn(filename);
    PlsrRational r;
    pLsrFreeC(filename);
    pLsrPrimRationalFromUIntp(r, f);
    return r;
}

static PlsrRational pLsrPGetStdin()
{
    PlsrRational r;
    pLsrPrimRationalFromUIntp(r, (uintp)stdin);
    return r;
}

static PlsrRational pLsrPInputByte(PlsrRational a)
{
    uintp f;
    uintp c;
    PlsrRational r;
    pLsrPrimUIntpFromRational(f, a);
    c = pLsrInputByte(f);
    pLsrPrimRationalFromUIntp(r, c);
    return r;
}

static PlsrPAny pLsrPInputStringT(PlsrRational a, PlsrPAny b)
{
    uintp f;
    pLsrPrimUIntpFromRational(f, a);
    char *delimiters = pLsrPStringToCString(b);
    char *s = pLsrInputString(f, delimiters);
    pLsrFreeC(delimiters);
    PlsrPAny res = pLsrCStringToPStringT(s);
    pLsrFreeC(s);
    return res;
}

static PlsrPAny pLsrPInputStringD(PlsrRational a, PlsrPAny b)
{
    uintp f;
    pLsrPrimUIntpFromRational(f, a);
    char *delimiters = pLsrPStringToCString(b);
    char *s = pLsrInputString(f, delimiters);
    pLsrFreeC(delimiters);
    PlsrPAny res = pLsrCStringToPStringD(s);
    pLsrFreeC(s);
    return res;
}

static PlsrPAny pLsrPInputAllT(PlsrRational a)
{
    uintp f;
    pLsrPrimUIntpFromRational(f, a);
    char *s = pLsrInputAll(f);
    PlsrPAny res = pLsrCStringToPStringT(s);
    pLsrFreeC(s);
    return res;
}

static PlsrPAny pLsrPInputAllD(PlsrRational a)
{
    uintp f;
    pLsrPrimUIntpFromRational(f, a);
    char *s = pLsrInputAll(f);
    PlsrPAny res = pLsrCStringToPStringD(s);
    pLsrFreeC(s);
    return res;
}

static PlsrPBool pLsrPIsEOF(PlsrRational a)
{
    uintp f;
    pLsrPrimUIntpFromRational(f, a);
    uintp c = pLsrIsEOF(f);
    return toPlsrPBool(c);
}

static void pLsrPCloseIn(PlsrRational a)
{
    uintp f;
    pLsrPrimUIntpFromRational(f, a);
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
    PlsrRational r;
    pLsrPrimRationalFromUIntp(r, i);
    return r;
}

static PlsrRational pLsrString2Rat(char *str)
{
    return pLsrRationalFromCString(str);
}

static PlsrRational pLsrPString2Rat(PlsrPAny a)
{
    char* str = pLsrPStringToCString(a);
    PlsrRational r = pLsrString2Rat (str);
    pLsrFreeC(str);
    return r;
}

static PlsrPAny pLsrPRat2StringT(PlsrRational r)
{
    char* s = pLsrCStringFromRational(r);
    PlsrPAny o = pLsrCStringToPStringT(s);
    pLsrFreeC(s);
    return o;
}                

static PlsrPAny pLsrPRat2StringD(PlsrRational r)
{
    char* s = pLsrCStringFromRational(r);
    PlsrPAny o = pLsrCStringToPStringD(s);
    pLsrFreeC(s);
    return o;
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
    uintp pi;
    pLsrPrimUIntpFromRational(pi, p);
    char str[100];
    sprintf(str, "%.*f", pi, f);
    return pLsrCStringToPStringT(str);
}                

static PlsrPAny pLsrPFloat2StringD(float f, PlsrRational p)
{
    uintp pi;
    pLsrPrimUIntpFromRational(pi, p);
    char str[100];
    sprintf(str, "%.*f", pi, f);
    return pLsrCStringToPStringD(str);
}

/* These are for the rats as unsafe integers hack */
static PlsrPAny pLsrPFloat2StringIT(float f, sintp p)
{
    uintp pi = (uintp) p;
    char str[100];
    sprintf(str, "%.*f", pi, f);
    return pLsrCStringToPStringT(str);
}                

static PlsrPAny pLsrPFloat2StringID(float f, sintp p)
{
    uintp pi = (uintp) p;
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

static PlsrRational pLsrPNubSlow(PlsrPAny a)
{
    assert(pLsrVTableGetTag(pLsrObjectGetVTable(a)) == VSetTag);
    return (PlsrRational)pLsrPSetGet(a);
}

#define pLsrPrimPNub(dest, a) ((dest) = pLsrPNubSlow(a))

static PlsrPAny pLsrPDomSlow(PlsrPAny a)
{
    if (pLsrVTableGetTag(pLsrObjectGetVTable(a)) == VArrayTag) {
        PlsrRational r;
        pLsrPrimRationalFromUIntp(r, pLsrPArrayOGetLen(a));
        return pLsrPSetNew((PlsrObjectB) r);
    } else {
        pLsrRuntimeError("PDom unimplemented for nonarrays");
        return 0;
    }
}

#define pLsrPrimPDom(dest, a) ((dest) = pLsrPDomSlow(a))

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
    uintp n;
    pLsrPrimUIntpFromRational(n, i);
  
    t0[n] = pLsrGetCurrentTime();
    // printf("init Timer(%d) at %llu\n", n, t0[n]);
}

static float pLsrPGetTimer(PlsrRational i)
{
    uint64 t1;
    float delta;
    uintp n;
    pLsrPrimUIntpFromRational(n, i);
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
