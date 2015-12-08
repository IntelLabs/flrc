/* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

#ifndef _PLSR_PRIMS_RUNTIME_H_
#define _PLSR_PRIMS_RUNTIME_H_

#ifdef P_USE_PILLAR
#  pragma pillar_managed(off)
#  define to __to__
#endif /* P_USE_PILLAR */

#ifdef PLSR_LINUX
#include <sys/time.h>
#endif

#ifdef P_USE_PILLAR
#  undef to
#  pragma pillar_managed(on)
#endif

/**********************************************************************
 * IO
 */

/* IO Hacks */

#define BUF_SZ (1<<16)

struct PlsrBufList {
    char buf[BUF_SZ];
    struct PlsrBufList* next;
};

static char* pLsrReadLine()
{
    struct PlsrBufList* l = 0;
    char* res;
    uintp fullBufs = -1, len, cur, lastAmt, cont;
    do {
        struct PlsrBufList* p = l;
        char* lastRead;
        fullBufs++;
        l = (struct PlsrBufList*)pLsrAllocC(sizeof(struct PlsrBufList));
        l->next = p;
        lastRead = fgets(l->buf, BUF_SZ, stdin);
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

static char* pLsrReadFile(char* fn)
{
    FILE* f = fopen(fn, "r");
    struct PlsrBufList* l = 0;
    size_t lastRead;
    uintp fullBufs = -1, len, cur;
    char* res;
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
    fclose(f);
    assert(cur==0);
    return res;
}

static void pLsrWriteFile(char* fn, uintp n, char* s)
{
    FILE* f = fopen(fn, "wb");
    assert(f);
    fwrite(s, 1, n, f);
    fclose(f);
}
    
static char* pLsrPrimRReadAllStdin()
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

static uintp pLsrPrimNativeOpenOut(char* fn)
{
    FILE* f = fopen(fn, "wb");
    assert(f);
    return (uintp)f;
}

static void pLsrPrimNativeOutputByte(uintp fi, char b)
{
    FILE* f = (FILE*)fi;
    fwrite(&b, 1, 1, f);
}

static void pLsrPrimNativeCloseOut(uintp fi)
{
    FILE* f = (FILE*)fi;
    fclose(f);
}

static uintp pLsrPrimNativeOpenIn(char* fn)
{
    FILE* f = fopen(fn, "r");
    assert(f);
    return (uintp)f;
}

static uintp pLsrPrimNativeInputByte(uintp fi)
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

static char* pLsrPrimNativeInputString(uintp fi, char *delimiters)
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

static char* pLsrPrimNativeInputAll(uintp fi)
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

static uintp pLsrPrimNativeIsEOF(uintp fi)
{
    FILE* f = (FILE*)fi;
    return (uintp)feof(f);
}

static void pLsrPrimNativeCloseIn(uintp fi)
{
    FILE* f = (FILE*)fi;
    fclose(f);
}

static char* pLsrPrimNativeInputLine(uintp fi)
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

/* The natives */

static PlsrRational pLsrPrimROpenOut(PlsrPAny a)
{
    char* filename = pLsrPStringToCString(a);
    uintp f = pLsrPrimNativeOpenOut(filename);
    PlsrRational r;
    pLsrFreeC(filename);
    pLsrRationalFromUIntp(r, f);
    return r;
}

#define pLsrPrimROpenOutT pLsrPrimROpenOut

static PlsrRational pLsrPrimRGetStdout()
{
    PlsrRational r;
    pLsrRationalFromUIntp(r, (uintp)stdout);
    return r;
}

#define pLsrPrimRGetStdoutT pLsrPrimRGetStdout

static void pLsrPrimROutputByte(PlsrRational a, PlsrRational b)
{
    uintp ai;
    uintp bi;

    pLsrUIntpFromRational(ai, a);
    pLsrUIntpFromRational(bi, b);
    pLsrPrimNativeOutputByte(ai, (char)bi);
}

#define pLsrPrimROutputByteT pLsrPrimROutputByte

static void pLsrPrimRCloseOut(PlsrRational a)
{
    uintp ai;
    pLsrUIntpFromRational(ai, a);
    pLsrPrimNativeCloseOut(ai);
}

#define pLsrPrimRCloseOutT pLsrPrimRCloseOut

static PlsrRational pLsrPrimROpenIn(PlsrPAny a)
{
    char* filename = pLsrPStringToCString(a);
    uintp f = pLsrPrimNativeOpenIn(filename);
    PlsrRational r;
    pLsrFreeC(filename);
    pLsrRationalFromUIntp(r, f);
    return r;
}

#define pLsrPrimROpenInT pLsrPrimROpenIn

static PlsrRational pLsrPrimRGetStdin()
{
    PlsrRational r;
    pLsrRationalFromUIntp(r, (uintp)stdin);
    return r;
}

#define pLsrPrimRGetStdinT pLsrPrimRGetStdin

static PlsrRational pLsrPrimRInputByte(PlsrRational a)
{
    uintp f;
    uintp c;
    PlsrRational r;
    pLsrUIntpFromRational(f, a);
    c = pLsrPrimNativeInputByte(f);
    pLsrRationalFromUIntp(r, c);
    return r;
}

#define pLsrPrimRInputByteT pLsrPrimRInputByte

static PlsrPAny pLsrPrimRInputString(PlsrRational a, PlsrPAny b)
{
    uintp f;
    pLsrUIntpFromRational(f, a);
    char *delimiters = pLsrPStringToCString(b);
    char *s = pLsrPrimNativeInputString(f, delimiters);
    pLsrFreeC(delimiters);
    PlsrPAny res = pLsrCStringToPString(s);
    pLsrFreeC(s);
    return res;
}

static PlsrPAny pLsrPrimRInputStringT(PlsrRational a, PlsrPAny b)
{
    uintp f;
    pLsrUIntpFromRational(f, a);
    char *delimiters = pLsrPStringToCString(b);
    char *s = pLsrPrimNativeInputString(f, delimiters);
    pLsrFreeC(delimiters);
    PlsrPAny res = pLsrCStringToPStringT(s);
    pLsrFreeC(s);
    return res;
}

static PlsrPAny pLsrPrimRInputAll(PlsrRational a)
{
    uintp f;
    pLsrUIntpFromRational(f, a);
    char *s = pLsrPrimNativeInputAll(f);
    PlsrPAny res = pLsrCStringToPString(s);
    pLsrFreeC(s);
    return res;
}

static PlsrPAny pLsrPrimRInputAllT(PlsrRational a)
{
    uintp f;
    pLsrUIntpFromRational(f, a);
    char *s = pLsrPrimNativeInputAll(f);
    PlsrPAny res = pLsrCStringToPStringT(s);
    pLsrFreeC(s);
    return res;
}

static PlsrBoolean pLsrPrimRIsEOF(PlsrRational a)
{
    uintp f;
    pLsrUIntpFromRational(f, a);
    uintp c = pLsrPrimNativeIsEOF(f);
    return toPlsrBoolean(c);
}

#define pLsrPrimRIsEOFT pLsrPrimRIsEOF

static void pLsrPrimRCloseIn(PlsrRational a)
{
    uintp f;
    pLsrUIntpFromRational(f, a);
    pLsrPrimNativeCloseIn(f);
}

#define pLsrPrimRCloseInT pLsrPrimRCloseIn

/**********************************************************************
 * Core Natives
 */

static float pLsrPrimRFloatMk(PlsrRational a, PlsrRational b)
{
    sint32 m;
    sint32 e;
    double r, p;
    pLsrSInt32FromRational(m, a);
    pLsrSInt32FromRational(e, b);
    r = m;
    p = 10;
    if (e<0) e=-e, p=0.1;
    while (e-->0) r*=p;
    return r;
}

#define pLsrPrimRFloatMkT pLsrPrimRFloatMk

static void pLsrPrimRWriteln(PlsrPAny a)
{
    pLsrValuePrint(a);
    printf("\n");
}

#define pLsrPrimRWritelnT pLsrPrimRWriteln

static PlsrPAny pLsrPrimRReadln()
{
    char* s = pLsrPrimNativeInputLine((uintp)stdin);
    PlsrPAny res = pLsrCStringToPString(s);
    pLsrFreeC(s);
    return res;
}

static PlsrPAny pLsrPrimRReadlnT()
{
    char* s = pLsrPrimNativeInputLine((uintp)stdin);
    PlsrPAny res = pLsrCStringToPStringT(s);
    pLsrFreeC(s);
    return res;
}

static PlsrPAny pLsrPrimRAssert(PlsrPAny a)
{
    pLsrRuntimeError("PAssert unimplemented");
    return 0;
}

#define pLsrPrimRAssertT pLsrPrimRAssert

static PlsrPAny pLsrPrimRError(PlsrPAny a)
{
    printf("Fatal P error: ");
    pLsrValuePrint(a);
    printf("\nExiting...\n");
    pLsrExit(-1);
    return 0;
}

#define pLsrPrimRErrorT pLsrPrimRError

static void pLsrPrimRDebug(PlsrPAny a)
{
    printf("P Debug: ");
    pLsrValuePrint(a);
    printf("\n");
}

#define pLsrPrimRDebugT pLsrPrimRDebug

/**********************************************************************
 * Command Line support
 */

uintp pargc;
const char **pargv;

static PlsrPAny pLsrPrimRCommandLineT()
{
    PlsrPAny res = pLsrPArrayONew(pargc);
    for (uintp i = 0; i < pargc; i++) {
        PlsrPAny ps = pLsrCStringToPStringT(pargv[i]);
        PlsrThunkBRef pst = pLsrThunkNewValRef((PlsrRef) ps);
        pLsrWriteBarrierRefBase(res, pLsrPArrayOElt(res, i), (PlsrObjectB)pst);
    }
    return res;
}

static PlsrPAny pLsrPrimRCommandLine()
{
    PlsrPAny res = pLsrPArrayONew (pargc);
    for (uintp i = 0; i < pargc; i++) {
        PlsrPAny ps = pLsrCStringToPString(pargv[i]);
        pLsrWriteBarrierRefBase(res, pLsrPArrayOElt(res, i), ps);
    }
    return res;
}

#define pLsrCommandLineCount() (pargc)
#define pLsrCommandLineGet(i) ((char*)pargv[i])

/**********************************************************************
 * Conversions
 */

static uintp pLsrString2Nat(char *str)
{
  return (uintp)atoi(str);
}

static PlsrRational pLsrStringToRat(char *str)
{
    return pLsrRationalFromCString(str);
}

static float pLsrStringToFloat(char *str)
{
  return atof(str);
}


static PlsrRational pLsrPrimRStringToNat(PlsrPAny a)
{
    char* str = pLsrPStringToCString(a);
    uintp i = pLsrString2Nat (str);
    pLsrFreeC(str);
    PlsrRational r;
    pLsrRationalFromUIntp(r, i);
    return r;
}

#define pLsrPrimRStringToNatT pLsrPrimRStringToNat

static PlsrRational pLsrPrimRStringToRat(PlsrPAny a)
{
    char* str = pLsrPStringToCString(a);
    PlsrRational r = pLsrStringToRat (str);
    pLsrFreeC(str);
    return r;
}

#define pLsrPrimRStringToRatT pLsrPrimRStringToRat

static PlsrPAny pLsrPrimRRatToString(PlsrRational r)
{
    char* s = pLsrCStringFromRational(r);
    PlsrPAny o = pLsrCStringToPString(s);
    pLsrFreeC(s);
    return o;
}                

static PlsrPAny pLsrPrimRRatToStringT(PlsrRational r)
{
    char* s = pLsrCStringFromRational(r);
    PlsrPAny o = pLsrCStringToPStringT(s);
    pLsrFreeC(s);
    return o;
}                

static float pLsrPrimRStringToFloat(PlsrPAny a)
{
    char* str = pLsrPStringToCString(a);
    float f = pLsrStringToFloat (str);
    pLsrFreeC(str);
    return f;
}

#define pLsrPrimRStringToFloatT pLsrPrimRStringToFloat

static PlsrPAny pLsrPrimRFloatToString(float f, PlsrRational p)
{
    uintp pi;
    pLsrUIntpFromRational(pi, p);
    char str[100];
    sprintf(str, "%.*f", pi, f);
    return pLsrCStringToPString(str);
}

static PlsrPAny pLsrPrimRFloatToStringT(float f, PlsrRational p)
{
    uintp pi;
    pLsrUIntpFromRational(pi, p);
    char str[100];
    sprintf(str, "%.*f", pi, f);
    return pLsrCStringToPStringT(str);
}                

/* These are for the rats as unsafe integers hack */
static PlsrPAny pLsrPrimRFloat2StringI(float f, sintp p)
{
    uintp pi = (uintp) p;
    char str[100];
    sprintf(str, "%.*f", pi, f);
    return pLsrCStringToPString(str);
}

static PlsrPAny pLsrPrimRFloat2StringIT(float f, sintp p)
{
    uintp pi = (uintp) p;
    char str[100];
    sprintf(str, "%.*f", pi, f);
    return pLsrCStringToPStringT(str);
}                

#define pLsrPrimRRatNumerator pLsrRatNumerator
#define pLsrPrimRRatNumeratorT pLsrRatNumeratorT

#define pLsrPrimRRatDenominator pLsrRatDenominator
#define pLsrPrimRRatDenominatorT pLsrRatDenominatorT


/**********************************************************************
 * Equality and unification
 */

static PlsrBoolean pLsrPrimREqualSlow(PlsrPAny v1, PlsrPAny v2)
{
    PlsrVTable vt1;
    PlsrVTable vt2;
    uintp i;
    vt1 = pLsrObjectGetVTable(v1);
    vt2 = pLsrObjectGetVTable(v2);
    if (pLsrVTableGetTag(vt1) != pLsrVTableGetTag(vt2)) return 0;
    switch(pLsrVTableGetTag(vt1)) {
    case VRatTag: {
        PlsrBoolean b;
        PlsrRational r1, r2;
        pLsrRationalFromPRat(r1, v1);
        pLsrRationalFromPRat(r2, v2);
        pLsrRationalEQ(b, r1, r2);
        return b;
    }
    case VNameTag: 
        return pLsrPNameGetTag(v1) == pLsrPNameGetTag(v2);
    case VFloatTag:
        return pLsrPFloatGet(v1) == pLsrPFloatGet(v2);
    case VArrayTag:
        if (pLsrPArrayOGetLen(v1) != pLsrPArrayOGetLen(v2)) return 0;
        for(i=0; i<pLsrPArrayOGetLen(v1); i++) {
            PlsrPAny v1e;
            pLsrPArrayOEltEval(v1e, v1, i);
            PlsrPAny v2e;
            pLsrPArrayOEltEval(v2e, v2, i);
            if (!pLsrPrimREqualSlow(v1e, v2e)) return 0;
        }
        return 1;
    case VSumTag:
        if (pLsrPNameGetTag(pLsrPSumGetTag(v1)) !=
            pLsrPNameGetTag(pLsrPSumGetTag(v2)))
            return 0;
        pLsrPSumGetValEval(v1, v1);
        pLsrPSumGetValEval(v2, v2);
        return pLsrPrimREqualSlow(v1, v2);
    case VFunctionTag:
        pLsrRuntimeError("PEq - can't compare functions for equality");
        break;
    default:
        printf("P equality: %d unimplemented\n", pLsrVTableGetTag(vt1));
        break;
    }
    pLsrRuntimeError("PEq");
    return 0;
}

#define pLsrPrimREqual(v1, v2)                                              \
    (((v1 == v2) && (pLsrVTableGetTag(pLsrObjectGetVTable(v1)) != VFloatTag)) || (pLsrPrimREqualSlow(v1, v2)))


#define pLsrPrimREqualT pLsrPrimREqual


/**********************************************************************
 * Domains
 */

/* XXX NG: note that we abuse sets here.  Dom should return a finite set
 * of the indices of the array, and nub should return its size.  Instead
 * dom returns an option set with a rat in that is the length of the array
 * and nub just returns this rat.
 */

static PlsrRational pLsrPrimRNub(PlsrPAny a)
{
    assert(pLsrVTableGetTag(pLsrObjectGetVTable(a)) == VSetTag);
    return (PlsrRational)pLsrPSetGet(a);
}

#define pLsrPrimRNubT pLsrPrimRNub

static PlsrPAny pLsrPrimRDom(PlsrPAny a)
{
    if (pLsrVTableGetTag(pLsrObjectGetVTable(a)) == VArrayTag) {
        PlsrRational r;
        pLsrRationalFromUIntp(r, pLsrPArrayOGetLen(a));
        return pLsrPSetNew((PlsrObjectB) r);
    } else {
        pLsrRuntimeError("PDom unimplemented for nonarrays");
        return 0;
    }
}

#define pLsrPrimRDomT pLsrPrimRDom

static uintp pLsrPrimRRatToUIntpChecked(PlsrRational a)
{                                                                      
    uintp dest;
    if (sizeof(uint32) != sizeof(uintp)) {                         
        pLsrRuntimeError("PRatToUintpChecked only implemented for uintp = uint32"); 
    } else {
        pLsrRationalToUInt32Checked(dest, a);
    }   
    return dest;
}
#define pLsrPrimRRatToUIntpCheckedT pLsrPrimRRatToUIntpChecked

/**********************************************************************
 * Timers 
 */

/* XXX: WL: WIN32 implementation, not work on Linux */
uint64 t0[16];

static uint64 pLsrGetCurrentTime()
{
#ifdef PLSR_LINUX
    struct timeval tv;
    gettimeofday(&tv,NULL);
    uint64 time;
    time = (((uint64)tv.tv_sec) * 1000000) + ((uint64)tv.tv_usec);
    time *= 10; // convert from micro-seconds to 100-nanosecond intervals    
#else // PLSR_LINUX
    FILETIME ft;
    uint32 lowTime;
    uint32 highTime;
    uint64 time;

    GetSystemTimeAsFileTime(&ft);
    lowTime = ft.dwLowDateTime;
    highTime = ft.dwHighDateTime;

    time = ((uint64)highTime)<<32 | (uint64)lowTime;
#endif // PLSR_LINUX
    return time;
}

static void pLsrPrimRResetTimer(PlsrRational i)
{
    uintp n;
    pLsrUIntpFromRational(n, i);
  
    t0[n] = pLsrGetCurrentTime();
    // printf("init Timer(%d) at %llu\n", n, t0[n]);
}

#define pLsrPrimRResetTimerT pLsrPrimRResetTimer

static float pLsrPrimRGetTimer(PlsrRational i)
{
    uint64 t1;
    float delta;
    uintp n;
    pLsrUIntpFromRational(n, i);
    t1 = pLsrGetCurrentTime();
    // printf("get Timer(%d) at %llu\n", n, t1);

    delta = (t1-t0[n])/10000000.0; //interval is 100 ns
    return delta;
}

#define pLsrPrimRGetTimerT pLsrPrimRGetTimer

/**********************************************************************
 * VTune primitives
 */

static void pLsrPrimRVTuneAttach()
{
#ifdef P_USE_PILLAR
    prtVtuneResume(); 
#endif
}

#define pLsrPrimRVTuneAttachT pLsrPrimRVTuneAttach

static void pLsrPrimRVTuneDetach()
{
#ifdef P_USE_PILLAR
    prtVtunePause(); 
#endif
}

#define pLsrPrimRVTuneDetachT pLsrPrimRVTuneDetach

/* XXX NG: deprecate */
static void pLsrPrimRArrayEval(PlsrPAny v, uintp ordered)
{
    switch(pLsrVTableGetTag(pLsrObjectGetVTable(v))) {
    case VArrayTag: {
        uintp i;
        for(i=0; i<pLsrPArrayOGetLen(v); i++)
            pLsrThunkEvalRef((PlsrThunkBRef)pLsrPArrayOElt(v, i));
        break;
    }
    case VArrayIdxTag: {
        uintp i;
        uintp len = pLsrPArrayIGetLen(v);
        for(i=0; i < len; i++)
            pLsrThunkEvalRef((PlsrThunkBRef)pLsrPArrayIElt(v, i));
        break;
    }
    default:
        pLsrRuntimeError("arrayEval of non-array");
    }
}

#define pLsrPrimRArrayEvalT pLsrPrimRArrayEval

/**********************************************************************
 * Integer Hash
 */

#define pLsrPrimRIntHash(i) pLsrIntegerHash(i)
#define pLsrPrimRIntHashT pLsrPrimRIntHash

#endif /* !_PLSR_PRIMS_RUNTIME_H_ */
