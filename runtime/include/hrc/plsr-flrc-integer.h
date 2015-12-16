/* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

#ifndef _PLSR_AP_INTEGER_H_
#define _PLSR_AP_INTEGER_H_

typedef uint64 PlsrDPUInt;
typedef uint32 PlsrSPUInt;

static uintp pLsrSPUIntHash(PlsrSPUInt d)
{
    return d+(d<<2);
}

typedef struct {
    PlsrSPUInt fst;
    PlsrSPUInt snd;
} PlsrSPUIntPair;

#define pLsrSPUIntBase (( PlsrDPUInt ) 0x0000000100000000)
#define pLsrSPUIntMax  (( PlsrSPUInt ) 0xFFFFFFFF)
#define pLsrSPBitCount (( PlsrSPUInt ) 32)
#define pLsrDPLowerMask (( PlsrDPUInt ) 0x00000000FFFFFFFF)
#define pLsrDPUpperMask (( PlsrDPUInt ) 0xFFFFFFFF00000000)

#define pLsrDPUIntFromSPUInt(x) ((PlsrDPUInt)(x))
#define pLsrSPUIntFromDPUInt(x) ((PlsrSPUInt)(x))
#define pLsrSPUIntFromDPUIntUpper(x) ((PlsrSPUInt)((x) >> pLsrSPBitCount))

static PlsrSPUIntPair pLsrDPUIntSplit(PlsrDPUInt x) {
    PlsrSPUIntPair y = { .fst = pLsrSPUIntFromDPUInt(x),
                         .snd = pLsrSPUIntFromDPUIntUpper(x)};
    return y;
}
/* x, y, carry are at most 2^32 -1, so
 * x + y + carry are at most 2^33 + 2^32 - 3.
 */
static PlsrSPUIntPair pLsrSPUIntAddWithCarry(PlsrSPUInt x, PlsrSPUInt y, PlsrSPUInt carry) {
    return pLsrDPUIntSplit(pLsrDPUIntFromSPUInt(x) +
                           pLsrDPUIntFromSPUInt(y) +
                           pLsrDPUIntFromSPUInt(carry));
}

/* x, y, carry are at most 2^32 -1, so
 * x * y is at most 2^64 - 2^33 + 2, so
 * x * y + carry is at most 2^64 - 2^33 + 2^32 + 2
 * which is less than 2^64 - 1
 */
static PlsrSPUIntPair pLsrSPUIntMulWithCarry(PlsrSPUInt x, PlsrSPUInt y, PlsrSPUInt carry) {
    return pLsrDPUIntSplit(pLsrDPUIntFromSPUInt(x) *
                           pLsrDPUIntFromSPUInt(y) +
                           pLsrDPUIntFromSPUInt(carry));
}

/* x, y are at most 2^32 - 1, borrow is at most 1
 * y + borrow is at most 2^32
 * if x < y + borrow then
 *   x + base is less than y + borrow  + 2^32
 *   x + base - (y + borrow) is less than 2^32
 * else
 *   x is greater than y and less than 2^32-1, so
 *   x - y is greater than zero and less than 2^32-1
 */
static PlsrSPUIntPair pLsrSPUIntSubWithBorrow(PlsrSPUInt x, PlsrSPUInt y, PlsrSPUInt borrow) {
    PlsrDPUInt x2 = pLsrDPUIntFromSPUInt(x);
    PlsrDPUInt y2 = pLsrDPUIntFromSPUInt(y) + pLsrDPUIntFromSPUInt(borrow);
    PlsrDPUInt b = 0;
    if (x2 < y2) {
        x2 += pLsrSPUIntBase;
        b = 1;
    }
    return (PlsrSPUIntPair) {.fst = x2 - y2, .snd = b};
}


/* -1 => less, 0 => equal, 1 => greater */
static int pLsrSPUIntCompare(PlsrSPUInt x, PlsrSPUInt y) {
    if (x < y) return -1;
    else if (x == y) return 0;
    else return 1;
}


/************* Digit lists ****************/

struct PlsrSPUIntListS_;

#ifdef P_USE_PILLAR
typedef PlsrRef PlsrSPUIntList;
#else /* !P_USE_PILLAR */
typedef struct PlsrSPUIntListS_* PlsrSPUIntList;
#endif /* !P_USE_PILLAR */

/* This structure must always be initialised immediately after allocation
 * without an intervening GC.  If this is not the case, the mutability
 * property of the vtable must be changed.
 */
typedef struct PlsrSPUIntListS_ {
    PlsrVTable vtable;
    PlsrSPUInt digit;
    PlsrSPUIntList next;
} PlsrSPUIntListS;


/* SPUIntLists represent non-negative numbers in base pLsrSPUIntBase.
 * The list contains the list of digits in order from least to
 * most significant, with the obvious interpretation.  A canonical
 * list has a non-zero most significant digit.  All mathematical
 * operations  should be correct for canonical or non-canonical lists,
 * unless otherwise noted.  (I don't think this is necessary).
 * All mathematical operations should return canonical lists
 * if given canonical lists as arguments.
*/

#define pLsrSPUIntListPadding                                           \
    (sizeof(PlsrSPUIntListS) - sizeof(PlsrVTable) - sizeof (PlsrSPUInt) - sizeof (PlsrSPUIntList))
pLsrVTableStatic(pLsrSPUIntListVTable_, "*sp list element*", pLsrSPUIntListPadding);
#define pLsrSPUIntListVTable (&pLsrSPUIntListVTable_)

#define pLsrSPUIntListRep(x) ((PlsrSPUIntListS*)(x))

#define pLsrSPUIntListEmpty NULL

static PlsrSPUIntListS pLsrSPUIntListOne_ =
    { .vtable = pLsrSPUIntListVTable,
      .digit = 1,
      .next = pLsrSPUIntListEmpty};
#define pLsrSPUIntListOne ((PlsrSPUIntList) &pLsrSPUIntListOne_)

static PlsrSPUIntListS pLsrSPUIntListUInt32Max_ =
    { .vtable = pLsrSPUIntListVTable,
      .digit = pLsrSPUIntMax,
      .next = pLsrSPUIntListEmpty};
#define pLsrSPUIntListUInt32Max ((PlsrSPUIntList) &pLsrSPUIntListUInt32Max_)

static PlsrSPUIntList pLsrSPUIntListCons(PlsrSPUInt x, PlsrSPUIntList l) {
    uintp size = sizeof(PlsrSPUIntListS);
    PlsrSPUIntList result;
    noyield {
        pLsrAlloc(PlsrSPUIntList, result, pLsrSPUIntListVTable, size);
        pLsrSPUIntListRep(result)->digit = x;
        pLsrWriteBarrierRefOptBase(result, pLsrSPUIntListRep(result)->next, l);
    }
    return result;
}

static void pLsrSPUIntListSetNext(PlsrSPUIntList l1, PlsrSPUIntList l2) {
    pLsrWriteBarrierRefBase(l1, pLsrSPUIntListRep(l1)->next, l2);
    return;
}

static PlsrSPUIntList pLsrSPUIntListSingleton(PlsrSPUInt x) {
    return pLsrSPUIntListCons (x, pLsrSPUIntListEmpty);
}

static PlsrSPUInt pLsrSPUIntListHd(PlsrSPUIntList l) {
    assert(l);
    return pLsrSPUIntListRep(l)->digit;
}

static PlsrSPUIntList pLsrSPUIntListTl(PlsrSPUIntList l) {
    assert(l);
    return pLsrSPUIntListRep(l)->next;
}

static void pLsrSPUIntListPrintDigits(PlsrSPUIntList l) {
    if (l==pLsrSPUIntListEmpty) {
        printf ("\n");
    } else {
        printf ("%u ", pLsrSPUIntListHd(l));
        pLsrSPUIntListPrintDigits(pLsrSPUIntListTl(l));
    }
}

static PlsrSPUInt pLsrSPUIntListLast(PlsrSPUIntList l) {
    assert(l);
    while (pLsrSPUIntListEmpty != pLsrSPUIntListTl(l)) {
        l = pLsrSPUIntListTl(l);
    };
    return pLsrSPUIntListHd(l);
}


static PlsrSPUIntList pLsrSPUIntListConsIfNonZero(PlsrSPUInt x, PlsrSPUIntList l) {
    if (x == 0) {
        return l;
    } else {
        return pLsrSPUIntListCons (x, l);
    }
}

static PlsrSPUIntList pLsrSPUIntListCanonicalCons(PlsrSPUInt x, PlsrSPUIntList l) {
    if (pLsrSPUIntListEmpty == l) {
        return pLsrSPUIntListConsIfNonZero(x, l);
    } else {
        return pLsrSPUIntListCons(x, l);
    }

}

static PlsrSPUIntList pLsrSPUIntListRevAppend(PlsrSPUIntList l1, PlsrSPUIntList l2) {
    while (pLsrSPUIntListEmpty != l1) {
        l2 = pLsrSPUIntListCons(pLsrSPUIntListHd(l1), l2);
        l1 = pLsrSPUIntListTl(l1);
    }
    return l2;
}

static PlsrSPUIntList pLsrSPUIntListRev(PlsrSPUIntList l1) {
    return pLsrSPUIntListRevAppend(l1, pLsrSPUIntListEmpty);
}

static PlsrSPUIntList pLsrSPUIntListTakeRev(PlsrSPUIntList l, int n) {
    PlsrSPUIntList acc = pLsrSPUIntListEmpty;
    while (n > 0) {
        assert(l);
        acc = pLsrSPUIntListCons(pLsrSPUIntListHd(l), acc);
        l = pLsrSPUIntListTl(l);
        n--;
    }
    return acc;
}

static PlsrSPUIntList pLsrSPUIntListTake(PlsrSPUIntList l, int n) {
    PlsrSPUIntList acc = pLsrSPUIntListTakeRev(l, n);
    return pLsrSPUIntListRev(acc);
}

static PlsrSPUIntList pLsrSPUIntListDrop(PlsrSPUIntList l, int n) {
    while (n > 0) {
        assert(l);
        l = pLsrSPUIntListTl(l);
        n--;
    }
    return l;
}


static uint32 pLsrSPUIntListDigitCount(PlsrSPUIntList l) {
    uint32 i=0;
    while (pLsrSPUIntListEmpty != l) {
        l = pLsrSPUIntListTl(l);
        i++;
    };
    return i;
}

static int pLsrSPUIntListCompare(PlsrSPUIntList l1, PlsrSPUIntList l2) {

    if (pLsrSPUIntListEmpty == l1) {
        /* Both empty => equal
         * l1 empty, l2 not => l1 < l2*/
        if (pLsrSPUIntListEmpty == l2) {
            return 0;
        } else {
            return -1;
        }
    }

    if (pLsrSPUIntListEmpty == l2) {
        /* l1 non-empty, l2 empty => l1 > l2 */
        return 1;
    }

    /* Both non-empty */
    {
        PlsrSPUInt d1 = pLsrSPUIntListHd(l1);
        PlsrSPUInt d2 = pLsrSPUIntListHd(l2);
        l1 = pLsrSPUIntListTl(l1);
        l2 = pLsrSPUIntListTl(l2);
        int c = pLsrSPUIntListCompare(l1, l2);
        if (c == 0) {
            return pLsrSPUIntCompare (d1, d2);
        } else {
            return c;
        }
    }
}

static PlsrBoolean pLsrSPUIntListLess(PlsrSPUIntList l1, PlsrSPUIntList l2) {
    return pLsrSPUIntListCompare(l1, l2) == -1;
}

static PlsrBoolean pLsrSPUIntListLessOrEqual(PlsrSPUIntList l1, PlsrSPUIntList l2) {
    return pLsrSPUIntListCompare(l1, l2) <= 0;
}

static PlsrBoolean pLsrSPUIntListGreater(PlsrSPUIntList l1, PlsrSPUIntList l2) {
    return pLsrSPUIntListCompare(l1, l2) == 1;
}

static PlsrBoolean pLsrSPUIntListGreaterOrEqual(PlsrSPUIntList l1, PlsrSPUIntList l2) {
    return pLsrSPUIntListCompare(l1, l2) >= 0;
}

static PlsrBoolean pLsrSPUIntListEqual(PlsrSPUIntList l1, PlsrSPUIntList l2) {
    return pLsrSPUIntListCompare(l1, l2) == 0;
}

static PlsrBoolean pLsrSPUIntListNotEqual(PlsrSPUIntList l1, PlsrSPUIntList l2) {
    return pLsrSPUIntListCompare(l1, l2) != 0;
}

static PlsrSPUIntList pLsrSPUIntListSubWithBorrow(PlsrSPUIntList l1, PlsrSPUIntList l2, PlsrSPUInt borrow) {
    if (pLsrSPUIntListEmpty == l2) {
        if (0 == borrow) {
            return l1;
        } else {
            return pLsrSPUIntListSubWithBorrow (l1,
                                                pLsrSPUIntListSingleton(borrow),
                                                0);
        }
    } else {
        assert(l1);
        PlsrSPUInt d1 = pLsrSPUIntListHd(l1);
        PlsrSPUInt d2 = pLsrSPUIntListHd(l2);
        PlsrSPUIntPair p = pLsrSPUIntSubWithBorrow(d1, d2, borrow);
        PlsrSPUInt d3 = p.fst;
        borrow = p.snd;
        l1 = pLsrSPUIntListTl(l1);
        l2 = pLsrSPUIntListTl(l2);
        PlsrSPUIntList upper = pLsrSPUIntListSubWithBorrow(l1, l2, borrow);
        return pLsrSPUIntListCanonicalCons(d3, upper);
    }
}

static PlsrSPUIntList pLsrSPUIntListSub(PlsrSPUIntList l1, PlsrSPUIntList l2) {
    assert(pLsrSPUIntListCompare(l1, l2) >= 0);
    return pLsrSPUIntListSubWithBorrow(l1, l2, 0);
}

static PlsrSPUIntList pLsrSPUIntListAdd(PlsrSPUIntList l1, PlsrSPUIntList l2) {
    PlsrSPUIntList acc = pLsrSPUIntListEmpty;
    PlsrSPUInt carry = 0;

    if (pLsrSPUIntListEmpty == l1) return l2;
    if (pLsrSPUIntListEmpty == l2) return l1;

    /* While we have digits in both, loop */
    while ((pLsrSPUIntListEmpty != l1) &&
           (pLsrSPUIntListEmpty != l2)) {
        PlsrSPUInt d1 = pLsrSPUIntListHd(l1);
        PlsrSPUInt d2 = pLsrSPUIntListHd(l2);
        PlsrSPUIntPair p = pLsrSPUIntAddWithCarry(d1, d2, carry);
        PlsrSPUInt d3 = p.fst;
        carry = p.snd;
        l1 = pLsrSPUIntListTl(l1);
        l2 = pLsrSPUIntListTl(l2);
        acc = pLsrSPUIntListCons(d3, acc);
    }

    if (0 == carry) {
        /* At least one is empty, no carry */
        if (pLsrSPUIntListEmpty == l1) {
            /* l2 might be non-empty, so keep its digits */
            return pLsrSPUIntListRevAppend(acc, l2);
        } else {
            /* l1 might be non-empty, so keep its digits
             * l2 must be empty, so drop it. */
            return pLsrSPUIntListRevAppend(acc, l1);
        }
    } else {
        /* We have a carry.  One of the li may be non-empty:
         * if so, loop over its digits adding in carries until
         * we run out of digits or there is not a carry */
        PlsrSPUIntList l;
        if (pLsrSPUIntListEmpty == l1) {
            l = l2;
        } else {
            l = l1;
        }
        while ((pLsrSPUIntListEmpty != l) && (0 != carry)) {
            PlsrSPUInt d = pLsrSPUIntListHd(l);
            PlsrSPUIntPair p = pLsrSPUIntAddWithCarry(d, 0, carry);
            d = p.fst;
            carry = p.snd;
            l = pLsrSPUIntListTl(l);
            acc = pLsrSPUIntListCons(d, acc);
        }
        /* Either carry is zero or l is empty.
         * if carry is zero, drop it and pre-pend (rev acc) to l
         * if carry is non-zero, then pre-pend it to acc,
         *  and pre-pend (rev acc) to l */
        acc = pLsrSPUIntListConsIfNonZero(carry, acc);
        return pLsrSPUIntListRevAppend(acc, l);
    }
}

static PlsrSPUIntList pLsrSPUIntSPUIntListMul(PlsrSPUInt digit, PlsrSPUIntList l) {
    PlsrSPUInt carry = 0;
    PlsrSPUIntList acc = pLsrSPUIntListEmpty;

    if (0 == digit) return pLsrSPUIntListEmpty;

    while (pLsrSPUIntListEmpty != l) {
        PlsrSPUInt x = pLsrSPUIntListHd(l);
        PlsrSPUIntPair p = pLsrSPUIntMulWithCarry(digit, x, carry);
        acc = pLsrSPUIntListCons(p.fst, acc);
        carry = p.snd;
        l = pLsrSPUIntListTl(l);
    }
    acc = pLsrSPUIntListConsIfNonZero(carry, acc);
    acc = pLsrSPUIntListRev(acc);
    return acc;
}

/* Shift dn...d0 left by x digits, filling with zeros.  If the list
 * is empty, returns the empty lists (that is, preserves canonical
 * form).
 * */
static PlsrSPUIntList pLsrSPUIntListShiftLeft(PlsrSPUIntList l, PlsrSPUInt by) {
    if (pLsrSPUIntListEmpty == l) return l;
    while (by > 0) {
        l = pLsrSPUIntListCons(0, l);
        by--;
    }
    return l;
}


static PlsrSPUIntList pLsrSPUIntListMul(PlsrSPUIntList l1, PlsrSPUIntList l2) {
    PlsrSPUIntList result = pLsrSPUIntListEmpty;
    PlsrSPUInt shift = 0;

    while (pLsrSPUIntListEmpty != l2) {
        PlsrSPUInt digit = pLsrSPUIntListHd(l2);
        PlsrSPUIntList l = pLsrSPUIntSPUIntListMul(digit, l1);
        l = pLsrSPUIntListShiftLeft(l, shift);
        result = pLsrSPUIntListAdd(result, l);
        l2 = pLsrSPUIntListTl(l2);
        shift++;
    }
    return result;
}


/* d != 0 */
static void pLsrSPUIntListSPUIntDivMod(PlsrSPUIntList* quotO, PlsrSPUInt* remO, PlsrSPUIntList l, PlsrSPUInt digit) {
    PlsrDPUInt r = 0;
    PlsrDPUInt d = pLsrDPUIntFromSPUInt(digit);
    PlsrSPUIntList q = pLsrSPUIntListEmpty;
    if (pLsrSPUIntListEmpty != l) {
        l = pLsrSPUIntListRev(l);
        while (pLsrSPUIntListEmpty != l) {
            PlsrDPUInt t = pLsrDPUIntFromSPUInt(pLsrSPUIntListHd(l));
            l = pLsrSPUIntListTl(l);
            t += pLsrSPUIntBase * r;
            q = pLsrSPUIntListCanonicalCons(pLsrSPUIntFromDPUInt(t / d), q);
            r = t % d;
        }
    }
    *quotO = q;
    *remO = pLsrSPUIntFromDPUInt(r);
    return;
}



static PlsrSPUInt pLsrSPUIntListDivModEstimateQDigit(PlsrSPUIntList digits, PlsrDPUInt v[2], PlsrSPUInt n) {
    int i = 0;
    PlsrSPUIntList tmp = digits;
    PlsrDPUInt u[3] = {0, 0, 0};
    for (i=0; (i < (n-2)) && (pLsrSPUIntListEmpty != tmp); i++) {
        tmp = pLsrSPUIntListTl(tmp);
    }
    /* Find the nth, n-1th, and n-2th digits of digits.
     * If they are not there, then they are implicitly zero.
     */
    for (i=0;(i < 3) && (pLsrSPUIntListEmpty != tmp); i++) {
        u[i] = pLsrDPUIntFromSPUInt(pLsrSPUIntListHd(tmp));
        tmp = pLsrSPUIntListTl(tmp);

    }
    /* qhat - 2 <= q <= qhat */
    PlsrDPUInt qhat = (u[2]*pLsrSPUIntBase + u[1]) / v[1];
    PlsrDPUInt rhat = (u[2]*pLsrSPUIntBase + u[1]) % v[1];
    if ((qhat >= pLsrSPUIntBase) ||
        (qhat * v[0]) > pLsrSPUIntBase * rhat + u[0]) {
        qhat--;
        rhat += v[1];
        if (rhat < pLsrSPUIntBase) {
            if ((qhat >= pLsrSPUIntBase) ||
                (qhat * v[0]) > pLsrSPUIntBase * rhat + u[0]) {
                qhat--;
            }
        }
    }
    return pLsrSPUIntFromDPUInt(qhat);
}

/* Divide two positive numbers num and den, where num >= den.
 * num and den each have at least 2 elements
 */
static void pLsrSPUIntListLongDivMod(PlsrSPUIntList* quotO, PlsrSPUIntList* remO,
                                     PlsrSPUIntList num, PlsrSPUIntList den) {
    int n = 0;
    PlsrDPUInt v[2] = {0, 0};
    assert (pLsrSPUIntListCompare (num, den) >= 0);

    /* Compute the length and the last two elements of the divisor*/
    {
        PlsrSPUIntList tmp = den;
        while (pLsrSPUIntListEmpty != tmp) {
            v[0] = v[1];
            v[1] = pLsrDPUIntFromSPUInt(pLsrSPUIntListHd(tmp));
            tmp = pLsrSPUIntListTl(tmp);
            n++;
        }
    }
    assert(v[1]);
    PlsrSPUInt normValue = pLsrSPUIntFromDPUInt(pLsrSPUIntBase / (v[1] + 1));

    /*
     * ensure that the most significant digit of den is >= pLsrSPUIntBase/2 .
     */

    num = pLsrSPUIntSPUIntListMul(normValue, num);
    den = pLsrSPUIntSPUIntListMul(normValue, den);

    /* Last elements may have changed, so recompute */
    {
        PlsrSPUIntList tmp = den;
        while (pLsrSPUIntListEmpty != tmp) {
            v[0] = v[1];
            v[1] = pLsrDPUIntFromSPUInt(pLsrSPUIntListHd(tmp));
            tmp = pLsrSPUIntListTl(tmp);
        }
    }

    PlsrSPUIntList numDigits = pLsrSPUIntListRev(num);
    PlsrSPUIntList q = pLsrSPUIntListEmpty;

    /* There are at least n digits in num.
     * The following loop successively divides n+1 digits of num by
     * the n digits of den.  There may be less than n+1 non-zero
     * digits available in the canonical form.  Unlike Knuth, we do
     * not keep leading zeroes, instead keeping the intermediate result
     * in canonical form.  The code to obtain the leading digits
     * for estimating the quotient digit needs to do the right thing
     * when the digits are not available.
     */
    PlsrSPUIntList curDigits = pLsrSPUIntListTakeRev(numDigits, n);

    /* Grab the n+1th digit if available */
    numDigits = pLsrSPUIntListDrop(numDigits, n);
    if (pLsrSPUIntListEmpty != numDigits) {
        curDigits = pLsrSPUIntListCons(pLsrSPUIntListHd(numDigits), curDigits);
        numDigits = pLsrSPUIntListTl(numDigits);
    }

    while (1) {

        PlsrSPUInt qhat = pLsrSPUIntListDivModEstimateQDigit(curDigits, v, n);
        PlsrSPUIntList partial = pLsrSPUIntSPUIntListMul (qhat, den);
        if (pLsrSPUIntListLess(curDigits, partial)) {
            qhat--;
            partial = pLsrSPUIntListSub(partial, den);
        }
        assert (pLsrSPUIntListGreaterOrEqual(curDigits, partial));
        curDigits = pLsrSPUIntListSub(curDigits, partial);
        q = pLsrSPUIntListCons(qhat, q);
        if (pLsrSPUIntListEmpty == numDigits) {
            break;
        }
        curDigits = pLsrSPUIntListCons(pLsrSPUIntListHd(numDigits), curDigits);
        numDigits = pLsrSPUIntListTl(numDigits);
    }

    {
        PlsrSPUInt dead = 0;  /* Unused */
        pLsrSPUIntListSPUIntDivMod(remO, &dead, curDigits, normValue);
    }
    *quotO = q;
    return;
}

/* Divide two positive numbers num and den */
static void pLsrSPUIntListDivMod(PlsrSPUIntList* quotO, PlsrSPUIntList* remO, PlsrSPUIntList num, PlsrSPUIntList den) {

    if (pLsrSPUIntListEmpty == pLsrSPUIntListTl(den)) {
        PlsrSPUInt rO = 0;
        pLsrSPUIntListSPUIntDivMod(quotO, &rO, num, pLsrSPUIntListHd(den));
        *remO = pLsrSPUIntListConsIfNonZero(rO, pLsrSPUIntListEmpty);
    } else if (pLsrSPUIntListCompare (num, den) >= 0) {
        pLsrSPUIntListLongDivMod(quotO, remO, num, den);
    } else {
        *quotO = pLsrSPUIntListEmpty;
        *remO = num;
    }
}


/* i != 0 */
static PlsrSPUIntList pLsrSPUIntListFromUInt32(uint32 i) {
    return pLsrSPUIntListSingleton(i);
}

/* l is non-empty (non-zero) */
static uint32 pLsrUInt32FromSPUIntList(PlsrSPUIntList l) {
    return pLsrSPUIntListHd(l);
}

/* Arbitrary precision integers */
typedef enum {
    PlsrAPNeg,
    PlsrAPPos,
    PlsrAPZero
} PlsrAPSign;

/* This structure must always be initialised immediately after allocation
 * without an intervening GC.  If this is not the case, the mutability
 * property of the vtable must be changed.
 */
typedef struct PlsrAPIntS_ {
    PlsrVTable vtable;
    PlsrAPSign sign;
    PlsrSPUIntList digits;
} PlsrAPIntS;

#define pLsrAPIntPadding                                        \
    (sizeof(PlsrAPIntS) - sizeof(PlsrAPSign) - sizeof(PlsrSPUIntList))
pLsrVTableStatic(pLsrAPIntVTable_, "*ap integer*", pLsrAPIntPadding);
#define pLsrAPIntVTable (&pLsrAPIntVTable_)


#ifdef P_USE_PILLAR
typedef PlsrRef PlsrAPInt;
#else /* !P_USE_PILLAR */
typedef PlsrAPIntS* PlsrAPInt;
#endif /* !P_USE_PILLAR */

#ifdef PLSR_AP_INT_TRACE
static void pLsrAPIntShow1(char * s, PlsrAPInt i);
static void pLsrAPIntShow2(char * s, PlsrAPInt i1, PlsrAPInt i2);
static void pLsrAPIntShow3(char * s, PlsrAPInt i1, PlsrAPInt i2, PlsrAPInt i3);
static void pLsrAPIntShow4(char * s, PlsrAPInt i1, PlsrAPInt i2, PlsrAPInt i3, PlsrAPInt i4);
#define pLsrAPIntTrace0(s) printf("APInt: %s\n", s)
#define pLsrAPIntTrace1(s, i1)  pLsrAPIntShow1(s, i1)
#define pLsrAPIntTrace2(s, i1, i2) pLsrAPIntShow2(s, i1, i2)
#define pLsrAPIntTrace3(s, i1, i2, i3) pLsrAPIntShow3(s, i1, i2, i3)
#define pLsrAPIntTrace4(s, i1, i2, i3, i4) pLsrAPIntShow4(s, i1, i2, i3, i4)
#define pLsrAPIntTraceFmt1(s, i1)  printf(s, i1)
#define pLsrAPIntTraceFmt2(s, i1, i2)  printf(s, i1, i2)
#else
#define pLsrAPIntTrace0(s)
#define pLsrAPIntTrace1(s, i1)
#define pLsrAPIntTrace2(s, i1, i2)
#define pLsrAPIntTrace3(s, i1, i2, i3)
#define pLsrAPIntTrace4(s, i1, i2, i3, i4)
#define pLsrAPIntTraceFmt1(s, i1)
#define pLsrAPIntTraceFmt2(s, i1, i2)
#endif

static PlsrAPIntS pLsrAPIntZero_ = {.vtable = pLsrAPIntVTable,
                                    .sign = PlsrAPZero,
                                    .digits = pLsrSPUIntListEmpty };
#define pLsrAPIntZero ((PlsrAPInt) &pLsrAPIntZero_)

static PlsrAPIntS pLsrAPIntOne_ = {.vtable = pLsrAPIntVTable,
                                   .sign = PlsrAPPos,
                                   .digits = (PlsrSPUIntList) &pLsrSPUIntListOne_ };
#define pLsrAPIntOne ((PlsrAPInt) &pLsrAPIntOne_)


static PlsrAPIntS pLsrAPIntMinusOne_ = {.vtable = pLsrAPIntVTable,
                                        .sign = PlsrAPNeg,
                                        .digits = (PlsrSPUIntList) &pLsrSPUIntListOne_ };
#define pLsrAPIntMinusOne ((PlsrAPInt) &pLsrAPIntMinusOne_)

static PlsrAPIntS pLsrAPIntMinusUInt32Max_ = {.vtable = pLsrAPIntVTable,
                                              .sign = PlsrAPNeg,
                                              .digits = (PlsrSPUIntList) &pLsrSPUIntListUInt32Max_ };
#define pLsrAPIntMinusUInt32Max ((PlsrAPInt) &pLsrAPIntMinusUInt32Max_)

#define pLsrAPIntRep(l) ((PlsrAPIntS*)(l))
#define pLsrAPIntDigits(l) (pLsrAPIntRep(l)->digits)
#define pLsrAPIntSign(l) (pLsrAPIntRep(l)->sign)

static void pLsrPrintAPIntRaw(PlsrAPInt a) {
    PlsrAPSign sign = pLsrAPIntSign(a);
    PlsrSPUIntList digits = pLsrAPIntDigits(a);

    if (PlsrAPNeg == sign) {
        printf("- ");
    }

    while(digits) {
        printf ("%u ", pLsrSPUIntListHd(digits));
        digits = pLsrSPUIntListTl(digits);
    }
    return;
}

static PlsrAPInt pLsrAPIntMk(PlsrAPSign sign, PlsrSPUIntList digits) {
    uintp size = sizeof(PlsrAPIntS);
    PlsrAPInt a;
    noyield {
        pLsrAlloc(PlsrAPInt, a, pLsrAPIntVTable, size);
        pLsrWriteBarrierRefOptBase(a, pLsrAPIntRep(a)->digits, digits);
        pLsrAPIntRep(a)->sign = sign;
    }
    return a;
}

static int pLsrAPIntCompareAbs(PlsrAPInt a, PlsrAPInt b) {
    return pLsrSPUIntListCompare(pLsrAPIntDigits(a),
                                 pLsrAPIntDigits(b));
}

static int pLsrAPIntCompare(PlsrAPInt a, PlsrAPInt b) {

    PlsrAPSign sa = pLsrAPIntSign(a);
    PlsrAPSign sb = pLsrAPIntSign(b);
    pLsrAPIntTrace2("Compare", a, b);
    if (sa == sb) {
        switch (sa) {
        case PlsrAPZero:
            return 0;
        case PlsrAPPos:
            return pLsrAPIntCompareAbs(a, b);
        case PlsrAPNeg:
            return pLsrAPIntCompareAbs(b, a);
        }
    }

    if (PlsrAPNeg == sa) return -1;
    if (PlsrAPNeg == sb) return 1;
    if (PlsrAPZero == sa) return -1;
    if (PlsrAPZero == sb) return 1;
    assert(0);
    return 0;
}

static PlsrBoolean pLsrAPIntLess(PlsrAPInt a, PlsrAPInt b) {
    return pLsrAPIntCompare(a, b) == -1;
}

static PlsrBoolean pLsrAPIntGreater(PlsrAPInt a, PlsrAPInt b) {
    return pLsrAPIntCompare(a, b) == 1;
}

static PlsrBoolean pLsrAPIntEqual(PlsrAPInt a, PlsrAPInt b) {
    return pLsrAPIntCompare(a, b) == 0;
}

static PlsrBoolean pLsrAPIntNotEqual(PlsrAPInt a, PlsrAPInt b) {
    return pLsrAPIntCompare(a, b) != 0;
}

static PlsrBoolean pLsrAPIntLessOrEqual(PlsrAPInt a, PlsrAPInt b) {
    return pLsrAPIntCompare(a, b) != 1;
}

static PlsrBoolean pLsrAPIntGreaterOrEqual(PlsrAPInt a, PlsrAPInt b) {
    return pLsrAPIntCompare(a, b) != -1;
}

static PlsrAPInt pLsrAPIntBNot(PlsrAPInt a)
{
    pLsrRuntimeError("FLRC integers do not support bitwise ops\n");
    assert(0);
    return(0);
}

static PlsrAPInt pLsrAPIntBAnd(PlsrAPInt a, PlsrAPInt b)
{
    pLsrRuntimeError("FLRC integers do not support bitwise ops\n");
    assert(0);
    return(0);
}

static PlsrAPInt pLsrAPIntBOr(PlsrAPInt a, PlsrAPInt b)
{
    pLsrRuntimeError("FLRC integers do not support bitwise ops\n");
    assert(0);
    return(0);
}

static PlsrAPInt pLsrAPIntBShiftL(PlsrAPInt a, PlsrAPInt b)
{
    pLsrRuntimeError("FLRC integers do not support bitwise ops\n");
    assert(0);
    return(0);
}

static PlsrAPInt pLsrAPIntBShiftR(PlsrAPInt a, PlsrAPInt b)
{
    pLsrRuntimeError("FLRC integers do not support bitwise ops\n");
    assert(0);
    return(0);
}

static PlsrAPInt pLsrAPIntBXor(PlsrAPInt a, PlsrAPInt b)
{
    pLsrRuntimeError("FLRC integers do not support bitwise ops\n");
    assert(0);
    return(0);
}

static PlsrAPInt pLsrAPIntNegate(PlsrAPInt a) {
    switch (pLsrAPIntSign(a)) {
    case PlsrAPZero: return a;
    case PlsrAPPos :
        return pLsrAPIntMk(PlsrAPNeg, pLsrAPIntDigits(a));
    case PlsrAPNeg:
        return pLsrAPIntMk(PlsrAPPos, pLsrAPIntDigits(a));
    }
    assert(0);
    return 0;
}

static PlsrAPInt pLsrAPIntAbs(PlsrAPInt a) {
    switch (pLsrAPIntSign(a)) {
    case PlsrAPNeg:
        return pLsrAPIntMk(PlsrAPPos, pLsrAPIntDigits(a));
    case PlsrAPZero:
    case PlsrAPPos : return a;
    }
    assert(0);
    return 0;
}


static PlsrAPInt pLsrAPIntAddAbs(PlsrAPInt a, PlsrAPInt b) {
    PlsrSPUIntList digits = pLsrSPUIntListAdd(pLsrAPIntDigits(a),
                                              pLsrAPIntDigits(b));
    return pLsrAPIntMk(PlsrAPPos, digits);
}

/* |a| - |b|, assumes |a| >= |b| */
static PlsrAPInt pLsrAPIntSubAbs1(PlsrAPInt a, PlsrAPInt b) {
    PlsrSPUIntList digits = pLsrSPUIntListSub(pLsrAPIntDigits(a),
                                              pLsrAPIntDigits(b));
    PlsrAPSign sign;
    if (digits) {sign = PlsrAPPos;} else {sign = PlsrAPZero;}
    return pLsrAPIntMk(sign, digits);
}

/* |a| - |b| */
static PlsrAPInt pLsrAPIntSubAbs(PlsrAPInt a, PlsrAPInt b) {
    switch (pLsrAPIntCompareAbs (a, b)) {
    case -1:
        return pLsrAPIntNegate(pLsrAPIntSubAbs1(b, a));
    case 1:
        return pLsrAPIntSubAbs1(a, b);
    default:
        return pLsrAPIntZero;
    }
}

static PlsrAPInt pLsrAPIntAdd(PlsrAPInt a, PlsrAPInt b) {
    PlsrAPSign sa = pLsrAPIntSign(a);
    PlsrAPSign sb = pLsrAPIntSign(b);
    PlsrAPInt res;
    if (PlsrAPZero == sa) {
        res = b;
    } else if (PlsrAPZero == sb) {
        res = a;
    } else if (PlsrAPNeg == sa) {
        if (PlsrAPNeg == sb) {
            res = pLsrAPIntNegate(pLsrAPIntAddAbs (a, b));
        } else {
            res = pLsrAPIntSubAbs (b, a);
        }
    } else { /* a is positive */
        if (PlsrAPNeg == sb) {
            res = pLsrAPIntSubAbs (a, b);
        } else {
            res = pLsrAPIntAddAbs (b, a);
        }
    }
    pLsrAPIntTrace3("mpz_add", a, b, res);
    return res;
}


static PlsrAPInt pLsrAPIntSub(PlsrAPInt a, PlsrAPInt b) {
    PlsrAPSign sa = pLsrAPIntSign(a);
    PlsrAPSign sb = pLsrAPIntSign(b);

    if (PlsrAPZero == sa) return pLsrAPIntNegate(b);
    if (PlsrAPZero == sb) return a;
    if (PlsrAPNeg == sa) {
        if (PlsrAPNeg == sb) {
            /* (-a) - (-b) = (-a) + b = b - a = |-b| - |-a| */
            return pLsrAPIntSubAbs(b, a);
        } else {
            /* (-a) - b = -(-((-a) - b) = -(a + b) = - (|-a| + |-b|) */
            return pLsrAPIntNegate(pLsrAPIntAddAbs (b, a));
        }
    } else { /* a is positive */
        if (PlsrAPNeg == sb) {
            /* a - (-b) = a + b = |a| + |-b| */
            return pLsrAPIntAddAbs (a, b);
        } else {
            /* a - b = |a| - |b| */
            return pLsrAPIntSubAbs (a, b);
        }
    }
}

static PlsrBoolean pLsrAPIntIsZero(PlsrAPInt a) {
    return (PlsrAPZero == pLsrAPIntSign(a));
}

static PlsrAPInt pLsrAPIntMulAbs(PlsrAPInt a, PlsrAPInt b) {
    PlsrSPUIntList digits = pLsrSPUIntListMul(pLsrAPIntDigits(a),
                                              pLsrAPIntDigits(b));
    return pLsrAPIntMk(PlsrAPPos, digits);
}

static PlsrAPInt pLsrAPIntMul(PlsrAPInt a, PlsrAPInt b) {
    PlsrAPSign sa = pLsrAPIntSign(a);
    PlsrAPSign sb = pLsrAPIntSign(b);
    PlsrAPInt res;
    if (PlsrAPZero == sa) res = pLsrAPIntZero;
    else if (PlsrAPZero == sb) res = pLsrAPIntZero;
    else if (sa == sb) res = pLsrAPIntMulAbs (a, b);
    else res = pLsrAPIntNegate (pLsrAPIntMulAbs (a, b));
    pLsrAPIntTrace3("mpz_mul", a, b, res);
    return res;
}


/* Return {floor(|a|/|b|), |a|%|b|}, a,b != 0 */
static void pLsrAPIntDivModAbs(PlsrAPInt* quotO, PlsrAPInt* remO, PlsrAPInt a, PlsrAPInt b) {
    PlsrSPUIntList qDigits = NULL;
    PlsrSPUIntList rDigits = NULL;
    pLsrSPUIntListDivMod(&qDigits,
                         &rDigits,
                         pLsrAPIntDigits(a),
                         pLsrAPIntDigits(b));
    PlsrAPSign qSign;
    if (qDigits) {qSign = PlsrAPPos;} else {qSign = PlsrAPZero;}
    PlsrAPSign rSign;
    if (rDigits) {rSign = PlsrAPPos;} else {rSign = PlsrAPZero;}
    *quotO = pLsrAPIntMk(qSign, qDigits);
    *remO = pLsrAPIntMk(rSign, rDigits);
    return;
}

/* We provide three different versions of division, corresponding to the
 * different versions described by Boute ("The Euclidean Definition of
 * the Functions div and mod"), and Leijen ("Division and Modulus for
 * Computer Scientists").
 * All satisfy Euclids theorem:  a = qd + r, |r| < |d|.
 * div/mod,  F-division, (Knuth), (div-dominant), sign(r) = sign (d)
 * quot/rem, T-division,          (div-dominant), sign(r) = sign (a)
 *           E-division, (Boute), (mod-dominant), r is positive
 */

/* F-division in the terminology of Boute.
 * div/mod.  a/d = (q, r) s.t. a = qd + r, |r| < |d| and
 * 1) r has same sign as d
 * 2) q is floor(a/d) (round to negative infinity)
 *
 * We want A = QD + R
 * Assume we have:
 *    a = qd + r, a, d, q, r, positive, 0 <= r < d.
 * where a = |A|, d = |D|
 *
 * 1) both neg: want -a = Q(-d) + R, -d < R <= 0
 *     choose Q = q, R = -r
 *      A = QD    +  R
 *     -a = (q)-d + -r = -qd - r
 *      a = qd + r
 * 2) numerator neg: want -a = Qd + R, 0 <= R < d
 *     if r = 0, choose R = 0, Q = -q, otherwise
 *     choose Q = -(q+1), R = d - r
 *      A = QD      + R
 *     -a = -(q+1)d + d - r = -qd -d + d -r = -qd -r
 *      a = qd + r
 * 3) denominator neg: want a = Q(-d) + R, -d < R <= 0
 *     if r = 0, choose R = 0, Q = -q, otherwise
 *     choose Q = -(q+1), R = - (d - r)
 *      A = QD         + R
 *      a = -(q+1)(-d) + -(d -r) = qd + d -d + r = qd + r
 *      a = qd + r
 */

static void pLsrAPIntDivModF(PlsrAPInt* quotO, PlsrAPInt* remO, PlsrAPInt a, PlsrAPInt b) {
    PlsrAPSign sa = pLsrAPIntSign(a);
    PlsrAPSign sb = pLsrAPIntSign(b);
    PlsrAPInt q = NULL;
    PlsrAPInt r = NULL;

    if (PlsrAPZero == sa) {
        *quotO = pLsrAPIntZero;
        *remO = pLsrAPIntZero;
        return;
    }

    if (PlsrAPZero == sb) {
        pLsrRuntimeError ("Divide by zero");
    }

    a = pLsrAPIntAbs(a);
    b = pLsrAPIntAbs(b);
    pLsrAPIntDivModAbs(&q, &r, a, b);
    if (sa != sb) {
        if (pLsrAPIntIsZero(r)) {
            q = pLsrAPIntNegate(q);
        } else {
            q = pLsrAPIntNegate(pLsrAPIntAdd(q, pLsrAPIntOne));
            r = pLsrAPIntSub(b, r);
        }
    }

    if (PlsrAPNeg == sb) {
        r = pLsrAPIntNegate(r);
    }
    *quotO = q;
    *remO = r;
    return;
}

static PlsrAPInt pLsrAPIntDivF(PlsrAPInt a, PlsrAPInt b) {
    PlsrAPInt quot = NULL;
    PlsrAPInt rem = NULL;
    pLsrAPIntDivModF(&quot, &rem, a, b);
    return quot;
}

static PlsrAPInt pLsrAPIntModF(PlsrAPInt a, PlsrAPInt b) {
    PlsrAPInt quot = NULL;
    PlsrAPInt rem = NULL;
    pLsrAPIntDivModF(&quot, &rem, a, b);
    return rem;
}

/* T-division in the terminology of Boute.
 * quot/rem.  a/d = (q, r) s.t. a = qd + r, |r| < |d| and
 * 1) r has same sign as a
 * 2) q is trunc(a/d) (round to zero)
 *
 * We want A = QD + R
 * Assume we have:
 *    a = qd + r, a, d, q, r, positive, 0 <= r < d.
 * where a = |A|, d = |D|
 *
 * 1) both neg: want -a = Q(-d) + R, -d < R <= 0
 *     choose Q = q, R = -r
 *      A = QD    + R
 *     -a = q(-d) + -r = -qd - r
 *      a = qd + r
 * 2) numerator neg: want -a = Qd + R, -d < R <= 0
 *     choose Q = -q, R = -r
 *      A = QD  + R
 *     -a = -qd + -r
 *      a = qd + r
 * 3) denominator neg: want a = Q(-d) + R, 0 <= R < d
 *     choose R = -q, R = r
 *      A = QD     + R
 *      a = -q(-d) + r = qd + r
 *      a = qd + r
 */
static void pLsrAPIntDivModT(PlsrAPInt* quotO, PlsrAPInt* remO, PlsrAPInt a, PlsrAPInt b) {
    PlsrAPSign sa = pLsrAPIntSign(a);
    PlsrAPSign sb = pLsrAPIntSign(b);
    PlsrAPInt q = NULL;
    PlsrAPInt r = NULL;

    if (PlsrAPZero == sa) {
        *quotO = pLsrAPIntZero;
        *remO = pLsrAPIntZero;
        return;
    }

    if (PlsrAPZero == sb) {
        pLsrRuntimeError ("Divide by zero");
    }

    a = pLsrAPIntAbs(a);
    b = pLsrAPIntAbs(b);
    pLsrAPIntDivModAbs(&q, &r, a, b);
    if (sa != sb) {
        q = pLsrAPIntNegate(q);
    }

    if (PlsrAPNeg == sa) {
        r = pLsrAPIntNegate(r);
    }
    *quotO = q;
    *remO = r;
    return;
}

static PlsrAPInt pLsrAPIntDivT(PlsrAPInt a, PlsrAPInt b) {
    PlsrAPInt quot = NULL;
    PlsrAPInt rem = NULL;
    pLsrAPIntDivModT(&quot, &rem, a, b);
    return quot;
}

static PlsrAPInt pLsrAPIntModT(PlsrAPInt a, PlsrAPInt b) {
    PlsrAPInt quot = NULL;
    PlsrAPInt rem = NULL;
    pLsrAPIntDivModT(&quot, &rem, a, b);
    return rem;
}


/* R-division in the terminology of Boute.  a/d = (q, r) s.t.
 * divide.  a/d = (q, r) s.t. a = qd + r, |r| < |d| and
 * 1) r is positive
 *
 * We want A = QD + R
 * Assume we have:
 *    a = qd + r, a, d, q, r, positive, 0 <= r < d.
 * where a = |A|, d = |D|
 *
 * 1) both neg: want -a = Q(-d) + R
 *     if r = 0, choose R = 0, Q = q, otherwise
 *     choose Q = q + 1, R = d - r
 *      A =  QD     + R
 *     -a = (q+1)-d + (d-r) = -qd -d + d -r = -qd -r
 *      a = qd + r
 * 2) numerator neg: want -a = Qd + R
 *     if r = 0, choose R = 0, Q = -q, otherwise
 *     choose Q = -(q+1), R = d - r
 *      A = QD      + R
 *     -a = -(q+1)d + d - r = -qd -d + d -r = -qd -r
 *      a = qd + r
 * 3) denominator neg: want a = Q(-d) + R
 *     choose Q = -q, R = r
 *      A = QD     + R
 *      a = -q(-d) + r
 *      a = qd + r
 */
static void pLsrAPIntDivModE(PlsrAPInt* quotO, PlsrAPInt* remO, PlsrAPInt a, PlsrAPInt b) {
    PlsrAPSign sa = pLsrAPIntSign(a);
    PlsrAPSign sb = pLsrAPIntSign(b);
    PlsrAPInt q = NULL;
    PlsrAPInt r = NULL;

    if (PlsrAPZero == sa) {
        *quotO = pLsrAPIntZero;
        *remO = pLsrAPIntZero;
        return;
    }

    if (PlsrAPZero == sb) {
        pLsrRuntimeError ("Divide by zero");
    }

    a = pLsrAPIntAbs(a);
    b = pLsrAPIntAbs(b);
    pLsrAPIntDivModAbs(&q, &r, a, b);

    if (!pLsrAPIntIsZero(r)) {
        if (PlsrAPNeg == sa) {
            q = pLsrAPIntAdd(q, pLsrAPIntOne);
            r = pLsrAPIntSub(b, r);
        }
    }
    if (sa != sb) {
        q = pLsrAPIntNegate(q);
    }
    *quotO = q;
    *remO = r;
    return;
}

static PlsrAPInt pLsrAPIntDivE(PlsrAPInt a, PlsrAPInt b) {
    PlsrAPInt q;
    PlsrAPInt r;
    pLsrAPIntDivModE(&q, &r, a, b);
    return q;
}

static PlsrAPInt pLsrAPIntModE(PlsrAPInt a, PlsrAPInt b) {
    PlsrAPInt q;
    PlsrAPInt r;
    pLsrAPIntDivModE(&q, &r, a, b);
    return r;
}

static PlsrAPInt pLsrAPIntFromSInt32(sint32 i) {
    PlsrAPSign sign;
    PlsrSPUIntList digits;
    uint32 ui;
    if (i == 0) return pLsrAPIntZero;
    if(i < 0) {
        sign = PlsrAPNeg;
        ui = pLsrUInt32FromNegativeSInt32Abs(i);
    } else {
        sign = PlsrAPPos;
        ui = i;
    }
    digits = pLsrSPUIntListFromUInt32(ui);
    return pLsrAPIntMk(sign, digits);
}

static sint32 pLsrSInt32FromAPInt(PlsrAPInt a) {
    PlsrAPSign sign = pLsrAPIntSign(a);
    if (PlsrAPZero == sign) return 0;

    uint32 i = pLsrUInt32FromSPUIntList(pLsrAPIntDigits(a));
    if (PlsrAPNeg == sign) {
        return -((sint32)i); /* By invariant no overflow*/
    } else {
        return (sint32)i;
    }
}

static PlsrAPInt pLsrAPIntFromUInt32(uint32 i) {
    PlsrSPUIntList digits;
    if (i == 0) return pLsrAPIntZero;

    digits = pLsrSPUIntListFromUInt32(i);
    return pLsrAPIntMk(PlsrAPPos, digits);
}

static uint32 pLsrUInt32FromAPInt(PlsrAPInt a) {
    if (pLsrAPIntIsZero(a)) {
        return 0;
    } else {
        return pLsrUInt32FromSPUIntList(pLsrAPIntDigits(a));
    }
}

#define pLsrMkAPIntFromSIntLarge(TName, T, UT, uFromNegT)               \
    static PlsrAPInt pLsrAPIntFrom##TName(T i) {                        \
        PlsrAPSign sign;                                                \
        PlsrSPUIntList digits;                                          \
        UT ui;                                                          \
        if (i == 0) return pLsrAPIntZero;                               \
        if(i < 0) {                                                     \
            sign = PlsrAPNeg;                                           \
            ui = uFromNegT(i);                                          \
        } else {                                                        \
            sign = PlsrAPPos;                                           \
            ui = (UT) i;                                                \
        }                                                               \
        if (sizeof(T) <= sizeof(uint32)) {                              \
            digits = pLsrSPUIntListFromUInt32(ui);                      \
        } else {                                                        \
            digits = pLsrSPUIntListEmpty;                               \
            while(ui > 0) {                                             \
                digits = pLsrSPUIntListCons(ui % pLsrSPUIntBase, digits); \
                ui = ui / pLsrSPUIntBase;                               \
            }                                                           \
            digits = pLsrSPUIntListRev(digits);                         \
        }                                                               \
        PlsrAPInt res = pLsrAPIntMk(sign, digits);                      \
        pLsrAPIntTrace1(#T, res);                                       \
        return res;                                                     \
    }

/*pLsrAPIntFromSIntp*/
pLsrMkAPIntFromSIntLarge(SIntp, sintp, uintp, pLsrUIntpFromNegativeSIntpAbs);
/*pLsrAPIntFromSInt64*/
pLsrMkAPIntFromSIntLarge(SInt64, sint64, uint64, pLsrUInt64FromNegativeSInt64Abs);

#define pLsrMkSIntLargeFromAPInt(TName, T)                      \
    static T pLsr##TName##FromAPInt(PlsrAPInt a) {              \
        pLsrAPIntTrace1(#T, a);                                 \
        T res;                                                  \
        PlsrAPSign sign = pLsrAPIntSign(a);                     \
        T i = 0;                                                \
        if (PlsrAPZero == sign) {                               \
            res = 0;                                            \
        } else {                                                \
            if (sizeof(T) <= sizeof(uint32)) {                          \
                i = pLsrUInt32FromSPUIntList(pLsrAPIntDigits(a));       \
            } else {                                                    \
                sintp shift = 0;                                        \
                PlsrSPUIntList digits = pLsrAPIntDigits(a);             \
                while(digits) {                                         \
                    i += ((T) pLsrSPUIntListHd(digits)) << shift;       \
                    digits = pLsrSPUIntListTl(digits);                  \
                    shift += (sizeof(PlsrSPUInt))*CHAR_BIT;             \
                }                                                       \
            }                                                           \
            if (PlsrAPNeg == sign) {res = -i;} else {res =  i;}         \
        }                                                               \
        pLsrAPIntTraceFmt2("To %s %lld\n", #T, (sint64) res);           \
        return res;                                                     \
    }

/*pLsrSIntpFromAPInt*/
pLsrMkSIntLargeFromAPInt(SIntp, sintp);
/*pLsrSInt64FromAPInt*/
pLsrMkSIntLargeFromAPInt(SInt64, sint64);

#define pLsrMkAPIntFromUIntLarge(TName, T)                              \
    static PlsrAPInt pLsrAPIntFrom##TName(T ui) {                       \
        PlsrAPSign sign;                                                \
        PlsrSPUIntList digits;                                          \
        if (ui == 0) return pLsrAPIntZero;                              \
        sign = PlsrAPPos;                                               \
        if (sizeof(T) <= sizeof(uint32)) {                              \
            digits = pLsrSPUIntListFromUInt32(ui);                      \
        } else {                                                        \
            digits = pLsrSPUIntListEmpty;                               \
            while(ui > 0) {                                             \
                digits = pLsrSPUIntListCons(ui % pLsrSPUIntBase, digits); \
                ui = ui / pLsrSPUIntBase;                               \
            }                                                           \
            digits = pLsrSPUIntListRev(digits);                         \
        }                                                               \
        return pLsrAPIntMk(sign, digits);                               \
    }

/*pLsrAPIntFromUIntp*/
pLsrMkAPIntFromUIntLarge(UIntp, uintp);
/*pLsrAPIntFromUInt64*/
pLsrMkAPIntFromUIntLarge(UInt64, uint64);

#define pLsrMkUIntLargeFromAPInt(TName, T)                      \
    static T pLsr##TName##FromAPInt(PlsrAPInt a) {              \
        PlsrAPSign sign = pLsrAPIntSign(a);                     \
        T i = 0;                                                \
        if (PlsrAPZero == sign) return 0;                       \
                                                                \
        if (sizeof(T) <= sizeof(uint32)) {                      \
            i = pLsrUInt32FromSPUIntList(pLsrAPIntDigits(a));   \
        } else {                                                \
            sintp shift = 0;                                    \
            PlsrSPUIntList digits = pLsrAPIntDigits(a);         \
            while(digits) {                                     \
                i += ((T) pLsrSPUIntListHd(digits)) << shift;   \
                digits = pLsrSPUIntListTl(digits);              \
                shift += (sizeof(PlsrSPUInt))*CHAR_BIT;         \
            }                                                   \
        }                                                       \
        return  i;                                              \
    }

/*pLsrUIntpFromAPInt*/
pLsrMkUIntLargeFromAPInt(UIntp, uintp);
/*pLsrUInt64FromAPInt*/
pLsrMkUIntLargeFromAPInt(UInt64, uint64);

static uint32 pLsrAPIntFitsInSInt32(PlsrAPInt a) {
    PlsrAPSign sign = pLsrAPIntSign(a);
    PlsrSPUIntList digits = pLsrAPIntDigits(a);
    if (PlsrAPZero == sign) return 1;
    if (pLsrSPUIntListDigitCount(digits) > 1) return 0;
    return (pLsrSPUIntListHd(digits)) <= SINT32_MAX;
}

/* Returns SINT32_MIN if (a >= upper) or (a <= lower)
 * Otherwise returns a.
 * To be useful, (lower > SINT32_MIN) should be true.
 */
static sint32 pLsrAPIntCheckRangeSInt32(PlsrAPInt a, sint32 upper, sint32 lower)
{
    if (pLsrAPIntFitsInSInt32(a)) {
        sint32 ai = pLsrSInt32FromAPInt(a);
        return ((ai <= upper) && (ai >= lower)) ? ai : SINT32_MIN;
    } else {
        return SINT32_MIN;
    }
}

static PlsrAPInt pLsrAPIntFromFloat32(float32 f) {
    pLsrRuntimeError("pLsrAPIntFromFloat32 not implemented");
    return 0;
}

static PlsrAPInt pLsrAPIntFromFloat64(float64 f) {
    pLsrRuntimeError("pLsrAPIntFromFloat64 not implemented");
    return 0;
}

static float32 pLsrFloat32FromAPInt(PlsrAPInt a) {
    pLsrRuntimeError("pLsrFloat32FromAPInt not implemented");
    return 0;
}

static float64 pLsrFloat64FromAPInt(PlsrAPInt a) {
    pLsrRuntimeError("pLsrFloat64FromAPInt not implemented");
    return 0;
}

static char pLsrAPIntDecimalDigits[] =
    {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'};

static char* pLsrCStringFromAPInt(PlsrAPInt a) {
    PlsrSPUIntList dDigits = pLsrSPUIntListEmpty;
    PlsrAPInt ten = pLsrAPIntFromUInt32(10);
    PlsrAPSign sign = pLsrAPIntSign(a);
    uint32 digits=0;

    a = pLsrAPIntAbs(a);
    while (!pLsrAPIntIsZero(a)) {
        PlsrAPInt q = NULL;
        PlsrAPInt r = NULL;
        pLsrAPIntDivModE(&q, &r, a, ten);
        uint32 digit = pLsrUInt32FromAPInt(r);
        dDigits = pLsrSPUIntListCons(digit, dDigits);
        digits++;
        a = q;
    }

    uint32 i = 0;
    char* string = (char*) pLsrAllocC((digits + 2) * sizeof(char));

    if (PlsrAPNeg == sign) {
        string[i++] = '-';
    } else if (PlsrAPZero == sign) {
        string[i++] = '0';
    }
    while(dDigits) {
        uint32 digit = pLsrSPUIntListHd(dDigits);
        string[i++] = pLsrAPIntDecimalDigits[digit];
        dDigits = pLsrSPUIntListTl(dDigits);
    }
    string[i++] = '\0';
    return string;
}

static PlsrAPInt pLsrAPIntFromCString(char* s) {
    PlsrAPInt result = pLsrAPIntZero;
    uint32 negate = 0;
    uint32 pos = 0;

    if (s[0] == '-' || s[0] == '~') {
        negate = 1;
        pos++;
    }
    if (s[pos] == '0' && s[pos+1] == 'x') {
        PlsrAPInt sixteen = pLsrAPIntFromUInt32(16);
        pos+=2;
        while(s[pos] != '\0') {
            char c = s[pos];
            uint32 i;
            if ('0' <= c && c <= '9') {
                i = c - '0';
            } else if ('A' <= c && c <= 'F') {
                i = c - 'A' + 10;
            } else if ('a' <= c && c <= 'f') {
                i = c - 'a' + 10;
            } else {
                i = 0;
                assert(0);
            }
            PlsrAPInt digit = pLsrAPIntFromUInt32(i);
            result = pLsrAPIntMul(result, sixteen);
            result = pLsrAPIntAdd(result, digit);
            pos++;
        }
    } else {
        PlsrAPInt ten = pLsrAPIntFromUInt32(10);

        while(s[pos] != '\0') {
            char c = s[pos];
            assert('0' <= c && c <= '9');
            PlsrAPInt digit = pLsrAPIntFromUInt32(c - '0');
            result = pLsrAPIntMul(result, ten);
            result = pLsrAPIntAdd(result, digit);
            pos++;
        }
    }
    if (negate) {
        result = pLsrAPIntNegate(result);
    }
    return result;
}

/* Casts */

static sint8 pLsrAPIntCastToSInt8(PlsrAPInt a)
{
    PlsrAPSign sign = pLsrAPIntSign(a);
    if (PlsrAPZero == sign) return 0;
    uint32 i = pLsrUInt32FromSPUIntList(pLsrAPIntDigits(a));
    if (PlsrAPNeg == sign) {
        return (sint8)-((sint32)i);
    } else {
        return (sint8)(sint32)i;
    }
}

static sint16 pLsrAPIntCastToSInt16(PlsrAPInt a)
{
    PlsrAPSign sign = pLsrAPIntSign(a);
    if (PlsrAPZero == sign) return 0;
    uint32 i = pLsrUInt32FromSPUIntList(pLsrAPIntDigits(a));
    if (PlsrAPNeg == sign) {
        return (sint16)-((sint32)i);
    } else {
        return (sint16)(sint32)i;
    }
}

static sint32 pLsrAPIntCastToSInt32(PlsrAPInt a)
{
    PlsrAPSign sign = pLsrAPIntSign(a);
    if (PlsrAPZero == sign) return 0;
    uint32 i = pLsrUInt32FromSPUIntList(pLsrAPIntDigits(a));
    if (PlsrAPNeg == sign) {
        return -((sint32)i);
    } else {
        return (sint32)i;
    }
}

static sint64 pLsrAPIntCastToSInt64(PlsrAPInt a)
{
    sint64 res = 0;
    PlsrAPSign sign = pLsrAPIntSign(a);
    sint64 i = 0;

    if (PlsrAPZero != sign) {
        sintp shift = 0;
        PlsrSPUIntList digits = pLsrAPIntDigits(a);
        while(digits && shift<64) {
            i += ((sint64) pLsrSPUIntListHd(digits)) << shift;
            digits = pLsrSPUIntListTl(digits);
            shift += (sizeof(PlsrSPUInt))*CHAR_BIT;
        }
        if (PlsrAPNeg == sign) {res = -i;} else {res =  i;}
    }
    return res;
}

static uint8 pLsrAPIntCastToUInt8(PlsrAPInt a)
{
    PlsrAPSign sign = pLsrAPIntSign(a);
    if (PlsrAPZero == sign) return 0;
    uint32 i = pLsrUInt32FromSPUIntList(pLsrAPIntDigits(a));
    if (PlsrAPNeg == sign) {
        return (uint8)(uint32)-((sint32)i);
    } else {
        return (uint8)i;
    }
}

static uint16 pLsrAPIntCastToUInt16(PlsrAPInt a)
{
    PlsrAPSign sign = pLsrAPIntSign(a);
    if (PlsrAPZero == sign) return 0;
    uint32 i = pLsrUInt32FromSPUIntList(pLsrAPIntDigits(a));
    if (PlsrAPNeg == sign) {
        return (uint16)(uint32)-((sint32)i);
    } else {
        return (uint16)i;
    }
}

static uint32 pLsrAPIntCastToUInt32(PlsrAPInt a)
{
    PlsrAPSign sign = pLsrAPIntSign(a);
    if (PlsrAPZero == sign) return 0;
    uint32 i = pLsrUInt32FromSPUIntList(pLsrAPIntDigits(a));
    if (PlsrAPNeg == sign) {
        return (uint32)-((sint32)i);
    } else {
        return i;
    }
}

static uint64 pLsrAPIntCastToUInt64(PlsrAPInt a)
{
    sint64 res;
    PlsrAPSign sign = pLsrAPIntSign(a);
    sint64 i = 0;
    if (PlsrAPZero == sign) {
        res = 0;
    } else {
        sintp shift = 0;
        PlsrSPUIntList digits = pLsrAPIntDigits(a);
        while(digits && shift<64) {
            i += ((sint64) pLsrSPUIntListHd(digits)) << shift;
            digits = pLsrSPUIntListTl(digits);
            shift += (sizeof(PlsrSPUInt))*CHAR_BIT;
        }
        if (PlsrAPNeg == sign) {res = -i;} else {res =  i;}
    }
    return (uint64)res;
}

static float32 pLsrAPIntCastToFloat32(PlsrAPInt a)
{
    float32 res = 0;
    PlsrAPSign sign = pLsrAPIntSign(a);
    if (sign != PlsrAPZero) {
        sintp shift = 0;
        PlsrSPUIntList digits = pLsrAPIntDigits(a);
        while(digits) {
            res += ((float32)pLsrSPUIntListHd(digits))*(float32)pow((double)2,(double)shift);
            digits = pLsrSPUIntListTl(digits);
            shift += sizeof(PlsrSPUInt)*8;
        }
        if (sign == PlsrAPNeg) res = -res;
    }
    return res;
}

static float64 pLsrAPIntCastToFloat64(PlsrAPInt a)
{
    float64 res = 0;
    PlsrAPSign sign = pLsrAPIntSign(a);
    if (sign != PlsrAPZero) {
        sintp shift = 0;
        PlsrSPUIntList digits = pLsrAPIntDigits(a);
        while(digits) {
            res += ((float64)pLsrSPUIntListHd(digits))*pow((double)2,(double)shift);
            digits = pLsrSPUIntListTl(digits);
            shift += sizeof(PlsrSPUInt)*8;
        }
        if (sign == PlsrAPNeg) res = -res;
    }
    return res;
}

static PlsrAPInt pLsrAPIntGcd(PlsrAPInt a, PlsrAPInt b)
{
    a = pLsrAPIntAbs(a);
    b = pLsrAPIntAbs(b);
    while (!pLsrAPIntIsZero(b)) {
        PlsrAPInt r = pLsrAPIntModF(a, b);
        a = b;
        b = r;
    }
    return a;
}

static void pLsrAPIntTest() {

    { /* Test addition with the fibonacci series */
        int i;
        PlsrAPInt cur = pLsrAPIntZero;
        PlsrAPInt next = pLsrAPIntOne;
        for(i=0;i<100;i++) {
            PlsrAPInt nextnext = pLsrAPIntAdd(cur, next);
            char* curS = pLsrCStringFromAPInt(cur);
            char* nextS = pLsrCStringFromAPInt(next);
            printf("fib of %d = %s, next is %s\n", i, curS, nextS);
            cur = next;
            next = nextnext;
            pLsrFreeC(curS);
            pLsrFreeC(nextS);
        }
    }
    { /* Test multiplication with factorial */
        int i;
        PlsrAPInt cur = pLsrAPIntOne;
        PlsrAPInt I = pLsrAPIntZero;
        for(i=0;i<100;i++) {
            char* curS = pLsrCStringFromAPInt(cur);
            printf("fact of %d = %s\n", i, curS);
            I = pLsrAPIntAdd(I, pLsrAPIntOne);
            cur = pLsrAPIntMul(I, cur);
            pLsrFreeC(curS);
        }
    }

}


#define pLsrSPUIntListSize (sizeof(PlsrSPUIntListS))
#define pLsrAPIntSize (sizeof(PlsrAPIntS))

#define pLsrSPUIntListAlignment 4
#define pLsrAPIntAlignment 4

static void pLsrAPIntRegisterVTables()
{
    static PgcIsRef pLsrSPUIntListRefs[pLsrSPUIntListSize/P_WORD_SIZE] = { 0, 0, 1};
    static PgcIsRef pLsrAPIntRefs[pLsrAPIntSize/P_WORD_SIZE] = { 0, 0, 1};


    assert(pLsrSPUIntListSize/P_WORD_SIZE == 3);
    assert(pLsrAPIntSize/P_WORD_SIZE == 3);


    pLsrVTableRegister(pLsrSPUIntListVTable, pLsrSPUIntListAlignment, pLsrSPUIntListSize,
                       pLsrSPUIntListRefs, 0, 0, 0, PGC_ALWAYS_IMMUTABLE, 0);
    pLsrVTableRegister(pLsrAPIntVTable, pLsrAPIntAlignment, pLsrAPIntSize, pLsrAPIntRefs, 0, 0, 0,
                       PGC_ALWAYS_IMMUTABLE, 0);
}

#define pLsrAPIntGlobalsCount 6

static PlsrObjectB pLsrAPIntGlobals[] =
    {
        (PlsrObjectB) pLsrSPUIntListOne,
        (PlsrObjectB) pLsrSPUIntListUInt32Max,
        (PlsrObjectB) pLsrAPIntZero,
        (PlsrObjectB) pLsrAPIntOne,
        (PlsrObjectB) pLsrAPIntMinusOne,
        (PlsrObjectB) pLsrAPIntMinusUInt32Max,
        (PlsrObjectB) NULL /* This must be last */
    };

static void pLsrAPIntRegisterGlobals() {
    assert(pLsrAPIntGlobals[pLsrAPIntGlobalsCount] == NULL);
    pLsrGcRegisterGlobals (pLsrAPIntGlobals, pLsrAPIntGlobalsCount);
};

static void pLsrAPIntInitialize(uintp memLimit)
{
}

#define hashPair(h1, h2) ((h1)+((h2)<<5)+(h2)+720)

static uintp pLsrAPIntHash(PlsrAPInt i)
{
    uintp h;
    PlsrSPUIntList ds;
    switch pLsrAPIntSign(i) {
    case PlsrAPNeg: h = 987; break;
    case PlsrAPPos: h = 0; break;
    case PlsrAPZero: return 0;
    }
    for(ds = pLsrAPIntDigits(i); ds; ds = pLsrSPUIntListTl(ds)) h = hashPair(h, pLsrSPUIntHash(pLsrSPUIntListHd(ds)));
    return h;
}

/* This function must match the previous one */
static uintp pLsrSInt32Hash(sint32 i)
{
    uintp h;
    if (i==0) return 0;
    if (i<0) { i = -i; h = 987; } else { h = 0; }
    return hashPair(h, pLsrSPUIntHash(i));
}

/* Static global construction */

#define pLsrAPIntDigitListStaticEmpty pLsrSPUIntListEmpty

/* Define flattened initializers to work around a pillar compiler bug.  */
#define pLsrAPIntDigitListStaticConsUnboxedDef(var, digit1, digits)    \
    static PlsrSPUIntListS var = {.vtable = pLsrSPUIntListVTable, .digit = digit1, .next = digits}
#define pLsrAPIntDigitListStaticConsRef(uvar) \
    ((PlsrSPUIntList) & uvar)
#define pLsrAPIntStaticUnboxedDef(var, sign1, digits1)                 \
    static PlsrAPIntS var = {.vtable = pLsrAPIntVTable, .sign = sign1, .digits = digits1}

#ifdef PLSR_AP_INT_TRACE
static void pLsrAPIntShow1(char * s, PlsrAPInt i)
{
    char* res = pLsrCStringFromAPInt(i);
    printf("APInt: %s, %s\n", s, res);
    pLsrFreeC(res);
}
static void pLsrAPIntShow2(char * s, PlsrAPInt i1, PlsrAPInt i2)
{
    char* res1 = pLsrCStringFromAPInt(i1);
    char* res2 = pLsrCStringFromAPInt(i2);
    printf("APInt: %s, (%s, %s)\n", s, res1, res2);
    pLsrFreeC(res1);
    pLsrFreeC(res2);
}
static void pLsrAPIntShow3(char * s, PlsrAPInt i1, PlsrAPInt i2, PlsrAPInt i3)
{
    char* res1 = pLsrCStringFromAPInt(i1);
    char* res2 = pLsrCStringFromAPInt(i2);
    char* res3 = pLsrCStringFromAPInt(i3);
    printf("APInt: %s, (%s, %s, %s)\n", s, res1, res2, res3);
    pLsrFreeC(res1);
    pLsrFreeC(res2);
    pLsrFreeC(res3);
}
static void pLsrAPIntShow4(char * s, PlsrAPInt i1, PlsrAPInt i2, PlsrAPInt i3, PlsrAPInt i4)
{
    char* res1 = pLsrCStringFromAPInt(i1);
    char* res2 = pLsrCStringFromAPInt(i2);
    char* res3 = pLsrCStringFromAPInt(i3);
    char* res4 = pLsrCStringFromAPInt(i4);
    printf("APInt: %s, (%s, %s, %s, %s)\n", s, res1, res2, res3, res4);
    pLsrFreeC(res1);
    pLsrFreeC(res2);
    pLsrFreeC(res3);
    pLsrFreeC(res4);
}
#endif

#endif /*_PLSR_FLRC_INTEGER_H_ */
