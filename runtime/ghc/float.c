/* The following double/float decoding code is taken from GHC */
#include <float.h>
#define SIZEOF_DOUBLE 8
#define MY_DMINEXP  ((DBL_MIN_EXP) - (DBL_MANT_DIG) - 1)
/* DMINEXP is defined in values.h on Linux (for example) */
#define DHIGHBIT 0x00100000
#define DMSBIT   0x80000000

#define MY_FMINEXP  ((FLT_MIN_EXP) - (FLT_MANT_DIG) - 1)
#define FHIGHBIT 0x00800000
#define FMSBIT   0x80000000
#define L 0
#define H 1
static void
__decodeDouble_2Int (sint32 *man_sign, uint32 *man_high, uint32 *man_low, sint32 *exp, double dbl)
{
    /* Do some bit fiddling on IEEE */
    unsigned int low, high; 	     	/* assuming 32 bit ints */
    int sign, iexp;
    union { double d; unsigned int i[2]; } u;	/* assuming 32 bit ints, 64 bit double */

    u.d = dbl;	    /* grab chunks of the double */
    low = u.i[L];
    high = u.i[H];

    if (low == 0 && (high & ~DMSBIT) == 0) {
	*man_low = 0;
	*man_high = 0;
	*exp = 0L;
    } else {
	iexp = ((high >> 20) & 0x7ff) + MY_DMINEXP;
	sign = high;

	high &= DHIGHBIT-1;
	if (iexp != MY_DMINEXP)	/* don't add hidden bit to denorms */
	    high |= DHIGHBIT;
	else {
	    iexp++;
	    /* A denorm, normalize the mantissa */
	    while (! (high & DHIGHBIT)) {
		high <<= 1;
		if (low & DMSBIT)
		    high++;
		low <<= 1;
		iexp--;
	    }
	}
        *exp = (sint32) iexp;
	*man_low = low;
	*man_high = high;
	*man_sign = (sign < 0) ? -1 : 1;
    }
}

static void
__decodeFloat_Int (sint32 *man, sint32 *exp, float flt)
{
    /* Do some bit fiddling on IEEE */
    int high, sign; 	    	    /* assuming 32 bit ints */
    union { float f; int i; } u;    /* assuming 32 bit float and int */

    u.f = flt;	    /* grab the float */
    high = u.i;

    if ((high & ~FMSBIT) == 0) {
	*man = 0;
	*exp = 0;
    } else {
	*exp = ((high >> 23) & 0xff) + MY_FMINEXP;
	sign = high;

	high &= FHIGHBIT-1;
	if (*exp != MY_FMINEXP)	/* don't add hidden bit to denorms */
	    high |= FHIGHBIT;
	else {
	    (*exp)++;
	    /* A denorm, normalize the mantissa */
	    while (! (high & FHIGHBIT)) {
		high <<= 1;
		(*exp)--;
	    }
	}
	*man = high;
	if (sign < 0)
	    *man = - *man;
    }
}


