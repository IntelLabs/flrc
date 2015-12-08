/* -----------------------------------------------------------------------------
 *
 * (c) Lennart Augustsson
 * (c) The GHC Team, 1998-2000
 *
 * Miscellaneous support for floating-point primitives
 *
 * ---------------------------------------------------------------------------*/

#ifndef _GHC_FLOAT_H_
#define _GHC_FLOAT_H_

#include <assert.h>
#include <math.h>
#include <float.h>
#include "hrc/pil.h"

#define IEEE_FLOATING_POINT 1

/*
 * Encoding and decoding Doubles.  Code based on the HBC code
 * (lib/fltcode.c).
 */

#if IEEE_FLOATING_POINT
#define MY_DMINEXP  ((DBL_MIN_EXP) - (DBL_MANT_DIG) - 1)
/* DMINEXP is defined in values.h on Linux (for example) */
#define DHIGHBIT 0x00100000
#define DMSBIT   0x80000000

#define MY_FMINEXP  ((FLT_MIN_EXP) - (FLT_MANT_DIG) - 1)
#define FHIGHBIT 0x00800000
#define FMSBIT   0x80000000
#endif

#if defined(WORDS_BIGENDIAN) || defined(FLOAT_WORDS_BIGENDIAN)
#define L 1
#define H 0
#else
#define L 0
#define H 1
#endif

#define __abs(a)		(( (a) >= 0 ) ? (a) : (-(a)))

typedef void* StgStablePtr;
#define ASSERT assert
//typedef uint32 nat;
typedef float StgFloat;
typedef double StgDouble;
typedef sintp I_;
typedef uintp W_;
#define SIZEOF_FLOAT 4
#define SIZEOF_DOUBLE 8

StgDouble __2Int_encodeDouble (I_ j_high, I_ j_low, I_ e);
StgDouble __word_encodeDouble (W_ j, I_ e);
StgDouble __int_encodeDouble (I_ j, I_ e);
StgFloat __int_encodeFloat (I_ j, I_ e);
StgFloat __word_encodeFloat (W_ j, I_ e);
void __decodeDouble_2Int (I_ *man_sign, W_ *man_high, W_ *man_low, I_ *exp, StgDouble dbl);
void __decodeFloat_Int (I_ *man, I_ *exp, StgFloat flt);

#endif
