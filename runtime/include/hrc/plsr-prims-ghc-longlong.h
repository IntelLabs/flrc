/* The Haskell Research Compiler */
/*
 * Redistribution and use in source and binary forms, with or without modification, are permitted 
 * provided that the following conditions are met:
 * 1.   Redistributions of source code must retain the above copyright notice, this list of 
 * conditions and the following disclaimer.
 * 2.   Redistributions in binary form must reproduce the above copyright notice, this list of
 * conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 * BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef _PLSR_PRIMS_GHC_LONGLONG_H_
#define _PLSR_PRIMS_GHC_LONGLONG_H_

/* get type definitions from pil.h */
#include "hrc/pil.h"

#define hs_gtWord64(a,b) ((a)> (b))
#define hs_geWord64(a,b) ((a)>=(b))
#define hs_eqWord64(a,b) ((a)==(b))
#define hs_neWord64(a,b) ((a)!=(b))
#define hs_ltWord64(a,b) ((a)< (b))
#define hs_leWord64(a,b) ((a)<=(b))
#define hs_gtInt64(a,b) ((a)> (b))
#define hs_geInt64(a,b) ((a)>=(b))
#define hs_eqInt64(a,b) ((a)==(b))
#define hs_neInt64(a,b) ((a)!=(b))
#define hs_ltInt64(a,b) ((a)< (b))
#define hs_leInt64(a,b) ((a)<=(b))

#define hs_remWord64(a,b)  ((a)% (b))
#define hs_quotWord64(a,b) ((a)/ (b))

#define hs_remInt64(a,b)   ((a)% (b))
#define hs_quotInt64(a,b)  ((a)/ (b))
#define hs_negateInt64(a)  (-(a))
#define hs_plusInt64(a,b)  ((a)+ (b))
#define hs_minusInt64(a,b) ((a)- (b))
#define hs_timesInt64(a,b) ((a)* (b))

#define hs_and64(a,b) ((a)& (b))
#define hs_or64(a,b)  ((a)| (b))
#define hs_xor64(a,b) ((a)^ (b))
#define hs_not64(a)   ( ~(a))

#define hs_uncheckedShiftL64(a,b)   ((a)<< (b))
#define hs_uncheckedShiftRL64(a,b)  ((a)>> (b))
#define hs_uncheckedIShiftL64(a,b)  ((a)<< (b))
#define hs_uncheckedIShiftRA64(a,b) ((a)>> (b))
#define hs_uncheckedIShiftRL64(a,b) ((sint64)((uint64)(a)>>(b)))

#define hs_intToInt64(i)    ((sint64) (i))
#define hs_int64ToInt(i)    ((sintp)  (i))
#define hs_int64ToWord64(i) ((uint64) (i))
#define hs_wordToWord64(w)  ((uint64) (w))
#define hs_word64ToWord(w)  ((uintp)  (w))
#define hs_word64ToInt64(w) ((sint64) (w))

#endif
