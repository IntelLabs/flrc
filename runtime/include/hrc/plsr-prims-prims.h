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

#ifndef _PLSR_PRIMS_PRIMS_H_
#define _PLSR_PRIMS_PRIMS_H_

/**********************************************************************
 * Unboxed Machine Integers and Floating-Point
 */

#define binArith(t, oper, dest, a, b)           \
    ((dest) = ((t) ((t) a) oper ((t) b)))

#define binArithPrefix(t, oper, dest, a, b)           \
  ((dest) = ((t) (oper ((t) a, (t) b))))
    
#define unArith(t, oper, dest, a)               \
    ((dest) = ((t) (oper ((t) a))))

#define binPred(t, oper, dest, a, b)                    \
    ((dest) = ((PlsrBoolean) (((t) a) oper ((t) b))))

#define pLsrPrimNumericConv(dest, tTo, a)       \
    ((dest) = (tTo) (a))

/* SInt8 ops */
#define pLsrPrimPSInt8Plus(dest, a, b)   binArith(sint8, +, dest, a, b)   
#define pLsrPrimPSInt8Minus(dest, a, b)  binArith(sint8, -, dest, a, b)      
#define pLsrPrimPSInt8Times(dest, a, b)  binArith(sint8, *, dest, a, b)      
#define pLsrPrimPSInt8DivT(dest, a, b)   binArith(sint8, /, dest, a, b)
#define pLsrPrimPSInt8ModT(dest, a, b)   binArith(sint8, %, dest, a, b)
#define pLsrPrimPSInt8Negate(dest, a)    unArith(sint8, -, dest, a)

#define pLsrPrimPSInt8EQ(dest, a, b) binPred(sint8, ==, dest, a, b)     
#define pLsrPrimPSInt8NE(dest, a, b) binPred(sint8, !=, dest, a, b)     
#define pLsrPrimPSInt8LT(dest, a, b) binPred(sint8, <, dest, a, b)      
#define pLsrPrimPSInt8LE(dest, a, b) binPred(sint8, <=, dest, a, b)

#define pLsrPrimPSInt8FromSInt8(dest, a)   pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPSInt8FromSInt16(dest, a)  pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPSInt8FromSInt32(dest, a)  pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPSInt8FromSInt64(dest, a)  pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPSInt8FromUInt8(dest, a)   pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPSInt8FromUInt16(dest, a)  pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPSInt8FromUInt32(dest, a)  pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPSInt8FromUInt64(dest, a)  pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPSInt8FromFloat32(dest, a) pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPSInt8FromFloat64(dest, a) pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPSInt8FromRational(dest, a) pLsrSInt8FromRational(dest, a)
#define pLsrPrimPSInt8FromInteger(dest, a) pLsrSInt8FromInteger(dest, a)

#define pLsrPrimPSInt8CastSInt8(dest, a)   pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPSInt16CastSInt8(dest, a)  pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPSInt32CastSInt8(dest, a)  pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPSInt64CastSInt8(dest, a)  pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPUInt8CastSInt8(dest, a)   pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPUInt16CastSInt8(dest, a)  pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPUInt32CastSInt8(dest, a)  pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPUInt64CastSInt8(dest, a)  pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPFloat32CastSInt8(dest, a) pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPFloat64CastSInt8(dest, a) pLsrPrimNumericConv(dest, sint8, a)
#define pLsrPrimPIntegerCastSInt8(dest, a) pLsrIntegerCastToSInt8(dest, a)

/* SInt16 ops */
#define pLsrPrimPSInt16Plus(dest, a, b)   binArith(sint16, +, dest, a, b)   
#define pLsrPrimPSInt16Minus(dest, a, b)  binArith(sint16, -, dest, a, b)      
#define pLsrPrimPSInt16Times(dest, a, b)  binArith(sint16, *, dest, a, b)      
#define pLsrPrimPSInt16DivT(dest, a, b)   binArith(sint16, /, dest, a, b)
#define pLsrPrimPSInt16ModT(dest, a, b)   binArith(sint16, %, dest, a, b)
#define pLsrPrimPSInt16Negate(dest, a)    unArith(sint16, -, dest, a)

#define pLsrPrimPSInt16EQ(dest, a, b) binPred(sint16, ==, dest, a, b)     
#define pLsrPrimPSInt16NE(dest, a, b) binPred(sint16, !=, dest, a, b)     
#define pLsrPrimPSInt16LT(dest, a, b) binPred(sint16, <, dest, a, b)      
#define pLsrPrimPSInt16LE(dest, a, b) binPred(sint16, <=, dest, a, b)

#define pLsrPrimPSInt16FromSInt8(dest, a) pLsrPrimNumericConv(dest, sint16, a)
#define pLsrPrimPSInt16FromSInt16(dest, a) pLsrPrimNumericConv(dest, sint16, a)
#define pLsrPrimPSInt16FromSInt32(dest, a) pLsrPrimNumericConv(dest, sint16, a)
#define pLsrPrimPSInt16FromSInt64(dest, a) pLsrPrimNumericConv(dest, sint16, a
#define pLsrPrimPSInt16FromUInt8(dest, a) pLsrPrimNumericConv(dest, sint16, a)
#define pLsrPrimPSInt16FromUInt16(dest, a) pLsrPrimNumericConv(dest, sint16, a)
#define pLsrPrimPSInt16FromUInt32(dest, a) pLsrPrimNumericConv(dest, sint16, a)
#define pLsrPrimPSInt16FromUInt64(dest, a) pLsrPrimNumericConv(dest, sint16, a)
#define pLsrPrimPSInt16FromFloat32(dest, a) pLsrPrimNumericConv(dest, sint16, a)
#define pLsrPrimPSInt16FromFloat64(dest, a) pLsrPrimNumericConv(dest, sint16, a)
#define pLsrPrimPSInt16FromRational(dest, a) pLsrSInt16FromRational(dest, a)
#define pLsrPrimPSInt16FromInteger(dest, a) pLsrSInt16FromInteger(dest, a)

#define pLsrPrimPSInt8CastSInt16(dest, a) pLsrPrimNumericConv(dest, sint16, a)
#define pLsrPrimPSInt16CastSInt16(dest, a) pLsrPrimNumericConv(dest, sint16, a)
#define pLsrPrimPSInt32CastSInt16(dest, a) pLsrPrimNumericConv(dest, sint16, a)
#define pLsrPrimPSInt64CastSInt16(dest, a) pLsrPrimNumericConv(dest, sint16, a)
#define pLsrPrimPUInt8CastSInt16(dest, a) pLsrPrimNumericConv(dest, sint16, a)
#define pLsrPrimPUInt16CastSInt16(dest, a) pLsrPrimNumericConv(dest, sint16, a)
#define pLsrPrimPUInt32CastSInt16(dest, a) pLsrPrimNumericConv(dest, sint16, a)
#define pLsrPrimPUInt64CastSInt16(dest, a) pLsrPrimNumericConv(dest, sint16, a)
#define pLsrPrimPFloat32CastSInt16(dest, a) pLsrPrimNumericConv(dest, sint16, a)
#define pLsrPrimPFloat64CastSInt16(dest, a) pLsrPrimNumericConv(dest, sint16, a)
#define pLsrPrimPIntegerCastSInt16(dest, a) pLsrIntegerCastToSInt16(dest, a)

/* SInt32 ops */
#define pLsrPrimPSInt32Plus(dest, a, b)   binArith(sint32, +, dest, a, b)   
#define pLsrPrimPSInt32Minus(dest, a, b)  binArith(sint32, -, dest, a, b)      
#define pLsrPrimPSInt32Times(dest, a, b)  binArith(sint32, *, dest, a, b)      
#define pLsrPrimPSInt32DivT(dest, a, b)    binArith(sint32, /, dest, a, b)
#define pLsrPrimPSInt32ModT(dest, a, b)    binArith(sint32, %, dest, a, b)
#define pLsrPrimPSInt32DivModT(dest1, dest2, a, b) pLsrSInt32DivModT(dest1, dest2, a, b)
#define pLsrPrimPSInt32DivModE(dest1, dest2, a, b) pLsrSInt32DivModE(dest1, dest2, a, b)
#define pLsrPrimPSInt32DivModF(dest1, dest2, a, b) pLsrSInt32DivModF(dest1, dest2, a, b)
#define pLsrPrimPSInt32Negate(dest, a)    unArith(sint32, -, dest, a)

#define pLsrSInt32DivModT(dest1, dest2, a, b)   \
    do {                                        \
        div_t q = div (a, b);                   \
        dest1 = q.quot;                      \
        dest2 = q.rem;                       \
    } while (0)

#define pLsrPrimPSInt32EQ(dest, a, b) binPred(sint32, ==, dest, a, b)     
#define pLsrPrimPSInt32NE(dest, a, b) binPred(sint32, !=, dest, a, b)     
#define pLsrPrimPSInt32LT(dest, a, b) binPred(sint32, <, dest, a, b)      
#define pLsrPrimPSInt32LE(dest, a, b) binPred(sint32, <=, dest, a, b)

#define pLsrPrimPSInt32FromSInt8(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPSInt32FromSInt16(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPSInt32FromSInt32(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPSInt32FromSInt64(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPSInt32FromUInt8(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPSInt32FromUInt16(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPSInt32FromUInt32(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPSInt32FromUInt64(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPSInt32FromFloat32(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPSInt32FromFloat64(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPSInt32FromRational(dest, a) pLsrSInt32FromRational(dest, a)
#define pLsrPrimPSInt32FromInteger(dest, a) pLsrSInt32FromInteger(dest, a)

#define pLsrPrimPSInt8CastSInt32(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPSInt16CastSInt32(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPSInt32CastSInt32(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPSInt64CastSInt32(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPUInt8CastSInt32(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPUInt16CastSInt32(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPUInt32CastSInt32(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPUInt64CastSInt32(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPFloat32CastSInt32(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPFloat64CastSInt32(dest, a) pLsrPrimNumericConv(dest, sint32, a)
#define pLsrPrimPIntegerCastSInt32(dest, a) pLsrIntegerCastToSInt32(dest, a)

/* SInt64 ops */
#define pLsrPrimPSInt64Plus(dest, a, b)   binArith(sint64, +, dest, a, b)   
#define pLsrPrimPSInt64Minus(dest, a, b)  binArith(sint64, -, dest, a, b)      
#define pLsrPrimPSInt64Times(dest, a, b)  binArith(sint64, *, dest, a, b)      
#define pLsrPrimPSInt64DivT(dest, a, b)    binArith(sint64, /, dest, a, b)
#define pLsrPrimPSInt64ModT(dest, a, b)    binArith(sint64, %, dest, a, b)
#define pLsrPrimPSInt64DivModT(dest1, dest2, a, b) pLsrSInt64DivModT(dest1, dest2, a, b)
#define pLsrPrimPSInt64DivModE(dest1, dest2, a, b) pLsrSInt64DivModE(dest1, dest2, a, b)
#define pLsrPrimPSInt64DivModF(dest1, dest2, a, b) pLsrSInt64DivModF(dest1, dest2, a, b)
#define pLsrPrimPSInt64Negate(dest, a)    unArith(sint64, -, dest, a)

#define pLsrSInt64DivModT(dest1, dest2, a, b)                           \
    do {                                                                \
        lldiv_t q = lldiv (a, b);                                       \
        dest1 = q.quot;                                                 \
        dest2 = q.rem;                                                  \
    } while (0)

#define pLsrPrimPSInt64EQ(dest, a, b) binPred(sint64, ==, dest, a, b)     
#define pLsrPrimPSInt64NE(dest, a, b) binPred(sint64, !=, dest, a, b)     
#define pLsrPrimPSInt64LT(dest, a, b) binPred(sint64, <, dest, a, b)      
#define pLsrPrimPSInt64LE(dest, a, b) binPred(sint64, <=, dest, a, b)

#define pLsrPrimPSInt64FromSInt8(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPSInt64FromSInt16(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPSInt64FromSInt32(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPSInt64FromSInt64(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPSInt64FromUInt8(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPSInt64FromUInt16(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPSInt64FromUInt32(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPSInt64FromUInt64(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPSInt64FromFloat32(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPSInt64FromFloat64(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPSInt64FromRational(dest, a) pLsrSInt64FromRational(dest, a)
#define pLsrPrimPSInt64FromInteger(dest, a) pLsrSInt64FromInteger(dest, a)

#define pLsrPrimPSInt8CastSInt64(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPSInt16CastSInt64(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPSInt32CastSInt64(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPSInt64CastSInt64(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPUInt8CastSInt64(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPUInt16CastSInt64(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPUInt32CastSInt64(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPUInt64CastSInt64(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPFloat32CastSInt64(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPFloat64CastSInt64(dest, a) pLsrPrimNumericConv(dest, sint64, a)
#define pLsrPrimPIntegerCastSInt64(dest, a) pLsrIntegerCastToSInt64(dest, a)

/* UInt8 ops */
#define pLsrPrimPUInt8Plus(dest, a, b)   binArith(uint8, +, dest, a, b)   
#define pLsrPrimPUInt8Minus(dest, a, b)  binArith(uint8, -, dest, a, b)      
#define pLsrPrimPUInt8Times(dest, a, b)  binArith(uint8, *, dest, a, b)      
#define pLsrPrimPUInt8DivT(dest, a, b)    binArith(uint8, /, dest, a, b)
#define pLsrPrimPUInt8ModT(dest, a, b)    binArith(uint8, %, dest, a, b)
#define pLsrPrimPUInt8Negate(dest, a)    unArith(uint8, -, dest, a)

#define pLsrPrimPUInt8EQ(dest, a, b) binPred(uint8, ==, dest, a, b)     
#define pLsrPrimPUInt8NE(dest, a, b) binPred(uint8, !=, dest, a, b)     
#define pLsrPrimPUInt8LT(dest, a, b) binPred(uint8, <, dest, a, b)      
#define pLsrPrimPUInt8LE(dest, a, b) binPred(uint8, <=, dest, a, b)

#define pLsrPrimPUInt8FromSInt8(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPUInt8FromSInt16(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPUInt8FromSInt32(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPUInt8FromSInt64(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPUInt8FromUInt8(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPUInt8FromUInt16(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPUInt8FromUInt32(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPUInt8FromUInt64(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPUInt8FromFloat32(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPUInt8FromFloat64(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPUInt8FromRational(dest, a) pLsrUInt8FromRational(dest, a)
#define pLsrPrimPUInt8FromInteger(dest, a) pLsrUInt8FromInteger(dest, a)

#define pLsrPrimPSInt8CastUInt8(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPSInt16CastUInt8(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPSInt32CastUInt8(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPSInt64CastUInt8(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPUInt8CastUInt8(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPUInt16CastUInt8(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPUInt32CastUInt8(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPUInt64CastUInt8(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPFloat32CastUInt8(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPFloat64CastUInt8(dest, a) pLsrPrimNumericConv(dest, uint8, a)
#define pLsrPrimPIntegerCastUInt8(dest, a) pLsrIntegerCastToUInt8(dest, a)

/* UInt16 ops */
#define pLsrPrimPUInt16Plus(dest, a, b)   binArith(uint16, +, dest, a, b)   
#define pLsrPrimPUInt16Minus(dest, a, b)  binArith(uint16, -, dest, a, b)      
#define pLsrPrimPUInt16Times(dest, a, b)  binArith(uint16, *, dest, a, b)      
#define pLsrPrimPUInt16DivT(dest, a, b)    binArith(uint16, /, dest, a, b)
#define pLsrPrimPUInt16ModT(dest, a, b)    binArith(uint16, %, dest, a, b)
#define pLsrPrimPUInt16Negate(dest, a)    unArith(uint16, -, dest, a)

#define pLsrPrimPUInt16EQ(dest, a, b) binPred(uint16, ==, dest, a, b)     
#define pLsrPrimPUInt16NE(dest, a, b) binPred(uint16, !=, dest, a, b)     
#define pLsrPrimPUInt16LT(dest, a, b) binPred(uint16, <, dest, a, b)      
#define pLsrPrimPUInt16LE(dest, a, b) binPred(uint16, <=, dest, a, b)

#define pLsrPrimPUInt16FromSInt8(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPUInt16FromSInt16(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPUInt16FromSInt32(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPUInt16FromSInt64(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPUInt16FromUInt8(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPUInt16FromUInt16(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPUInt16FromUInt32(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPUInt16FromUInt64(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPUInt16FromFloat32(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPUInt16FromFloat64(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPUInt16FromRational(dest, a) pLsrUInt16FromRational(dest, a)
#define pLsrPrimPUInt16FromInteger(dest, a) pLsrUInt16FromInteger(dest, a)

#define pLsrPrimPSInt8CastUInt16(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPSInt16CastUInt16(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPSInt32CastUInt16(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPSInt64CastUInt16(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPUInt8CastUInt16(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPUInt16CastUInt16(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPUInt32CastUInt16(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPUInt64CastUInt16(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPFloat32CastUInt16(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPFloat64CastUInt16(dest, a) pLsrPrimNumericConv(dest, uint16, a)
#define pLsrPrimPIntegerCastUInt16(dest, a) pLsrIntegerCastToUInt16(dest, a)

/* UInt32 ops */
#define pLsrPrimPUInt32Plus(dest, a, b)   binArith(uint32, +, dest, a, b)   
#define pLsrPrimPUInt32Minus(dest, a, b)  binArith(uint32, -, dest, a, b)      
#define pLsrPrimPUInt32Times(dest, a, b)  binArith(uint32, *, dest, a, b)      
#define pLsrPrimPUInt32DivT(dest, a, b)    binArith(uint32, /, dest, a, b)
#define pLsrPrimPUInt32ModT(dest, a, b)    binArith(uint32, %, dest, a, b)
#define pLsrPrimPUInt32DivModT(dest1, dest2, a, b) pLsrUInt32DivModT(dest1, dest2, a, b)
#define pLsrPrimPUInt32DivModE(dest1, dest2, a, b) pLsrUInt32DivModE(dest1, dest2, a, b)
#define pLsrPrimPUInt32DivModF(dest1, dest2, a, b) pLsrUInt32DivModF(dest1, dest2, a, b)
#define pLsrPrimPUInt32Negate(dest, a)    unArith(uint32, -, dest, a)
#define pLsrPrimPUInt32Max(dest, a, b)  binArithPrefix(uint32, max, dest, a, b)
#define pLsrPrimPUInt32Min(dest, a, b)  binArithPrefix(uint32, min, dest, a, b)

#define pLsrUInt32DivModT(dest1, dest2, a, b)   \
    do {                                        \
        dest1 = (a)/(b);                        \
        dest2 = (a)%(b);                        \
    } while (0)

#define pLsrPrimPUInt32EQ(dest, a, b) binPred(uint32, ==, dest, a, b)     
#define pLsrPrimPUInt32NE(dest, a, b) binPred(uint32, !=, dest, a, b)     
#define pLsrPrimPUInt32LT(dest, a, b) binPred(uint32, <, dest, a, b)      
#define pLsrPrimPUInt32LE(dest, a, b) binPred(uint32, <=, dest, a, b)

#define pLsrPrimPUInt32FromSInt8(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPUInt32FromSInt16(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPUInt32FromSInt32(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPUInt32FromSInt64(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPUInt32FromUInt8(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPUInt32FromUInt16(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPUInt32FromUInt32(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPUInt32FromUInt64(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPUInt32FromFloat32(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPUInt32FromFloat64(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPUInt32FromRational(dest, a) pLsrUInt32FromRational(dest, a)
#define pLsrPrimPUInt32FromInteger(dest, a) pLsrUInt32FromInteger(dest, a)

#define pLsrPrimPSInt8CastUInt32(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPSInt16CastUInt32(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPSInt32CastUInt32(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPSInt64CastUInt32(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPUInt8CastUInt32(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPUInt16CastUInt32(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPUInt32CastUInt32(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPUInt64CastUInt32(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPFloat32CastUInt32(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPFloat64CastUInt32(dest, a) pLsrPrimNumericConv(dest, uint32, a)
#define pLsrPrimPIntegerCastUInt32(dest, a) pLsrIntegerCastToUInt32(dest, a)

#define pLsrPrimPUInt32BNot(dest, b) ((dest) = ~(b)) 
#define pLsrPrimPUInt32BOr(dest, a, b) binArith(uint32, |, dest, a, b)
#define pLsrPrimPSInt32BOr(dest, a, b) binArith(sint32, |, dest, a, b)
#define pLsrPrimPUInt32BAnd(dest, a, b) binArith(uint32, &, dest, a, b)
#define pLsrPrimPSInt32BAnd(dest, a, b) binArith(sint32, &, dest, a, b)
#define pLsrPrimPUInt32BXor(dest, a, b) binArith(uint32, ^, dest, a, b)
#define pLsrPrimPSInt32BXor(dest, a, b) binArith(sint32, ^, dest, a, b)
#define pLsrPrimPSInt32BShiftL(dest, a, b) binArith(sint32, <<, dest, a, b)
/*Note: implementation defined on negative values */
#define pLsrPrimPSInt32BShiftR(dest, a, b) binArith(sint32, >>, dest, a, b)
#define pLsrPrimPUInt32BShiftL(dest, a, b) binArith(uint32, <<, dest, a, b)
#define pLsrPrimPSInt32BShiftR(dest, a, b) binArith(sint32, >>, dest, a, b)
#define pLsrPrimPUInt32BShiftR(dest, a, b) binArith(uint32, >>, dest, a, b)

/* UInt64 ops */
#define pLsrPrimPUInt64Plus(dest, a, b)   binArith(uint64, +, dest, a, b)   
#define pLsrPrimPUInt64Minus(dest, a, b)  binArith(uint64, -, dest, a, b)      
#define pLsrPrimPUInt64Times(dest, a, b)  binArith(uint64, *, dest, a, b)      
#define pLsrPrimPUInt64DivT(dest, a, b)   binArith(uint64, /, dest, a, b)
#define pLsrPrimPUInt64ModT(dest, a, b)   binArith(uint64, %, dest, a, b)
#define pLsrPrimPUInt64Negate(dest, a)    unArith(uint64, -, dest, a)

#define pLsrPrimPUInt64EQ(dest, a, b) binPred(uint64, ==, dest, a, b)     
#define pLsrPrimPUInt64NE(dest, a, b) binPred(uint64, !=, dest, a, b)     
#define pLsrPrimPUInt64LT(dest, a, b) binPred(uint64, <, dest, a, b)      
#define pLsrPrimPUInt64LE(dest, a, b) binPred(uint64, <=, dest, a, b)

#define pLsrPrimPUInt64FromSInt8(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPUInt64FromSInt16(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPUInt64FromSInt32(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPUInt64FromSInt64(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPUInt64FromUInt8(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPUInt64FromUInt16(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPUInt64FromUInt32(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPUInt64FromUInt64(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPUInt64FromFloat32(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPUInt64FromFloat64(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPUInt64FromRational(dest, a) pLsrUInt64FromRational(dest, a)
#define pLsrPrimPUInt64FromInteger(dest, a) pLsrUInt64FromInteger(dest, a)

#define pLsrPrimPSInt8CastUInt64(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPSInt16CastUInt64(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPSInt32CastUInt64(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPSInt64CastUInt64(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPUInt8CastUInt64(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPUInt16CastUInt64(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPUInt32CastUInt64(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPUInt64CastUInt64(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPFloat32CastUInt64(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPFloat64CastUInt64(dest, a) pLsrPrimNumericConv(dest, uint64, a)
#define pLsrPrimPIntegerCastUInt64(dest, a) pLsrIntegerCastToUInt64(dest, a)

#define pLsrPrimPUInt64BNot(dest, b) ((dest) = ~(b)) 
#define pLsrPrimPUInt64BOr(dest, a, b) binArith(uint64, |, dest, a, b)
#define pLsrPrimPSInt64BOr(dest, a, b) binArith(sint64, |, dest, a, b)
#define pLsrPrimPUInt64BAnd(dest, a, b) binArith(uint64, &, dest, a, b)
#define pLsrPrimPSInt64BAnd(dest, a, b) binArith(sint64, &, dest, a, b)
#define pLsrPrimPUInt64BXor(dest, a, b) binArith(uint64, ^, dest, a, b)
#define pLsrPrimPSInt64BXor(dest, a, b) binArith(sint64, ^, dest, a, b)
#define pLsrPrimPSInt64BShiftL(dest, a, b) binArith(sint64, <<, dest, a, b)
/*Note: implementation defined on negative values */
#define pLsrPrimPSInt64BShiftR(dest, a, b) binArith(sint64, >>, dest, a, b)
#define pLsrPrimPUInt64BShiftL(dest, a, b) binArith(uint64, <<, dest, a, b)
#define pLsrPrimPSInt64BShiftR(dest, a, b) binArith(sint64, >>, dest, a, b)
#define pLsrPrimPUInt64BShiftR(dest, a, b) binArith(uint64, >>, dest, a, b)

/* Float32 ops */
#define pLsrPrimPFloat32Plus(dest, a, b)   binArith(float32, +, dest, a, b)   
#define pLsrPrimPFloat32Minus(dest, a, b)  binArith(float32, -, dest, a, b)      
#define pLsrPrimPFloat32Times(dest, a, b)  binArith(float32, *, dest, a, b)      
#define pLsrPrimPFloat32Divide(dest, a, b) binArith(float32, /, dest, a, b)
#if (defined(__pillar__) && !defined(__pillar2c__))
#warning "Using double precision math for floats to work around pillar bug"
#define pLsrPrimPFloat32Max(dest,  a, b) binArithPrefix(float32, fmax, dest, a, b)
#define pLsrPrimPFloat32Min(dest, a, b)  binArithPrefix(float32, fmin, dest, a, b)
#define pLsrPrimPFloat32ModT(dest, a, b) binArithPrefix(float32, fmod, dest, a, b)
#else
#define pLsrPrimPFloat32Max(dest,  a, b) binArithPrefix(float32, fmaxf, dest, a, b)
#define pLsrPrimPFloat32Min(dest, a, b)  binArithPrefix(float32, fminf, dest, a, b)
#define pLsrPrimPFloat32ModT(dest, a, b) binArithPrefix(float32, fmodf, dest, a, b)
#endif
#define pLsrPrimPFloat32Negate(dest, a)    unArith(float32, -, dest, a)

#define pLsrPrimPFloat32EQ(dest, a, b) binPred(float32, ==, dest, a, b)     
#define pLsrPrimPFloat32NE(dest, a, b) binPred(float32, !=, dest, a, b)     
#define pLsrPrimPFloat32LT(dest, a, b) binPred(float32, <, dest, a, b)      
#define pLsrPrimPFloat32LE(dest, a, b) binPred(float32, <=, dest, a, b)

#define pLsrPrimPFloat32FromSInt8(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPFloat32FromSInt16(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPFloat32FromSInt32(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPFloat32FromSInt64(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPFloat32FromUInt8(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPFloat32FromUInt16(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPFloat32FromUInt32(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPFloat32FromUInt64(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPFloat32FromFloat32(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPFloat32FromFloat64(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPFloat32FromRational(dest, a) pLsrFloat32FromRational(dest, a)
#define pLsrPrimPFloat32FromInteger(dest, a) pLsrFloat32FromInteger(dest, a)

#define pLsrPrimPSInt8CastFloat32(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPSInt16CastFloat32(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPSInt32CastFloat32(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPSInt64CastFloat32(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPUInt8CastFloat32(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPUInt16CastFloat32(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPUInt32CastFloat32(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPUInt64CastFloat32(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPFloat32CastFloat32(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPFloat64CastFloat32(dest, a) pLsrPrimNumericConv(dest, float32, a)
#define pLsrPrimPIntegerCastFloat32(dest, a) pLsrIntegerCastToFloat32(dest, a)

/* XXX NG: need to find a header file for this */
float64 __cdecl trunc(float64);

#if (defined(__pillar__) && !defined(__pillar2c__))
#warning "Using double precision math for floats to work around pillar bug"

#define pLsrPrimPFloat32ACos(dest, a)   ((dest) = (float32)acos((float32)(a)))
#define pLsrPrimPFloat32ASin(dest, a)   ((dest) = (float32)asin((float32)(a)))
#define pLsrPrimPFloat32ATan(dest, a)   ((dest) = (float32)atan((float32)(a)))
#define pLsrPrimPFloat32TanH(dest, a)   ((dest) = (float32)tanh((float32)(a)))
#define pLsrPrimPFloat32CosH(dest, a)   ((dest) = (float32)cosh((float32)(a)))
#define pLsrPrimPFloat32SinH(dest, a)   ((dest) = (float32)sinh((float32)(a)))
#define pLsrPrimPFloat32Ceil(dest, a)   ((dest) = (float32)ceil((float32)(a)))
#define pLsrPrimPFloat32Cos(dest, a)    ((dest) = (float32)cos((float32)(a)))
#define pLsrPrimPFloat32Exp(dest, a)    ((dest) = (float32)exp((float32)(a)))
#define pLsrPrimPFloat32Floor(dest, a)  ((dest) = (float32)floor((float32)(a)))
#define pLsrPrimPFloat32Ln(dest, a)     ((dest) = (float32)log((float32)(a)))
#define pLsrPrimPFloat32Rcp(dest, a)    ((dest) = (((float32)1.0)/((float32) a)))
#define pLsrPrimPFloat32Sin(dest, a)    ((dest) = (float32)sin((float32)(a)))
#define pLsrPrimPFloat32Sqrt(dest, a)   ((dest) = (float32)sqrt((float32)(a)))
#define pLsrPrimPFloat32Tan(dest, a)    ((dest) = (float32)tan((float32)(a)))
#define pLsrPrimPFloat32Trunc(dest, a)  ((dest) = (float32)trunc((float32)(a)))
#define pLsrPrimPFloat32Pow(dest, a, b) ((dest) = (float32)pow((float32)(a), (float32)(b)))
#else
#define pLsrPrimPFloat32ACos(dest, a)   ((dest) = acosf((float32)(a)))
#define pLsrPrimPFloat32ATan(dest, a)   ((dest) = atanf((float32)(a)))
#define pLsrPrimPFloat32ASin(dest, a)   ((dest) = asinf((float32)(a)))
#define pLsrPrimPFloat32TanH(dest, a)   ((dest) = tanhf((float32)(a)))
#define pLsrPrimPFloat32CosH(dest, a)   ((dest) = coshf((float32)(a)))
#define pLsrPrimPFloat32SinH(dest, a)   ((dest) = sinhf((float32)(a)))
#define pLsrPrimPFloat32Ceil(dest, a)   ((dest) = ceilf((float32)(a)))
#define pLsrPrimPFloat32Cos(dest, a)    ((dest) = cosf((float32)(a)))
#define pLsrPrimPFloat32Exp(dest, a)    ((dest) = expf((float32)(a)))
#define pLsrPrimPFloat32Floor(dest, a)  ((dest) = floorf((float32)(a)))
#define pLsrPrimPFloat32Ln(dest, a)     ((dest) = logf((float32)(a)))
#define pLsrPrimPFloat32Rcp(dest, a)    ((dest) = (((float32)1.0)/((float32) a)))
#define pLsrPrimPFloat32Sin(dest, a)    ((dest) = sinf((float32)(a)))
#define pLsrPrimPFloat32Sqrt(dest, a)   ((dest) = sqrtf((float32)(a)))
#define pLsrPrimPFloat32Tan(dest, a)    ((dest) = tanf((float32)(a)))
#define pLsrPrimPFloat32Trunc(dest, a)  ((dest) = truncf((float32)(a)))
#define pLsrPrimPFloat32Pow(dest, a, b) ((dest) = powf((float32)(a), (float32)(b)))
#endif

/* Float64 ops */
#define pLsrPrimPFloat64Plus(dest, a, b)   binArith(float64, +, dest, a, b)   
#define pLsrPrimPFloat64Minus(dest, a, b)  binArith(float64, -, dest, a, b)      
#define pLsrPrimPFloat64Times(dest, a, b)  binArith(float64, *, dest, a, b)      
#define pLsrPrimPFloat64Divide(dest, a, b) binArith(float64, /, dest, a, b)
#define pLsrPrimPFloat64Max(dest,  a, b) binArithPrefix(float64, fmax, dest, a, b)
#define pLsrPrimPFloat64Min(dest, a, b)  binArithPrefix(float64, fmin, dest, a, b)
#define pLsrPrimPFloat64ModT(dest, a, b) binArithPrefix(float64, fmod, dest, a, b)
#define pLsrPrimPFloat64Negate(dest, a)    unArith(float64, -, dest, a)

#define pLsrPrimPFloat64EQ(dest, a, b) binPred(float64, ==, dest, a, b)     
#define pLsrPrimPFloat64NE(dest, a, b) binPred(float64, !=, dest, a, b)     
#define pLsrPrimPFloat64LT(dest, a, b) binPred(float64, <, dest, a, b)      
#define pLsrPrimPFloat64LE(dest, a, b) binPred(float64, <=, dest, a, b)

#define pLsrPrimPFloat64FromSInt8(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPFloat64FromSInt16(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPFloat64FromSInt32(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPFloat64FromSInt64(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPFloat64FromUInt8(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPFloat64FromUInt16(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPFloat64FromUInt32(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPFloat64FromUInt64(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPFloat64FromFloat32(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPFloat64FromFloat64(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPFloat64FromRational(dest, a) pLsrFloat64FromRational(dest, a)
#define pLsrPrimPFloat64FromInteger(dest, a) pLsrFloat64FromInteger(dest, a)

#define pLsrPrimPSInt8CastFloat64(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPSInt16CastFloat64(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPSInt32CastFloat64(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPSInt64CastFloat64(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPUInt8CastFloat64(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPUInt16CastFloat64(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPUInt32CastFloat64(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPUInt64CastFloat64(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPFloat32CastFloat64(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPFloat64CastFloat64(dest, a) pLsrPrimNumericConv(dest, float64, a)
#define pLsrPrimPIntegerCastFloat64(dest, a) pLsrIntegerCastToFloat64(dest, a)

#define pLsrPrimPFloat64ACos(dest, a)   ((dest) = acos((float64)(a)))
#define pLsrPrimPFloat64ATan(dest, a)   ((dest) = atan((float64)(a)))
#define pLsrPrimPFloat64ASin(dest, a)   ((dest) = asin((float64)(a)))
#define pLsrPrimPFloat64TanH(dest, a)   ((dest) = tanh((float64)(a)))
#define pLsrPrimPFloat64CosH(dest, a)   ((dest) = cosh((float64)(a)))
#define pLsrPrimPFloat64SinH(dest, a)   ((dest) = sinh((float64)(a)))
#define pLsrPrimPFloat64Ceil(dest, a)   ((dest) = ceil((float64)(a)))
#define pLsrPrimPFloat64Cos(dest, a)    ((dest) = cos((float64)(a)))
#define pLsrPrimPFloat64Floor(dest, a)  ((dest) = floor((float64)(a)))
#define pLsrPrimPFloat64Exp(dest, a)    ((dest) = exp((float64)(a)))
#define pLsrPrimPFloat64Ln(dest, a)     ((dest) = log((float64)(a)))
#define pLsrPrimPFloat64Rcp(dest, a)    ((dest) = (((float64)1.0)/((float64) a)))
#define pLsrPrimPFloat64Sin(dest, a)    ((dest) = sin((float64)(a)))
#define pLsrPrimPFloat64Sqrt(dest, a)   ((dest) = sqrt((float64)(a)))
#define pLsrPrimPFloat64Tan(dest, a)    ((dest) = tan((float64)(a)))
#define pLsrPrimPFloat64Trunc(dest, a)  ((dest) = trunc((float64)(a)))
#define pLsrPrimPFloat64Pow(dest, a, b) ((dest) = pow((float64)(a), (float64)(b)))

/* Arbitrary precision integer ops */ 
#define pLsrPrimPIntegerPlus(dest, a, b)    pLsrIntegerPlus(dest, a, b)
#define pLsrPrimPIntegerMinus(dest, a, b)   pLsrIntegerMinus(dest, a, b)
#define pLsrPrimPIntegerTimes(dest, a, b)   pLsrIntegerTimes(dest, a, b)
#define pLsrPrimPIntegerDivT(dest, a, b)    pLsrIntegerDivT(dest, a, b)
#define pLsrPrimPIntegerDivE(dest, a, b)    pLsrIntegerDivE(dest, a, b)
#define pLsrPrimPIntegerDivF(dest, a, b)    pLsrIntegerDivF(dest, a, b)
#define pLsrPrimPIntegerModT(dest, a, b)    pLsrIntegerModT(dest, a, b)
#define pLsrPrimPIntegerModE(dest, a, b)    pLsrIntegerModE(dest, a, b)
#define pLsrPrimPIntegerModF(dest, a, b)    pLsrIntegerModF(dest, a, b)
#define pLsrPrimPIntegerDivModT(dest1, dest2, a, b) pLsrIntegerDivModT(dest1, dest2, a, b)
#define pLsrPrimPIntegerDivModE(dest1, dest2, a, b) pLsrIntegerDivModE(dest1, dest2, a, b)
#define pLsrPrimPIntegerDivModF(dest1, dest2, a, b) pLsrIntegerDivModF(dest1, dest2, a, b)
#define pLsrPrimPIntegerNegate(dest, a)     pLsrIntegerNegate(dest, a)

#define pLsrPrimPIntegerEQ(dest, a, b) pLsrIntegerEQ(dest, a, b)
#define pLsrPrimPIntegerNE(dest, a, b) pLsrIntegerNE(dest, a, b)
#define pLsrPrimPIntegerLT(dest, a, b) pLsrIntegerLT(dest, a, b)
#define pLsrPrimPIntegerLE(dest, a, b) pLsrIntegerLE(dest, a, b)

#define pLsrPrimPIntegerFromSInt8(dest, a) pLsrIntegerFromSInt8(dest, a)
#define pLsrPrimPIntegerFromSInt16(dest, a) pLsrIntegerFromSInt16(dest, a)
#define pLsrPrimPIntegerFromSInt32(dest, a) pLsrIntegerFromSInt32(dest, a)
#define pLsrPrimPIntegerFromSInt64(dest, a) pLsrIntegerFromSInt64(dest, a)
#define pLsrPrimPIntegerFromUInt8(dest, a) pLsrIntegerFromUInt8(dest, a)
#define pLsrPrimPIntegerFromUInt16(dest, a) pLsrIntegerFromUInt16(dest, a)
#define pLsrPrimPIntegerFromUInt32(dest, a) pLsrIntegerFromUInt32(dest, a)
#define pLsrPrimPIntegerFromUInt64(dest, a) pLsrIntegerFromUInt64(dest, a)
#define pLsrPrimPIntegerFromFloat32(dest, a) pLsrIntegerFromFloat32(dest, a)
#define pLsrPrimPIntegerFromFloat64(dest, a) pLsrIntegerFromFloat64(dest, a)
#define pLsrPrimPIntegerFromRational(dest, a) pLsrIntegerFromRational(dest, a)
#define pLsrPrimPIntegerFromInteger(dest, a) pLsrIntegerFromInteger(dest, a)

#define pLsrPrimPSInt8CastInteger(dest, a) pLsrIntegerFromSInt8(dest, a)
#define pLsrPrimPSInt16CastInteger(dest, a) pLsrIntegerFromSInt16(dest, a)
#define pLsrPrimPSInt32CastInteger(dest, a) pLsrIntegerFromSInt32(dest, a)
#define pLsrPrimPSInt64CastInteger(dest, a) pLsrIntegerFromSInt64(dest, a)
#define pLsrPrimPUInt8CastInteger(dest, a) pLsrIntegerFromUInt8(dest, a)
#define pLsrPrimPUInt16CastInteger(dest, a) pLsrIntegerFromUInt16(dest, a)
#define pLsrPrimPUInt32CastInteger(dest, a) pLsrIntegerFromUInt32(dest, a)
#define pLsrPrimPUInt64CastInteger(dest, a) pLsrIntegerFromUInt64(dest, a)
#define pLsrPrimPIntegerCastInteger(dest, a) pLsrIntegerFromInteger(dest, a)

#define pLsrPrimPIntegerBNot(dest, a)       pLsrIntegerBNot(dest, a)
#define pLsrPrimPIntegerBAnd(dest, a, b)    pLsrIntegerBAnd(dest, a, b)
#define pLsrPrimPIntegerBOr(dest, a, b)     pLsrIntegerBOr(dest, a, b)
#define pLsrPrimPIntegerBShiftL(dest, a, b) pLsrIntegerBShiftL(dest, a, b)
#define pLsrPrimPIntegerBShiftR(dest, a, b) pLsrIntegerBShiftR(dest, a, b)
#define pLsrPrimPIntegerBXor(dest, a, b)    pLsrIntegerBXor(dest, a, b)

/* Rational ops */ 
#define pLsrPrimPRationalPlus(dest, a, b)    pLsrRationalPlus(dest, a, b)
#define pLsrPrimPRationalMinus(dest, a, b)   pLsrRationalMinus(dest, a, b)
#define pLsrPrimPRationalTimes(dest, a, b)   pLsrRationalTimes(dest, a, b)
#define pLsrPrimPRationalDivT(dest, a, b)    pLsrRationalDivT(dest, a, b)
#define pLsrPrimPRationalDivE(dest, a, b)    pLsrRationalDivE(dest, a, b)
#define pLsrPrimPRationalDivF(dest, a, b)    pLsrRationalDivF(dest, a, b)
#define pLsrPrimPRationalModT(dest, a, b)    pLsrRationalModT(dest, a, b)
#define pLsrPrimPRationalModE(dest, a, b)    pLsrRationalModE(dest, a, b)
#define pLsrPrimPRationalModF(dest, a, b)    pLsrRationalModF(dest, a, b)
#define pLsrPrimPRationalDivModT(dest1, dest2, a, b) pLsrRationalDivModT(dest1, dest2, a, b)
#define pLsrPrimPRationalDivModE(dest1, dest2, a, b) pLsrRationalDivModE(dest1, dest2, a, b)
#define pLsrPrimPRationalDivModF(dest1, dest2, a, b) pLsrRationalDivModF(dest1, dest2, a, b)
#define pLsrPrimPRationalDivide(dest, a)     pLsrRationalDivide(dest, a)
#define pLsrPrimPRationalNegate(dest, a)     pLsrRationalNegate(dest, a)

#define pLsrPrimPRationalEQ(dest, a, b) pLsrRationalEQ(dest, a, b)
#define pLsrPrimPRationalNE(dest, a, b) pLsrRationalNE(dest, a, b)
#define pLsrPrimPRationalLT(dest, a, b) pLsrRationalLT(dest, a, b)
#define pLsrPrimPRationalLE(dest, a, b) pLsrRationalLE(dest, a, b)

#define pLsrPrimPRationalFromSInt8(dest, a) pLsrRationalFromSInt8(dest, a)
#define pLsrPrimPRationalFromSInt16(dest, a) pLsrRationalFromSInt16(dest, a)
#define pLsrPrimPRationalFromSInt32(dest, a) pLsrRationalFromSInt32(dest, a)
#define pLsrPrimPRationalFromSInt64(dest, a) pLsrRationalFromSInt64(dest, a)
#define pLsrPrimPRationalFromUInt8(dest, a) pLsrRationalFromUInt8(dest, a)
#define pLsrPrimPRationalFromUInt16(dest, a) pLsrRationalFromUInt16(dest, a)
#define pLsrPrimPRationalFromUInt32(dest, a) pLsrRationalFromUInt32(dest, a)
#define pLsrPrimPRationalFromUInt64(dest, a) pLsrRationalFromUInt64(dest, a)
#define pLsrPrimPRationalFromFloat32(dest, a) pLsrRationalFromFloat32(dest, a)
#define pLsrPrimPRationalFromFloat64(dest, a) pLsrRationalFromFloat64(dest, a)
#define pLsrPrimPRationalFromRational(dest, a) pLsrRationalFromRational(dest, a)
#define pLsrPrimPRationalFromInteger(dest, a) pLsrRationalFromInteger(dest, a)

/**********************************************************************
 * Names
 */

#define pLsrPrimPNameGetString(dest, n) (dest = pLsrPNameGetString(n))
#define pLsrPrimPNameGetHash(dest, n) (dest = pLsrPNameGetHash(n))

/**********************************************************************
 * CString
 */

#define pLsrPrimPCStringAllocate(dest, len)      \
    do {                                        \
        dest = (char*) pLsrAllocC(len+1);       \
        dest[len] = '\0';                       \
    } while(0)

#define pLsrPrimPCStringDeallocate(str) (pLsrFreeC(str))
#define pLsrPrimPCStringGetLen(dest, str) ((dest) = strlen(str))
#define pLsrPrimPCStringGetChar(dest, str, idx) ((dest) = (str)[idx])
#define pLsrPrimPCStringSetChar(str, idx, c) ((str)[idx] = (c))

/**********************************************************************
 * Pointer Equality
 */

#define pLsrPrimPPtrEq(dest, p1, p2) (dest = ((void*)p1)==((void*)p2))

/**********************************************************************
 * Conditional MOV
 */
#define pLsrPrimPCondMov(dest, b, u, v) ((dest) = ((b) ? (u) : (v)))

/**********************************************************************
 * Booleans
 */

#define pLsrPrimPBooleanNot(dest, b0)     ((dest) = !(b0))
#define pLsrPrimPBooleanAnd(dest, b0, b1) ((dest) = (b0) && (b1))
#define pLsrPrimPBooleanOr(dest, b0, b1)  ((dest) = (b0) || (b1))
#define pLsrPrimPBooleanXOr(dest, b0, b1) ((dest) = (b0) ^ (b1))
#define pLsrPrimPBooleanEq(dest, b0, b1)  ((dest) = (b0) == (b1))


#endif /* _PLSR_PRIMS_PRIMS_H_ */
