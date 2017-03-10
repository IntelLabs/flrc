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

/* Arbitrary precision numbers */

#ifndef _PLSR_NUMERIC_H_
#define _PLSR_NUMERIC_H_

/**********************************************************************
 * Arbitrary precision integers
 */

#include "hrc/plsr-integer.h"

/**********************************************************************
 * Arbitrary precision rationals
 */

#include "hrc/plsr-rational.h"

/**********************************************************************
 * Some miscellaneous floating point stuff
 */

static char* pLsrCStringFromFloat32(float32 flt)
{
    char* str = pLsrAllocC(30);
    sprintf(str, "%f", flt);
    return str;
}

static float32 pLsrFloat32FromCString(char* str)
{
    float32 res = 0.0f;
    sscanf(str, "%f", &res);
    return res;
}

/**********************************************************************
 * GC registration functions
 */

static void pLsrNumericRegisterVTables()
{
    pLsrIntegerRegisterVTables();
    pLsrRationalRegisterVTables();
}

static void pLsrNumericRegisterGlobals()
{
    pLsrIntegerRegisterGlobals();
    pLsrRationalRegisterGlobals();
}

static void pLsrNumericCheckAssertions()
{
    pLsrRationalCheckAssertions();
}

static void pLsrNumericInitialize(uintp memLimit)
{
    pLsrAPIntInitialize(memLimit);
    pLsrAPRatInitialize();
}
#endif /* !_PLSR_NUMERIC_H_ */
