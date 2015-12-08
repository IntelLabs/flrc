/* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

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
