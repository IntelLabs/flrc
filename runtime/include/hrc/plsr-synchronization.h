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

/* Synchronization primitives */

#ifndef _PLSR_SYNCHRONIZATION_H_
#define _PLSR_SYNCHRONIZATION_H_

#ifdef P_USE_PILLAR

#include "pgc/pgc.h"

#ifdef __pillar2c__
#define SYNCHCC __pcdecl
#else
#define SYNCHCC __cdecl
#endif

static PrtBool SYNCHCC iFlcSyncEqual(volatile void *location, void *data)
{
    return (*(void **)location == data) ? PrtTrue : PrtFalse;
}

static void iFlcSynchWaitNull(ref loc, uintp off)
{
    prtYieldUntilMovable(iFlcSyncEqual, &pLsrObjectField(loc, off, void**), NULL,
                         PrtInfiniteWaitCycles64, PrtGcTagOffset, (void*)off);
}

static PrtBool SYNCHCC iFlcSyncNotEqual(volatile void *location, void *data)
{
    return (*(void **)location != data) ? PrtTrue : PrtFalse;
}

static void iFlcSynchWaitNonNull(ref loc, uintp off)
{
    prtYieldUntilMovable(iFlcSyncNotEqual, &pLsrObjectField(loc, off, void**), NULL,
                         PrtInfiniteWaitCycles64, PrtGcTagOffset, (void*)off);
}

static void iFlcSynchWaitEqualVoidS(ref loc, uintp off, void *data)
{
    prtYieldUntilMovable(iFlcSyncEqual, &pLsrObjectField(loc, off, void**), data,
                         PrtInfiniteWaitCycles64, PrtGcTagOffset, (void*)off);
}

/* Note that this function should not be __pcdecl as it takes and returns refs */
static inline ref pLsrSynchLockedCmpxchgRef(ref* loc, ref cmpValue, ref newValue)
{
    return (ref)pgc_write_ref_cas(loc,newValue,cmpValue);
}

/* Taken from mcrt/src/mcrtthreads/mcrtia32.h */
/* Attempts to atomically set *loc to newValue only if *loc==cmpValue.  Returns the value read
 * from *loc: cmpValue if successful, otherwise the contents of loc.
*/
static inline uint32 SYNCHCC pLsrSynchLockedCmpxchgUInt32(volatile uint32 * loc, uint32 cmpValue, uint32 newValue)
{
#ifdef PLSR_GNU_ASM
    uint32 value;
    __asm__ __volatile__(
        "lock; cmpxchgl %3,(%1)"
        : "=a"(value)
        : "r"(loc), "a"(cmpValue), "d"(newValue)
        : "memory");
    return value;
#else
#ifdef INTEL64
    register uint32 value;
    __asm {
        mov rcx, loc
        mov edx, newValue
        mov eax, cmpValue
        lock cmpxchg dword ptr[rcx], edx
        mov value, eax
    }
    return value;
#else // !INTEL64
    register uint32 value;
    __asm {
        mov ecx, loc
        mov edx, newValue
        mov eax, cmpValue
        lock cmpxchg dword ptr[ecx], edx
        mov value, eax
    }
    return value;
#endif // INTEL64
#endif
}

static inline uint64 SYNCHCC pLsrSynchLockedCmpxchgUInt64(volatile uint64 * loc, uint64 cmpValue, uint64 newValue)
{
#ifdef PLSR_GNU_ASM
    uint64 result;
    __asm__ __volatile__(
        "lock; cmpxchg %3,(%1)"
        : "=a"(result)
        : "r"(loc), "a"(cmpValue), "r"(newValue)
        : "memory");
    return result;
#else
#ifdef INTEL64
  uint64 value;
  __asm {
    mov rcx, loc;
    mov rdx, newValue;
    mov rax, cmpValue;
    lock cmpxchg qword ptr[rcx], rdx;
    mov value, rax;
  }
  return value;
#else // !INTEL64
  uint32 hiCmp = (uint32) (UINT64_C(0xffffffff) & (cmpValue >> 32));
  uint32 lowCmp = (uint32) (UINT64_C(0xffffffff) & cmpValue);

  uint32 hiNew = (uint32) (UINT64_C(0xffffffff) & (newValue >> 32));
  uint32 lowNew = (uint32) (UINT64_C(0xffffffff) & newValue);

  uint32 lowRes;
  uint32 hiRes;

  __asm {
      mov edx, hiCmp
      mov eax, lowCmp
      mov ecx, hiNew
      mov ebx, lowNew
      mov esi, loc
      lock cmpxchg8b qword ptr[esi]
      mov lowRes, eax
      mov hiRes, edx
  }
  {
      uint64 res = (((uint64)hiRes)<<32)|((uint64)lowRes);
      return res;
  }
#endif // INTEL64
#endif
}

#if (P_WORD_SIZE==4)
#define pLsrSynchLockedCmpxchgUIntp pLsrSynchLockedCmpxchgUInt32
#else
#if (P_WORD_SIZE==8)
#define pLsrSynchLockedCmpxchgUIntp pLsrSynchLockedCmpxchgUInt64
#else
#error "Unsupported word size"
#endif /* P_WORD_SIZE=64*/
#endif /* P_WORD_SIZE=32*/

static inline int SYNCHCC pLsrSynchCmpAndSetUInt32(volatile uint32 * loc, uint32 cmpValue, uint32 newValue)
{
    return (pLsrSynchLockedCmpxchgUInt32(loc, cmpValue, newValue) == cmpValue);
}

static inline int SYNCHCC pLsrSynchCmpAndSetUInt64(volatile uint64 * loc, uint64 cmpValue, uint64 newValue)
{
    return (pLsrSynchLockedCmpxchgUInt64(loc, cmpValue, newValue) == cmpValue);
}

static inline int SYNCHCC pLsrSynchCmpAndSetUIntp(volatile uintp * loc, uintp cmpValue, uintp newValue)
{
    return (pLsrSynchLockedCmpxchgUIntp(loc, cmpValue, newValue) == cmpValue);
}

/* N.B.  Timeouts are in absolute cycle time, not deltas from now.
 * XXX Change this to PrtInfiniteTimeoutCycles64 when it becomes available
 */
#define pLsrSynchInfiniteTimeoutCycles64 UINT64_C(0xFFFFFFFFFFFFFFFF)

PrtBool SYNCHCC pLsrSynchYieldPredicateNotEqualUInt32(volatile void *location, void *data) {
    return ((*(volatile uint32*)location) != (uint32) data) ? PrtTrue : PrtFalse;
}

PrtBool SYNCHCC pLsrSynchYieldPredicateNotEqualUIntp(volatile void *location, void *data) {
    return ((*(volatile uintp*)location) != (uintp) data) ? PrtTrue : PrtFalse;
}

PrtBool SYNCHCC pLsrSynchYieldPredicateEqualUInt32(volatile void *location, void *data) {
    return ((*(volatile uint32*)location) == (uint32) data) ? PrtTrue : PrtFalse;
}

PrtBool SYNCHCC pLsrSynchYieldPredicateEqualUIntp(volatile void *location, void *data) {
    return ((*(volatile uintp*)location) == (uintp) data) ? PrtTrue : PrtFalse;
}

void pLsrSynchYieldUntilNotEqualUInt32(volatile uint32*loc, uint32 value)
{
    prtYieldUntil(pLsrSynchYieldPredicateNotEqualUInt32, (void*) loc, (void*) value, pLsrSynchInfiniteTimeoutCycles64);
}

void pLsrSynchYieldUntilNotEqualUIntp(volatile uintp*loc, uintp value)
{
    prtYieldUntil(pLsrSynchYieldPredicateNotEqualUIntp, (void*) loc, (void*) value, pLsrSynchInfiniteTimeoutCycles64);
}

void pLsrSynchYieldUntilEqualUInt32(volatile uint32*loc, uint32 value)
{
    prtYieldUntil(pLsrSynchYieldPredicateEqualUInt32, (void*) loc, (void*) value, pLsrSynchInfiniteTimeoutCycles64);
}

void pLsrSynchYieldUntilEqualUIntp(volatile uintp*loc, uintp value)
{
    prtYieldUntil(pLsrSynchYieldPredicateEqualUIntp, (void*) loc, (void*) value, pLsrSynchInfiniteTimeoutCycles64);
}

static inline uintp pLsrSynchAtomicTakeUIntp(volatile uintp*loc, uintp emptyValue) {
    while (1) {
        uintp contents =*loc;
        if (contents != emptyValue) {
            if (pLsrSynchCmpAndSetUIntp(loc, contents, emptyValue)) {
                return contents;
            }
        } else {
            pLsrSynchYieldUntilNotEqualUIntp(loc, emptyValue);
        }
    }
}

static inline void SYNCHCC pLsrSynchAtomicPutUIntp(volatile uintp*loc, uintp value){
    *loc = value;
}

/* This should probably have some spinning and exponential backoff */
static inline uint32 pLsrSynchAtomicTakeUInt32(volatile uint32*loc, uint32 emptyValue) {
    while (1) {
        uint32 contents =*loc;
        if (contents != emptyValue) {
            if (pLsrSynchCmpAndSetUInt32(loc, contents, emptyValue)) {
                return contents;
            }
        } else {
            pLsrSynchYieldUntilNotEqualUInt32(loc, emptyValue);
        }
    }
}

static inline void SYNCHCC pLsrSynchAtomicPutUInt32(volatile uint32*loc, uint32 value){
    *loc = value;
}

#endif

#endif /* !_PLSR_SYNCHRONIZATION_H_ */
