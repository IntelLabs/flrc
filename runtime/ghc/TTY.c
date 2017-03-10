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

/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * TTY-related functionality
 *
 * ---------------------------------------------------------------------------*/
/*
#include "PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h" // __hscore_get/set prototypes
#include "TTY.h"
*/
#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#include "hrc/ghc/TTY.h"

// Here we save the terminal settings on the standard file
// descriptors, if we need to change them (eg. to support NoBuffering
// input).
static void *saved_termios[3] = {NULL,NULL,NULL};

void*
__hscore_get_saved_termios(I_ fd0)
{
  int fd = (int) fd0;
  return (0 <= fd && fd < (int)(sizeof(saved_termios) / sizeof(*saved_termios))) ?
    saved_termios[fd] : NULL;
}

void
__hscore_set_saved_termios(I_ fd0, void* ts)
{
  int fd = (int) fd0;
  if (0 <= fd && fd < (int)(sizeof(saved_termios) / sizeof(*saved_termios))) {
    saved_termios[fd] = ts;
  }
}

void
resetTerminalSettings (void)
{
#if HAVE_TERMIOS_H
    // Reset the terminal settings on the standard file descriptors,
    // if we changed them.  See System.Posix.Internals.tcSetAttr for
    // more details, including the reason we termporarily disable
    // SIGTTOU here.
    {
	int fd;
	sigset_t sigset, old_sigset;
	sigemptyset(&sigset);
	sigaddset(&sigset, SIGTTOU);
	sigprocmask(SIG_BLOCK, &sigset, &old_sigset);
	for (fd = 0; fd <= 2; fd++) {
	    struct termios* ts = (struct termios*)__hscore_get_saved_termios(fd);
	    if (ts != NULL) {
		tcsetattr(fd,TCSANOW,ts);
	    }
	}
	sigprocmask(SIG_SETMASK, &old_sigset, NULL);
    }
#endif
}
