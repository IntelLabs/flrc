/* The Haskell Research Compiler */
/* COPYRIGHT_NOTICE_1 */

#ifndef _GHC_TTY_H_
#define _GHC_TTY_H_

#include "hrc/ghc/float.h"

#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

void* __hscore_get_saved_termios(I_ fd0);
void __hscore_set_saved_termios(I_ fd0, void* ts);
void resetTerminalSettings (void);

#endif
