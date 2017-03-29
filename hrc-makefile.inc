# The Haskell Research Compiler

# Redistribution and use in source and binary forms, with or without modification, are permitted 
# provided that the following conditions are met:
# 1.   Redistributions of source code must retain the above copyright notice, this list of 
# conditions and the following disclaimer.
# 2.   Redistributions in binary form must reproduce the above copyright notice, this list of
# conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
# BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
# OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
# IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

all: all2

INSTALL_BINDIR     = $(HRC_PREFIX)/bin
INSTALL_INCLUDEDIR = $(HRC_PREFIX)/include/hrc
INSTALL_LIBDIR     = $(HRC_PREFIX)/lib

BINDIR = bin
RUNDIR = runtime
RUNDIR_INCLUDEDIR = $(RUNDIR)/include

install: $(HRC_GHC_RUNTIME_LIBS) $(HRC_EXE)
	install -d $(INSTALL_BINDIR) $(INSTALL_INCLUDEDIR) $(INSTALL_INCLUDEDIR)/ghc $(INSTALL_LIBDIR)
	install $(HRC_GHC_RUNTIMEDIR)/*.a $(INSTALL_LIBDIR)
	install $(HRC_EXE) $(INSTALL_BINDIR)
	install $(RUNDIR_INCLUDEDIR)/hrc/*.h $(INSTALL_INCLUDEDIR)
	install $(RUNDIR_INCLUDEDIR)/hrc/ghc/*.h $(INSTALL_INCLUDEDIR)/ghc

ifeq (Windows_NT, $(OS))
  EXE := .exe
else
  EXE :=
endif

### HRC-GHC runtime
CC = gcc
AR = ar

ARFLAGS  = rvs
CPPFLAGS = -I"$(RUNDIR_INCLUDEDIR)" $(PLATFORM_CPPFLAGS) `pkg-config --cflags flrc-lib` -Wall
CFLAGS   = $(CPPFLAGS)

HRC_GHC_RUNTIMEDIR = $(RUNDIR)/ghc

HRC_GHC_RUNTIME_LIBS = \
  $(HRC_GHC_RUNTIMEDIR)/libhrc_ghc_runtime32.a \
	$(HRC_GHC_RUNTIMEDIR)/libhrc_ghc_runtime64.a

HRC_GHC_RUNTIME_OBJECTS32 = \
  $(HRC_GHC_RUNTIMEDIR)/float32.o \
	$(HRC_GHC_RUNTIMEDIR)/Globals32.o \
	$(HRC_GHC_RUNTIMEDIR)/plsr-util32.o \
	$(HRC_GHC_RUNTIMEDIR)/thread32.o \
	$(HRC_GHC_RUNTIMEDIR)/TTY32.o

HRC_GHC_RUNTIME_OBJECTS64 = \
  $(HRC_GHC_RUNTIMEDIR)/float64.o \
	$(HRC_GHC_RUNTIMEDIR)/Globals64.o \
	$(HRC_GHC_RUNTIMEDIR)/plsr-util64.o \
	$(HRC_GHC_RUNTIMEDIR)/thread64.o \
	$(HRC_GHC_RUNTIMEDIR)/TTY64.o

$(HRC_GHC_RUNTIMEDIR)/%32.o: $(HRC_GHC_RUNTIMEDIR)/%.c
	$(CC) $(CFLAGS) -DP_WORD_SIZE=4 $< -c -o $@

$(HRC_GHC_RUNTIMEDIR)/%64.o: $(HRC_GHC_RUNTIMEDIR)/%.c
	$(CC) $(CFLAGS) -DP_WORD_SIZE=8 $< -c -o $@

$(HRC_GHC_RUNTIMEDIR)/libhrc_ghc_runtime32.a: $(HRC_GHC_RUNTIME_OBJECTS32)
	$(AR) $(ARFLAGS) $@ $<

$(HRC_GHC_RUNTIMEDIR)/libhrc_ghc_runtime64.a: $(HRC_GHC_RUNTIME_OBJECTS64)
	$(AR) $(ARFLAGS) $@ $^

hrc-ghc-runtime: $(HRC_GHC_RUNTIME_LIBS)

### Old P Front End

OFE = frontend-old
OFE_EXE = $(BINDIR)/$(OFE)$(EXE)
OFE_SRCS = frontend-old/p.cpp

ofe: $(OFE_EXE)

$(OFE_EXE): build/vs2010/$(OFE)/$(OFE)/$(OFE).vcxproj $(OFE_SRCS)
	cd build/vs2010/$(OFE)/$(OFE); devenv $(OFE).vcxproj /rebuild Debug; cp ../Debug/frontend-old.exe ../../../../bin
#$(OFE_EXE): build/vc60/$(OFE)/$(OFE).dsp $(OFE_SRCS)
#	cd build/vc60/$(OFE); msdev $(OFE).dsp /MAKE "p - Win32 Debug" /REBUILD

### New P Front End

NFE = frontend-new
NFE_EXE = $(BINDIR)/$(NFE)$(EXE)
NFE_SRCS = $(NFE)/p.hs

nfe: $(NFE_EXE)

$(NFE_EXE): $(NFE_SRCS)
	ghc -fno-gen-manifest $(NFE_SRCS) -o $(NFE_EXE)

RR_EXE = $(BINDIR)/p-runtime$(EXE)
RR_SRCS = $(NFE)/p-runtime.hs

$(RR_EXE): $(RR_SRCS)
	ghc $(RR_SRCS) -o $(RR_EXE)

### Main part of the compiler

HRC_VERSION = v1.0

# keep these in alpabetic order
COMPILER_SRCS = \
  compiler/back-end/back-end.mlb \
  compiler/back-end/back-end.sml \
  compiler/back-end/mil-to-pil.sml \
  compiler/back-end/outputter.sml \
  compiler/back-end/pil.sml \
  compiler/back-end/runtime.sml \
  compiler/common/chat.sml \
  compiler/common/common.mlb \
  compiler/common/compare.sml \
  compiler/common/config.sml \
  compiler/common/dataflow.sml \
  compiler/common/dominance.sml \
  compiler/common/effect.sml \
  compiler/common/fail.sml \
  compiler/common/globals.sml \
  compiler/common/graph.sml \
  compiler/common/identifier.sml \
  compiler/common/int-arb.sml \
  compiler/common/intr.sml \
  compiler/common/lub.sml \
  compiler/common/pass.sml \
  compiler/common/path.sml \
  compiler/common/rat.sml \
  compiler/common/rename.sml \
  compiler/common/topo-sort.sml \
  compiler/common/tuple.sml \
  compiler/common/try.sml \
  compiler/common/type-rep.sml \
  compiler/common/utils.sml \
  compiler/common/z-coding.sml \
  compiler/driver.sml \
  compiler/mil/analyse.sml \
  compiler/mil/bound-vars.sml \
  compiler/mil/call-graph.sml \
  compiler/mil/cfg.sml \
  compiler/mil/check.sml \
  compiler/mil/code-copy.sml \
  compiler/mil/compile.mlb \
  compiler/mil/compile.sml \
  compiler/mil/dataflow-analysis.sml \
  compiler/mil/dependence-analysis.sml \
  compiler/mil/extended-layout.sml \
  compiler/mil/fmil.sml \
  compiler/mil/free-vars.sml \
  compiler/mil/imil/block.sml \
  compiler/mil/imil/both-mil.sml \
  compiler/mil/imil/common.sml \
  compiler/mil/imil/def.sml \
  compiler/mil/imil/enumerate.sml \
  compiler/mil/imil/func.sml \
  compiler/mil/imil/global.sml \
  compiler/mil/imil/instr.sml \
  compiler/mil/imil/item.sml \
  compiler/mil/imil/layout.sml \
  compiler/mil/imil/t.sml \
  compiler/mil/imil/types.sml \
  compiler/mil/imil/use.sml \
  compiler/mil/imil/var.sml \
  compiler/mil/imil/workset.sml \
  compiler/mil/imil/imil.mlb \
  compiler/mil/imil/imil.sml \
  compiler/mil/layout.sml \
  compiler/mil/loop.sml \
  compiler/mil/lower/lower.mlb \
  compiler/mil/lower/mil-to-core-mil.sml \
  compiler/mil/lower/vector.sml \
  compiler/mil/mil.mlb \
  compiler/mil/mil.sml \
  compiler/mil/name-small-values.sml \
  compiler/mil/number-instructions.sml \
  compiler/mil/optimise/annotated-cg-printer.sml \
  compiler/mil/optimise/branch-remove.sml \
  compiler/mil/optimise/cfg-simplify.sml \
  compiler/mil/optimise/contify.sml \
  compiler/mil/optimise/cse.sml \
  compiler/mil/optimise/double-diamond.sml \
  compiler/mil/optimise/fun-known.sml \
  compiler/mil/optimise/fx-analysis.sml \
  compiler/mil/optimise/inline-aggressive.sml \
  compiler/mil/optimise/inline-leaves.sml \
  compiler/mil/optimise/inline-profile.sml \
  compiler/mil/optimise/inline-rewrite.sml  \
  compiler/mil/optimise/inline-small.sml \
  compiler/mil/optimise/iv-cse.sml \
  compiler/mil/optimise/licm.sml \
  compiler/mil/optimise/loop-invert.sml \
  compiler/mil/optimise/optimise.mlb \
  compiler/mil/optimise/rep/analyze.sml \
  compiler/mil/optimise/rep/base.sml \
  compiler/mil/optimise/rep/dead-code.sml \
  compiler/mil/optimise/rep/driver.sml \
  compiler/mil/optimise/rep/flatten.sml \
  compiler/mil/optimise/rep/flowgraph.sml \
  compiler/mil/optimise/rep/node.sml \
  compiler/mil/optimise/rep/object.sml \
  compiler/mil/optimise/rep/optimize.sml \
  compiler/mil/optimise/rep/prep.sml \
  compiler/mil/optimise/rep/reconstruct.sml \
  compiler/mil/optimise/rep/seq.sml \
  compiler/mil/optimise/rep/show.sml \
  compiler/mil/optimise/rep/summary.sml \
  compiler/mil/optimise/rep/rep.mlb \
  compiler/mil/optimise/rep/rep.sml \
  compiler/mil/optimise/simple-escape.sml \
  compiler/mil/optimise/simplify.sml \
  compiler/mil/optimise/thunks.sml \
  compiler/mil/optimise/vectorize.sml \
  compiler/mil/p-object-model.sml \
  compiler/mil/parse.sml \
  compiler/mil/prims.sml \
  compiler/mil/prims-utils.sml \
  compiler/mil/profile.sml \
  compiler/mil/rename.sml \
  compiler/mil/rewrite.sml \
  compiler/mil/shape-analysis.sml \
  compiler/mil/stats.sml \
  compiler/mil/stream.sml \
  compiler/mil/stream2.sml \
  compiler/mil/transform.sml \
  compiler/mil/type.sml \
  compiler/mil/utils.sml \
  compiler/mil/utils2.sml

COMPILER_STD_OPTS = @MLton -- \
	-verbose 1 -runtime 'use-mmap true' \
	-codegen native \
	-native-split 900000
# The following don't work on 64-bit
#       -ieee-fp true -max-heap 800m

ifeq (y, $(fast))
  COMPILER_OPTS :=$(COMPILER_STD_OPTS)
else
  COMPILER_OPTS :=$(COMPILER_STD_OPTS)  -const 'Exn.keepHistory true'
endif

# keep these in alpabetic order
HASKELL_SRCS = \
  compiler/anorm-strict/anorm-strict-analyze.sml \
  compiler/anorm-strict/anorm-strict-clone.sml \
  compiler/anorm-strict/anorm-strict-closure-convert.sml \
  compiler/anorm-strict/anorm-strict-free-vars.sml \
  compiler/anorm-strict/anorm-strict-layout.sml \
  compiler/anorm-strict/anorm-strict.mlb \
  compiler/anorm-strict/anorm-strict-optimize.sml \
  compiler/anorm-strict/anorm-strict-rewrite.sml \
  compiler/anorm-strict/stats.sml 	\
  compiler/anorm-strict/anorm-strict.sml \
  compiler/anorm-strict/anorm-strict-utils.sml \
  compiler/as-to-mil/as-to-mil.mlb \
  compiler/as-to-mil/ghc-prim.sml \
  compiler/as-to-mil/to-mil.sml \
  compiler/as-to-mil/utils.sml \
  compiler/anorm-lazy/anorm-lazy.sml \
  compiler/anorm-lazy/anorm-lazy-analyze.sml \
  compiler/anorm-lazy/abs-core.sml \
  compiler/anorm-lazy/abs-eval.sml \
  compiler/anorm-lazy/stats.sml \
  compiler/anorm-lazy/strictness.sml \
  compiler/anorm-lazy/to-abs-core.sml \
  compiler/ch-to-as/ch-to-as.mlb \
  compiler/ch-to-as/to-lazy.sml \
  compiler/ch-to-as/to-strict.sml \
  compiler/core-hs/core-hs.mlb \
  compiler/core-hs/core-hs.sml \
  compiler/core-hs/ghc-prim-op.sml \
  compiler/core-hs/ghc-prim-type.sml \
  compiler/core-hs/layout.sml \
  compiler/core-hs/normalize.sml \
  compiler/core-hs/parse.sml \
  compiler/core-hs/link-option.sml \
  compiler/core-hs/core-hs.lex.sml \
  compiler/core-hs/core-hs.grm.sig \
  compiler/core-hs/core-hs.grm.sml \
  compiler/haskell.sml

# keep these in alpabetic order
P_SRCS = \
  compiler/core-op/core-op.mlb \
  compiler/core-op/core-p.sml \
  compiler/core-op/fix-up.sml \
  compiler/core-op/layout.sml \
  compiler/core-op/parse.sml \
  compiler/core-op/prims.sml \
  compiler/core-op/stats.sml \
  compiler/core-op/utils.sml \
  compiler/core-p/common.sml \
  compiler/core-p/common-utils.sml \
  compiler/core-p/core-p.mlb \
  compiler/core-p/linear.sml \
  compiler/core-p/linear-analyse.sml \
  compiler/core-p/linear-dce.sml \
  compiler/core-p/linear-free-variables.sml \
  compiler/core-p/linear-layout.sml \
  compiler/core-p/linear-optimise.sml \
  compiler/core-p/linear-simplify.sml \
  compiler/core-p/linear-sink1.sml \
  compiler/core-p/linear-sink2.sml \
  compiler/core-p/linear-stats.sml \
  compiler/core-p/linear-type.sml \
  compiler/core-p/linear-utils.sml \
  compiler/core-p/natives.sml \
  compiler/core-p/parse.sml \
  compiler/core-p/prims.sml \
  compiler/core-p/raw-layout.sml \
  compiler/core-p/raw-stats.sml \
  compiler/core-p/raw-to-linear.sml \
  compiler/core-p/raw-utils.sml \
  compiler/core-p/raw.sml \
  compiler/core-p/type.sml \
  compiler/hil/analyse.sml \
  compiler/hil/check.sml \
  compiler/hil/dce.sml \
  compiler/hil/free-vars.sml \
  compiler/hil/hil.mlb \
  compiler/hil/hil.sml \
  compiler/hil/layout.sml \
  compiler/hil/optimise.sml \
  compiler/hil/simplifier.sml \
  compiler/hil/utils.sml

# make-version
make-version: $(BINDIR)/make-version.sh
	sh $(BINDIR)/make-version.sh "$(HRC_VERSION)" compiler/version.sml $(HRC_PREFIX)

### hrc

HRC = hrc
HRC_EXE = $(BINDIR)/$(HRC)$(EXE)

HRC_SRCS = $(COMPILER_SRCS) $(HASKELL_SRCS) compiler/hrc.mlb compiler/hrc.sml

hrc: $(HRC_EXE)

$(HRC_EXE): make-version $(HRC_SRCS) compiler/hrc.mlb
	cd compiler; \
	rm -f hrc.*.c hrc.*.s; \
	mlton $(COMPILER_OPTS) -stop g $(HRC).mlb && \
	mlton $(COMPILER_OPTS) -output ../$(HRC_EXE) hrc.*.c hrc.*.s && \
	rm -f hrc.*.c hrc.*.s

### P LSR

# Keep these in alphabetic order
PLSR_SRCS = \
  $(RUNDIR_INCLUDEDIR)/hrc/pil.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-ap-integer.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-ap-rational.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-gc.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-gmp-integer.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-gmp-integer-gallocate.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-flrc-integer.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-integer.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-lightweight-thunk.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-main.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-numeric.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-objects.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-params.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-prims-prims.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-prims-runtime.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-prims-vector-avx.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-prims-vector-sse.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-prims-vector.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-prims.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-ptk-thunk.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-rational.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-tagged-int32.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-thunk.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-util.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-value.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr-vector.h \
  $(RUNDIR_INCLUDEDIR)/hrc/plsr.h

### Main stuff

all2: hrc-ghc-runtime hrc

partclean:
	rm -f runtime/ghc/*.a
	rm -f `find bin build compiler runtime benchmarks \
	            -name \\*~ -o -name \\*.obj -o -name \\*.o -o -name gc.log -o -name .pilink -o -name .pilicl`
	rm -f $(NFE)/*.{hi,o,exe} $(NFE)/*~
	rm -rf build/vc60/$(OFE)/Debug
	rm -f  build/vc60/$(OFE)/$(OFE).{ncb,opt,plg}
	rm -rf build/vc60/pdebug/Debug
	rm -rf build/vc60/pdebug/Release
	rm -rf build/vc60/pdebug/pdebug.{ncb,opt,plg}
	rm -f `find tests -name \\*~ -o -name \\*.exe -o \
                          -name \\*.obj -o -name \\*.o -o -name \\*.cmp -o -name \\*.exp -o \
                          -name \\*.lib -o -name \\*.pdb -o -name \\*.suo -o -name \\*.ilk -o -name gc.log`
	rm -f `find tests -name \\*.c -a -not -name matrix-multiply-c.c`
	-make -C benchmarks/barnes-hut clean
	-make -C benchmarks/cloth-nobc clean
	-make -C benchmarks/matrix-multiply clean
	-make -C benchmarks/quicksort clean
	-make -C benchmarks/raytracer clean
	-make -C benchmarks/smvp clean
	-make -C benchmarks/smvp-direct clean
	-make -C benchmarks/sudoku clean
	-make -C benchmarks/nofib clean
	rm -f *~
	rm -f mlmon.out gc.log root_map.txt *.ppm compiler/version.sml

clean: partclean
	rm -f $(OFE_EXE) $(NFE_EXE) $(HRC_EXE) regressions.log smvp.res

### Testing

test: all
	$(BINDIR)/regress.sh -short -nomake

regress: all
	$(BINDIR)/regress.sh -nomake

torture: all
	$(BINDIR)/regress.sh -torture -nomake

### ML-Yacc and ML-Lex
compiler/core-hs/core-hs.lex.sml: compiler/core-hs/core-hs.lex compiler/core-hs/core-hs.grm.sml
	mllex $<
compiler/core-hs/core-hs.grm.sml: compiler/core-hs/core-hs.grm
	mlyacc $<
compiler/core-hs/core-hs.grm.sig: compiler/core-hs/core-hs.grm
	mlyacc $<
