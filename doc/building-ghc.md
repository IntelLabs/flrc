# How to patch, build and use GHC 7.6.3 with HRC

HRC uses GHC as a frontend to compile user programs or libraries to an
intermediate representation (IR) called external core before it translates
external core to HRC's set of internal IRs and eventually compiles down to
binary via an external C compiler.

Moreover, GHC and its libraries have to be (slightly) patched in order to
produce external cores that is suitable for HRC to consume, mostly to remove
GHC-specific runtime references, and to insert additional primitives required
for immutable array and so on . Also, HRC has runtime support for arbitrary
precision integers, so GHC's `integer-gmp` library is no longer compatible.
We've implemented a new library based on `integer-simple` to integrate GHC's
big integer with HRC's.

However, recent GHC versions have dropped the support for external core (due to
lack of maintainers), and therefore, HRC is stuck with an older version of GHC
7.6.3. 

Because HRC does not support GHC-specific runtime calls or primitives, the
patched GHC 7.6 will not produce code containing them. So it does not work 100%
as a full compiler by itself. In particular, if you compile anything with this
patched compiler, the result binary will most likely fail to run. So you should
not install it to standard system location, but rather a place that is
isolated from other GHC installations. We'll refer to this location of your
choice as `${PREFIX}` in this guide.

Also, this guide only applies to x86 64-bit Linux installation.


## Building a patched GHC 7.6.3 from source

First of all, you'll need an even older version to bootstrap GHC 7.6.3. For
this purpose, we used GHC 7.4.2. You can either find a distro supported binary
package that you can install on your own, or use [Nix] to get one. 

Besides GHC 7.4.2, we also need a working binary of [happy] and
[alex].  They do not have to be older versions, or even compiled by GHC
7.4.2. In theory, any working version that you can find for your distro should
work just fine.

The following steps assumes we use `bash` and we are in `flrc` top directory, 
otherwise the `patches` directory needs to be spelled out in full path to refer
to `flrc/patches`.

### Step 1. Clone GHC source, and switch to ghc-7.6 branch.

```
git clone http://git.haskell.org/ghc.git
cd ghc
git checkout ghc-7.6
```

### Step 2. Patch GHC

```
patch -p1 < ../patches/ghc-7.6-hrc.patch
cp mk/build.mk.hrc mk/build.mk
```

### Step 3. Clone GHC libraries, and checkout ghc-7.6 branch too.

```
perl sync-all --dph get
perl sync-all checkout ghc-7.6
```

### Step 4. Create ghc-7.6 branch for those that do not already have it

```
pushd libraries/random && git checkout -b ghc-7.6 random-1.0.1.1-release && popd
pushd libraries/primitive && git checkout -b ghc-7.6 75c3379b && popd
pushd libraries/vector && git checkout -b ghc-7.6 c4c5a740 && popd
```

### Step 5. We only need vector but not dph (though it is ok to keep it if versions match)

```
rm -rf libraries/dph
```

### Step 6. Patch libraries

```
for libname in Cabal base integer-simple primitive vector; do
  pushd libraries/${libname} && patch -p1 < ../../../patches/ghc-${libname}-7.6-hrc.patch && popd
done
```

### Step 7. Bootstrap and build 

```
perl boot
RELEASE=yes ./configure --prefix=${PREFIX}
make
```

### Step 8. If everything went ok, install GHC 7.6.

```
make install
```


## Use GHC 7.6 with HRC

We usually prefer not to have the patched GHC in `$PATH`, so the way
we run `hrc` is as follows:

```
PATH=${PREFIX}/bin:$PATH hrc ...
```

or `make` as:

```
PATH=${PREFIX}/bin:$PATH make 
```

But you can set `${PREFIX}/bin` in `$PATH` and save typing a few words.

### Use cabal to install libraries 

The patched GHC comes with patched Cabal support that installs
the external core required by HRC along side the usual installation
of a Haskell package. The most convenient way to do this is with 
the usual `cabal` command, and any recent `cabal` binary (from
[cabal-install] package) that comes with your 
distro should work, even those that came with newer Haskell 
Platform or installed from Stack.

To compile a package with HRC using `cabal`, we have to add
a few extra flags as follows:

```
  PATH=${PREFIX}/bin:$PATH cabal install --global \
    --cabal-lib-version=1.16.1 \
    --prefix=${PREFIX} \
    --package-db=${PREFIX}/lib/ghc-7.6.3/lib/package.conf.d \
    [package name]
```

This is because we need to instruct cabal to specifically choose a Cabal
library version of 1.16.1, and install to the DB files of our patched GHC 7.6.3,
instead of to the default user location such as `${HOME}/.cabal`.  Please note
that the file path for the `--package-db=` option might be different on your
machine, so you need to make sure the DB directory `package.conf.d` it refers to
actually exists.

For example, we can use `cabal` to install `parsec-3.1.1`. We choose this
particular version because later parsec has a lot of extra dependencies, which
are not fully tested with HRC.

```
PATH=${PREFIX}/bin:$PATH cabal install --global \
  --cabal-lib-version=1.16.1 --prefix=... --package-db=... \
  parsec-3.1.1
```

For another example, we can install a patched [Repa] library for HRC.
Assume we are in `flrc` top directory. We choose version 3.2.2.2 because later
versions will suck in a later version of vector, breaking the required
dependence on our patched vector library installed together with the patched
GHC.

```
curl https://hackage.haskell.org/package/repa-3.2.2.2/repa-3.2.2.2.tar.gz|tar zxf -
cd repa-3.2.2.2 
patch -p1 < ../patches/ghc-repa-3.2.2.2-hrc.patch
PATH=${PREFIX}/bin:$PATH cabal install --global \
  --cabal-lib-version=1.16.1 --prefix=... --package-db=...
```

People having trouble compiling with an existing cabal tool should double check
if there is any cached `setup` program in `${HOME}/.cabal` directory, and if
so, please remove them and try again.

### Compile and run HRC benchmarks

With the modified [Repa] installed, we can move on to compiling the
benchmark programs from [flrc-benchmarks]. These Haskell programs
are discussed in detail in the [Measuring the Haskell Gap][haskellgap] paper.

```
cd flrc-benchmarks
PATH=${PREFIX}/bin:$PATH cabal install --dependencies-only --global \
  --cabal-lib-version=1.16.1 --prefix=... --package-db=...
```

The above command only installs dependencies. To actually compile the programs
with HRC, we'll instead use `make`, either in the benchmarks directory, or in
each benchmark's own directory:

```
PATH=${PREFIX/bin:$PATH make
```

The verbosity setting of compiling these benchmarks are set to be 0, which
actually means some minimal information about the compilation process is
outputted. The last step in this compilation process is to use tools `pilicl`
and `pilink' from `flrc-lib` to compile Pillar programs to C, and then use a
C/C++ compiler (GCC by default) to compile and link to binary. If you want to
use ICC, you can run `make` like this:

```
HRC_CC=icc PATH=${PREFIX/bin:$PATH make
```

For now, HRC can be used directly to compile haskell programs. There is no
existing integration of choosing HRC as an alternative compiler when using
cabal. Our patched Cabal package for GHC 7.6 is only intended to install GHC's
external core together during package install, and to help with native library
linking.

To run the benchmarks, go to each directory, and run the executables like
this:

```
./<executable> @PPiler maxHeap 1024 --
```

The default settings for these benchmarks usually requires bigger memory, and
hence we use the `@PPiler` runtime options, and sometimes increasing the
memory beyond 1024M also helps.

All benchmarks should also compile and install just fine with a regular GHC, 
although they were only tested with GHC 7.6 and older version of Repa.

## For Nix Users

If you use [Nix], here is how you can spawn a sub-shell with needed tools
and packages to work with all (the compilation) of FLRC-LIB, FLRC and HRC:

```bash
nix-shell -p git curl wget m4 autoconf automake gcc nasm yacc flex libtool gmp \
mlton pkgconfig which perl haskell.compiler.ghc742 haskell.packages.ghc742.happy \
haskell.packages.ghc742.alex cabal-install
```

[nix]: https://nixos.org/nix
[happy]: https://hackage.haskell.org/package/happy
[alex]: https://hackage.haskell.org/package/alex
[repa]: https://hackage.haskell.org/package/repa
[cabal-install]: https://hackage.haskell.org/package/cabal-install
[haskellgap]: http://dl.acm.org/citation.cfm?doid=2620678.2620685
[flrc-benchmarks]: https://github.com/IntelLabs/flrc-benchmarks
