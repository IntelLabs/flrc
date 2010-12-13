#!/bin/bash

# Make a version.sml file with brief version and build information for the PPiler.
# usage: make-version.sh version lang_version outfile
# where:
#   version is the PPiler version string
#   lang_version is the P language version string
#   outfile is the filename to put the output into

version=$1
lang=$2
out=$3
build="`date '+%F %R'` on `hostname`"

rm -f $out

echo "structure Version =" >> $out
echo "struct" >> $out
echo "  val ppilerVersion = \"$version\"" >> $out
echo "  val langVersion = \"$lang\"" >> $out
echo "  val build = \"$build\"" >> $out
echo "end" >> $out
