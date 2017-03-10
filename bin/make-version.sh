#!/bin/bash

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

# Make a version.sml file with brief version and build information for the flrc.
# usage: make-version.sh version outfile
# where:
#   version is the flrc version string
#   outfile is the filename to put the output into

version=$1
out=$2
prefix=$3
# Windows hostname outputs a final carriage return
build="`date '+%F %R'` on `hostname | tr -d '\r'`"

rm -f $out

echo "structure Version =" >> $out
echo "struct" >> $out
echo "  val flrcVersion = \"$version\"" >> $out
echo "  val build = \"$build\"" >> $out
echo "  val prefix = Path.fromString \"$prefix\"" >> $out
echo "end" >> $out
