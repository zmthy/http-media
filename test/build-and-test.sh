#!/bin/sh

DIR=dist/hpc
rm -Rf $DIR
mkdir -p $DIR

EXCLUDES='Main
Network.HTTP.Accept.Tests
Network.HTTP.Accept.MediaType.Tests
Network.HTTP.Accept.MediaType.Gen'

EXCL=""

for e in $EXCLUDES; do
    EXCL="$EXCL --exclude=$e"
done

HPC="hpc markup $EXCL --destdir=$DIR tests"

cabal configure && cabal build && dist/build/tests/tests && $HPC

rm -f tests.tix

