#!/bin/sh

if cabal configure && cabal build; then
    DIR=dist/hpc
    rm -Rf $DIR
    mkdir -p $DIR

    EXCLUDES='Main
    Network.HTTP.Accept.Gen
    Network.HTTP.Accept.Tests
    Network.HTTP.Accept.Match.Tests
    Network.HTTP.Accept.MediaType.Tests
    Network.HTTP.Accept.MediaType.Gen'

    EXCL=""

    for e in $EXCLUDES; do
        EXCL="$EXCL --exclude=$e"
    done

    dist/build/tests/tests
    hpc markup $EXCL --destdir=$DIR tests
fi

rm -f tests.tix

