#!/bin/sh

cabal configure && cabal build && dist/build/tests/tests

