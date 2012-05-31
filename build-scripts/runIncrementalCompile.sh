#!/bin/bash

rm -f kontrakcja-test.tix && cabal install --only-dependencies --reinstall --force-reinstalls && cabal configure -ftest-coverage && cabal build --ghc-options="-O0 -c" && cabal build --ghc-options="-O0 -optl -O0 -optl --reduce-memory-overheads -optl --hash-size=3"

