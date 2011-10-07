#!/bin/bash

rm -f kontrakcja-test.tix && cabal update && cabal clean && cabal install --only-dependencies && cabal configure -ftest-coverage && cabal build

