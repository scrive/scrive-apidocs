#!/bin/bash

cabal install --only-dependencies && cabal -ftest-coverage configure && cabal build --ghc-options="-O0 -c" && cabal build --ghc-options="-O0 -optl -O0 -optl --reduce-memory-overheads -optl --hash-size=3"

