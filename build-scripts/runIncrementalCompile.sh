#!/bin/bash

cabal install --only-dependencies && cabal configure && cabal build --ghc-options="-O0 -c" && cabal build --ghc-options="-O0 -optl -O0 -optl --reduce-memory-overheads -optl --hash-size=3"

