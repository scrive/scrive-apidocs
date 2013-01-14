#!/bin/bash

rm -f kontrakcja-test.tix
cabal install --only-dependencies
cabal configure -ftest-coverage
cabal build --ghc-options="-O0 -c"

if [ "$TEAMCITY_VERSION" = "" ]; then
  cabal build --ghc-options="-O0 -optl -O0"
else
  cabal build --ghc-options="-O0 -optl -O0" 2>&1 | runghc build-scripts/Teamcity.hs ghc
  exit ${PIPESTATUS[0]}
fi
