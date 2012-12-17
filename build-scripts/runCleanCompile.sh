#!/bin/bash -e

rm -f kontrakcja-test.tix
cabal update
cabal clean
cabal install --only-dependencies
cabal configure -ftest-coverage

if [ x$TEAMCITY_VERSION == x ]; then
  cabal build
else
  cabal build 2>&1 | runghc build-scripts/Teamcity.hs ghc
fi
