#!/bin/bash -e

rm -f kontrakcja-test.tix
cabal update
cabal clean
cabal install --only-dependencies
cabal configure -ftest-coverage

if [ "$TEAMCITY_VERSION" == "" ]; then
  cabal build
else
  cabal build 2>&1 | runghc build-scripts/Teamcity.hs ghc
  exit ${PIPESTATUS[0]}
fi
