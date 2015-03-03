#!/bin/bash -e

if [ -z "$1" ]; then
  echo "Usage: $0 <cabal sandbox directory path> [ghc-options]"
  exit 1
fi

cabal sandbox init --sandbox="$1"

rm -f kontrakcja-test.tix
cabal update
cabal clean
cabal install --only-dependencies --force-reinstalls --enable-library-profiling
cabal configure -ftest-coverage --enable-executable-profiling

if [ "$TEAMCITY_VERSION" = "" ]; then
  cabal build
else
  cabal build 2>&1 | runghc build-scripts/Teamcity.hs ghc
  exit "${PIPESTATUS[0]}"
fi
