#!/bin/bash

if [ -z "$1" ]; then
  echo "Usage: $0 <cabal-dev sandbox directory path>"
  exit 1
fi

rm -f kontrakcja-test.tix
cabal-dev --sandbox="$1" clean
cabal-dev --sandbox="$1" install --only-dependencies
cabal-dev --sandbox="$1" configure -ftest-coverage
cabal-dev --sandbox="$1" build --ghc-options="-O0 -c"

if [ "$TEAMCITY_VERSION" = "" ]; then
  cabal-dev --sandbox="$1" build --ghc-options="-O0 -optl -O0"
else
  cabal-dev --sandbox="$1" build --ghc-options="-O0 -optl -O0" 2>&1 | runghc build-scripts/Teamcity.hs ghc
  exit "${PIPESTATUS[0]}"
fi
