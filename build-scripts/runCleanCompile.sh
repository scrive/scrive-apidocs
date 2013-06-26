#!/bin/bash -e

if [ -z "$1" ]; then
  echo "Usage: $0 <cabal-dev sandbox directory path>"
  exit
fi

rm -f kontrakcja-test.tix
cabal-dev --sandbox="$1" update
cabal-dev --sandbox="$1" clean
cabal-dev --sandbox="$1" install --only-dependencies
cabal-dev --sandbox="$1" configure -ftest-coverage

if [ "$TEAMCITY_VERSION" = "" ]; then
  cabal-dev --sandbox="$1" build
else
  cabal-dev --sandbox="$1" build 2>&1 | runghc build-scripts/Teamcity.hs ghc
  exit "${PIPESTATUS[0]}"
fi
