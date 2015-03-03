#!/bin/bash -e

if [ -z "$1" ]; then
  echo "Usage: $0 <cabal sandbox directory path>"
  exit 1
fi

cabal sandbox init --sandbox="$1"

rm -f kontrakcja-test.tix
./cabal_config_freeze.sh
cabal update
cabal clean
cabal install --only-dependencies --force-reinstalls
cabal configure -ftest-coverage

if [ "$TEAMCITY_VERSION" = "" ]; then
  cabal build --ghc-options="-O0 -optl -O0"
else
  cabal build --ghc-options="-O0 -optl -O0" 2>&1 | runghc build-scripts/Teamcity.hs ghc
  exit "${PIPESTATUS[0]}"
fi


