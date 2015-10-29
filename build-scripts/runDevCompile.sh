#!/bin/bash -e

# This script assumes NGINX_CONF_PATH wich is a path to the include file that sends requests to php-fpm

if [ ! -e cabal_config_freeze.sh ]; then
  echo "No cabal_config_freeze.sh found (this script must be run from the top directory)."
  exit 1
fi

if [ -z "$1" ]; then
  echo "Usage: $0 <cabal sandbox directory path>"
  exit 1
fi

cabal sandbox init --sandbox="$1"

rm -f kontrakcja-test.tix
cabal update
cabal clean
cabal install --only-dependencies --force-reinstalls
# ./cabal_config_freeze.sh  Disabled since it was not working on build server. Ask GP for details
cabal configure -ftest-coverage

if [ "$TEAMCITY_VERSION" = "" ]; then
  cabal build --ghc-options="-O0 -optl -O0"
else
  cabal build --ghc-options="-O0 -optl -O0" 2>&1 | runghc build-scripts/Teamcity.hs ghc
  exit "${PIPESTATUS[0]}"
fi

echo "Generating urls"
./scripts/UrlList.sh $NGINX_CONF_PATH
