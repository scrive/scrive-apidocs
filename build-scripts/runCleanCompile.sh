#!/bin/bash -e

# This script assumes NGINX_CONF_PATH wich is a path to the include file that sends requests to php-fpm

if [ -z "$1" ]; then
  echo "Usage: $0 <cabal sandbox directory path>"
  exit 1
fi

cabal sandbox init --sandbox="$1"

rm -f kontrakcja-test.tix
cabal update
cabal clean
cabal install --enable-tests --only-dependencies --force-reinstalls
cabal configure --enable-tests -ftest-coverage -fenable-routinglist

if [ "$TEAMCITY_VERSION" = "" ]; then
  cabal build
else
  cabal build 2>&1 | runghc build-scripts/Teamcity.hs ghc
  RESULT="${PIPESTATUS[0]}"
  echo "Generating urls"
  if [ "$NGINX_CONF_PATH" = "" ]; then
    echo "No NGINX_CONF_PATH set, not generating routing list"
  else
    ./dist/build/routinglist/routinglist $NGINX_CONF_PATH
  fi
  exit "${RESULT}"
fi
