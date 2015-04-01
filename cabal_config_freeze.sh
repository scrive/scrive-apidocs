#!/bin/bash
#
# This cabal_config_freeze.sh script makes sure that cabal.config is up to date.
#
# Running cabal_config_freeze.sh without arguments checks if current
# cabal.config has exact version information for every used
# dependency. It returns nonzero code if it is not the case.
# cabal_config_freeze.sh should be used from a build system, for
# example TeamCity.
#
# Running `cabal_config_freeze.sh update` updates `cabal.config`.
#
# Note that `cabal freeze` produces too strict rules in `cabal.config`
# as those include current GHC version and base packages. To relax
# this a little we remove offending lines from `cabal.config`.
#



set -o nounset
set -o errexit

if [ -d .git ]; then
    git checkout cabal.config
fi

cp cabal.config cabal.config.original
cabal freeze "$@"
sed                             \
     -e '/ array /d'            \
     -e '/ base /d'             \
     -e '/ containers /d'       \
     -e '/ deepseq /d'          \
     -e '/ directory /d'        \
     -e '/ ghc-prim /d'         \
     -e '/ integer-gmp /d'      \
     -e '/ kontrakcja /d'       \
     -e '/ old-locale /d'       \
     -e '/ old-time /d'         \
     -e '/ pretty /d'           \
     -e '/ rts /d'              \
     -e '/ template-haskell /d' \
     -e '/ temporary /d'        \
     -e '/ time /d'             \
     -e '/ unix /d'             \
     < cabal.config > cabal.config.fixed

if [ "${1-}" == "update" ]; then
    mv cabal.config.fixed cabal.config
    echo "Run 'git commit cabal.config'"
elif ! diff cabal.config.original cabal.config.fixed; then
    mv cabal.config.original cabal.config
    echo "cabal.config is out of date" >&2
    echo "To update it you need to run" >&2
    echo "   $0 update" >&2
    echo "and then commit yout changes to cabal.config" >&2
    exit 3
else
    mv cabal.config.original cabal.config
    echo "cabal.config is up to date"
fi
