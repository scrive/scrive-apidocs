#!/bin/sh

set -eux


export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
rm -f ./.ghc.environment.*

# Check that the prerequisites for running the shake script are
# installed.
ghc-pkg list --user --global > ghc-pkg.list

if grep shake ghc-pkg.list > /dev/null 2>&1
then
    echo "Shake is already installed."
else
    echo "Shake is not installed, please run 'cabal install shake'."
    exit 1
fi

if grep aeson ghc-pkg.list > /dev/null 2>&1
then
    echo "aeson is already installed."
else
    echo "aeson is not installed, please run 'cabal install aeson'."
    exit 1
fi

rm ghc-pkg.list

# Build and run the Shake script.
mkdir -p _shake
ghc --make Shake/Shake.hs -Wall -Werror -rtsopts -with-rtsopts=-I0 \
    -outputdir=_shake -o _shake/build && _shake/build --color "$@"
