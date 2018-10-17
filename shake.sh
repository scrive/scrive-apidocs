#!/bin/sh

set -eux


export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

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
# The list of packages in scope MUST be kept in sync with Shake.GetHsDeps.
mkdir -p _shake
ghc -hide-all-packages \
    -package Cabal \
    -package aeson \
    -package attoparsec \
    -package base \
    -package bytestring \
    -package containers \
    -package directory \
    -package extra \
    -package filepath \
    -package mtl \
    -package pretty \
    -package process \
    -package shake \
    -package text \
    -package time \
    -package unordered-containers \
    --make Shake/Shake.hs -Wall -Werror -rtsopts -with-rtsopts=-I0 \
    -outputdir=_shake -o _shake/build && _shake/build --color "$@"
