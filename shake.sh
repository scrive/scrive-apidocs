#!/bin/sh
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
mkdir -p _shake
export SHAKE_BUILD_CABAL_CONFIGURE_OPTS="--ghc-options=\"-fno-warn-deprecated-flags\""
ghc --make Shake/Shake.hs -Wall -rtsopts -with-rtsopts=-I0 -outputdir=_shake -o _shake/build && _shake/build --color "$@"
