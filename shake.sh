#!/bin/sh
mkdir -p _shake
ghc --make Shake/Shake.hs -Werror -Wall -rtsopts -with-rtsopts=-I0 -outputdir=_shake -o _shake/build && _shake/build --color "$@"
