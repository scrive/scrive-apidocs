#!/usr/bin/env bash

set -eux

cabal v2-build kontrakcja-shake
cabal v2-run kontrakcja-shake -- "$@"
