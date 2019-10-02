#!/usr/bin/env bash

set -eux

source "$(dirname "$0")/env.sh"

cabal v2-build kontrakcja-shake
cabal v2-run kontrakcja-shake -- $@
