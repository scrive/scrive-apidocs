#!/usr/bin/env bash

set -eux

./ci/scripts/run-with-nix.sh \
  nix-shell \
    -A $GHC_TARGET.manual-shell release.nix \
    --run "$*"
