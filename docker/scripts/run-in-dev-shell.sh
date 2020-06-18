#!/usr/bin/env bash

set -eux

source ./docker/scripts/setup-nix.sh

nix-shell \
  -A $GHC_TARGET.dev-shell release.nix \
  --run "$*"
