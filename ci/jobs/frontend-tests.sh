#!/usr/bin/env bash

set -eux

./ci/scripts/run-in-docker.sh \
  ./ci/scripts/run-with-nix.sh \
    nix-shell \
      --arg extra-run-deps "pkgs: hsPkgs: [ pkgs.chromium ]" \
      -A $GHC_TARGET.dev-shell \
      release.nix \
      --run "./ci/scripts/run-frontend-tests.sh"
