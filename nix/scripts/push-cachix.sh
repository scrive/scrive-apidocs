#!/usr/bin/env bash

set -eux

GHC_VERSION=${GHC_VERSION:-ghc88}
CACHIX_STORE=${CACHIX_STORE:-scrive}

nix_deps_archive=$(nix-build --no-out-link nix/releases/nix-deps.nix)
nix_deps=$(mktemp -d -t kontrakcja-nix-XXXX)
( cd $nix_deps && tar xzf $nix_deps_archive/kontrakcja-nix.tar.gz )

instances=$(nix-instantiate -A $GHC_VERSION.shell-deps $nix_deps/release.nix)

nix-store -qR --include-outputs $instances | cachix push $CACHIX_STORE

rm -rf $nix_deps
