#!/usr/bin/env bash

set -eux

nix_deps_archive=$(nix-build --no-out-link nix/releases/nix-deps.nix)
nix_deps=$(mktemp -d -t kontrakcja-nix-XXXX)
( cd $nix_deps && tar xzf $nix_deps_archive/kontrakcja-nix.tar.gz )

plan=$(nix-build --no-out-link $nix_deps/nix/releases/plan.nix)

rm -rf nix/plans

cp -r $plan nix/plans

find nix/plans/ -type d -exec chmod 755 {} \;

rm -rf $nix_deps
