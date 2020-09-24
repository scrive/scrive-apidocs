#!/usr/bin/env bash

set -eux

# This is set to nix-collect-garbage to clean up Nix store
# after each shell cache is pushed, to prevent self hosted
# runner on AWS Fargate from running out of disk space.
nix_collect_garbage=${nix_collect_garbage:-true}

$nix_collect_garbage
df -h

nix_deps=$(nix-build --no-out-link nix/releases/nix-deps.nix)
mkdir -p result
( cd result && tar xzf $nix_deps/kontrakcja-nix.tar.gz )

nix-shell -j4 -A $1 result/release.nix --run true

nix-store -qR --include-outputs $(nix-instantiate -A $1 result/release.nix) \
  | cachix push scrive

rm -rf result
$nix_collect_garbage
df -h
