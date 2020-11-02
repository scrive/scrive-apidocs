#!/usr/bin/env bash

set -euxo pipefail

nix_collect_garbage=${nix_collect_garbage:-true}

$nix_collect_garbage
df -h

ghc_versions=(
  "ghc88"
  "ghc810"
)

ghc_builds=(
  "shell-deps"
  "lint-shell"
  "dist-shell"
  "backend-shell"
  "frontend-shell"
  "selenium-shell"
  "detect-unused-shell"
  "manual-backend-shell"
  "kontrakcja-frontend"
)

other_builds=(
  "new-frontend"
  "haskell-plans"
)

nix_deps_archive=$(nix-build --no-out-link nix/releases/nix-deps.nix)
nix_deps=$(mktemp -d -t kontrakcja-nix-XXXX)
( cd $nix_deps && tar xzf $nix_deps_archive/kontrakcja-nix.tar.gz )

function cache_deps {
  nix-shell -j4 -A $1 $nix_deps/release.nix --run true
  instance=$(nix-instantiate -A "$1" $nix_deps/release.nix)
  nix-store -qR --include-outputs $instance \
      | xargs nix-store --query --requisites \
      | cachix push scrive
}

for ghc_version in "${ghc_versions[@]}"
do
  for ghc_build in "${ghc_builds[@]}"
  do
    cache_deps "$ghc_version.$ghc_build"
  done
done


for build in "${other_builds[@]}"
do
  cache_deps "$build"
done

rm -rf $nix_deps
$nix_collect_garbage
df -h
