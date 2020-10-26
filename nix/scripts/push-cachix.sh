#!/usr/bin/env bash

set -euxo pipefail

CACHIX_STORE=${CACHIX_STORE:-scrive}

nix_deps_archive=$(nix-build --no-out-link nix/releases/nix-deps.nix)
nix_deps=$(mktemp -d -t kontrakcja-nix-XXXX)
( cd $nix_deps && tar xzf $nix_deps_archive/kontrakcja-nix.tar.gz )

instances=(
  $(nix-instantiate -A ghc88.shell-deps $nix_deps/release.nix)
  $(nix-instantiate -A ghc810.shell-deps $nix_deps/release.nix)
  $(nix-instantiate -A api-docs $nix_deps/release.nix)
  $(nix-instantiate $nix_deps/nix/releases/new-frontend.nix)
  $(nix-instantiate $nix_deps/nix/releases/plan.nix)
)

for instance in "${instances[@]}"
do
  nix-store -qR --include-outputs $instance \
    | xargs nix-store --query --requisites \
    | cachix push $CACHIX_STORE
done

rm -rf $nix_deps
