#!/usr/bin/env bash

set -euxo pipefail

nix-prefetch-git ssh://git@github.com/scrive/new_frontend.git \
  > nix/source/new-frontend.json

result=$(nix-instantiate --eval --json nix/source/new-frontend.nix | tr -d '"')

cp $result/package.json $result/package-lock.json nix/node-deps/new-frontend/

( cd nix/node-deps/new-frontend && \
  node2nix -c default-original.nix --nodejs-12 -d -l package-lock.json
)
