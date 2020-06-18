#!/usr/bin/env bash

set -eux

docker build \
  --tag kontrakcja \
  --build-arg UID=$(id -u) \
  --build-arg GID=$(id -g) \
  -f docker/Dockerfile docker

echo '$KONTRAKCJA_TEMP:' $KONTRAKCJA_TEMP

mkdir -p "$KONTRAKCJA_TEMP"
ls -la "$KONTRAKCJA_TEMP"

mkdir -p "$KONTRAKCJA_TEMP/nix" "$KONTRAKCJA_TEMP/cache" "$KONTRAKCJA_TEMP/cabal"

docker run --rm \
  -v `pwd`:/data/kontrakcja \
  -v "$KONTRAKCJA_TEMP/nix:/nix" \
  -v "$KONTRAKCJA_TEMP/cache:/home/kontrakcja/.cache/nix" \
  -v "$KONTRAKCJA_TEMP/cabal:/home/kontrakcja/.cabal" \
  -v "$SSH_AUTH_SOCK:/var/run/ssh-agent.sock" \
  --env CACHIX_TOKEN \
  --env GHC_TARGET \
  --env PDFTOOLS_CONFIG \
  kontrakcja \
  "$@"
