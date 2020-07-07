#!/usr/bin/env bash

set -eux

docker build \
  --tag kontrakcja \
  --build-arg UID=$(id -u) \
  --build-arg GID=$(id -g) \
  -f ci/Dockerfile ci

echo '$KONTRAKCJA_TEMP:' $KONTRAKCJA_TEMP

if [ ${CLEAR_KONTRAKCJA_TEMP:-0} = 1 ]
then
  echo 'Clearing $KONTRAKCJA_TEMP directory at' $KONTRAKCJA_TEMP
  chmod -R +w "$KONTRAKCJA_TEMP/nix/store"
  rm -rf "$KONTRAKCJA_TEMP"
fi

mkdir -p "$KONTRAKCJA_TEMP"
ls -la "$KONTRAKCJA_TEMP"

mkdir -p "$KONTRAKCJA_TEMP/nix" "$KONTRAKCJA_TEMP/.cache/nix" "$KONTRAKCJA_TEMP/.cabal"

docker run --rm \
  -v `pwd`:/data/kontrakcja \
  -v "$KONTRAKCJA_TEMP/nix:/nix" \
  -v "$KONTRAKCJA_TEMP/.cache/nix:/home/kontrakcja/.cache/nix" \
  -v "$KONTRAKCJA_TEMP/.cabal:/home/kontrakcja/.cabal" \
  -v "$SSH_AUTH_SOCK:/var/run/ssh-agent.sock" \
  --env GHC_TARGET \
  --env CACHIX_AUTH_TOKEN \
  --env PDFTOOLS_CONFIG \
  kontrakcja \
  "$@"
