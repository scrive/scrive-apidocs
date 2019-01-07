#!/usr/bin/env bash

base="$(dirname "$(readlink -f "$0")")"

. "$base/../nix/kontrakcja-test-setup.sh"

if [[ "$@" != "" ]]; then
  eval "$@"
fi
