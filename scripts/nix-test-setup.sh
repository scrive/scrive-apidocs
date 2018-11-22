#!/usr/bin/env bash

base="$(dirname "$(readlink -f "$0")")"

scrivepdftools="$1"

. "$base/../nix/kontrakcja-test-setup.sh"

shift
if [[ "$@" != "" ]]; then
  eval "$@"
fi
