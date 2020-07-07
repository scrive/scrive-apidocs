#!/usr/bin/env bash

set -eux

source ./ci/scripts/setup-nix.sh

"$@"
