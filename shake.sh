#!/usr/bin/env bash

set -eux

script_dir="$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )"
source "$script_dir/env.sh"

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

cabal v2-update
cabal v2-build kontrakcja-shake
cabal v2-run kontrakcja-shake -- --color "$@"
