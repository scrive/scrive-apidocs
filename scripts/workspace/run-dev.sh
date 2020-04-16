#!/usr/bin/env bash

trap 'kill 0' EXIT

script_dir="$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )"

KONTRAKCJA_ROOT=${KONTRAKCJA_ROOT:-`pwd -P`}
KONTRAKCJA_WORKSPACE=${KONTRAKCJA_WORKSPACE:-"$KONTRAKCJA_ROOT"}

echo "Running dev setup"
echo "KONTRAKCJA_ROOT: $KONTRAKCJA_ROOT"
echo "KONTRAKCJA_WORKSPACE: $KONTRAKCJA_WORKSPACE"

"$script_dir/run-services.sh" &
sleep 5

"$script_dir/run-servers.sh" "$@" &

wait
