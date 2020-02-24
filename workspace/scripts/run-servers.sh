#!/usr/bin/env bash

trap 'kill 0' EXIT

script_dir="$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )"

export KONTRAKCJA_ROOT=${KONTRAKCJA_ROOT:-`pwd -P`}
export KONTRAKCJA_WORKSPACE=${KONTRAKCJA_WORKSPACE:-"$KONTRAKCJA_ROOT"}

echo "Running Kontrakcja services"

cabal v2-run kontrakcja-migrate

cabal v2-run mailing-server &
cabal v2-run messenger-server &
cabal v2-run cron > cron.log 2>&1 &
cabal v2-run kontrakcja-server &

if [ -z "$1" ]; then
    "$script_dir/shake.sh" frontend &
fi

wait
