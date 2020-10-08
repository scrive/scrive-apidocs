#! /usr/bin/env nix-shell
#! nix-shell -i bash -p postgresql -p jq

if [ $# -ne 1 ]; then
    echo "$0 <kontrakcja_config>" > /dev/stderr
    exit 1
fi

DB_CONNECTION_STRING="$(jq .database -r < $1)"
psql -d "${DB_CONNECTION_STRING}"
