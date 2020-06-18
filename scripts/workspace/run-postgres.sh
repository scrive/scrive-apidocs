#!/usr/bin/env bash

set -eax
trap 'kill 0' EXIT

KONTRAKCJA_WORKSPACE=${KONTRAKCJA_WORKSPACE:-`pwd -P`}
db_path="$KONTRAKCJA_WORKSPACE/_local/data"

echo "Running PostgreSQL from $db_path"
postgres -D "$db_path" -h "" -k "$db_path"
