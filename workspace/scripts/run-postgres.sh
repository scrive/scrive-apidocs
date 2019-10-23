#!/usr/bin/env bash

set -eax
trap 'kill 0' EXIT

KONTRAKCJA_WORKSPACE=${KONTRAKCJA_WORKSPACE:-`pwd -P`}
db_path="$KONTRAKCJA_WORKSPACE/_local/data"

if [[ ! -e "$db_path/postmaster.pid" ]]; then
  echo "Running PostgreSQL from $db_path"
  postgres -D "$db_path" -h "" -k "$db_path"
else
  echo "PostgreSQL is already running"
fi
