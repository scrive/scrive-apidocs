#!/usr/bin/env bash

KONTRAKCJA_WORKSPACE=${KONTRAKCJA_WORKSPACE:-"."}
db_path="$KONTRAKCJA_WORKSPACE/_local/data"

if [[ ! -e "$db_path/postmaster.pid" ]]; then
  echo "Running PostgreSQL from $db_path"
  postgres -D "$db_path" -h "" -k "$db_path"
else
  echo "PostgreSQL is already running"
fi
