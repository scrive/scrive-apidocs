#!/usr/bin/env bash

set -eax
trap 'kill 0' EXIT

KONTRAKCJA_WORKSPACE=${KONTRAKCJA_WORKSPACE:-`pwd -P`}
db_path="$KONTRAKCJA_WORKSPACE/_local/data"
pid_file="$db_path/postmaster.pid"

if [[ ! -e "$pid_file" ]]; then
  echo "Running PostgreSQL from $db_path"
  postgres -D "$db_path" -h "" -k "$db_path"
else
  pid=$(head -n 1 "$pid_file")
  if ! kill -0 $pid > /dev/null 2>&1; then
    rm "$pid_file"
    echo "Running PostgreSQL from $db_path"
    postgres -D "$db_path" -h "" -k "$db_path"
  else
    echo "PostgresSQL is already running, following it."
    tail -f /dev/null --pid "$pid"
  fi
fi
