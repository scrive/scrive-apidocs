#!/usr/bin/env bash

set -eax
trap 'kill 0' EXIT

script_dir="$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )"

KONTRAKCJA_WORKSPACE=${KONTRAKCJA_WORKSPACE:-`pwd -P`}
db_path="$KONTRAKCJA_WORKSPACE/_local/data"

LANG=${LANG:-"C.UTF-8"}
PG_LOCALE=${PG_LOCALE:-"$LANG"}

if [[ ! -e "$db_path" ]]; then
  mkdir -p "$db_path"

  echo "Initializing database store at $db_path"
  initdb --pgdata "$db_path" --locale "$PG_LOCALE"

  echo "Starting postgres"
  $script_dir/run-postgres.sh &
  sleep 3

  echo "Creating kontrakcja db"
  createdb -h "$db_path" kontrakcja

  echo "Creating kontrakcja_test db"
  createdb -h "$db_path" kontrakcja_test

else
  echo "database store is already initialized at" $db_path
fi
