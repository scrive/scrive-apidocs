#!/usr/bin/env bash

set -eux

# ls -la
# kontrakcja_dist=$(gunzip -c kontrakcja.nar.gz | nix-store --import | tail -n1)
kontrakcja_dist=result
ls -la "$kontrakcja_dist/"

set +x
echo "$PDFTOOLS_CONFIG" > ./pdftools-lambda.local.json
set -x

./scripts/workspace/generate-config.sh

db_path="$(pwd)/_local/data"
supervisor_config="$(pwd)/ci/supervisord.conf"

trap "supervisorctl -c $supervisor_config shutdown" SIGINT SIGTERM EXIT

mkdir -p "$db_path"

initdb --pgdata "$db_path" --locale "en_US.UTF-8"

supervisord -c "$supervisor_config"

supervisorctl -c "$supervisor_config" start postgres fakes3

supervisorctl -c "$supervisor_config" status postgres fakes3

createdb -h "$db_path" kontrakcja_test

"$kontrakcja_dist/bin/kontrakcja-test"
