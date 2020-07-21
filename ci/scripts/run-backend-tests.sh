#!/usr/bin/env bash

set -eux

set +x
echo "$PDFTOOLS_CONFIG" > ./pdftools-lambda.local.json
set -x

./scripts/workspace/generate-config.sh

db_path="$(pwd)/_local/data"
supervisor_config="$(pwd)/ci/supervisord.conf"

mkdir -p "$db_path"

initdb --pgdata "$db_path" --locale "en_US.UTF-8"

supervisord -c "$supervisor_config"

supervisorctl -c "$supervisor_config" start postgres fakes3

supervisorctl -c "$supervisor_config" status

createdb -h "$db_path" kontrakcja_test

cabal update

./shake.sh server

cabal run kontrakcja-test -- --plain --output-dir _build/test-outputs

supervisorctl -c "$supervisor_config" stop all
