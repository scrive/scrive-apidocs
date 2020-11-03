#!/usr/bin/env bash

set -eux

if [[ -v PDFTOOLS_CONFIG ]]
then
  echo "Using PDF Tools at AWS Lambda"
  set +x
  echo "$PDFTOOLS_CONFIG" > ./pdftools-lambda.local.json
  set -x
fi

export FAKES3_PORT=$(./ci/workflow/scripts/random-port.sh)
export SAM_PORT=$(./ci/workflow/scripts/random-port.sh)

./scripts/workspace/generate-config.sh

db_path="$(pwd)/_local/data"
supervisor_config="$(pwd)/ci/supervisord.conf"

trap "supervisorctl -c $supervisor_config shutdown" SIGINT SIGTERM EXIT

mkdir -p "$db_path"

initdb --pgdata "$db_path" --locale "en_US.UTF-8"

supervisord -c "$supervisor_config"

supervisorctl -c "$supervisor_config" start postgres fakes3

if [[ ! -v PDFTOOLS_CONFIG ]]
then
  supervisorctl -c "$supervisor_config" start sam
fi

supervisorctl -c "$supervisor_config" status postgres fakes3

createdb -h "$db_path" kontrakcja_test

set +e
cabal run kontrakcja-test -- --plain > logs/kontrakcja-test.log
exit_code=$?

if [ $exit_code -ne 0 ]
then
  echo "Some of the tests have failed:"
  grep -n '\[Failed\]' logs/kontrakcja-test.log
fi

echo "Test completed. Outputting the last 20 lines of test. Full log is available in logs/kontrakcja-test.log"
tail -n 20 logs/kontrakcja-test.log

exit $exit_code
