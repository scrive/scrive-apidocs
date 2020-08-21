#!/usr/bin/env bash

set -eux

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

set +e
cabal run kontrakcja-test -- --plain > logs/kontrakcja-test.log
exit_code=$?

if [ $exit_code -ne 0 ]
then
  echo "Some of the tests have failed:"
  grep -n '\[Failed\]' logs/kontrakcja-test.log
fi

echo "Test completed. Outputting the last 50 lines of test. Full log is available in logs/kontrakcja-test.log"
tail -n 50 logs/kontrakcja-test.log

exit $exit_code
