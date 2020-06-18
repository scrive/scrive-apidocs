#!/usr/bin/env bash

set -eux

mkdir -p workspace

export KONTRAKCJA_ROOT=`pwd`

export KONTRAKCJA_WORKSPACE="${KONTRAKCJA_ROOT}/$(mktemp -d -p workspace)"

echo "building in $KONTRAKCJA_WORKSPACE"

cd "$KONTRAKCJA_WORKSPACE"

mkdir -p supervisor/logs
ln -s ../../scripts/workspace/shake.sh

pattern='s;packages: . ./Shake;packages: ../.. ../../Shake;'
sed "$pattern" ../../cabal.project > cabal.project
sed "$pattern" ../../cabal-nix.project > cabal-nix.project
cp ../../cabal.project.freeze ./

cabal v2-update
./shake.sh all

set +x
echo "$PDFTOOLS_CONFIG" > ./pdftools-lambda.local.json
set -x

"$KONTRAKCJA_ROOT"/scripts/workspace/generate-config.sh

db_path="$KONTRAKCJA_WORKSPACE/_local/data"

mkdir -p "$db_path"

initdb --pgdata "$db_path" --locale "en_US.UTF-8"

supervisord -c "$KONTRAKCJA_ROOT"/docker/supervisord.conf

supervisorctl -c "$KONTRAKCJA_ROOT"/docker/supervisord.conf start all

supervisorctl -c "$KONTRAKCJA_ROOT"/docker/supervisord.conf status

createdb -h "$db_path" kontrakcja_test

cabal v2-run kontrakcja-test

supervisorctl -c "$KONTRAKCJA_ROOT"/docker/supervisord.conf stop all
