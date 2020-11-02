#!/usr/bin/env bash

set -euxo pipefail

mkdir -p workspace

ls -la workspace

git status

git worktree add --force --detach workspace/base-branch $BASE_BRANCH

export KONTRAKCJA_ROOT="$(pwd)"
export KONTRAKCJA_WORKSPACE="$(pwd)"

(cd workspace/base-branch && git status)

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

supervisorctl -c "$supervisor_config" status || true

createdb -h "$db_path" kontrakcja_test

# Use kontrakcja_test db for both kontrakcja-migrate and kontrakcja-test
sed -i "s/dbname='kontrakcja'/dbname='kontrakcja_test'/g" kontrakcja.conf

master_migrate=$(nix-build -A ghc88.kontrakcja-project.kontrakcja.components.exes.kontrakcja-migrate workspace/base-branch/release.nix)

cabal configure --disable-optimization --enable-tests

cabal build kontrakcja-migrate
cabal build kontrakcja-test

$master_migrate/bin/kontrakcja-migrate

cabal run kontrakcja-migrate
cabal run kontrakcja-test
