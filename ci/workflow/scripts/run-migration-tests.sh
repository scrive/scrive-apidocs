#!/usr/bin/env bash

set -eux

mkdir -p workspace

ls -la workspace

git status

git worktree add --force --detach workspace/base-branch $BASE_BRANCH

(cd workspace/base-branch && git status)

nix-build -j2 -o base-release -A ghc88.dist workspace/base-branch/release.nix

nix-build -j2 -o current-release -A ghc88.dist release.nix

export KONTRAKCJA_WORKSPACE="$(pwd)"

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

./base-release/bin/kontrakcja-migrate
./current-release/bin/kontrakcja-migrate
./current-release/bin/kontrakcja-test
