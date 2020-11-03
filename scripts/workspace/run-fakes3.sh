#!/usr/bin/env bash

set -eux

trap 'kill 0' EXIT

KONTRAKCJA_ROOT=${KONTRAKCJA_ROOT:-`pwd -P`}
KONTRAKCJA_WORKSPACE=${KONTRAKCJA_WORKSPACE:-"$KONTRAKCJA_ROOT"}
FAKES3_PORT=${FAKES3_PORT:-4568}

storage_path="$KONTRAKCJA_WORKSPACE/_local/s3files"

mkdir -p "$storage_path"

echo "Running Fake S3 on port $FAKES3_PORT at $storage_path"

fakes3 --root "$storage_path" --port "$FAKES3_PORT"
