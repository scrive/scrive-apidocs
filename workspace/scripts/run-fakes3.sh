#!/usr/bin/env bash

KONTRAKCJA_ROOT=${KONTRAKCJA_ROOT:-`pwd -P`}
KONTRAKCJA_WORKSPACE=${KONTRAKCJA_WORKSPACE:-"$KONTRAKCJA_ROOT"}

storage_path="$KONTRAKCJA_WORKSPACE/_local/s3files"

mkdir -p "$storage_path"

echo "Running Fake S3 at $storage_path"

fakes3 --root "$storage_path" --port 4568
