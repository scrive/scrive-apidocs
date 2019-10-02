#!/usr/bin/env bash

KONTRAKCJA_WORKSPACE=${KONTRAKCJA_WORKSPACE:-"."}
storage_path="$KONTRAKCJA_WORKSPACE/_local/s3files"

mkdir -p "$storage_path"

echo "Running Fake S3 at $storage_path"

fakes3 --root "$storage_path" --port 4568
