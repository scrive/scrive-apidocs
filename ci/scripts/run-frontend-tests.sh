#!/usr/bin/env bash

set -eux

export CHROME_BIN=$(which chromium)

( cd frontend && npm install )
( cd frontend-elm && npm install )

./shake.sh frontend
./shake.sh test-frontend
