#!/usr/bin/env bash

set -eux

export CHROME_BIN=$(which chromium)

( cd frontend && npm install )
( cd frontend-elm && npm install )

cd frontend

grunt build --new-build
grunt test:fast --new-build
