#!/usr/bin/env bash

set -eux
export CHROME_BIN=$(which chromium)

( cd frontend && npm install )
( cd frontend-elm && npm install )

pushd frontend

npm run build:nix
npm test

popd

pushd frontend-elm

npm run validate-format

popd
