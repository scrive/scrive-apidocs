#!/usr/bin/env bash

set -eux

( cd frontend && npm install )
( cd frontend-elm && npm install )

./shake.sh frontend
./shake.sh test-frontend
