#!/usr/bin/env bash

set -eux

export BUILD_DATE=$(date -u +%Y%m%d-%H%M%S)

./shake.sh --enable-optimisation --disable-executable-dynamic dist
