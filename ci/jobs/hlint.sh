#!/usr/bin/env bash

set -eux

./ci/scripts/run-in-docker.sh \
  ./ci/scripts/run-in-dev-shell.sh \
    ./shake.sh hlint
