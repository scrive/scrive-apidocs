#!/usr/bin/env bash

set -eux
source $(dirname "$0")/env.sh

workspace/scripts/run-dev.sh "$@"
