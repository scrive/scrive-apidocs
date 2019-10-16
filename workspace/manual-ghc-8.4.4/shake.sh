#!/usr/bin/env bash

set -eux

source "$(dirname "$0")/env.sh"

../scripts/shake.sh "$@"
