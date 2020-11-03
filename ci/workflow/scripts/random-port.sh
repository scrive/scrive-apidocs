#!/usr/bin/env bash

set -eu

(
  nc -v -n -l 0 2>&1 &
  pid=$!
  sleep 0.1
  kill $pid
) | cut -d ' ' -f 4
