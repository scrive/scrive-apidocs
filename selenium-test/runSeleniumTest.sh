#!/bin/bash

errors=0

function run {
  for t in "$@"; do
    flvrec.py -o $(basename "$t").flv localhost:0 &
    FLVREC_PID=$!
    env  # debug this thing
    spec --colour --format specdoc "$t" || errors=1
    kill -s SIGINT $FLVREC_PID
  done
}

if [ "$1" == "" ]; then
  echo "usage: $0 [all|<spec-name.rb>]"
  exit 1
fi
if [ "$1" == "all" ]; then
  run $(dirname "$0")/src/specs/*.rb
else
  run "$@"
fi
exit $errors
