#!/bin/bash

errors=0

function run {
  mkdir -p selenium_screenshots
  for t in "$@"; do
    flvrec.py -P ~/.vnc_pwdfile -o $(basename "$t" .rb).flv localhost:1 &
    FLVREC_PID=$!
    rspec --colour --format documentation "$t" || errors=1
    kill -s SIGINT $FLVREC_PID
  done
}

if [ "$1" == "" ]; then
  echo "usage: $0 [all|<spec-name.rb>] [lang_code]"
  exit 1
fi
if [ "$2" == "" ]; then
    export SELENIUM_TEST_LANG=en
else
    export SELENIUM_TEST_LANG="$2"
fi
set -- "$1"  # clear second arg
if [ "$1" == "all" ]; then
  run $(dirname "$0")/src/specs/*.rb
else
  run "$@"
fi
if [ $errors != 0 ]; then
  echo "At least one test failed"
fi
exit $errors
