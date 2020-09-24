#!/usr/bin/env bash

set -eux

supervisor_config="$(pwd)/ci/supervisord.conf"

trap "supervisorctl -c $supervisor_config shutdown" SIGINT SIGTERM EXIT

supervisord -c "$supervisor_config"

supervisorctl -c "$supervisor_config" start selenium-server

supervisorctl -c "$supervisor_config" status || true

set +x
echo "$SELENIUM_CONFIG" > selenium-test/config.py
set -x

( cd selenium-test/$1 && xvfb-run ./run.py "${@:2}" )
