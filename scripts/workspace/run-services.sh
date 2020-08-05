#!/usr/bin/env bash

trap 'kill 0' EXIT

script_dir="$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )"

echo "Running external services"

$script_dir/run-postgres.sh &
$script_dir/run-fakes3.sh &
$script_dir/run-sam.sh &
$script_dir/run-nginx.sh &

wait
