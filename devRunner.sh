#!/usr/bin/env bash

set -eax

script_dir="$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )"
source "$script_dir/env.sh"

echo "DEV RUNNER:"
echo ""

echo "MIGRATING DATABASE"
cabal v2-run kontrakcja-migrate

echo ""
echo "STARTING S3 SERVER"
    mkdir -p s3files
    fakes3 --root s3files --port 4568 &
    echo $! > _s3_pid
    echo "started S3 server with pid $(cat _s3_pid)"
    sleep 1

if grep --quiet ".*gateway_url.*:.*localhost.*" kontrakcja.conf; then
    echo ""
    echo "STARTING LOCAL LAMBDA"
    sam local start-api &
    echo $! > _lambda_pid
    echo "started local lambda server with pid $(cat _lambda_pid)"
fi

echo "STARTING MAILER SERVER"
    cabal v2-run mailing-server &
    echo $! > _mailer_pid
    echo "started mailer with pid $(cat _mailer_pid)"
    sleep 1

echo ""
echo "STARTING SMS SERVER"
    cabal v2-run messenger-server &
    echo $! > _mailer_pid
    echo "started sms sender with pid $(cat _mailer_pid)"
    sleep 1

echo ""
echo "STARTING CRON SERVER"
    cabal v2-run cron &
    echo $! > _cron_pid
    echo "started mailer with pid $(cat _cron_pid)"
    sleep 1

# if there's an argument, dont start grunt
if [ -z "$1" ]; then
    echo ""
    echo "STARTING GRUNT SERVER"
    if grep --quiet ".*production.*:.*true.*" kontrakcja.conf; then
        GRUNT_TASK="server:dist"
    else
        GRUNT_TASK="server"
    fi
    cd frontend/
    grunt "${GRUNT_TASK}" &
    echo $! > _grunt_pid
    echo "started server with pid $(cat _grunt_pid)"
    cd ../
else
    shift
fi

echo ""
echo "STARTING MAIN SERVER"
    cabal v2-run kontrakcja-server "$@" &
    echo $! > _server_pid
    echo "started server with pid $(cat _server_pid)"

wait
