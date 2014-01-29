#!/bin/bash

echo "DEV RUNNER:"
echo ""

echo "MIGRATING DATABASE"
dist/build/kontrakcja-migrate/kontrakcja-migrate


echo "STARTING MAILER SERVER"
    dist/build/mailing-server/mailing-server &
    echo $! > _mailer_pid
    echo "started mailer with pid $(cat _mailer_pid)"
    sleep 1

echo ""
echo "STARTING SMS SERVER"
    dist/build/messenger-server/messenger-server &
    echo $! > _mailer_pid
    echo "started sms sender with pid $(cat _mailer_pid)"
    sleep 1

echo ""

echo "STARTING CRON SERVER"
    dist/build/cron/cron &
    echo $! > _cron_pid
    echo "started mailer with pid $(cat _cron_pid)"
    sleep 1

echo ""
echo "STARTING GRUNT SERVER"
    cd frontend/
    grunt server &
    echo $! > _grunt_pid
    echo "started server with pid $(cat _grunt_pid)"
    cd ../

echo ""
echo "STARTING MAIN SERVER"
    dist/build/kontrakcja-server/kontrakcja-server "$@" &
    echo $! > _server_pid
    echo "started server with pid $(cat _server_pid)"

wait
