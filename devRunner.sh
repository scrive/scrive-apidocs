#!/bin/bash

echo "DEV RUNNER:" 
echo ""

echo "STARTING MAILER SERVER"
    dist/build/mailing-server/mailing-server &
    echo $! > _mailer_pid
    echo "started mailer with pid" 
    sleep 1

echo ""
echo "STARTING MAIN SERVER"
    dist/build/kontrakcja-server/kontrakcja-server "$@" &
    echo $! > _server_pid
    echo "started server with pid" 
    more _pid

wait

   
