#!/bin/bash

export PATH=/home/admin/bin:$PATH

while [ true ]; do
    echo "Starting dev..."
    #
    # here we are a bit strict
    # staging has lower stack size than production
    # so if any laziness errors follow we have early warnings
    #
    nice dist/build/kontrakcja-server/kontrakcja-server +RTS -K20M &
    echo $! > _pid
    echo "started server with pid" 
    more _pid
    wait
    echo "Process ended, restarting in 5 sek..."
    sleep 5
done
