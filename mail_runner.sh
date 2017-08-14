#!/bin/bash

# Note: if you're using new-build, this assumes that './shake.sh
# --new-build dist' is being used to build the project.

export PATH=/home/admin/bin:$PATH

while [ true ]; do
    echo "Starting mailer..."
    #
    # here we are a bit strict
    # staging has lower stack size than production
    # so if any laziness errors follow we have early warnings
    #
    nice dist/build/mailing-server/mailing-server &
    echo $! > _mailer_pid
    echo "started mailing server with pid"
    more _mailer_pid
    wait
    echo "Process ended, restarting in 5 sek..."
    sleep 5
done
