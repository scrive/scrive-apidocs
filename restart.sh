#!/bin/bash
KONTRA_DIR="$( cd -P "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

kill `cat $KONTRA_DIR/_pid`
kill `cat $KONTRA_DIR/_mailer_pid`