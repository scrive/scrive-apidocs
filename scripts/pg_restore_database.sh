#!/bin/bash

echo "Executing psql restore as:"
echo "    gunzip -c $3.gz | psql -d \"user=\'$1\' password=\'\$2\'\ dbname='$3'\" -1 -c - -f $3_update.log"
echo " (take that data from kontrakcja.conf)"

gunzip -c $3.gz | psql -d "user='$1' password='$2' dbname='$3'" -1 -f - -L $3_update.log

