#!/bin/bash

echo "Executing pg_dump as:"
echo "    pg_dump \"user=\'$1\' password=\'\$2\' dbname=\'$3\' \| gzip \> $3.gz"
echo " (take that data from kontrakcja.conf)"


pg_dump "user='$1' password='$2' dbname='$3'" | gzip > $3.gz

