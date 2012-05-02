#!/bin/bash

#
# This script should be in ./scripts directory.
# Lets establish where is the root directory:
#
ROOT1=`dirname $0`
ROOT=`dirname $ROOT1`

export PATH=/opt/postgresql-9.1.3/bin:$PATH
export PGHOST=/var/run/postgresql9
export PGPORT=5332

LOCAL_STATE=$ROOT/_local/kontrakcja_state
CONF=$ROOT/kontrakcja.conf
ALLDATA=all-data-`date +%Y-%m-%d-%H-%M-%S`

DBSTRING=`sed -e '/dbConfig/!d' -e 's/^.*dbConfig[ \\t]*=[ \\t]*//' $ROOT/kontrakcja.conf`

LAST_CHECKPOINT_FILE=`ls -r $LOCAL_STATE/checkpoints-* | head -1`
CURRENT_FILE=$LOCAL_STATE/current-0000000000

export PGDATABASE=`echo $DBSTRING | sed -e s/^.*dbname=\'// -e s/\'.*//`
export PGPASSWORD=`echo $DBSTRING | sed -e s/^.*password=\'// -e s/\'.*//`
export PGUSER=`echo $DBSTRING | sed -e s/^.*user=\'// -e s/\'.*//`

echo "Dump all data uses:"
echo "  destination directory:  $ALLDATA"
echo "  current:                $CURRENT_FILE"
echo "  last checkpoint:        $LAST_CHECKPOINT_FILE"
echo "  database:               $PGDATABASE"
echo "  user:                   $PGUSER"
echo "  password:               $PGPASSWORD"

mkdir $ALLDATA

echo Copying Happstack state files...
cp $CURRENT_FILE $ALLDATA
cp $LAST_CHECKPOINT_FILE $ALLDATA

echo Dumping postgresql database...

pg_dump -v -f $ALLDATA/psql_database.dump -Z9 --no-password -F custom


echo Everything is in $ALLDATA. Done!
