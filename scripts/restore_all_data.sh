#!/bin/bash

ALLDATA=$1

if [ x$ALLDATA = x ]
then
  echo Usage:
  echo    $0 file.bin
  echo where file.bin is an pg_dump arrchive.
  exit 3
fi


#
# This script should be in ./scripts directory.
# Lets establish where is the root directory:
#
ROOT_SCRIPTS=`dirname $0`
ROOT=`dirname $ROOT_SCRIPTS`

CONF=$ROOT/kontrakcja.conf

DBSTRING=`sed -e '/dbConfig/!d' -e 's/^.*dbConfig[ \\t]*=[ \\t]*//' $ROOT/kontrakcja.conf`


export PGDATABASE=`echo $DBSTRING | sed -e s/^.*dbname=\'// -e s/\'.*//`
export PGPASSWORD=`echo $DBSTRING | sed -e s/^.*password=\'// -e s/\'.*//`
export PGUSER=`echo $DBSTRING | sed -e s/^.*user=\'// -e s/\'.*//`

echo "Dump all data uses:"
echo "  source dump:            $ALLDATA"
echo "  database:               $PGDATABASE"
echo "  user:                   $PGUSER"
echo "  password:               $PGPASSWORD"


# Documenting an annoyance here:
#
# We could use pg-restore with -c (clean) mode, so it does DROP
# of every object that it tries to restore, but:
#   - DROP fails if this object does not exist
#   - DROP is only for objects that are going to be restored
#          and there is no CASCADE, so it does not clean attached
#          constraints for example
#
# Same story goes about other database features. We need to DROP
# LANGUAGE plpgsql so it can be recreated just a moment later.
#
# So we need to plainly drop the database before restoring.
# Warning: this is not transactional so be careful!
#
echo Dropping old database
dropdb -i $PGDATABASE --no-password
createdb $PGDATABASE -O $PGUSER --no-password --locale=sv_SE.UTF-8
psql $PGDATABASE -c "ALTER DATABASE $PGDATABASE SET TIMEZONE = UTC"
psql $PGDATABASE -c "DROP EXTENSION plpgsql"

echo Restoring postgresql database...
pg_restore -j4 --no-privileges -O -v -d $PGDATABASE $ALLDATA --no-password

if [ $? -ne 0 ]
then
    echo Database restore did not return zero, it probably failed.
    exit 3
fi


echo Everything restored from $ALLDATA. Done!
