#!/bin/bash

ALLDATA=$1

if [ x$ALLDATA = x ]
then
  echo Usage:
  echo    $0 dir
  echo where dir is an arrchive directory created by 'dump_all_data.sh' script.
  exit 3
fi

if [ ! -f $ALLDATA/current-0000000000 ]
then
  echo The file $ALLDATA/current-0000000000 does not exist,
  echo it seems that $ALLDATA is not valid archive.
  exit 3
fi


#
# This script should be in ./scripts directory.
# Lets establish where is the root directory:
#
ROOT_SCRIPTS=`dirname $0`
ROOT=`dirname $ROOT_SCRIPTS`

LOCAL_STATE=$ROOT/_local/kontrakcja_state
CONF=$ROOT/kontrakcja.conf

DBSTRING=`sed -e '/dbConfig/!d' -e 's/^.*dbConfig[ \\t]*=[ \\t]*//' $ROOT/kontrakcja.conf`


export PGDATABASE=`echo $DBSTRING | sed -e s/^.*dbname=\'// -e s/\'.*//`
export PGPASSWORD=`echo $DBSTRING | sed -e s/^.*password=\'// -e s/\'.*//`
export PGUSER=`echo $DBSTRING | sed -e s/^.*user=\'// -e s/\'.*//`

echo "Dump all data uses:"
echo "  source directory:       $ALLDATA"
echo "  current:                $CURRENT_FILE"
echo "  last checkpoint:        $LAST_CHECKPOINT_FILE"
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
pg_restore -O -1 -v -d $PGDATABASE $ALLDATA/psql_database.dump --no-password

if [ $? -ne 0 ]
then
    echo Database restore did not return zero, it probably failed
    echo Command was:
    echo "    pg_restore -O -1 -d $PGDATABASE $ALLDATA/psql_database.dump --no-password"
    echo Exiting to not make more mess.
    exit 3
fi

echo Removing old Happstack state files...
rm -v $LOCAL_STATE/*

echo Copying new Happstack state files...
cp -v $ALLDATA/current-* $ALLDATA/checkpoints-* $LOCAL_STATE


echo Everything restored from $ALLDATA. Done!
