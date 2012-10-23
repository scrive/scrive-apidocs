#!/bin/bash

#
# This script should be in ./scripts directory.
# Lets establish where is the root directory:
#
ROOT_SCRIPTS=`dirname $0`
ROOT=`dirname $ROOT_SCRIPTS`

LOCAL_STATE=$ROOT/_local/kontrakcja_state
CONF=$ROOT/kontrakcja.conf

DBSTRING=`cat $ROOT/kontrakcja_test.conf`

export PGDATABASE=`echo $DBSTRING | sed -e s/^.*dbname=\'// -e s/\'.*//`
export PGPASSWORD=`echo $DBSTRING | sed -e s/^.*password=\'// -e s/\'.*//`
export PGUSER=`echo $DBSTRING | sed -e s/^.*user=\'// -e s/\'.*//`

echo "Settings used for database:"
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
echo Dropping old test database: $PGDATABASE
dropdb -i $PGDATABASE --no-password
createdb $PGDATABASE -O $PGUSER --no-password --locale=sv_SE.UTF-8
psql $PGDATABASE -c "ALTER DATABASE $PGDATABASE SET TIMEZONE = UTC"

echo You have a clean new test database $PGDATABASE. Done!
