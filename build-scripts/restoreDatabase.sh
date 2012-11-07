#!/bin/bash

# Test number of arguments passed
if [ "$#" != "2" ] ; then
    echo "restoreDatabase.sh usage:" ; echo
    echo "   restoreDatabase.sh <path to dump file> <environment name>" ; echo
    echo "Currently this script is only supported for prod and staging"
    exit 3
fi

# Check we have passed a supported environment name
if [ "$2" != "prod" ] && [ "$2" != "staging" ] ; then
    echo "This script is only supported for prod and staging environments."
    exit 3
fi

# Check ~/.pgpass exists so the dump will work
if [ ! -f "${HOME}/.pgpass" ] ; then
    echo "${HOME}/.pgpass does not exist so the database dump will not work. Please create it with valid credentials."
    exit 3
fi

# Check dump file exists
if [ ! -f "${1}" ] ; then
    echo "The dump file ${1} does not exist so the database restore will not work."
    exit 3
fi

# Drop DB, create new one, load database into it
dropdb ${2}
createdb -O ${2} ${2}
psql ${2} -c "ALTER DATABASE $PGDATABASE SET TIMEZONE = UTC"
psql ${2} -c "DROP EXTENSION plpgsql"
pg_restore -O -1 -v --username=${2} -d ${2} ${1}
