#!/bin/bash

echo "Executing psql DROP and CREATE DATABASE as:"
echo "    gunzip -c $2.gz | psql -d \"user=\'postgres\' password=\'\$1\'\ dbname='postgres'\" -1"
echo " (take that data from kontrakcja.conf)"

psql -d "user='postgres' password='$1' dbname='postgres'" -1 -a -c "DROP DATABASE IF EXISTS $2"
psql -d "user='postgres' password='$1' dbname='postgres'" -1 -a -c "CREATE DATABASE $2;"

# psql
# drop database databasename;
# \q

# Now create the database again:

# createdb -O databasename username


