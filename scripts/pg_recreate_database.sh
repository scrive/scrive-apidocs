#!/bin/bash


psql -d "user='postgres' dbname='postgres'" -a -c "DROP DATABASE IF EXISTS $1;"
psql -d "user='postgres' dbname='postgres'" -a -c "CREATE DATABASE $1 WITH OWNER $2;"
psql -d "user='postgres' dbname='postgres'" -a -c "ALTER DATABASE $1 SET TIMEZONE = 'UTC';"

