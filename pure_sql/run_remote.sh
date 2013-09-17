#!/bin/bash -e

if [ -z "$1" -o ! -f "$2" ];
then
    echo "Usage"
    echo ""
    echo "    $0 SERVER SCRIPT"
    echo
    echo " where SERVER is the server to connect to with ssh and that has ~/kontrakcja/kontrakcja.conf available there"
    echo "   and SCRIPT is a local SQL script file that should be executed on the remote server"

    exit 200
fi

DBCONFIGLINE=`ssh $1 grep dbConfig kontrakcja/kontrakcja.conf`
DBCONNSTRING=`echo $DBCONFIGLINE | sed -e 's/^.*\(".*"\).*$/\1/'`
DBCONNSTRINGHIDDEN=`echo $DBCONNSTRING | sed -e "s/password='[^']*'/password='*******'/"`

echo "Server is: $1"
echo "Script is: $2"
echo "Database connection string is:"
echo "    $DBCONNSTRINGHIDDEN"
echo "This script will stop at the very end where you will be able to type ROLLBACK or COMMIT."
echo "Press ENTER to continue now (Ctrl-C to exit at any time)."

read -s

echo "OK, starting..."

cat $2 - | ssh $1 time /opt/postgresql-9.1.5/bin/psql $DBCONNSTRING -
