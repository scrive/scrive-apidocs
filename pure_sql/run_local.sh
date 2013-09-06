#!/bin/bash -e

if [ -z "$1" -o ! -f "$2" ];
then
    echo "Usage"
    echo ""
    echo "    $0 DBNAME SCRIPT"
    echo
    echo " where DBNAME is either database name you have access to or full connstring"
    echo "   and SCRIPT is a local SQL script file that should be executed"

    exit 200
fi

echo "DB is: $1"
echo "Script is: $2"
echo "This script will stop at the very end where you will be able to type ROLLBACK or COMMIT."
echo "Press ENTER to continue now (Ctrl-C to exit at any time)."

read -s

echo "OK, starting..."

cat $2 - | time psql $1
