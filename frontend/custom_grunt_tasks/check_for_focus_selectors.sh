#!/bin/bash -e

echo "Searching for use of :focus selector ...";
rm -f jslint-focus.txt;
(grep -nHr :focus $1 || true) > jslint-focus.txt;

if grep -q ":focus" jslint-focus.txt
then
   cat jslint-focus.txt;
   rm -f jslint-focus.txt;
   exit 1;
fi
rm -f jslint-focus.txt;

exit 0;
