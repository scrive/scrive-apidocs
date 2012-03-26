#!/bin/bash

rm -f jslint.txt;
ls public/js | egrep -v "jquery|backbone|underscore|livequery" | awk '{print "gjslint public/js/"$1}' | sh > jslint.txt;

echo "Searching for missing semicolons or extra commas"

rm -f jslint-serious.txt;
grep -E 'FILE|0010|0121' jslint.txt > jslint-serious.txt;

if grep -q "Line" jslint-serious.txt
then
   echo "Found some"
   cat jslint-serious.txt
   exit 1
fi

echo "None found"
exit 0
