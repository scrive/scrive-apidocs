#!/bin/bash -e

ls public/js | egrep -v "jquery|backbone|underscore|livequery" | awk '{print "gjslint public/js/"$1}' | sh > jslint.txt
cat jslint.txt

