#!/bin/bash

ls public/js | egrep -v "jquery|backbone|underscore|livequery" | awk '{print "java -jar ~/jslint.jar public/js/"$1}' | sh > jslint.txt && cat jslint.txt

