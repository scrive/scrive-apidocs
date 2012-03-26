#!/bin/bash -e

echo "# N.B. this script requires gjslint, please install if you have not already";
echo "# https://developers.google.com/closure/utilities/docs/linter_howto";

rm -f jslint-files.txt;
ls public/js/*.js | egrep -v "jquery|backbone|underscore|livequery" > jslint-files.txt;

if [ $# -eq 0 ]
then
  echo "# This script can optionally check the javascript code compiles too.";
  echo "# If you would like this please download jar from";
  echo "# https://developers.google.com/closure/compiler/docs/gettingstarted_app";
  echo "# then pass the path as an argument e.g.";
  echo "# ./build-scripts/createJSLintReports.sh compiler.jar";
else
  echo "Checking javascript compiles...";
  rm -f jslint-compile.txt;
  cat jslint-files.txt | awk '{print "java -jar '$1' --js_output_file jslint-compile.tmp --js "$1}' | sh &> jslint-compile.txt;

  if grep -q "ERROR" jslint-compile.txt
  then
    cat jslint-compile.txt;
    exit 1;
  fi
fi

echo "Creating jslint report...";
rm -f jslint.txt;
cat jslint-files.txt | awk '{print "gjslint "$1" || true"}' | sh > jslint.txt;

echo "Searching for missing semicolons or extra commas...";
rm -f jslint-serious.txt;
grep -E 'FILE|0010|0121' jslint.txt > jslint-serious.txt;

if grep -q "Line" jslint-serious.txt
then
   cat jslint-serious.txt;
   exit 1;
fi

echo "Searching for use of :focus selector ...";
rm -f jslint-focus.txt;
cat jslint-files.txt | awk '{print "grep -nH :focus "$1" || true"}' | sh > jslint-focus.txt;
rm -f jslint-focus-serious.txt;
#todo we should really get rid of this exception
(egrep -v "global.js:53" jslint-focus.txt > jslint-focus-serious.txt || true);
if grep -q ":focus" jslint-focus-serious.txt
then
   cat jslint-focus-serious.txt;
   exit 1;
fi

exit 0;