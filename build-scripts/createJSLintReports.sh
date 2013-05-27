#!/bin/bash -e

echo "# N.B. this script requires gjslint, please install if you have not already";
echo "# https://developers.google.com/closure/utilities/docs/linter_howto";


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

echo "Creating jslint report..."
(gjslint --disable 1,2,5,110,120,131,220 --nosummary --nobeep -r public/js || true) > jslint.txt

if grep -q "Line" jslint.txt
then
   cat jslint.txt;
   exit 1;
fi

echo "Searching for use of :focus selector ...";
rm -f jslint-focus.txt;
grep -nHr :focus public/js  > jslint-focus.txt;

if grep -q ":focus" jslint-focus.txt
then
   cat jslint-focus.txt;
   exit 1;
fi

exit 0;
