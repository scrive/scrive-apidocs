#!/bin/bash

if [ "$1" == "" ]; then
  echo "usage: ./runSeleniumTests.sh [all|<spec-name.rb>]"
elif [ "$1" == "all" ]; then
  ls selenium-test/src/specs/*.rb | awk '{print "spec --colour --format specdoc "$1}' | sh
else
  spec --colour --format specdoc selenium-test/src/specs/"$1"
fi

