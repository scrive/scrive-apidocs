#!/bin/bash

if [ "$1" == "" ]; then
  echo "usage: ./build-scripts/runSeleniumTestWithServer.sh [all|<spec-name.rb>]"
else
  echo "starting selenium server, expecting you to put jar at: selenium-test/lib/selenium-server-standalone-2.7.0.jar"
  rm -f selenium-server.out
  java -jar selenium-test/lib/selenium-server-standalone-2.35.0.jar > selenium-server.out &
  SSPID=$!
  echo $SSPID > selenium-server.pid
  sleep 10

  ./selenium-test/runSeleniumTest.sh $1

  echo "stopping selenium server"
  kill `more selenium-server.pid`
  sleep 10
  rm selenium-server.pid
fi
