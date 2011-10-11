#!/bin/bash

if [ "$1" == "" ]; then
  echo "usage: ./build-scripts/runAllSeleniumTests.sh [existing-selenium|<path/selenium-server-standalone-2.*.jar>] [existing-kontrakcja|local-kontrakcja]"
elif [ "$2" == ""]; then
  echo "usage: ./build-scripts/runAllSeleniumTests.sh [existing-selenium|<path/selenium-server-standalone-2.*.jar>] [existing-kontrakcja|local-kontrakcja]"
else
  if [ "$1" == "existing-selenium" ]; then
    echo "using existing selenium server"
  else
    echo "starting selenium server"
    rm -f selenium-server.out
    java -jar $1 > selenium-server.out &
    SSPID=$!
    echo $SSPID > selenium-server.pid
    sleep 10
  fi
  
  if [ "$2" == "existing-kontrakcja" ]; then
    echo "using existing kontrakcja server"
  else
    echo "starting local kontrakcja server"
    rm -f kontrakcja-server.out
    ./dist/build/kontrakcja-server/kontrakcja-server > kontrakcja-server.out &
    KSPID=$!
    echo $KSPID > kontrakcja-server.pid
    sleep 10
  fi

  ./selenium-test/runSeleniumTest.sh all

  if [ "$1" == "existing-selenium" ]; then
    echo "using existing selenium server"
  else
    echo "stopping selenium server"
    kill `more selenium-server.pid`
    sleep 10
    rm selenium-server.pid
  fi
  
  if [ "$2" == "existing-kontrakcja" ]; then
    echo "using existing kontrakcja server"
  else
    echo "stopping local kontrakcja server"
    kill `more kontrakcja-server.pid`
    sleep 10
    rm kontrakcja-server.pid
  fi
fi
