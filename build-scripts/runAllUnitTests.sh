#!/bin/bash -e


if [ x$TEAMCITY_VERSION == x ]; then

./dist/build/kontrakcja-test/kontrakcja-test --plain all

else

./dist/build/kontrakcja-test/kontrakcja-test --plain all | runghc build-scripts/Teamcity.hs hunit

fi
