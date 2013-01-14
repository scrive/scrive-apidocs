#!/bin/bash -e


if [ "$TEAMCITY_VERSION" = "" ]; then

./dist/build/kontrakcja-test/kontrakcja-test --plain all

else

./dist/build/kontrakcja-test/kontrakcja-test --plain all | runghc build-scripts/Teamcity.hs hunit
exit ${PIPESTATUS[0]}

fi
