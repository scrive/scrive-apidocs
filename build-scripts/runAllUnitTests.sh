#!/bin/bash -e

export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8

if [ "$TEAMCITY_VERSION" = "" ]; then

./dist/build/kontrakcja-test/kontrakcja-test --plain $@

else

./dist/build/kontrakcja-test/kontrakcja-test --plain $@ --output-dir test/artefacts | runghc build-scripts/Teamcity.hs hunit
exit "${PIPESTATUS[0]}"

fi
