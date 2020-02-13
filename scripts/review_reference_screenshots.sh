#!/bin/bash

# you need jq and feh on Linux, only jq on mac
# run it and hit 'q' to view next screenshot on Linux

case $(uname) in
    'Darwin') VIEWER=open;;
    *)        VIEWER=feh;;
esac

for x in `ls files/reference_screenshots/`; do
  jq '.image' < files/reference_screenshots/$x | perl -pe 's/"$//' | perl -pe 's#"data:image/jpeg;base64,##' | base64 -d > /tmp/test_$x.png
  open /tmp/test_$x.png
done
