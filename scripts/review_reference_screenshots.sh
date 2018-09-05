#!/bin/bash

# you need jq and feh
# run it and hit 'q' to view next screenshot

for x in `ls files/reference_screenshots/`; do
    jq '.image' < files/reference_screenshots/$x | head --bytes -2 | tail --bytes +25 | base64 -d > /tmp/test.png
    feh /tmp/test.png
done
