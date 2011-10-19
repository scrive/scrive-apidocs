#!/bin/bash

rm -rf coverage-reports && ls test/src | awk -F "." '{print "--exclude="$1}' | hpc markup `xargs echo` --destdir=coverage-reports kontrakcja-test.tix

