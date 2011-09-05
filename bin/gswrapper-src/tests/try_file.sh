#!/bin/bash

tmp=$(mktemp)
./wrapper.sh /bin/dd if=/dev/zero of=$tmp
echo "Size of the file created:"
ls -lh $tmp | awk '// { print $5}'
rm -rf $tmp
