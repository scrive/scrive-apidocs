#!/bin/bash

echo "Maximum number of simultaneous processes startable"
./wrapper.sh ./processes.sh 2>/dev/null | tail -n1
