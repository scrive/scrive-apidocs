#!/bin/bash

sed -e ':a' -e 'N' -e '$!ba' -e 's/\n/\\n/g'
