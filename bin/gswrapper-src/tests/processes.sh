#!/bin/bash

run_p() {
echo $1
run_p $[ $1 + 1 ] &
}

run_p 1
