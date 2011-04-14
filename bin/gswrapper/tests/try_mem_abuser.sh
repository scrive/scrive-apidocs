#!/bin/bash

run_abuser() {
  rm -rf ./mem_abuser
  gcc mem_abuser.c -o mem_abuser -fno-pie -fno-stack-protector -DINC=$2 -DINIT=$1 > /dev/null
  ./wrapper.sh ./mem_abuser | tail -n 1
  rm -rf ./mem_abuser
}

run=$( run_abuser 0 $[ 1024 * 1024 ] )
run=$( run_abuser "$run" 1024 )
run=$( run_abuser "$run" 1 )
echo "this is the number of bytes allocated"
echo "$run"
echo "this should be a reasonable amount of Mbytes"
echo "( $run / 1024 / 1024 )" | bc -l
