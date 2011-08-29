#!/bin/bash

run_abuser() {
  rm -rf ./stack_abuser
  gcc stack_abuser.c -o stack_abuser -fno-pie -fno-stack-protector -DINC=$2 -DINIT=$1 -O0 > /dev/null
  ./wrapper.sh ./stack_abuser 2>&1 | tail -n 1
  rm -rf ./stack_abuser
}

run=$( run_abuser 0 1024 )
run=$( run_abuser "$run" 32 )
run=$( run_abuser "$run" 1 )
echo "this is the number of iterations done"
echo "$run"
echo "this should be a reasonable amount of bytes (i.e. 32)"
echo "8192 / ( $run / 1024 )" | bc -l
