#!/bin/bash


rm -rf ./mem_abuser ./stack_abuser ./wrapper.sh
gcc gswrapper.c -o wrapper.sh '-DGS="/bin/bash"'
./try_time.sh 2>/dev/null
./try_mem_abuser.sh 2>/dev/null
./try_stack_abuser.sh 2>/dev/null
./try_file.sh 2>/dev/null
./try_processes.sh 2>/dev/null
rm -rf ./mem_abuser ./stack_abuser ./wrapper.sh
