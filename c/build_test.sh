#!/bin/bash
rm -f test
#gcc -arch x86_64 -pagezero_size 0x100000 -o test entry.S utils.c lispo.c runtime.c 
gcc -arch x86_64 -pagezero_size 0x2000000 -o test entry.S utils.c lispo.c runtime.c 
nm -g -U -P -t d test > test.nm
