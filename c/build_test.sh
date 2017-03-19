#!/bin/bash
rm -f test
gcc -o test entry.S utils.c lispo.c runtime.c 
nm -g -U -P -t d test > test.nm
