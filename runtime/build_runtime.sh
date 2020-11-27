#!/bin/bash
rm -f test
#gcc -arch x86_64 -pagezero_size 0x100000 -o test entry.S utils.c lispo.c runtime.c 
#gcc -arch x86_64 -pagezero_size 0x2000000 -o runtime.exe entry.S utils.c lispo.c runtime.c -g
gcc -arch x86_64 -pagezero_size 0x2000000 -o runtime.exe entry.S utils.c lispo.c hash.c runtime.c -g3 -dynamic -twolevel_namespace -bind_at_load -Wl,-no_pie

nm -g -U -P -t d runtime.exe > runtime.nm
