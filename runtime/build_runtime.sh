#!/bin/bash
gcc -shared -o runtime.so -fPIC entry.S utils.c lispo.c runtime.c
