#!/bin/bash

T_DIR="compiler-regression-tests/T"
NIL_DIR="compiler-regression-tests/NIL"

for file in ${T_DIR}/*; do
    echo "Running: ./runtime.exe $file"
    ./runtime.exe $file T
    exit_status=$?
    if [ $exit_status -ne 1 ]; then
	echo "error on core file -  $file"
	exit 1
    fi
done

for file in ${NIL_DIR}/*; do
    echo "Running: ./runtime.exe $file"
    ./runtime.exe $file NIL
    exit_status=$?
    exit_status=$?
    if [ $exit_status -ne 0 ]; then
	echo "error on core file -  $file"
	exit 1
    fi
done
