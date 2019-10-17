#!/bin/bash

T_DIR="compiler-regression-tests/T"
NIL_DIR="compiler-regression-tests/NIL"

for file in ${T_DIR}/*; do
    echo "Running: ./runtime.exe $file T" 
    ./runtime.exe $file T
    if [ $? -ne 0 ]; then
	echo "error on core file -  $file"
	exit 1
    fi
done

for file in ${NIL_DIR}/*; do
    echo "Running: ./runtime.exe $file NIL"
    ./runtime.exe $file NIL
    if [ $? -ne 0 ]; then
	echo "error on core file -  $file"
	exit 1
    fi
done

echo
echo "------------------------- STATUS OK -------------------------"
