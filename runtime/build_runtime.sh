#!/bin/bash
rm -f test

build_linux() {
    echo "Building for Linux"
    gcc -m64 -o runtime.exe entry.S utils.c lispo.c hash.c runtime.c -g3 -no-pie
    nm -g -P -t d -S runtime.exe | grep -v @ | grep -v __ |  awk '{print $1 " " $2 " " $3}' > runtime.nm
}

build_mac() {
    echo "Building for Mac"
    gcc -arch x86_64 -pagezero_size 0x2000000 -o runtime.exe entry.S utils.c lispo.c hash.c runtime.c -g3 -dynamic -twolevel_namespace -bind_at_load -Wl,-no_pie
nm -g -U -P -t d runtime.exe > runtime.nm
}

build_unknown() {
    echo "Unsupported OS: $OS"
    exit 1
}


OS="$(uname -s)"

case "$OS" in
    Linux)
        build_linux
        ;;
    Darwin)
        build_mac
        ;;
    *)
        build_unknown
        ;;
esac
