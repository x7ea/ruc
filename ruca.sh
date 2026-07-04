#!/usr/bin/env bash

set -e
cd "$(dirname "$0")"

if [ "$#" -lt 1 ]; then
    cat ./README.md
elif [ $1 = "--update" ]; then
    git stash & git pull
    cargo install --path .
elif [ $1 = "--test" ]; then
    for file in $(find ./app -type f); do
        ./ruca.sh $file
    done
else
    cat $1 | ruca > main.asm
    nasm -w-implicit-abs-deprecated -f elf64 -g -F dwarf -o main.o main.asm

    if grep -Eq '^[[:space:]]*use[[:space:]].*viewkit' "$1"; then
        VIEWKIT_ROOT="${VIEWKIT_ROOT:-../viewKit}"
        VIEWKIT_INCLUDE="${VIEWKIT_INCLUDE:-$VIEWKIT_ROOT/lib/include}"
        VIEWKIT_LIB="${VIEWKIT_LIB:-$VIEWKIT_ROOT/target/release}"

        if [ ! -f "$VIEWKIT_INCLUDE/viewkit.h" ]; then
            echo "missing $VIEWKIT_INCLUDE/viewkit.h" >&2
            exit 1
        fi

        if [ ! -f "$VIEWKIT_LIB/libviewkit.so" ]; then
            echo "missing $VIEWKIT_LIB/libviewkit.so" >&2
            echo "run 'cargo build --release' in ViewKit" >&2
            exit 1
        fi

        gcc \
            main.o \
            ./lib/viewkit.c \
            $(pkg-config --cflags --libs gtk+-3.0) \
            -I"$VIEWKIT_INCLUDE" \
            -L"$VIEWKIT_LIB" \
            -Wl,-rpath,"$VIEWKIT_LIB" \
            -lviewkit \
            -no-pie \
            -z noexecstack \
            -O3 \
            -rdynamic \
            -o main
    else
        gcc \
            main.o \
            $(pkg-config --cflags --libs gtk+-3.0) \
            -no-pie \
            -z noexecstack \
            -O3 \
            -rdynamic \
            -o main
    fi

    ./main
fi
