run() {
    cat $1 | ruca > main.asm
    nasm -w-implicit-abs-deprecated -f elf64 -g -F dwarf -o main.o main.asm
    gcc main.o $(pkg-config --cflags --libs gtk+-3.0) -no-pie -z noexecstack -O3 -rdynamic -o main
}

for src; do
    run $src
    ./main
done
