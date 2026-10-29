compile() {
    cat $1 | ruc > main.asm
    nasm -w-implicit-abs-deprecated -f elf64 -g -F dwarf -o main.o main.asm
    gcc main.o $(pkg-config --silence-errors --cflags --libs libmicrohttpd) -no-pie -z noexecstack -O3 -rdynamic -pthread -o main
}

for src; do
    compile $src
    ./main
done
