compile() {
    cat $1 | ruc > main.asm
    nasm -w-implicit-abs-deprecated -f elf64 -g -F dwarf -o main.o main.asm
    gcc main.o $(pkg-config --silence-errors --cflags --libs gtk+-3.0) -no-pie -z noexecstack -O3 -rdynamic -o main
}

for src; do
    compile $src
    ./main
done
