if  [ "$#" -lt 1 ]; then
    cat ./README.md
elif [ $1 = "--update" ]; then
    git stash
    git pull
    cargo install --path .
else
    cat $1 | ruca > main.asm
    nasm -w-implicit-abs-deprecated -f elf64 -g -F dwarf -o main.o main.asm
    gcc -no-pie -z noexecstack -O3 -rdynamic -o main main.o
    ./main
fi
