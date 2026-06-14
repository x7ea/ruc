if  [ "$#" -lt 1 ]; then
    cat ./README.md
elif [ $1 = "--update" ]; then
    git stash & git pull
    cargo install --path .
elif [ $1 = "--test" ]; then
    for file in $(find ./app -type f); do
        ./ruca.sh $file
    done
elif [ $1 = "--demo" ]; then
    for file in $(find ./app -type f); do
        clear
        echo "Example: $file"
        cat $file
        read
        ./ruca.sh $file
        read
    done
else
    cat $1 | ruca > main.asm
    nasm -w-implicit-abs-deprecated -f elf64 -g -F dwarf -o main.o main.asm
    gcc main.o $(pkg-config --cflags --libs gtk+-3.0) -no-pie -z noexecstack -O3 -rdynamic -o main
    ./main
fi
