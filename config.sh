apt install git curl build-essential
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

git clone https://github.com/x7ea/ruc.git
cd ./ruc

cargo install --path .
./ruc.sh  