apt install git curl build-essential libgtk-3-0 libgtk-3-dev
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

git clone https://github.com/x7ea/ruc.git
cd ./ruc

cargo install --path .
./ruc.sh ./app/hello.rc