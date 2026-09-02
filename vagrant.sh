mkdir machine
cd machine

vagrant init generic/ubuntu2204
vagrant up

vagrant ssh < config.sh
