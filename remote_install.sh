#!/bin/bash

echo "Host?"
read -e host

echo "################"
echo ${host}
/usr/bin/rsync -hri --times ./ ${host}:~/dotfiles/
# scp ~/.ssh/*.txt ${host}:~/.ssh/
ssh ${host} ./dotfiles/install.sh
