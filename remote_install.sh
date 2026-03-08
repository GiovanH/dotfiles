#!/bin/bash

host=$1

echo "################"
echo ${host}
/usr/bin/rsync $2 -hri --times \
  --exclude "auto-save-list" --exclude "eshell" --exclude "savehist" \
  --exclude ".emacs.d/straight" --exclude ".emacs.d/elpa" \
  ./ ${host}:~/dotfiles/
# scp ~/.ssh/*.txt ${host}:~/.ssh/
ssh ${host} ./dotfiles/install.sh
