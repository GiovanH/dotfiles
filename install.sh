#!/bin/bash

wd=$(dirname "$(readlink -f "$0")")

for dotfile in ${wd}/home/.?*[^~]; do
    dotfile=$(basename ${dotfile})
    dest=~/${dotfile}
    if [[ -L ${dest} ]]; then
        rm ${dest}
    elif [[ -f ${dest} ]]; then
        mv -v ${dest} ${dest}.$(date -Iminutes).bak
    fi
    ln -s ${wd}/home/${dotfile} ${dest}
    ls -al ${dest}
done

for dotdir in .emacs.d; do
    # /usr/bin/rsync -hir --executability --times --backup ${wd}/${dotdir}/ ~/${dotdir}/
    dotdir=$(basename ${dotdir})
    dest=~/${dotdir}
    if [[ -L ${dest} ]]; then
        rm ${dest}
    elif [[ -d ${dest} ]]; then
        mv -v ${dest} ${dest}.$(date -Iminutes).bak
    fi
    ln -s ${wd}/${dotdir} ${dest}
    ls -al ${dest}
    echo pass
done
