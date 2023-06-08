# ~/.bashrc: executed by bash(1) for interactive shells.
# Also invoked manually by .bash_profile
# This file is for globals

# [ -f /etc/profile ] && source /etc/profile
# [ -f /etc/bashrc ] && source /etc/bashrc

# Add an item to the PATH list.
# From /etc/profile, where it's automatically unset :(
pathmunge () {
  case ":${PATH}:" in
    *:"$1":*)
      ;;
    *)
      if [ "$2" = "after" ] ; then
        PATH=$PATH:$1
      else
        PATH=$1:$PATH
      fi
  esac
}

# Unix settings

export LESS="--chop-long-lines --RAW-CONTROL-CHARS" # see LESS(1)

export VISUAL="vim"
export EDITOR="vim"

# Use standard blues
export LS_COLORS=$(echo $LS_COLORS | sed 's/=38;5;27:/=34;27:/')

# Misc :)
alias whence='type -a' # where, of a sort

# Some shortcuts for different directory listings
alias ls='ls -h --color=auto'                 # classify files in colour
alias dir='ls --color=auto --format=vertical'

# User bin, man, info

pathmunge "${HOME}/dotfiles/scripts"

pathmunge "${HOME}/bin"
pathmunge "${HOME}/foss/bin"
pathmunge "${HOME}/.local/bin"
[[ -d "/usr/libexec/git-core" ]] && pathmunge "/usr/libexec/git-core" && export GIT_EXEC_PATH="/usr/libexec/git-core"
[[ -d "${HOME}/foss/libexec/git-core" ]] && pathmunge "${HOME}/foss/libexec/git-core" && export GIT_EXEC_PATH=${HOME}/foss/libexec/git-core

#[[ -d "${HOME}/man" ]] &&
MANPATH="${HOME}/man:${HOME}/.local/man:${MANPATH}"
#[[ -d "${HOME}/info" ]] &&
INFOPATH="${HOME}/info:${HOME}/.local/info:${INFOPATH}"

CPATH="${HOME}/foss/usr/include:${CPATH}"
CPATH="${HOME}/.local/include:${CPATH}"
LD_LIBRARY_PATH="${HOME}/.local/lib:${LD_LIBRARY_PATH}"
LD_LIBRARY_PATH="${HOME}/foss/usr/lib:${LD_LIBRARY_PATH}"

# User functions

sourceif () { [[ -f "$1" ]] && . "$1"; } # || echo "missing file $1"; }

sourceif ${HOME}/.bash_colors
sourceif ${HOME}/.bash_personal

export HOSTNAME_NICE="$(hostname -f 2>/dev/null || hostname 2>/dev/null || cat /etc/hostname)"
sourceif ~/.bashrc_$(uname -o)
sourceif ~/.bashrc_${HOSTNAME_NICE}
sourceif ~/.bashrc_local

# Programmable completion enhancements
sourceif /etc/bash_completion
sourceif ${HOME}/.bash_completion

mkdir -p ${HOME}/.local/share/completions/
for shellfile in ${HOME}/.local/share/completions/*sh; do
  sourceif $shellfile
done

# Bash options

shopt -s globstar
shopt -s extglob
shopt -s expand_aliases

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

## Make bash append rather than overwrite the history on disk
shopt -s histappend

## don't put duplicate lines or lines starting with space in history, and erase duplicate lines.
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=10000
export HISTFILESIZE=20000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

## When changing directory small typos can be ignored by bash
## for example, cd /vr/lgo/apaache would find /var/log/apache
shopt -s cdspell

# stop killing evw
shopt -s checkjobs

export HOSTNAME_NICE="$(hostname -f 2>/dev/null || hostname 2>/dev/null || cat /etc/hostname)"
HOST_FAMILY="UNK"

sourceif "${HOME}/.profile_$(uname -o)"
sourceif "${HOME}/.profile_${HOSTNAME_NICE}"
sourceif "${HOME}/.profile_local"

