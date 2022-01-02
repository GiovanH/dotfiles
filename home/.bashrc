# ~/.bashrc: executed by bash(1) for interactive shells.
# Also invoked manually by .bash_profile
# This file is for globals

[ -f /etc/bashrc ] && source /etc/bashrc

# Default to human readable figures
# alias df='df -h'
# alias du='du -h'
#
# Misc :)
# alias less='less -r'                          # raw control characters
# alias whence='type -a'                        # where, of a sort
# alias grep='grep --color'                     # show differences in colour
# alias egrep='egrep --color=auto'              # show differences in colour
# alias fgrep='fgrep --color=auto'              # show differences in colour
#
# Some shortcuts for different directory listings
alias ls='ls -h --color=auto'                 # classify files in colour
# alias dir='ls --color=auto --format=vertical'
# alias vdir='ls --color=auto --format=long'
alias ll='ls -l'                              # long list
alias la='ls -A'                              # all but . and ..
alias l='ls -CF'                              #

# Umask
#
# /etc/profile sets 022, removing write perms to group + others.
# Set a more restrictive umask: i.e. no exec perms for others:
# umask 027
# Paranoid: neither group nor others have any perms:
# umask 077

# User bin, man, info

[[ -d "${HOME}/bin" ]] && PATH="${HOME}/bin:${PATH}"
[[ -d "${HOME}/man" ]] && MANPATH="${HOME}/man:${MANPATH}"
[[ -d "${HOME}/info" ]] && INFOPATH="${HOME}/info:${INFOPATH}"

PATH="${HOME}/dotfiles/scripts:${PATH}"

# User functions

source ~/.bash_colors
[[ -f ${HOME}/.bash_personal ]] && . ${HOME}/.bash_personal

# Programmable completion enhancements
[[ -f /etc/bash_completion ]] && . /etc/bash_completion

# Bash options

shopt -s globstar

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

HOST_FAMILY="UNK"
COLOR_HOST=$COLOR_PURPLE
CYG_NEWLINE=' '
if [[ "$(uname)" =~ "CYGWIN_NT" ]]; then
    HOST_FAMILY="CYGWIN"
    COLOR_HOST=$COLOR_GREEN
    CYG_NEWLINE=$'\n'

    source ~/.profile_cygwin
fi
[[ -f ~/.profile_local ]] && . ~/.profile_local
export HOST_FAMILY

function parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

# export PS1="\[\e]0;\w\a\]\n\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\n\$"
# export PS1="${COLOR_GREEN}\u${COLOR_NC}@${COLOR_HOST}\h${COLOR_NC}:\w${COLOR_PURPLE}\$(parse_git_branch)${COLOR_NC} > "

export PS1="${COLOR_GREEN}\u${COLOR_NC}@${COLOR_HOST}\h${COLOR_BROWN}:\w${COLOR_PURPLE}\$(parse_git_branch)${CYG_NEWLINE}${COLOR_NC}> "

