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
      # end black magic
      if [ "$2" = "after" ] ; then
        PATH=$PATH:$1
      else
        PATH=$1:$PATH
      fi
  esac
}

# Unix settings
export LESS="--chop-long-lines --RAW-CONTROL-CHARS" # see LESS(1)

# Use standard blues
export LS_COLORS=$(echo $LS_COLORS | sed 's/=38;5;27/=34;5;27/g')

# Misc :)
alias whence='type -a' # where, of a sort

# Some shortcuts for different directory listings
alias ls='ls -h --color=auto' # classify files in colour
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

# Programmable completion enhancements
sourceif /etc/bash_completion
sourceif ${HOME}/.bash_completion

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

# sgio's super cool PS1 prompt

export HOSTNAME_NICE="$(hostname -f 2>/dev/null || hostname 2>/dev/null || cat /etc/hostname)"
HOST_FAMILY="UNK"
PSCOLOR_HOST=$PSCOLOR_PURPLE

sourceif "${HOME}/.profile_$(uname -o)"
sourceif "${HOME}/.profile_${HOSTNAME_NICE}"
sourceif "${HOME}/.profile_local"
export HOST_FAMILY

EXIT='?'
TITLE_ANSI='\[\033]0;${EXIT} ${HOSTNAME_NICE}:${PWD/$HOME/~}\007\]'
export PS1=${PSCOLOR_GREEN}'\u'${PSCOLOR_NC}@${PSCOLOR_HOST}'\H'${PSCOLOR_BROWN}':\w'${PSCOLOR_PURPLE}'${PS_GIT_REF}${CYG_NEWLINE}'${PSCOLOR_NC}'${EXIT} '

show_command_in_title_bar () { return; }
# don't break rsync, tramp, etc

if [ "$TERM" == "xterm-256color" ]; then
  # I am sure that $TERM is xterm-256color.
  # Update gnome-terminal title
  show_command_in_title_bar()
  {
    case "$BASH_COMMAND" in
    *\033]0*) # Command sets its own title;
      ;;
    *)
      local this_command=$(HISTTIMEFORMAT= history 1 | sed -e "s/^[ ]*[0-9]*[ ]*//");
      printf "\033]0;${this_command} ${HOSTNAME_NICE}:${PWD/$HOME/~}\007" > /dev/stderr
      ;;
    esac
  }
  trap show_command_in_title_bar DEBUG
  export PS1=${TITLE_ANSI}${PS1}
fi

_make_prompt() {
  # Save return code or '>' if 0
  EXIT=$(if [ $? -eq 0 ]; then echo '>'; else echo $?; fi)
  # Check for a git ref in the cwd
  PS_GIT_REF=$(git symbolic-ref --short HEAD 2>/dev/null)
  if [[ ! -z ${PS_GIT_REF} ]]; then
    PS_GIT_REF=" (${PS_GIT_REF})"
    # Insert newline if git ref (i.e. 'long')
    CYG_NEWLINE=$'\n'
  else
    CYG_NEWLINE=' '
  fi
  # show_command_in_title_bar # show
  # export PS1="\033]0;${EXIT} ${HOSTNAME_NICE}:${PWD/$HOME/~}\007${PSCOLOR_GREEN}\u${PSCOLOR_NC}@${PSCOLOR_HOST}\H${PSCOLOR_BROWN}:\w${PSCOLOR_PURPLE}${PS_GIT_REF}${CYG_NEWLINE}${PSCOLOR_NC}${EXIT} "
}

PROMPT_COMMAND='_make_prompt'

