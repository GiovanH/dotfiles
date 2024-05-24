#!/bin/bash
# ~/.bash_profile: executed by bash(1) for interactive login or non-login shells.
# This file is for information that will only be needed in interactive, non-script sessions.

## Fixes

# source the users bashrc (including any locals) if the profile file was the entrypoint
if [ -f "${HOME}/.bashrc" ] ; then
  source "${HOME}/.bashrc"
fi

if [ ! -z "$TMOUT" ]; then
  echo "Detected ephemeral shell, bootstrapping real session."
  exec env TMOUT= bash --init-file ~/.bash_profile
fi

## Programmable completion enhancements

mkdir -p ~/.local/share/completions/
for shellfile in \
  /etc/bash_completion \
    ~/dotfiles/support/.bash_completion \
    ~/.local/share/completions/*sh; do
  sourceif "$shellfile"
done

## Bash options

shopt -s globstar
shopt -s extglob
shopt -s expand_aliases
shopt -s checkwinsize
shopt -s cdspell

# stop killing evw
shopt -s checkjobs

# per-process history
[[ -d ~/.history ]] || mkdir --mode=0700 ~/.history
[[ -d ~/.history ]] && chmod 0700 ~/.history
mkdir -p "$HOME/.history"
HISTSUFFIX=`tty | sed 's/\///g;s/^dev//g'`
export HISTFILE="$HOME/.history/$HISTSUFFIX"
touch $HISTFILE


# Make bash append rather than overwrite the history on disk
shopt -s histappend
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=10000
export HISTFILESIZE=20000

alias hgrep="grep -r $HOME/.history/ -h -e"

# Append all history
export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND;}history -a"

## sgio's super cool PS1 prompt

export HOSTNAME_NICE="$(hostname -f 2>/dev/null || hostname 2>/dev/null || cat /etc/hostname)"
case $USER in
  gio)
    PSCOLOR_HOST=$PSCOLOR_RED ;;
  pi)
    PSCOLOR_HOST=$PSCOLOR_YELLOW ;;
esac
case $HOSTNAME_NICE in
  pearl)
    PSCOLOR_HOST=$PSCOLOR_CYAN ;;
esac

export PSCOLOR_HOST=${PSCOLOR_HOST:-$PSCOLOR_PURPLE}
export PSCOLOR_USER=${PSCOLOR_USER:-$PSCOLOR_GREEN}

EXIT='?'

export PS1=${PSCOLOR_GREEN}'\u'${PSCOLOR_NC}'@'${PSCOLOR_HOST}'\H'${PSCOLOR_BROWN}':\w'${PSCOLOR_PURPLE}'${PS_GIT_REF}${MAYBE_NEWLINE}'${PSCOLOR_NC}'${EXIT} '

if [ "$TERM" == "xterm-256color" ]; then
  # I am sure that $TERM is xterm-256color.
  # shellcheck disable=SC2016
  TITLE_ANSI='\[\033]0;${EXIT} ${HOSTNAME_NICE}:${PWD/$HOME/~}\007\]'
  # Update window title
  show_command_in_title_bar()
  {
    case "$BASH_COMMAND" in
    *\033]0*) # Command sets its own title; pass
      ;;
    *)
      local this_command=$(HISTTIMEFORMAT= history 1 | sed -e "s/^[ ]*[0-9]*[ ]*//");
      printf "\033]0;%s ${HOSTNAME_NICE}:${PWD/$HOME/~}\007" "${this_command}" > /dev/stderr
      ;;
    esac
  }
  trap show_command_in_title_bar DEBUG
  export PS1=${TITLE_ANSI}${PS1}
fi

_make_prompt() {
  # Save return code or '>' if 0
  # shellcheck disable=SC2181
  EXIT=$(if [ $? -eq 0 ]; then echo '>'; else echo $?; fi)
  # Check for a git ref in the cwd
  PS_GIT_REF=$(git symbolic-ref --short HEAD 2>/dev/null)
  if [[ -n ${PS_GIT_REF} ]]; then
    PS_GIT_REF=" (${PS_GIT_REF})"
    # Insert newline if git ref (i.e. 'long')
    MAYBE_NEWLINE=$'\n'
  else
    MAYBE_NEWLINE=' '
  fi
}

PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND;}_make_prompt"

