#!/bin/bash
# This file is for information that will only be needed in interactive, non-script sessions.

# Local Variables:
# flymake-shellcheck-allow-external-files: t
# End:

# Don't warn about masking return values
# shellcheck disable=SC2155

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# sourceif () {
#   [[ -f "$1" ]] && . "$1"; # || echo "missing file $1";
# }

# envmunge () {
#   _var=$1
#   _val=$2
#   _sep=${3:-:}
#   _pos=${4:-:}
#   case "${_sep}${!_var}${_sep}" in
#     *${_sep}"$_val"${_sep}*)
#       ;;
#     *)
#       if [ -z "${!_var}" ]; then
#         # Empty
#         printf -v "$_var" '%s' "${_val}"
#       else
#         # Not empty
#         if [ "$_pos" = "after" ] ; then
#           printf -v "$_var" '%s' "${!_var}${_sep}${_val}"
#         else
#           printf -v "$_var" '%s' "${_val}${_sep}${!_var}"
#         fi
#       fi
#   esac
# }

## Programmable completion enhancements

mkdir -p ~/.local/share/completions/
for shellfile in \
  /etc/bash_completion \
    ~/dotfiles/support/completion.sh \
    ~/.local/share/completions/*sh; do
  sourceif "$shellfile"
done

## Bash options

shopt -s globstar
shopt -s extglob
shopt -s expand_aliases

shopt -s checkwinsize
shopt -s cdspell
shopt -s checkjobs

## History

# per-process history
HISTSUFFIX=$(tty | sed 's/\///g;s/^dev//g')
export HISTFILE="$HOME/.history/$HISTSUFFIX"
touch "$HISTFILE"

# Make bash append rather than overwrite the history on disk
shopt -s histappend
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=10000
export HISTFILESIZE=20000

hgrep() {
  # Search and show live uniqs
  grep -r "$HOME/.history/" -h -e "$@" \
    | perl -ne 'print unless $seen{$_}++'
}

# Append all history
envmunge PROMPT_COMMAND 'history -a' ';' after

## sgio's super cool PS1 prompt

export HOSTNAME_NICE="$(hostname -f 2>/dev/null || hostname 2>/dev/null || cat /etc/hostname)"
case $USER in
  gio)
    PSCOLOR_HOST=$PSCOLOR_RED ;;
  pi)
    PSCOLOR_HOST=$PSCOLOR_YELLOW ;;
esac
case $HOSTNAME_NICE in
  rescuepup)
    PSCOLOR_HOST=$PSCOLOR_PURPLE ;;
  pearl)
    PSCOLOR_HOST=$PSCOLOR_CYAN ;;
esac

export PSCOLOR_HOST="${PSCOLOR_HOST:-$PSCOLOR_PURPLE}"
export PSCOLOR_USER="${PSCOLOR_USER:-$PSCOLOR_GREEN}"

EXIT='?'

export PS1=${PSCOLOR_GREEN}'\u'${PSCOLOR_NC}'@'${PSCOLOR_HOST}'\H'${PSCOLOR_BROWN}':\w'${PSCOLOR_PURPLE}'${PS_GIT_REF}${MAYBE_NEWLINE}'${PSCOLOR_NC}'${EXIT} '

if [ "$TERM" == "xterm-256color" ] || [ "$TERM" == "cygwin" ]; then
  _title_fmt="$(tput tsl)%s$(tput fsl)"
  title() {
    printf "$_title_fmt" "$@" 1>&2
  }

  # shellcheck disable=SC2016
  # TITLE_ANSI=$(tput tsl)'${EXIT} ${HOSTNAME_NICE}:${PWD/$HOME/~}'$(tput fsl)
  # Update window title
  show_command_in_title_bar() {
    # shellcheck disable=SC1001
    case "$BASH_COMMAND" in
    *\033]0*) # Command sets its own title; pass
      ;;
    *)
      local this_command="$(HISTTIMEFORMAT='' history 1 | sed -e "s/^[ ]*[0-9]*[ ]*//");"
      title "${this_command} ${HOSTNAME_NICE}:${PWD/$HOME/~}"
      ;;
    esac
  }
  # trap show_command_in_title_bar DEBUG
  # export PS1="${TITLE_ANSI}${PS1}"

  envmunge PROMPT_COMMAND _make_prompt ';'
fi

_make_prompt() {
  # Save return code or '>' if 0
  # shellcheck disable=SC2181
  EXIT=$(if [ $? -eq 0 ]; then echo '$'; else echo $?; fi)
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

# END PUBLIC FILE
