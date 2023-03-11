# ~/.bash_profile: executed by bash(1) for interactive login or non-login shells.
# This file is for information that will only be needed in interactive, non-script sessions.

# source the users bashrc if it exists
if [ -f "${HOME}/.bashrc" ] ; then
  source "${HOME}/.bashrc"
fi

# sgio's super cool PS1 prompt

export HOSTNAME_NICE="$(hostname -f 2>/dev/null || hostname 2>/dev/null || cat /etc/hostname)"
HOST_FAMILY="UNK"
PSCOLOR_HOST=$PSCOLOR_PURPLE

[[ -f ~/.profile_$(uname -s) ]] && . "~/.profile_$(uname -s)"
[[ -f ~/.profile_${HOSTNAME_NICE} ]] && . "~/.profile_${HOSTNAME_NICE}"
[[ -f ~/.profile_local ]] && . ~/.profile_local
export HOST_FAMILY

EXIT='?'
TITLE_ANSI=''
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
