#!/bin/bash

source ~/dotfiles/support/.bash_colors

invis() {
  echo -ne "\e[8m$*\e[0m\r"
}

logmajor() {
  echo
  echo -e "${COLOR_BOLD}$*${COLOR_NC}"
}

logparam() {
  # Log, alternating colors such that every other arg is a highlighted param.
  # Use LP_LABELC to format odd strings, LP_PARAMC to format even strings.
  for ((i=1; i <= ${#@}; i+=2)); do
    part=( "${@:i:2}" )
    echo -n -e "${LP_LABELC:-$COLOR_NC}${part[0]} "
    # part[1] can be empty if count is odd
    echo -n -e "${LP_PARAMC:-$COLOR_CYAN}${part[1]:-} "
  done
  echo -e "${COLOR_NC}"
}

logmparam() { echo && LP_LABELC=$COLOR_BOLD logparam "$@"; }

logerror() {
  echo -e "${COLOR_RED}❌ $*${COLOR_NC}" > /dev/stderr
}

logwarning() {
  echo -e "${COLOR_YELLOW}⚠ $*${COLOR_NC}" > /dev/stderr
}

# Colorize stderr. 'colorerr {command}'
colorerr() {
  set -o pipefail
  "$@" 2> >(sed $'s,.*,\e[31m&\e[m,'>&2)
}

if [ "${BASH_SOURCE[0]}" -ef "$0" ]
then
  echo Test:
  invis "You don't see this"
  logmajor "Major"
  logparam "Parameter is" "highlighted"
  logerror "Error"
  logwarning "Warning"
  colorerr "touch /nosuch/file"
fi
