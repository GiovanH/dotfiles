#!/bin/bash

hyperloop() {

  # Run command and extract command options in the form of <(cmd)> from
  # stdout, then prompt the user to run one of them.

  hyperl_d="./hyperl"
  [ -d "/run/user/$UID/" ] && hyperl_d=$(mktemp -d /run/user/$UID/hyperl-XXXXXXXX)

  hyperloop_menu=$1
  hl_c="$hyperloop_menu"
  shift 1;
  mkdir -p .hyperl/
  mkfifo "${hyperl_d}"/pipe

  (
  set +m
  trap "rm -r \"${hyperl_d}\"; exit" SIGINT SIGTERM
  while :; do
    # Run command c and capture output
    # Hack: Capture output w/o creating subshell with new scope
    # cat "${hyperl_d}"/pipe | tee "${hyperl_d}"/last &
    # eval $hl_c > "${hyperl_d}"/pipe

    eval $hl_c | tee "${hyperl_d}"/last

    # Eval selections from output
    while read -r cmd; do
      echo $cmd
      eval $cmd
    done < <(perl -l -n -e 'print "$1" while m/\<\(!(.+?)\)\>/g' "${hyperl_d}"/last)

    # Pull context from output
    readarray -t hl_cmds < <(
      echo $hyperloop_menu;
      for c2 in "$@"; do echo $c2; done
      perl -l -n -e 'print "$1" while m/\<\(([^!].+?)\)\>/g' "${hyperl_d}"/last
    )
    # Hide selection if choices are long (output + choices > screen)
    [ $(dc --expression "$(wc -l "${hyperl_d}"/last | awk '{print $1}') ${#hl_cmds[@]} + p") -gt $(tput lines) ] && gum confirm --
    # Prompt until reply is valid
    hl_c=$(gum choose -- "${hl_cmds[@]}")
  done
  )
}
export -f hyperloop

hyperl_test_nesting() {
  echo "<(!hyperl_nested() { echo 'scoped!'; })>"

  echo "<(echo root | more)>"
  echo "<(hyperl_nested | less)>"
}
