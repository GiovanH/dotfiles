#!/bin/bash

source ~/dotfiles/support/logging.sh

react_old() {
  if [ -z "$1" ] || [ -z "$2" ]; then
    echo "Usage: react <[./]file-to-watch> <[./]action> <to> <take>"
  elif ! [ -r "$1" ]; then
    echo "Can't react to $1, permission denied"
  else
    TARGET="$1"; shift
    ACTION="$*"
    LTIME=NEVER
    while sleep 1; do
      ATIME=$(stat -c %Z "$TARGET")
      if [[ "$ATIME" != "${LTIME}" ]]; then
        LTIME=$ATIME
        echo -n "$(date -Iminutes) "
        bash -c "$ACTION"
      fi
    done
  fi
}
export -f react_old

react() {
  if [ -z "$1" ] || [ -z "$2" ]; then
    echo "Usage: react [shell command] [file] {file...}"
  else
    # Migration helper
    if [ -f "$1" ] || [ ! -f "$2" ]; then
      logerror "react signature has changed!"
      react_old $*
    fi

    ACTION="$1"; shift

    REACT_SPEED=${REACT_SPEED:-2}
    LTIME=-1
    # Precheck

    # Note special array syntax
    for path in "$@"; do
      if ! [ -r "$path" ]; then
        echo "Can't react to $path, permission denied"
        ls -al "$path"
        return
      fi
    done

    # Spinner
    spin='-\|/'
    spinner_i=0

    while sleep "$REACT_SPEED"; do
      printf "\r[%s] Waiting..." "${spin:$spinner_i:1}"
      spinner_i=$(( (spinner_i+1) %4 ))

      did_run_fuse=
      for TARGET in "$@"; do
        ATIME=$(stat -c %Z "$TARGET")
        printf "\r[%s] Waiting..." "${spin:$spinner_i:1}"
        if [[ "$ATIME" != "${LTIME}" ]] && [[ "$ATIME" -gt "$LTIME" ]]; then
          export LTIME="$ATIME"
          if [ -z "$did_run_fuse" ]; then
            printf "\r              \r"
            # echo -n "$(date -Iminutes) "
            ( eval "$ACTION" )
            did_run_fuse=true
          fi
        fi
      done
    done
  fi
}

# Only execute if invoked directly, not sourced
if [ "${BASH_SOURCE[0]}" -ef "$0" ]; then
  react "$@"
fi

# If sourced, just add the functions to the context.
# END PUBLIC FILE