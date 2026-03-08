#!/bin/bash

which gum &> /dev/null && echo "Don't replace gum with gum polyfill!" && return

gum() {
  cmd=$1
  shift
  args=$(printf '%q ' "$@")
  pre=$(echo "$args" | grep -Po ".+?(?= -- )")
  post=$(echo "$args" | grep -Po "(?<= -- ).+")
  args_array=("$@")
  for sep_index in "${!args_array[@]}"; do
    [[ "${args_array[$sep_index]}" == "--" ]] && break;
  done
  # post_array="${args_array[@]:$((sep_index+1))}"

  case $cmd in
    spin)
      title=$(echo "$args" | grep -Po '(?<=--title ).+(?= --)|$' || echo "Running $post")
      echo $title >&2
      $post
      return $?
      ;;
    confirm)
      echo $@
      echo Press any key to continue or Ctrl+C to cancel
      read -n 1 </dev/tty
      return $?
      ;;
    choose)
      echo "$pre"
      select c in "${args_array[@]:$((sep_index+1))}"; do
        if [[ -n "$c" ]]; then
          echo $c
          return $?
          break;
        fi
      done
      ;;
    *)
      echo "Missing polyfill for command $cmd"
      return 1
      ;;
  esac
}
