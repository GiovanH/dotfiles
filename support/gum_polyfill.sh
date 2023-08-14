#!/bin/bash

which gum &> /dev/null && return
# echo "No gum!"

gum() {
  cmd=$1
  shift
  args=$(printf '%q\n' "$@")
  # echo "$args"
  pre=$(echo "$args" | grep -Po ".+?(?= --)")
  post=$(echo "$args" | grep -Po "(?<=-- ).+")

  args_array=("$@")
  for sep_index in "${!args_array[@]}"; do
    [[ "${args_array[$sep_index]}" == "--" ]] && break;
  done
  # post_array="${args_array[@]:$((sep_index+1))}"

  case $cmd in
    spin)
      echo $pre
      $post
      return $?
      ;;
    confirm)
      echo $pre
      echo Ctrl+C to cancel, otherwise continue
      read -n 1
      return $?
      ;;
    choose)
      echo "pre $pre"
      select c in "${args_array[@]:$((sep_index+1))}"; do
       echo $c
       return $?
      done
      ;;
    *)
      echo "Missing polyfill for command $cmd"
      return 1
      ;;
  esac
}