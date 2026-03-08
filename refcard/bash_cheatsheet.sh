#!/bin/bash

# Parse delimited input
parse_input() {
  # With known number
  read -r dog cat < <(echo "mandy carmie")
  echo Dog is $dog, cat is $cat

  IFS=: read -r dog cat < <(echo "mandy:carmie")
  echo Dog is $dog, cat is $cat
}

# Case
echo -n "The $ANIMAL has "
case $ANIMAL in
  horse | dog | cat) echo -n "four";;
  man | kangaroo ) echo -n "two";;
  *) echo -n "an unknown number of";;
esac
echo " legs."

# Only execute if invoked directly, not sourced
if [ "${BASH_SOURCE[0]}" -ef "$0" ]
then
  # resolve
  parse_input
fi
