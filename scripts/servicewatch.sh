#!/bin/bash

sysd_svc="${1:-pcloud.service}"
regex="${2:-corrupted on transfer}"

echo "Info, service '$sysd_svc' will be restarted on match with '$regex'"

while read -r line; do
  # note we DONT quote $regex because we're using ~=
  if [[ "$line" =~ $regex ]]; then
    echo "Info, restarting as line matched : $line"
    systemctl restart "$sysd_svc"
  else
    :;
    # echo "Info, line didn't match: $line"
  fi
done < <( journalctl -u $sysd_svc -f )
