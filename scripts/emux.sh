#!/usr/bin/env bash

# Run a command insite an active emacs instance,
# like a terminal multiplexer.

set -o nounset -o errexit -o pipefail
argcmd="$@"
cmd="${argcmd:=$SHELL}"
exec emacsclient -e <<EOF
(let ((vterm-shell "${cmd}")) (if (fboundp 'vterm) (vterm t) (term vterm-shell)))
EOF
