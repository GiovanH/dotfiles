#!/bin/bash
# Emacs integration and utilities
# Extracted from .bash_personal for better organization
#
# Functions:
# - ensure_ev_daemon: Start emacs daemon if not running
# - ev, evw, ec, ecw, magit: Various emacs/emacsclient shortcuts
# - emacs_repair, emacs_repair_heavy: Emacs package maintenance
# - smerge, gm_smerge_all: Git merge conflict resolution
# - ediff: File comparison tool

# alias emacsw=$(getbin emacs-w32 emacs) # Windowed
# alias emacs="emacs -nw --no-splash" # Terminal

alias ev="emacs" # -l ~/.emacs.d/ev-init.el"
alias evw="emacsw" # -l ~/.emacs.d/ev-init.el"

ensure_ev_daemon() {
  if [ "$(ps uxf | grep -E "emacs" | grep "daemon" || pgrep -a -f 'emacs.+daemon')" ]; then
    :
  else
    ev --daemon
  fi
}

alias ec="ensure_ev_daemon; emacsclient --create-frame -nw -t --eval '(menu-bar-mode -1)'"
# shellcheck disable=2139
alias ecw="ensure_ev_daemon; $(getbin emacsclientw emacsclient-w32 emacsclient) --create-frame --eval '(menu-bar-mode 1)'"
alias magit='ec --eval "(progn (magit) (delete-other-windows))"'

# ~/.emacs.d/**/*.elc
emacs_repair() {
  rm -v ~/.emacs.d/{.,lisp}/*.elc; emacs --debug-init --eval '(progn
    (load-file \"~/.emacs.d/init-extra.el\")
    (my-package-update)
    (kill-emacs)
  )'
}
emacs_repair_heavy() {
  rm -v ~/.emacs.d/**/*.elc; emacs --debug-init --eval '(progn
    (load-file \"~/.emacs.d/init-extra.el\")
    (my-package-update)
    (byte-recompile-directory (expand-file-name \"~/.emacs.d\") 0)
  )'
}

# Use emacs smerge/ediff mode to resolve a git conflicted file.
smerge() {( set -ev
  if [ -z "$1" ]; then
    echo "Usage: smerge <conflicted_file>"
    echo "Automatically quits emacs and 'git add's the file when ediff is done."
    return
  fi
  emacs "$1" --eval "$(cat << EOF
(progn
  (add-hook 'ediff-after-quit-hook-internal 'save-buffers-kill-emacs)
  (smerge-mode)
  (smerge-ediff)
  (let ((cp
        (car (seq-filter
          (lambda (window)
            (equal "*Ediff Control Panel*" (buffer-name (window-buffer window))))
          (window-list-1 nil 0 t)))))
    (message cp)
    (select-window cp)
  )
)
EOF
)"
  git add "$1"
)}

gm_smerge_all() {
  for f in $(git status -s | grep -Po '^(AA|UU) \K(.+)$'); do
    smerge "$f"
  done
}

ediff() {
  if [ -z "$1" ] || [ -z "$2" ]; then echo "Usage: ediff <file_A> <file_B>"; return; fi
  emacs -nw --eval "(progn
      (add-hook 'ediff-after-quit-hook-internal 'save-buffers-kill-emacs)
      (ediff-files \"$1\" \"$2\")
  )";
}
