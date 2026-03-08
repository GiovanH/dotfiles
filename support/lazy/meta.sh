#!/bin/bash


shhelp() {
  cat ~/dotfiles/home/{.bash_profile,.bashrc,} | frem '/alias (.+?)=(.+)/g' '$1' | column
  cat ~/dotfiles/home/{.bash_profile,.bashrc,} | frem '/^ *([A-Za-z_-]+)\(\)/g' '$1' | column
}

shtranslate() {
  # Given a shell script, create a globally usable version
  # by expanding any local shell aliases or functions.
  _extractfont() {
    fontout=$(mktemp)
    fontface="$2"
    emacs "$1" --batch --no-init-file \
    --eval "(require 'text-property-search)" \
    --eval "(require 'subr-x)" \
    --eval "(font-lock-mode 1)" \
    --eval "(font-lock-fontify-buffer)" \
    --eval "(normal-mode)" \
    --eval '(message "%S" major-mode)' \
    --eval "(defun get-strings-of-face (facematch)
    (setq words '())
    (save-excursion
      (goto-char 0)
      (while (setq match (text-property-search-forward 'face facematch t))
        (push
          (string-trim
            (buffer-substring-no-properties
              (prop-match-beginning match)
              (prop-match-end match)))
          words)
      )
    )
    (reverse words))" \
    --eval '(append-to-file (string-join (get-strings-of-face '"$fontface"') "\n") nil "'"$fontout"'")' \
    --eval '(kill-emacs)'

    cat "$fontout"
    rm "$fontout"
  }

  input="$1"
  header=$(mktemp)

  commands=$(_extractfont "$input" nil | grep -o -P '^ *[^#][^=]+?( |\n|$)' | sort -u)
  echo "$commands" 1>&2
  for c in $commands; do
    t="$(type "$c")"
    case $t in
      "$c is a shell builtin")
        ;;
      "$c is hashed "*)
        ;;
      "$c is a function"*)
        type "$c" | grep -v "is a function" | sed -E 's/ (rem|eg) (.+);/ # \2 /g' >> "$header"
        ;;
      "$c is aliased to "*)
        alias "$c" >> "$header"
        ;;
      *)
        echo UNKNOWN 1>&2
        echo "$t" 1>&2
        ;;
    esac
  done

  headered=$(mktemp)

  cat "$header" > "$headered"
  cat "$input" >> "$headered"

  for var in $(grep -o -P 'COLOR_[A-Z]+' "$headered"); do
    set | grep "^$var=" >> "$header"
  done

  head -n 1 "$input"
  echo
  cat "$header"
  echo
  tail -n +2 "$input"
}