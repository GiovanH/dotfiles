#!/bin/bash

# Local Variables:
# flymake-shellcheck-allow-external-files: t
# End:

# Use standard blues
export LS_COLORS=$(echo $LS_COLORS | sed 's/=38;5;27:/=34;27:/')

# Common color/format aliases
alias ls='ls -h --color=auto'                 # classify files in colour
alias dir='ls --color=auto --format=vertical'

export LESS="--chop-long-lines --RAW-CONTROL-CHARS" # see LESS(1)

# IDEs should be invoked manually; no emacs here.
export VISUAL=$(getbin vim vi nano)
export EDITOR=$VISUAL

# User bin, man, info

pathmunge "${HOME}/dotfiles/scripts"
pathmunge "${HOME}/.local/bin"

envmunge MANPATH "${HOME}/.local/man"
envmunge INFOPATH "${HOME}/.local/info"
envmunge CPATH "${HOME}/.local/include"
envmunge LD_LIBRARY_PATH "${HOME}/.local/lib"

## Help and docs


makehelp() {
  # List targets from a makefile
  # cat Makefile \
  #   | grep -Pzo "(?<=\n).+:.*\n(\t#[^\n]+\n)*" \
  #   | sed 's/\t/  /g'
  grep -E '(^[^.#[:space:]].*:)|(##)' Makefile
}

q() {
  llm -m anthropic/claude-3-5-haiku-latest \
    -s "Answer in as few words as possible. Use a brief style with short replies." \
    "$*"
}

## Shell hacks

# Use scripts as extensionless commands
for sc in ~/dotfiles/scripts/*.{py,sh}; do
  [[ ! -f "$sc" ]] && continue
  filename="$(basename -- "$sc")"
  hash -p "$sc" "${filename%.*}"
done

use() {
  require_gum
  if [ -z "$1" ]; then
    echo "use: Pick alternative executable from to set as an alias"
    echo "Usage: use [query] (choose from PATH)"
    echo "  e.g  use find"
    echo "   or: use [path to bin]"
    echo "  e.g  use ./bin/ffmpeg"
    return
  fi
  if [[ -f "$1" ]]; then
    # Literal file
    binpath="$(readlink -f "$1")"
    cmd="$(basename "$1")"
  else
    cmd="$1"
    binpath=$(eval "gum choose -- $(type -ap "$1" | sed "s|$1 is ||" | linestostrings)" | tr -d '\n')
  fi
  eval "alias '$cmd'='$(printf "%q" "$binpath")'"
  type "$cmd"
}

## Common functions and basic aliases

# shellcheck disable=SC2139
alias killall="killall -u $USER" # only killall own processes

# cat with labels
alias catl="tail -v -n +1"

# cd + ls
cdl() { cd "$@" && ls; }

# This violates my "no alias just for some standard switches" rule but I consistently forget these commands so I'm documenting them
# Perl printwhile program, equivalent to sed -E
alias psed='perl ${PERLFLAGS:-} -p -e'
# perl printwhile program inplace, equivalent to sed -i
alias psedi='perl ${PERLFLAGS:-} -pi -e'

# shellcheck disable=2154
alias uniqs='perl -ne '\''print unless $seen{$_}++'\'''

# use bfs if installed
# shellcheck disable=SC2139
# alias find="$(getbin bfs find)"

# Fast find file by partial name
ff() { find . -iname "*${*}*"; }

# Unified diff piped into vim for colouring.
vdiff() { diff --unified "$@" | vim -R - ; }

# No-op, to be used as magic comments that show up in `type`
rem() { return; }

alias require_gum="which gum &> /dev/null || source ~/dotfiles/support/gum_polyfill.sh"

# Clean up space. Overwritten in local files based on platforms/hosts! Just sets up functions here.
# shellcheck disable=SC2317
clean () {
  shopt -s globstar

  require_gum

  _clean_files_in_dir() {
    if [ -z "$1" ]; then echo "No files!" >/dev/stderr; return; fi
    gum spin --title "Scanning $1" --show-output -- du -sh "${1:?}"
    gum spin --title "Cleaning $1" --show-output -- rm -rv "${1:?}"/*
    echo "Finished $@"
  }

  _clean_subdirs_in_dir() {
    if [ -z "$1" ]; then echo "No files!" >/dev/stderr; return; fi
    gum spin --title "Scanning $1" --show-output -- du -sh "${1:?}"
    gum spin --title "Cleaning $1" --show-output -- rm -rv "${1:?}"/*/
    echo "Finished $@"
  }

  _clean_paths() {
    if [ -z "$1" ]; then echo "No files!" >/dev/stderr; return; fi
    gum spin --title "Scanning $1" --show-output -- du -sh "$@"
    gum spin --title "Cleaning $1" --show-output -- rm -rv "$@"
    echo "Finished $@"
  }
}

## Data viz

# Pipe a stream into livesort and see a sorted list in real time
livesort() {
  linesfd=$(mktemp)
  while read -r line; do
    echo "$line" >> "$linesfd"
    clear
    sort "$@" "$linesfd"
  done
  rm "$linesfd"
}

alias tsv="column -t -s $'\t'" # Pretty-print tsv data
alias csv="column -t -s ," # Pretty-print csv data
# recursively grep a terragrunt directory, excluding junk

# [{a: b}, {a: c}] -> "A, B, C"
# shellcheck disable=SC2154
alias jqcsv="jq -r '
  (map(keys) | add | unique) as \$cols
  | map(. as \$row | \$cols | map(\$row[.])) as \$rows
  | \$cols, \$rows[] | @csv'"

csv2html() {
  echo "<table>" ;
  print_header=true
  while read -r INPUT ; do
    row="<tr><td>${INPUT//,/</td><td>}</td></tr>" ;
    if $print_header;then
      echo "$row" | sed -e 's/td>/th>/g'
      print_header=false
    else
      echo "$row"
    fi
  done < "$1" ;
  echo "</table>"
}

## Data manipulation

pyp() {
  python -c 'print('"$@"')'
}

linestostrings() {
  # Pipe this a newline-delimited file to get a single quote delimited oneline.
  tr -d '\015' | awk 'BEGIN { ORS="" } { print p"\047"$0"\047"; p=" " } END { print "\n" }'
}

## Console output

. "$HOME/dotfiles/support/logging.sh"

# Log screen session directly to html ansi
ansilog() {
  screen -L "$@"
  ansi2html.sh < screenlog.0 > screenlog.html
  rm screenlog.0
  ls -al screenlog.html
}

## Flow control

confirm() {
  echo "$@"
  echo Ctrl+C to cancel, otherwise continue;
  read -r -n 1 < /dev/tty
}

# Execute command $1 in directory $2..$N, aborting on non-zero return.
dmap() {( # set -e  # fail early
  cmd="$1"
  shift # pop command from arg list
  for path in "$@"; do
    pushd "$path"
    $cmd
    test $? -gt 0 && pwd && read -r -n 1 -p "Ctrl+C to quit, any other key to continue. "
    popd &>/dev/null
  done
)}

## File management
# Extracted to support/file-management.sh

## Software

# Node version manager lazy-loader
if [[ -d "$HOME/.nvm" ]]; then
  nvm() {

    export NVM_DIR="$(cygpath $HOME 2>/dev/null || echo "$HOME")/.nvm"
    sourceif /usr/share/nvm/init-nvm.sh
    sourceif "$NVM_DIR/nvm.sh"  # This loads nvm
    sourceif "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
    echo "nvm initialized"
    nvm "$@"
  }
fi

## Git -- git.sh

## Docker

alias docker-compose-reload="(set -x; \
  docker-compose pull && \
  docker-compose down && \
  docker-compose up -d && \
  docker-compose logs -f
)"

alias docker-compose-kill="(set -eu -o pipefail; docker-compose ps -q | xargs -r docker inspect --format '{{.State.Pid}}' | xargs -r sudo kill -9); docker-compose down"

## Emacs stuff
# Extracted to support/emacs.sh and support/file-management.sh

## ytdlp archive

ytdlpa() {
  yt-dlp "$@" \
    --yes-playlist \
    --abort-on-unavailable-fragment \
    --write-info-json \
    --embed-chapters --embed-subs --embed-thumbnail --merge-output-format mkv \
    -o "[%(upload_date>%Y-%m-%d)s] %(title)s %(vcodec)s,%(acodec)s.%(ext)s"
}

## Modern personal

pip_update() {
  require_gum

  choice_table=$(
    gum spin --title "Loading outdated packages" --show-output -- \
      python3 -m pip list --outdated
  )
  choices=$(
    gum choose --no-limit -- $(\
      echo "$choice_table" | tail -n +3 | cut -d' ' -f 1
  ))
  gum confirm "$choices" && \
    python3 -m pip install -U $choices
}

alias suffer="echo \$(cat /dev/urandom | tr -cd 'uawgkh' | head -c 20)"
alias good="echo \$(cat /dev/urandom | tr -cd 'mhuwg' | head -c 20)"

# Misc

## Goofs

alias motd="shuf -n 1 ~/dotfiles/home/motd.txt"
alias snake="emacs -l ~/.emacs.d/runsnake.el --no-splash"
t2() {
  xterm -geometry 50x26-0-0 -bg "#303030" -fg "#666" -fa 'DejaVu Sans Mono' -fs 6 -title xterm \
    -e "emacs -nw -q --eval '(progn
      (add-to-list (quote load-path) \"~/.emacs.d/lisp/\")
      (require (quote seq))
      (load-file \"~/.emacs.d/lisp/tetris2.el\")
      (tetris)
    )'";
}

# END PUBLIC FILE
