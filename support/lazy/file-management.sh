#!/bin/bash
# File management utilities and backup functions

makebak() {
  # for outname in f; if not outname; write; break
  for date in "$(date -I)" "$(date -Ihours)" "$(date -Iminutes)"; do
    if [ -d "$1" ]; then
      outfile="$(basename "$1").$(sluggify "${date}").tar.gz"
      [[ ! -f "$outfile" ]] && tar czfv "$outfile" "$1" && ls -al "$outfile" && return
    elif [ -f "$1" ]; then
      outfile="$1.$(sluggify "${date}").bak"
      [[ ! -f "$outfile" ]] && cp -v -r --no-clobber "$1" "$outfile" && ls -al "$outfile" && return
    else
      echo "Couldn't backup $1!" 1>&2
    fi
  done
  echo "Couldn't backup $1!" 1>&2
}
export -f makebak

toggledisabled() {
  prename -v 's/(?<!.disabled)$/.disabled/' "$1";
  [[ -f "$1" ]] && prename -v 's/.disabled$//' "$1"
}
export -f toggledisabled

flatten_all_in() {
  if [ -z "$1" ]; then type flatten_all_in; return; fi
  (
    set -eu -o pipefail
    cd "$1"
    prename -v "s/\// - /g" -- */**/*.*
    delete_empty_dirs
  )
}

# shellcheck disable=SC2016
splitpath() {
  if [ -z "$1" ]; then type splitpath; return; fi
  filepath="$1" # "./relative folder/file name.j2.ext"
  rem filepath = '${dirname}/${filestem}.${extension}'
  rem filepath = '${dirname}/${filename}'
  dirname=$(dirname "$filepath") # "./relative folder"
  export dirname
  filename=$(basename -- "$filepath") # "file name.j2.ext"
  export filename
  export extension="${filename##*.}" # "ext"
  export filestem="${filename%.*}" # "file name.j2"
}

pull_out_oneoffs_in() {
  (
    set -eu -o pipefail

    # shellcheck disable=SC2016
    if [ -z "$1" ]; then
      echo 'usage: pull_out_oneoffs_in SOURCEDIR [THRESHHOLD]"'
      echo 'For each subdirectory ($SOURCEDIR/*/), if the count of files in the folder is'
      echo 'lower than THRESHHOLD (default 5), move its files to $SOURCEDIR/unsorted/'
      echo 'unless $SOURCEDIR/keep/$subdir exists.'
      return;
    fi

    subdir="${1}"
    few_threshhold=${2:-5}

    logparam "Moving files from folders with" "$few_threshhold" "or fewer files in" "$subdir" "to" "$subdir/unsorted"

    cd "$subdir"
    mkdir -p ./unsorted/

    while read -r dirname; do
      # count=$(sh -c 'set -- "$0"/*.* && echo $#' "$dirname")
      count=$(find "$dirname" -mindepth 1 -maxdepth 1 | wc -l)
      # logparam "$dirname" "$count"
      if [ "$count" -gt "0" ] && [ "$count" -le "$few_threshhold" ]; then
        if [[ ! -d "./keep/$dirname" ]]; then
          logparam "mv" "$dirname/*" '(' "$count" ") -> " "$subdir/unsorted/"
          # confirm
          mv -v -t ./unsorted/ "$dirname"/*
        else
          logwarning "keep dir './keep/$dirname' exists, not moving files."
        fi
      fi
    done < <(/bin/find "$subdir"/*  -mindepth 0 -maxdepth 0 -type d -not -ipath "*keep*" -and -not -ipath "*unsorted*")

    delete_empty_dirs
  )
}

# Extract an archive file to a directory automatically based on filetype
extractto() {
  if [ -z "$2" ]; then echo "usage: extractto SOURCEFILE DESTDIR"; return; fi
  filename=$1
  dest=$2
  ext="${filename##*.}"
  # Run correct extract command for filetype
  if [[ $ext == "tar" || $ext == "gz" ]]; then
    tar -xf "${filename}" -C "${dest}"
    return $?
  elif [[ $ext == "zip" ]]; then
    unzip -n "${filename}" -d "${dest}"
    return $?
  elif [[ $ext == "rar" || $ext == "7z" ]]; then
    7z x "$filename" -o"${dest}"
    return $?
  else
    echo "Unknown extension $ext for file $filename"
    return 1
  fi
  # Deduplicate leading subdirectories
  # (
    # shopt -s dotglob
    # prename -v 's|^(.+)/\g1/|$1/|g' "$dest"/*/*
    # [[ -d "$dest/$dest" ]] && rmdir -v "$dest/$dest"
  # ) || :;
}
export -f extractto

delete_empty_dirs() {
  # -not -path '*/.*'
  find .  -type d -empty -exec echo "rmdir '{}'" \; -delete;
}
export -f delete_empty_dirs

delete_empty_dirs_in () {(
  if [ -z "$1" ]; then type delete_empty_dirs_in; return; fi
  cd "$1" && delete_empty_dirs
)}

extracttoself() {
  archive="$1"
  splitpath "$archive"
  extractto "$archive" "$dirname/$filestem/"
}

extract_flatten_all() {
  while read -r z; do
    extracttoself "$z" \
    && flatten_all_in "$dirname/" \
    && rm -v "$z"
  done < <(find . \( -iname "*.zip" -o -iname "*.gz" -o -iname "*.rar" \))
}

find_sparse_directories() {
  # List directories with N or fewer files in them
  maxfiles=${1:-'8'}
  find ./*/ -type d -exec sh -c 'set -- "$0"/*.*; [ $# -le '"$maxfiles"' ]' {} \; -print
}

# Use emacs to copy the contents of a file into the X clipboard
copyf() {
  if [ -z "$1" ]; then echo "Usage: copyf <source_text_file>"; return; fi
  if (type xclip 2>/dev/null); then
    cat "$1" | xclip -i
  elif [ -e "/dev/clipboard" ]; then
    cat "$1" > /dev/clipboard
  else
    emacs -Q --eval '(progn
      (setq x-select-enable-clipboard-manager t)
      (switch-to-buffer (generate-new-buffer "clip"))
      (insert-file-contents "'"$1"'")
      (fit-frame-to-buffer)
      (mark-whole-buffer)
      (defun docopy ()
       (interactive)
       (kill-new (buffer-string))
       (x-select-text (buffer-string))
       (sit-for 1) ; wait to flush ???
       (save-buffers-kill-terminal) ; Emacs takes a second to "flush" to the clipboard
      )
      (local-set-key "n" '\''docopy) ; needs interactive window ???
      (message "Press n to copy and exit.")
    )'
  fi
}

# Use emacs to write the contents of the X clipboard to a file
pastef() {
  if [ -z "$1" ]; then echo "Usage: w/ text in your clipboard, pastef <dest_text_file>"; return; fi
  if (type xclip); then
    xclip -o > "$1"
  elif [ -e "/dev/clipboard" ]; then
    cat /dev/clipboard > "$1"
  else
    emacs -Q --eval '(progn
      (setq x-select-enable-clipboard-manager t)
      (find-file "'"$1"'")
      (yank)
      (save-buffer)
      (kill-emacs)
    )'
  fi
  wc "$1"
  tail -n 2 "$1" | cut -c 1-80 -
}

# Binary versions using base64
copybf() {
  # Powershell:
  # using namespace System.Management.Automation.Host
  #
  # param([Parameter(Mandatory)]$filename)
  # [Environment]::CurrentDirectory = (Get-Location -PSProvider FileSystem).ProviderPath
  #
  # write-host "Encoding binary text"
  # $clip = [convert]::ToBase64String((Get-Content -path $filename -Encoding byte))
  #
  # write-host "Writing to clipboard"
  # Set-Clipboard -Value $clip

  if [ -z "$1" ]; then echo "Usage: copybf <source_binary_file>"; return; fi
  base64 "$1" > "$1".b64
  copyf "$1".b64
  rm "$1".b64
}

pastebf() {
  # Powershell:
  # using namespace System.Management.Automation.Host
  #
  # param([Parameter(Mandatory)]$filename)
  # [Environment]::CurrentDirectory = (Get-Location -PSProvider FileSystem).ProviderPath
  #
  # write-host "Decoding binary text"
  # $bytes = [Convert]::FromBase64String((Get-Clipboard))
  #
  # write-host "Writing to ($filename)"
  # [IO.File]::WriteAllBytes($filename, $bytes)

  if [ -z "$1" ]; then echo "Usage: w/ b64 text in your clipboard, pastebf <dest_binary_file>"; return; fi
  pastef "$1".b64
  base64 --decode "$1".b64 > "$1"
  rm "$1".b64
}

copytardir() {
  tar -cvzf - "$@" --exclude tardir.b64 | base64 > tardir.b64
  copyf tardir.b64
  rm tardir.b64
}

tarbak() {
  if [ -z "$1" ]; then echo "usage: tarbak [title] {files...}"; return; fi
  title=$1; shift
  outfile="${title} $(date '+%Y-%m-%d').tar.gz"
  tar -zcv -f "${outfile}" --checkpoint=.100 "$@"
}

sluggify() {
  echo "$@" | iconv -t ascii//TRANSLIT | sed -r s/[^a-zA-Z0-9-]+/_/g | sed -r s/^-+\|-+$//g
  # | tr A-Z a-z
}
export -f sluggify

sedic () {
  if [ -z "$2" ]; then
    echo " Usage: sedic [FLAGS]PATTERN FILES [FILES...]"
  fi

  flags=""
  if [[ $1 == "-"* ]]; then
    flags="$1"
    shift
  fi

  pattern="$1"
  shift

  for path in "$@"; do
    tmpfile=$(mktemp "/tmp/$(basename -- "$path").XXX")
    sed "$pattern" "$path" > "$tmpfile" || continue
    diff -u "$path" "$tmpfile"
    if [ $? -eq 1 ]; then
      confirm && cp -v "$tmpfile" "$path"
    fi
    rm "$tmpfile"
  done
}


ddimage () {( set -e
  target_drive=${target_drive:-"\\\\Deep16/Backup/"}
  if [ -z "$1" -o -z "$2" ]; then
    echo "Take a compressed .img.gz backup of a device and save it to \$target_drive='$target_drive'"
    echo "Usage: ddimage [device] [label]"
    echo "i.e.   ddimage /dev/sde rpi_backup"
    echo "Get partitions from 'mount' or 'cat /proc/partitions'"
    return
  fi
  device=$1
  label=$2
  outfile="${target_drive}/${label}_$(date '+%Y-%m-%d').img.gz"
  echo $outfile
  if [ -f "$outfile" ]; then
    echo "File exists! Aborting."
    # TODO: If file is a stub created by trying this command without permissions, ignore.
    return
  fi
  dd if=$device bs=64K status=progress | gzip -c > "$outfile"
)}
