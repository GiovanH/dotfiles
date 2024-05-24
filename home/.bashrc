# ~/.bashrc: executed by bash(1) for interactive shells.
# Also invoked manually by .bash_profile
# This file is for globals

## Global script utils

# Add an item to the PATH list.
# From /etc/profile, where it's automatically unset :(
pathmunge () {
  case ":${PATH}:" in
    *:"$1":*)
      ;;
    *)
      if [ "$2" = "after" ] ; then
        PATH=$PATH:$1
      else
        PATH=$1:$PATH
      fi
  esac
}

# Find the first real executable in a list, prioritizing user files and then system builtins instead of PATH order.
getex () {
  for q in $@; do
    for p in '~/.local/bin/' '/usr/bin/' '/bin/' ''; do
      which "$p$q" &>/dev/null && echo "$p$q" && return
    done
  done
}
export -f getex

# Copy a bash function
copy_function () {
  local ORIG_FUNC=$(declare -f $1)
  local NEWNAME_FUNC="$2${ORIG_FUNC#$1}"
  eval "$NEWNAME_FUNC"
}

## Unix settings

export LESS="--chop-long-lines --RAW-CONTROL-CHARS" # see LESS(1)

# IDEs should be invoked manually; no emacs here.
export VISUAL=$(getex vim vi nano)
export EDITOR=$VISUAL

# Use standard blues
export LS_COLORS=$(echo $LS_COLORS | sed 's/=38;5;27:/=34;27:/')

# Common color/format aliases
alias ls='ls -h --color=auto'                 # classify files in colour
alias dir='ls --color=auto --format=vertical'

# User bin, man, info

pathmunge "${HOME}/dotfiles/scripts"
pathmunge "${HOME}/bin"
pathmunge "${HOME}/.local/bin"

MANPATH="${HOME}/man:${HOME}/.local/man:${MANPATH}"
INFOPATH="${HOME}/info:${HOME}/.local/info:${INFOPATH}"

CPATH="${HOME}/.local/include:${CPATH}"
LD_LIBRARY_PATH="${HOME}/.local/lib:${LD_LIBRARY_PATH}"

## User functions, locals

sourceif () {
  [[ -f "$1" ]] && . "$1"; # || echo "missing file $1";
}

sourceif ~/dotfiles/support/.bash_colors
sourceif ~/.bash_personal

export HOSTNAME="$($(getex hostname) -f 2>/dev/null || cat /etc/hostname)"

# Source specifics for OS
sourceif ~/dotfiles/local/.bashrc_$(uname -o | sed 's/[^a-zA-Z0-9]/_/g')

# Source specifics for HOST or DOMAIN
for localrc in ~/dotfiles/local/.bashrc_*; do
  # Chop suffix from rc filename
  localrc_suffix="${localrc##~/dotfiles/local/.bashrc_}"
  # Source if HOSTNAME ends with SUFFIX
  [[ "$HOSTNAME" == *"${localrc_suffix}" ]] && sourceif "$localrc"
done

# Source manual local file
sourceif ~/.bashrc_local

