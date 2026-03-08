#!/bin/bash

# Squash fixup commits between head and the latest push
alias autosquash='git rebase --interactive --autosquash $(git rev-parse origin/HEAD)'

gm_rm_their_deletions()  { git rm  "$(git status -s | grep -Po '^.D \K(.+)$')"; }
gm_rm_our_additions()    { git add "$(git status -s | grep -Po '^AU \K(.+)$')"; }
gm_add_their_additions() { git add "$(git status -s | grep -Po '^.A \K(.+)$')"; }
gm_wassup() { tail "$(git status -s | grep -Po '^(.D|AU|A.) \K(.+)$')" < /dev/null; }

git_migrate_http() {
  git remote -v \
    | frem \
      '/([^ ]+)[\t ]+https?:\/\/github.com\/(.+) \(push\)/g' \
      'git remote set-url $1 git\@github.com:$2' \
      -
}

git_clone_or_update() {
  eg Clone a repo, or pull-update it if it already exists.
  repo=$1
  autodest=${repo##*/} # ## delete longest match of pattern from start
  autodest=${autodest%.git} # %  delete shortest match of pattern from end
  export gitdest="${2:-$autodest}"
  git clone "$repo" "$gitdest" 2>/dev/null \
    || (cd "$gitdest" && git stash; git pull)
  return 0
}

# This should always fail in dangerous case, but replacing the -d with -D makes it work. SHWD.
alias git_purge_branches='git_old_local_branches | while read r; do echo $r; git branch -d $r; done'

git_cherry_pick_jira() {
  if [ -z "$1" ] || [ -z "$2" ]; then
    echo "cherry_pick_jira: cherry-pick all the commits for a specific jira ID since a given ref"
    echo ""
    echo "Usage: git_cherry_pick_jira [JIRAID] [refrange]      {cherry-pick options}"
    echo " e.g.  git_cherry_pick_jira 'JIRA-1' 'release/1.0..'"
    echo "       git_cherry_pick_jira 'JIRA-1' 'HEAD~20..'     '--no-commit'"
    echo "       git_cherry_pick_jira 'JIRA-1' '..develop'     # backport"
    echo ""
    return
  fi

  jiraid=$1
  refrange=$2
  cp_opts=$3

  (set -e
    logmajor "Fetching all refs"
    git fetch --all --tags --progress
    git-show-ref --heads

    logmajor "Writing todo list"
    # Rebase settings: commits since release head matching our jira ID in reverse order, no merges.
    logopts="--grep=^${jiraid} --reverse --no-merges ${refrange} "
    echo git log ${logopts}
    todo_list=$(git log --pretty=%H ${logopts}) # Todo list as a list of hashes
    # Pretty-print todo list for user
    git log --abbrev-commit --decorate ${logopts} \
      --format=format:'%C(yellow)%h%C(reset) - %C(dim green)%<(12,trunc)(%ci)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(auto)%d%C(reset)' ${logopts}

    [ -z "$todo_list" ] && echo "${COLOR_BOLD}Empty todo list!${COLOR_NC} Aborting" && return 1
    logmajor "Cherry-picking commits"
    git cherry-pick ${todo_list} --strategy=recursive -X theirs $cp_opts

    logmajor "Cherry-pick done."
    git diff ${refrange} --stat
    git status
  )
}

git_log_search() {
  git log -S'$@' -p | grep -i '$@'
}