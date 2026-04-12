#!/bin/bash

if [ -z "$1" ]; then
echo -e "frem: Format RegEx Matches. Like grep, but with match formatting\n"
echo " Usage: frem PATTERN                  FORMAT           FILES [FILES...]"
echo "   e.g. frem '/(.+)=(.+)/'            '\$1: \$2'         gradle.properties"
echo "        frem '/(?<k>.+)=(?<v>.+)/'    '\$+{k}: \$+{v}'   gradle.properties"
echo "        frem '/(\d+) (?:.+? )*(.+)/g' 'proc=\$2 pid=\$1' <(ps)"
echo "        frem '/(.+)+(.+)/e'           '(\$1 + \$2)'      calc.txt"
echo "        PERLFLAGS='-0777' frem ...     ...             multilinefile.txt"
echo "   Note: /e is *not* passed through to perl but uses branches under the hood."
return
fi
pattern=$1
reply="${2:-\$1}"
files="${*:3}"
flags=$(echo "$pattern" | rev | cut -d '/' -f 1)

PERLFLAGS="${PERLFLAGS:--l}"
op="if"

# Eval: unquote and eval reply expression
if [[ "$flags" == *"e"* ]]; then
repl="$reply"
oldflags="$flags"
flags="${flags//e/}"
pattern="${pattern//\/$oldflags//$flags}"
else
reply=$(echo "$reply" | sed 's/"/\\"/g')
repl="\"$reply\""
fi
if [[ "$flags" == *"g"* ]]; then
op="while"
fi
if [[ "$flags" == *"m"* ]]; then
PERLFLAGS='-0777'
fi
# shellcheck disable=SC2086
perl ${PERLFLAGS} -n -e "print $repl $op $pattern" "${files:--}"