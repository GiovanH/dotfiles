## Conditionals

if [[ expr ]]; then ...; fi

| name        | expr    | example            |
|-------------|---------|--------------------|
| regex       | =~      | [[ $line =~ a+b ]] |
| not         | ! expr  |                    |
| file exists | -e file |                    |
| dir exists  | -d file |                    |
| link exists | -L file |                    |
| var set     | -v var  |                    |
| empty       | -v str  |                    |
| not empty   | -n str  |                    |

## Arrays

| name                | expr                              |
|---------------------|-----------------------------------|
| create array        | name[i] = v                       |
| declare indexed     | declare -a name                   |
| declare associative | declare -A name                   |
| assign              | name=(val1 val2)                  |
| assign              | name=([key1]=val1 [key2]=val2 … ) |
| reference           | ${name[subscript]}                |
| ERROR               | $name[subscript]                  |

## Readline

| keys    | command                  |
|---------|--------------------------|
| C-a C-e | start, end of line       |
| M-f M-b | forward, back word       |
| C-l     | clear screen             |
| M-C-l   | clear screen and display |
| C-r     | reverse search history   |
| C-q     | quoted insert (symbols)  |
| C-t M-t | transpose char, word     |
| M-/     | complete filename        |
| C-g     | abort                         |

## Jobs

cmd & -- start cmd in background
^Z -- suspend current
bg -- resume suspended in background
fg -- resume suspended in foreground
wait -- join background jobs

## Recipes

See `bash_cheatsheet.sh`

## Variable Expansion

| alias    | expr              | if unset | if null  | if set      |
|----------|-------------------|----------|----------|-------------|
| default  | ${var:−word}      | word     | word     | value       |
| assign   | ${var:=word}      | =word    | =word    | nop         |
| require  | ${var:?word}      | word >&2 | word >&2 | nop         |
| iffalsey | ${var:+word}      | null     | null     | word        |

| alias | expr              |             |
|-------|-------------------|-------------|
| slice | ${var:n}          | var[n:]     |
| slice | ${var:n:m}        | var[n:n+m]  |
| len   | ${#var}           | len(var)    |
| sub   | ${var/pat/str}    | r/pat/str/  |
| subg  | ${var//pat/str}   | r/pat/str/g |
| arith | $(( expression )) |             |
|       | "$@"              | "a" "b c"   |
|       | "$*"              | "a b c"     |
