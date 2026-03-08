## Patterns

| Match                       | Token           | Inverse    | Captures |
|-----------------------------|-----------------|------------|----------|
| Any whitespace character    | `\s`            | `\S`       | Yes      |
| Any digit                   | `\d`            | `\D`       | Yes      |
| Any word                    | `\w`            | `\W`       | Yes      |
| A word boundary             | `\b`            | `\B`       | No       |
| Non-capturing group         | `(?:...)`       |            | No       |
| Named group                 | `(?P<name>...)` |            | Yes      |
| Comment group               | `(?#...)`       |            | No       |
| Contents of capture group 1 | `\g<1>`         |            | Yes      |
| Lookahead                   | `(?=...)`       | `(?!...)`  | No       |
| Lookbehind                  | `(?<=...)`      | `(?<!...)` | No       |

## Flags

- **G**lobal
- **M**ultiline
- case **I**nsensitive
- **x** ignore whitespace
- **S**ingle line
- **U**nicode
- **A**scii
