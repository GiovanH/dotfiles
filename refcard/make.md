<https://imfeld.dev/notes/makefile_cheatsheet>

# References
- An example that will do what you want 90% of the time: [https://ddrscott.github.io/blog/2021/i-heart-make/](https://ddrscott.github.io/blog/2021/i-heart-make/)

```Makefile
cbr_files = $(wildcard *.cbr)
# Quick substitution
# Alternatively, $(patsubst %.cbr,%.pdf,$(cbr_files))
pdf_files = $(cbr_files:.cbr=.pdf)
image_dir = .images-$<

all: ${pdf_files}
    @echo All done

clean:
    rm ${pdf_files}

%.pdf: %.cbr
    @echo Building $< into $@
    rar x $< ${image_dir}/
    find ${image_dir}/ -size 0 -ls -delete
    magick convert ${image_dir}/* $@
    rm -rf ${image_dir}
    @echo $@ is ready
```

- More in-depth tutorial: [https://makefiletutorial.com/](https://makefiletutorial.com/)

# Targets
- `target_name: dependencies`
- A target name can be a filename, multiple filenames (space separated), or just a word.
- Dependencies work the same way.
- The indented lines after a target are the commands to run.
```
	all: $(wildcard *.json)
	convert_json $@
```

- A target can use double colons `file::` to allow defining the target multiple times. Otherwise this is an error.

# Two types of variables
- Recursively-expanded variables are lazily evaluated, and reevaluated every time the variable is used. These use the `=` operator to assign. e.v. `VARNAME = value`
- Simply-expanded variables use `:=` to assign, and are just evaluated at the time of assignment. e.g. `VARNAME := VALUE`
- Variables are referenced using `$(VARNAME)` or `${VARNAME}`

# Automatic Variables
- Make provides a bunch of ways to make things easier when working with multiple targets at once.
- `$@` will reference the dependency that the target is running against, one dependency at a time.
- `$<` references the target for the current dependency (i.e. the output filename when running on filenames)
- `$?` is the list of all dependencies newer than the target.
- `$^` is the list of all prerequisites.

# Pattern matching
- The `%` wildcard operator is used to perform pattern match and replacement.
- `$(filenames): %.html: %.md` will match every `*.md` target and generate a matching `.html` target for that filename.
- Then later you can do `%.html:` with no dependencies to define the commands to run.
- You can also just do `%.html: %.md:` to set up a dependency from any `.md` to turn into a `.html` file.
- You can still override these targets with a target like `index.html:` if you want a different behavior for a particular file.
- But you still need a way to set up the main target, and so something like this usually works well:
	- ```
		SRC := $(wildcard *.pptx)
		OUT := $(SRC:.pptx=.json)
		all: $(OUT)

		%.json: %.pptx
			compile.sh $< > $@
		```
            
# Working with filenames
- `$(wildcard *.json)` will execute a file glob and expand to the matching filenames.

# String substitution
- `$(patsubst INPUT_PATTERN,OUTPUT_PATTERN,STRING_TO_MATCH_AND_REPLACE)`
- This has numerous shorthands
- `$(VARIABLE:%.o=%.c)` to change `.o` to `.c` for every file in `VARIABLE`.
- `$(VARIABLE:.o=.c)` also works when it's just a suffix.

# More
- When running at command, prefixing it with `@` will suppress printing the command output as it runs.
- Shell commands can be run using `VARNAME := $(shell command args)`
- If you need to recursively call `make`, use `$(MAKE)` instead since it will use all the same CLI flags passed to the current call.

Disable builtin rules with `.SUFFIXES:`
