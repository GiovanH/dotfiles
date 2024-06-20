.SUFFIXES:

dotfiles = ~/dotfiles
footer = END PUBLIC FILE
privtag = PRIV
allfiles = \
	$(addprefix home/,.bash_personal .bash_profile .bashrc .gitconfig \
	            .gitignore .vimrc .fonts.conf .minttyrc .XCompose .tmux.conf) \
	$(addprefix local/,.bashrc_Cygwin .bashrc_GNU_Linux) \
	$(addprefix reg/,bog.runasinvoker.reg gio.makebak.reg gio.toggledisabled.reg) \
	$(addprefix scripts/,jqt shlex j2 servicewatch.sh) \
	$(addprefix support/,gum_polyfill.sh .bash_colors .bash_completion logging.sh hyperloop.sh certutils.sh) \
	$(addprefix .emacs.d/,init.el init-extra.el) \
	$(addprefix .config/,ruff.toml)

# .githooks/prepare-commit-msg

.PHONY: all
all: allfiles

.PHONY: allfiles
allfiles: $(allfiles) Makefile

.SECONDEXPANSION:

.SUFFIXES:

.PHONY: check
check:
	@-diff -r ./home/ ../home/ -q
	@-diff -r ./local/ ../local/ -q
	@-diff -r ./reg/ ../reg/ -q
	@-diff -r ./scripts/ ../scripts/ -q
	@-diff -r ./support/ ../support/ -q
	@-diff -r ./.emacs.d/ ../.emacs.d/ -q

define compile_cmd
	@mkdir -p "`dirname $@`"
	@grep -E '$(ICOM) ?$(footer)' $< > /dev/null # ensure footer exists
	sed -n -r '/$(ICOM) $(footer)/q;p' $< | perl -pe "s/(\n|^ *)(.+?)$(ICOM) ?$(privtag)//g" > $@
endef

define copy_cmd
	@mkdir -p "`dirname $@`"
	cp $< $@
endef

home/.%: ICOM = \#
home/.% :: $(dotfiles)/home/.%
	$(call compile_cmd,ICOM)

% :: $(dotfiles)/$$@
	$(call copy_cmd)

home/.vimrc :: $(dotfiles)/home/.vimrc
	$(call copy_cmd)

home/.minttyrc :: $(dotfiles)/home/.minttyrc
	$(call copy_cmd)

home/.XCompose :: $(dotfiles)/home/.XCompose
	$(call copy_cmd)

home/%.conf :: $(dotfiles)/home/%.conf
	$(call copy_cmd)

reg/%.reg :: $(dotfiles)/reg/%.reg
	$(call copy_cmd)

.emacs.d/%.el: ICOM = \;
.emacs.d/%.el :: $(dotfiles)/.emacs.d/%.el
	$(call compile_cmd,ICOM)

.emacs.d/%.sh: ICOM = \#
.emacs.d/%.sh :: $(dotfiles)/.emacs.d/%.sh
	$(call compile_cmd,ICOM)

.emacs.d/%.patch :: $(dotfiles)/.emacs.d/%.patch
	$(call copy_cmd)

local/%: ICOM = \#
local/% :: $(dotfiles)/local/%
	$(call compile_cmd,ICOM)

.githooks/%: $(dotfiles)/.githooks/%
	$(call copy_cmd)

scripts/% :: $(dotfiles)/scripts/%
	$(call copy_cmd)

support/% :: $(dotfiles)/support/%
	$(call copy_cmd)

.ssh/config.j2: ICOM = \#
.ssh/config.j2 :: ~/.ssh/config.j2
	$(call compile_cmd,ICOM)

.config/gtk-3.0/settings.ini :: ~/.config/gtk-3.0/settings.ini
	$(call copy_cmd)

.chef/knife.rb :: ~/.chef/knife.rb
	$(call copy_cmd)

compareroots = config home reg scripts local service support .config
tell_me_what_all_cool_stuff_isnt_in_the_makefile_yet:
	mkdir -p $(compareroots)
	bash -c 'diff <(find $(compareroots) | sort -u) <(cd ../; find $(compareroots) | sort -u)'

.PHONY: clean
clean:
	-rm -r $(allfiles)
