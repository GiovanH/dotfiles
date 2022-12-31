.SUFFIXES:

dotfiles = ~/dotfiles
footer = END PUBLIC FILE
privtag = PRIV
allfiles = \
	$(addprefix home/,.bash_personal .bash_colors .bash_completion .bash_profile .bashrc .gitconfig .gitignore .vimrc .fonts.conf .profile_CYGWIN_NT) \
	$(addprefix scripts/,jqt shlex j2) \
	$(addprefix .emacs.d/,init.el init-extra.el ev-init.el) \
	.githooks/prepare-commit-msg

.PHONY: all
all: allfiles

.PHONY: allfiles
allfiles: $(allfiles) Makefile

define compile_cmd
	@mkdir -p "`dirname $@`"
	@grep -E '$(ICOM) ?$(footer)' $< > /dev/null # ensure footer exists
	sed -n -r '/$(ICOM) $(footer)/q;p' $< | perl -pe "s/(\n|^ *)(.+?)$(ICOM) ?$(privtag)//g" > $@
endef

define copy_cmd
	@mkdir -p "`dirname $@`"
	cp $< $@
endef

home/.fonts.conf :: $(dotfiles)/home/.fonts.conf
	$(call copy_cmd)
home/.vimrc :: $(dotfiles)/home/.vimrc
	$(call copy_cmd)

.emacs.d/%.el : ICOM = \\\;
.emacs.d/%.el :: $(dotfiles)/.emacs.d/%.el
	$(call compile_cmd,ICOM)

.emacs.d/%.sh: ICOM = \#
.emacs.d/%.sh :: $(dotfiles)/.emacs.d/%.sh
	$(call compile_cmd,ICOM)

.emacs.d/%.patch :: $(dotfiles)/.emacs.d/%.patch
	$(call copy_cmd)

home/.% : ICOM = \#
home/.% :: $(dotfiles)/home/.%
	$(call compile_cmd,ICOM)

local/.%: ICOM = \#
local/.% :: $(dotfiles)/local/.%
	$(call compile_cmd,ICOM)

.githooks/%: $(dotfiles)/.githooks/%
	$(call copy_cmd)

scripts/% :: $(dotfiles)/scripts/%
	$(call copy_cmd)

.ssh/config.j2: ICOM = \#
.ssh/config.j2 :: ~/.ssh/config.j2
	$(call compile_cmd,ICOM)

.config/gtk-3.0/settings.ini :: ~/.config/gtk-3.0/settings.ini
	$(call copy_cmd)

.chef/knife.rb :: ~/.chef/knife.rb
	$(call copy_cmd)

.PHONY: clean
clean:
	-rm -r $(allfiles) out.html
