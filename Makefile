tars: .emacs.d.tar 
	# tar --exclude "*.tar" -cf $(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')-$(git log -n1 --format=format:"%h").tar .

.emacs.d.tar: $(shell find .emacs.d -type f)
	tar --exclude .emacs.d/elpa/archives --exclude "*.elc" -cf .emacs.d.tar .emacs.d/

phony: tars
