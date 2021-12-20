tars: .emacs.d.tar

.emacs.d.tar: $(shell find .emacs.d -type f)
	tar --exclude .emacs.d/elpa/archives -cf .emacs.d.tar .emacs.d/

phony: tars
