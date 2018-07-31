emacs ?= emacs

.PHONY: test clean

ELS  =  nix.el nix-company.el nix-drv-mode.el nix-format.el \
	nix-instantiate.el nix-mode.el nix-mode-mmm.el \
	nix-prettify-mode.el nix-repl.el nix-search.el nix-shebang.el \
	nix-shell.el nix-store.el
ELCS = $(ELS:.el=.elc)

DOCS = nix-mode.info nix-mode.html nix-mode.pdf

all: $(ELCS) $(DOCS) test

test:
	emacs -Q -batch -l nix-mode.el \
		-l tests/nix-mode-tests.el \
		-l tests/nix-font-lock-tests.el \
		-f ert-run-tests-batch-and-exit

%.elc: %.el
	emacs -Q -batch -L . --eval "(progn\
	(when (file-exists-p \"$@\")\
	  (delete-file \"$@\"))\
	(fset 'message* (symbol-function 'message))\
	(fset 'message  (lambda (f &rest a)\
	                  (unless (equal f \"Wrote %s\")\
	                    (apply 'message* f a)))))" \
	-f batch-byte-compile $<

%.texi: %.org
	emacs   --batch -Q \
		-l ox-extra -l org-man -l ox-texinfo.el $< \
		-f org-texinfo-export-to-texinfo
	@echo >> $@

%.info: %.texi
	makeinfo --no-split $< -o $@

%.html: %.texi
	makeinfo --html --no-split $<

%.pdf: %.texi
	texi2pdf --clean $< > /dev/null

clean:
	rm *.elc
