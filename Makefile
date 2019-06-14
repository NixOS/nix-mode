.PHONY: test clean install run

ELS  =  nix.el nix-company.el nix-drv-mode.el nix-format.el \
	nix-instantiate.el nix-mode.el nix-mode-mmm.el \
	nix-prettify-mode.el nix-repl.el nix-search.el nix-shebang.el \
	nix-shell.el nix-store.el
ELCS = $(ELS:.el=.elc)

DOCS = nix-mode.info nix-mode.html # nix-mode.pdf

DESTDIR =
PREFIX  = /usr

all: $(ELCS) $(DOCS)

check:
	emacs   -batch -L . \
		-l tests/nix-mode-tests.el \
		-l tests/nix-font-lock-tests.el \
		-f ert-run-tests-batch-and-exit

install: $(ELCS) $(DOCS)
	mkdir -p $(DESTDIR)$(PREFIX)/share/emacs/site-lisp/nix-mode/
	cp $(ELCS) $(DESTDIR)$(PREFIX)/share/emacs/site-lisp/nix-mode/

	mkdir -p $(DESTDIR)$(PREFIX)/share/doc/nix-mode/
	cp $(DOCS) $(DESTDIR)$(PREFIX)/share/doc/nix-mode/

clean:
	rm -f $(ELCS) $(DOCS)

run:
	emacs -l nix-mode.el

%.elc: %.el
	emacs -batch -L . --eval "(progn\
	(when (file-exists-p \"$@\")\
	  (delete-file \"$@\"))\
	(fset 'message* (symbol-function 'message))\
	(fset 'message  (lambda (f &rest a)\
	                  (unless (equal f \"Wrote %s\")\
	                    (apply 'message* f a)))))" \
	-f batch-byte-compile $<

%.texi: %.org
	emacs   --batch \
		-l ox-extra -l org-man -l ox-texinfo $< \
		-f org-texinfo-export-to-texinfo

%.info: %.texi
	makeinfo --no-split $< -o $@

%.html: %.texi
	makeinfo --html --no-split $<

%.pdf: %.org
	emacs   --batch \
		-l ox-extra -l ox-latex $< \
		-f org-latex-export-to-pdf
