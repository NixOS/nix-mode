.PHONY: test clean install run

ELS  =  nix.el nix-company.el nix-drv-mode.el nix-format.el \
	nix-instantiate.el nix-mode.el nix-mode-mmm.el \
	nix-prettify-mode.el nix-repl.el nix-search.el nix-shebang.el \
	nix-shell.el nix-store.el
ELCS = $(ELS:.el=.elc)

TESTS = tests/nix-mode-tests.el tests/nix-font-lock-tests.el

DESTDIR =
PREFIX  = /usr

all: $(ELCS) nix-mode.info nix-mode.html AUTHORS.md

check: $(TESTS) $(ELCS)
	emacs   -batch -L . \
		$(foreach test,$(TESTS),-l $(test)) \
		-f ert-run-tests-batch-and-exit

install: $(ELCS) nix-mode.info nix-mode.html AUTHORS.md
	mkdir -p $(DESTDIR)$(PREFIX)/share/emacs/site-lisp/nix-mode/
	cp $(ELCS) $(DESTDIR)$(PREFIX)/share/emacs/site-lisp/nix-mode/

	mkdir -p $(DESTDIR)$(PREFIX)/share/doc/nix-mode/
	cp nix-mode.html $(DESTDIR)$(PREFIX)/share/doc/nix-mode/

	mkdir -p $(DESTDIR)$(PREFIX)/share/info/
	cp nix-mode.info $(DESTDIR)$(PREFIX)/share/info/

	mkdir -p $(DESTDIR)$(PREFIX)/share/doc/
	test -f AUTHORS.md && cp AUTHORS.md $(DESTDIR)$(PREFIX)/share/doc/ || true

AUTHORS.md:
	@test -e .git \
	&& (printf "$$AUTHORS_HEADER\n" > $@ \
	&& git log --pretty=format:'- %aN <%aE>' | sort -u >> $@ \
	&& printf "done\n" ; ) \
	|| printf "FAILED (non-fatal)\n"

clean:
	rm -f $(ELCS) $(DOCS)

run:
	emacs -Q -L . -l nix-mode.el

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
		-l ox-extra -l ox-texinfo $< \
		-f org-texinfo-export-to-texinfo

%.info: %.texi
	makeinfo --no-split $< -o $@

%.html: %.texi
	makeinfo --html --no-split $<

%.pdf: %.org
	emacs   --batch \
		-l ox-extra -l ox-latex $< \
		-f org-latex-export-to-pdf
