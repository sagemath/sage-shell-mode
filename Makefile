EMACS ?= emacs
CASK ?= $(HOME)/.cask/bin/cask

test-compile:
	$(CASK) exec $(EMACS) -Q -eval "(progn (setq byte-compile-delete-errors nil) (setq byte-compile-error-on-warn t))" \
	-batch -f batch-byte-compile sage-shell-mode.el

test: clean test-compile
	$(CASK) exec $(EMACS) -Q -batch -L . -l test/sage-shell-mode-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f sage-shell-mode.elc

.PHONY: test-compile test clean
