EMACS ?= emacs
CASK ?= $(HOME)/.cask/bin/cask

compile:
	$(CASK) exec $(EMACS) -Q -batch -f batch-byte-compile sage-shell-mode.el

test: clean compile
	$(CASK) exec $(EMACS) -Q -batch -L . -l test/sage-shell-mode-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f sage-shell-mode.elc

.PHONY: compile test clean
