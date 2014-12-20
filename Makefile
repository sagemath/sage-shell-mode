EMACS = emacs
CASK = cask

compile:
	$(CASK) exec $(EMACS) -Q -batch -f batch-byte-compile sage-shell-mode.el

test: clean
	$(CASK) exec $(EMACS) -Q -batch -L . -l test/sage-shell-mode-test.el -f ert-run-tests-batch-and-exit
	$(MAKE) compile

clean:
	rm -f sage-shell-mode.elc

.PHONY: compile test clean
