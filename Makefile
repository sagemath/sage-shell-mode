EMACS ?= emacs
CASK ?= $(HOME)/.cask/bin/cask

message:
	@echo "\nThis make file is provided for testing purposes only using Cask."
	@echo "To use sage-shell-mode from this clone, simply add the following"
	@echo "to your Emacs init file:"
	@echo "\n(add-to-list 'load-path \"$(PWD)\")\n(require 'sage-shell-mode)\n"

test-compile:
	EMACS=emacs $(CASK) install
	$(CASK) exec $(EMACS) -Q -eval "(progn (setq byte-compile-delete-errors nil) (setq byte-compile-error-on-warn t) (add-to-list 'load-path \"$(PWD)\"))" \
	-batch -f batch-byte-compile sage-shell-mode.el sage-shell-blocks.el sage-shell-view.el

test: clean test-compile
	$(CASK) exec $(EMACS) -Q -batch -L . -l test/sage-shell-mode-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc

.PHONY: message test-compile test clean
