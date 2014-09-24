EMACS ?= emacs
CASK ?= cask

all:
	${MAKE} clean
	${MAKE} test
	${MAKE} compile
	${MAKE} test
	${MAKE} clean

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile sage-shell-mode.el
test:
	$(MAKE) clean && ${CASK} exec ${EMACS} -Q -batch -L . -l test/sage-shell-mode-test.el -f ert-run-tests-batch-and-exit
clean:
	rm -f sage-shell-mode.elc

.PHONY: all compile test clean
