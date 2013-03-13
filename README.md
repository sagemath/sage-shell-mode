sage-shell-mode
===============

Emacs front end for Sage

# Overview
sage-shell-mode is an Emacs front end for [Sage](http://www.sagemath.org/).
sage-shell-mode provides sources of auto-complete or anything.

# Installation
To install sage-shell-mode, copy and paste the following code in *scratch* buffer,
evaluate it and follow the instruction.
You can evaluate the elisp code in *scratch* buffer by hitting C-j at the last closed parentheses.
By default, sage-shell-mode will be installed in "~/.emacs.d/sage-shell". If you want change
the installation directory, change the characters between double quotes at the third line
of the code.


```lisp
(progn
  (setq sage-install-url "https://raw.github.com/stakemori/sage-shell-mode/master/"
        sage-install-installation-directory "~/.emacs.d/sage-shell")
  (url-retrieve
   (concat sage-install-url "sage-install.el")
   (lambda (s)
     (goto-char (point-max))
     (eval-print-last-sexp))))
```
