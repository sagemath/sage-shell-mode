;;; auto-complete-sage.el --- An auto-complete source for sage-shell.
;; Copyright (C) 2012-2014 Sho Takemori.

;; Author: Sho Takemori <stakemorii@gmail.com>
;; URL: https://github.com/stakemori/auto-complete-sage
;; Keywords: Sage, math, auto-complete
;; Created: 2012
;; Version: 0.0.1
;; Package-Requires: ((auto-complete "1.4.0") (sage-shell "0.0.1"))

;;; License
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Installation
;; 1. Install sage-shell.el. See the comment of sage-shell.el for the installation.
;; 2. Ensure that auto-complete.el is in your load-path.
;; 3. Put this file to your load-path and bytecompile it.
;; 4. Put the following lines to ~/.emacs.
;; (setq ac-modes (append '(sage-mode sage-shell-mode) ac-modes))
;; (add-hook 'sage-shell-mode-hook 'sage-shell-ac:add-sources)
;; (add-hook 'sage-mode-hook 'sage-edit-ac:add-sources)

;;; Code:
(require 'auto-complete)
(require 'sage-shell)
(eval-when-compile (require 'cl))


;;; sage-shell-ac
(add-to-list 'ac-modes 'sage-shell-mode)
(add-hook 'sage-shell-mode-hook 'sage-shell-ac:add-sources)

;;;###autoload
(defun sage-shell-ac:add-sources ()
  (add-to-list 'ac-sources 'ac-source-sage-shell)
  (add-to-list 'ac-sources 'ac-source-words-in-sage-buffers t))

(defvar sage-shell-ac:python-kwds
    '("abs" "all" "and" "any" "apply" "as" "assert" "basestring" "bin"
      "bool" "break" "buffer" "bytearray" "callable" "chr" "class"
      "classmethod" "cmp" "coerce" "compile" "complex" "continue" "def"
      "del" "delattr" "dict" "dir" "divmod" "elif" "else" "enumerate" "eval"
      "except" "exec" "execfile" "file" "filter" "finally" "float" "for"
      "format" "from" "frozenset" "getattr" "global" "globals" "hasattr"
      "hash" "help" "hex" "id" "if" "import" "import" "in" "input" "input"
      "int" "intern" "is" "isinstance" "issubclass" "iter" "lambda" "len"
      "list" "locals" "long" "map" "max" "memoryview" "min" "next" "not"
      "object" "oct" "open" "or" "ord" "pass" "pow" "print" "print"
      "property" "raise" "range" "raw" "reduce" "reload" "repr" "return"
      "reversed" "round" "set" "setattr" "slice" "sorted" "staticmethod"
      "str" "sum" "super" "try" "tuple" "type" "unichr" "unicode" "vars"
      "while" "with" "xrange" "yield" "zip" "__import__"))

(defun sage-shell-ac:init ()
  (when (sage-shell:output-finished-p)
    (sage-shell-cpl:completion-init
     (sage-shell-cpl:get 'interface)
     (sage-shell-cpl:get 'var-base-name)
     (equal this-command 'auto-complete))))

(defun sage-shell-ac:candidates ()
  (when (and (sage-shell:redirect-finished-p)
             (sage-shell:output-finished-p))
    (let* ((keywords (if (and (not (sage-shell-cpl:get 'var-base-name))
                              (equal (sage-shell-cpl:get 'interface) "sage"))
                         sage-shell-ac:python-kwds
                       nil)))
      (append keywords (sage-shell-cpl:candidates)))))

(defvar ac-source-sage-shell
  '((init . sage-shell-ac:init)
    (prefix . sage-shell-cpl:prefix)
    (candidates . sage-shell-ac:candidates)
    (cache)))


;; sage-edit-ac
(add-to-list 'ac-modes 'sage-mode)
(add-hook 'sage-mode-hook 'sage-edit-ac:add-sources)

;;;###autoload
(defun sage-edit-ac:add-sources ()
  (add-to-list 'ac-sources 'ac-source-sage-commands)
  (add-to-list 'ac-sources 'ac-source-words-in-sage-buffers t)
  (setq ac-sources
        (delete 'ac-source-words-in-same-mode-buffers ac-sources)))

(defvar sage-edit-ac:sage-commands nil)
(make-variable-buffer-local 'sage-edit-ac:sage-commands)

(defun sage-edit-ac:candidates ()
  (append
   (and sage-shell:process-buffer
        (and (sage-shell:redirect-finished-p)
             (sage-shell:output-finished-p))
        (or sage-edit-ac:sage-commands
            (setq sage-edit-ac:sage-commands
                  (sage:awhen (get-buffer sage-shell:process-buffer)
                    (with-current-buffer it
                      (or (sage-shell-cpl:get-cmd-lst "sage")
                          (sage-shell:update-sage-commands)))))))
   sage-shell-ac:python-kwds))

(defvar ac-source-sage-commands
  '((init . (lambda () (sage-edit:set-sage-proc-buf-internal nil nil)))
    (candidates . sage-edit-ac:candidates)
    (cache)))

(defun sage-edit-ac:words-in-sage-buffers ()
  (ac-word-candidates
   (lambda (buf)
     (sage:in (buffer-local-value 'major-mode buf) sage:sage-modes))))

(defvar ac-source-words-in-sage-buffers
  '((init . ac-update-word-index)
    (candidates . sage-edit-ac:words-in-sage-buffers)))

(provide 'auto-complete-sage)
;;; auto-complete-sage.el ends here
