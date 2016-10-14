;; Copyright (C) 2016 Sho Takemori.
;; Author: Sho Takemori <stakemorii@gmail.com>
;; URL: https://github.com/sagemath/sage-shell-mode

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

;;; Commentary:
;; Code for backward compatibility.
;;; Code
(require 'cl-lib)

(defvar sage-shell-compat-var-aliases
  '((sage-shell-sagetex:pop-to-error-buffer . sage-shell-sagetex-pop-to-error-buffer)
    (sage-shell:scroll-show-maximum-output . sage-shell-scroll-show-maximum-output)
    (sage-shell:scroll-to-the-bottom . sage-shell-scroll-to-the-bottom)
    (sage-shell-sagetex:pre-latex-command . sage-shell-sagetex-pre-latex-command)
    (sage-shell-sagetex:latex-command . sage-shell-sagetex-latex-command)
    (sage-shell-sagetex:add-to-texinputs-p . sage-shell-sagetex-add-to-texinputs-p)
    (sage-shell-sagetex:auctex-command-name . sage-shell-sagetex-auctex-command-name)
    (sage-shell:prefer-development-file-p . sage-shell-prefer-development-file-p)
    (sage-shell:input-history-cache-file . sage-shell-input-history-cache-file)
    (sage-shell:use-prompt-toolkit . sage-shell-use-prompt-toolkit)
    (sage-shell:make-error-link-p . sage-shell-make-error-link-p)
    (sage-shell:completion-ignore-case . sage-shell-completion-ignore-case)
    (sage-shell:sage-executable . sage-shell-sage-executable)
    (sage-shell:sage-root . sage-shell-sage-root)
    (sage-shell-edit:display-function . sage-shell-edit-display-function)
    (sage-shell:completion-function . sage-shell-completion-function)
    (sage-shell:inspect-ingnore-classes . sage-shell-inspect-ingnore-classes)
    (sage-shell:list-outputs-reversed-order-p . sage-shell-list-outputs-reversed-order-p)
    (sage-shell:completion-candidate-regexp . sage-shell-completion-candidate-regexp)
    (sage-shell-edit:temp-file-header . sage-shell-edit-temp-file-header)
    (sage-shell:check-ipython-version-on-startup . sage-shell-check-ipython-version-on-startup)
    (sage-shell:help-completion-function . sage-shell-help-completion-function)
    (sage-shell:command . sage-shell-command)
    (sage-shell-pdb:activate . sage-shell-pdb-activate)
    (sage-shell:list-outputs-max-line-num . sage-shell-list-outputs-max-line-num)
    (sage-shell:add-to-texinputs-p . sage-shell-add-to-texinputs-p)))

(defvar sage-shell-compat-func-aliases
  '((sage-shell:sagetex-load-file . sage-shell-sagetex-load-file)
    (sage-shell-sagetex:compile-file . sage-shell-sagetex-compile-file)
    (sage-shell:define-alias . sage-shell-define-alias)
    (sage-shell-edit:load-file . sage-shell-edit-load-file)
    (sage-shell:check-ipython-version . sage-shell-check-ipython-version)
    (sage-shell-edit:send-defun-and-go . sage-shell-edit-send-defun-and-go)
    (sage-shell-sagetex:compile-current-file . sage-shell-sagetex-compile-current-file)
    (sage-shell-edit:send-line . sage-shell-edit-send-line)
    (sage-shell-sagetex:load-file . sage-shell-sagetex-load-file)
    (sage-shell-edit:load-current-file-and-go . sage-shell-edit-load-current-file-and-go)
    (sage-shell-edit:send-region . sage-shell-edit-send-region)
    (sage-shell:ido-input-history . sage-shell-ido-input-history)
    (sage-shell:attach-file . sage-shell-attach-file)
    (sage-shell-pdb:input-next . sage-shell-pdb-input-next)
    (sage-shell:sage-mode . sage-shell-sage-mode)
    (sage-shell-pdb:input-continue . sage-shell-pdb-input-continue)
    (sage-shell-edit:send-buffer-and-go . sage-shell-edit-send-buffer-and-go)
    (sage-shell:set-process-buffer . sage-shell-set-process-buffer)
    (sage-shell-edit:load-current-file . sage-shell-edit-load-current-file)
    (sage-shell-pdb:input-step . sage-shell-pdb-input-step)
    (sage-shell-blocks:send-current . sage-shell-blocks-send-current)
    (sage-shell:run-sage . sage-shell-run-sage)
    (sage-shell-edit:load-file-and-go . sage-shell-edit-load-file-and-go)
    (sage-shell:restart-sage . sage-shell-restart-sage)
    (sage-shell-edit:send-defun . sage-shell-edit-send-defun)
    (sage-shell:copy-previous-output-to-kill-ring . sage-shell-copy-previous-output-to-kill-ring)
    (sage-shell:load-file . sage-shell-load-file)
    (sage-shell:clear-current-buffer . sage-shell-clear-current-buffer)
    (sage-shell:list-outputs-mode . sage-shell-list-outputs-mode)
    (sage-shell-sagetex:run-latex-and-load-file . sage-shell-sagetex-run-latex-and-load-file)
    (sage-shell-edit:send-line* . sage-shell-edit-send-line*)
    (sage-shell-help:forward-history . sage-shell-help-forward-history)
    (sage-shell:complete . sage-shell-complete)
    (sage-shell-pdb:set-break-point-at-point . sage-shell-pdb-set-break-point-at-point)
    (sage-shell-edit:send-line-and-go . sage-shell-edit-send-line-and-go)
    (sage-shell-help:send-current-line . sage-shell-help-send-current-line)
    (sage-shell-blocks:pull-next . sage-shell-blocks-pull-next)
    (sage-shell:sage-minor-mode . sage-shell-sage-minor-mode)
    (sage-shell-blocks:forward . sage-shell-blocks-forward)
    (sage-shell-pdb:input-quit . sage-shell-pdb-input-quit)
    (sage-shell:list-outputs . sage-shell-list-outputs)
    (sage-shell:run-new-sage . sage-shell-run-new-sage)
    (sage-shell:help-mode . sage-shell-help-mode)
    (sage-shell:delete-output . sage-shell-delete-output)
    (sage-shell-edit:attach-file . sage-shell-edit-attach-file)
    (sage-shell-help:backward-history . sage-shell-help-backward-history)
    (sage-shell:help . sage-shell-help)
    (sage-shell-pdb:input-help . sage-shell-pdb-input-help)
    (sage-shell-edit:send--buffer . sage-shell-edit-send--buffer)
    (sage-shell-pdb:input-down . sage-shell-pdb-input-down)
    (sage-shell:send-doctest . sage-shell-send-doctest)
    (sage-shell-edit:send-region-and-go . sage-shell-edit-send-region-and-go)
    (sage-shell:send-input . sage-shell-send-input)
    (sage-shell-sagetex:run-latex-and-load-current-file . sage-shell-sagetex-run-latex-and-load-current-file)
    (sage-shell-edit:pop-to-process-buffer . sage-shell-edit-pop-to-process-buffer)
    (sage-shell-pdb:input-where . sage-shell-pdb-input-where)
    (sage-shell:next-input . sage-shell-next-input)
    (sage-shell:delchar-or-maybe-eof . sage-shell-delchar-or-maybe-eof)
    (sage-shell:output-backward . sage-shell-output-backward)
    (sage-shell:interrupt-subjob . sage-shell-interrupt-subjob)
    (sage-shell-pdb:input-until . sage-shell-pdb-input-until)
    (sage-shell-pdb:input-up . sage-shell-pdb-input-up)
    (sage-shell:output-forward . sage-shell-output-forward)
    (sage-shell-edit:send-buffer . sage-shell-edit-send-buffer)
    (sage-shell:comint-send-input . sage-shell-comint-send-input)
    (sage-shell-sagetex:error-mode . sage-shell-sagetex-error-mode)
    (sage-shell-edit:send--buffer-and-go . sage-shell-edit-send--buffer-and-go)
    (sage-shell:send-eof . sage-shell-send-eof)
    (sage-shell-sagetex:load-current-file . sage-shell-sagetex-load-current-file)
    (sage-shell-pdb:input-run . sage-shell-pdb-input-run)
    (sage-shell-blocks:backward . sage-shell-blocks-backward)
    (sage-shell:send-all-doctests . sage-shell-send-all-doctests)))

(defun sage-shell-compat--define-aliases ()
  (let ((var "sage-shell-mode 0.3"))
    (cl-loop for (old . new) in sage-shell-compat-var-aliases
             do (define-obsolete-variable-alias old new var))
    (cl-loop for (old . new) in sage-shell-compat-func-aliases
             do (define-obsolete-function-alias old new var))))


;;;###autoload
(progn
  (define-obsolete-function-alias 'sage-shell:run-sage 'sage-shell-run-sage)
  (define-obsolete-function-alias 'sage-shell:run-new-sage 'sage-shell-run-new-sage)
  (define-obsolete-function-alias 'sage-shell:sage-mode 'sage-shell-sage-mode)
  (define-obsolete-variable-alias 'sage-shell:sage-mode-hook 'sage-shell-sage-mode-hook)
  (define-obsolete-variable-alias 'sage-shell:sage-mode-map 'sage-shell-sage-mode-map))

(provide 'sage-shell-compat)
