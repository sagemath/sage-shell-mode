;;; sage-shell-blocks.el --- Support for structuring Sage code in sheets

;; Copyright (C) 2013-2018 Johan Rosenkilde

;; Author: Johan Rosenkilde <jsrn@jsrn.dk>
;; URL: https://github.com/sagemath/sage-shell-mode
;; Keywords: Sage, math

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

;; This file adds functionality which supports structuring experimental Sage
;; code in "sheets", where the code is bundled in "blocks" akin to the boxes of
;; the Notebook. Functions are provided for convenient handling of such blocks.
;; The file injects a few keybindings into `sage-shell:sage-mode' as well as
;; `sage-shell-mode'.

;; A block is defined by a line beginning with `sage-shell-blocks:delimiter'.

;;; Code:
(require 'sage-shell-mode)
(defcustom sage-shell-blocks:delimiter "###"
  "Any line matching the regular expression `sage-shell-blocks:delimiter' at the
  beginning of the line is considered a start of a block.

Note that '^' to match at the beginning of the line should not be added to
`sage-shell-blocks:delimiter'.
Strange behaviour might arise if `sage-shell-blocks:delimiter' matches multiple lines
at a time."
  :type 'string
  :group 'sage-shell)

(defcustom sage-shell-blocks:title-decorate " ---- "
  "When printing titles of blocks, put this decoration around the
title for easy recognition"
  :type 'string
  :group 'sage-shell)

;;
;; Functionality for Sage source files
;;
;;;###autoload
(defun sage-shell-blocks:backward (arg)
  "Move backwards to the last beginning of a block."
  (interactive "p")
  (if (< arg 0)
      (sage-shell-blocks:forward (- arg))
    (while (and (> arg 0)
		(search-backward-regexp (concat "^" sage-shell-blocks:delimiter) nil 'move))
      (setq arg (- arg 1)))))

;;;###autoload
(defun sage-shell-blocks:forward (arg)
  "Move forwards to the next beginning of a block."
  (interactive "p")
  (if (< arg 0)
      (sage-shell-blocks:backward (- arg))
    ;; If point is on a delimiter, we should skip this, so search from beginning of
    ;; next line (this will match immediately, if next line is a delimiter)
    (let ((re  (concat "^" sage-shell-blocks:delimiter)))
      (when (looking-at re)
	(forward-line))
      ;; search forward: if it worked, move to begin of delimiter, otherwise end of file
      (while (and (> arg 0)
		  (search-forward-regexp re nil 'move))
	(setq arg (- arg 1)))
      ;; We successfully found something so move to the beginning of the match
      (when (= arg 0)
	(goto-char (match-beginning 0))))))

;;;###autoload
(defun sage-shell-blocks:send-current ()
  "Send the block that the point is currently in to the inferior shell.
Move to end of block sent."
  (interactive)
  ;; Border-case: if we're standing on a delimiter, sage-shell-blocks:backward will go
  ;; to previous delimiter, but we should send from this delimiter and forwards.
  (sage-shell-blocks:forward 1)
  (let* ((this-buf (current-buffer))
	 (enddelim (point))
	 (backdelim (save-excursion
		      (sage-shell-blocks:backward 1)
		      (point)))
	 title)
    ;; Copy the region to a temp buffer.
    ;; Possibly change the first line if it contains a title
    (with-temp-buffer
      (insert-buffer-substring this-buf backdelim enddelim)
      (goto-char (point-min))
      (when (looking-at sage-shell-blocks:delimiter)
	(progn
	  (goto-char (match-end 0))
	  (setq title (buffer-substring (point)
					(progn (end-of-line) (point))))
	  (when (string-match "^ *\\([^ ].*[^ ]\\) *$" title)
	    (setq title (match-string 1 title)))
	  (unless (equal title "")
	    (insert (concat "\nprint(\"" sage-shell-blocks:title-decorate title sage-shell-blocks:title-decorate "\")")))))
      (sage-shell-edit:send-region (point-min) (point-max)))))

;; Functionality for the inferior shell
;;
;;;###autoload
(defun sage-shell-blocks:pull-next ()
  "Evaluate the next block of the last visited file in Sage mode."
  (interactive)
  ;; Find the first buffer in buffer-list which is in sage-shell:sage-mode
  (let* ((lst (buffer-list))
	 (buf
	  (catch 'break
	    (while lst
	      (if (with-current-buffer (car lst) (derived-mode-p 'sage-shell:sage-mode))
		  (throw 'break (car lst))
		(setq lst (cdr lst)))))))
    (if buf
	(progn
	  (switch-to-buffer-other-window buf)
	  (sage-shell-blocks:send-current))
      (error "No sage-shell:sage-mode buffer found"))))

(provide 'sage-shell-blocks)

;;; sage-blocks.el ends here
