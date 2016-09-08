;;; sage-shell-blocks.el --- Support for structuring Sage code in sheets

;; Copyright (C) 2013-2016 Johan Rosenkilde

;; Author: Johan Rosenkilde <jsrn@jsrn.dk>
;; Keywords: sage

;;; Commentary:

;; This file adds functionality which supports structuring experimental Sage
;; code in "sheets", where the code is bundled in "blocks" akin to the boxes of
;; the Notebook. Functions are provided for convenient handling of such blocks.
;; The file injects a few keybindings into `sage-mode' as well as
;; `sage-shell-mode'.

;; A block is defined by a line beginning with `sage-shell:block-delimiter'.

;;; Code:
(defcustom sage-shell:block-delimiter "###"
  "Any line matching the regular expression `sage-shell:block-delimiter' at the
  beginning of the line is considered a start of a block.

Note that '^' to match at the beginning of the line should not be added to
`sage-shell:block-delimiter'.
Strange behaviour might arise if `sage-shell:block-delimiter' matches multiple lines
at a time."
  :type 'string
  :group 'sage)

(defcustom sage-shell:block-title-decorate " ---- "
  "When printing titles of blocks, put this decoration around the
title for easy recognition"
  :type 'string
  :group 'sage)

;;
;; Functionality for Sage source files
;;
(defun sage-shell:backward-block (arg)
  "Move backwards to the last beginning of a block."
  (interactive "p")
  (if (< arg 0)
      (sage-shell:forward-block (- arg))
    (while (and (> arg 0)
		(search-backward-regexp (concat "^" sage-shell:block-delimiter) nil 'move))
      (setq arg (- arg 1)))))

(defun sage-shell:forward-block (arg)
  "Move forwards to the next beginning of a block."
  (interactive "p")
  (if (< arg 0)
      (sage-shell:backward-block (- arg))
    ;; If point is on a delimiter, we should skip this, so search from beginning of
    ;; next line (this will match immediately, if next line is a delimiter)
    (let ((re  (concat "^" sage-shell:block-delimiter)))
      (when (looking-at re)
	(forward-line))
      ;; search forward: if it worked, move to begin of delimiter, otherwise end of file
      (while (and (> arg 0)
		  (search-forward-regexp re nil 'move))
	(setq arg (- arg 1)))
      ;; We successfully found something so move to the beginning of the match
      (when (= arg 0)
	(goto-char (match-beginning 0))))))

(defun sage-shell:send-current-block ()
  "Send the block that the point is currently in to the inferior shell.
Move to end of block sent."
  (interactive)
  ;; Border-case: if we're standing on a delimiter, sage-shell:backward-block will go
  ;; to previous delimiter, but we should send from this delimiter and forwards.
  (sage-shell:forward-block 1)
  (let* ((this-buf (current-buffer))
	 (enddelim (point))
	 (backdelim (save-excursion
		      (sage-shell:backward-block 1)
		      (point)))
	 title)
    ;; Copy the region to a temp buffer.
    ;; Possibly change the first line if it contains a title
    (with-temp-buffer
      (insert-buffer-substring this-buf backdelim enddelim)
      (goto-char (point-min))
      (when (looking-at sage-shell:block-delimiter)
	(progn
	  (goto-char (match-end 0))
	  (setq title (buffer-substring (point)
					(progn (end-of-line) (point))))
	  (when (string-match "^ *\\([^ ].*[^ ]\\) *$" title)
	    (setq title (match-string 1 title)))
	  (unless (equal title "")
	    (insert (concat "\nprint(\"" sage-shell:block-title-decorate title sage-shell:block-title-decorate "\")")))))
      (sage-shell:send-region (point-min) (point-max)))))

(defun sage-shell:blocks-default-keybindings ()
  "Bind default keys for working with Sage blocks.

The following are added to `sage-mode':
  C-M-{      `sage-shell:backward-block'
  C-M-}      `sage-shell:forward-block'
  C-<return> `sage-shell:send-current-block'

The following are added to `sage-shell-mode':
  C-<return> `sage-shell:pull-next-block'"
  (define-key sage-shell:mode-map (kbd "C-<return>") 'sage-shell:send-current-block)
  (define-key sage-shell:mode-map (kbd "C-M-{")        'sage-shell:backward-block)
  (define-key sage-shell:mode-map (kbd "C-M-}")        'sage-shell:forward-block)
  (define-key inferior-sage-shell:mode-map (kbd "C-<return>") 'sage-shell:pull-next-block))

;;
;; Functionality for the inferior shell
;;
(defun sage-shell:pull-next-block ()
  "Evaluate the next block of the last visited file in Sage mode."
  (interactive)
  ;; Find the first buffer in buffer-list which is in sage-shell-mode
  (let* ((lst (buffer-list))
	 (buf
	  (catch 'break
	    (while lst
	      (if (with-current-buffer (car lst) (derived-mode-p 'sage-shell-mode))
		  (throw 'break (car lst))
		(setq lst (cdr lst)))))))
    (if buf
	(progn
	  (switch-to-buffer-other-window buf)
	  (sage-shell:send-current-block))
      (error "No sage-shell-mode buffer found"))))

(provide 'sage-shell-blocks)

;;; sage-blocks.el ends here
