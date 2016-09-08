;;; sage-blocks.el --- Support for structuring Sage code in sheets

;; Copyright (C) 2013 Johan S. R. Nielsen

;; Author: Johan S. R. Nielsen <jsrn@jsrn.dk>
;; Keywords: sage

;;; Commentary:

;; This file adds functionality which supports structuring experimental Sage
;; code in "sheets", where the code is bundled in "blocks" akin to the boxes of
;; the Notebook. The core is concerned with convenient handling of such
;; blocks. The file injects a few keybindings into `sage-mode' as well as
;; `inferior-sage-mode'.

;; A block is defined by a line beginning with `sage-block-delimiter'.

;;; Code:
(defcustom sage-block-delimiter "###"
  "Any line matching the regular expression `sage-block-delimiter' at the
  beginning of the line is considered a start of a block.

Note that '^' to match at the beginning of the line should not be added to
`sage-block-delimiter'.
Strange behaviour might arise if `sage-block-delimiter' matches multiple lines
at a time."
  :type 'string
  :group 'sage)

(defcustom sage-block-title-decorate " ---- "
  "When printing titles of blocks, put this decoration around the
title for easy recognition"
  :type 'string
  :group 'sage)

;;
;; Functionality for Sage source files
;;
(defun sage-backward-block (arg)
  "Move backwards to the last beginning of a block."
  (interactive "p")
  (if (< arg 0)
      (sage-forward-block (- arg))
    (while (and (> arg 0)
		(search-backward-regexp (concat "^" sage-block-delimiter) nil 'move))
      (setq arg (- arg 1)))))

(defun sage-forward-block (arg)
  "Move forwards to the next beginning of a block."
  (interactive "p")
  (if (< arg 0)
      (sage-backward-block (- arg))
    ;; If point is on a delimiter, we should skip this, so search from beginning of
    ;; next line (this will match immediately, if next line is a delimiter)
    (let ((re  (concat "^" sage-block-delimiter)))
      (when (looking-at re)
	(forward-line))
      ;; search forward: if it worked, move to begin of delimiter, otherwise end of file
      (while (and (> arg 0)
		  (search-forward-regexp re nil 'move))
	(setq arg (- arg 1)))
      ;; We successfully found something so move to the beginning of the match
      (when (= arg 0)
	(goto-char (match-beginning 0))))))

(defun sage-send-current-block ()
  "Send the block that the point is currently in to the inferior shell.
Move to end of block sent."
  (interactive)
  ;; Border-case: if we're standing on a delimiter, sage-backward-block will go
  ;; to previous delimiter, but we should send from this delimiter and forwards.
  (sage-forward-block 1)
  (let* ((this-buf (current-buffer))
	 (enddelim (point))
	 (backdelim (save-excursion
		      (sage-backward-block 1)
		      (point)))
	 title)
    ;; Copy the region to a temp buffer.
    ;; Possibly change the first line if it contains a title
    (with-temp-buffer
      (insert-buffer-substring this-buf backdelim enddelim)
      (goto-char (point-min))
      (when (looking-at sage-block-delimiter)
	(progn
	  (goto-char (match-end 0))
	  (setq title (buffer-substring (point)
					(progn (end-of-line) (point))))
	  (when (string-match "^ *\\([^ ].*[^ ]\\) *$" title)
	    (setq title (match-string 1 title)))
	  (unless (equal title "")
	    (insert (concat "\nprint(\"" sage-block-title-decorate title sage-block-title-decorate "\")")))))
      (sage-send-region (point-min) (point-max)))))

(defun sage-blocks-default-keybindings ()
  "Bind default keys for working with sage blocks.

These are
  C-M-{      `sage-backward-block'
  C-M-}      `sage-forward-block'
  C-<return> `sage-send-current-block'

in `sage-mode' and in `inferior-sage-mode-map':

  C-<return> `sage-pull-next-block'"
  (define-key sage-mode-map (kbd "C-<return>") 'sage-send-current-block)
  (define-key sage-mode-map (kbd "C-M-{")        'sage-backward-block)
  (define-key sage-mode-map (kbd "C-M-}")        'sage-forward-block)
  (define-key inferior-sage-mode-map (kbd "C-<return>") 'sage-pull-next-block))

;;
;; Functionality for the inferior shell
;;
(defun sage-pull-next-block ()
  "Evaluate the next block of the last visited file in Sage mode."
  (interactive)
  ;; Find the first buffer in buffer-list which is in sage-mode
  (let* ((lst (buffer-list))
	 (buf
	  (catch 'break
	    (while lst
	      (if (with-current-buffer (car lst) (derived-mode-p 'sage-mode))
		  (throw 'break (car lst))
		(setq lst (cdr lst)))))))
    (if buf
	(progn
	  (switch-to-buffer-other-window buf)
	  (sage-send-current-block))
      (error "No sage-mode buffer found"))))

(provide 'sage-blocks)

;;; sage-blocks.el ends here
