;;; sage-shell-info.el --- functions related to Sage and info -*- lexical-binding: t -*-

;; Copyright (C) 2016 Sho Takemori.
;; Author: Sho Takemori <stakemorii@gmail.com>
;; URL: https://github.com/sagemath/sage-shell-mode

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code
(require 'sage-shell-mode)

(defsubst sage-shell--sage-info-p ()
  (and Info-current-file
       (string-match-p (concat "^" (regexp-quote (sage-shell:sage-root)))
                       Info-current-file)))

(defun sage-shell--info-matcher-keywords (lim)
  (when (sage-shell--sage-info-p)
    (re-search-forward sage-shell-help:fontlock-keyword-regexp lim t)))

(defun sage-shell--info-matcher-funcs (lim)
  (when (sage-shell--sage-info-p)
    (re-search-forward (rx (group
                            (or "Function" "Class" "Method" "Class Method"
                                "Static Method" "Attribute")
                            ":"))
                       lim t)))

(defun sage-shell-info-send-doctest ()
  (interactive)
  (when (sage-shell--sage-info-p)
    (sage-shell:send-doctest nil)))

(defun sage-shell-info-init ()
  (font-lock-add-keywords
   nil
   '((sage-shell--info-matcher-keywords 1 font-lock-keyword-face)
     (sage-shell--info-matcher-funcs 1 font-lock-function-name-face)))
  (use-local-map (copy-keymap Info-mode-map))
  (local-set-key (kbd "C-c C-d") 'sage-shell:send-doctest)
  (local-set-key (kbd "C-C C-z") 'sage-shell-edit:pop-to-process-buffer))

;;;###autoload
(defun sage-shell-info (&optional file-or-node)
  "Similar to M-x info, but highlights keywords and define some key-bindings."
  (interactive)
  (info file-or-node
        (if (numberp current-prefix-arg)
            (format "*SageInfo*<%s>" current-prefix-arg)
          "*SageInfo*"))
  (sage-shell-info-init))

(defun sage-shell--get-module-doc (mod buf)
  (sage-shell:send-command-to-string
   (format "%s(%s)"
           (sage-shell:py-mod-func "print_module_doc")
           mod)
   buf))

;; TODO: Treat wild card
(defun sage-shell--rst-extract-file (filename)
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (while (re-search-forward (rx bol ".. toctree::") nil t)
      (forward-line 1)
      (let ((end nil))
        (save-excursion
          (setq end
                (copy-marker
                 (if (re-search-forward (rx bol (or "." alnum))
                                        nil t)
                     (point)
                   (point-max))
                 t)))
        (while (re-search-forward
                (rx (1+ space)
                    (group (1+ (or "." "/" alnum "-" "_"))))
                end t)
          (let ((toc-file (match-string 1)))
            (unless (string-match-p (rx bol "sage/") toc-file)
              (let ((toc-file (expand-file-name
                               (concat toc-file ".rst")
                               (file-name-directory filename))))
                (when (file-exists-p toc-file)
                  (delete-region (line-beginning-position)
                                 (line-end-position))
                  (insert (sage-shell--rst-extract-file toc-file)))))))))
    (buffer-string)))

(defun sage-shell--parse-index-rst (file proc-buf)
  (with-temp-buffer
    (insert (sage-shell--rst-extract-file file))
    (goto-char (point-min))
    ;; extract automodule
    (save-excursion
      (while (re-search-forward (rx ".. automodule::"
                                    (1+ space)
                                    (group
                                     (and "sage" (1+ (or alnum "_" ".")))))
                                nil t)
        (let ((mod (match-string 1)))
          (delete-region (line-beginning-position)
                         (line-end-position))
          (insert (sage-shell--get-module-doc mod proc-buf)))))
    (buffer-string)))

(provide 'sage-shell-info)
;;; sage-shell-info.el ends here
