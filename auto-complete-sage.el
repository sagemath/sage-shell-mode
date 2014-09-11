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

(defcustom sage-shell-ac:use-quick-help nil
  "If non nil, use quick help in Sage process buffers."
  :group 'sage-shell
  :type 'boolean)

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
    (document . sage-shell-ac:ac-doc)
    (cache)))

(defun sage-shell-ac:parse-argspec (doc)
  (let ((case-fold-search nil))
    (with-temp-buffer
      (insert doc)
      (goto-char (point-min))
      (cond ((re-search-forward "Definition:" nil t)
             (skip-chars-forward " \t")
             (buffer-substring (point) (line-end-position)))
            (t "")))))

(defun sage-shell-ac:popup-help (async)
  "When ASYNC is non nil, then this returns nil.  This adds the
document to `sage-shell-ac:doc-arg-alist' and shows popup help by
`sage:ac-pos-tip-show-quick-help'. When ASYNC is nil, this
returns a document help as a string."
  (when ac-menu
    (let* ((item (popup-selected-item ac-menu))
           (objname-to-send (sage-shell-cpl:to-objname-to-send item))
           (aspec nil)
           (doc-maybe nil))
      (unless (sage:in objname-to-send sage-shell-ac:python-kwds)
        (sage:aif (assoc objname-to-send sage-shell-ac:doc-arg-alist)
            (setq aspec (cadr it) doc-maybe (cddr it))
          (setq doc-maybe (sage-shell:send-magic-cmd-base
                           "pinfo" objname-to-send async)))
        (cond
         (async (lexical-let ((objname-to-send objname-to-send)
                              (aspec aspec) (doc-maybe doc-maybe)
                              (item item))
                  (sage-shell:after-redirect-finished
                    (when (and ac-menu
                               (equal (popup-selected-item ac-menu) item))
                      (setq doc-maybe (sage:get-value doc-maybe)
                            aspec (sage-shell-ac:parse-argspec doc-maybe))
                      (add-to-list 'sage-shell-ac:doc-arg-alist
                                   (cons objname-to-send (cons aspec doc-maybe)))
                      (sage-shell-ac:quick-help item aspec doc-maybe))
                    nil)))
         (t (add-to-list 'sage-shell-ac:doc-arg-alist
                         (cons objname-to-send (cons aspec doc-maybe)))
            (setq doc-maybe (sage:get-value doc-maybe)
                  aspec (sage-shell-ac:parse-argspec doc-maybe))
            (sage-shell-ac:doc-argspec-transformer item aspec doc-maybe)))))))

(defun sage-shell-ac:quick-help (item aspec doc-maybe)
  (when (and (null this-command)
             (ac-menu-live-p)
             (null ac-quick-help)
             (cond ((fboundp 'ac-quick-help-use-pos-tip-p)
                    (funcall 'ac-quick-help-use-pos-tip-p))
                   (t (and ac-quick-help-prefer-pos-tip
                           window-system
                           (featurep 'pos-tip)))))
    (setq ac-quick-help
          (funcall
           'sage-shell-ac:ac-pos-tip-show-quick-help
           ac-menu
           (sage-shell-ac:doc-argspec-transformer
            item aspec doc-maybe)
           nil
           :point ac-point
           :height ac-quick-help-height
           :nowait t))))

(defun sage-shell-ac:ac-doc (can)
  (when (sage-shell:at-top-level-p)
    (cond ((eq last-command 'ac-help)
           (sage-shell-ac:popup-help nil))
          (sage-shell-ac:use-quick-help
           (sage-shell-ac:popup-help t)))))

(defadvice ac-help (around sage-quick-help)
  (if (eq major-mode 'sage-shell-mode)
      (sage-shell-ac:ac-help)
    ad-do-it))
(ad-enable-advice 'ac-help 'around 'sage-quick-help)
(ad-activate 'ac-help)

(defun sage-shell-ac:ac-help ()
  (interactive)
  (when ac-menu
    (sage-shell-ac:popup-menu-show-help ac-menu)))

(defun sage-shell-ac:doc-argspec-transformer (item raw-argspec raw-doc)
  (format "%s\n\n%s"
          (sage-shell-ac:argspec-trans item raw-argspec)
          (sage-shell-ac:doc-trans raw-doc)))

(defun sage-shell-ac:argspec-trans (item raw-argspec)
  (let* ((var-base-name (sage-shell-cpl:get 'var-base-name))
         (name (cond (var-base-name (concat var-base-name "." item))
                     (t item))))
    (cond
     ;; if not callable
     ((not (string-match "(" raw-argspec)) name)

     (t (let* ((str (progn
                      (string-match (rx "(" (group (0+ ascii)) ")")
                                    raw-argspec)
                      (match-string 1 raw-argspec)))
               (vars (split-string str "[ ,]+"))
               (vars1
                (loop for var in vars
                      if (or (string-match  "\\<names\\>" var)
                             (not (string-match
                                   (rx (or "=" "self")) var)))
                      collect var))
               (dot (cond ((or (equal vars vars1)
                               (and (sage:in "self" vars)
                                    (equal (length vars)
                                           (1+ (length vars1)))))

                           "")
                          ((null vars1) "...")
                          (t ", ..."))))
          (format "%s(%s%s)" name (mapconcat 'identity vars1 ", ")
                  dot))))))

(defun sage-shell-ac:doc-trans (raw-doc)
  (let ((func (sage-shell-interfaces:get
               (sage-shell-cpl:get 'interface)
               'doc-trans)))
    (when (functionp func)
      (funcall func raw-doc))))

(defun sage-shell-ac:doc-trans-generic (raw-doc)
  (let ((case-fold-search nil))
    (with-temp-buffer
      (insert raw-doc)
      (goto-char (point-min))
      (let ((start (re-search-forward "Docstring:" nil t))
            (end (or (re-search-forward (rx (or "INPUT"
                                                "OUTPUT"
                                                "TESTS"
                                                "Constructor Docstring"
                                                "EXAMPLES"
                                                "EXAMPLE"
                                                "AUTHORS"
                                                "Call def"
                                                "Calling Docstring") ":")
                                        nil t)
                     (point-max))))
        (cond (start
               (narrow-to-region start end)
               (goto-char (point-max))
               (delete-region (line-beginning-position)
                              (line-end-position))
               (delete-blank-lines)
               (goto-char (point-min))
               (delete-blank-lines)
               (buffer-string))
              (t ""))))))



;; copied from popup.el
(defun sage-shell-ac:popup-menu-show-help (menu &optional persist item)
  (sage-shell-ac:popup-item-show-help (or item (popup-selected-item menu))
                                      persist))

(defun sage-shell-ac:popup-item-show-help (item &optional persist)
  (when item
    (if (not persist)
        (save-window-excursion
          (when (sage-shell-ac:popup-item-show-help-1 item)
            (block nil
              (while t
                (clear-this-command-keys)
                (let ((key (read-key-sequence-vector nil)))
                  (case (key-binding key)
                    ('scroll-other-window
                     (scroll-other-window))
                    ('scroll-other-window-down
                     (scroll-other-window-down nil))
                    (t
                     (setq unread-command-events (append key unread-command-events))
                     (return))))))))
      (sage-shell-ac:popup-item-show-help-1 item))))

(defun sage-shell-ac:popup-item-show-help-1 (item)
  (let ((doc (sage-shell-ac:popup-help nil)))
    (when doc
      (with-current-buffer (get-buffer-create " *Popup Help*")
        (erase-buffer)
        (insert doc)
        (goto-char (point-min))
        (display-buffer (current-buffer)))
      t)))

;; copied from auto-complete.el
(defun sage-shell-ac:ac-pos-tip-show-quick-help
  (menu doc &optional item &rest args)
  (let* ((point (plist-get args :point))
         (around nil)
         (parent-offset (popup-offset menu)))
    (when (stringp doc)
      (if (popup-hidden-p menu)
          (setq around t)
        (setq point nil))
      (with-no-warnings
        (pos-tip-show doc
                      'popup-tip-face
                      (or point
                          (and menu
                               (popup-child-point menu parent-offset))
                          (point))
                      nil 0
                      popup-tip-max-width
                      nil nil
                      (and (not around) 0))
        (unless (plist-get args :nowait)
          (clear-this-command-keys)
          (unwind-protect
              (push (read-event (plist-get args :prompt)) unread-command-events)
            (pos-tip-hide))
          t)))))


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
