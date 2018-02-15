;;; sage-shell-mode.el --- A front-end for Sage Math -*- lexical-binding: t -*-

;; Copyright (C) 2012 - 2018 Sho Takemori.
;; Author: Sho Takemori <stakemorii@gmail.com>
;; URL: https://github.com/sagemath/sage-shell-mode
;; Package-Requires: ((cl-lib "0.6.1") (emacs "24.4") (let-alist "1.0.5") (deferred "0.5.1"))
;; Keywords: Sage, math
;; Version: 0.3

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

;; This package provides a front end for Sage (http://www.sagemath.org/)
;; and a major mode derived from python-mode (sage-shell:sage-mode).

;; To use this package, check the return value of (executable-find "sage").
;; If (executable-find "sage") is a string, you are ready to use this package.
;; If not, put the following line to ~/.emacs.d/init.el
;; (setq sage-shell:sage-root "/path/to/sage/root_directory")
;; And replace /path/to/sage/root_directory to the path to $SAGE_ROOT.

;; Then you can run Sage process in Emacs by M-x sage-shell:run-sage.
;; You can run multiple Sage processes by M-x sage-shell:run-new-sage.
;; By putting the following line to ~/.emacs.d/init.el,
;; (sage-shell:define-alias)
;; you can run Sage by M-x run-sage instead of M-x sage-shell:run-sage.

;; Please visit https://github.com/sagemath/sage-shell-mode for more
;; infomation.

;;; Code:
;; TODO
;; 1. Disabel auto indent (cf. IPython's issue #9888).
;; 2. Fix sage-shell-edit:exec-command-base when the line is not empty.
;; 3. Add support for simple prompt.
;; 4. Fix sage-shell-edit:exec-command-base when insert-command-p is non-nil.

;; Requireing cl-lib when compile time is necessary in Emacs 24.1 and 24.2
(require 'md5)
(eval-and-compile (require 'cl-lib)
                  (require 'let-alist))
(require 'deferred)
(require 'pcomplete)
(require 'eldoc)
(require 'info)

;;; Global variables for users
(defgroup sage-shell
  nil "Run Sage process in a buffer."
  :group 'languages)

(defgroup sage-shell-sagetex
  nil "Group for SageTeX."
  :group 'sage-shell)

(defcustom sage-shell:sage-root nil
  "SAGE_ROOT directory. If the Sage executable in your PATH
  and (exeutable-find \"sage\") is non-nil, then you do not have
  to set this variable."
  :group 'sage-shell
  :type '(choice (directory :tag "Directory")
                 (const :tag "Not specified" nil)))

(defcustom sage-shell:sage-executable nil
  "Name of the Sage executable. If the Sage executable in your
  PATH and (exeutable-find \"sage\") is non-nil, then you do not
  have to set this variable."
  :group 'sage-shell
  :type '(choice (string :tag "Executable file of Sage")
                 (const :tag "Not specified" nil)))

;;;###autoload
(defvaralias 'sage-shell:command 'sage-shell:sage-executable)

(defcustom sage-shell:input-history-cache-file
  nil
  "If non nil, then `comint-input-ring' is saved to this file when the Sage process exits."
  :group 'sage-shell
  :type '(choice (file :tag "file")
                 (const :tag "Off" nil)))

(defcustom sage-shell:completion-function 'completion-at-point
  "Function used for `sage-shell:complete'."
  :group 'sage-shell
  :type '(choice (const :tag "default" completion-at-point)
                 (const :tag "pcomplete" pcomplete)
                 (const :tag "auto-complete" auto-complete)
                 (const :tag "anything" anything-sage-shell)
                 (const :tag "helm" helm-sage-shell)))

(defcustom sage-shell:help-completion-function 'sage-shell:help1
  "Completion function used for `sage-shell:help'."
  :group 'sage-shell
  :type '(choice
          (const :tag "default" sage-shell:help1)
          (const :tag "anything" anything-sage-shell-describe-object-at-point)
          (const :tag "helm" helm-sage-shell-describe-object-at-point)))


(defcustom sage-shell:use-unicode-banner t
  "Non-nil means use unicode character in Sage's banner."
  :type 'boolean
  :group 'sage-shell)

(defcustom sage-shell:completion-ignore-case nil
  "Non-nil means don't consider case significant in completion."
  :type 'boolean
  :group 'sage-shell)

(defcustom sage-shell:completion-candidate-regexp (rx (1+ (or alnum "_")))
  "Regexp used for collect completions when completion-at-point is called."
  :type 'regexp
  :group 'sage-shell)

(defcustom sage-shell:make-error-link-p t
  "If non-nil and the output contains an error line,
output-filter-function creates a link to the file where the error
is raised."
  :type 'boolean
  :group 'sage-shell)

(defcustom sage-shell:prefer-development-file-p t
  "If non nil, prefer a source file in src directory rather than
site-packages directory."
  :group 'sage-shell
  :type 'boolean)

(defcustom sage-shell-pdb:activate t
  "Non-nil makes  Sage shell enable pdbtracking."
  :type 'boolean
  :group 'sage-shell)

;;; Borrowed from esh-mode.el (eshell).
(defcustom sage-shell:scroll-show-maximum-output t
  "Controls how interpreter output causes window to scroll.
If non-nil, then show the maximum output when the window is scrolled."
  :type 'boolean
  :group 'sage-shell)

(defcustom sage-shell:scroll-to-the-bottom nil
  "Non nil means scrolling to the bottom when sending the input."
  :type 'boolean
  :group 'sage-shell)

(defcustom sage-shell:list-outputs-max-line-num 5
  "Max number of lines of the outputs displayed in the buffer created
by `sage-shell:list-outputs' and other related commands.
Nil means it does not truncate the outputs."
  :type 'integer
  :group 'sage-shell)

(defcustom sage-shell:list-outputs-reversed-order-p t
  "Non nil means outputs ordered by the reversed order."
  :type 'boolean
  :group 'sage-shell)

(defcustom sage-shell-edit:display-function nil
  "This variable handles how the process buffer will be
displayed. If non-nil, this function will be called after sending
the contents of a buffer, a region or a file to the Sage
process."
  :type '(choice (const :tag "default" nil)
                 (const :tag "display-buffer" 'display-buffer)
                 (const :tag "pop-to-buffer" 'pop-to-buffer))
  :group 'sage-shell)

(defcustom sage-shell:inspect-ingnore-classes nil
  "If non-nil, this should be a list of strings.
Each string shoud be a class of Sage. When non-nil instances or methods
of these classes are ignored by `ac-quick-help' and `eldoc'.
If the value is equal to '(\"\"), then it does not ignore anything."
  :group 'sage-shell
  :type '(repeat string))

(defcustom sage-shell-edit:temp-file-header "# -*- coding: utf-8 -*-\n"
  "`sage-shell-edit:send-region', `sage-shell-edit:send-buffer' and related commands use a temporary file.
This string will be inserted to the temporary file before evaluating code."
  :type 'string
  :group 'sage-shell)


(defcustom sage-shell:use-prompt-toolkit t
  "Non `nil' means the Sage process uses the new prompt of IPython 5."
  :type 'boolean
  :group 'sage-shell)
;; (make-variable-buffer-local 'sage-shell:use-prompt-toolkit)

(defcustom sage-shell:check-ipython-version-on-startup nil
  "Non `nil' means check if `sage-shell:use-prompt-toolkit' is correctly set when starting the Sage process.
The checking is done asyncally."
  :type 'boolean
  :group 'sage-shell)

(defcustom sage-shell-sagetex:pre-latex-command
  "latex -interaction=nonstopmode"
  "This LaTeX command will be called by
`sage-shell-sagetex:compile-file' before loading a .sagetex.sage
file."
  :group 'sage-shell-sagetex
  :type 'string)

(defcustom sage-shell-sagetex:latex-command "latex -interaction=nonstopmode"
  "If `sage-shell-sagetex:auctex-command-name' is nil (by default
it is nil), then this variable is used for
`sage-shell-sagetex:compile-file' after loading a .sagetex.sage
file. It should be a LaTeX command without a file
name. `sage-shell-sagetex:compile-file' will call the LaTeX
command after loading a .sagetex.sage file."
  :group 'sage-shell-sagetex
  :type 'string)

(defcustom sage-shell-sagetex:auctex-command-name nil
  "This variable is for AUCTeX users. If non-nil, it should be a
name of an element of `TeX-command-list', i.e. the first element
of an element of the list. Then the corresponding LaTeX command
will be called after loading a .sagetex.sage file by
`sage-shell-sagetex:compile-file'. If this value is
non-nil, then the value of `sage-shell-sagetex:latex-command'
will be ignored."
  :group 'sage-shell-sagetex
  :type '(choice (const :tag "Not Specified" nil)
                 (string :tag "LaTeX command")))

(defcustom sage-shell-sagetex:add-to-texinputs-p t
  "Non-nil means sage-shell-mode adds
$SAGE_ROOT/local/share/texmf/tex/generic/sagetex/ to TEXINPUTS."
  :type 'boolean
  :group 'sage-shell-sagetex)

;;;###autoload
(defvaralias 'sage-shell:add-to-texinputs-p
  'sage-shell-sagetex:add-to-texinputs-p)

(defcustom sage-shell-sagetex:pop-to-error-buffer t
  "Non-nil means pop to the SageTeX error buffer."
  :type 'boolean
  :group 'sage-shell-sagetex)


(eval-and-compile
  (cond ((fboundp 'setq-local)
         (defmacro sage-shell:setq-local (var val)
           (list 'setq-local var val)))
        (t (defmacro sage-shell:setq-local (var val)
             "Set variable VAR to value VAL in current buffer."
             ;; Can't use backquote here, it's too early in the bootstrap.
             (list 'set (list 'make-local-variable (list 'quote var)) val)))))

;;; Anaphoric macros
(defmacro sage-shell:aand (&rest args)
  "`it' is binded to the last evaluated argument"
  (declare (indent 0) (debug t))
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(sage-shell:aif ,(car args) (sage-shell:aand ,@(cdr args))))))

(defmacro sage-shell:aif (test-form then-form &rest else-forms)
  " Temporary variable `it' is the result of test-form."
  (declare (indent 2) (debug t))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defmacro sage-shell:awhen (test-form  &rest then-forms)
  " Temporary variable `it' is the result of test-form."
  (declare (indent 1) (debug t))
  `(let ((it ,test-form))
     (when it ,@then-forms)))

;; utilities
(eval-when-compile
  (defvar sage-shell:gensym-counter 0))
(defvar sage-shell:gensym-counter 0)

(defsubst sage-shell:gensym (&optional prefix)
  "Generate a new uninterned symbol.
The name is made by appending a number to PREFIX, default
\"sage-shell-gensym-\"."
  (if (fboundp 'cl-gensym)
      (cl-gensym prefix)
    (let ((pfix (if (stringp prefix) prefix "sage-shell-gensym-"))
          (num (if (integerp prefix) prefix
                 (prog1 sage-shell:gensym-counter
                   (cl-incf sage-shell:gensym-counter)))))
      (make-symbol (format "%s%d" pfix num)))))

(defsubst sage-shell:in (elt seq)
  (if (member elt seq) elt))

(defun sage-shell:remove-if (pred seq)
  (if (fboundp 'cl-remove-if)
      (cl-remove-if pred seq)
    (cl-loop for a in seq
             unless (funcall pred a)
             collect a)))

(defsubst sage-shell:nthcar-and-rest (m l)
  (cl-loop for i from 0 to (1- m)
           for a on l
           collect (car a) into x
           finally return (cons x a)))

(defsubst sage-shell:group (l &optional n)
  (let ((r l)
        (a nil)
        (n (or n 2))
        (res nil))
    (while r
      (setq a (sage-shell:nthcar-and-rest n r)
            r (cdr a))
      (push (car a) res))
    (nreverse res)))

(defmacro sage-shell:if-let* (varlist test-form then-form &rest else-forms)
  "VARLIST is like varlist of let*."
  (declare (indent 3) (debug t))
  `(let* ,varlist
     (if ,test-form
         ,then-form
       ,@else-forms)))

(defmacro sage-shell:when-let* (varlist test-form &rest then-forms)
  "VARLIST is like varlist of let*."
  (declare (indent 2) (debug t))
  `(let* ,varlist
     (when ,test-form
       ,@then-forms)))

(defmacro sage-shell:unless-let* (varlist test-form &rest then-forms)
  "VARLIST is like varlist of let*."
  (declare (indent 2) (debug t))
  `(let* ,varlist
     (unless ,test-form
       ,@then-forms)))

(defmacro sage-shell:acond (&rest clauses)
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
          (sym (sage-shell:gensym "sage-shell")))
      `(let ((,sym ,(car cl1)))
         (if ,sym
             (let ((it ,sym)) ,@(cdr cl1))
           (sage-shell:acond ,@(cdr clauses)))))))

(cl-defmacro sage-shell:with-gensym ((&rest names) &rest body)
  (declare (indent 1) (debug t))
  `(let ,(cl-loop for n in names collect `(,n (sage-shell:gensym "sage-shell")))
     ,@body))

(defmacro sage-shell:define-keys (keymap &rest defs)
  (declare (indent 1))
  (append (list 'progn)
          (cl-loop for i from 0 to (1- (/ (length defs) 2))
                   collect
                   `(define-key
                      ,keymap
                      (kbd ,(nth (* 2 i) defs))
                      ,(nth (1+ (* 2 i)) defs)))))

(defmacro sage-shell:as-soon-as (form &rest body)
  (declare (indent 1))
  (let ((sym (sage-shell:gensym "sage-shell")))
    `(cond (,form (progn ,@body))
           (t (let ((,sym nil))
                (setq ,sym
                      (run-with-timer
                       0.01 0.01
                       (lambda () (condition-case err-var
                                      (when ,form
                                        (unwind-protect
                                            (progn ,@body)
                                          (cancel-timer ,sym)))
                                    (error (cancel-timer ,sym)
                                           (signal (car err-var)
                                                   (cdr err-var))))))))))))

(defmacro sage-shell:substitute-key-def (old-command new-command
                                                     search-keymap
                                                     def-keymap
                                                     &rest default-keys)
  `(sage-shell:aif (where-is-internal ',old-command ,search-keymap)
       (cl-loop for key in it
                do (define-key ,def-keymap key ',new-command))
     (cl-loop for key in (list ,@default-keys)
              do (define-key ,def-keymap key ',new-command))))

(defun sage-shell:get-value (value-or-func &rest args)
  "If VALUE-OR-FUNC is a function, then this returns the value
returned from the function, otherwise, this returns it self. "
  (if (functionp value-or-func)
      (apply value-or-func args)
    value-or-func))


(defsubst sage-shell:line-beginning-position ()
  (let ((inhibit-field-text-motion t))
    (line-beginning-position)))

(defmacro sage-shell:with-current-buffer-safe (buf-maybe &rest body)
  (declare (indent 1))
  `(when (and (buffer-live-p ,buf-maybe)
              (get-buffer ,buf-maybe))
     (with-current-buffer (get-buffer ,buf-maybe)
       ,@body)))

(defmacro sage-shell:if-process-alive (then-form &optional else-form)
  (declare (indent 0))
  `(if (and sage-shell:process-buffer
            (get-buffer-process sage-shell:process-buffer))
       ,then-form
     ,else-form))

(defmacro sage-shell:when-process-alive (&rest body)
  (declare (indent 0))
  `(sage-shell:if-process-alive
     (progn
       ,@body)))

(defsubst sage-shell:goto-line (n)
  (goto-char (point-min))
  (forward-line (1- n)))

(defun sage-shell:trim-right (s)
  (if (string-match (rx (1+ (or whitespace "\n")) buffer-end) s)
      (replace-match "" t t s)
    s))

(defun sage-shell:trim-left (s)
  (if (string-match (rx buffer-start (1+ (or whitespace "\n"))) s)
      (replace-match "" t t s)
    s))

(defmacro sage-shell:labels (bindings &rest body)
  (declare (indent 1) (debug cl-flet))
  (let ((labels-sym (if (string< emacs-version "24.3")
                        'labels
                      'cl-labels)))
    `(,labels-sym ,bindings
                  ,@body)))

(defmacro sage-shell:->> (x form &rest forms)
  (if (not forms)
      (if (sequencep form)
          (append form (list x))
        (list form x))
    `(sage-shell:->> (sage-shell:->> ,x ,form) ,@forms)))

(defmacro sage-shell:with-default-directory (directory &rest body)
  (declare (indent 1) (debug t))
  `(let ((default-directory (or (and ,directory
                                     (file-name-as-directory ,directory))
                                default-directory)))
     ,@body))

(defvar sage-shell:sage-modes '(sage-shell:sage-mode sage-shell-mode))

(defmacro sage-shell:push-elmts (state &rest attributes-values)
  "Push elements and returns new STATE."
  (declare (indent 1))
  `(setq ,state
         (append ,(cons 'list
                        (cl-loop for (a b) in
                                 (sage-shell:group attributes-values)
                                 collect (list 'cons a b)))
                 ,state)))

(defmacro sage-shell:chain (var &rest forms)
  (declare (indent 1))
  (append '(progn)
          (cl-loop for f in forms
                   collect
                   `(setq ,var ,f))))

(defmacro sage-shell:with-selected-window-if-possible (win &rest forms)
  (declare (indent 1))
  `(save-selected-window
     (when (and (windowp ,win)
                (window-live-p ,win))
       (select-window win))
     ,@forms))

;; cl-every raise warnings in Emacs 24.1 and 24.2
(defsubst sage-shell-every (f l)
  (cl-loop for x in l always (funcall f x)))

(defmacro sage-shell-when-emacs25-or-later (&rest body)
  (declare (indent 0))
  (unless (version< emacs-version "25.1")
    `(progn ,@body)))

;; Copied from pdf-tools
(cl-deftype sage-shell-list-of (type)
  `(satisfies
    (lambda (l)
      (and (listp l)
           (sage-shell-every
            (lambda (x)
              ;; Calling cl-typep raises warning in Emacs 24.
              (with-no-warnings
                (cl-typep x ',type)))
            l)))))

(cl-deftype sage-shell-alist-of (key-type val-type)
  `(satisfies
    (lambda (l)
      (and (listp l)
           (sage-shell-every
            (lambda (x)
              (with-no-warnings
                (and (cl-typep (car x) ',key-type)
                     (cl-typep (cdr x) ',val-type))))
            l)))))


;;; sage-shell
(defvar sage-shell-interfaces:other-interfaces
  '("axiom"
    "gap"
    "gap3"
    "giac"
    "gp"
    "mathematica"
    "gnuplot"
    "kash"
    "magma"
    "maple"
    "maxima"
    "matlab"
    "mathematica"
    "mwrank"
    "octave"
    "pari"
    "r"
    "singular"))


(defvar sage-shell:exec-path-error-msg
  (concat "Please set `sage-shell:sage-root' or"
          " `sage-shell:sage-executable' correctly."))

(defvar sage-shell:sage-root--cached nil)
(defun sage-shell:sage-root ()
  (or (sage-shell:aif sage-shell:sage-root
          (file-name-as-directory (expand-file-name it)))
      sage-shell:sage-root--cached
      (setq sage-shell:sage-root--cached
            (sage-shell:aif (and (sage-shell:sage-executable)
                                 (executable-find (sage-shell:sage-executable)))
                (sage-shell:->>  it
                                 file-truename
                                 file-name-directory)))))

(defun sage-shell:sage-executable ()
  (or sage-shell:sage-executable
      (if (stringp sage-shell:sage-root)
          (expand-file-name "sage" sage-shell:sage-root)
        (sage-shell:aif (executable-find "sage")
            (file-truename it)))))


(defvar sage-shell:output-finished-regexp-rx
  `(or (and (1+ (and (or "sage:" "sage0:" ">>>" "....:"
                         "(Pdb)" "ipdb>" "(gdb)") (1+ " ")))
            line-end)
       (and (or ,@sage-shell-interfaces:other-interfaces)
            ": " line-end)))

(defvar sage-shell:output-finished-regexp
  (rx-to-string
   `(and line-start ,sage-shell:output-finished-regexp-rx)))

(defvar sage-shell:process-buffer nil)
(make-variable-buffer-local 'sage-shell:process-buffer)

(defvar sage-shell:prompt-regexp
  (rx line-start
      (1+ (and (or "sage:" "sage0:" ">>>" "....:"
                   "(Pdb)" "ipdb>" "(gdb)") " ")))
  "Regular expression matching the Sage prompt.")

(defvar sage-shell:-prompt-regexp-no-eol-rx
  `(and (or
         (1+ (and (or "sage:" "sage0:" ">>>" "....:"
                      "(Pdb)" "ipdb>" "(gdb)") " "))
         (and (or ,@sage-shell-interfaces:other-interfaces)
              ": "))))

(defvar sage-shell:-prompt-regexp-no-eol
  (rx-to-string
   `(and line-start ,sage-shell:-prompt-regexp-no-eol-rx)))

;; cache buffers
(defvar sage-shell-indent:indenting-buffer-name " *sage-indent*")
(defvar sage-shell:output--buffer nil "The output buffer associated
to a process buffer.")
(make-variable-buffer-local 'sage-shell:output--buffer)


(defvar sage-shell:output-filter-finished-hook nil
  "Run after output finished.")
(make-variable-buffer-local 'sage-shell:output-filter-finished-hook)

(defvar sage-shell:redirect-filter-finished-hook nil
  "Run after redirect finished.")
(make-variable-buffer-local 'sage-shell:redirect-filter-finished-hook)


;;; Menu
(defvar sage-shell:in-out-menu-spec
  '("In/Out"
    ["Expand History Before Point" comint-replace-by-expanded-history t]
    ["List Input History" comint-dynamic-list-input-ring t]
    ["Previous Input" comint-previous-input t]
    ["Next Input" sage-shell:next-input t]
    ["Previous Matching Current Input"
     comint-previous-matching-input-from-input t]
    ["Next Matching Current Input" comint-next-matching-input-from-input t]
    ["Previous Matching Input..." comint-previous-matching-input t]
    ["Next Matching Input..." comint-next-matching-input t]
    ["Backward Matching Input..." comint-backward-matching-input t]
    ["Forward Matching Input..." comint-forward-matching-input t]
    ["Isearch Input String Backward..." comint-history-isearch-backward t]
    ["Isearch Input Regexp Backward..."
     comint-history-isearch-backward-regexp t]
    ["Copy Old Input" comint-copy-old-input t]
    ["Kill Current Input" comint-kill-input t]
    ["Show Current Output Group" comint-show-output t]
    ["Show Maximum Output" comint-show-maximum-output t]
    ["Backward Output Group" comint-previous-prompt t]
    ["Forward Output Group" comint-next-prompt t]
    ["Write Current Output Group to File" comint-write-output t]
    ["Append Current Output Group to File" comint-append-output-to-file t]
    ["Delete Current Output Group" comint-delete-output t]))
(defvar sage-shell:singal-menu-spec
  '("Signals"
    ["EOF"    sage-shell:send-eof t]
    ["KILL"   comint-kill-subjob t]
    ["QUIT"   comint-quit-subjob t]
    ["CONT"   comint-continue-subjob t]
    ["STOP"   comint-stop-subjob t]
    ["BREAK"  comint-interrupt-subjob t]))

(defvar sage-shell:menu-spec
  `("Sage"
    ,sage-shell:in-out-menu-spec
    ,sage-shell:singal-menu-spec
    "----"
    ["Toggle inline typesetting" sage-shell-view-toggle-inline-output]
    ["Toggle inline plots" sage-shell-view-toggle-inline-plots]))

(defvar sage-shell:output-finished-p nil)
(make-variable-buffer-local 'sage-shell:output-finished-p)

(cl-defun sage-shell:output-finished-p
    (&optional (buffer sage-shell:process-buffer))
  (buffer-local-value 'sage-shell:output-finished-p buffer))

(defun sage-shell:output-buffer ()
  "The output buffer associated to the process buffer"
  (sage-shell:with-current-buffer-safe sage-shell:process-buffer
    (cond ((and (bufferp sage-shell:output--buffer)
                (buffer-live-p sage-shell:output--buffer))
           sage-shell:output--buffer)
          (t (setq sage-shell:output--buffer
                   (get-buffer-create (format " *sage-output-%s*" (buffer-name))))))))

;;; Borrowed from Gallina's pyhon.el.
(defvar sage-shell-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give punctuation syntax to ASCII that normally has symbol
    ;; syntax or has word syntax and isn't a letter.
    (let ((symbol (string-to-syntax "_"))
          (sst (standard-syntax-table)))
      (dotimes (i 128)
        (unless (= i ?_)
          (if (equal symbol (aref sst i))
              (modify-syntax-entry i "." table)))))
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)
    ;; exceptions
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "$" table)
    table))

(defvar sage-shell:delete-temp-dir-p t)

(define-derived-mode sage-shell-mode comint-mode
  "Sage-repl" "Execute Sage commands interactively."

  (set-syntax-table sage-shell-mode-syntax-table)
  (set (make-local-variable 'completion-ignore-case)
       sage-shell:completion-ignore-case)
  (setq font-lock-defaults '(sage-shell:font-lock-keywords
                             nil nil nil beginning-of-line))
  (setq sage-shell:process-buffer (current-buffer))
  (setq comint-prompt-regexp
        (concat "^"
                (regexp-opt
                 (append (mapcar (lambda (x) (concat x ":"))
                                 sage-shell-interfaces:other-interfaces)
                         '("sage:" "sage0:" ">>>" "....:"
                           "(Pdb)" "ipdb>" "(gdb)"))))
        comint-prompt-read-only t
        comint-input-ring-file-name sage-shell:input-history-cache-file)
  (set (make-local-variable 'comint-redirect-finished-regexp)
       comint-prompt-regexp)
  (comint-read-input-ring t)

  (set (make-local-variable 'comint-redirect-completed) t)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'font-lock-syntactic-face-function)
       (lambda (state)
         (cond ((nth 3 state) font-lock-string-face)
               ((get-text-property (point) 'field) 'default)
               ((save-excursion
                  (beginning-of-line)
                  (re-search-forward "#" (line-end-position) t))
                font-lock-comment-face)
               (t 'default))))
  (set (make-local-variable 'comint-use-prompt-regexp) t)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "^[ \t]*#+ *")
  (set (make-local-variable 'comint-output-filter-functions)
       (remove 'comint-postoutput-scroll-to-bottom
               comint-output-filter-functions))
  (set (make-local-variable 'eldoc-documentation-function)
       #'sage-shell:eldoc-function)
  (set (make-local-variable 'yank-excluded-properties)
       (cons 'syntax-table yank-excluded-properties))
  (when sage-shell:scroll-show-maximum-output
    (set (make-local-variable 'scroll-conservatively) 1000))
  ;; Ignore duplicates in command history
  (setq comint-input-ignoredups t)
  (add-hook 'completion-at-point-functions
            'sage-shell:completion-at-point-func nil t)

  ;; Run init functions after Sage loaded.
  (add-to-list 'sage-shell:output-filter-finished-hook
               (lambda () (sage-shell:after-init-function
                           sage-shell:process-buffer)))
  (sage-shell:pcomplete-setup)
  ;; ansi-color-process-output may cause an error.
  (remove-hook 'comint-output-filter-functions
               'ansi-color-process-output t)
  (add-hook 'sage-shell:process-exit-hook
            #'sage-shell:-after-send-eof-func nil t)
  (add-hook 'kill-buffer-hook
            #'sage-shell:-kill-buffer-func nil t)
  (when sage-shell:delete-temp-dir-p
    (add-hook 'sage-shell:process-exit-hook
              #'sage-shell-edit:delete-temp-dir)))

(easy-menu-define sage-shell-menu
  sage-shell-mode-map "sage-shell menu"
  sage-shell:menu-spec)

(defvar sage-shell-mode-hook nil "Hook run when entering Sage Shell mode.")

(defun sage-shell:interrupt-subjob ()
  "Interrupt the current subjob."
  (interactive)
  (unless comint-redirect-completed
    (sage-shell:redirect-cleanup)
    (setq comint-redirect-completed t))
  (sage-shell:prepare-for-send)
  (if sage-shell:use-prompt-toolkit
      (progn
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (line (buffer-substring-no-properties bol eol))
               (proc (get-buffer-process (current-buffer))))
          (add-hook 'sage-shell:-pre-output-filter-hook
                    (lambda () (let ((inhibit-redisplay t))
                             (delete-region bol eol))))
          (process-send-string proc (concat line ""))
          ;; Delete whitespaces
          (add-hook 'sage-shell:-post-output-filter-hook
                    (lambda ()
                      (delete-region (point) (point-max))))))
    (comint-interrupt-subjob)))

(sage-shell:define-keys sage-shell-mode-map
  "TAB" 'sage-shell-tab-command
  "C-d" 'sage-shell:delchar-or-maybe-eof
  "RET" 'sage-shell:send-input
  "C-c C-i" 'sage-shell:complete
  "C-c C-h" 'sage-shell:help
  "C-c C-l" 'sage-shell:load-file
  "C-c M-o" 'sage-shell:clear-current-buffer
  "C-c o" 'sage-shell:list-outputs
  "C-c M-w" 'sage-shell:copy-previous-output-to-kill-ring)

(define-key sage-shell-mode-map [remap comint-next-input]
  'sage-shell:next-input)

(define-key sage-shell-mode-map [remap comint-delete-output]
  'sage-shell:delete-output)

(define-key sage-shell-mode-map [remap comint-interrupt-subjob]
  'sage-shell:interrupt-subjob)

(defun sage-shell:delchar-or-maybe-eof (arg)
  "Delete ARG characters forward or send an EOF to Sage process.
Sends an EOF only if point is at the end of the buffer and there is no input. "
  (interactive "p")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and proc (eobp) (= (point) (marker-position (process-mark proc)))
             (sage-shell:redirect-finished-p))
        (sage-shell:send-eof)
      (delete-char arg))))

(defun sage-shell:send-eof ()
  "Send an EOF to the current buffer's process."
  (interactive)
  (sage-shell:comint-send-input t t)
  (process-send-eof))

(defun sage-shell:-after-send-eof-func ()
  ;; kill cache buffes
  (cl-loop for bufn in (list (sage-shell:output-buffer)
                             sage-shell-indent:indenting-buffer-name)
           if (get-buffer bufn)
           do (kill-buffer bufn))

  ;; write comint-input-ring
  (comint-write-input-ring))

(defun sage-shell:ido-input-history ()
  (interactive)
  (insert (ido-completing-read "Input history: "
                               (ring-elements comint-input-ring))))

;; comint.el uses comint-input-ring-index for the input history, but
;; comint-send-input sets it to nil. So we need another global variable.
(defvar sage-shell:input-ring-index nil)
(make-variable-buffer-local 'sage-shell:input-ring-index)
(defun sage-shell:next-input (arg)
  "Cycles through forward input history. This command enables inserting
succesive lines in history."
  (interactive "*p")
  (cond
   (comint-input-ring-index (comint-next-input arg))
   ((not sage-shell:input-ring-index))
   (t (comint-previous-input (+ (- arg) 2 sage-shell:input-ring-index))
      (setq comint-input-ring-index
            (+ (- arg) 1 sage-shell:input-ring-index)))))


(defun sage-shell:-make-buf-if-needed (buf-maybe)
  (cond ((or (stringp buf-maybe) (bufferp buf-maybe))
         (get-buffer-create buf-maybe))
        (t (sage-shell:output-buffer))))

(defun sage-shell:send-command-sync
    (command &optional process-buffer output-buffer to-string raw)
  "internal function"
  (sage-shell:run-cell-raw-output
   command :sync t
   :process-buffer process-buffer
   :output-buffer output-buffer
   :to-string to-string
   :raw raw))

(defun sage-shell:wait-for-redirection-to-complete
    (&optional msec process-buffer)
  (let ((msec (or msec 1))
        (proc-buf (or process-buffer sage-shell:process-buffer)))
    (sage-shell:awhen (get-buffer proc-buf)
      (with-current-buffer it
        (while (null comint-redirect-completed)
          (accept-process-output nil 0 msec))))))

(cl-defstruct sage-shell:output-stct
  output success)

(defun sage-shell:eval-state (raw-output)
  "Parse output of run_cell_and_print_state."
  (let* ((success-str (substring-no-properties raw-output -2 -1))
         (output (substring-no-properties raw-output 0 -2))
         (success (cond ((string= success-str "0")
                         t)
                        ((string= success-str "1")
                         nil)
                        (t (error "Invalid output.")))))
    (make-sage-shell:output-stct
     :success success
     :output output)))


(defun sage-shell--error-callback (res)
  (unless (sage-shell:output-stct-success res)
    (error (sage-shell:output-stct-output res))))

(cl-defun sage-shell:run-cell (cell &key callback
                                    output-buffer
                                    process-buffer
                                    sync)
  (cl-check-type callback (or null function))
  (cl-check-type process-buffer (or null string buffer))
  (cl-check-type output-buffer (or null string buffer))
  (cl-check-type cell string)
  (let ((evaluator (sage-shell:py-mod-func "run_cell_and_print_state"))
        (callback (when (functionp callback)
                    (lambda (output)
                      (funcall callback (sage-shell:eval-state output))))))
    (sage-shell:run-cell-raw-output cell
                                    :output-buffer output-buffer
                                    :process-buffer process-buffer
                                    :sync sync
                                    :evaluator evaluator
                                    :callback callback)))

(defmacro sage-shell:after-redirect-finished (&rest body)
  (declare (indent 0))
  `(cond ((sage-shell:redirect-finished-p)
          (with-current-buffer sage-shell:process-buffer
            (progn ,@body)))
         (t (with-current-buffer sage-shell:process-buffer
              (add-to-list 'sage-shell:redirect-filter-finished-hook
                           (lambda () ,@body))))))

(defmacro sage-shell:push-to-redirect-finished-hook (&rest body)
  (declare (indent 0))
  `(with-current-buffer sage-shell:process-buffer
     (push (lambda () ,@body)
           sage-shell:redirect-filter-finished-hook)))

(cl-defun sage-shell:run-cell-raw-output (cell &key callback
                                               process-buffer
                                               output-buffer
                                               sync
                                               raw
                                               evaluator
                                               evaluator-key-args
                                               to-string)
  "CELL is a string which will be sent to the proces buffer,
When non-nil, CALLBACK should be a function and will be called if the
evaluation completes. The output will be passed as its argument.
If RAW is non-nil, CELL will be sent by process-send-string directly.
Otherwise return value of `sage-shell:-make-exec-cmd' is used.
If EVALUATOR is non-nil, it should be a Python function
which is similar to emacs_sage_shell.run_cell_and_print_msg_id."
  (cl-check-type cell string)
  (cl-check-type callback (or null function))
  (cl-check-type process-buffer (or null string buffer))
  (cl-check-type output-buffer (or null string buffer))
  (cl-check-type evaluator (or null string))
  (sage-shell-when-emacs25-or-later
    (cl-check-type evaluator-key-args (sage-shell-alist-of string string)))
  (unless (and (bufferp sage-shell:process-buffer)
               (buffer-live-p sage-shell:process-buffer)
               (get-buffer-process sage-shell:process-buffer))
    (setq sage-shell:process-buffer (get-buffer process-buffer)))
  (let ((proc-buf sage-shell:process-buffer)
        (out-buf (sage-shell:-make-buf-if-needed output-buffer))
        ;; If to-string is non-nil sync should be non-nil
        (sync (if to-string t sync)))

    (with-current-buffer proc-buf
      (sage-shell:wait-for-redirection-to-complete))

    (with-current-buffer out-buf
      (erase-buffer))

    (when (functionp callback)
      (sage-shell:push-to-redirect-finished-hook
       (let ((raw-output
              (sage-shell:-redirect-get-buffer-string out-buf)))
         (funcall callback raw-output))))

    (with-current-buffer proc-buf
      (sage-shell:redirect-setup out-buf proc-buf raw)
      (process-send-string
       proc-buf (sage-shell:-make-exec-cmd
                 cell raw evaluator evaluator-key-args))
      (when sync
        (sage-shell:wait-for-redirection-to-complete)))

    (when to-string
      (sage-shell:-redirect-get-buffer-string out-buf))))

(defvar sage-shell:-redirection-msg-id nil)

(defun sage-shell:-redirect-get-buffer-string (output-buffer)
  (with-current-buffer output-buffer
    (buffer-string)))


(defun sage-shell:send-command
    (command &optional process-buffer output-buffer sync raw)
  "Send COMMAND to PROCESS-BUFFER's process.  PROCESS-BUFFER is a
buffer where process is alive.  If OUTPUT-BUFFER is the exisiting
bufffer then the out put is inserted to the buffer. Otherwise
output buffer is the return value of `sage-shell:output-buffer'.
When sync is nill this return a lambda function to get the result."
  (let* ((output nil)
         (res-func (lambda () output))
         (output-buffer (sage-shell:-make-buf-if-needed output-buffer))
         (callback
          (unless sync
            (lambda (x)
              (setq output x)))))
    (sage-shell:run-cell-raw-output
     command
     :process-buffer process-buffer
     :output-buffer output-buffer
     :callback callback
     :sync sync
     :raw raw)
    (unless sync res-func)))


(defun sage-shell:-rdct-msg-id-start (msg-id)
  (concat msg-id "start"))

(defun sage-shell:-rdct-msg-id-end (msg-id)
  (concat msg-id "end"))

(defun sage-shell:-make-exec-cmd (raw-cmd
                                  raw
                                  &optional
                                  evaluator
                                  evaluator-key-args)
  (if raw
      (format "%s\n" raw-cmd)
    (let ((evaluator
           (or evaluator (sage-shell:py-mod-func "run_cell_and_print_msg_id"))))
      (format "%s(\"%s\", '%s', '%s'%s)\n"
              evaluator
              (sage-shell:escepe-string raw-cmd)
              (sage-shell:-rdct-msg-id-start
               sage-shell:-redirection-msg-id)
              (sage-shell:-rdct-msg-id-end
               sage-shell:-redirection-msg-id)
              (sage-shell:aif evaluator-key-args
                  (concat ", "
                          (mapconcat
                           (lambda (x) (format "%s=%s" (car x) (cdr x)))
                           it ", "))
                "")))))

(defun sage-shell:send-command-to-string (command &optional process-buffer raw)
  "Send process to command and return output as string."
  (sage-shell:send-command-sync command process-buffer nil t raw))

(defvar sage-shell:run-history nil "sage command history.")

(defvar sage-shell:python-module "_emacs_sage_shell"
  "Name of the python module.")

(defconst sage-shell:python-module-true-name "emacs_sage_shell")

(defun sage-shell:py-mod-func (funcname)
  (format "%s.%s" sage-shell:python-module
          funcname))

(defvar sage-shell:python-script-directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(let ((f (expand-file-name
          (format "%s.py" sage-shell:python-module-true-name)
          sage-shell:python-script-directory)))
  (unless (or (file-exists-p f)
              (file-exists-p (concat f "c")))
    (error (format "Cannot find the module %s"
                   sage-shell:python-module-true-name))))

(defun sage-shell:remove-trailing-slash (s)
  (if (string-match (rx "/" eol) s)
      (substring s 0 -1)
    s))

(defvar sage-shell:init-command-list
  (list
   (format "import %s as %s"
           sage-shell:python-module-true-name
           sage-shell:python-module)
   (format "sys.path.append('%s')"
           (sage-shell:remove-trailing-slash
            sage-shell:python-script-directory)))
  "Sage command list evaluated after loading Sage.")

(defvar sage-shell:term-name "emacs"
  "Name to use for TERM.")

(defun sage-shell:-kill-buffer-func ()
  "Purpose of this function is to run
`sage-shell:-after-send-eof-func' even if the process buffer is killed."
  (let ((proc (get-buffer-process (current-buffer))))
    (when (and proc (process-live-p proc))
      (sage-shell:-after-send-eof-func))))

(defvar sage-shell:process-exit-hook nil
  "List of functions to be called after the Sage process exits.")

(defun sage-shell:-process-sentinel-generator (default-sentinel)
  (lambda (proc msg)
    (funcall default-sentinel proc msg)
    (unless (process-live-p proc)
      (let ((buf (process-buffer proc)))
        (when (buffer-live-p buf)
          (with-current-buffer (process-buffer proc)
            (run-hooks 'sage-shell:process-exit-hook)))))))

(defun sage-shell:start-sage-process (cmd buffer)
  (if sage-shell:use-prompt-toolkit
      (sage-shell:-start-sage-process-prompt-toolkit cmd buffer)
    (sage-shell:-start-sage-process-readline cmd buffer))
  (let* ((proc (get-buffer-process buffer))
         (default-sentinel (process-sentinel proc)))
    (set-process-sentinel
     proc
     (sage-shell:-process-sentinel-generator default-sentinel)))
  (when sage-shell:check-ipython-version-on-startup
    (sage-shell:check-ipython-version
     " To disable this checking, set `sage-shell:check-ipython-version-on-startup' to `nil'.")))

(defcustom sage-shell:shell-command "/bin/sh"
  "Path of a command that behaves like a shell. It should accept -c option. This variable is used only when `sage-shell:use-prompt-toolkit' is non-`nil'."
  :type 'string
  :group 'sage-shell)

(defcustom sage-shell:shell-command-options nil
  "Command line options for `sage-shell:shell-command'."
  :type '(repeat string)
  :group 'sage-shell)

(defun sage-shell:-start-sage-process-prompt-toolkit (cmd buffer)
  (let* ((cmdlist (split-string cmd))
         (win (selected-window))
         (win-size
          (if (equal (window-buffer win) buffer)
              (sage-shell:-window-size win)
            (save-window-excursion
              (let ((win (display-buffer buffer)))
                (sage-shell:-window-size win))))))
    (let ((args (append
                 sage-shell:shell-command-options
                 (list "-c"
                       (format "TERM=%s; stty -nl echo rows %d columns %d sane 2>/dev/null;\
if [ $1 = .. ]; then shift; fi; exec \"$@\""
                               sage-shell:term-name (cdr win-size) (car win-size))
                       "..")
                 cmdlist)))
      (apply 'make-comint-in-buffer "Sage" buffer
             sage-shell:shell-command nil args))
    (let ((proc (get-buffer-process buffer)))
      (process-put proc 'sage-shell:win-height (cdr win-size))
      (process-put proc 'sage-shell:win-width (car win-size)))))

(defun sage-shell:-start-sage-process-readline (cmd buffer)
  (let ((cmdlist (split-string cmd)))
    (apply 'make-comint-in-buffer "Sage" buffer
           (car cmdlist) nil (cdr cmdlist))))

(defvar sage-shell:init-finished-p nil)
(make-variable-buffer-local 'sage-shell:init-finished-p)

(defvar sage-shell:check--sage-root-ok nil)

(defvar sage-shell-after-prompt-hook nil
  "Hook run after the fisrt prompt is displayed.")

(defun sage-shell:after-init-function (buffer)
  "Runs after starting Sage"
  (sage-shell:run-cell-raw-output
   (sage-shell:join-command
    (sage-shell:aif sage-shell:inspect-ingnore-classes
        (let ((cmd (format "%s.ignore_classes = [%s]"
                           sage-shell:python-module
                           (mapconcat 'identity it ", "))))
          (cons cmd sage-shell:init-command-list))
      sage-shell:init-command-list))
   :process-buffer buffer :sync t :raw t)
  (setq sage-shell:init-finished-p t)
  (run-hooks 'sage-shell-after-prompt-hook)
  (unless (sage-shell:check--sage-root)
    ;; Fix (sage-shell:sage-root)
    (setq sage-shell:sage-root--cached
          (let* ((s (sage-shell:send-command-to-string
                     (format "%s()"
                             (sage-shell:py-mod-func "print_sage_root"))))
                 (s1 (replace-regexp-in-string "
" "" s)))
            (if (string-match (rx "/" eol) s1)
                s1
              (concat s1 "/"))))
    (setq sage-shell:check--sage-root-ok t))
  (when sage-shell-sagetex:add-to-texinputs-p
    (sage-shell-sagetex:add-to-texinputs))
  (let ((win (get-buffer-window buffer)))
    (when win
      (with-selected-window win
        (goto-char (process-mark (get-buffer-process
                                  buffer)))))))

(defun sage-shell:check--sage-root ()
  (or sage-shell:check--sage-root-ok
      (setq sage-shell:check--sage-root-ok
            (when (sage-shell:check--sage-root1)
              t))))

(defun sage-shell:check--sage-root1 ()
  "Check (sage-shell:sage-root)."
  (and (cl-loop for a in '("devel" "src")
                for d = (expand-file-name a (sage-shell:sage-root))
                thereis (and (file-exists-p d)
                             (file-directory-p d)))
       (let ((ver-txt (expand-file-name "VERSION.txt" (sage-shell:sage-root))))
         (and (file-exists-p ver-txt)
              (with-temp-buffer
                (insert-file-contents-literally ver-txt)
                (goto-char (point-min))
                (let ((case-fold-search nil))
                  (re-search-forward (rx bow "Sage" eow) nil t)))))))

(defun sage-shell:-shell-buf-name-new-num (&optional host-name)
  (let ((num 0))
    (while (get-buffer (sage-shell:-shell-buffer-name num host-name))
      (setq num (1+ num)))
    num))

(defun sage-shell:-shell-buffer-name (&optional num host-name)
  (let ((num-s (if (and num (/= num 0)) (format "<%d>" num) ""))
        (host-name-s (if host-name (format "@%s" host-name) "")))
    (format "*Sage%s%s*" num-s host-name-s)))

(defun sage-shell:shell-buffer-name (new &optional host-name)
  (cond (new (let ((num (sage-shell:-shell-buf-name-new-num host-name)))
               (sage-shell:-shell-buffer-name num host-name)))
        (t (sage-shell:-shell-buffer-name))))

(defcustom sage-shell:ask-command-options t
  "Non-`nil' means being asked sage command options when invoking M-x run-sage."
  :type 'boolean
  :group 'sage-shell)

(defun sage-shell:read-command ()
  (unless (and (sage-shell:sage-executable)
               (executable-find (sage-shell:sage-executable)))
    (error sage-shell:exec-path-error-msg))
  (let ((lst (and
              sage-shell:ask-command-options
              (split-string
               (read-from-minibuffer "Run sage (like this): "
                                     "sage" nil nil 'sage-shell:run-history
                                     "sage") " "))))
    (format "%s %s" (sage-shell:sage-executable)
            (mapconcat 'identity (cdr lst) " "))))

(cl-defun sage-shell:run (cmd new &key
                              (switch-function 'switch-to-buffer)
                              buffer-name)
  "Running Sage function internal.
SIWTCH-FUNCTION is 'no-switch, or a function with one
argument. If buffer-name is non-nil, it will be the buffer name of the process buffer."
  (let ((buf (get-buffer-create (if (stringp buffer-name)
                                    buffer-name
                                  (sage-shell:shell-buffer-name new))))
        (cur-buf (current-buffer)))
    (when (and sage-shell:use-prompt-toolkit (version< emacs-version "24.4"))
      (error "Please use Emacs 24.4 or later."))
    (cond ((eq switch-function 'no-switch)
           (switch-to-buffer cur-buf))
          (t (funcall switch-function buf)))

    (unless (get-buffer-process buf)
      (sage-shell:start-sage-process cmd buf)
      (with-current-buffer buf

        (let ((proc (get-buffer-process buf)))
          (set-process-filter proc 'sage-shell:output-filter)
          ;; Don't call window--adjust-process-windows in windows.el
          ;; It calls the process-filter and modify the output buffer.
          (when sage-shell:use-prompt-toolkit
            (process-put proc 'adjust-window-size-function (lambda (_proc _win) nil)))
          (sage-shell-mode))))
    ;; Tell the process the window size for Ipython5's newprompt
    (when sage-shell:use-prompt-toolkit
      (sage-shell:-adjust-window-size)
      ;; (add-hook 'window-configuration-change-hook
      ;;           'sage-shell:-adjust-window-size nil t)
      )
    buf))

;;;###autoload
(defun sage-shell:run-sage (cmd)
  (interactive (list (sage-shell:read-command)))
  (sage-shell:run cmd nil))

;;;###autoload
(defun sage-shell:run-new-sage (cmd)
  (interactive (list (sage-shell:read-command)))
  (sage-shell:run cmd t))

(defun sage-shell:restart-sage (cmd)
  "Restart the Sage process. If there are multiple Sage processes, then this
function asks which process is to be restarted."
  (interactive (list (sage-shell:read-command)))
  (sage-shell-edit:set-sage-proc-buf-internal
   nil t (concat
          "There are multiple Sage processes. "
          "Please select the process buffer to be restarted: "))
  (let* ((buf sage-shell:process-buffer)
         (proc (get-buffer-process buf)))
    (with-current-buffer buf
      (add-hook 'sage-shell:process-exit-hook
                (lambda ()
                  (sage-shell:run cmd nil
                                  :buffer-name (buffer-name buf)
                                  :switch-function 'display-buffer))
                t t)
      (process-send-eof proc))))

(defun sage-shell:check-ipython-version (&optional startup-msg)
  "Check IPython version and check if sage-shell:use-prompt-toolkit is correctly set."
  (interactive)
  (message "Checking IPython version...")
  (deferred:$
    (deferred:process
      (sage-shell:sage-executable)
      "-c" "import IPython; print IPython.version_info[0]")
    (deferred:nextc it
      (lambda (x)
        (message (concat
                  "Checking IPython version... Done."
                  startup-msg))
        (let ((version (string-to-number (sage-shell:trim-right x)))
              (msg nil))
          (cond ((and (< version 5) sage-shell:use-prompt-toolkit)
                 (setq msg
                       (concat
                        "You should set `sage-shell:use-prompt-toolkit' to nil.\n"
                        "Set it to nil and restart the SageMath process.")))
                ((and (>= version 5) (null sage-shell:use-prompt-toolkit))
                 (setq msg
                       (concat
                        "You should set `sage-shell:use-prompt-toolkit' to t.\n"
                        "Set it to t and restart the SageMath process."))))
          (when msg
            (display-message-or-buffer msg)))))))

(defun sage-shell-tab-command ()
  (interactive)
  (cond
   ((and
     (null sage-shell:use-prompt-toolkit)
     (null (sage-shell:at-top-level-p))
     (looking-back (concat sage-shell:prompt-regexp " *")
                   (sage-shell:line-beginning-position)))
    (sage-shell-indent:indent-line))
   (t (sage-shell:complete))))

(defvar sage-shell:sage-version nil)
(defun sage-shell:sage-version ()
  (or sage-shell:sage-version
      (let ((str (shell-command-to-string "sage -version")))
        (when (string-match "Sage Version \\([0-9]\\.[0-9]\\)" str)
          (setq sage-shell:sage-version
                (string-to-number (match-string 1 str)))))))

(defvar sage-shell:dot-sage "~/.sage" "DOT_SAGE directory.")

(defun sage-shell:redirect-finished-p ()
  (buffer-local-value 'comint-redirect-completed sage-shell:process-buffer))

(defvar sage-shell-cpl:-cands-in-current-session nil
  "A cached list of user defined variables in the Sage process buffer.")
(make-variable-buffer-local 'sage-shell-cpl:-cands-in-current-session)

(defvar sage-shell:clear-command-cache-hook nil
  "A hook that runs each time after sage-shell:clear-command-cache is called.")

(defun sage-shell:clear-command-cache ()
  (sage-shell:with-current-buffer-safe sage-shell:process-buffer
    (sage-shell:when-process-alive
      (sage-shell-cpl:set-cmd-lst "sage" nil)
      (setq sage-shell-cpl:-cands-in-current-session nil)
      (sage-shell:clear-completion-sync-cached)
      (sage-shell:-clear-eldoc-cache)
      (sage-shell-cpl:-clear-argspec-cache)
      (run-hooks 'sage-shell:clear-command-cache-hook))))

(defun sage-shell:update-sage-commands ()
  (sage-shell-interfaces:update-cmd-lst "sage"))

(defalias 'sage-shell:load-file 'sage-shell-edit:load-file)
(defalias 'sage-shell:attach-file 'sage-shell-edit:attach-file)
(defalias 'sage-shell-help:send-current-line 'sage-shell:send-doctest)

(defun sage-shell:send-magic-cmd-base (magic-command objname &optional async)
  "If `async' is nil, return the result as string, otherwise
returns a lamda function with no args to obtain the result."
  (let* ((cmd (format "%%%s %s" magic-command objname)))
    (cond (async (sage-shell:send-command cmd)
                 (lambda ()
                   (sage-shell:with-current-buffer-safe
                       (sage-shell:output-buffer)
                     (buffer-string))))
          (t (sage-shell:send-command-to-string cmd)))))

(defun sage-shell:insert-action (can)
  (let ((beg (sage-shell:word-at-pt-beg)))
    (delete-region beg (point))
    (insert can)))

(defun sage-shell:word-at-pt-beg (&optional pt interface)
  "This can be used in both sage-shell-mode and sage-shell:sage-mode."
  (save-excursion
    (when pt (goto-char pt))
    (let ((chars (sage-shell-interfaces:get
                  (or interface
                      (sage-shell-interfaces:current-interface) "sage")
                  'var-chars)))
      (skip-chars-backward chars) (point))))

(defun sage-shell:word-at-point (&optional pt)
  (buffer-substring (point) (sage-shell:word-at-pt-beg pt)))

(defun sage-shell:help1 ()
  (sage-shell-help:describe-symbol
   (sage-shell-cpl:to-objname-to-send
    ;; This function set the current state.
    (sage-shell:completing-read-commands))))

(defun sage-shell:complete ()
  (interactive)
  (sage-shell:when-process-alive
    (cond
     ((member sage-shell:completion-function '(auto-complete pcomplete))
      (let ((this-command (cl-case sage-shell:completion-function
                            (auto-complete 'auto-complete)
                            (pcomplete 'pcomplete)
                            (t this-command)))
            (last-command
             (if (and (or (eq last-command 'sage-shell-tab-command)
                          (eq last-command 'sage-shell:complete))
                      (eq sage-shell:completion-function 'pcomplete))
                 'pcomplete
               last-command)))
        (call-interactively sage-shell:completion-function)))
     (t (let ((minibuffer-history nil))
          (completion-at-point))))))

(defun sage-shell:help ()
  (interactive)
  (sage-shell:when-process-alive
    (cond ((functionp sage-shell:help-completion-function)
           (funcall sage-shell:help-completion-function))
          (t (sage-shell:help1)))))

(defvar sage-shell:minibuffer-history nil)

(defun sage-shell:completing-read-commands ()
  (completing-read "Sage Objects: "
                   (sage-shell-cpl:candidates-sync)
                   nil nil (sage-shell:word-at-point)
                   sage-shell:minibuffer-history))


;; Copied from sage-mode.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar sage-shell:site-packages-regexp
  "\\(local/lib/python[0-9.]*/site-packages.*?\\)/sage"
  "Regexp to match sage site-packages files.

Match group 1 will be replaced with devel/sage-branch")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sage-shell:src-version (filename)
  "If FILENAME is in site-packages, current branch version, else FILENAME."
  (save-match-data
    (let* ((match (string-match sage-shell:site-packages-regexp filename)))
      (sage-shell:aif (and filename match
                           (cl-loop for a in '("devel" "src")
                                    if (file-exists-p
                                        (expand-file-name
                                         a (sage-shell:sage-root)))
                                    return a))
          (let* ((dr (concat (substring filename 0
                                        (match-beginning 1))
                             it))
                 (branch (cond ((string= "devel" it)
                                (or (file-symlink-p (concat dr "/sage"))
                                    "/sage"))
                               (t "")))
                 (base1 (substring filename (match-end 1)))
                 (base (if (string-match (rx "so" eol) base1)
                           (concat (file-name-sans-extension base1) ".pyx")
                         base1)))
            (let ((-fname (concat dr branch base)))
              (if (file-exists-p -fname)
                  -fname
                filename)))
        filename))))

(defun sage-shell:site-package-version (filename)
  "Inverse to `sage-shell:src-version'"
  (cond ((sage-shell:aand
           (string-match (expand-file-name "src/sage/"
                                           (sage-shell:sage-root))
                         filename)
           (= it 0))
         (let* ((python-dir1
                 (expand-file-name "local/lib/python2.7/site-packages/"
                                   (sage-shell:sage-root)))
                (python-dir (if (file-exists-p python-dir1)
                                (file-truename python-dir1)))
                (sfile-name1 (expand-file-name
                              (concat "sage/"
                                      (substring filename (match-end 0)))
                              python-dir)))
           (if (file-exists-p sfile-name1)
               sfile-name1
             filename)))
        (t filename)))

(defun sage-shell:source-file-and-line-num (obj)
  "Return (cons sourcefile line-number)"
  (let ((str (sage-shell:send-command-to-string
              (format "%s(%s)"
                      (sage-shell:py-mod-func "print_source_file_and_line_num")
                      obj))))
    (when (string-match (rx (group (1+ (not (syntax whitespace))))
                            (1+ " ") "*" (1+ " ")
                            (group (1+ num)))
                        str)
      (let ((src-file (match-string 1 str)))
        (cons (if sage-shell:prefer-development-file-p
                  (sage-shell:src-version src-file)
                src-file)
              (string-to-number (match-string 2 str)))))))

(cl-defun sage-shell:find-source-in-view-mode (obj &optional (offset 0))
  (let* ((src-line (sage-shell:source-file-and-line-num obj))
         (src (car-safe src-line))
         (line (cdr-safe src-line))
         (proc-buf sage-shell:process-buffer))
    (if (and src-line
             (file-readable-p src))
        (let* ((buf (find-file-noselect src))
               (win (display-buffer buf)))
          (with-current-buffer buf
            (view-mode 1)
            (select-window win)
            (sage-shell:goto-line line)
            (recenter offset)
            (setq sage-shell:process-buffer proc-buf)))
      (message "Source file not found."))))

(defun sage-shell:join-command (cmds)
  (mapconcat 'identity (reverse cmds) "; "))


(defvar sage-shell:-eldoc-syntax-table
  (let ((table (copy-syntax-table sage-shell-mode-syntax-table)))
    (modify-syntax-entry ?\[ " " table)
    (modify-syntax-entry ?\] " " table)
    table))

(defun sage-shell:-in-func-call-p (&optional pt limit)
  "Returns a list s.t.
  0: begging of funciton
  1: end of funciton
  2: funciton name
  3: inside string or not"
  (with-syntax-table sage-shell:-eldoc-syntax-table
    (let* ((bd (or limit (line-beginning-position)))
           (pt (or pt (point)))
           (pps (parse-partial-sexp bd pt))
           (beg-of-ls (cadr pps)))
      (save-excursion
        (when beg-of-ls
          (goto-char beg-of-ls)
          (skip-chars-backward " " bd)
          (skip-chars-backward
           (concat (sage-shell-interfaces:get "sage" 'var-chars) ".") bd)
          (cond
           ;; Inside tuple
           ((looking-at (rx (0+ whitespace) "("))
            (sage-shell:-in-func-call-p nil bd))
           (t (unless (looking-at (rx (0+ whitespace) "."))
                (list (point) beg-of-ls
                      (buffer-substring-no-properties (point) beg-of-ls)
                      (nth 3 pps))))))))))

;; eldoc
(defvar sage-shell:-eldoc-cache nil)
(defun sage-shell:-clear-eldoc-cache ()
  (setq sage-shell:-eldoc-cache nil))
(make-variable-buffer-local 'sage-shell:-eldoc-cache)

(when (fboundp #'eldoc-add-command)
  (eldoc-add-command #'sage-shell-tab-command))

(defun sage-shell:eldoc-function ()
  (when (and (sage-shell:redirect-and-output-finished-p)
             (sage-shell:at-top-level-and-in-sage-p))
    (sage-shell:-eldoc-function (sage-shell-cpl:parse-current-state))))

(defun sage-shell:-eldoc-function (state)
  "Has less state than `sage-shell:eldoc-function'"
  (let* ((func-name (sage-shell-cpl:get state 'in-function-call))
         (base-name (sage-shell-cpl:get state 'in-function-call-base-name))
         (func-end (sage-shell-cpl:get state 'in-function-call-end))
         (buf-args-str (when func-name
                         (buffer-substring-no-properties
                          (1+ func-end)
                          (save-excursion
                            (skip-chars-forward
                             "[a-zA-Z0-9_ =]"
                             (line-end-position))
                            (point)))))
         (s (and func-name
                 (sage-shell:-eldoc-function-str
                  func-name
                  (sage-shell:aif base-name (format "'%s'" it) "None"))))
         (keyword-reg (rx bol
                          (0+ whitespace)
                          (group (1+ (or alnum "_")))
                          (0+ whitespace) "=")))
    (when (and func-name s
               (not (string-match (rx "[noargspec]") s))
               (not (string= s "")))
      (let* ((buf-args (sage-shell:-eldoc-split-buffer-args buf-args-str))
             (last-arg (car (last buf-args)))
             (beg-end (cond ((string-match keyword-reg last-arg)
                             (sage-shell:-eldoc-highlight-beg-end
                              func-name s
                              (match-string 1 last-arg) nil))
                            ((cl-loop for s in buf-args
                                      never (string-match keyword-reg s))
                             (sage-shell:-eldoc-highlight-beg-end
                              func-name s nil (1- (length buf-args)))))))
        (if beg-end
            (let ((s-noprop (substring-no-properties s)))
              (add-text-properties (car beg-end)
                                   (cdr beg-end)
                                   '(face eldoc-highlight-function-argument)
                                   s-noprop)
              s-noprop)
          s)))))

(defun sage-shell:-eldoc-split-buffer-args (buf-args-str)
  (split-string
   ;; Replace "," inside string or brackets by space.
   (with-temp-buffer
     (with-syntax-table sage-shell-mode-syntax-table
       (insert (replace-regexp-in-string
                (rx "\n") " " buf-args-str))
       (goto-char (point-min))
       (while (re-search-forward (rx ",") nil t)
         (let ((pps (parse-partial-sexp (point-min)
                                        (point))))
           ;; Inside a string, list or tuple.
           (if (or (nth 3 pps)
                   (sage-shell:awhen (nth 1 pps)
                     (memq (char-after it)
                           (list ?\[ ?\())))
               (replace-match " "))))
       (buffer-string))) ","))

(defun sage-shell:-eldoc-function-str (func-name base-name)
  (sage-shell:with-current-buffer-safe sage-shell:process-buffer
    (sage-shell:when-process-alive
      (let ((cache (assoc-default func-name sage-shell:-eldoc-cache)))
        (cond
         (cache cache)
         ((and (sage-shell:at-top-level-and-in-sage-p)
               (sage-shell:redirect-and-output-finished-p))
          (let* ((res (sage-shell:->>
                       (sage-shell:py-mod-func
                        (format "print_def('%s', base_name=%s)"
                                func-name base-name))
                       sage-shell:send-command-to-string
                       (funcall
                        (lambda (s)
                          (replace-regexp-in-string (rx "\n") "" s nil t)))
                       sage-shell:trim-right)))
            (push (cons func-name res) sage-shell:-eldoc-cache)
            res)))))))


(defvar sage-shell:-eldoc-args-syntax-table
  (let ((table (copy-syntax-table sage-shell-mode-syntax-table)))
    (modify-syntax-entry ?* "w" table)
    table))

(defun sage-shell:-eldoc-highlight-beg-end (func-name def-str keyword idx)
  (with-syntax-table sage-shell:-eldoc-args-syntax-table
    (let* ((func-len (length func-name))
           (args-s (substring def-str (1+ func-len) -1))
           (args (sage-shell:->>
                  (sage-shell:-eldoc-split-buffer-args args-s)
                  (mapcar (lambda (s) (sage-shell:trim-left s)))))
           (rest-args (list args args-s func-len)))
      (cond (keyword
             (or (apply #'sage-shell:-eldoc--hl-beg-end-1
                        (rx-to-string
                         `(or (and bol
                                   ,keyword
                                   symbol-end
                                   (zero-or-one "=")
                                   (1+ nonl))
                              (and bol ,keyword eol)))
                        rest-args)
                 (apply #'sage-shell:-eldoc--hl-beg-end-1
                        (rx bol "**" (1+ (or alnum "_")))
                        rest-args)))
            (idx (let ((rest (nthcdr idx args)))
                   (cond ((and rest (not (string-match (rx bol "*")
                                                       (car rest))))
                          (sage-shell:-eldoc--hl-beg-end
                           rest func-len (length args-s)))
                         (t (apply #'sage-shell:-eldoc--hl-beg-end-1
                                   (rx bol "*" (1+ (or alnum "_")))
                                   rest-args)))))))))

(defun sage-shell:-eldoc--hl-beg-end-1 (reg args args-s func-len)
  "Find argument which matches REG and compute (cons beg end)"
  (sage-shell:aif (cl-loop for xs on args
                           if (string-match reg (car xs))
                           return xs)
      (sage-shell:-eldoc--hl-beg-end it func-len (length args-s))))


(defun sage-shell:-eldoc--hl-beg-end (rest func-len len-args-s)
  "Compute (cons beg end) form REST."
  (cons (sage-shell:-eldoc-highlight-indx-fn
         func-len len-args-s rest)
        (- (sage-shell:-eldoc-highlight-indx-fn
            func-len len-args-s (cdr rest))
           ;; if not the last element, delete the length of ", ".
           (if (cdr rest) 2 0))))

(defun sage-shell:-eldoc-highlight-indx-fn (func-len len-args-s ls)
  (+ (1+ func-len)
     (- len-args-s
        (cl-loop for a on ls
                 summing (+ (length (car a))
                            (if (cdr a) 2 0))))))


(defun sage-shell:python-syntax-output-p (line)
  "Return non nil if LINE contains a sentence with the python
  syntax. If returned value is non-nil and non-number, then whole
  line is regarded as python sentence. If a number, then it
  indicates the point where comment starts."
  (cond
   ((string-match-p (rx line-start "Pasting code; enter") line) nil)
   ((string-match-p "'\\|\"" line) nil)
   ((string-match-p (rx "Error" eow (1+ whitespace) "Traceback") line) t)
   ((string-match-p (rx "Error" (group ": ")) line) (match-beginning 1))
   (t t)))

(defun sage-shell:comment-out-output ()
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        (case-fold-search nil))
    (save-excursion
      (when comint-last-output-start
        (cl-loop initially
                 (goto-char comint-last-output-start)
                 while (not (eobp))
                 do
                 (unless (save-excursion
                           (forward-line 0)
                           (looking-at sage-shell:-prompt-regexp-no-eol))
                   (let ((p (sage-shell:python-syntax-output-p
                             (buffer-substring (line-beginning-position)
                                               (line-end-position)))))
                     (cond ((not p)
                            (put-text-property
                             (line-beginning-position)
                             (min (point-max)
                                  (1+ (line-beginning-position)))
                             'syntax-table (cons 11 nil)))
                           ((numberp p)
                            (put-text-property
                             p (min (point-max) (1+ p)) 'syntax-table
                             (cons 11 nil))))))
                 (forward-line 1))))))

(defmacro sage-shell:after-output-finished (&rest body)
  (declare (indent 0))
  `(cond ((sage-shell:output-finished-p)
          (with-current-buffer sage-shell:process-buffer
            (progn ,@body)))
         (t (with-current-buffer sage-shell:process-buffer
              (add-to-list 'sage-shell:output-filter-finished-hook
                           (lambda () ,@body))))))

(defmacro sage-shell:push-to-output-finished-hook (&rest body)
  (declare (indent 0))
  `(with-current-buffer sage-shell:process-buffer
     (push (lambda () ,@body)
           sage-shell:output-filter-finished-hook)))

(defun sage-shell:run-hook-once (hook)
  (let ((hook-saved (symbol-value hook)))
    (set hook nil)
    (cl-loop for f in (nreverse hook-saved) do (funcall f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copied from windows.el of Emacs 25.0.
(defun sage-shell:-window-max-chars-per-line (&optional window _face)
  (with-selected-window (window-normalize-window window t)
    (let* ((window-width (with-no-warnings
                           (window-body-width window t)))
           (font-width (frame-char-width))
           (ncols (/ window-width font-width)))
      (if (and (display-graphic-p)
               (bound-and-true-p overflow-newline-into-fringe)
               (not
                (or (eq left-fringe-width 0)
                    (and (null left-fringe-width)
                         (= (frame-parameter nil 'left-fringe) 0))))
               (not
                (or (eq right-fringe-width 0)
                    (and (null right-fringe-width)
                         (= (frame-parameter nil 'right-fringe) 0)))))
          ncols
        ;; FIXME: This should remove 1 more column when there are no
        ;; fringes, lines are truncated, and the window is hscrolled,
        ;; but EOL is not in the view, because then there are 2
        ;; truncation glyphs, not one.
        (1- ncols)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sage-shell:-proc-window-size (process window)
  (cond ((and (boundp 'window-adjust-process-window-size-function)
              (functionp window-adjust-process-window-size-function))
         ;; Emacs 25.1 has window-adjust-process-window-size-function
         (funcall window-adjust-process-window-size-function
                  process (list window)))
        (t (sage-shell:-window-size window))))

(defun sage-shell:-window-size (window)
  (let ((width (cond ((fboundp 'window-max-chars-per-line)
                      ;; To silence warning of flycheck-package
                      (funcall #'window-max-chars-per-line window))
                     (t (sage-shell:-window-max-chars-per-line
                         window)))))
    (cons width (window-body-height window))))

(defun sage-shell:-adjust-window-size-each (proc win)
  (let ((buf (window-buffer win)))
    (with-current-buffer buf
      (when (and proc (process-live-p proc)
                 (eq major-mode 'sage-shell-mode)
                 (sage-shell:redirect-and-output-finished-p))
        (let ((sizes (sage-shell:-proc-window-size proc win))
              (width (process-get proc 'sage-shell:win-width)))
          (when (and sizes (not (equal (car sizes) width)))
            (set-process-window-size proc (cdr sizes) (car sizes))
            (process-put proc 'sage-shell:win-height (cdr sizes))
            (process-put proc 'sage-shell:win-width (car sizes))))))))

(defun sage-shell:-adjust-window-size ()
  (walk-windows
   (lambda (win)
     (let* ((buf (window-buffer win))
            (proc (get-buffer-process buf)))
       (sage-shell:-adjust-window-size-each
        proc win)))))

(defun sage-shell-adjust-window-size ()
  "Tell the emacs window size of the process buffer to the Sage process"
  (interactive)
  (sage-shell:-adjust-window-size))

(defsubst sage-shell:-convert-to-ascii-banner (string)
  (cond ((not (or sage-shell:init-finished-p
                  sage-shell:use-unicode-banner))
         (sage-shell:->> string
                         (replace-regexp-in-string (rx (or "─" "━"
                                                           "┌" "┐"
                                                           "└" "┘"
                                                           "┏" "┓"
                                                           "┗" "┛"))
                                                   "-")
                         (replace-regexp-in-string (rx (or "│" "┃")) "|")))
        (t string)))

(defvar sage-shell:-ansi-escpace-handler-alist
  `((?n . ,#'ignore)
    (?J . ,#'sage-shell:-delete-display)
    (?D . ,#'sage-shell:-cursor-back)
    (?C . ,#'sage-shell:-cursor-forward)
    (?A . ,#'sage-shell:-cursor-up)
    (?B . ,#'sage-shell:-cursor-down)
    (?K . ,#'sage-shell:-delete-line)
    (?H . ,#'sage-shell:-move-cursor-pos))
  "An alist for ansi escapse sequences consisting of
(cons char function-symbol)).")

(defvar sage-shell:-ansi-escape-regexp
  (rx-to-string
   `(and
     "[" (group (0+ (or num ";")))
     (group
      (or ,@(cl-loop for x in sage-shell:-ansi-escpace-handler-alist
                     collect (char-to-string (car x))))))))

(defun sage-shell:-decompose-ansi-escape-seq (str)
  (setq str (sage-shell:-ansi-escape-filter-out str))
  (let ((case-fold-search nil)
        (str-saved str)
        (res nil))
    (cl-loop while (string-match sage-shell:-ansi-escape-regexp str)
             for chr = (string-to-char (match-string 2 str))
             for args = (unless (string= (match-string 1 str) "")
                          (mapcar #'string-to-number
                                  (split-string (match-string 1 str) ";")))
             do
             (unless (string= (substring str 0 (match-beginning 0)) "")
               (push (substring str 0 (match-beginning 0)) res))
             (push (list chr args) res)
             (setq str (substring str (match-end 0)))
             finally return (if (null res)
                                str-saved
                              (progn
                                (unless (string= str "")
                                  (push str res))
                                (nreverse res))))))

(defvar sage-shell:-ansi-escape-drop-regexp
  (rx (and "["
           (0+ (or num ";" "=" "?"))
           (regexp "[Rmhl]"))))

(defun sage-shell:-ansi-escape-filter-out (str)
  (let ((case-fold-search nil))
    (replace-regexp-in-string sage-shell:-ansi-escape-drop-regexp
                              ""
                              str nil t)))

(defun sage-shell:-insert-str (str)
  (let ((str (propertize str 'field 'output)))
    (cond ((eobp)
           (insert str))
          (t (insert str)
             (delete-region (point)
                            (min (+ (point) (length str))
                                 (line-end-position)))))))

(defun sage-shell:-insert-and-handle-ansi-escape (proc str)
  "Insert strings sended by the process and handle ANSI escape sequences,
and remove sequences matched by sage-shell:-ansi-escape-regexp.
Return value is not deifned."
  (let* ((case-fold-search nil)
         (seqs (sage-shell:-decompose-ansi-escape-seq str)))
    (cond ((stringp seqs)
           (sage-shell:-insert-and-handle-char seqs))
          (t (dolist (a seqs)
               (cond ((listp a)
                      (let ((args (cadr a)))
                        (apply (assoc-default
                                (car a)
                                sage-shell:-ansi-escpace-handler-alist)
                               proc args)))
                     (t (sage-shell:-insert-and-handle-char a))))))))

(defvar sage-shell:-char-handler-alist
  '((?\r . (lambda () (forward-line 0)))
    (?\n . (lambda () (sage-shell:-down 1)))))

(defvar sage-shell:-char-handler-regexp
  (rx (or "\n" "")))

(defun sage-shell:-insert-and-handle-char (str)
  "Insert STR. But call the corresponding function if car of
`sage-shell:-char-handler-alist' is seen."
  (setq str (replace-regexp-in-string (rx (1+ "")) "" str))
  (let ((str-rpcd (replace-regexp-in-string (rx "\n") "\n" str)))
    (cond ((and (eobp)
                (null (string-match-p (rx "")  str-rpcd)))
           (sage-shell:-insert-str str-rpcd))
          (t (while (string-match sage-shell:-char-handler-regexp str)
               (let ((beg (match-beginning 0))
                     (end (match-end 0))
                     (m-str (match-string 0 str)))
                 (when (> beg 0)
                   (sage-shell:-insert-str (substring str 0 beg)))
                 (funcall (assoc-default (string-to-char m-str)
                                         sage-shell:-char-handler-alist))
                 (setq str (substring str end))))
             (sage-shell:-insert-str str)))))

(defun sage-shell:-down (down)
  "Similar to term-down."
  (let ((start-column (current-column))
        (inhibit-field-text-motion t))
    (cond ((>= down 0)
           (dotimes (_ down)
             (cond ((progn (end-of-line) (eobp))
                    ;; This case for exception of forward-line
                    (insert "\n")
                    (forward-line 1))
                   (t (when (> (forward-line 1) 0)
                        (insert "\n")
                        (forward-line 1))))))
          (t (forward-line down)))
    ;; Go to the same column
    (move-to-column start-column t)))

(defun sage-shell:-bol ()
  (goto-char (sage-shell:line-beginning-position))
  (when (or (looking-at sage-shell:prompt-regexp)
            (looking-at (rx-to-string
                         `(and
                           (or ,@sage-shell-interfaces:other-interfaces)
                           ": "))))
    (goto-char (match-end 0))))

(defun sage-shell:-move-cursor-pos (_proc &rest args)
  (let ((c (or (car args) 1))
        (r (or (cadr args) 1))
        (start-line (save-excursion
                      (goto-char (window-start))
                      (line-number-at-pos))))
    (goto-char (point-min))
    (forward-line (- (+ r start-line) 2))
    (sage-shell:-bol)
    (forward-char (1- c))))

(defun sage-shell:-cursor-down (_proc &rest args)
  (let ((n (or (car args) 1)))
    (sage-shell:-down n)))

(defun sage-shell:-cursor-up (_proc &rest args)
  (let ((n (or (car args) 1)))
    (sage-shell:-down (- n))))

(defun sage-shell:-cursor-back (_proc &rest args)
  (let* ((n (or (car args) 1))
         (bol (sage-shell:line-beginning-position))
         (to (max bol (- (point) n))))
    (goto-char to)))

(defun sage-shell:-cursor-forward (_proc &rest args)
  (let* ((inhibit-field-text-motion t)
         (n (or (car args) 1))
         (le (line-end-position))
         (to (min le (+ (point) n))))
    (goto-char to)))

(defun sage-shell:-current-column ()
  "Return the current column."
  (1+ (- (current-column)
         (save-excursion
           (sage-shell:-bol)
           (current-column)))))

(defun sage-shell:-current-row ()
  "Return the current row."
  (let ((start-line (save-excursion
                      (goto-char (window-start))
                      (line-number-at-pos))))
    (1+ (- (line-number-at-pos) start-line))))

(defvar sage-shell:-report-cursor-pos-p t)
(make-variable-buffer-local 'sage-shell:-report-cursor-pos-p)

(defun sage-shell:-report-cursor-pos (proc &rest args)
  (let ((arg (car args)))
    (cond ((equal arg 6)
           (sage-shell:after-output-finished
             (with-current-buffer sage-shell:process-buffer
               (when (and (save-excursion
                            (forward-line 0)
                            (looking-at sage-shell:-prompt-regexp-no-eol))
                          sage-shell:-report-cursor-pos-p)
                 (let ((row (cond ((= (line-end-position) (point-max))
                                   (process-get proc 'sage-shell:win-height))
                                  (t (sage-shell:-current-row)))))
                   (process-send-string
                    proc
                    (format "\e[%s;%sR"
                            row
                            (sage-shell:-current-column)))))))))))

(defun sage-shell:-delete-to-end-of-output (&optional pt)
  "Assuming current point is at output, delete output text from
the point to end of the buffer"
  (let ((inhibit-field-text-motion nil)
        (inhibit-read-only t)
        (pt (or pt (point))))
    (save-excursion
      (goto-char pt)
      (delete-region pt (line-end-position))
      (while (progn (forward-line 1)
                    (and (sage-shell:-at-output-p (point))
                         (not (eobp))))
        (delete-region (line-beginning-position) (line-end-position))))))

(defun sage-shell:-delete-to-end-of-line (&optional pt)
  (let ((inhibit-field-text-motion nil)
        (inhibit-read-only t))
    (save-excursion
      (when pt
        (goto-char pt))
      (delete-region (point) (line-end-position)))))

(defun sage-shell:-delete-line (_proc &rest args)
  (when (sage-shell:-at-output-p (point))
    (let ((n (car args))
          (inhibit-read-only t)
          (start-column (current-column)))
      (cond ((or (null n) (equal n 0))
             (sage-shell:-delete-to-end-of-line))
            ((equal n 1)
             (delete-region
              (sage-shell:line-beginning-position)
              (point)))
            ((equal n 2)
             (sage-shell:-delete-to-end-of-line
              (sage-shell:line-beginning-position))))
      (move-to-column start-column t))))

(defun sage-shell:-at-output-p (pt)
  "Return non-nil if the pt is on output."
  (eq (plist-get (text-properties-at pt) 'field) 'output))

(defun sage-shell:-delete-display (_proc &rest args)
  ;; Don't delete user's input.
  (when (sage-shell:-at-output-p (point))
    (let ((n (car args))
          (inhibit-read-only t))
      (cond ((or (null n) (equal n 0))
             (sage-shell:-delete-to-end-of-output))
            ((equal n 1)
             (delete-region (point-min) (point)))
            (t
             (sage-shell:-delete-to-end-of-output (point-min)))))))

;; In recent version comint.el,
;; `comint-redirect-original-filter-function` is removed.
(defvar sage-shell:comint-redirect-original-filter-function nil)

;; Ansi escapse sequences may be divived by Emacs
(defvar sage-shell:-pending-outputs nil)
(make-variable-buffer-local 'sage-shell:-pending-outputs)


(defun sage-shell:-psh-to-pending-out (string)
  "Push string to sage-shell:-pending-outputs if necessary and
return string for output."
  (let ((pending-outputs
         (concat (mapconcat #'identity sage-shell:-pending-outputs "")
                 string)))
    (cond ((string-match-p
            (rx (or (and "[" (0+ (or num ";" "?"))) "") eol)
            pending-outputs)
           (push string sage-shell:-pending-outputs)
           ;; Return the empty and wait for next output
           "")
          (t (setq sage-shell:-pending-outputs nil)
             pending-outputs))))

(defsubst sage-shell:-drop-escape-seq-and-blank (str)
  (setq str (replace-regexp-in-string sage-shell:-ansi-escape-regexp
                                      "" str nil t))
  (replace-regexp-in-string (rx (or space "\n")) "" str nil t))

(defun sage-shell:output-filter (process string)
  (setq string (sage-shell:-ansi-escape-filter-out string))
  (setq string (sage-shell:-convert-to-ascii-banner string))
  (unless (string= string "")
    (let ((oprocbuf (process-buffer process)))
      (sage-shell:with-current-buffer-safe (and string oprocbuf)
        (let ((win (get-buffer-window (process-buffer process))))
          (sage-shell:with-selected-window-if-possible win
            (setq string (sage-shell:-psh-to-pending-out string))
            (sage-shell:output-filter-no-rdct process string))
          (when sage-shell:output-finished-p
            (when sage-shell:scroll-to-the-bottom
              (comint-postoutput-scroll-to-bottom string))
            (let ((str-dropped (sage-shell:-drop-escape-seq-and-blank string)))
              (unless (string= str-dropped "")
                (setq buffer-undo-list nil)))
            (sage-shell:run-hook-once
             'sage-shell:output-filter-finished-hook)))))))

(defvar sage-shell:redirect-restore-filter-p t)
(make-variable-buffer-local 'sage-shell:redirect-restore-filter-p)

(defvar sage-shell:attach-file-reloading-regexp
  (rx bol "### reloading attached file " (1+ nonl) "modified at "
      (1+ (or num ":"))  " ###"))

(defun sage-shell:output-filter-no-rdct (process string)
  ;; Insert STRING
  (let ((inhibit-read-only t)
        (saved-point (copy-marker (point) t))
        (inhibit-modification-hooks t))

    ;; We temporarily remove any buffer narrowing, in case the
    ;; process mark is outside of the restriction
    (save-restriction
      (widen)

      (goto-char (process-mark process))
      (sage-shell:run-hook-once
       'sage-shell:-pre-output-filter-hook)
      (set-marker comint-last-output-start (point))

      ;; insert-before-markers is a bad thing. XXX
      ;; Luckily we don't have to use it any more, we use
      ;; window-point-insertion-type instead.
      (sage-shell:-insert-and-handle-ansi-escape process string)

      ;; Advance process-mark
      (set-marker (process-mark process) (point))

      (sage-shell:run-hook-once 'sage-shell:-post-output-filter-hook)

      (unless comint-inhibit-carriage-motion
        ;; Interpret any carriage motion characters (newline, backspace)
        (comint-carriage-motion comint-last-output-start (point)))

      (goto-char saved-point)
      (run-hook-with-args 'comint-output-filter-functions string)
      (set-marker saved-point (point))

      (goto-char (process-mark process)) ; in case a filter moved it

      (when (let ((bol (sage-shell:line-beginning-position)))
              (string-match-p
               sage-shell:output-finished-regexp
               (buffer-substring-no-properties
                bol (min (+ bol 10) (line-end-position)))))
        (setq sage-shell:output-finished-p t))

      (when (and (bolp)
                 (string-match-p
                  sage-shell:attach-file-reloading-regexp
                  (buffer-substring
                   comint-last-output-start
                   (point))))
        (sage-shell:clear-command-cache)
        (sage-shell:output-filter process "sage: "))

      (add-text-properties comint-last-output-start
                           (process-mark process)
                           '(front-sticky
                             (field inhibit-line-move-field-capture)
                             rear-nonsticky t
                             field output
                             inhibit-line-move-field-capture t))
      ;; Comment out output if the syntax of a line does not looks like
      ;; python syntax.
      (sage-shell:comment-out-output)

      (when sage-shell:output-finished-p
        ;; create links in the output buffer.
        (when sage-shell:make-error-link-p
          (sage-shell:make-error-links comint-last-input-end (point)))
        (sage-shell-pdb:comint-output-filter-function)

        ;; Delete whitespaces from the end of line to point-max
        (when sage-shell:use-prompt-toolkit
          (save-excursion
            (goto-char (process-mark process))
            (let ((inhibit-field-text-motion t))
              (end-of-line))
            (let ((str (buffer-substring-no-properties
                        (point)
                        (point-max))))
              (when (string= (sage-shell:trim-left str) "")
                (delete-region (point) (point-max)))))))

      ;; sage-shell:output-filter-finished-hook may change the current buffer.
      (with-current-buffer (process-buffer process)
        (goto-char (process-mark process))
        (when (and sage-shell:output-finished-p
                   (null sage-shell:use-prompt-toolkit))
          (sage-shell-indent:insert-whitespace)))
      (goto-char saved-point))))

(defun sage-shell:highlight-prompt1 (prompt-start prompt-end)
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (when comint-prompt-read-only
      (or (= (point-min) prompt-start)
          (get-text-property (1- prompt-start) 'read-only)
          (put-text-property
           (1- prompt-start) prompt-start 'read-only 'fence))
      (add-text-properties
       prompt-start prompt-end
       '(read-only t rear-nonsticky t front-sticky (read-only) field output
                   face comint-highlight-prompt)))))

(defun sage-shell:-delete-output (pt)
  "Delete region between pt and process-mark"
  (let ((proc (get-buffer-process (current-buffer)))
        (replacement nil)
        (inhibit-read-only t))
    (save-excursion
      (let ((pmark (progn (goto-char (process-mark proc))
                          (forward-line 0)
                          (point-marker))))
        (dolist (ov (overlays-in pt pmark))
          (when (overlay-get ov 'sage-shell-view)
            (delete-overlay ov)))
        (delete-region pt pmark)
        (goto-char (process-mark proc))
        (setq replacement (buffer-substring pmark (point)))
        (delete-region pmark (point))))
    (sage-shell:clear-completion-sync-cached)
    ;; Output message and put back prompt
    (let ((sage-shell:output-filter-finished-hook nil))
      (sage-shell:prepare-for-send)
      (sage-shell:output-filter proc replacement))))

(defun sage-shell:clear-current-buffer ()
  "Delete all output in the current buffer. This does not delete
the last prompt."
  (interactive)
  (sage-shell:-delete-output (point-min)))

(defun sage-shell:delete-output ()
  "Delete all output from interpreter since last input.
Does not delete the prompt. If `sage-shell:use-prompt-toolkit' is
non-nil, then this function may not delete lines which match
sage-shell:-prompt-regexp-no-eol."
  (interactive)
  (let ((bol (sage-shell:line-beginning-position))
        (out-start (sage-shell:-last-input-start-maybe)))
    (when (< out-start bol)
      (sage-shell:-delete-output out-start))))

(defmacro sage-shell:font-lock-when-sage-line (&rest forms)
  `(when (save-match-data
           (save-excursion
             (forward-line 0)
             (looking-at sage-shell:prompt-regexp)))
     ,@forms))

(defmacro sage-shell:-define-font-lock-matcher (name keywords)
  (declare (indent 1))
  `(defun ,name (lim)
     (sage-shell:font-lock-when-sage-line
      (let ((match
             (re-search-forward
              (rx symbol-start
                  (or ,@keywords) symbol-end)
              lim t)))
        (sage-shell:awhen match
          (let ((chbf (char-before (match-beginning 0))))
            (if (null chbf)
                it
              (and (/= chbf ?.) it))))))))

(sage-shell:-define-font-lock-matcher sage-shell:font-lock-matcher-keywords
  ("and" "del" "from" "not" "while" "as" "elif"
   "global" "or" "with" "assert" "else" "if" "pass" "yield"
   "break" "except" "import" "class" "in" "raise" "continue"
   "finally" "is" "return" "def" "for" "lambda"
   "try"
   ;; Python 2:
   "print" "exec"
   ;; Python 3: False, None, and True are listed as keywords on the
   ;; Python 3 documentation, but since they also qualify as
   ;; constants they are fontified like that in order to keep
   ;; font-lock consistent between Python versions.
   "nonlocal"
   ;; Extra:
   "self"))

(sage-shell:-define-font-lock-matcher sage-shell:font-lock-matcher-builtin
  ("abs" "all" "any" "bin" "bool" "callable" "chr" "classmethod"
   "compile" "complex" "delattr" "dict" "dir" "divmod" "enumerate"
   "eval" "filter" "float" "format" "frozenset" "getattr" "globals"
   "hasattr" "hash" "help" "hex" "id" "input" "int" "isinstance"
   "issubclass" "iter" "len" "list" "locals" "map" "max" "memoryview"
   "min" "next" "object" "oct" "open" "ord" "pow" "print" "property"
   "range" "repr" "reversed" "round" "set" "setattr" "slice" "sorted"
   "staticmethod" "str" "sum" "super" "tuple" "type" "vars" "zip"
   "__import__"
   ;; Python 2:
   "basestring" "cmp" "execfile" "file" "long" "raw_input" "reduce"
   "reload" "unichr" "unicode" "xrange" "apply" "buffer" "coerce"
   "intern"
   ;; Python 3:
   "ascii" "bytearray" "bytes" "exec"
   ;; Extra:
   "__all__" "__doc__" "__name__" "__package__"))

(defun sage-shell:font-lock-matcher-def (lim)
  (sage-shell:font-lock-when-sage-line
   (re-search-forward (rx symbol-start "def" (1+ space)
                          (group (1+ (or word ?_))))
                      lim t)))

(defun sage-shell:font-lock-matcher-class (lim)
  (sage-shell:font-lock-when-sage-line
   (re-search-forward
    (rx symbol-start "class" (1+ space) (group (1+ (or word ?_))))
    lim t)))

(defun sage-shell:font-lock-matcher-prompt (lim)
  (when (re-search-forward
         sage-shell:-prompt-regexp-no-eol
         lim t)
    (sage-shell:highlight-prompt1
     (match-beginning 0)
     (match-end 0))))

(defvar sage-shell:font-lock-keywords
  ;; Keywords
  `((sage-shell:font-lock-matcher-keywords . font-lock-keyword-face)
    ;; Exceptions
    (,(rx symbol-start
          (or "ArithmeticError" "AssertionError" "AttributeError"
              "BaseException" "BufferError" "BytesWarning" "DeprecationWarning"
              "EOFError" "EnvironmentError" "Exception" "FloatingPointError"
              "FutureWarning" "GeneratorExit" "IOError" "ImportError"
              "ImportWarning" "IndentationError" "IndexError" "KeyError"
              "KeyboardInterrupt" "LookupError" "MemoryError" "NameError"
              "NotImplementedError" "OSError" "OverflowError"
              "PendingDeprecationWarning" "ReferenceError" "RuntimeError"
              "RuntimeWarning" "StandardError" "StopIteration" "SyntaxError"
              "SyntaxWarning" "SystemError" "SystemExit" "TabError" "TypeError"
              "UnboundLocalError" "UnicodeDecodeError" "UnicodeEncodeError"
              "UnicodeError" "UnicodeTranslateError" "UnicodeWarning"
              "UserWarning" "ValueError" "Warning" "ZeroDivisionError")
          symbol-end) . font-lock-warning-face)
    ;; functions
    (sage-shell:font-lock-matcher-def (1 font-lock-function-name-face))
    ;; classes
    (sage-shell:font-lock-matcher-class (1 font-lock-type-face))
    ;; Constants
    (,(rx symbol-start
          (or "Ellipsis" "False" "None" "NotImplemented" "True" "__debug__")
          symbol-end) . font-lock-constant-face)
    ;; Decorators.
    (,(rx line-start (* (any " \t")) (group "@" (1+ (or word ?_))
                                            (0+ "." (1+ (or word ?_)))))
     (1 font-lock-type-face))
    ;; Builtin Exceptions
    (,(rx symbol-start
          (or
           "ArithmeticError" "AssertionError" "AttributeError" "BaseException"
           "DeprecationWarning" "EOFError" "EnvironmentError" "Exception"
           "FloatingPointError" "FutureWarning" "GeneratorExit" "IOError"
           "ImportError" "ImportWarning" "IndexError" "KeyError"
           "KeyboardInterrupt" "LookupError" "MemoryError" "NameError"
           "NotImplementedError" "OSError" "OverflowError"
           "PendingDeprecationWarning" "ReferenceError" "RuntimeError"
           "RuntimeWarning" "StopIteration" "SyntaxError" "SyntaxWarning"
           "SystemError" "SystemExit" "TypeError" "UnboundLocalError"
           "UnicodeDecodeError" "UnicodeEncodeError" "UnicodeError"
           "UnicodeTranslateError" "UnicodeWarning" "UserWarning" "VMSError"
           "ValueError" "Warning" "WindowsError" "ZeroDivisionError"
           ;; Python 2:
           "StandardError"
           ;; Python 3:
           "BufferError" "BytesWarning" "IndentationError" "ResourceWarning"
           "TabError")
          symbol-end) . font-lock-type-face)
    ;; Builtins
    (sage-shell:font-lock-matcher-builtin . font-lock-builtin-face)
    ;; Prompts
    (sage-shell:font-lock-matcher-prompt . 'comint-highlight-prompt)))

(defvar sage-shell:redirect-last-point nil)


(defsubst sage-shell:-redirect-finished-regexp (msg_id)
  (cond ((string= msg_id "")
         sage-shell:output-finished-regexp)
        (t (rx-to-string
            `(and bol
                  ,(sage-shell:-rdct-msg-id-end msg_id)
                  "\n"
                  ,sage-shell:output-finished-regexp-rx)))))

(defun sage-shell:redirect-filter (process input-string)
  (when process

    (let* ((proc-buf (process-buffer process))
           (msg-id (buffer-local-value 'sage-shell:-redirection-msg-id
                                       proc-buf))
           (inhibit-modification-hooks t))
      (with-current-buffer proc-buf
        (setq input-string (sage-shell:-psh-to-pending-out input-string))
        (let ((out-buf comint-redirect-output-buffer)
              (f-regexp (sage-shell:-redirect-finished-regexp
                         sage-shell:-redirection-msg-id)))
          (set-buffer out-buf)
          ;; Send output to all registered buffers
          ;; Go to the end of the buffer
          (goto-char (point-max))
          ;; Insert the output
          (let ((inhibit-read-only t)
                (view-read-only nil))
            (sage-shell:-insert-and-handle-ansi-escape
             process input-string))

          ;; If we see the prompt, tidy up
          (when (save-excursion
                  (sage-shell:aif sage-shell:redirect-last-point
                      (progn (goto-char it)
                             (forward-line -1))
                    (goto-char (point-min)))
                  (re-search-forward f-regexp nil t))
            (replace-match "")
            (when (and msg-id
                       (not (string= msg-id "")))
              (let ((msg-id-start (sage-shell:-rdct-msg-id-start
                                   msg-id)))
                (save-excursion
                  (goto-char (point-min))
                  (when (re-search-forward
                         (rx-to-string `(and bol ,msg-id-start "\n")) nil t)
                    (delete-region (point-min) (match-end 0))))))
            (set-buffer proc-buf)
            (sage-shell:redirect-cleanup)
            (sage-shell:run-hook-once
             'sage-shell:redirect-filter-finished-hook))
          ;; sage-shell:redirect-filter-finished-hook may change the current
          ;; buffer
          (with-current-buffer proc-buf
            (setq sage-shell:redirect-last-point (point))))))))

(cl-defun sage-shell:redirect-cleanup ()
  (when sage-shell:redirect-restore-filter-p
    (set-process-filter (get-buffer-process (current-buffer))
                        sage-shell:comint-redirect-original-filter-function))

  ;; Set the completed flag
  (setq comint-redirect-completed t))

(defun sage-shell:-new-rdct-msg-id ()
  (format "emacs_sage_shell_msg_id%s"
          (md5 (cl-loop for i in (current-time)
                        concat (format "%d" i)))))

(defun sage-shell:escepe-string (s)
  (setq s (replace-regexp-in-string (rx "\\") "\\\\" s t t))
  (setq s (replace-regexp-in-string (rx "\n") "\\n" s t t))
  (replace-regexp-in-string (rx "\"") "\\\"" s t t))

(cl-defun sage-shell:redirect-setup
    (output-buffer process raw &optional
                   (filter 'sage-shell:redirect-filter))
  (let* ((process-buffer (if (processp process)
                             (process-buffer process)
                           process))
         (proc (get-buffer-process process-buffer)))
    ;; Change to the process buffer
    (with-current-buffer output-buffer
      (setq sage-shell:process-buffer process-buffer))
    (with-current-buffer process-buffer
      ;; Set up for redirection
      (setq sage-shell:redirect-last-point nil)
      (setq sage-shell:-pending-outputs nil)
      (sage-shell:setq-local
       sage-shell:-redirection-msg-id
       (if raw
           ""
         (sage-shell:-new-rdct-msg-id)))
      (let ((mode-line-process mode-line-process))
        (comint-redirect-setup
         output-buffer
         (current-buffer)                ; Comint Buffer
         comint-redirect-finished-regexp ; Finished Regexp
         nil))                           ; Echo input

      (when filter
        ;; Save the old filter
        (setq sage-shell:comint-redirect-original-filter-function
              (process-filter proc))
        (set-process-filter proc filter)))))

(defun sage-shell:comint-send-input (&optional no-newline artificial)
  "This function is almost same as `comint-send-input'. But this
function does not highlight the input."
  (interactive)
  (sage-shell:prepare-for-send)
  (comint-send-input no-newline artificial)
  (add-text-properties comint-last-input-start
                       comint-last-input-end
                       '(font-lock-face nil)))


;;; sage-shell-indent
(defvar sage-shell:prompt1-regexp (rx line-start (1+ (or "sage:" "sage0:"))))
(defvar sage-shell:prompt2-regexp (rx line-start (1+ "....:")) )

(defun sage-shell:send-line-to-indenting-buffer-and-indent (line)
  (save-excursion
    (forward-line 0)
    (let ((after-prompt1 (looking-at sage-shell:prompt1-regexp))
          (after-prompt2 (looking-at sage-shell:prompt2-regexp)))
      (with-current-buffer (sage-shell-indent:get-indenting-buffer)
        (when after-prompt1
          (erase-buffer))
        (when (or after-prompt1 after-prompt2)
          (if (string= (make-string (length line) (string-to-char " "))
                       line)
              (delete-region (point-at-bol) (point)))
          (delete-region (point-at-bol) (point))
          (insert line)
          ;; If line contains triple quotes or top-level return statement, the
          ;; indent function raises an error.
          (if (or (string-match (rx (repeat 3 (or"'" "\""))) line)
                  (and after-prompt1
                       (string-match
                        (rx bol (0+ whitespace) "return" symbol-end) line)))
              (newline)
            (newline-and-indent)))))))

(defun sage-shell:prepare-for-send ()
  (sage-shell:wait-for-redirection-to-complete)
  (setq sage-shell:output-finished-p nil))

(defun sage-shell-update-sage-commands-p (line)
  (string-match (rx symbol-start (or "import" "reload" "attach" "load")
                    symbol-end) line))

(defvar sage-shell:clear-commands-regexp
  (rx bol "clear" (zero-or-more space) eol))

(defvar sage-shell:-pre-output-filter-hook nil)
(make-variable-buffer-local 'sage-shell:-pre-output-filter-hook)
(defvar sage-shell:-post-output-filter-hook nil
  "Unlike `sage-shell:output-filter-finished-hook', the filter function runs
this hook after inserting string.")
(make-variable-buffer-local 'sage-shell:-post-output-filter-hook)

(defun sage-shell:-current-line1 ()
  (save-excursion
    (let ((inhibit-field-text-motion t))
      (end-of-line))
    (cond ((sage-shell:-at-output-p (point))
           (buffer-substring-no-properties
            (sage-shell:-bol)
            (line-end-position)))
          (t (let ((inhibit-field-text-motion nil))
               (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))))))

(defun sage-shell:-current-line (proc-mark &optional pos)
  "Return the command line at POS in the Sage shell.
In case of multiple lines, it returns a list of
lines beg end"
  (save-excursion
    (when pos
      (goto-char pos))
    (let ((line nil))
      (cond
       ;; Multiple lines
       ((and (<= proc-mark (point))
             (progn (setq line (buffer-substring-no-properties
                                proc-mark
                                (line-end-position)))
                    (string-match-p (rx "\n") line)))
        (list (split-string line "\n")
              proc-mark (line-end-position)))
       ((progn
          (beginning-of-line)
          (or
           ;; The last line in the buffer
           (save-excursion (end-of-line) (eobp))
           ;; readline
           (null sage-shell:use-prompt-toolkit)
           ;; multiple-input
           (looking-at-p (rx (0+ nonl) ":" (0+ whitespace) eol))))
        (sage-shell:-current-line1))
       (t (let ((line (sage-shell:-current-line1)))
            (forward-line 1)
            (while (looking-at-p sage-shell:prompt2-regexp)
              (setq line (concat line (sage-shell:-current-line1)))
              (forward-line 1))
            line))))))

(defun sage-shell:-ring-push-if-new (ring item)
  (unless (or (ring-empty-p ring)
              (equal (ring-ref ring 0) item))
    (ring-insert ring item)))

;; This function has many side effects:
;; * Set `sage-shell:input-ring-index'.
;; * Set `sage-shell:output-finished-p'.
;; * If current line is like '****?' then pop to the help buffer.
;; * Send current line to indenting buffer.
;; * (comint-send-input)
;; * change default-directory

(defsubst sage-shell:-cpaste-lines (lines)
  (append (cons "%cpaste" lines) (list "--")))

(defun sage-shell:send-input ()
  "Send current line to Sage process. "
  (interactive)
  (let ((process (get-buffer-process (current-buffer))))
    (when (process-live-p process)
      (let ((line (sage-shell:-current-line (process-mark process)))
            (inhibit-read-only t))
        (cond ((listp line)
               (let ((lines (car line)))
                 (delete-region (cadr line) (cl-caddr line))
                 (sage-shell:-send--lines-internal
                  (sage-shell:-cpaste-lines lines))))
              (t (sage-shell:-send-input-one-line line)
                 (sage-shell:-clear-cache-in-send-input)))))))

(defun sage-shell:-send-input-one-line (line)
  (let ((inhibit-read-only t)
        (at-tl-in-sage-p (sage-shell:at-top-level-and-in-sage-p)))

    ;; If we're currently completing, stop.  We're definitely done
    ;; completing, and by sending the input, we might cause side effects
    ;; that will confuse the code running in the completion
    ;; post-command-hook.
    (when (and (fboundp 'completion-in-region-mode)
               (boundp 'completion-in-region-mode)
               completion-in-region-mode)
      (completion-in-region-mode -1))

    ;; If current line contains %gap, gap.console(), gap.interact(), %gp, ...
    ;; then update the command list.
    (sage-shell:awhen (sage-shell-cpl:switch-to-another-interface-p line)
      (unless (sage-shell-cpl:get-cmd-lst it)
        (sage-shell-interfaces:update-cmd-lst it)))

    ;; Since comint-send-input sets comint-input-ring-index to nil,
    ;; restore its value
    (setq sage-shell:input-ring-index comint-input-ring-index)

    (cond
     ;; if current line is ***? and current interface is sage then
     ;; show help or find-file-read-only source file.
     ((and at-tl-in-sage-p
           (string-match
            (rx (or (and bol (group (1+ nonl)) "??" (0+ blank) eol)
                    (and bol (0+ blank) "??" (group (1+ nonl)) eol)))
            line))
      (sage-shell:-ring-push-if-new comint-input-ring line)
      (sage-shell:find-source-in-view-mode
       (or (match-string-no-properties 1 line)
           (match-string-no-properties 2 line)))
      (sage-shell:send-blank-line))
     ((and at-tl-in-sage-p
           (string-match
            (rx (or (and bol (group (1+ nonl)) "?" (0+ blank) eol)
                    (and bol (0+ blank) "?" (group (1+ nonl)) eol)))
            line))
      (sage-shell:-ring-push-if-new comint-input-ring line)
      (sage-shell-help:describe-symbol
       (or (match-string-no-properties 1 line)
           (match-string-no-properties 2 line)))
      (sage-shell:send-blank-line))
     ((and at-tl-in-sage-p
           (string-match (rx bol (zero-or-more blank)
                             "help"
                             (zero-or-more blank)
                             "("
                             (group (1+ nonl))
                             (zero-or-more blank) ")"
                             (zero-or-more blank)
                             eol) line))
      (sage-shell:-ring-push-if-new comint-input-ring line)
      (sage-shell-help:describe-symbol
       (match-string-no-properties 1 line) "help(%s)")
      (sage-shell:send-blank-line))

     ;; send current line to process normally
     (t
      ;; FIXME: Run sage-shell:send-line-to-indenting-buffer-and-indent
      ;; if sage-shell:use-prompt-toolkit is non-nil.
      ;; To do it, disable auto-indent.
      (unless sage-shell:use-prompt-toolkit
        (sage-shell:send-line-to-indenting-buffer-and-indent line))
      (cond (sage-shell:use-prompt-toolkit
             (let* ((proc (get-buffer-process sage-shell:process-buffer))
                    (proc-pos (copy-marker
                               (marker-position (process-mark proc))
                               t))
                    (line-end (copy-marker
                               (progn (goto-char proc-pos)
                                      (line-end-position))
                               t)))
               (add-hook 'sage-shell:-pre-output-filter-hook
                         (lambda () (let ((inhibit-redisplay t))
                                  (delete-region proc-pos line-end))))
               (let ((comint-input-sender
                      (lambda (proc _str) (process-send-string proc line))))
                 (sage-shell:comint-send-input t)
                 (process-send-string proc ""))))
            (t (sage-shell:comint-send-input)))))

    ;; If current line contains from ... import *, then update sage commands
    (when (sage-shell-update-sage-commands-p line)
      (sage-shell:update-sage-commands))
    (when at-tl-in-sage-p
      ;; change default-directory if needed
      (cond ((and
              (string-match (rx bol (zero-or-more blank)
                                (zero-or-one "%")
                                "cd" (zero-or-more blank)
                                (group (one-or-more (regexp "[^\n \t]"))))
                            line)
              (file-exists-p (match-string 1 line)))
             (ignore-errors
               (cd (match-string 1 line))))
            ((string-match (rx bol (zero-or-more blank)
                               (zero-or-one "%")
                               "cd" (zero-or-more blank)
                               eol)
                           line)
             (cd "~")))

      (sage-shell-cpl:-add-to-cands-in-cur-session line)
      (when (string-match sage-shell:clear-commands-regexp line)
        (add-hook 'sage-shell:output-filter-finished-hook
                  #'sage-shell:clear-current-buffer)))))

(defun sage-shell:-clear-cache-in-send-input ()
  (sage-shell:-inputs-outputs-clear-cache)
  (sage-shell:-clear-eldoc-cache)
  (sage-shell-cpl:-clear-argspec-cache))

(defun sage-shell-cpl:-add-to-cands-in-cur-session (line)
  (let ((regexp-asg
         (rx bol
             (group
              (0+ (and symbol-start
                       (1+ (or "_" alnum))
                       (and (0+ whitespace) "," (0+ whitespace))))
              (and symbol-start (1+ (or "_" alnum)) symbol-end))
             (0+ whitespace) "=" (0+ whitespace)
             symbol-start))
        (regexp-def-or-class
         (rx bol
             symbol-start
             (or "def" "class")
             symbol-end
             (1+ whitespace)
             (group (1+ (or "_" alnum))))))

    (cond ((string-match regexp-asg line)
           ;; When assignment is performed, add vars to the cached command
           ;; list.
           (let ((str-s (split-string (match-string 1 line)
                                      (rx (1+ (or "," " "))))))
             (setq sage-shell-cpl:-cands-in-current-session
                   (append str-s
                           sage-shell-cpl:-cands-in-current-session))))
          ((string-match regexp-def-or-class line)
           (let ((name (match-string 1 line)))
             (setq sage-shell-cpl:-cands-in-current-session
                   (cons name sage-shell-cpl:-cands-in-current-session)))))))


(defun sage-shell:send-blank-line ()
  (cond (sage-shell:use-prompt-toolkit
         (let ((win (get-buffer-window sage-shell:process-buffer)))
           (sage-shell-edit:exec-command-base
            :command "" :insert-command-p t
            :callback
            (lambda ()
              (sage-shell:with-selected-window-if-possible win
                (goto-char (process-mark (get-buffer-process
                                          sage-shell:process-buffer))))))))
        (t (sage-shell:send-blank-line-readline))))

(defun sage-shell:send-blank-line-readline ()
  (with-current-buffer sage-shell:process-buffer
    (let ((comint-input-sender
           (lambda (proc _str) (comint-simple-send proc "")))
          (win (get-buffer-window sage-shell:process-buffer)))
      (sage-shell:with-selected-window-if-possible win
        (sage-shell:comint-send-input)))))

(defun sage-shell:at-top-level-p ()
  (save-excursion
    (forward-line 0)
    (or (looking-at sage-shell:prompt1-regexp)
        (cl-loop for i in sage-shell-interfaces:other-interfaces
                 thereis (looking-at (format "^%s: " i))))))

(cl-defun sage-shell:at-top-level-and-in-sage-p
    (&optional (cur-intf (sage-shell-interfaces:current-interface)))
  "returns non nil if and only if current interface is sage and
the current line is not in a block."
  (and (string= cur-intf "sage")
       (save-excursion
         (forward-line 0)
         (looking-at sage-shell:prompt1-regexp))))

(defun sage-shell-indent:get-indenting-buffer ()
  "Return a temporary buffer set in python-mode. Create one if necessary."
  (let ((buf (get-buffer-create sage-shell-indent:indenting-buffer-name)))
    (set-buffer buf)
    (unless (eq major-mode 'python-mode)
      (python-mode)
      (if (boundp 'python-guess-indent)
          (set (make-local-variable 'python-guess-indent) nil))
      (if (boundp 'python-indent-guess-indent-offset)
          (set (make-local-variable 'python-indent-guess-indent-offset) nil)))
    buf))

(defun sage-shell-indent:insert-whitespace ()
  "Insert whitespaces if sage-shell:prompt2-regexp regexp
matches last process output."
  (when (save-excursion
          (forward-line 0)
          (looking-at sage-shell:prompt2-regexp))
    (let ((indent-str nil))
      (with-current-buffer (sage-shell-indent:get-indenting-buffer)
        (setq indent-str (buffer-substring (point-at-bol) (point))))
      (when (get-buffer-process sage-shell:process-buffer)
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert indent-str)))))


(defun sage-shell-indent:indent-line ()
  " indent-line function using indent-line function in
python-mode"
  (let ((this-command 'indent-for-tab-command)
        (last-command (if (eq last-command 'sage-shell-tab-command)
                          'indent-for-tab-command
                        last-command))
        (line (buffer-substring-no-properties
               (save-excursion (comint-bol) (point))
               (line-end-position)))
        (col (- (point) (save-excursion (comint-bol) (point))))
        col1 line1)
    (with-current-buffer (sage-shell-indent:get-indenting-buffer)
      (delete-region (line-beginning-position)
                     (line-end-position))
      (insert line)
      (beginning-of-line) (goto-char (+ (point) col))
      ;; use indent function of python-mode
      (indent-for-tab-command)
      (setq col1  (- (point) (line-beginning-position))
            line1 (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
    (delete-region (line-beginning-position) (line-end-position))
    (save-excursion (insert line1))
    (comint-bol) (goto-char (+ (point) col1))))


;;; sage-shell-help
(defvar sage-shell-help:fontlock-keyword-regexp
  (rx (group (or (and bow (or "Base Class"
                              "Definition"
                              "DEFINITION"
                              "IMPLEMENTATION"
                              "String form"
                              "Docstring"
                              "EXAMPLES"
                              "EXAMPLE"
                              "File"
                              "Loaded File"
                              "Source File"
                              "OUTPUT"
                              "INPUT"
                              "Namespace"
                              "String Form"
                              "TESTS"
                              "Methods defined here"
                              "AUTHORS"
                              "AUTHOR"
                              "Type"
                              "Class Docstring"
                              "Class docstring"
                              "Constructor Docstring"
                              "Constructor information"
                              "ALGORITHM"
                              "Call def"
                              "Call docstring"
                              "Init definition"
                              "Init docstring"
                              "Signature"
                              "Init signature"
                              "OPTIONAL"
                              "REFERENCES"
                              "Bases")
                      ":")
                 (and bol (or "NAME"
                              "FILE"
                              "DATA"
                              "DESCRIPTION"
                              "PACKAGE CONTENTS"
                              "FUNCTIONS"
                              "CLASSES")
                      eol)
                 (and bow "sage: ")))))

(defvar sage-shell-help:help-buffer-name "*Sage Document*")

(define-derived-mode sage-shell:help-mode help-mode "Sage-doc"
  "Help mode for Sage"
  (font-lock-mode 1)
  (view-mode 1)
  (font-lock-add-keywords
   nil `((,sage-shell-help:fontlock-keyword-regexp 1 font-lock-keyword-face))))

(sage-shell:define-keys sage-shell:help-mode-map
  "C-c C-d" 'sage-shell:send-doctest
  "C-c C-j" 'sage-shell:send-doctest
  "C-c C-z" 'sage-shell-edit:pop-to-process-buffer)

(defvar sage-shell-help:symbol-not-found-regexp
  "Object `.*?` not found."
  "Regexp that matches Sage's 'symbol not found' warning.")

(defun sage-shell-help:help-buffer-init (symbol)
  (let* ((case-fold-search t)
         (linenum (sage-shell-help:find-symbols-line-num
                   symbol sage-shell:process-buffer)))
    (goto-char (point-min))
    (save-excursion
      (while (re-search-forward (rx bow "sage: ") nil t)
        (replace-match (propertize "sage: " 'read-only t))))
    ;; make button for source file
    (save-excursion
      (while (and (re-search-forward (rx (or (and bol (or "Source File"
                                                          "Loaded File"
                                                          "File") ":"
                                                          (1+ blank))
                                             (and bol "FILE" eol)))
                                     nil t)
                  (re-search-forward "[^ \n\t]+" nil t))
        (let ((fname (match-string 0)))
          (sage-shell-help:file-type-make-button
           (match-beginning 0)
           (match-end 0)
           (if sage-shell:prefer-development-file-p
               (sage-shell:src-version fname)
             fname)
           linenum))))
    (when sage-shell:make-error-link-p
      (sage-shell:make-error-links (point-min) (point-max)))))

(cl-defun sage-shell-help:file-type-make-button
    (beg end file line &optional cont (recenter-arg 0))
  "If cont is a function, then cont will be called after visiting the file."
  (unless (button-at beg)
    (make-text-button
     beg end
     'sage-shell:file file 'sage-shell:line line
     'sage-shell:cont cont
     'sage-shell:buffer (current-buffer)
     'sage-shell:proc-buf sage-shell:process-buffer
     'action `(lambda (button)
                (let* ((linenum (button-get button 'sage-shell:line))
                       (cont (button-get button 'sage-shell:cont))
                       (proc-buf (button-get button 'sage-shell:proc-buf))
                       (file (button-get button 'sage-shell:file))
                       (cbuf (button-get button 'sage-shell:buffer)))
                  (with-selected-window (get-buffer-window cbuf)
                    (find-file-other-window file))
                  (with-current-buffer (get-file-buffer file)
                    (setq sage-shell:process-buffer proc-buf))
                  (select-window (get-buffer-window (get-file-buffer file)))
                  (when linenum
                    (goto-char (point-min))
                    (forward-line (1- linenum))
                    (recenter ,recenter-arg))
                  (when (functionp cont)
                    (funcall cont))))
     'follow-link t)))


(defun sage-shell-help:find-symbols-line-num (symbol process-buffer)
  "return line number"
  (let* ((str (sage-shell:send-command-to-string
               (format "%s(%s)"
                       (sage-shell:py-mod-func "print_source_line")
                       symbol)
               process-buffer)))
    (when (string-match "\\([0-9]+\\)" str)
      (string-to-number (match-string 1 str)))))

(defvar sage-shell-help:help-contents-list nil "help contents list")
(defvar sage-shell-help:help-contents-list-index 0
  "index 0 item is the newest.")

(defun sage-shell-help:make-forward-back-button ()
  (let ((len (length sage-shell-help:help-contents-list))
        (idx sage-shell-help:help-contents-list-index))
    (sage-shell:labels ((insert-btn
                         (text arg)
                         (insert-text-button
                          text
                          'sage-shell:help-history-arg arg
                          'action
                          (lambda (btn)
                            (sage-shell-help:forward-history
                             (button-get btn 'sage-shell:help-history-arg)))
                          'follow-link t))
                        (insert-forward-back-btn
                         (sym)
                         (save-excursion
                           (goto-char (point-max))
                           (newline)
                           (cl-case sym
                             ('forward (insert-btn "[forward]" 1))
                             ('back (insert-btn "[back]" -1))
                             ('both (insert-btn "[back]" -1)
                                    (insert "  ")
                                    (insert-btn "[forward]" 1))))))

      (cond
       ((eq len 1))
       ((and (> idx 0) (< idx (1- len))) (insert-forward-back-btn 'both))
       ((eq idx 0) (insert-forward-back-btn 'back))
       ((eq idx (1- len)) (insert-forward-back-btn 'forward))))))

(defun sage-shell-help:forward-history (arg)
  (interactive "p")
  (let ((inhibit-read-only t)
        (view-read-only nil)
        (lst sage-shell-help:help-contents-list)
        (idx sage-shell-help:help-contents-list-index))
    (erase-buffer)
    ;; insert history contents
    (insert (nth (- idx arg) lst))
    ;; update index
    (setq sage-shell-help:help-contents-list-index
          (- idx arg))
    ;; make button
    (sage-shell-help:make-forward-back-button)
    (goto-char (point-min))))

(defun sage-shell-help:backward-history (arg)
  (interactive "p")
  (sage-shell-help:forward-history (- arg)))

;; remap help-mode-map
(define-key sage-shell:help-mode-map
  [remap help-go-back] 'sage-shell-help:backward-history)
(define-key sage-shell:help-mode-map
  [remap help-go-forward] 'sage-shell-help:forward-history)


(cl-defun sage-shell-help:describe-symbol
    (symbol &optional
            (cmd (format "%s(%%S)" (sage-shell:py-mod-func "print_info"))))
  "Describe symbol, display help buffer and select the window."
  (let* ((buf (get-buffer-create sage-shell-help:help-buffer-name))
         (cmd-str (format cmd symbol))
         (str (sage-shell:send-command-to-string cmd-str))
         (proc (current-buffer)))
    (cond ((string-match sage-shell-help:symbol-not-found-regexp str)
           (message (format "Object %s not found." symbol)))
          (t (let ((inhibit-read-only t)
                   (help-window-select t)
                   (view-read-only nil))
               (with-help-window (buffer-name buf)
                 (with-current-buffer buf
                   (erase-buffer) (sage-shell:help-mode)
                   (setq sage-shell:process-buffer proc)
                   (make-marker)
                   (insert str)
                   (sage-shell-help:help-buffer-init symbol)

                   ;; add to history list
                   (let ((contents (buffer-substring (point-min) (point-max)))
                         (lst sage-shell-help:help-contents-list))
                     (unless (equal (car lst) contents)
                       (setq sage-shell-help:help-contents-list
                             (cons contents lst))))
                   ;; update history index
                   (setq sage-shell-help:help-contents-list-index 0)
                   ;; make forward or backward button
                   (sage-shell-help:make-forward-back-button))))))))

;;; make err link
(defvar sage-shell:make-err-link--line-regexp
  (rx bol (group "/" (repeat 1 255 nonl)
                 (or ".pyc" ".pyx" ".py" ".so")) eow " in"))

(defun sage-shell:make-err-link--fname-conv (filename)
  (cond ((string-match (rx (or ".py" ".pyc") eol) filename)
         (let ((fname (concat (file-name-sans-extension filename) ".py")))
           (if sage-shell:prefer-development-file-p
               (sage-shell:src-version fname)
             fname)))
        ((string-match (rx ".so" eol) filename)
         (sage-shell:src-version filename))
        (t filename)))

(defun sage-shell:research-forward-w-bd (reg bd)
  (when (< (point) bd)
    (re-search-forward reg bd t)))

(defun sage-shell:make-error-links (beg end)
  (save-excursion
    (goto-char beg)
    (cl-loop while (sage-shell:research-forward-w-bd
                    sage-shell:make-err-link--line-regexp end)
             for fbeg = (match-beginning 1)
             for fend = (match-end 1)
             for file-org-name = (match-string 1)
             for filename = (sage-shell:make-err-link--fname-conv
                             file-org-name)
             for linenum = nil
             for cont = nil
             if (or (and (string-match (rx ".so" eol) file-org-name)
                         (prog1 t
                           (forward-line 0)
                           (when (re-search-forward
                                  (rx "in" (1+ space)
                                      (group (1+ (or alnum "_" "-" "."))))
                                  (line-end-position) t)
                             (setq cont (sage-shell:make-error-links--cont
                                         file-org-name (match-string 1))))))
                    (progn
                      (forward-line 1)
                      (and (looking-at (rx bol (1+ whitespace)
                                           (1+ num)))
                           (when (sage-shell:research-forward-w-bd
                                  (rx bol (0+ "-") ">"
                                      (1+ whitespace)
                                      (group (1+ num)))
                                  end)
                             (setq linenum
                                   (string-to-number (match-string 1)))))))
             do
             (sage-shell-help:file-type-make-button
              fbeg fend filename linenum cont nil))))


(defun sage-shell:make-error-links--cont (f-org-name func-name)
  (when (and (sage-shell:awhen (string-match (sage-shell:sage-root) f-org-name)
               (= it 0))
             (string-match (rx "site-packages/" (group "sage" (1+ nonl)))
                           (file-name-sans-extension f-org-name)))
    (let* ((l1 (split-string (match-string 1 f-org-name) "/"))
           (l2 (split-string func-name (rx ".")))
           (func-ls (cl-loop for a1 on l1 for a2 on l2
                             unless a1
                             finally return a2)))
      (lambda ()
        (goto-char (point-min))
        (dolist (a func-ls)
          (re-search-forward
           (rx-to-string
            `(and (or "def" "cdef" "cpdef" "class")
                  (or whitespace
                      (and whitespace
                           (0+ nonl)
                           whitespace))
                  ,a (0+ whitespace) "("))
           nil t))
        (forward-line 0)))))


;; inputs outputs
(defvar sage-shell:list-outputs-buffer "*Sage Outputs*")
(defvar sage-shell:lo-delim "@sage_shell_delim@")
(defvar sage-shell:list-outputs-points nil)

(defun sage-shell:list-outputs ()
  (interactive)
  (let ((b (get-buffer-create sage-shell:list-outputs-buffer))
        (proc-buf sage-shell:process-buffer))
    (with-current-buffer b
      (let ((inhibit-read-only t)
            (view-read-only nil))
        (erase-buffer)
        (sage-shell:list-outputs-mode)
        (setq sage-shell:process-buffer proc-buf)
        (let* ((win (get-buffer-window (pop-to-buffer b)))
               (delim (mapconcat
                       (lambda (_x) "-")
                       (number-sequence 1 (window-width win)) ""))
               (out (sage-shell:-inputs-outputs)))
          (with-current-buffer b
            (save-excursion
              (goto-char (point-min))
              (let ((pts (cl-loop for a in out
                                  with pts = nil
                                  do
                                  (insert a)
                                  (insert delim)
                                  (push (1+ (point)) pts)
                                  finally return (nreverse pts))))
                (setq sage-shell:list-outputs-points
                      (cons 1 (butlast pts)))))))))))


(defvar sage-shell:-inputs-outputs-cached nil)
(defun sage-shell:-inputs-outputs-clear-cache ()
  (setq sage-shell:-inputs-outputs-cached nil))
(make-variable-buffer-local 'sage-shell:-inputs-outputs-cached)
(defun sage-shell:-inputs-outputs ()
  (with-current-buffer sage-shell:process-buffer
    (sage-shell:aif sage-shell:-inputs-outputs-cached
        it
      (when (sage-shell:redirect-and-output-finished-p)
        (setq sage-shell:-inputs-outputs-cached
              (let ((s (sage-shell:send-command-to-string
                        (sage-shell:py-mod-func
                         (format "print_inputs_outputs(%s, '%s', %s)"
                                 (or sage-shell:list-outputs-max-line-num
                                     "None")
                                 sage-shell:lo-delim
                                 (if sage-shell:list-outputs-reversed-order-p
                                     "True"
                                   "False"))))))
                (butlast (split-string s sage-shell:lo-delim))))))))

(defun sage-shell:output-forward (arg)
  (interactive "p")
  (let* ((pt (point))
         (marg (- arg))
         (cr (cl-loop for a on sage-shell:list-outputs-points
                      if (and (<= (car a) pt) (cadr a) (< pt (cadr a)))
                      return a
                      finally return (last sage-shell:list-outputs-points))))
    (let* ((l-cr (length cr))
           (l-pts (length sage-shell:list-outputs-points))
           (dest (cond ((>= arg l-cr) (car (last cr)))
                       ((= arg 0) (car cr))
                       ((> arg 0) (nth arg cr))
                       ((>= marg (- l-pts l-cr)) 1)
                       (t (nth (- l-pts l-cr marg)
                               sage-shell:list-outputs-points)))))
      (goto-char dest))))

(defun sage-shell:output-backward (arg)
  (interactive "p")
  (sage-shell:output-forward (- arg)))

(define-derived-mode sage-shell:list-outputs-mode special-mode
  "Sage Outputs"
  (font-lock-add-keywords
   nil `((,(rx bol (or "In " "Out") "[" (1+ num) "]:") .
          'comint-highlight-prompt))))

(sage-shell:define-keys sage-shell:list-outputs-mode-map
  "n" 'sage-shell:output-forward
  "p" 'sage-shell:output-backward)

(defun sage-shell:-last-input-start-maybe ()
  (cond (sage-shell:use-prompt-toolkit
         (let ((inhibit-field-text-motion t)
               (bol (sage-shell:line-beginning-position)))
           (save-excursion
             (goto-char comint-last-input-end)
             (while (progn
                      (forward-line 0)
                      (and
                       (< (point) bol)
                       (looking-at-p sage-shell:-prompt-regexp-no-eol)
                       (progn (end-of-line)
                              (not (eobp)))))
               (forward-line 1))
             (point))))
        (t comint-last-input-end)))

(defun sage-shell:last-output-beg-end ()
  (sage-shell:with-current-buffer-safe sage-shell:process-buffer
    (save-excursion
      (goto-char (process-mark (get-buffer-process sage-shell:process-buffer)))
      (list (sage-shell:-last-input-start-maybe)
            (1- (sage-shell:line-beginning-position))))))

(defun sage-shell:copy-previous-output-to-kill-ring ()
  "Save the previous visible output to `kill-ring'.
If `sage-shell:use-prompt-toolkit' is non-nil, then the first several
lines which match sage-shell:-prompt-regexp-no-eol are dropped from the output."
  (interactive)
  (sage-shell:with-current-buffer-safe sage-shell:process-buffer
    (kill-new (apply #'buffer-substring-no-properties
                     (sage-shell:last-output-beg-end)))))


;;; sage-shell-interfaces

(defvar sage-shell-interfaces:optional-interfaces
  '("axiom" "gap3" "gnuplot" "kash" "magma"
    "maple" "matlab" "mathematica" "octave"
    "giac")
  "Interfaces not installed by default.")

(defun sage-shell-cpl:interface-trans (interface)
  (cond ((string= interface "pari") "gp")
        (t interface)))

(defun sage-shell-interfaces:current-interface ()
  (save-excursion
    (forward-line 0)
    (let ((int (or (cl-loop for str in sage-shell-interfaces:other-interfaces
                            if (looking-at (concat str ": ")) return str)
                   (when (looking-at sage-shell:prompt-regexp)
                     "sage"))))
      (if int
          (sage-shell-cpl:interface-trans int)))))

;; Define many global variables
(cl-loop for i in (append sage-shell-interfaces:other-interfaces '("sage"))
         do
         (set (intern (format "sage-shell-cpl:%s-info" i))
              (list
               ;; command regexp
               (cons 'cmd-rxp "[a-zA-Z0-9_]+")
               (cons 'var-chars "a-zA-Z0-9_")
               ;; if 'trait_names' has the argument 'verbose' then its message.
               (cons 'verbose nil)
               ;; cache file
               (cons 'cache-file nil)
               ;; name of executable
               (cons 'executable nil))))

(defun sage-shell-interfaces:set (interface &rest attributes-values)
  (when (sage-shell:in
         interface (cons "sage" sage-shell-interfaces:other-interfaces))
    (let ((alist (symbol-value
                  (intern (format "sage-shell-cpl:%s-info" interface)))))
      (cl-loop for (att val) in (sage-shell:group attributes-values)
               do
               (sage-shell:aif (assoc att alist)
                   (setcdr it val)
                 (error (format "No such attribute %S" att)))
               finally return val))))

(defun sage-shell-interfaces:get (interface attribute)
  (when (sage-shell:in interface
                       (cons "sage" sage-shell-interfaces:other-interfaces))
    (let ((alist (symbol-value
                  (intern (format "sage-shell-cpl:%s-info" interface)))))
      (sage-shell:aif (assoc attribute alist)
          (cdr-safe it)
        (error (format "No such attribute %S" attribute))))))

(defun sage-shell-interfaces:update-cmd-lst (itfc)
  "Update command list sync"
  (with-current-buffer sage-shell:process-buffer
    (sage-shell-cpl:set-cmd-lst itfc nil)
    (let ((sexp (sage-shell-cpl:completion-init
                 t :compl-state
                 `((interface . ,itfc)
                   (var-base-name . nil)
                   (types "interface")))))
      (sage-shell-cpl:candidates
       :sexp sexp
       :state `((interface . ,itfc)
                (types "interface")
                (var-base-name . nil)))
      (sage-shell-cpl:get-cmd-lst itfc))))

(defun sage-shell-interfaces:looking-back-var (interface)
  (let ((rgexp (sage-shell-interfaces:get interface 'cmd-rxp))
        (chars (sage-shell-interfaces:get interface 'var-chars)))
    (save-excursion
      (when (and (not (= (skip-chars-backward chars) 0))
                 (looking-at rgexp))
        (point)))))

(defun sage-shell-interfaces:executable-find (int)
  (sage-shell:aif (sage-shell-interfaces:get int 'executable)
      (executable-find it)
    (executable-find int)))

;; Executble of mathematica is math.
(sage-shell-interfaces:set "mathematica" 'executable "math")

;; set verbose message and cache-file name
(cl-loop for itf in '("maple" "maxima")
         do
         (sage-shell-interfaces:set
          itf
          'cache-file
          (expand-file-name (format "%s_commandlist_cache.sobj" itf)
                            sage-shell:dot-sage))
         (sage-shell-interfaces:set
          itf
          'verbose
          (format
           (concat
            "Building %s command completion list (this takes "
            "a few seconds only the first time you do it).\n"
            "To force rebuild later, delete %s.")
           itf
           (sage-shell-interfaces:get itf 'cache-file))))

(sage-shell-interfaces:set
 "magma"
 'cache-file
 (expand-file-name "magma_intrinsic_cache.sobj" sage-shell:dot-sage))

(sage-shell-interfaces:set
 "magma"
 'verbose
 (format
  (concat
   "Creating list of all Magma intrinsics for use in completion. "
   "This takes a few minutes the first time, but is saved to the "
   "file '%s' for future instant use. "
   "Magma may produce errors during this process, which are safe to ignore. "
   "Delete that file to force recreation of this cache. \n"
   "Scanning Magma types ...")
  (sage-shell-interfaces:get "magma" 'cache-file)))


;;; sage-shell-cpl

(defconst sage-shell-cpl-state-keys
  '(
    ;; name of the interface (string)
    interface
    ;; nil or the point of the beggining of completion
    prefix
    ;; nil or the base name of the variable name
    var-base-name
    ;; nil or string.
    module-name
    ;; nil or the function name. Used by eldoc
    in-function-call
    ;; nil or integer. Used by eldoc
    in-function-call-end
    ;; nil or the base name of the function in function call.
    ;; Used by eldoc
    in-function-call-base-name
    ;; In some cases, we need different kinds of candidates.
    ;; For example, candidates which follow "gap." should contain
    ;; gap commands and attributes of a variable gap.
    ;; An element of tyjpes should be equal to one of
    ;; "interface", "attributes", "modules", "vars-in-module", "in-function-call".

    ;; When "modules" in in type and module-name is nil, then candidates shoud
    ;; contain top level modules in sys.path.  If module-name is non-nil, it
    ;; should contain sub-modules in module-name.
    types))

(defvar sage-shell-cpl:current-state nil)

(defun sage-shell-cpl-statep (l)
  (and (listp l)
       (sage-shell-every (lambda (x)
                           (and (listp x)
                                (memq (car x) sage-shell-cpl-state-keys)))
                         l)
       (stringp (assoc-default 'interface l))
       (let-alist l
         (and (sage-shell-every (lambda (x) (or (null x) (integerp x)))
                                (list .in-function-call-end
                                      .prefix))
              (sage-shell-every (lambda (x) (or (null x) (stringp x)))
                                (list .var-base-name
                                      .module-name
                                      .in-function-call
                                      .in-function-call-base-name))
              (listp .types)
              (sage-shell-every (lambda (x) (member x '("interface" "attributes"
                                                    "modules"
                                                    "vars-in-module"
                                                    "in-function-call")))
                                .types)))))
;; For old Emacs (Emacs 24.3 or older)
(defalias 'sage-shell-cpl-state-p #'sage-shell-cpl-statep)

(defun sage-shell:-to-python-dict (alst)
  "nil is converted to None."
  (format "{%s}"
          (mapconcat
           'identity
           (cl-loop for (a . b) in alst
                    collect (format "\"%s\": %s" a
                                    (cond ((or (stringp b)
                                               (numberp b)) (format "%S" b))
                                          ((eq b t) "True")
                                          ((eq b nil) "None")
                                          ((listp b)
                                           (sage-shell:-to-python-list b)))))
           ", ")))

(defun sage-shell:-to-python-list (ls)
  (format "[%s]"
          (mapconcat 'identity
                     (cl-loop for a in ls
                              collect (format "%S" a))
                     ", ")))

(make-variable-buffer-local 'sage-shell-cpl:current-state)


(defun sage-shell-cpl:get (state attribute)
  (sage-shell:aif (assoc attribute state)
      (cdr-safe it)))


(defun sage-shell-cpl:get-current (attribute)
  (sage-shell-cpl:get sage-shell-cpl:current-state
                      attribute))

(defun sage-shell-cpl:set (state &rest attributes-values)
  (cl-loop for (att val) in (sage-shell:group attributes-values)
           do
           (sage-shell:aif (assoc att state)
               (setcdr it val)
             (error (format "No such attribute %S" att)))
           finally return val))

(defun sage-shell-cpl:set-current (&rest attributes-values)
  (apply 'sage-shell-cpl:set sage-shell-cpl:current-state
         attributes-values))

(defun sage-shell-cpl:var-base-name-and-att-start (cur-intf)
  "Returns cons of the base name of the variable and the point of
   beginig of the attribute. For example, if there is a python
   code 'abc.de' and the point is at 'd' or 'e' and 'abc' does
   not call any functions, this returns cons of a string 'abc'
   and the point at 'd', otherwise nil."
  (let ((bol (line-beginning-position))
        (var-chars (sage-shell-interfaces:get cur-intf 'var-chars))
        att-beg base-end)
    (save-excursion
      (skip-chars-backward var-chars bol)
      (setq att-beg (point))
      (when (save-excursion
              (skip-chars-backward " " bol)
              (and (not (bobp))
                   (eq (char-after (1- (point))) ?.)))
        (save-excursion
          (skip-chars-backward " " bol)
          (forward-char -1)
          (skip-chars-backward " " bol)
          (setq base-end (point)))
        (sage-shell:awhen (sage-shell-cpl:base-name-att-beg-rec
                           var-chars bol)
          (let ((base-name (buffer-substring-no-properties it base-end)))
            (unless (or (string= base-name "")
                        ;; when base-name does not call any functions
                        (string-match (rx (or "(" "%"
                                              ;; There exists a possibility
                                              ;; that base-name contains '..'.
                                              (and "." (0+ whitespace)
                                                   ".")))
                                      base-name))
              (cons base-name att-beg))))))))

(defun sage-shell-cpl:base-name-att-beg-rec (var-chars bol)
  (save-excursion
    (skip-chars-backward var-chars bol)
    (if (not (save-excursion
               (skip-chars-backward " ")
               (eq (char-after (1- (point))) ?.)))
        (point)
      (forward-char -1)
      (skip-chars-backward " " bol)
      ;; 93 and 41 are chars of closed parens.
      (if (sage-shell:in (char-after (1- (point))) (list 93 41))
          (when (ignore-errors (backward-list))
            (skip-chars-backward " " bol)
            (sage-shell-cpl:base-name-att-beg-rec var-chars bol))
        (skip-chars-backward " " bol)
        (sage-shell-cpl:base-name-att-beg-rec var-chars bol)))))

(defun sage-shell-cpl:-parse-state-args (cur-intf)
  (let* ((base-and-beg (sage-shell-cpl:var-base-name-and-att-start cur-intf))
         (base-name (car-safe base-and-beg))
         (case-fold-search nil)
         (att-beg (cdr-safe base-and-beg))
         (itfcs sage-shell-interfaces:other-interfaces)
         (intf (unless att-beg
                 (or (sage-shell:in cur-intf itfcs)
                     (sage-shell:aif (sage-shell:-in-func-call-p)
                         (let ((func-name (cl-caddr it)))
                           (when (and
                                  func-name
                                  ;; Inside string?
                                  (nth 3 it)
                                  (string-match
                                   (rx-to-string
                                    `(and symbol-start
                                          (regexp ,(regexp-opt itfcs 1))
                                          symbol-end
                                          (zero-or-one ".eval"))) func-name))
                             (match-string-no-properties 1 func-name)))))))
         (import-state-p (and (save-excursion
                                (beginning-of-line)
                                (looking-at (rx (0+ space)
                                                (group (or "import" "from"))
                                                (1+ space))))
                              (string= cur-intf "sage")))
         (from-state-p (and import-state-p
                            (string= (match-string-no-properties 1) "from"))))
    (list base-name att-beg intf import-state-p from-state-p)))

(cl-defun sage-shell-cpl:parse-current-state
    (&optional (cur-intf (sage-shell-interfaces:current-interface)))
  "Returns the current state as an alist. Used in a repl buffer."
  (when cur-intf
    (cl-destructuring-bind
        (base-name att-beg intf import-state-p from-state-p)
        (sage-shell-cpl:-parse-state-args cur-intf)
      (let* ((case-fold-search nil) (state nil) (types nil)
             (itfcs sage-shell-interfaces:other-interfaces))
        (cl-destructuring-bind (types state)
            (cond
             ;; import statement
             (import-state-p
              (sage-shell-cpl:-parse-import-state
               base-name att-beg intf import-state-p from-state-p))
             ;; When the word at point is an attribute
             (att-beg
              (sage-shell:push-elmts state
                'var-base-name base-name
                'prefix att-beg)
              (push "attributes" types)
              (cond ((sage-shell:in base-name itfcs)
                     (sage-shell:push-elmts state
                       'interface base-name)
                     (push "interface" types))
                    (t (sage-shell:push-elmts state
                         'interface "sage")))
              (list types state))

             ;; When the current interface is not sage or the point is
             ;; in a function one of gp.eval, gp, gap.eval, ...
             (intf
              (sage-shell:push-elmts state
                'interface intf
                'var-base-name nil
                'prefix (sage-shell-interfaces:looking-back-var intf))
              (push "interface" types)
              (list types state))

             ;; When the current interface is sage
             ((string= cur-intf "sage")
              (let* ((pfx (sage-shell-interfaces:looking-back-var "sage"))
                     (chbf (and pfx (char-before pfx)))
                     (in-func-call (sage-shell:-in-func-call-p)))
                (sage-shell:push-elmts state
                  'interface "sage"
                  'var-base-name nil
                  'prefix pfx)
                ;; Unless parsing failed,
                (unless (and chbf (= chbf 46))
                  (push "interface" types))
                (when in-func-call
                  (push "in-function-call" types)
                  (setq state (sage-shell-cpl:-push-in-func-call-state
                               in-func-call state))))
              (list types state)))
          (sage-shell:push-elmts state
            'types types)
          (cl-check-type state sage-shell-cpl-state)
          ;; Returns state.
          state)))))

(defun sage-shell-cpl:-push-in-func-call-state (in-func-call state)
  "Assume in-func-call is non-nil."
  (let ((in-func-call-end (cadr in-func-call))
        (in-func-name (cl-caddr in-func-call))
        (in-function-call-bn (save-excursion
                               (goto-char (goto-char (cadr in-func-call)))
                               (car (sage-shell-cpl:var-base-name-and-att-start
                                     "sage")))))
    (sage-shell:push-elmts state
      'in-function-call in-func-name
      'in-function-call-base-name in-function-call-bn
      'in-function-call-end in-func-call-end)))

(defun sage-shell-cpl:-scb-and-looking-at (chars regexp)
  (let ((bol (line-beginning-position)))
    (skip-chars-backward chars bol)
    (skip-chars-backward " " bol)
    (skip-chars-backward chars bol)
    (looking-at regexp)))

(defun sage-shell-cpl:-parse-import-state
    (base-name _att-beg _intf _import-state-p from-state-p)
  (let ((state nil) (types nil)
        (var-chars (sage-shell-interfaces:get "sage" 'var-chars)))
    (sage-shell:push-elmts state
      'interface "sage"
      'prefix (sage-shell-interfaces:looking-back-var "sage"))
    (cond
     ;; Top level modules in sys.path
     ((save-excursion
        (or (and (not from-state-p)
                 (sage-shell-cpl:-scb-and-looking-at var-chars (rx "import")))
            (and from-state-p
                 (sage-shell-cpl:-scb-and-looking-at var-chars (rx "from")))))
      (push "modules" types))
     ;; sub-modules in a module
     ((and base-name (save-excursion
                       (sage-shell-cpl:-scb-and-looking-at
                        (concat var-chars ".") (rx (or "import" "from") " "))))
      (push "modules" types)
      (sage-shell:push-elmts state
        'module-name base-name))
     ;; Top level objects in a module
     ((save-excursion
        (and from-state-p
             (sage-shell-cpl:-from-import-state-one-line (point))))
      (push "vars-in-module" types)
      (sage-shell:push-elmts state
        'module-name (match-string-no-properties 1)))
     ;; Else let type be "interfaces".
     (t (push "interface" types)))
    (list types state)))

(defun sage-shell-cpl:-from-import-state-one-line (pt)
  "(match-begining 1) is the module name."
  (beginning-of-line)
  (when (re-search-forward
         (rx "from" (1+ space) (group (1+ (or alnum "_" ".")))
             (1+ space) "import" (1+ space))
         (line-end-position) t)
    (and (<= (match-end 1) pt)
         (save-match-data
           (beginning-of-line)
           (or (null (re-search-forward
                      (rx symbol-start "as" symbol-end)
                      (line-end-position) t))
               (<= pt (match-beginning 0)))))))

(defun sage-shell-cpl:parse-and-set-state ()
  "Parse the current state and set the state."
  (setq sage-shell-cpl:current-state (sage-shell-cpl:parse-current-state)))

(defun sage-shell-cpl:prefix ()
  (when (and (get-buffer-process sage-shell:process-buffer)
             (sage-shell-interfaces:current-interface))
    (sage-shell-cpl:parse-and-set-state)
    (sage-shell-cpl:get-current 'prefix)))


(defvar sage-shell-cpl:-modules-cached nil)
(defvar sage-shell-cpl:-argspec-cached nil)
(defun sage-shell-cpl:-clear-argspec-cache ()
  (setq sage-shell-cpl:-argspec-cached nil))
(make-variable-buffer-local 'sage-shell-cpl:-argspec-cached)

(defun sage-shell-cpl:-argspec-cache (in-function-call)
  (and in-function-call
       (or (assoc-default in-function-call sage-shell-cpl:-argspec-cached)
           (let ((eldoc-cache (assoc-default in-function-call
                                             sage-shell:-eldoc-cache)))
             (if (and eldoc-cache (not (equal eldoc-cache "")))
                 (let ((args (sage-shell:->>
                              (substring eldoc-cache
                                         (1+ (length in-function-call)) -1)
                              sage-shell:-eldoc-split-buffer-args
                              (mapcar (lambda (a) (sage-shell:trim-left a))))))
                   (cl-loop for a in args
                            if (string-match (rx bol (group (1+ (or alnum "_")))
                                                 symbol-end) a)
                            collect (concat (match-string 1 a) "="))))))))

(defun sage-shell-cpl:-mod-type (compl-state)
  (let ((types (sage-shell-cpl:get compl-state 'types)))
    (cl-loop for tp in '("modules" "vars-in-module")
             thereis (sage-shell:in tp types))))

(defun sage-shell-cpl:-cached-mod-key (mod-tp compl-state)
  (cons mod-tp (sage-shell-cpl:get compl-state 'module-name)))

(defun sage-shell-cpl:-push-cache-modules (compl-state sexp)
  (let* ((mod-tp (sage-shell-cpl:-mod-type compl-state))
         (res (assoc-default mod-tp sexp)))
    (sage-shell:awhen mod-tp
      (push (cons (sage-shell-cpl:-cached-mod-key mod-tp compl-state)
                  res)
            sage-shell-cpl:-modules-cached))))

(defun sage-shell-cpl:-push-cache-argspec (compl-state sexp)
  (let ((in-function-call (sage-shell-cpl:get compl-state 'in-function-call)))
    (when in-function-call
      (push (cons in-function-call (assoc-default "in-function-call" sexp))
            sage-shell-cpl:-argspec-cached))))

(cl-defun sage-shell-cpl:-types (compl-state make-cache-file-p)
  (let* ((types (sage-shell-cpl:get compl-state 'types))
         (interface (sage-shell-cpl:get compl-state 'interface))
         (in-function-call (sage-shell-cpl:get compl-state 'in-function-call))
         (update-cmd-p
          (cond (make-cache-file-p
                 (not (string= interface "magma")))
                ((sage-shell:in interface
                                sage-shell-interfaces:optional-interfaces)
                 (sage-shell-interfaces:executable-find interface))
                (t (null (sage-shell-cpl:get-cmd-lst interface))))))
    (sage-shell:chain types
      (if update-cmd-p
          types
        (sage-shell:remove-if (lambda (s) (string= s "interface")) types))
      (if (sage-shell:aand
            (sage-shell-cpl:-mod-type compl-state)
            (assoc-default
             (sage-shell-cpl:-cached-mod-key it compl-state)
             sage-shell-cpl:-modules-cached))
          (sage-shell:remove-if (lambda (s) (or (string= s "modules")
                                                (string= s "vars-in-module")))
                                types)
        types)
      (if (and in-function-call
               (or (assoc in-function-call sage-shell-cpl:-argspec-cached)
                   (assoc in-function-call sage-shell:-eldoc-cache)))
          (sage-shell:remove-if (lambda (s) (string= s "in-function-call")) types)
        types))))

(defvar sage-shell-cpl:-last-sexp nil)
(defvar sage-shell-cpl:-dict-keys
  '(interface var-base-name module-name in-function-call
              in-function-call-base-name))

(defun sage-shell:redirect-and-output-finished-p ()
  (and (sage-shell:redirect-finished-p)
       (sage-shell:output-finished-p)))

(cl-defun sage-shell-cpl:completion-init
    (sync &key (output-buffer (sage-shell:output-buffer))
          (compl-state sage-shell-cpl:current-state)
          (callback nil))
  "If SYNC is non-nil, return a sexp. If not return value has no
meaning and `sage-shell-cpl:-last-sexp' will be set when the
redirection is finished.
This function set the command list by using `sage-shell-cpl:set-cmd-lst'"
  (cl-check-type compl-state sage-shell-cpl-state)
  (cl-check-type callback (or null function))
  ;; when current line is not in a block and current interface is 'sage'
  (setq sage-shell-cpl:-last-sexp nil)
  (when (and (sage-shell:with-current-buffer-safe sage-shell:process-buffer
               (sage-shell:at-top-level-and-in-sage-p))
             (sage-shell:redirect-and-output-finished-p))
    (let* ((interface (sage-shell-cpl:get compl-state 'interface))
           (verbose (sage-shell-interfaces:get interface 'verbose))
           (make-cache-file-p
            ;; 'verbose' and 'interface' is installed and cache file
            ;; does not exit
            (and (not (sage-shell-cpl:get-cmd-lst interface))
                 verbose
                 (not (file-exists-p (sage-shell-interfaces:get
                                      interface 'cache-file)))
                 (or (not (sage-shell:in
                           interface
                           sage-shell-interfaces:optional-interfaces))
                     (sage-shell-interfaces:executable-find interface))))
           (types (sage-shell-cpl:-types compl-state make-cache-file-p)))
      (when make-cache-file-p
        ;; Show verbose message and make a cache file.
        (sage-shell-cpl:init-verbose interface verbose))
      (when types
        (let ((cmd (format
                    "%s(%s, %s)"
                    (sage-shell:py-mod-func "print_cpl_sexp")
                    (sage-shell:-to-python-list types)
                    (sage-shell:-to-python-dict
                     (cl-loop for a in sage-shell-cpl:-dict-keys
                              collect
                              (cons a (assoc-default a compl-state)))))))
          (sage-shell:run-cell
           cmd
           :output-buffer output-buffer
           :sync sync
           :callback
           (lambda (s)
             (sage-shell-cpl:-cpl-init-callback s compl-state)
             (when (functionp callback)
               (funcall callback s))))
          (if sync
              sage-shell-cpl:-last-sexp))))))

(defun sage-shell-cpl:-cpl-init-callback (s compl-state)
  (cl-check-type s sage-shell:output-stct)
  (cond ((sage-shell:output-stct-success s)
         (let ((output (sage-shell:output-stct-output s)))
           (unless (string-match (rx "))\n" buffer-end) output)
             (error "Invalid output"))
           (let ((lines (butlast (split-string output "\n"))))
             (when (cdr-safe lines)
               (display-message-or-buffer
                (cl-loop for a in (butlast lines)
                         concat a)))
             (setq sage-shell-cpl:-last-sexp
                   (read (car (last lines))))
             ;; Code for caching (change global vars)
             (sage-shell-cpl:-push-cache-modules
              compl-state sage-shell-cpl:-last-sexp)
             (sage-shell-cpl:-set-cmd-lst
              compl-state sage-shell-cpl:-last-sexp)
             (sage-shell-cpl:-push-cache-argspec
              compl-state sage-shell-cpl:-last-sexp))))
        ;; Error
        (t (display-message-or-buffer
            (sage-shell:output-stct-output s)))))

(defun sage-shell-cpl:-set-cmd-lst (state sexp)
  (let ((int (sage-shell-cpl:get state 'interface)))
    (when (and (sage-shell:in
                "interface" (sage-shell-cpl:get state 'types))
               (null (sage-shell-cpl:get-cmd-lst int)))
      (let ((ls (assoc-default "interface" sexp)))
        (when ls
          (sage-shell-cpl:set-cmd-lst
           int
           (cl-loop with regexp =
                    (format "^%s"
                            (sage-shell-interfaces:get int 'cmd-rxp))
                    for s in ls
                    if (string-match regexp s)
                    collect s)))))))

(defun sage-shell-cpl:init-verbose (interface verbose)
  (cond
   ((not (string= interface "magma"))
    (message verbose))

   (t (let* ((tmp-file (make-temp-file "sage" nil ".sage"))
             (proc-name "sage-shell-magma-complete"))
        ;; if the interface is magma, start new process
        (with-temp-buffer
          (insert (format "%s.trait_names(verbose=False)" interface))
          (write-region (point-min) (point-max) tmp-file nil 'silent))
        (unless (cl-loop for p in (process-list)
                         thereis
                         (equal (process-name p) proc-name))
          (message verbose)
          (let ((time (cadr (current-time)))
                (proc (start-process proc-name nil
                                     (sage-shell:sage-executable)
                                     tmp-file)))
            (set-process-sentinel
             proc
             (lambda (_proc _event)
               (message
                "Scanning Magma types ... Done! (%d seconds)\n Saving cache to
'%s' for future instant use\n.  Delete the above file to force re-creation of the cache."
                (- (cadr (current-time)) time)
                (sage-shell-interfaces:get "magma" 'cache-file))))))))))

(defun sage-shell-cpl:switch-to-another-interface-p (line)
  "Returns non nil when LINE contains %gp, gp.console(), gp.interact(), ..."
  (cl-loop for itf in sage-shell-interfaces:other-interfaces
           if (string-match
               (concat
                "\\("
                (mapconcat
                 'identity
                 (list (concat "%" itf "\\>")
                       (concat itf "\\.console()")
                       (concat itf "\\.interact()"))
                 "\\|") "\\)")
               line)
           return itf))

(defun sage-shell-cpl:cmds-symbol (intf)
  (intern (format "sage-shell-cpl:%s-commands" intf)))

(defvar sage-shell-cpl:sage-commands nil "List of global objects
of current Sage process.")
(make-variable-buffer-local 'sage-shell-cpl:sage-commands)

(cl-loop for i in sage-shell-interfaces:other-interfaces
         for sym = (sage-shell-cpl:cmds-symbol i)
         do (set sym nil))

(defun sage-shell-cpl:set-cmd-lst (intf lst)
  (if (sage-shell:in intf (cons "sage" sage-shell-interfaces:other-interfaces))
      (sage-shell:awhen (sage-shell:aand
                          sage-shell:process-buffer
                          (get-buffer it))
        (with-current-buffer it
          (set (sage-shell-cpl:cmds-symbol intf) lst)))
    (error (format "No interface %s" intf))))

(defun sage-shell-cpl:get-cmd-lst (intf)
  (if (sage-shell:in intf (cons "sage" sage-shell-interfaces:other-interfaces))
      (sage-shell:with-current-buffer-safe sage-shell:process-buffer
        (symbol-value (sage-shell-cpl:cmds-symbol intf)))
    (error (format "No interface %s" intf))))

(defun sage-shell-cpl:to-objname-to-send (can)
  (let* ((var-base-name (sage-shell-cpl:get-current 'var-base-name))
         (interface (cond ((sage-shell:in
                            (sage-shell-interfaces:current-interface)
                            sage-shell-interfaces:other-interfaces)
                           (sage-shell-interfaces:current-interface))
                          (t (sage-shell-cpl:get-current 'interface)))))
    (cond (var-base-name (concat var-base-name "." can))
          ((sage-shell:in interface sage-shell-interfaces:other-interfaces)
           (concat interface "." can))
          (t can))))

(defun sage-shell-cpl:candidates-sync (&optional regexp)
  (when (and (sage-shell-cpl:parse-and-set-state)
             (sage-shell:redirect-and-output-finished-p))
    (let ((cur-intf (sage-shell-interfaces:current-interface)))
      (sage-shell-cpl:candidates
       :sexp (sage-shell-cpl:completion-init t)
       :regexp (or regexp (sage-shell-interfaces:get cur-intf 'cmd-rxp))))))

(defun sage-shell-cpl:trans-sexp (sexp state)
  "Trasnform SEXP so that the union of cdr is an appropriate list
 of candidates."
  (let ((types (sage-shell-cpl:get state 'types))
        (int (sage-shell-cpl:get state 'interface))
        (mod-tp (sage-shell-cpl:-mod-type state))
        (in-function-call (sage-shell-cpl:get state 'in-function-call)))
    (sage-shell:chain sexp
      (cond ((and (sage-shell:in "interface" types)
                  (null (assoc "interface" sexp)))
             (cons (cons "interface" (sage-shell-cpl:get-cmd-lst int))
                   sexp))
            (t sexp))
      (let* ((ky (sage-shell-cpl:-cached-mod-key mod-tp state))
             (val (assoc-default ky sage-shell-cpl:-modules-cached)))
        (cond ((and mod-tp val (null (assoc-default mod-tp sexp)))
               (cons (cons mod-tp val) sexp))
              (t sexp)))
      (sage-shell:aif (and in-function-call
                           (sage-shell-cpl:-argspec-cache in-function-call))
          (cons (cons "in-function-call" it) sexp)
        sexp))))

(defun sage-shell-cpl:-default-regexp-alst (keys _state)
  (let ((regexp (sage-shell-interfaces:get "sage" 'cmd-rxp)))
    (cl-loop for k in keys
             collect
             (cons k regexp))))

(defun sage-shell-cpl:-use-filter-p (type state)
  (cond ((string= type "interface")
         (string= (sage-shell-cpl:get state 'interface) "sage"))
        (t (sage-shell:in type '("attributes")))))

(cl-defun sage-shell-cpl:candidates
    (&key (regexp nil) (sexp sage-shell-cpl:-last-sexp)
          (state sage-shell-cpl:current-state) (keys t))
  "Collect candidates matching (concat \"^\" REGEXP).
If KEYS is a list of string, then it collects only cdr of SEXP
whose key is in KEYS."
  (let* ((sexp1 (sage-shell-cpl:trans-sexp sexp state))
         (keys1 (cond ((listp keys) keys)
                      (t (sage-shell-cpl:get state 'types))))
         (regexp1 (format "^%s"
                          (or regexp (sage-shell-interfaces:get
                                      "sage" 'cmd-rxp)))))
    (cl-loop for type in keys1
             for cands = (assoc-default type sexp1)
             when cands
             append
             (if (sage-shell-cpl:-use-filter-p type state)
                 (cl-loop for s in cands
                          if (string-match regexp1 s)
                          collect s)
               cands))))

(defvar sage-shell:completion-sync-cached nil)
(make-variable-buffer-local 'sage-shell:completion-sync-cached)
(defun sage-shell:clear-completion-sync-cached ()
  (sage-shell:with-current-buffer-safe sage-shell:process-buffer
    (setq sage-shell:completion-sync-cached nil)))

(defvar sage-shell:-python-builtins
  '("__import__" "abs" "all" "and" "any" "apply" "as" "assert" "basestring"
    "bin" "bool" "break" "buffer" "bytearray" "callable" "chr" "class"
    "classmethod" "cmp" "coerce" "compile" "complex" "continue" "def" "del"
    "delattr" "dict" "dir" "divmod" "elif" "else" "enumerate" "eval" "except"
    "exec" "execfile" "file" "filter" "finally" "float" "for" "format" "from"
    "frozenset" "getattr" "global" "globals" "hasattr" "hash" "help" "hex"
    "id" "if" "import" "in" "input" "int" "intern" "is" "isinstance"
    "issubclass" "iter" "lambda" "len" "list" "locals" "long" "map" "max"
    "memoryview" "min" "next" "not" "object" "oct" "open" "or" "ord" "pass"
    "pow" "print" "print" "property" "raise" "range" "raw_input" "reduce"
    "reload" "repr" "return" "reversed" "round" "set" "setattr" "slice"
    "sorted" "staticmethod" "str" "sum" "super" "try" "tuple" "type"
    "unichr" "unicode" "vars" "while" "with" "xrange" "yield" "zip"))

(defun sage-shell:completion-at-point-func ()
  "Used for completion-at-point. The result is cached."
  (let ((wab (sage-shell:word-at-pt-beg)))
    (list wab (point) (sage-shell:-completion-at-point))))

(defun sage-shell:-completion-at-point ()
  "Return list of possible completions at point."
  (let ((old-int (sage-shell-cpl:get-current 'interface))
        (old-pref (sage-shell-cpl:get-current 'prefix))
        (wab (sage-shell:word-at-pt-beg))
        (var-name (progn
                    (sage-shell-cpl:parse-and-set-state)
                    (sage-shell-cpl:get-current 'var-base-name))))
    (cond ((and
            old-int (string= old-int "sage") old-pref
            ;; same line as the last completion
            (or (= (line-number-at-pos wab) (line-number-at-pos old-pref))
                (sage-shell:clear-completion-sync-cached))
            var-name
            (assoc-default var-name sage-shell:completion-sync-cached))
           (assoc-default var-name sage-shell:completion-sync-cached))
          (t (cond
              (var-name
               (setq sage-shell:completion-sync-cached
                     (cons (cons var-name
                                 (sage-shell-cpl:candidates-sync
                                  sage-shell:completion-candidate-regexp))
                           sage-shell:completion-sync-cached))
               (assoc-default var-name sage-shell:completion-sync-cached))
              (t (sage-shell:-completion-at-pt-func-append
                  (sage-shell-cpl:candidates-sync
                   sage-shell:completion-candidate-regexp))))))))

(defun sage-shell:-completion-at-pt-func-append (ls)
  (append
   (when (and (sage-shell:in "interface"
                             (sage-shell-cpl:get-current 'types))
              (string= (sage-shell-cpl:get-current 'interface) "sage"))
     (append sage-shell:-python-builtins
             sage-shell-cpl:-cands-in-current-session))
   ls))


(defun sage-shell:symbol-beg ()
  (save-excursion
    (let ((chars (sage-shell-interfaces:get
                  (sage-shell-interfaces:current-interface)
                  'var-chars)))
      (skip-chars-backward chars))
    (point)))

(defun sage-shell:pcomplete-parse-args ()
  (let ((sb (sage-shell:symbol-beg)))
    (list
     (list (buffer-substring-no-properties
            sb (point)))
     sb)))

(defun sage-shell:pcomplete-setup ()
  (set (make-local-variable 'pcomplete-parse-arguments-function)
       'sage-shell:pcomplete-parse-args)
  (set (make-local-variable 'pcomplete-default-completion-function)
       'sage-shell:pcomplete-default-completion)
  (set (make-local-variable 'pcomplete-command-completion-function)
       'sage-shell:pcomplete-default-completion)
  (set (make-local-variable 'pcomplete-termination-string) "")
  (set (make-local-variable 'pcomplete-cycle-completions) nil))

(defun sage-shell:pcomplete-default-completion ()
  (pcomplete-here
   (all-completions
    (buffer-substring-no-properties (sage-shell:symbol-beg) (point))
    (sage-shell:-completion-at-point))))

(defun sage-shell-edit:candidates-sync (&optional regexp)
  (let ((state (sage-shell-edit:parse-current-state)))
    (when (and state
               (progn
                 (sage-shell-edit:set-sage-proc-buf-internal nil nil)
                 sage-shell:process-buffer
                 (buffer-live-p sage-shell:process-buffer))
               (sage-shell:redirect-and-output-finished-p))
      (sage-shell-cpl:candidates
       :sexp (sage-shell-cpl:completion-init t :compl-state state)
       :state state
       :regexp (or regexp (sage-shell-interfaces:get "sage" 'cmd-rxp))))))

(defun sage-shell-edit:completion-at-point-func ()
  "Used for completion-at-point."
  (let ((wab (sage-shell:word-at-pt-beg)))
    (list wab (point) (sage-shell-edit:candidates-sync))))


;;; sage-edit
(defun sage-shell-edit:process-alist ()
  (or (sage-shell:aif (and sage-shell:process-buffer
                           (buffer-live-p sage-shell:process-buffer)
                           (get-buffer-process sage-shell:process-buffer))
          (list (cons it (process-name it))))
      (let ((case-fold-search nil))
        (cl-loop for proc in (process-list)
                 for buffer-name = (sage-shell:aif (process-buffer proc)
                                       (and (buffer-live-p it)
                                            (buffer-name it)))
                 for proc-name = (process-name proc)
                 if (and buffer-name
                         (string-match
                          (rx bol "*Sage"
                              (zero-or-one "<" (1+ (or alnum "-" "." "_")) ">")
                              (zero-or-one "@" (1+ (or alnum "-" "." "_"))) "*"
                              eol)
                          buffer-name))
                 collect (cons proc-name proc)))))

(defun sage-shell:set-process-buffer ()
  (interactive)
  (setq sage-shell:process-buffer nil)
  (sage-shell-edit:set-sage-proc-buf-internal)
  (sage-shell:aif (get-buffer sage-shell:process-buffer)
      (message (format "Set the process buffer to buffer %s."
                       (buffer-name it)))))

(cl-defun sage-shell-edit:set-sage-proc-buf-internal
    (&optional (start-p t) (select-p t)
               (select-msg (concat
                            "There are multiple Sage processes. "
                            "Please select the process buffer: ")))
  "Set `sage-shell:process-buffer'"
  (or (and (buffer-live-p sage-shell:process-buffer)
           (get-buffer-process sage-shell:process-buffer))
      (let ((proc-alist (sage-shell-edit:process-alist))
            (cur-buf (current-buffer)))
        (cond
         ;; if there are no processes
         ((null proc-alist)
          (when (and start-p
                     (y-or-n-p (concat "Threre are no Sage processes. "
                                       "Start new process? ")))
            (let ((proc-buf
                   (sage-shell:run
                    (sage-shell:read-command) nil
                    :switch-function 'display-buffer)))
              (with-current-buffer cur-buf
                (setq sage-shell:process-buffer proc-buf)))))
         ;; if there are multiple processes
         ((consp (cdr proc-alist))
          (when select-p
            (let* ((buffer-names
                    (cl-loop for x in proc-alist
                             collect (buffer-name (process-buffer (cdr x)))))
                   (buffer-name
                    (completing-read
                     select-msg
                     buffer-names nil nil
                     (try-completion "" buffer-names)))
                   (proc (get-buffer-process buffer-name)))
              (setq sage-shell:process-buffer (process-buffer proc)))))
         ;; if there is exactly one process
         (t (setq sage-shell:process-buffer
                  (process-buffer (cdar proc-alist))))))))

(defvar sage-shell:original-mode-line-process nil)

(cl-defun sage-shell:change-mode-line-process (on &optional (name "load"))
  (cond (on
         (unless sage-shell:original-mode-line-process
           (setq sage-shell:original-mode-line-process mode-line-process))
         (setq mode-line-process
               (sage-shell:aif mode-line-process
                   (list (concat (car it) " " name))
                 (list (concat ":%s " name)))))
        (t (setq mode-line-process sage-shell:original-mode-line-process))))

(cl-defun sage-shell-edit:exec-command-base
    (&key command pre-message post-message switch-p
          (display-function nil) (insert-command-p t) (before-sentence nil)
          (push-to-input-history-p nil)
          callback)
  "If `insert-command-p' is non-nil, then it inserts `command' in
the process buffer. If `before-sentence' is non-nil, it will be
inserted in the process buffer before executing the command."
  (cl-check-type command string)
  (cl-check-type pre-message (or null string))
  (cl-check-type post-message (or null string))
  (cl-check-type display-function (or null function))
  (cl-check-type callback (or null function))

  ;; set sage process buffer
  (sage-shell-edit:set-sage-proc-buf-internal)

  (sage-shell:when-process-alive
    (sage-shell-edit:-exec-command-base
     command pre-message post-message switch-p
     display-function insert-command-p before-sentence
     push-to-input-history-p
     callback)))

(defun sage-shell-edit:-exec-command-base
    (command pre-message post-message switch-p
             display-function insert-command-p before-sentence
             push-to-input-history-p
             callback)

  (with-current-buffer sage-shell:process-buffer
    (sage-shell:change-mode-line-process t)
    (force-mode-line-update))

  (sage-shell:awhen pre-message (message it))

  (sage-shell:as-soon-as (sage-shell:output-finished-p)
    (sage-shell:push-to-output-finished-hook
      (when post-message
        (message post-message))

      ;; display buffer
      (when display-function
        (let ((win (funcall display-function sage-shell:process-buffer)))
          (sage-shell:with-selected-window-if-possible win
            (goto-char
             (process-mark
              (get-buffer-process sage-shell:process-buffer))))))

      (with-current-buffer sage-shell:process-buffer
        (sage-shell:change-mode-line-process nil))

      (when (functionp callback)
        (funcall callback)))

    (let ((win (get-buffer-window sage-shell:process-buffer))
          (args (list command insert-command-p before-sentence
                      push-to-input-history-p callback)))
      (sage-shell:with-selected-window-if-possible win
        (apply 'sage-shell-edit:exec-cmd-internal args))))
  (when switch-p (pop-to-buffer sage-shell:process-buffer)))

(defun sage-shell-edit:exec-cmd-internal
    (command insert-command-p before-sentence push-to-input-history-p _callback)
  (let* ((proc (get-buffer-process sage-shell:process-buffer))
         (pmark (process-mark proc)))
    (with-current-buffer sage-shell:process-buffer
      (goto-char pmark)
      (end-of-line)
      (let* ((bol (comint-line-beginning-position))
             (eol (line-end-position))
             (line (buffer-substring-no-properties bol eol)))
        (delete-region bol eol)
        (sage-shell:awhen before-sentence
          (insert it)
          (set-marker pmark (point)))
        (cond (insert-command-p
               (goto-char pmark)
               (insert command)
               (if push-to-input-history-p
                   (sage-shell:send-input)
                 (let ((comint-input-ring (make-ring 1)))
                   (sage-shell:send-input))))
              (t (sage-shell:prepare-for-send)
                 (comint-send-string proc (concat command "\n"))))
        (save-excursion
          (insert line))))))

(defvar sage-shell-edit:temp-file-base-name "sage_shell_mode_temp")

(defun sage-shell-edit:make-temp-dir ()
  (make-temp-file "sage_shell_mode" 'directory))

(defvar sage-shell-edit:temp-directory nil)
(make-variable-buffer-local 'sage-shell-edit:temp-directory)

(defun sage-shell-edit:delete-temp-dir ()
  (when (and (stringp sage-shell-edit:temp-directory)
             (string= (file-name-as-directory temporary-file-directory)
                      (file-name-directory sage-shell-edit:temp-directory))
             (file-exists-p sage-shell-edit:temp-directory))
    (delete-directory sage-shell-edit:temp-directory t)))

(defun sage-shell-edit--set-and-make-temp-dir ()
  (unless sage-shell:process-buffer
    (sage-shell-edit:set-sage-proc-buf-internal))
  (with-current-buffer sage-shell:process-buffer
    ;; In case temp dir is removed,
    (unless (and
             (stringp sage-shell-edit:temp-directory)
             (file-exists-p sage-shell-edit:temp-directory)
             (file-writable-p sage-shell-edit:temp-directory))
      (setq sage-shell-edit:temp-directory
            (sage-shell-edit:make-temp-dir)))))

(defun sage-shell-edit:temp-file (ext)
  (sage-shell-edit--set-and-make-temp-dir)
  (expand-file-name
   (concat sage-shell-edit:temp-file-base-name "." ext)
   (buffer-local-value 'sage-shell-edit:temp-directory
                       sage-shell:process-buffer)))

(defun sage-shell-edit:write-region-to-file (start end file)
  (let* ((orig-start (min start end))
         (buf-str (buffer-substring-no-properties start end))
         (offset (save-excursion
                   (goto-char orig-start)
                   (- (point) (line-beginning-position)))))
    (with-temp-buffer
      (when sage-shell-edit:temp-file-header
        (insert sage-shell-edit:temp-file-header))
      (insert (make-string offset (string-to-char " ")))
      (save-excursion
        (insert buf-str))
      (re-search-forward (rx (not whitespace)) nil t)
      (beginning-of-line)
      (when (looking-at " +")           ; need dummy block
        (insert "if True:\n"))
      (goto-char (point-max))
      (unless (bolp)
        (newline))
      (write-region nil nil  file nil 'nomsg))
    ;; return temp file name
    file))

(defun sage-shell-edit:make-temp-file-from-region (start end)
  "Make temp file from region and return temp file name."
  (let ((f (sage-shell-edit:temp-file
            (let ((bfn (buffer-file-name)))
              (if (and bfn (member (file-name-extension bfn) '("py" "sage")))
                  (file-name-extension bfn)
                "sage")))))
    (sage-shell-edit:write-region-to-file start end f)))

(defun sage-shell-edit:beg-of-defun-position ()
  (min (save-excursion
         (end-of-defun)
         (beginning-of-defun) (point))
       (save-excursion (beginning-of-defun)
                       (point))))

(defun sage-shell-edit:end-of-defun-position ()
  (save-excursion
    (goto-char (sage-shell-edit:beg-of-defun-position))
    (end-of-defun)
    (point)))

(defun sage-shell-edit:block-name ()
  (let ((beg (sage-shell-edit:beg-of-defun-position)))
    (save-excursion
      (buffer-substring
       (progn (goto-char beg)
              (skip-chars-forward " \t") (point))
       (progn (end-of-line)
              (skip-chars-backward " \t:")
              (point))))))

(eval-when-compile
  (defvar sage-shell-edit:exec-command-base-alist
    (list (cons '-buffer (list :beg '(point-min)
                               :end '(point-max)
                               :name "buffer"
                               :doc "internal function"))
          (cons 'region (list :beg 'beg :end 'end :name "region"
                              :interactive "r"
                              :args '(beg end)))
          (cons 'defun
                (list :beg '(sage-shell-edit:beg-of-defun-position)
                      :end '(sage-shell-edit:end-of-defun-position)
                      :name '(format "block: %s" (sage-shell-edit:block-name))
                      :doc-name "def (or class)"))
          (cons 'line (list :beg '(line-beginning-position)
                            :end '(line-end-position)
                            :name "line")))))

(defun sage-shell-edit--send-obj-command (beg end)
  (let ((contents (buffer-substring-no-properties beg end))
        (temp-file nil))
    (setq temp-file
          (cond ((string-match-p (rx bol "%%cython") contents)
                 ;; ip.run_cell doesn't work well with %%cython
                 ;; if there are comments.
                 (let ((sage-shell-edit:temp-file-header nil))
                   (sage-shell-edit:make-temp-file-from-region
                    beg end)))
                (t (sage-shell-edit:make-temp-file-from-region
                    beg end))))
    (if (string-match-p (rx bol (1+ "%") alnum) contents)
        (format "%s('%s')"
                (sage-shell:py-mod-func "read_file_and_run_contents") temp-file)
      (format "load('%s')" temp-file))))

(cl-defmacro sage-shell-edit:send-obj-base
    (&key type switch-p (display-function 'sage-shell-edit:display-function))
  (declare (debug t))
  (let* ((plst (assoc-default type sage-shell-edit:exec-command-base-alist))
         (beg (plist-get plst :beg))
         (end (plist-get plst :end))
         (command `(sage-shell-edit--send-obj-command ,beg ,end)))
    `(save-excursion
       (sage-shell-edit:exec-command-base
        :command ,command
        :pre-message (format "Loading the %s to the Sage process..."
                             ,(plist-get plst :name))
        :post-message (format "Loading the %s to the Sage process... Done."
                              ,(plist-get plst :name))
        :switch-p ,switch-p
        :display-function ,display-function)
       (sage-shell:clear-command-cache))))

(defmacro sage-shell-edit:defun-exec-commands ()
  (append '(progn)
          (cl-loop for (type . plist) in sage-shell-edit:exec-command-base-alist
                   for func-name-base = (format "sage-shell-edit:send-%s" type)
                   for doc-string = (or (plist-get plist :doc)
                                        (format "Evaluate the current %S in the Sage process."
                                                type))
                   for int = (sage-shell:aif (plist-get plist :interactive)
                                 `(interactive ,it)
                               '(interactive))
                   for args = (sage-shell:aif (plist-get plist :args) it '())
                   append
                   (cl-loop for b in '(t nil)
                            for func-name =  (cond (b (concat func-name-base "-and-go"))
                                                   (t func-name-base))
                            collect
                            `(defun ,(intern func-name) ,args ,doc-string ,int
                                    (sage-shell-edit:send-obj-base :type ,type :switch-p ,b))))))

(sage-shell-edit:defun-exec-commands)

(defun sage-shell-edit:send-buffer ()
  "Evaluate the contents of the current buffer in the Sage process."
  (interactive)
  (let ((bfn (buffer-file-name)))
    (cond ((and bfn (not (buffer-modified-p)))
           (sage-shell-edit:load-file bfn))
          (t (sage-shell-edit:send--buffer)))))

(defun sage-shell-edit:send-buffer-and-go ()
  "Evaluate the contents of the current buffer in the Sage process."
  (interactive)
  (let ((bfn (buffer-file-name)))
    (cond ((and bfn (not (buffer-modified-p)))
           (sage-shell-edit:load-file-and-go bfn))
          (t (sage-shell-edit:send--buffer-and-go)))))

(defvar sage-shell:file-extensions '("sage" "py" "spyx" "pyx"))

(defun sage-shell-edit:read-script-file ()
  (read-file-name
   "Load Sage file: "
   nil
   (sage-shell:awhen (buffer-file-name) it)
   nil
   nil
   (lambda (name)
     (or
      (file-directory-p name)
      (string-match
       (concat "\\." (regexp-opt sage-shell:file-extensions) "$") name)))))

(defun sage-shell-edit:send-line* ()
  "Like sage-shell-edit:send-line, but insert the line in the process buffer."
  (interactive)
  (sage-shell-edit:exec-command-base :command (buffer-substring
                                               (line-beginning-position)
                                               (line-end-position))
                                     :insert-command-p t
                                     :display-function 'display-buffer
                                     :push-to-input-history-p t))

(defvar sage-shell:-test-prompt-regexp
  (rx line-start (? (or "-" "+")) (0+ (or space punct))
      (1+ (or "sage: " ">>> "))))

(defun sage-shell:-doctest-lines ()
  "If the current line start with a sage: prompt, return lines for doctest."
  (save-excursion
    (forward-line 0)
    (when (looking-at sage-shell:-test-prompt-regexp)
      (let* ((inhibit-field-text-motion t)
             (lines (list (buffer-substring-no-properties
                           (match-end 0)
                           (line-end-position))))
             (regexp (rx line-start (0+ whitespace) "....: ")))
        (forward-line 1)
        (cl-loop while (and
                        (not (eobp))
                        (progn
                          (beginning-of-line)
                          (looking-at regexp)))
                 do
                 (push (buffer-substring-no-properties
                        (match-end 0)
                        (line-end-position))
                       lines)
                 (forward-line 1)
                 finally return (nreverse lines))))))

(defun sage-shell:send-doctest (arg)
  (interactive "P")
  "If looking at a sage: prompt, send the current doctest lines to the Sage process.
With prefix argument, send all doctests (at sage: prompts) until
the end of the docstring."
  (sage-shell-edit:set-sage-proc-buf-internal :select-p t)
  ;; Some code are copied from sage-test.el provided by sage-mode
  (unless arg
    (save-excursion
      (forward-line 0)
      (unless (looking-at sage-shell:-test-prompt-regexp)
        (error "Not at a sage: prompt"))))
  (cond ((or (derived-mode-p 'sage-shell:help-mode)
             (derived-mode-p 'Info-mode))
         (sage-shell:-send-current-doctest
          (lambda ()
            (end-of-line)
            (or
             (re-search-forward sage-shell:-test-prompt-regexp nil t)
             (forward-line 1)))))
        (arg (sage-shell:send-all-doctests))
        ((derived-mode-p 'python-mode)
         (sage-shell:-send-current-doctest
          (lambda ()
            (let ((s-start-end (sage-shell:-beg-end-of-docstring)))
              (end-of-line)
              (or (and s-start-end
                       (re-search-forward sage-shell:-test-prompt-regexp
                                          (cdr s-start-end) t))
                  (forward-line 1))))))))

(defun sage-shell:-beg-end-of-docstring ()
  "Return cons of the string beg and the string end if the point is in a string.
Othewise return nil."
  (let* ((ppss (syntax-ppss))
         (in-string (nth 3 ppss))
         (string-start (nth 8 ppss))
         (string-end nil))
    (when in-string
      (save-excursion
        (goto-char string-start)
        (ignore-errors
          (forward-sexp)
          (setq string-end (point)))
        (when (and string-start string-end)
          (cons string-start string-end))))))

;; TODO use indefinite extent and delete this global variable
;; Drop Emacs 24.1 and 24.2 support
(defvar sage-shell:-doctest-marker (make-marker))

(defun sage-shell:send-all-doctests ()
  "Evaluate all doctests inside current docstring."
  (interactive)
  (sage-shell-edit:set-sage-proc-buf-internal :select-p t)
  (let* ((s-start-end (sage-shell:-beg-end-of-docstring))
         (buf (current-buffer))
         (string-start (car s-start-end))
         (string-end (cdr s-start-end)))
    (unless s-start-end
      (error "Not inside a docstring."))
    (with-current-buffer sage-shell:process-buffer
      (setq sage-shell:-report-cursor-pos-p nil))

    (goto-char string-start)
    (set-marker sage-shell:-doctest-marker (point))
    (sage-shell:-send-current-doctest-rec string-end buf)))

(defun sage-shell:-send-current-doctest-rec (bd buf)
  (with-current-buffer buf
    (goto-char sage-shell:-doctest-marker)
    (cond ((and (<= (point) bd)
                (re-search-forward sage-shell:-test-prompt-regexp
                                   bd t))
           (set-marker sage-shell:-doctest-marker (point))
           (sage-shell:-send-current-doctest
            (lambda ()
              (sage-shell:-send-current-doctest-rec bd buf))))
          (t (with-current-buffer sage-shell:process-buffer
               (setq sage-shell:-report-cursor-pos-p t))))))

(defun sage-shell:-send-current-doctest (&optional callback)
  (let ((lines (sage-shell:-doctest-lines))
        (buf (current-buffer)))
    (cond
     ((null lines))
     ((null (cdr lines))
      (sage-shell-edit:exec-command-base
       :command (car lines)
       :insert-command-p t
       :display-function 'display-buffer
       :push-to-input-history-p t
       :callback (when callback
                   (lambda ()
                     (with-current-buffer buf
                       (funcall callback))))))
     (t
      (let ((lines (sage-shell:-cpaste-lines lines)))
        (sage-shell:-send--lines-internal
         lines
         (lambda ()
           (with-current-buffer buf
             (funcall callback)))))))))

(defun sage-shell:-send--lines-internal (lines &optional callback)
  (sage-shell-when-emacs25-or-later
    (cl-check-type lines (sage-shell-list-of string)))
  (cl-check-type callback (or null function))
  (when lines
    (with-current-buffer sage-shell:process-buffer
      (sage-shell:setq-local
       sage-shell:output-finished-regexp
       (rx-to-string
        `(and line-start
              (or ,sage-shell:output-finished-regexp-rx
                  (and ":" line-end))))))
    (sage-shell-edit:exec-command-base
     :command (car lines)
     :insert-command-p t
     :display-function 'display-buffer
     :push-to-input-history-p t
     :callback
     (lambda ()
       (cond ((cdr lines)
              (sage-shell:-send--lines-internal
               (cdr lines) callback))
             (t (with-current-buffer sage-shell:process-buffer
                  (sage-shell:setq-local
                   sage-shell:output-finished-regexp
                   (default-value
                     'sage-shell:output-finished-regexp))
                  (when (functionp callback)
                    (funcall callback)))))))))

(cl-defun sage-shell-edit:load-file-base
    (&key command file-name switch-p
          (display-function sage-shell-edit:display-function)
          (insert-command-p t) (before-sentence nil)
          (gerund "Loading"))
  (cl-check-type command (or null string))
  (cl-check-type file-name (or null string))
  (let ((buf (cl-loop for b in (buffer-list)
                      for bfn = (buffer-file-name b)
                      if (and bfn (equal (expand-file-name bfn)
                                         (expand-file-name file-name))
                              (buffer-modified-p b))
                      return b)))
    (when (and buf (y-or-n-p (message "Save file %s?"
                                      (buffer-file-name buf))))
      (with-current-buffer buf
        (save-buffer))))

  (sage-shell-edit:exec-command-base
   :command (or command (format "load('%s')" file-name))
   :switch-p switch-p
   :display-function display-function
   :pre-message (format "%s %s to the Sage process..."
                        gerund (file-name-nondirectory file-name))
   :post-message (format "%s %s to the Sage process... Done."
                         gerund (file-name-nondirectory file-name))
   :insert-command-p insert-command-p
   :before-sentence before-sentence)
  (sage-shell:clear-command-cache))

(defun sage-shell-edit:load-file (file-name)
  "Load a Sage file FILE-NAME to the Sage process."
  (interactive (list (sage-shell-edit:read-script-file)))
  (sage-shell-edit:load-file-base
   :file-name file-name))

(defun sage-shell-edit:attach-file (file-name)
  "Attach a Sage file FILE-NAME to the Sage process."
  (interactive (list (sage-shell-edit:read-script-file)))
  (sage-shell-edit:load-file-base
   :command (format "attach('%s')" file-name)
   :file-name file-name
   :gerund "Attaching"))

(defun sage-shell-edit:load-file-and-go (file-name)
  "Load a Sage file FILE-NAME to the Sage process."
  (interactive (list (sage-shell-edit:read-script-file)))
  (sage-shell-edit:load-file-base
   :file-name file-name
   :switch-p t))

(defun sage-shell-edit:load-current-file ()
  "Load the current file to the Sage process."
  (interactive)
  (sage-shell:aif (buffer-file-name) (sage-shell-edit:load-file it)))

(defun sage-shell-edit:load-current-file-and-go ()
  "Load the current file to the Sage process."
  (interactive)
  (sage-shell:aif (buffer-file-name) (sage-shell-edit:load-file-and-go it)))

(defun sage-shell-edit:pop-to-process-buffer ()
  "Switch to the Sage process buffer."
  (interactive)
  (unless (buffer-live-p sage-shell:process-buffer)
    (sage-shell-edit:set-sage-proc-buf-internal))
  (pop-to-buffer sage-shell:process-buffer))


(defvar sage-shell-edit:-pps-backward-lim 3000)
(defun sage-shell-edit:parse-current-state ()
  "Parse completion state in sage-shell:sage-mode."
  (let* ((pt (point))
         (bd (sage-shell:aif sage-shell-edit:-pps-backward-lim
                 (max (point-min) (- pt it))
               (point-min)))
         ;; Nearest top level block
         (beg-of-block (save-excursion
                         (when (re-search-backward (rx bol (or "_" alnum)) bd t)
                           (match-beginning 0))))
         (state
          (list (cons 'interface "sage")
                (cons 'prefix (sage-shell-interfaces:looking-back-var "sage"))))
         (types nil)
         (chars (sage-shell-interfaces:get "sage" 'var-chars))
         (base-att-beg (sage-shell-cpl:var-base-name-and-att-start "sage"))
         (base-name (car base-att-beg))
         (in-import-line-p
          (save-excursion
            (beginning-of-line)
            (looking-at (rx (0+ space) (group (or "from" "import")) space))))
         (import-or-from (if in-import-line-p
                             (match-string-no-properties 1))))
    (cond
     ;; Import statement
     ((or in-import-line-p
          (and beg-of-block
               (save-excursion
                 (goto-char beg-of-block)
                 (looking-at (rx bol (group (or "from" "import")) space)))))
      (let* ((import-or-from (or import-or-from (match-string-no-properties 1)))
             (from-state-p (string= import-or-from "from")))
        (cond
         ;; Top level modules in sys.path
         ((save-excursion
            (and
             in-import-line-p
             (or (and (not from-state-p)
                      (sage-shell-cpl:-scb-and-looking-at chars (rx "import")))
                 (and from-state-p
                      (sage-shell-cpl:-scb-and-looking-at chars (rx "from"))))))
          (push "modules" types))
         ;; Sub-modules in a module
         ((save-excursion
            (and in-import-line-p base-name
                 (sage-shell-cpl:-scb-and-looking-at
                  (concat chars ".") (rx (or "import" "from") " "))))
          (push "modules" types)
          (sage-shell:push-elmts state 'module-name base-name))
         ;; Top level objects in a module
         ((save-excursion
            (and from-state-p
                 (progn (unless in-import-line-p
                          (goto-char beg-of-block))
                        (sage-shell-cpl:-from-import-state-one-line pt))))
          (push "vars-in-module" types)
          (sage-shell:push-elmts state
            'module-name (match-string-no-properties 1))))))
     ;; Else if base-name is nil, type is '("interface")
     ((null base-name)
      (push "interface" types)
      (let* ((funcall-lim
              (save-excursion
                (when (re-search-backward
                       (rx (or "_" alnum) "(")
                       (sage-shell:aif sage-shell-edit:-pps-backward-lim
                           (max (- (point) it) (point-min))) t)
                  (line-beginning-position))))
             (in-func-call (and funcall-lim
                                (sage-shell:-in-func-call-p
                                 nil funcall-lim))))
        (sage-shell:awhen in-func-call
          (setq state (sage-shell-cpl:-push-in-func-call-state it state))))))

    (sage-shell:push-elmts state
      'types types)
    (cl-check-type state sage-shell-cpl-state)
    ;; Returns state.
    state))


(defvar sage-shell-edit:-eldoc-orig-func nil)

(defun sage-shell-edit:eldoc-function ()
  (let ((orig-eldoc (sage-shell:aif (functionp sage-shell-edit:-eldoc-orig-func)
                        (ignore-errors (funcall it)))))
    (if orig-eldoc
        ;; Prefer original implementation.
        orig-eldoc
      (let* ((state (sage-shell-edit:parse-current-state))
             (in-function-call (sage-shell-cpl:get state 'in-function-call))
             (base-name (sage-shell-cpl:get state 'in-function-call-base-name))
             (sage-int-state '((interface . "sage")
                               (types "interface"))))
        (when (and in-function-call
                   (progn
                     (sage-shell-edit:set-sage-proc-buf-internal nil nil)
                     sage-shell:process-buffer)
                   (get-buffer sage-shell:process-buffer)
                   (sage-shell:in (if base-name
                                      (sage-shell:trim-right
                                       (car (split-string base-name (rx "."))))
                                    in-function-call)
                                  (append
                                   (buffer-local-value
                                    'sage-shell-cpl:-cands-in-current-session
                                    sage-shell:process-buffer)
                                   (or (sage-shell-cpl:candidates
                                        :state sage-int-state)
                                       ;; If cache is not created, update
                                       ;; the command list.
                                       (progn (sage-shell:update-sage-commands)
                                              (sage-shell-cpl:candidates
                                               :state sage-int-state))))))
          (sage-shell:-eldoc-function state))))))


;;; sage-shell:sage-mode
;;;###autoload
(define-derived-mode sage-shell:sage-mode python-mode "Sage"
  (setq sage-shell-edit:-eldoc-orig-func eldoc-documentation-function)
  (set (make-local-variable 'eldoc-documentation-function)
       #'sage-shell-edit:eldoc-function)
  (add-hook 'completion-at-point-functions
            'sage-shell-edit:completion-at-point-func nil t))

(sage-shell:define-keys sage-shell:sage-mode-map
  "C-c C-c" 'sage-shell-edit:send-buffer
  "C-c C-r" 'sage-shell-edit:send-region
  "C-M-x" 'sage-shell-edit:send-defun
  "C-c C-l" 'sage-shell-edit:load-file
  "C-c C-z" 'sage-shell-edit:pop-to-process-buffer
  "C-c C-j" 'sage-shell-edit:send-line*
  "C-c C-d"   'sage-shell:send-doctest)


;;; Alias
;;;###autoload
(defvar sage-shell:func-alias-alist
  '((sage-shell:sage-mode . sage-mode)
    (sage-shell:run-sage . run-sage)
    (sage-shell:run-new-sage . run-new-sage)))

;;;###autoload
(defvar sage-shell:var-alias-alist
  '((sage-shell:sage-mode-map . sage-mode-map)
    (sage-shell:sage-mode-hook . sage-mode-hook)
    (sage-shell:sage-mode-syntax-table . sage-mode-syntax-table)
    (sage-shell:sage-mode-abbrev-table . sage-mode-abbrev-table)))

;;;###autoload
(progn
  (defun sage-shell:define-alias ()
    "Define aliases as follows:
| Original name                     | Alias                  |
|-----------------------------------+------------------------|
| sage-shell:sage-mode              | sage-mode              |
| sage-shell:sage-mode-map          | sage-mode-map          |
| sage-shell:sage-mode-hook         | sage-mode-hook         |
| sage-shell:sage-mode-syntax-table | sage-mode-syntax-table |
| sage-shell:sage-mode-abbrev-table | sage-mode-abbrev-table |
| sage-shell:run-sage               | run-sage               |
| sage-shell:run-new-sage           | run-new-sage           |
|-----------------------------------+------------------------|
"
    (interactive)
    (dolist (c sage-shell:func-alias-alist)
      (defalias (cdr c) (car c)))
    (dolist (c sage-shell:var-alias-alist)
      (defvaralias (cdr c) (car c)))))

;;;###autoload
(add-to-list 'auto-mode-alist `(,(rx ".sage" eos) . sage-shell:sage-mode))


;;; sage-shell-pdb
(defun sage-shell-pdb:send--command (cmd)
  (when (sage-shell-pdb:pdb-prompt-p)
    (sage-shell-edit:exec-command-base
     :command cmd
     :insert-command-p t
     :display-function 'display-buffer
     :callback (lambda ()
                 (with-current-buffer sage-shell:process-buffer
                   (goto-char (process-mark (get-buffer-process
                                             sage-shell:process-buffer))))))))

(eval-when-compile
  (defvar sage-shell-pdb:command-list
    '("next" "step" "where" "up" "down" "until" "continue" "help"
      "run" "quit")))

(defmacro sage-shell-pdb:define--commands ()
  (append '(progn)
          (cl-loop for cmd in sage-shell-pdb:command-list
                   collect
                   `(defun ,(intern (concat
                                     "sage-shell-pdb:input-"
                                     cmd))
                        ()
                      (interactive)
                      ,(format "Input '%s' in the process buffer." cmd)
                      (sage-shell-pdb:send--command ,cmd)))))

;; Define sage-shell-pdb:input-next, etc.
(sage-shell-pdb:define--commands)

(defun sage-shell-pdb:set-break-point-at-point ()
  (interactive)
  (let ((file (buffer-file-name))
        (line (save-restriction
                (widen)
                (line-number-at-pos))))
    (when file
      (sage-shell-pdb:send--command
       (format "break %s:%s" (sage-shell:site-package-version file)
               line)))))

(defun sage-shell-pdb:pdb-prompt-p ()
  (sage-shell-edit:set-sage-proc-buf-internal nil)
  (with-current-buffer sage-shell:process-buffer
    (save-excursion
      ;; goto last prompt
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (forward-line 0)
      (looking-at (rx (or "(Pdb)" "ipdb>") " ")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 Borrowed from Gallina's python.el.                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar sage-shell-pdb:stacktrace-info-regexp
  "> \\([^\"(<]+\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]+\\)()"
  "Regular expression matching stacktrace information.
Used to extract the current line and module being inspected.")

(defvar sage-shell-pdb:tracked-buffer nil
  "Variable containing the value of the current tracked buffer.
Never set this variable directly, use
`sage-shell-pdb:set-tracked-buffer' instead.")

(defvar sage-shell-pdb:buffers-to-kill nil
  "List of buffers to be deleted after tracking finishes.")

(defun sage-shell-pdb:set-tracked-buffer (file-name)
  "Set the buffer for FILE-NAME as the tracked buffer.
Internally it uses the `sage-shell-pdb:tracked-buffer' variable.
Returns the tracked buffer."
  (let ((file-buffer (get-file-buffer
                      (concat (file-remote-p default-directory)
                              file-name))))
    (if file-buffer
        (setq sage-shell-pdb:tracked-buffer file-buffer)
      (setq file-buffer (find-file-noselect file-name))
      (when (not (sage-shell:in file-buffer sage-shell-pdb:buffers-to-kill))
        (push file-buffer sage-shell-pdb:buffers-to-kill)))
    file-buffer))

(defun sage-shell-pdb:comint-output-filter-function ()
  "Move overlay arrow to current pdb line in tracked buffer. "
  (when sage-shell-pdb:activate
    (let* ((full-output (ansi-color-filter-apply
                         (buffer-substring comint-last-input-end (point-max))))
           (line-number)
           (file-name
            (with-temp-buffer
              (insert full-output)
              ;; When the debugger encounters a pdb.set_trace()
              ;; command, it prints a single stack frame.  Sometimes
              ;; it prints a bit of extra information about the
              ;; arguments of the present function.  When ipdb
              ;; encounters an exception, it prints the _entire_ stack
              ;; trace.  To handle all of these cases, we want to find
              ;; the _last_ stack frame printed in the most recent
              ;; batch of output, then jump to the corresponding
              ;; file/line number.
              (goto-char (point-max))
              (when (re-search-backward sage-shell-pdb:stacktrace-info-regexp nil t)
                (setq line-number (string-to-number
                                   (match-string-no-properties 2)))
                (match-string-no-properties 1)))))
      (if (and file-name line-number)
          (let* ((tracked-buffer
                  (sage-shell-pdb:set-tracked-buffer file-name))
                 (shell-buffer (current-buffer))
                 (tracked-buffer-window (get-buffer-window tracked-buffer))
                 (tracked-buffer-line-pos))
            (with-current-buffer tracked-buffer
              (set (make-local-variable 'overlay-arrow-string) "=>")
              (set (make-local-variable 'overlay-arrow-position) (make-marker))
              (setq tracked-buffer-line-pos (progn
                                              (goto-char (point-min))
                                              (forward-line (1- line-number))
                                              (point-marker)))
              (when tracked-buffer-window
                (set-window-point
                 tracked-buffer-window tracked-buffer-line-pos))
              (set-marker overlay-arrow-position tracked-buffer-line-pos))
            (pop-to-buffer tracked-buffer)
            (switch-to-buffer-other-window shell-buffer))
        (when sage-shell-pdb:tracked-buffer
          (with-current-buffer sage-shell-pdb:tracked-buffer
            (set-marker overlay-arrow-position nil))
          (unless (sage-shell-pdb:pdb-prompt-p)
            (mapc #'(lambda (buffer)
                      (ignore-errors (kill-buffer buffer)))
                  sage-shell-pdb:buffers-to-kill))
          (setq sage-shell-pdb:tracked-buffer nil
                sage-shell-pdb:buffers-to-kill nil))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; sagetex
(defun sage-shell-sagetex:add-to-texinputs ()
  "Add $SAGE_ROOT/local/share/texmf/tex/generic/sagetex/ to TEXINPUTS."
  (sage-shell:awhen (sage-shell:sage-root)
    (let ((texinputs (getenv "TEXINPUTS"))
          (sagetexdir (let ((dir1 (expand-file-name
                                   "local/share/texmf/tex/generic/sagetex"
                                   it))
                            (dir2 (expand-file-name
                                   "local/share/texmf/tex/latex/sagetex"
                                   it)))
                        (if (file-exists-p dir1) dir1 dir2))))
      (when (and (not (and texinputs
                           (sage-shell:in sagetexdir
                                          (split-string texinputs ":"))))
                 (file-exists-p sagetexdir))
        (setenv "TEXINPUTS" (concat texinputs sagetexdir ":"))))))

(defun sage-shell-sagetex:tex-to-sagetex-file (f)
  (concat (file-name-sans-extension
           (expand-file-name
            (sage-shell-sagetex:tex-master-maybe f)
            default-directory)) ".sagetex.sage"))

;;;###autoload
(defun sage-shell-sagetex:load-file (filename)
  "Load a .sagetex.sage file to an existing Sage process.
If TeX-master is set, this function uses its variable."
  (interactive
   (list (sage-shell-sagetex:read-latex-file)))
  (let ((dflt (sage-shell-sagetex:tex-to-sagetex-file
               filename)))
    (sage-shell-edit:load-file-base
     :command (format "%s('%s')" (sage-shell:py-mod-func "sage_tex_load")
                      dflt)
     :file-name dflt)))

;;;###autoload
(defun sage-shell-sagetex:load-current-file ()
  (interactive)
  (sage-shell-sagetex:-load-current-file
   'sage-shell-sagetex:load-file))


;;;###autoload
(defalias 'sage-shell:sagetex-load-file 'sage-shell-sagetex:load-file)

(defvar sage-shell-sagetex:latex-command-func
  'sage-shell-sagetex:post-command
  "This varable should be a function with one argument (the file
name of a LaTeX file) which returns a LaTeX command with the
file name.")

(defun sage-shell:TeX-shell ()
  "Name of shell used to parse TeX commands."
  (cond ((boundp 'TeX-shell) TeX-shell)
        ((memq system-type '(ms-dos emx windows-nt)) shell-file-name)
        (t "/bin/sh")))

(defun sage-shell:TeX-shell-command-option ()
  "Shell argument indicating that next argument is the command."
  (cond
   ((boundp 'TeX-shell-command-option) TeX-shell-command-option)
   ((memq system-type '(ms-dos emx windows-nt))
    (cond ((boundp 'shell-command-option)
           shell-command-option)
          ((boundp 'shell-command-switch)
           shell-command-switch)
          (t "/c")))
   (t                                   ;Unix & EMX (Emacs 19 port to OS/2)
    "-c")))

(defun sage-shell-sagetex:tex-master-maybe (f &optional nondir)
  (let* ((b (get-file-buffer f))
         (tm (when (and (bufferp b)
                        (boundp 'TeX-master))
               (buffer-local-value 'TeX-master b))))
    (let ((ms (cond ((and tm (stringp tm))
                     (expand-file-name tm (file-name-directory f)))
                    (t f))))
      (if nondir (file-name-nondirectory ms)
        ms))))

(defun sage-shell-sagetex:pre-command (f)
  (format "%s %s" sage-shell-sagetex:pre-latex-command
          (sage-shell-sagetex:tex-master-maybe f)))

(defun sage-shell-sagetex:-auctex-cmd ()
  "When auctex command is available returns
`sage-shell-sagetex:auctex-command-name' else nil"
  (sage-shell:awhen (and (featurep 'tex)
                         sage-shell-sagetex:auctex-command-name
                         (require 'tex-buf nil t)
                         (with-no-warnings
                           (assoc sage-shell-sagetex:auctex-command-name
                                  TeX-command-list)))
    sage-shell-sagetex:auctex-command-name))

(defun sage-shell-sagetex:post-command (f)
  (sage-shell:aif (sage-shell-sagetex:-auctex-cmd)
      (with-no-warnings
        (TeX-command-expand
         (nth 1 (assoc it TeX-command-list)) 'TeX-master-file))
    (format "%s %s" sage-shell-sagetex:latex-command
            (sage-shell-sagetex:tex-master-maybe f t))))

(defun sage-shell-sagetex:-load-and-run-latex (f)
  (sage-shell:push-to-output-finished-hook
    ;; Run process in the same directory of as f.
    (sage-shell:with-default-directory (file-name-directory f)
      (sage-shell-sagetex:-run-latex f t)))
  (sage-shell-sagetex:load-file f))

(defun sage-shell-sagetex:-run-latex (f &optional verbose)
  (let* ((cmd (let ((b (or (get-file-buffer f)
                           (current-buffer))))
                (with-current-buffer b
                  (funcall sage-shell-sagetex:latex-command-func f))))
         (cmd-name
          (sage-shell:aif (sage-shell-sagetex:-auctex-cmd)
              (format "`%s' %s" it (file-name-nondirectory f))
            cmd)))
    (deferred:$
      (deferred:$
        (deferred:next
          (lambda ()
            (message "Running \"%s\" ..." cmd-name)))
        (deferred:process
          (sage-shell:TeX-shell)
          (sage-shell:TeX-shell-command-option)
          cmd)
        (deferred:nextc it
          (lambda (_x) (when verbose
                         (message "Running \"%s\" ... Done." cmd-name)))))
      (deferred:error it
        (lambda (e) (sage-shell-sagetex:insert-error e))))))

(defmacro sage-shell-sagetex:-run-latex-and-do (f sym)
  `(progn
     (sage-shell-edit:set-sage-proc-buf-internal)
     (let ((f ,f))
       (sage-shell:as-soon-as (sage-shell:output-finished-p)
         (deferred:$
           (deferred:$
             (deferred:process
               (sage-shell:TeX-shell)
               (sage-shell:TeX-shell-command-option)
               (sage-shell-sagetex:pre-command f))
             (deferred:nextc it
               (lambda (_x) (,sym f))))
           (deferred:error it
             (lambda (e) (sage-shell-sagetex:insert-error e))))))))

;;;###autoload
(defun sage-shell-sagetex:compile-file (f)
  "This command runs LaTeX on the current file, loads the
.sagetex.sage file to an existing Sage process and runs LaTeX
again. If TeX-master is set, this function uses its variable.
See the documentation of
`sage-shell-sagetex:latex-command' and
`sage-shell-sagetex:auctex-command-name' for the customization."
  (interactive (list (sage-shell-sagetex:read-latex-file)))
  (sage-shell-sagetex:-run-latex-and-do
   f sage-shell-sagetex:-load-and-run-latex))

(defun sage-shell-sagetex:-load-current-file (func)
  (let ((f (buffer-file-name)))
    (sage-shell:aif (and f (string-match (rx ".tex" eol) f))
        (funcall func f)
      (message "Not valid LaTeX buffer."))))

;;;###autoload
(defun sage-shell-sagetex:compile-current-file ()
  (interactive)
  (sage-shell-sagetex:-load-current-file
   'sage-shell-sagetex:compile-file))

;;;###autoload
(defun sage-shell-sagetex:run-latex-and-load-file (f)
  "This command runs LaTeX and loads a .sagetex.sage file to the
exisiting Sage process. If TeX-master is set, this function uses its variable."
  (interactive (list (sage-shell-sagetex:read-latex-file)))
  (sage-shell-sagetex:-run-latex-and-do
   f sage-shell-sagetex:load-file))

;;;###autoload
(defun sage-shell-sagetex:run-latex-and-load-current-file ()
  (interactive)
  (sage-shell-sagetex:-load-current-file
   'sage-shell-sagetex:run-latex-and-load-file))

(defun sage-shell-sagetex:read-latex-file ()
  (let ((default-file (cond
                       ((and (boundp 'TeX-master)
                             (stringp TeX-master)
                             (file-exists-p TeX-master))
                        (and (boundp 'TeX-master) ;to silence warning
                             (file-name-nondirectory
                              (expand-file-name TeX-master))))
                       ((buffer-file-name) (file-name-nondirectory
                                            (buffer-file-name))))))
    (expand-file-name
     (read-file-name
      "LaTeX File: "
      nil
      default-file
      nil
      default-file
      (lambda (name)
        (string-match (rx ".tex" eol) name))))))


(defun sage-shell-sagetex:insert-error (e)
  (let ((b (get-buffer-create "*SageTeX-error*")))
    (with-current-buffer b
      (let ((inhibit-read-only t)
            (view-read-only nil))
        (erase-buffer)
        (insert (error-message-string e))
        (sage-shell-sagetex:error-mode)))
    (when sage-shell-sagetex:pop-to-error-buffer
      (pop-to-buffer b))))

(defvar sage-shell-sagetex:sage-blocks
  '("sagesilent"
    "sageblock"))

(defsubst sage-shell-sagetex:-env-regexp (beg-or-end)
  (rx-to-string
   `(and
     (0+ whitespace)
     "\\"
     ,beg-or-end
     (0+ whitespace)
     "{"
     (0+ whitespace)
     (regexp ,(regexp-opt sage-shell-sagetex:sage-blocks))
     (0+ whitespace)
     "}")))

(defun sage-shell-sagetex:-tex-env-region ()
  "Return the cons of (beg. end) for the current enveironment.
If the point is not in sageblock or sagesilent then this causes an error.
This requires AUCTEX."
  (let ((beg (with-no-warnings
               (save-excursion
                 (LaTeX-find-matching-begin)
                 (point))))
        (end (with-no-warnings
               (save-excursion
                 (LaTeX-find-matching-end)
                 (point)))))
    (cond ((save-excursion
             (goto-char beg)
             (looking-at-p (sage-shell-sagetex:-env-regexp "begin")))
           (cons (save-excursion
                   (goto-char beg)
                   (re-search-forward
                    (sage-shell-sagetex:-env-regexp "begin"))
                   (point))
                 (save-excursion
                   (goto-char end)
                   (re-search-backward (rx "\\end"))
                   (match-beginning 0))))
          (t (error "Not in a Sage environment.")))))

(defun sage-shell-sagetex:send-environment ()
  "If the point is in environment sagesilent or sageblock, then load the contents of environment using existing Sage process.
This requires AUCTeX."
  (interactive)
  (cl-destructuring-bind (beg . end) (sage-shell-sagetex:-tex-env-region)
    (sage-shell-edit:exec-command-base
     :command (sage-shell-edit--send-obj-command beg end)
     :pre-message (format "Loading the %s to the Sage process..."
                          "current environment")
     :post-message
     (format "Loading the %s to the Sage process... Done."
             "current environment")
     :switch-p nil)))

(define-derived-mode sage-shell-sagetex:error-mode special-mode "SageTeX-Error"
  "Error mode for SageTeX")

(defun sage-shell-blocks:default-keybindings ()
  "Bind default keys for working with Sage blocks.

The following are added to `sage-shell:sage-mode':
  C-M-{      `sage-shell-blocks:backward'
  C-M-}      `sage-shell-blocks:forward'
  C-<return> `sage-shell-blocks:send-current'

The following are added to `sage-shell-mode':
  C-<return> `sage-shell-blocks:pull-next'"
  (define-key sage-shell:sage-mode-map (kbd "C-<return>") 'sage-shell-blocks:send-current)
  (define-key sage-shell:sage-mode-map (kbd "C-M-{")      'sage-shell-blocks:backward)
  (define-key sage-shell:sage-mode-map (kbd "C-M-}")      'sage-shell-blocks:forward)
  (define-key sage-shell-mode-map (kbd "C-<return>")      'sage-shell-blocks:pull-next))
(sage-shell-blocks:default-keybindings)


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

(defun sage-shell-info (&optional file-or-node)
  "Similar to M-x info, but highlights keywords and define some key-bindings."
  (interactive)
  (info file-or-node
        (if (numberp current-prefix-arg)
            (format "*SageInfo*<%s>" current-prefix-arg)
          "*SageInfo*"))
  (sage-shell-info-init))

;; (package-generate-autoloads "sage-shell" default-directory)

(provide 'sage-shell-mode)
;;; sage-shell-mode.el ends here
