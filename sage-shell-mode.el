;;; sage-shell-mode.el --- A front-end for Sage Math

;; Copyright (C) 2012 - 2015 Sho Takemori.
;; Author: Sho Takemori <stakemorii@gmail.com>
;; URL: https://github.com/stakemori/sage-shell-mode
;; Package-Requires: ((cl-lib "0.5") (deferred "0.3.1"))
;; Keywords: Sage, math
;; Version: 0.0.4

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

;; Please visit https://github.com/stakemori/sage-shell-mode for more
;; infomation.

;;; Code:
(eval-when-compile (require 'cl))
(require 'cl-lib)
(require 'deferred)
(require 'pcomplete)

;;; Global variables for users
(defgroup sage-shell
  nil "Run Sage process in a buffer."
  :group 'languages)

(defgroup sage-shell-sagetex
  nil "Group for SageTeX."
  :group 'sage-shell)

(defcustom sage-shell:sage-root nil
  "SAGE_ROOT directory. If the Sage executable in your PATH and (exeutable-find \"sage\") is non-nil, then you do not have to set this variable."
  :group 'sage-shell
  :type '(choice (directory :tag "Directory")
                 (const :tag "Not specified" nil)))

(defcustom sage-shell:sage-executable nil
  "Name of the Sage executable. If the Sage executable in your PATH and (exeutable-find \"sage\") is non-nil, then you do not have to set this variable."
  :group 'sage-shell
  :type '(choice (string :tag "Executable file of Sage")
                 (const :tag "Not specified" nil)))

;;;###autoload
(defvaralias 'sage-shell:command 'sage-shell:sage-executable)

(defcustom sage-shell:input-history-cache-file
  nil
  "If non nil, after invoking `sage-shell:send-eof',`comint-input-ring' is saved to this file."
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
  :type '(choice (const :tag "default" sage-shell:help1)
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
  "If non-nil and the output contains an error line, output-filter-function creates a link to the file where the error is raised."
  :type 'boolean
  :group 'sage-shell)

(defcustom sage-shell:prefer-development-file-p t
  "If non nil, prefer a source file in src directory rather than site-packages directory."
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
  "Non-nil means sage-shell-mode adds $SAGE_ROOT/local/share/texmf/tex/generic/sagetex/ to TEXINPUTS."
  :type 'boolean
  :group 'sage-shell-sagetex)

;;;###autoload
(defvaralias 'sage-shell:add-to-texinputs-p
  'sage-shell-sagetex:add-to-texinputs-p)

(defcustom sage-shell-sagetex:pop-to-error-buffer t
  "Non-nil means pop to the SageTeX error buffer."
  :type 'boolean
  :group 'sage-shell-sagetex)


;;; Anaphoric macros
(defmacro sage-shell:ansetq (&rest rest)
  "Anaphoric setq. REST is a list of sym val sym1 val1... `it' is
the value of last sym"
  (declare (indent 0) (debug t))
  (cond ((eq (length rest) 2) `(let ((it ,(car rest)))
                                 (setq ,(nth 0 rest) ,(nth 1 rest))))
        ((> (length rest) 3) `(let ((it ,(car rest)))
                                (setq ,(nth 0 rest) ,(nth 1 rest))
                                (sage-shell:ansetq ,@(nthcdr 2 rest))))))

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

(cl-defsubst sage-shell:group (l &optional (n 2))
  (let ((r l)
        (a nil)
        (res nil))
    (while r
      (setq a (sage-shell:nthcar-and-rest n r)
            r (cdr a))
      (push (car a) res))
    (nreverse res)))

(cl-defsubst sage-shell:nthcar-and-rest (m l)
  (cl-loop for i from 0 to (1- m)
        for a on l
        collect (car a) into x
        finally (return (cons x a))))

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

(defsubst sage-shell:gensym (&optional prefix)
  "Generate a new uninterned symbol.
The name is made by appending a number to PREFIX, default \"sage-gensym-\"."
  (let ((pfix (if (stringp prefix) prefix "sage-gensym-"))
        (num (if (integerp prefix) prefix
               (prog1 sage-shell:gensym-counter
                 (cl-incf sage-shell:gensym-counter)))))
    (make-symbol (format "%s%d" pfix num))))

(defmacro sage-shell:acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (sage-shell:gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (sage-shell:acond ,@(cdr clauses)))))))

(cl-defmacro sage-shell:with-gensym ((&rest names) &rest body)
  (declare (indent 1) (debug t))
  `(let ,(cl-loop for n in names collect `(,n (sage-shell:gensym)))
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
  `(cond (,form (progn ,@body))
         (t (lexical-let ((timer-sym
                           (intern (format "sage-timer%d"
                                           sage-shell:gensym-counter))))
              (cl-incf sage-shell:gensym-counter)
              (set timer-sym
                   (run-with-timer
                    0.01 0.01
                    (lambda () (when ,form
                             (unwind-protect
                                 (progn ,@body)
                               (cancel-timer (symbol-value timer-sym)))))))))))

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


(defun sage-shell:line-beginning-position ()
  (save-excursion (forward-line 0) (point)))

(cl-defsubst sage-shell:in (elt lst &optional (test 'equal))
  (cl-loop for i in lst
        if (funcall test elt i)
        return i))

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

(defun sage-shell:trim-left (s)
  (if (string-match (rx "\n" buffer-end) s)
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
  (declare (indent 2) (debug t))
  `(let ((default-directory (or (and ,directory
                                     (file-name-as-directory ,directory))
                                default-directory)))
     ,@body))

(defvar sage-shell:sage-modes '(sage-shell:sage-mode sage-shell-mode))

(require 'compile)
(require 'ansi-color)


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
      (sage-shell:acond
       ((stringp sage-shell:sage-root)
        (expand-file-name "sage" sage-shell:sage-root))
       ((executable-find "sage") (file-truename it)))))


(defvar sage-shell:output-finished-regexp
  (concat (rx (group line-start
                   (1+ (and (or "sage:" "sage0:" ">>>" "....:"
                             "(Pdb)" "ipdb>" "(gdb)")
                         " "))
                   line-end))
        "\\|^\\("
        (regexp-opt sage-shell-interfaces:other-interfaces)
        ": $\\)"))

(defvar sage-shell:process-buffer nil)
(make-variable-buffer-local 'sage-shell:process-buffer)

(defvar sage-shell:prompt-regexp
  (rx line-start
      (1+ (and (or "sage:" "sage0:" ">>>" "....:"
                   "(Pdb)" "ipdb>" "(gdb)") " ")))
  "Regular expression matching the Sage prompt.")

(defvar sage-shell:prompt-generic-regexp
  (concat (rx (group line-start
                     (or "sage:" "sage0:" ">>>" "....:"
                         "(Pdb)" "ipdb>" "(gdb)")
                     " "))
          "\\|^\\("
          (regexp-opt sage-shell-interfaces:other-interfaces)
          ": \\)"))

;; cache buffers
(defvar sage-shell-indent:indenting-buffer-name " *sage-indent*")
(defvar sage-shell-cpl:attribute-completion-buffer
  " *sage-attribute-completion*"
  "Buffer name used for collect candidates of attributes of a instance.")
(defvar sage-shell:output-buffer " *sage-output*")

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
        ["Previous Matching Current Input" comint-previous-matching-input-from-input t]
        ["Next Matching Current Input" comint-next-matching-input-from-input t]
        ["Previous Matching Input..." comint-previous-matching-input t]
        ["Next Matching Input..." comint-next-matching-input t]
        ["Backward Matching Input..." comint-backward-matching-input t]
        ["Forward Matching Input..." comint-forward-matching-input t]
        ["Isearch Input String Backward..." comint-history-isearch-backward t]
        ["Isearch Input Regexp Backward..." comint-history-isearch-backward-regexp t]
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
    ,sage-shell:singal-menu-spec))

(defvar sage-shell:menu-defined-p nil)

(defvar sage-shell:output-finished-p nil)
(make-variable-buffer-local 'sage-shell:output-finished-p)
(cl-defun sage-shell:output-finished-p
    (&optional (buffer sage-shell:process-buffer))
  (buffer-local-value 'sage-shell:output-finished-p buffer))

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
  (when sage-shell:scroll-show-maximum-output
    (set (make-local-variable 'scroll-conservatively) 1000))
  ;; Ignore duplicates in command history
  (setq comint-input-ignoredups t)
  (add-hook 'completion-at-point-functions
            'sage-shell:completion-at-point-func nil t)
  (unless sage-shell:menu-defined-p
    (easy-menu-define sage-shell-menu
      sage-shell-mode-map "sage-shell menu"
      sage-shell:menu-spec)
    (setq sage-shell:menu-defined-p t))

  ;; Run init functions after Sage loaded.
  (add-to-list 'sage-shell:output-filter-finished-hook
               (lambda () (sage-shell:after-init-function
                       sage-shell:process-buffer)))
  (sage-shell:pcomplete-setup))

(defvar sage-shell-mode-hook nil "Hook run when entering Sage Shell mode.")

(defun sage-shell:interrupt-subjob ()
  "Interrupt the current subjob."
  (interactive)
  (comint-interrupt-subjob)
  ;; (setq comint-redirect-completed t
  ;;       sage-shell:output-finished-p t)
  )

(sage-shell:define-keys sage-shell-mode-map
  "TAB" 'sage-shell-tab-command
  "C-d" 'sage-shell:delchar-or-maybe-eof
  "RET" 'sage-shell:send-input
  "C-c C-i" 'sage-shell:complete
  "C-c C-h" 'sage-shell:help
  "C-c C-l" 'sage-shell:load-file
  "C-c M-o" 'sage-shell:clear-current-buffer
  "C-c o" 'sage-shell:list-outputs)

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
    (if (and proc (= (point) (marker-position (process-mark proc)))
             (sage-shell:output-finished-p)
             (sage-shell:redirect-finished-p))
        (sage-shell:send-eof)
      (delete-char arg))))

(defun sage-shell:send-eof ()
  "Send an EOF to the current buffer's process."
  (interactive)
  (sage-shell:comint-send-input t t)
  (process-send-eof)
  ;; kill cache buffes
  (cl-loop for bufn in (list sage-shell:output-buffer
                          sage-shell-indent:indenting-buffer-name
                          sage-shell-cpl:attribute-completion-buffer)
        if (get-buffer bufn)
        do (kill-buffer bufn))

  ;; write comint-input-ring
  (comint-write-input-ring))

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
        (t (get-buffer-create sage-shell:output-buffer))))

(defun sage-shell:send-command-sync
  (command &optional process-buffer output-buffer to-string)
  "internal function"
  (let ((out-buf (sage-shell:-make-buf-if-needed output-buffer))
        (proc-buf
         (or process-buffer sage-shell:process-buffer)))
    (with-current-buffer proc-buf
      (sage-shell:wait-for-redirection-to-complete))
    (with-current-buffer out-buf (erase-buffer))
    (with-current-buffer proc-buf
      (sage-shell:redirect-send-cmd-to-proc
       command out-buf proc-buf)
      (sage-shell:wait-for-redirection-to-complete))
    (when to-string
      (sage-shell:with-current-buffer-safe out-buf
        (buffer-string)))))

(defun sage-shell:wait-for-redirection-to-complete
  (&optional msec process-buffer)
  (let ((msec (or msec 1))
        (proc-buf (or process-buffer sage-shell:process-buffer)))
    (sage-shell:awhen (get-buffer proc-buf)
      (with-current-buffer it
        (while (null comint-redirect-completed)
          (accept-process-output nil 0 msec))))))

(defun sage-shell:send-command
  (command &optional process-buffer output-buffer sync)
  "Send COMMAND to PROCESS-BUFFER's process.  PROCESS-BUFFER is a
buffer where process is alive.  If OUTPUT-BUFFER is the exisiting
bufffer then the out put is inserted to the buffer. Otherwise
output buffer is `sage-shell:output-buffer'.
When sync is nill this return a lambda function to get the result."
  (if sync
      (sage-shell:send-command-sync command process-buffer output-buffer)
    (let ((proc-buf (or process-buffer sage-shell:process-buffer))
          (out-buf (sage-shell:-make-buf-if-needed
                    output-buffer)))
      (with-current-buffer out-buf (erase-buffer))
      (with-current-buffer proc-buf
        (sage-shell:wait-for-redirection-to-complete)
        (sage-shell:redirect-send-cmd-to-proc command out-buf proc-buf))
      (lexical-let ((out-buf out-buf))
        (lambda () (sage-shell:with-current-buffer-safe out-buf
                    (buffer-string)))))))

(defun sage-shell:send-command-to-string (command &optional process-buffer)
  "Send process to command and return output as string."
  (sage-shell:send-command-sync command process-buffer nil t))

(defvar sage-shell:run-history nil "sage command history.")

(defvar sage-shell:python-module "emacs_sage_shell"
  "Name of the python module.")

(defun sage-shell:py-mod-func (funcname)
  (format "%s.%s" sage-shell:python-module
          funcname))

(defvar sage-shell:python-script-directory
  (file-name-directory load-file-name))

(defun sage-shell:remove-trailing-slash (s)
  (if (string-match (rx "/" eol) s)
      (substring s 0 -1)
    s))

(defvar sage-shell:init-command-list
  (list
   (format "import %s" sage-shell:python-module)
   (format "sys.path.append('%s')"
           (sage-shell:remove-trailing-slash
            sage-shell:python-script-directory)))
  "Sage command list evaluated after loading Sage.")

(defun sage-shell:start-sage-process (cmd buffer)
  (let ((cmdlist (split-string cmd)))
    (apply 'make-comint-in-buffer "Sage" buffer
           (car cmdlist) nil (cdr cmdlist))))

(defvar sage-shell:init-finished-p nil)
(make-variable-buffer-local 'sage-shell:init-finished-p)

(defvar sage-shell:check--sage-root-ok nil)

(defun sage-shell:after-init-function (buffer)
  "Runs after starting Sage"
  (sage-shell:send-command
   (sage-shell:join-command sage-shell:init-command-list) buffer)
  (setq sage-shell:init-finished-p t)
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
    (sage-shell-sagetex:add-to-texinputs)))

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


(defun sage-shell:shell-buffer-name (new)
  (let* ((buffer-base-name "*Sage*"))
    (if new (generate-new-buffer buffer-base-name)
      (sage-shell:aif (get-buffer buffer-base-name) it
                (generate-new-buffer buffer-base-name)))))

(defun sage-shell:read-command ()
  (unless (and (sage-shell:sage-executable)
               (executable-find (sage-shell:sage-executable)))
    (error sage-shell:exec-path-error-msg))
  (let ((lst (split-string
              (read-from-minibuffer "Run sage (like this): "
                                    "sage" nil nil 'sage-shell:run-history
                                    "sage") " ")))
    (format "%s %s" (sage-shell:sage-executable)
            (mapconcat 'identity (cdr lst) " "))))

(cl-defun sage-shell:run (cmd new &optional
                            (switch-function 'switch-to-buffer))
  "Running Sage function internal.
SIWTCH-FUNCTION is 'no-switch, or a function with one
argument."
  (let ((buf (sage-shell:shell-buffer-name new)))
    (unless (get-buffer-process buf)
      (sage-shell:start-sage-process cmd buf)
      (with-current-buffer buf
        (set-process-filter (get-buffer-process buf) 'sage-shell:output-filter)
        (sage-shell-mode)))
    (unless (eq switch-function 'no-switch)
      (funcall switch-function buf))
    buf))

;;;###autoload
(defun sage-shell:run-sage (cmd)
  (interactive (list (sage-shell:read-command)))
  (sage-shell:run cmd nil))

;;;###autoload
(defun sage-shell:run-new-sage (cmd)
  (interactive (list (sage-shell:read-command)))
  (sage-shell:run cmd t))

(defun sage-shell-tab-command ()
  (interactive)
  (cond
   ((and (not (sage-shell:at-top-level-p))
         (looking-back (concat sage-shell:prompt-regexp " *")))
    (sage-shell-indent:indent-line))
   (t (sage-shell:complete))))

(defvar sage-shell:sage-version nil)
(defun sage-shell:sage-version ()
  (or sage-shell:sage-version
      (let ((str (shell-command-to-string "sage -version")))
        (when (string-match "Sage Version \\([0-9]\\.[0-9]\\)" str)
          (setq sage-shell:sage-version
                (string-to-number (match-string 1 str)))))))

(defvar sage-shell:print-all-att-sage-fn
  (sage-shell:py-mod-func "print_all_attributes"))

(defvar sage-shell:print-all-commands-fn
  (sage-shell:py-mod-func "print_all_commands"))

(defvar sage-shell:dot-sage "~/.sage" "DOT_SAGE directory.")

(defun sage-shell:redirect-finished-p ()
  (buffer-local-value 'comint-redirect-completed sage-shell:process-buffer))

(defvar sage-shell-cpl:-cands-in-current-session nil
  "A cached list of user defined variables in the Sage process buffer.")
(make-variable-buffer-local 'sage-shell-cpl:-cands-in-current-session)

(defvar sage-shell:clear-command-cache-hook nil
  "A hook that runs each time after sage-shell:clear-command-cache is called.")

(defun sage-shell:clear-command-cache ()
  (with-current-buffer sage-shell:process-buffer
    (sage-shell-cpl:set-cmd-lst "sage" nil)
    (setq sage-shell-cpl:-cands-in-current-session nil)
    (sage-shell:clear-completion-sync-cached)
    (run-hooks 'sage-shell:clear-command-cache-hook)))

(defun sage-shell:update-sage-commands ()
  (with-current-buffer sage-shell:process-buffer
    (sage-shell-cpl:set-cmd-lst "sage" nil)
    (sage-shell-cpl:completion-init t :compl-state '((interface . "sage")
                                                     (var-base-name . nil)))
    (sage-shell-cpl:candidates)
    (sage-shell-cpl:get-cmd-lst "sage")))

(defalias 'sage-shell:load-file 'sage-shell-edit:load-file)
(defalias 'sage-shell:attach-file 'sage-shell-edit:attach-file)

(defun sage-shell:send-magic-cmd-base (magic-command objname &optional async)
  "If `async' is nil, return the result as string, otherwise
returns a lamda function with no args to obtain the result."
  (let* ((cmd (format "%%%s %s" magic-command objname)))
    (cond (async (sage-shell:send-command cmd)
                 (lambda ()
                   (sage-shell:with-current-buffer-safe sage-shell:output-buffer
                     (buffer-string))))
          (t (sage-shell:send-command-to-string cmd)))))

(defun sage-shell:insert-action (can)
  (let ((beg (sage-shell:word-at-pt-beg)))
      (delete-region beg (point))
      (insert can)))

(defun sage-shell:word-at-pt-beg (&optional pt)
  (save-excursion
    (when pt (goto-char pt))
    (let ((chars (sage-shell-interfaces:get
                  (sage-shell-interfaces:current-interface)
                  'var-chars)))
      (skip-chars-backward chars) (point))))

(defun sage-shell:word-at-point (&optional pt)
  (buffer-substring (point) (sage-shell:word-at-pt-beg pt)))

(defun sage-shell:help1 ()
  (sage-shell-help:describe-symbol
   (sage-shell-cpl:to-objname-to-send
    (sage-shell:completing-read-commands))))

(defun sage-shell:complete ()
  (interactive)
  (sage-shell:when-process-alive
    (cond
     ((functionp sage-shell:completion-function)
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
            (concat dr branch base))
        filename))))

(defun sage-shell:site-package-version (filename)
  "Inverse to `sage-shell:src-version'"
  (cond ((sage-shell:aand
           (string-match (expand-file-name "src/sage/"
                                           (sage-shell:sage-root))
                         filename)
           (= it 0))
         (let* ((python-dir1
                 (expand-file-name "local/lib/python/site-packages/"
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


;; comint functions
(defun sage-shell:nullify-ring (ring)
  (cl-loop repeat (ring-size ring)
        do (ring-insert ring nil))
  ring)

(defvar sage-shell:output-ring
  (sage-shell:nullify-ring (make-ring 2))
  "An output from a Sage process is decomposed into parts.
This ring remebers the parts.")
(make-variable-buffer-local 'sage-shell:output-ring)

(defun sage-shell:python-syntax-output-p (line)
  "Return non nil if LINE contains a sentence with the python
  syntax. If returned value is non-nil and non-number, then whole
  line is regarded as python sentence. If a number, then it
  indicates the index in LINE where comment starts."
  (cond
   ((not (string-match "'\\|\"" line)) t)
   ((string-match (rx "Error" eow (1+ whitespace) "Traceback") line) t)
   ((string-match (rx "Error" (group ": ")) line) (match-beginning 1))
   ((and (not (string-match (rx (or "\"\"\"" "'''")) line))
         (string-match (rx (or (>= 3 whitespace)
                               "--> ")
                           (1+ digit)
                           (1+ whitespace)) line)) t)))

(defun sage-shell:comment-out-output ()
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        (case-fold-search nil))
    (save-excursion
      (when comint-last-output-start
        (cl-loop initially
                 (goto-char comint-last-output-start)
                 while
                 (and (re-search-forward "^." nil t)
                      (not (save-excursion
                             (forward-line 0)
                             (looking-at sage-shell:output-finished-regexp))))
                 do
                 (let ((p (sage-shell:python-syntax-output-p
                           (buffer-substring (line-beginning-position)
                                             (line-end-position)))))
                   (cond ((not p) (put-text-property
                                   (1- (point)) (point)
                                   'syntax-table (cons 11 nil)))
                         ((numberp p)
                          (setq p (+ p (line-beginning-position)))
                          (put-text-property
                           p (1+ p) 'syntax-table (cons 11 nil))))))))))

(defmacro sage-shell:after-output-finished (&rest body)
  (declare (indent 0))
  `(cond ((sage-shell:output-finished-p)
          (with-current-buffer sage-shell:process-buffer
            (progn ,@body)))
         (t (with-current-buffer sage-shell:process-buffer
              (add-to-list 'sage-shell:output-filter-finished-hook
                           (lambda () ,@body))))))

(defmacro sage-shell:after-redirect-finished (&rest body)
  (declare (indent 0))
  `(cond ((sage-shell:redirect-finished-p)
          (with-current-buffer sage-shell:process-buffer
            (progn ,@body)))
         (t (with-current-buffer sage-shell:process-buffer
              (add-to-list 'sage-shell:redirect-filter-finished-hook
                           (lambda () ,@body))))))

(defun sage-shell:run-hook-and-remove (hook process)
  (unwind-protect
      (cl-loop for f in (nreverse (symbol-value hook)) do (funcall f))
    (with-current-buffer (process-buffer process)
      (set hook nil))))

(defun sage-shell:ansi-color-filter-apply (string)
  (let* ((ansi-color-context nil)
         (res (ansi-color-filter-apply string)))
    (cond ((not (or sage-shell:init-finished-p
                    sage-shell:use-unicode-banner))
           (sage-shell:->> res
                     (replace-regexp-in-string (rx (or "─" "━"
                                                       "└" "┌"
                                                       "┐" "┘"))
                                               "-")
                     (replace-regexp-in-string (rx "│") "|")))
          (t res))))

;; In recent version comint.el,
;; `comint-redirect-original-filter-function` is removed.
(defvar sage-shell:comint-redirect-original-filter-function nil)

(defun sage-shell:output-filter (process string)
  (let ((oprocbuf (process-buffer process)))
    (sage-shell:with-current-buffer-safe (and string oprocbuf)
      (let ((string (sage-shell:ansi-color-filter-apply string))
            (win (get-buffer-window (process-buffer process))))
        (save-selected-window
          (when (and (windowp win) (window-live-p win))
            (select-window win))
          (sage-shell:output-filter-no-rdct process string))
        (when sage-shell:output-finished-p
          (when sage-shell:scroll-to-the-bottom
            (comint-postoutput-scroll-to-bottom string))
          (sage-shell:run-hook-and-remove
           'sage-shell:output-filter-finished-hook process))))))

(defvar sage-shell:redirect-restore-filter-p t)
(make-variable-buffer-local 'sage-shell:redirect-restore-filter-p)

(defvar sage-shell:attach-file-reloading-regexp
  (rx bol "### reloading attached file " (1+ nonl) "modified at "
      (1+ (or num ":"))  " ###"))

(defun sage-shell:output-filter-no-rdct (process string)
  ;; Insert STRING
  (let ((inhibit-read-only t)
        ;; The point should float after any insertion we do.
        (saved-point (copy-marker (point) t)))

    ;; We temporarily remove any buffer narrowing, in case the
    ;; process mark is outside of the restriction
    (save-restriction
      (widen)

      (goto-char (process-mark process))
      (set-marker comint-last-output-start (point))
      ;; insert-before-markers is a bad thing. XXX
      ;; Luckily we don't have to use it any more, we use
      ;; window-point-insertion-type instead.
      (insert string)

      ;; Advance process-mark
      (set-marker (process-mark process) (point))

      (unless comint-inhibit-carriage-motion
        ;; Interpret any carriage motion characters (newline, backspace)
        (comint-carriage-motion comint-last-output-start (point)))

      ;; Run these hooks with point where the user had it.
      (goto-char saved-point)
      (unless (string= string "")
        ;; push output to `sage-shell:output-ring'
        (ring-insert sage-shell:output-ring string)

        (let ((output (concat (ring-ref sage-shell:output-ring 1)
                              (ring-ref sage-shell:output-ring 0))))
          (when (string-match sage-shell:output-finished-regexp output)
            (setq sage-shell:output-finished-p t))
          (when (string-match sage-shell:attach-file-reloading-regexp string)
            (sage-shell:clear-command-cache)
            (sage-shell:output-filter process "sage: ")))

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

        (sage-shell-indent:indent-function string))
      (run-hook-with-args 'comint-output-filter-functions string)
      (set-marker saved-point (point))

      (goto-char (process-mark process)) ; in case a filter moved it

      (when sage-shell:output-finished-p
        (let ((lbp (sage-shell:line-beginning-position)))
          ;; Delete duplicate propmpt
          (when (get-text-property lbp 'read-only)
            (delete-region lbp comint-last-output-start))
          ;; Highlight the prompt
          (sage-shell:highlight-prompt lbp)

          ;; create links in the output buffer.
          (when sage-shell:make-error-link-p
            (sage-shell:make-error-links comint-last-input-end (point)))
          (sage-shell-pdb:comint-output-filter-function string)))
      ;; sage-shell:output-filter-finished-hook may change the current buffer.
      (with-current-buffer (process-buffer process)
        (goto-char saved-point)))))

(defun sage-shell:highlight-prompt (prompt-start)
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (when comint-prompt-read-only
      (or (= (point-min) prompt-start)
          (get-text-property (1- prompt-start) 'read-only)
          (put-text-property
           (1- prompt-start) prompt-start 'read-only 'fence))
      (add-text-properties
       prompt-start (point)
       '(read-only t rear-nonsticky t front-sticky (read-only) field output)))
    (unless (bolp)
      (let ((ov (make-overlay prompt-start (point))))
        (overlay-put ov
                     'font-lock-face 'comint-highlight-prompt)))))

(defun sage-shell:-delete-output (pt)
  "Delete region between pt and process-mark"
  (let ((proc (get-buffer-process (current-buffer)))
        (replacement nil)
        (inhibit-read-only t))
    (save-excursion
      (let ((pmark (progn (goto-char (process-mark proc))
                          (forward-line 0)
                          (point-marker))))
        (delete-region pt pmark)
        (goto-char (process-mark proc))
        (setq replacement (buffer-substring pmark (point)))
        (delete-region pmark (point))))
    (sage-shell:clear-completion-sync-cached)
    ;; Output message and put back prompt
    (sage-shell:output-filter proc replacement)))

(defun sage-shell:clear-current-buffer ()
  "Delete all output in the current buffer. This does not delete the last prompt."
  (interactive)
  (sage-shell:-delete-output (point-min)))

(defun sage-shell:delete-output ()
  "Delete all output from interpreter since last input.
Does not delete the prompt."
  (interactive)
  (sage-shell:-delete-output comint-last-input-end))

(defmacro sage-shell:font-lock-when-sage-line (&rest forms)
  `(when (save-match-data
           (save-excursion
             (forward-line 0)
             (looking-at sage-shell:prompt-regexp)))
     ,@forms))

(defun sage-shell:font-lock-matcher-keywords (lim)
  (sage-shell:font-lock-when-sage-line
   (re-search-forward
    (rx symbol-start
        (or
         "and" "del" "from" "not" "while" "as" "elif" "global" "or" "with"
         "assert" "else" "if" "pass" "yield" "break" "except" "import" "class"
         "in" "raise" "continue" "finally" "is" "return" "def" "for" "lambda"
         "try"
         ;; Python 2:
         "print" "exec"
         ;; Python 3:
         ;; False, None, and True are listed as keywords on the Python 3
         ;; documentation, but since they also qualify as constants they are
         ;; fontified like that in order to keep font-lock consistent between
         ;; Python versions.
         "nonlocal"
         ;; Extra:
         "self") symbol-end)
    lim t)))

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

(defun sage-shell:font-lock-matcher-builtin (lim)
  (sage-shell:font-lock-when-sage-line
   (re-search-forward
    (rx symbol-start
        (or
         "abs" "all" "any" "bin" "bool" "callable" "chr" "classmethod"
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
         "__all__" "__doc__" "__name__" "__package__")
        symbol-end)
    lim t)))

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
    (sage-shell:font-lock-matcher-builtin . font-lock-builtin-face)))

(defvar sage-shell:redirect-last-point nil)

(defun sage-shell:redirect-filter (process input-string)
  (when process
    (let ((proc-buf (process-buffer process))
          (input-string (sage-shell:ansi-color-filter-apply input-string)))
      (with-current-buffer proc-buf
        (let ((out-buf comint-redirect-output-buffer)
              (f-regexp sage-shell:output-finished-regexp))
          (set-buffer out-buf)
          ;; Send output to all registered buffers
          ;; Go to the end of the buffer
          (goto-char (point-max))
          ;; Insert the output
          (let ((inhibit-read-only t)
                (view-read-only nil))
            (insert input-string))

          ;; If we see the prompt, tidy up
          (when (save-excursion
                  (sage-shell:aif sage-shell:redirect-last-point
                      (progn (goto-char it)
                             (forward-line -1))
                    (goto-char (point-min)))
                  (re-search-forward f-regexp nil t))
            (replace-match "")
            (set-buffer proc-buf)
            (sage-shell:redirect-cleanup)
            (sage-shell:run-hook-and-remove
             'sage-shell:redirect-filter-finished-hook process))
          ;; sage-shell:redirect-filter-finished-hook may change the current buffer
          (with-current-buffer proc-buf
              (setq sage-shell:redirect-last-point (point))))))))

(defun sage-shell:prepare-for-redirect (proc  output-buffer
                                              &optional filter)
  "Assumes evaluated in process buffer of PROC"
  ;; Make sure there's a prompt in the current process buffer
  (and comint-redirect-perform-sanity-check
       (save-excursion
         (goto-char (point-max))
         (or (re-search-backward comint-prompt-regexp nil t)
             (error "No prompt found."))))

  ;; Set up for redirection
  (setq sage-shell:redirect-last-point nil)
  (let ((mode-line-process mode-line-process))
    (comint-redirect-setup
     output-buffer
     (current-buffer)                   ; Comint Buffer
     comint-redirect-finished-regexp    ; Finished Regexp
     nil))                              ; Echo input

  (when filter
    ;; Set the filter
    (setq sage-shell:comint-redirect-original-filter-function ; Save the old filter
          (process-filter proc))
    (set-process-filter proc filter)))

(cl-defun sage-shell:redirect-cleanup ()
  (when sage-shell:redirect-restore-filter-p
    (set-process-filter (get-buffer-process (current-buffer))
                        sage-shell:comint-redirect-original-filter-function))

  ;; Set the completed flag
  (setq comint-redirect-completed t))

(defun sage-shell:redirect-send-cmd-to-proc (command output-buffer process)
  (let* (;; The process buffer
         (process-buffer (if (processp process)
                             (process-buffer process)
                           process))
         (proc (get-buffer-process process-buffer)))
    ;; Change to the process buffer
    (with-current-buffer process-buffer

      (sage-shell:prepare-for-redirect proc output-buffer
                                      'sage-shell:redirect-filter)
      ;; Send the command
      (process-send-string (current-buffer) (concat command "\n")))))

(defun sage-shell:comint-send-input (&optional no-newline artificial)
  "This function is almost same as `comint-send-input'. But this
function does not highlight the input."
  (interactive)
  ;; Note that the input string does not include its terminal newline.
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (error "Current buffer has no process")
      (widen)
      (let* ((pmark (process-mark proc))
             (intxt (if (>= (point) (marker-position pmark))
                        (progn (if comint-eol-on-send (end-of-line))
                               (buffer-substring pmark (point)))
                      (let ((copy (funcall comint-get-old-input)))
                        (goto-char pmark)
                        (insert copy)
                        copy)))
             (input (if (not (eq comint-input-autoexpand 'input))
                        ;; Just whatever's already there.
                        intxt
                      ;; Expand and leave it visible in buffer.
                      (comint-replace-by-expanded-history t pmark)
                      (buffer-substring pmark (point))))
             (history (if (not (eq comint-input-autoexpand 'history))
                          input
                        ;; This is messy 'cos ultimately the original
                        ;; functions used do insertion, rather than return
                        ;; strings.  We have to expand, then insert back.
                        (comint-replace-by-expanded-history t pmark)
                        (let ((copy (buffer-substring pmark (point)))
                              (start (point)))
                          (insert input)
                          (delete-region pmark start)
                          copy))))

        (unless no-newline
          (insert ?\n))

        (comint-add-to-input-history (substring-no-properties
                                      history))

        (run-hook-with-args 'comint-input-filter-functions
                            (if no-newline input
                              (concat input "\n")))

        (let ((beg (marker-position pmark))
              (end (if no-newline (point) (1- (point))))
              (inhibit-modification-hooks t))
          (when (> end beg)
            (unless comint-use-prompt-regexp
              ;; Give old user input a field property of `input', to
              ;; distinguish it from both process output and unsent
              ;; input.  The terminating newline is put into a special
              ;; `boundary' field to make cursor movement between input
              ;; and output fields smoother.
              (add-text-properties
               beg end
               '(mouse-face highlight
                 help-echo "mouse-2: insert after prompt as new input"))))
          (unless (or no-newline comint-use-prompt-regexp)
            ;; Cover the terminating newline
            (add-text-properties end (1+ end)
                                 '(rear-nonsticky t
                                   field boundary
                                   inhibit-line-move-field-capture t))))

        (comint-snapshot-last-prompt)

        (setq comint-save-input-ring-index comint-input-ring-index)
        (setq comint-input-ring-index nil)
        ;; Update the markers before we send the input
        ;; in case we get output amidst sending the input.
        (set-marker comint-last-input-start pmark)
        (set-marker comint-last-input-end (point))
        (set-marker (process-mark proc) (point))
        ;; clear the "accumulation" marker
        (set-marker comint-accum-marker nil)
        (let ((comint-input-sender-no-newline no-newline))
          (funcall comint-input-sender proc input))

        ;; Optionally delete echoed input (after checking it).
        (when (and comint-process-echoes (not artificial))
          (let ((echo-len (- comint-last-input-end
                             comint-last-input-start)))
            ;; Wait for all input to be echoed:
            (while (and (accept-process-output proc)
                        (> (+ comint-last-input-end echo-len)
                           (point-max))
                        (zerop
                         (compare-buffer-substrings
                          nil comint-last-input-start
                          (- (point-max) echo-len)
                          ;; Above difference is equivalent to
                          ;; (+ comint-last-input-start
                          ;;    (- (point-max) comint-last-input-end))
                          nil comint-last-input-end (point-max)))))
            (if (and
                 (<= (+ comint-last-input-end echo-len)
                     (point-max))
                 (zerop
                  (compare-buffer-substrings
                   nil comint-last-input-start comint-last-input-end
                   nil comint-last-input-end
                   (+ comint-last-input-end echo-len))))
                ;; Certain parts of the text to be deleted may have
                ;; been mistaken for prompts.  We have to prevent
                ;; problems when `comint-prompt-read-only' is non-nil.
                (let ((inhibit-read-only t))
                  (delete-region comint-last-input-end
                                 (+ comint-last-input-end echo-len))
                  (when comint-prompt-read-only
                    (save-excursion
                      (goto-char comint-last-input-end)
                      (comint-update-fence)))))))

        ;; This used to call comint-output-filter-functions,
        ;; but that scrolled the buffer in undesirable ways.
        (run-hook-with-args 'comint-output-filter-functions "")))))


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
          ;; If line contains triple quotes, the indent function
          ;; raises an error.
          (if (string-match (rx (repeat 3 (or"'" "\""))) line)
              (newline)
            (newline-and-indent)))))))

(defun sage-shell:prepare-for-send ()
    (sage-shell:wait-for-redirection-to-complete)

    (sage-shell:nullify-ring sage-shell:output-ring)
    (setq sage-shell:output-finished-p nil))

(defun sage-shell-update-sage-commands-p (line)
  (string-match (rx symbol-start (or "import" "reload" "attach" "load")
                    symbol-end) line))

(defvar sage-shell:clear-commands-regexp
  (rx bol "clear" (zero-or-more space) eol))

;; This function has many side effects:
;; * Set `sage-shell:input-ring-index'.
;; * Set `sage-shell:output-finished-p'.
;; * Fill sage-shell:output-ring with nil.
;; * If current line is like '****?' then pop to the help buffer.
;; * Send current line to indenting buffer.
;; * (comint-send-input)
;; * change default-directory

(defun sage-shell:send-input ()
  "Send current line to Sage process. "
  (interactive)
  (when sage-shell:init-finished-p
    (let ((line (buffer-substring (point-at-bol) (line-end-position)))
          (inhibit-read-only t)
          (at-tl-in-sage-p (sage-shell:at-top-level-and-in-sage-p)))

      ;; If we're currently completing, stop.  We're definitely done
      ;; completing, and by sending the input, we might cause side effects
      ;; that will confuse the code running in the completion
      ;; post-command-hook.
      (when (and (fboundp 'completion-in-region-mode)
                 (boundp 'completion-in-region-mode)
                 completion-in-region-mode)
        (completion-in-region-mode -1))

      (sage-shell:prepare-for-send)
      ;; Since comint-send-input sets comint-input-ring-index to nil,
      ;; restore its value
      (setq sage-shell:input-ring-index comint-input-ring-index)

      ;; If current line contains %gap, gap.console(), gap.interact(), %gp, ...
      ;; then create completion buffer
      (sage-shell:awhen (sage-shell-cpl:switch-to-another-interface-p line)
        (sage-shell-cpl:completion-init
         t :compl-state `((interface . ,it) (var-base-name . nil))))


      ;; if current line is ***? and current interface is sage then
      ;; show help or find-file-read-only source file.
      (cond
       ((and at-tl-in-sage-p
             (string-match (rx bol (zero-or-more blank)
                               (group (1+ (or alnum "_" "." "[" "]")))
                               (zero-or-more blank)
                               (group (1+ "?"))
                               (zero-or-more blank) eol) line))
        (if (> (length (match-string 2 line)) 1)
            (sage-shell:find-source-in-view-mode (match-string 1 line))
          (sage-shell-help:describe-symbol (match-string 1 line)))
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
        (sage-shell-help:describe-symbol (match-string 1 line) "help(%s)")
        (sage-shell:send-blank-line))

       ;; send current line to indenting buffer and to process normally
       (t (sage-shell:send-line-to-indenting-buffer-and-indent line)
          (sage-shell:comint-send-input)))
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
                                 eol) line)
               (cd "~")))

        (sage-shell-cpl:-add-to-cands-in-cur-session line)
        (when (string-match sage-shell:clear-commands-regexp line)
          (sage-shell:clear-current-buffer))))))

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
  (with-current-buffer sage-shell:process-buffer
    (let ((comint-input-sender
           (lambda (proc str) (comint-simple-send proc "")))
          (win (get-buffer-window sage-shell:process-buffer)))
      (if (and (windowp win)
               (window-live-p win))
          (with-selected-window win
            (sage-shell:comint-send-input))
        (sage-shell:comint-send-input)))))

(defun sage-shell:at-top-level-p ()
  (save-excursion
    (forward-line 0)
    (or (looking-at sage-shell:prompt1-regexp)
        (cl-loop for i in sage-shell-interfaces:other-interfaces
              thereis (looking-at (format "^%s: " i))))))

(defun sage-shell:at-top-level-and-in-sage-p ()
  "returns non nil if and only if current interface is sage and
the current line is not in a block."
  (and (equal (sage-shell-interfaces:current-interface) "sage")
       (save-excursion
         (forward-line 0)
         (looking-at sage-shell:prompt1-regexp))))

(defun sage-shell-indent:get-indenting-buffer ()
  "Return a temporary buffer set in python-mode. Create one if necessary."
  (let ((buf (get-buffer-create sage-shell-indent:indenting-buffer-name)))
    (set-buffer buf)
    (unless (eq major-mode 'python-mode)
      (let ((python-mode-hook nil)) (python-mode)))
    buf))

(defun sage-shell-indent:indent-function (string)
  "Insert indentation string if sage-shell:prompt2-regexp regexp
matches last process output."
  (when (save-excursion
          (forward-line 0)
          (looking-at sage-shell:prompt2-regexp))
    (let ((indent-str nil))
      (with-current-buffer (sage-shell-indent:get-indenting-buffer)
        (setq indent-str (buffer-substring (point-at-bol) (point))))
      (when (get-buffer-process sage-shell:process-buffer)
        (goto-char (process-mark (get-buffer-process (current-buffer)))))
      (insert indent-str))))


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
                              "Init docstring")
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
  "C-c C-j" 'sage-shell-help:send-current-line
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
                (case sym
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


(cl-defun sage-shell-help:describe-symbol (symbol &optional
                                                  (cmd "%s?"))
  "Describe symbol, display help buffer and select the window."
  (let* ((buf (get-buffer-create sage-shell-help:help-buffer-name))
         (cmd-str (format cmd symbol))
         (str (sage-shell:send-command-to-string cmd-str))
         (proc (current-buffer)))
    (when (string-match sage-shell-help:symbol-not-found-regexp str)
      (error (format "Object %s not found." symbol)))

    (let ((inhibit-read-only t)
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
          (sage-shell-help:make-forward-back-button))))))

(defun sage-shell-help:send-current-line ()
  "In the help buffer, if current line contains a string 'sage:',
send current line to Sage process buffer."
  (interactive)
  (let ((line (save-excursion
                (beginning-of-line)
                (when (re-search-forward
                       (rx "sage: " (group (1+ nonl))) nil
                       (line-end-position))
                  (match-string-no-properties 1)))))
    (sage-shell:awhen line
      (sage-shell-edit:exec-command-base :command it :insert-command-p t
                                   :display-function 'display-buffer))))


;;; make err link
(defvar sage-shell:make-err-link--line-regexp
  (rx bol (group (1+ (regexp "[^
 ]")) (or ".pyc" ".pyx" ".py" ".so")) eow " in"))

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
                                  (rx bol (1+ "-") ">"
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
      (lexical-let ((func-ls func-ls))
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
          (forward-line 0))))))


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
                       (lambda (x) "-")
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


(defun sage-shell:-inputs-outputs ()
  (let ((s (sage-shell:send-command-to-string
            (sage-shell:py-mod-func
             (format "print_inputs_outputs(%s, '%s', %s)"
                     (or sage-shell:list-outputs-max-line-num
                         "None")
                     sage-shell:lo-delim
                     (if sage-shell:list-outputs-reversed-order-p
                         "True"
                       "False"))))))
    (butlast (split-string s sage-shell:lo-delim))))


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
                       ((equal arg 0) (car cr))
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
    ;; goto last prompt
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (forward-line 0)
    (sage-shell:->> (sage-shell:aif (cl-loop for str in sage-shell-interfaces:other-interfaces
                              if (looking-at (concat str ": ")) return str)
                  it
                (if (looking-at sage-shell:prompt-regexp)
                    "sage"
                  ;; otherwise
                  (let ((lstline (ring-ref comint-input-ring 0)))
                    (when (string-match
                           (rx (group (1+ alnum)) (or "." "_") "console")
                           lstline)
                      (match-string-no-properties 1 lstline)))))
              sage-shell-cpl:interface-trans)))

;; Define many global variables
(cl-loop for i in (append sage-shell-interfaces:other-interfaces '("sage"))
      do
      (set (intern (format "sage-shell-cpl:%s-info" i))
           (list
            ;; command regexp
            (cons 'cmd-rxp "[a-zA-Z0-9_]+")
            (cons 'var-chars "a-zA-Z0-9_")
            (cons 'completion-buffer nil)
            ;; if 'trait_names' has the argument 'verbose' then its message.
            (cons 'verbose nil)
            ;; cache file
            (cons 'cache-file nil)
            ;; Parse document help (used for pop up help)
            (cons 'doc-trans 'sage-shell-ac:doc-trans-generic))))

(defun sage-shell-interfaces:set (interface &rest attributes-values)
  (when (sage-shell:in interface (cons "sage" sage-shell-interfaces:other-interfaces))
    (let ((alist (symbol-value
                  (intern (format "sage-shell-cpl:%s-info" interface)))))
      (cl-loop for (att val) in (sage-shell:group attributes-values)
            do
            (sage-shell:aif (assoc att alist)
                (setcdr it val)
              (error (format "No such attribute %S" att)))
            finally return val))))

(defun sage-shell-interfaces:get (interface attribute)
  (when (sage-shell:in interface (cons "sage" sage-shell-interfaces:other-interfaces))
    (let ((alist (symbol-value
                  (intern (format "sage-shell-cpl:%s-info" interface)))))
      (sage-shell:aif (assoc attribute alist)
          (cdr-safe it)
        (error (format "No such attribute %S" attribute))))))

(defun sage-shell-interfaces:looking-back-var (interface)
  (let ((rgexp (sage-shell-interfaces:get interface 'cmd-rxp))
        (chars (sage-shell-interfaces:get interface 'var-chars)))
    (save-excursion
      (when (and (not (equal (skip-chars-backward chars) 0))
                 (looking-at rgexp))
        (point)))))

;; set completion buffer name
(cl-loop for itf in (append '("sage") sage-shell-interfaces:other-interfaces)
      do (sage-shell-interfaces:set itf
                                    'completion-buffer
                                    (format " *sage-%s-compeltion*" itf)))

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
(defvar sage-shell-cpl:current-state-default
  (list
   ;; name of the interface (string)
   (cons 'interface nil)
   ;; nil or the point of the beggining of completion
   (cons 'prefix nil)
   ;; nil or the base name of the variable name
   (cons 'var-base-name nil)
   ;; non-nil means use the command list of the current interface
   ;; for candidates.
   (cons 'use-cmd-lst nil)))

(defun sage-shell-cpl:state-default ()
  (cl-loop for (a . b) in sage-shell-cpl:current-state-default
           collect (cons a b)))

(defvar sage-shell-cpl:current-state (sage-shell-cpl:state-default))

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
                                          ((eq b nil) "None"))))
           ", ")))

(defun sage-shell:-to-python-list (ls)
  (format "[%s]"
          (mapconcat 'identity
                     (cl-loop for a in ls
                              collect (format "%S" a))
                     ", ")))

(make-variable-buffer-local 'sage-shell-cpl:current-state)


(defmacro sage-shell-cpl:get (state attribute)
  `(sage-shell:aif (assoc ,attribute
                          (buffer-local-value ',state
                                              sage-shell:process-buffer))
      (cdr-safe it)
    (error (format "No such attribute %S" ,attribute))))


(defun sage-shell-cpl:get-current (attribute)
  (sage-shell-cpl:get sage-shell-cpl:current-state
                      attribute))

(defun sage-shell-cpl:set (state &rest attributes-values)
  (with-current-buffer sage-shell:process-buffer
    (cl-loop for (att val) in (sage-shell:group attributes-values)
             do
             (sage-shell:aif (assoc att state)
                 (setcdr it val)
               (error (format "No such attribute %S" att)))
             finally return val)))

(defun sage-shell-cpl:set-current (&rest attributes-values)
  (apply 'sage-shell-cpl:set sage-shell-cpl:current-state
         attributes-values))

(defun sage-shell-cpl:completion-buffer-alist (interface var-base-name)
  "Return the alist of completion buffers. If cdr is nil, then it
is the buffer for the candidates of attribute."
  (let* ((other-interface-p
          (sage-shell:in interface sage-shell-interfaces:other-interfaces))
         (alst
          (list
           (cons var-base-name
                 (cons sage-shell-cpl:attribute-completion-buffer nil))
           (cons other-interface-p
                 (cons (sage-shell-interfaces:get
                        interface 'completion-buffer) interface))
           (cons (and (not other-interface-p) (not var-base-name))
                 (cons (sage-shell-interfaces:get
                        "sage" 'completion-buffer) "sage")))))
    (cl-loop with lst
          for (predicate . buffer) in alst
          if predicate
          do (push buffer lst)
          finally return lst)))

(defun sage-shell-cpl:var-base-name-and-att-start (&optional pt)
  "Returns cons of the base name of the variable and the point of
   beginig of the attribute. For example, if there is a python
   code 'abc.de' and the point is at 'd' or 'e' and 'abc' does
   not call any functions, this returns cons of a string 'abc'
   and the point at 'd', otherwise nil."
  (let ((pt (or pt (point)))
        (cm-bol (save-excursion (comint-bol)))
        (var-chars (sage-shell-interfaces:get
                    (sage-shell-interfaces:current-interface)
                    'var-chars))
        att-beg base-end)
    (save-excursion
      (goto-char pt)
      (skip-chars-backward var-chars)
      (setq att-beg (point))
      (when (looking-back (rx "." (0+ whitespace)))
        (save-excursion
          (skip-chars-backward " " cm-bol)
          (forward-char -1)
          (skip-chars-backward " " cm-bol)
          (setq base-end (point)))
        (sage-shell:awhen (sage-shell-cpl:base-name-att-beg-rec var-chars)
          (let ((base-name (buffer-substring-no-properties it base-end)))
            (unless (or (string= base-name "")
                        ;; when base-name does not call any functions
                        (string-match (rx (or (group (or alnum "_")
                                                     (0+ whitespace)
                                                     "(")
                                              "%"
                                              ;; There exists a possibility
                                              ;; that base-name contains '..'.
                                              (and "." (0+ whitespace)
                                                   ".")))
                                      base-name))
              (cons base-name att-beg))))))))

(defun sage-shell-cpl:base-name-att-beg-rec (var-chars)
  (let ((com-bol (save-excursion (comint-bol))))
    (save-excursion
      (skip-chars-backward var-chars)
      (if (not (looking-back (rx "." (0+ whitespace))))
          (point)
        (forward-char -1)
        (skip-chars-backward " " com-bol)
        (if (looking-back (rx (or ")" "]")))
            (when (ignore-errors (backward-list))
              (skip-chars-backward " " com-bol)
              (sage-shell-cpl:base-name-att-beg-rec var-chars))
          (skip-chars-backward " " com-bol)
          (sage-shell-cpl:base-name-att-beg-rec var-chars))))))

(defvar sage-shell-cpl:-all-cmds-delim-start
  (lambda (i) (format ";; Start of command list for %s" i)))

(defvar sage-shell-cpl:-all-cmds-delim-end
  (lambda (i) (format ";; End of command list for %s" i)))

(defun sage-shell:cpl:print-all-att-code (var-base-name)
  (format "%s('%s')" sage-shell:print-all-att-sage-fn var-base-name))

(defun sage-shell-cpl:print-all-commands-code (interface)
  (format "%s('%s', delim_start='%s', delim_end='%s')"
          sage-shell:print-all-commands-fn interface
          (funcall sage-shell-cpl:-all-cmds-delim-start interface)
          (funcall sage-shell-cpl:-all-cmds-delim-end interface)))

(defun sage-shell-cpl:prefix ()
  (setq sage-shell-cpl:current-state (sage-shell-cpl:state-default))
  (when (and (get-buffer-process sage-shell:process-buffer)
             (sage-shell-interfaces:current-interface))
    (let* ((case-fold-search nil)
           (base-and-beg (sage-shell-cpl:var-base-name-and-att-start))
           (base-name (car-safe base-and-beg))
           (att-beg (cdr-safe base-and-beg))
           (cur-intf (sage-shell-interfaces:current-interface))
           (itfcs sage-shell-interfaces:other-interfaces)
           (intf (unless att-beg
                   (or (sage-shell:in cur-intf itfcs)
                       (save-excursion
                         (let ((pt (point)))
                           (forward-line 0)
                           (sage-shell:awhen (re-search-forward
                                        (format "\\<%s\\(?:\\.eval\\)? *\\((\\)[^)\n]+"
                                                (regexp-opt itfcs 1)) nil t)
                             (when (and (<= (match-end 2) pt)
                                        (<= pt it))
                               (match-string-no-properties 1)))))))))
      (cond
       ;; when the word at point is an attribute
       ((and att-beg (sage-shell:at-top-level-and-in-sage-p))
        (sage-shell-cpl:set-current 'var-base-name base-name)
        (if (sage-shell:in base-name itfcs)
            (sage-shell-cpl:set-current 'interface base-name
                                        'use-cmd-lst t)
          (sage-shell-cpl:set-current 'interface "sage"))
        (sage-shell-cpl:set-current 'prefix att-beg))
       ;; when current interface is not sage or the point is
       ;; in a function one of gp.eval, gp, gap.eval, ...
       (intf
        (sage-shell-cpl:set-current 'interface intf)
        (sage-shell-cpl:set-current
         'use-cmd-lst t
         'var-base-name nil
         'prefix (sage-shell-interfaces:looking-back-var intf)))
       ;; when current interface is sage
       ((equal cur-intf "sage")
        (sage-shell-cpl:set-current
         'interface "sage"
         'var-base-name nil
         'prefix
         (sage-shell-interfaces:looking-back-var "sage")))))))

(cl-defun sage-shell-cpl:-types (compl-state make-cache-file-p)
  (let* ((interface (sage-shell-cpl:get compl-state 'interface))
         (var-base-name (sage-shell-cpl:get compl-state 'var-base-name))
         (other-interface-p
          (sage-shell:in interface sage-shell-interfaces:other-interfaces)))
    (let ((no-cache-for-cmds-p
           (and (not (sage-shell-cpl:get-cmd-lst interface))
                (or (not var-base-name) other-interface-p))))
      (let* ((interface-tp (if (cond (make-cache-file-p
                                      (not (string= interface "magma")))
                                     (no-cache-for-cmds-p t))
                               "interface"))
             (att-tp (if var-base-name "attributes")))
        (cl-loop for a in (list interface-tp att-tp)
                 if a collect a)))))

(defvar sage-shell-cpl:-last-sexp nil)
(cl-defun sage-shell-cpl:completion-init
    (sync &key (output-buffer sage-shell:output-buffer)
          (compl-state sage-shell-cpl:current-state))
  "If SYNC is non-nil, return a sexp. If not return value has no meaning and
`sage-shell-cpl:-last-sexp' will be set when the redirection is finished."
  ;; when current line is not in a block and current interface is 'sage'
  (setq sage-shell-cpl:-last-sexp nil)
  (when (and (sage-shell:at-top-level-and-in-sage-p)
             (sage-shell:redirect-finished-p)
             (sage-shell:output-finished-p))
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
                     (executable-find interface))))
           (types (sage-shell-cpl:-types compl-state make-cache-file-p)))
      (when make-cache-file-p
        ;; Show verbose message and make a cache file.
        (sage-shell-cpl:init-verbose interface verbose))
      (when types
        (let ((cmd (format "%s(%s, %s)"
                           (sage-shell:py-mod-func "print_cpl_sexp" )
                           (sage-shell:-to-python-list types)
                           (sage-shell:-to-python-dict compl-state))))
          (let ((cont (sage-shell:send-command cmd nil output-buffer sync)))
            (lexical-let ((output-buffer output-buffer)
                          (proc-buf sage-shell:process-buffer))
              (sage-shell:after-redirect-finished
                (with-current-buffer output-buffer
                  (goto-char (point-min))
                  (setq sage-shell-cpl:-last-sexp
                        (read (current-buffer))))))
            (if sync
                sage-shell-cpl:-last-sexp)))))))

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
          (lexical-let ((time (cadr (current-time)))
                        (proc (start-process proc-name nil
                                             (sage-shell:sage-executable)
                                             tmp-file)))
            (set-process-sentinel
             proc
             (lambda (proc event)
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
        do (return itf)))

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
      (sage-shell:awhen (sage-shell:aand sage-shell:process-buffer (get-buffer it))
        (with-current-buffer it
          (set (sage-shell-cpl:cmds-symbol intf) lst)))
    (error (format "No interface %s" intf))))

(defun sage-shell-cpl:get-cmd-lst (intf)
  (if (sage-shell:in intf (cons "sage" sage-shell-interfaces:other-interfaces))
      (sage-shell:awhen (sage-shell:aand sage-shell:process-buffer (get-buffer it))
        (with-current-buffer it
          (symbol-value (sage-shell-cpl:cmds-symbol intf))))
      (error (format "No interface %s" intf))))

(defun sage-shell-cpl:to-objname-to-send (can)
  (let* ((var-base-name (sage-shell-cpl:get-current 'var-base-name))
         (interface (cond ((sage-shell:in (sage-shell-interfaces:current-interface)
                                    sage-shell-interfaces:other-interfaces)
                           (sage-shell-interfaces:current-interface))
                          (t (sage-shell-cpl:get-current 'interface)))))
    (cond (var-base-name (concat var-base-name "." can))
          ((sage-shell:in interface sage-shell-interfaces:other-interfaces)
           (concat interface "." can))
          (t can))))

(defun sage-shell-cpl:candidates-sync (&optional regexp)
  (sage-shell-cpl:prefix)
  (let ((cur-intf (sage-shell-interfaces:current-interface)))
    ;; create candidates in some buffers
    (sage-shell-cpl:completion-init t)

    (sage-shell-cpl:candidates
     :regexp (or regexp (sage-shell-interfaces:get cur-intf 'cmd-rxp)))))

(defun sage-shell-cpl:-parse-sexp (regexp sexp state)
  (cl-loop for (tp . ls) in sage-shell-cpl:-last-sexp
           append
           (cond ((string= tp "interface")
                  (let* ((int (sage-shell-cpl:get state 'interface))
                         (cmd-lst (sage-shell-cpl:get-cmd-lst int)))
                    (unless cmd-lst
                      (let ((regexp-int
                             (concat
                              "^"
                              (or regexp
                                  (sage-shell-interfaces:get int 'cmd-rxp)))))
                        (sage-shell-cpl:set-cmd-lst
                         int
                         (cl-loop for a in ls
                                  if (string-match regexp-int a)
                                  collect a))))
                    (or cmd-lst (sage-shell-cpl:get-cmd-lst int))))
                 (t (let ((regexp1 (concat "^"
                                           (or regexp
                                               (sage-shell-interfaces:get
                                                "sage" 'cmd-rxp)))))
                      (cl-loop for a in ls
                               if (string-match regexp1 a)
                               collect a))))))

(cl-defun sage-shell-cpl:candidates (&key (regexp nil)
                                          (sexp sage-shell-cpl:-last-sexp)
                                          (state sage-shell-cpl:current-state))
  "Collect candidates matching (concat \"^\" regexp)"
  (let ((cands1 (sage-shell-cpl:-parse-sexp regexp sexp state)))
    (cond ((and (sage-shell-cpl:get state 'use-cmd-lst)
                (null (assoc "interface" sage-shell-cpl:-last-sexp)))
           (append cands1 (sage-shell-cpl:get-cmd-lst
                           (sage-shell-cpl:get-current 'interface))))
          (t cands1))))

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
    "id" "if" "import" "in" "input" "int" "intern" "is" "isinstance" "issubclass"
    "iter" "lambda" "len" "list" "locals" "long" "map" "max" "memoryview" "min"
    "next" "not" "object" "oct" "open" "or" "ord" "pass" "pow" "print" "print"
    "property" "raise" "range" "raw_input" "reduce" "reload" "repr" "return"
    "reversed" "round" "set" "setattr" "slice" "sorted" "staticmethod" "str"
    "sum" "super" "try" "tuple" "type" "unichr" "unicode" "vars"
    "while" "with" "xrange" "yield" "zip"))

(defun sage-shell:completion-at-point-func ()
  "Used for completion-at-point. The result is cached."
  (let ((old-int (sage-shell-cpl:get-current 'interface))
        (old-pref (sage-shell-cpl:get-current 'prefix))
        (old-name (sage-shell-cpl:get-current 'var-base-name))
        (wab (sage-shell:word-at-pt-beg))
        (var-name (progn
                    (sage-shell-cpl:prefix)
                    (sage-shell-cpl:get-current 'var-base-name))))
    (cond ((and
            old-int (string= old-int "sage") old-pref
            ;; same line as the last completion
            (or (= (line-number-at-pos wab) (line-number-at-pos old-pref))
                (sage-shell:clear-completion-sync-cached))
            var-name
            (assoc-default var-name sage-shell:completion-sync-cached))
           (list wab (point) (assoc-default var-name
                                            sage-shell:completion-sync-cached)))
          (t (list wab
                   (point)
                   (cond
                    (var-name
                     (setq sage-shell:completion-sync-cached
                           (cons (cons var-name
                                       (sage-shell-cpl:candidates-sync
                                        sage-shell:completion-candidate-regexp))
                                 sage-shell:completion-sync-cached))
                     (assoc-default var-name sage-shell:completion-sync-cached))
                    (t (append sage-shell:-python-builtins
                               sage-shell-cpl:-cands-in-current-session
                               (sage-shell-cpl:candidates-sync
                                sage-shell:completion-candidate-regexp)))))))))

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
   (all-completions (buffer-substring-no-properties
                     (sage-shell:symbol-beg)
                     (point))
                    (car (last (sage-shell:completion-at-point-func))))))


;;; sage-edit
(defun sage-shell-edit:process-alist ()
  (or (sage-shell:aif (get-buffer-process sage-shell:process-buffer)
          (list (cons it (process-name it))))
      (let ((case-fold-search nil))
        (cl-loop for proc in (process-list)
                 for buffer-name = (sage-shell:aif (process-buffer proc)
                                       (buffer-name it))
                 for proc-name = (process-name proc)
                 if (and buffer-name
                         (string-match (rx bol "*Sage*"
                                           (zero-or-one "<" (1+ num) ">") eol)
                                       buffer-name))
                 collect (cons proc-name proc)))))

(defun sage-shell:set-process-buffer ()
  (interactive)
  (setq sage-shell:process-buffer nil)
  (sage-shell-edit:set-sage-proc-buf-internal)
  (sage-shell:aif (get-buffer sage-shell:process-buffer)
      (message (format "Set the process buffer to buffer %s."
                       (buffer-name it)))))

;; TODO Remove unused argument verbose.
(cl-defun sage-shell-edit:set-sage-proc-buf-internal
    (&optional (start-p t) (select-p t))
  "Set `sage-shell:process-buffer'"
  (or (and (bufferp sage-shell:process-buffer)
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
                    (sage-shell:read-command) nil 'display-buffer)))
              (with-current-buffer cur-buf
                (setq sage-shell:process-buffer proc-buf)))))
         ;; if there are multiple processes
         ((consp (cdr proc-alist))
          (when select-p
            (let* ((buffer-names
                    (cl-loop for (proc-name . proc) in proc-alist
                             collect (buffer-name (process-buffer proc))))
                   (buffer-name
                    (completing-read
                     (concat
                      "There are multiple Sage processes. "
                      "Please select the process buffer: ")
                     buffer-names nil nil (car (last buffer-names))))
                   (proc (get-buffer-process buffer-name)))
              (setq sage-shell:process-buffer (process-buffer proc)))))
         ;; if there is exactly one process
         (t (setq sage-shell:process-buffer
                  (process-buffer (cdar proc-alist))))))))

(defvar sage-shell:original-mode-line-process nil)

(defun sage-shell:change-mode-line-process (on)
  (cond (on
         (unless sage-shell:original-mode-line-process
           (setq sage-shell:original-mode-line-process mode-line-process))
         (setq mode-line-process
               (sage-shell:aif mode-line-process
                   (list (concat (car it) " load"))
                 (list ":%s load"))))
        (t (setq mode-line-process sage-shell:original-mode-line-process))))

(cl-defun sage-shell-edit:exec-command-base
    (&key command pre-message post-message switch-p
          (display-function nil) (insert-command-p nil) (before-sentence nil))
  "If `insert-command-p' is non-nil, then it inserts `command' in
the process buffer. If `before-sentence' is non-nil, it will be
inserted in the process buffer before executing the command."
  ;; set sage process buffer
  (sage-shell-edit:set-sage-proc-buf-internal)

  (with-current-buffer sage-shell:process-buffer
    (sage-shell:change-mode-line-process t)
    (force-mode-line-update))

  (sage-shell:awhen pre-message (message it))

  (lexical-let ((command command)
                (post-message post-message)
                (insert-command-p insert-command-p)
                (display-function display-function)
                (before-sentence before-sentence))

    (sage-shell:as-soon-as (sage-shell:output-finished-p)
      (let ((win (get-buffer-window sage-shell:process-buffer))
            (args (list command insert-command-p before-sentence)))
        (if (and (windowp win) (window-live-p win))
            (with-selected-window win
              (apply 'sage-shell-edit:exec-cmd-internal args))
          (apply 'sage-shell-edit:exec-cmd-internal args)))
      (when post-message
        (sage-shell:after-output-finished
          (message post-message)))
      ;; display buffer
      (when display-function
        (sage-shell:after-output-finished
          (let ((win (funcall display-function sage-shell:process-buffer)))
            (when (and (windowp win)
                       (window-live-p win))
              (with-selected-window win
                (goto-char (process-mark
                            (get-buffer-process sage-shell:process-buffer))))))))
      (sage-shell:after-output-finished
        (with-current-buffer sage-shell:process-buffer
          (sage-shell:change-mode-line-process nil))))
    (when switch-p (pop-to-buffer sage-shell:process-buffer))))

(defun sage-shell-edit:exec-cmd-internal
    (command insert-command-p before-sentence)
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
               (sage-shell:send-input))
              (t (sage-shell:prepare-for-send)
                 (comint-send-string proc (concat command "\n"))))
        (save-excursion
          (insert line))))))

(defvar sage-shell-edit:temp-file-base-name "sage_shell_mode_temp")

(defun sage-shell-edit:make-temp-dir ()
  (make-temp-file "sage_shell_mode" 'directory))

(defvar sage-shell-edit:temp-directory nil)

(defun sage-shell-edit:delete-temp-dir ()
  (when (and (stringp sage-shell-edit:temp-directory)
             (string= (file-name-as-directory temporary-file-directory)
                      (file-name-directory sage-shell-edit:temp-directory))
             (file-exists-p sage-shell-edit:temp-directory))
    (delete-directory sage-shell-edit:temp-directory t)))

(defvar sage-shell:delete-temp-dir-when-kill-emacs t)

(when sage-shell:delete-temp-dir-when-kill-emacs
  (add-hook 'kill-emacs-hook 'sage-shell-edit:delete-temp-dir))

(defun sage-shell-edit:temp-file (ext)
  ;; In case temp dir is removed,
  (unless (and (stringp sage-shell-edit:temp-directory)
               (file-exists-p sage-shell-edit:temp-directory)
               (file-writable-p sage-shell-edit:temp-directory))
    (setq sage-shell-edit:temp-directory
          (sage-shell-edit:make-temp-dir)))
  (expand-file-name
   (concat sage-shell-edit:temp-file-base-name "." ext)
   sage-shell-edit:temp-directory))

(defvar sage-shell-edit:temp-file-header "# -*- coding: utf-8 -*-\n")

(defun sage-shell-edit:write-region-to-file (start end file)
  (let* ((orig-start (min start end))
         (buf-str (buffer-substring-no-properties start end))
         (offset (save-excursion
                   (goto-char orig-start)
                   (- (point) (line-beginning-position)))))
    (with-temp-buffer
      (insert sage-shell-edit:temp-file-header)
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
            (sage-shell:aif (buffer-file-name)
                (file-name-extension it)
              "sage"))))
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
    (list (cons 'buffer (list :beg '(point-min)
                              :end '(point-max)
                              :name "buffer"))
          (cons 'region (list :beg 'beg :end 'end :name "region"
                              :interactive "r"
                              :args '(beg end)))
          (cons 'defun (list :beg '(sage-shell-edit:beg-of-defun-position)
                             :end '(sage-shell-edit:end-of-defun-position)
                             :name '(format "block: %s" (sage-shell-edit:block-name))
                             :doc-name "def (or class)"))
          (cons 'line (list :beg '(line-beginning-position)
                            :end '(line-end-position)
                            :name "line")))))

(cl-defmacro sage-shell-edit:send-obj-base
    (&key type switch-p (display-function 'sage-shell-edit:display-function))
  (declare (debug t))
  (let* ((plst (assoc-default type sage-shell-edit:exec-command-base-alist))
         (command `(format "load('%s')"
                           (sage-shell-edit:make-temp-file-from-region
                            ,(plist-get plst :beg)
                            ,(plist-get plst :end)))))
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
         for doc-string = (format "Send the current %S to the Sage process."
                                  type)
         for int = (sage-shell:aif (plist-get plist :interactive)
                       `(interactive ,it)
                     '(interactive))
         for args = (sage-shell:aif (plist-get plist :args) it '())
         for doc-name = (sage-shell:aif (plist-get plist :doc-name)
                            it
                          (symbol-name type))
         append
         (cl-loop for b in '(t nil)
               for func-name =  (cond (b (concat func-name-base "-and-go"))
                                      (t func-name-base))
               collect
               `(defun ,(intern func-name) ,args ,doc-string ,int
                  (sage-shell-edit:send-obj-base :type ,type :switch-p ,b))))))

(sage-shell-edit:defun-exec-commands)

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
                               :display-function 'display-buffer))


(cl-defun sage-shell-edit:load-file-base
    (&key command file-name switch-p
          (display-function sage-shell-edit:display-function)
          (insert-command-p nil) (before-sentence nil)
          (gerund "Loading"))
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
  (pop-to-buffer sage-shell:process-buffer))


;;; sage-shell:sage-mode
;;;###autoload
(define-derived-mode sage-shell:sage-mode python-mode "Sage")

(sage-shell:define-keys sage-shell:sage-mode-map
  "C-c C-c" 'sage-shell-edit:send-buffer
  "C-c C-r" 'sage-shell-edit:send-region
  "C-M-x" 'sage-shell-edit:send-defun
  "C-c C-l" 'sage-shell-edit:load-file
  "C-c C-z" 'sage-shell-edit:pop-to-process-buffer
  "C-c C-j" 'sage-shell-edit:send-line)


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
(add-to-list 'auto-mode-alist (cons "\\.sage$" 'sage-shell:sage-mode))


;;; sage-shell-pdb
(defun sage-shell-pdb:send--command (cmd)
  (when (sage-shell-pdb:pdb-prompt-p)
    (sage-shell-edit:exec-command-base
     :command cmd
     :insert-command-p t
     :display-function 'display-buffer)
    (sage-shell:after-output-finished
      (with-current-buffer sage-shell:process-buffer
        (goto-char (process-mark (get-buffer-process
                                  sage-shell:process-buffer)))))))

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
      (when (not (member file-buffer sage-shell-pdb:buffers-to-kill))
        (add-to-list 'sage-shell-pdb:buffers-to-kill file-buffer)))
    file-buffer))

(defun sage-shell-pdb:comint-output-filter-function (output)
  "Move overlay arrow to current pdb line in tracked buffer.
Argument OUTPUT is a string with the output from the comint process."
  (when (and sage-shell-pdb:activate (not (string= output "")))
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
                sage-shell-pdb:buffers-to-kill nil)))))
  output)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; sagetex
(defun sage-shell-sagetex:add-to-texinputs ()
  "Add $SAGE_ROOT/local/share/texmf/tex/generic/sagetex/ to TEXINPUTS."
  (sage-shell:awhen (sage-shell:sage-root)
    (let ((texinputs (getenv "TEXINPUTS"))
          (sagetexdir (expand-file-name
                       "local/share/texmf/tex/generic/sagetex:"
                       it)))
      (unless (and texinputs
                   (sage-shell:in (substring sagetexdir 0 -1)
                                  (split-string texinputs ":")))
        (setenv "TEXINPUTS" (concat texinputs sagetexdir))))))

(defun sage-shell-sagetex:tex-to-sagetex-file (f)
  (concat (file-name-sans-extension
           (expand-file-name
            (sage-shell-sagetex:tex-master-maybe f)
            default-directory)) ".sagetex.sage"))

;;;###autoload
(defun sage-shell-sagetex:load-file (filename)
  "Load a .sagetex.sage file to an existing Sage process."
  (interactive
   (list (sage-shell-sagetex:read-latex-file)))
  (let ((dflt (sage-shell-sagetex:tex-to-sagetex-file
               filename)))
    (sage-shell-edit:load-file-base
     :command (format "%s('%s')" (sage-shell:py-mod-func "sage_tex_load")
                      dflt)
     :file-name dflt
     :before-sentence "# ")))

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
  (sage-shell-sagetex:load-file f)
  (lexical-let ((f f))
    (sage-shell:after-output-finished
      ;; Run process in the same directory of as f.
      (sage-shell:with-default-directory (file-name-directory f)
          (sage-shell-sagetex:-run-latex f t)))))

(defun sage-shell-sagetex:-run-latex (f &optional verbose)
  (lexical-let* ((verbose verbose)
                 (f f)
                 (cmd (let ((b (or (get-file-buffer f)
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
          (lambda (x) (when verbose
                    (message "Running \"%s\" ... Done." cmd-name)))))
      (deferred:error it
        (lambda (e) (sage-shell-sagetex:insert-error e))))))

(defmacro sage-shell-sagetex:-run-latex-and-do (f sym)
  `(progn
     (sage-shell-edit:set-sage-proc-buf-internal)
     (lexical-let ((f ,f))
       (sage-shell:as-soon-as (sage-shell:output-finished-p)
         (deferred:$
           (deferred:$
             (deferred:process
               (sage-shell:TeX-shell)
               (sage-shell:TeX-shell-command-option)
               (sage-shell-sagetex:pre-command f))
             (deferred:nextc it
               (lambda (x) (,sym f))))
           (deferred:error it
             (lambda (e) (sage-shell-sagetex:insert-error e))))))))

;;;###autoload
(defun sage-shell-sagetex:compile-file (f)
  "This command runs LaTeX on the current file, loads the
.sagetex.sage file to an existing Sage process and runs LaTeX
again. See the documentation of
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
exisiting Sage process."
  (interactive (list (sage-shell-sagetex:read-latex-file)))
  (sage-shell-sagetex:-run-latex-and-do
   f sage-shell-sagetex:load-file))

;;;###autoload
(defun sage-shell-sagetex:run-latex-and-load-current-file ()
  (interactive)
  (sage-shell-sagetex:-load-current-file
   'sage-shell-sagetex:run-latex-and-load-file))

(defun sage-shell-sagetex:read-latex-file ()
  (expand-file-name
   (read-file-name
    "LaTeX File: "
    nil
    (sage-shell:awhen (buffer-file-name) it)
    nil
    (sage-shell:awhen (buffer-file-name)
      (file-name-nondirectory it))
    (lambda (name)
      (string-match (rx ".tex" eol) name)))))

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

(define-derived-mode sage-shell-sagetex:error-mode special-mode "SageTeX-Error"
  "Error mode for SageTeX")

;; (package-generate-autoloads "sage-shell" default-directory)

(provide 'sage-shell-mode)
;;; sage-shell-mode.el ends here
