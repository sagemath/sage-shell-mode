;;; sage.el Sage front-end for GNU Emacs

;; Copyright (C) 2012, 2013 Sho Takemori
;; Author: Sho Takemori
;; Keywords: sage math
;; Created: 2012

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
;; 1. Put sage-shell.el, emacs_sage_shell.py and
;; sage-shell-autoloads.el into your load-path and bytecompile
;; sage-shell.el.
;;
;; 2. Put the following lines to ~/.emacs.
;; (require 'sage-shell-autoloads)
;; (add-to-list 'auto-mode-alist (cons "\\.sage$" 'sage-mode))
;;
;; 3. Add the location of the Sage executable to your PATH or
;; put the following line to ~/.emacs.
;; (setq sage-shell:sage-root "/path/to/sage_root_directory")
;;
;; Here /path/to/sage_root_directory is the location of Sage's root directory.
;; If you use Linux and install Sage from binary,
;; it is the path to the directory of the extracted Sage tarball.
;; If you use Mac OS X and install Sage as a Mac OS X application,
;; it is like
;; "/Applications/Sage-*.*-OSX-64bit-**.*.app/Contents/Resources/sage".





(eval-when-compile (require 'cl))
;;; Global variables for users

(defgroup sage-shell
  nil "Sage"
  :group 'languages)

(defcustom sage-shell:sage-root nil
  "SAGE_ROOT directory. If Sage executable is in your PATH, then it is not necessary to set this variable."
  :group 'sage-shell
  :type '(choice (directory :tag "Directory")
                 (const :tag "Not specified" nil)))

(defcustom sage-shell:input-history-cache-file
  (expand-file-name ".sage_input_history" user-emacs-directory)
  "If non nil, after invoking `sage-shell:send-eof',`comint-input-ring' is saved to this file."
  :group 'sage-shell
  :type '(choice (file :tag "file")
                 (const :tag "Off" nil)))

(defcustom sage-shell:completion-function 'completion-at-point
  "Function used for `sage-shell:complete'."
  :group 'sage-shell
  :type '(choice (const :tag "default" completion-at-point)
                 (const :tag "auto-complete" sage-shell-ac:auto-complete)
                 (const :tag "anything" anything-sage-shell)))


;;;; Code
;;; Anaphoric macros
(defmacro sage:ansetq (&rest rest)
  "Anaphoric setq. REST is a list of sym val sym1 val1... `it' is
the value of last sym"
  (declare (indent 0) (debug t))
  (cond ((eq (length rest) 2) `(let ((it ,(car rest)))
                                 (setq ,(nth 0 rest) ,(nth 1 rest))))
        ((> (length rest) 3) `(let ((it ,(car rest)))
                                (setq ,(nth 0 rest) ,(nth 1 rest))
                                (sage:ansetq ,@(nthcdr 2 rest))))))

(defmacro sage:aand (&rest args)
  "`it' is binded to the last evaluated argument"
  (declare (indent 0) (debug t))
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(sage:aif ,(car args) (sage:aand ,@(cdr args))))))

(defmacro sage:aif (test-form then-form &rest else-forms)
  " Temporary variable `it' is the result of test-form."
  (declare (indent 2) (debug t))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defmacro sage:awhen (test-form  &rest then-forms)
  " Temporary variable `it' is the result of test-form."
  (declare (indent 1) (debug t))
  `(let ((it ,test-form))
     (when it ,@then-forms)))

;; utilities
(eval-when-compile
  (defvar sage:gensym-counter 0))
(defvar sage:gensym-counter 0)

(defsubst* sage:group (l &optional (n 2))
  (let ((r l)
        (a nil)
        (res nil))
    (while r
      (setq a (sage:nthcar-and-rest n r)
            r (cdr a))
      (push (car a) res))
    (nreverse res)))

(defsubst* sage:nthcar-and-rest (m l)
  (loop for i from 0 to (1- m)
        for a on l
        collect (car a) into x
        finally (return (cons x a))))

(defmacro sage:if-let* (varlist test-form then-form &rest else-forms)
  "VARLIST is like varlist of let*."
  (declare (indent 3) (debug t))
  `(let* ,varlist
     (if ,test-form
         ,then-form
       ,@else-forms)))

(defmacro sage:when-let* (varlist test-form &rest then-forms)
  "VARLIST is like varlist of let*."
  (declare (indent 2) (debug t))
  `(let* ,varlist
     (when ,test-form
       ,@then-forms)))

(defmacro sage:unless-let* (varlist test-form &rest then-forms)
  "VARLIST is like varlist of let*."
  (declare (indent 2) (debug t))
  `(let* ,varlist
     (unless ,test-form
       ,@then-forms)))

(defsubst sage:gensym (&optional prefix)
  "Generate a new uninterned symbol.
The name is made by appending a number to PREFIX, default \"sage-gensym-\"."
  (let ((pfix (if (stringp prefix) prefix "sage-gensym-"))
        (num (if (integerp prefix) prefix
               (prog1 sage:gensym-counter
                 (incf sage:gensym-counter)))))
    (make-symbol (format "%s%d" pfix num))))

(defmacro sage:acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (sage:gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (sage:acond ,@(cdr clauses)))))))

(defmacro* sage:with-gensym ((&rest names) &rest body)
  (declare (indent 1) (debug t))
  `(let ,(loop for n in names collect `(,n (sage:gensym)))
     ,@body))

(defmacro sage:define-keys (keymap &rest defs)
  (declare (indent 1))
  (append (list 'progn)
          (loop for i from 0 to (1- (/ (length defs) 2))
                collect
                `(define-key
                   ,keymap
                   (kbd ,(nth (* 2 i) defs))
                   ,(nth (1+ (* 2 i)) defs)))))

(defmacro sage:as-soon-as (form &rest body)
  (declare (indent 1))
  `(cond (,form (progn ,@body))
         (t (lexical-let ((timer-sym
                           (intern (format "sage-timer%d"
                                           sage:gensym-counter))))
              (incf sage:gensym-counter)
              (set timer-sym
                   (run-with-timer
                    0.01 0.01
                    (lambda () (when ,form
                             (unwind-protect
                                 (progn ,@body)
                               (cancel-timer (symbol-value timer-sym)))))))))))

(defmacro sage:substitute-key-def (old-command new-command
                                               search-keymap
                                               def-keymap
                                               &rest default-keys)
  `(sage:aif (where-is-internal ',old-command ,search-keymap)
       (loop for key in it
             do (define-key ,def-keymap key ',new-command))
     (loop for key in (list ,@default-keys)
           do (define-key ,def-keymap key ',new-command))))

(defun sage:get-value (value-or-func &rest args)
  "If VALUE-OR-FUNC is a function, then this returns the value
returned from the function, otherwise, this returns it self. "
  (if (functionp value-or-func)
      (apply value-or-func args)
    value-or-func))


(defun sage:line-beginning-position ()
  (save-excursion (forward-line 0) (point)))

(defsubst* sage:in (elt lst &optional (test 'equal))
  (loop for i in lst
        if (funcall test elt i)
        return i))

(defmacro sage:with-current-buffer-safe (buf-maybe &rest body)
  (declare (indent 1))
  `(when (and ,buf-maybe
              (get-buffer ,buf-maybe))
     (with-current-buffer (get-buffer ,buf-maybe)
       ,@body)))

(defmacro sage:if-process-alive (then-form &optional else-form)
  (declare (indent 0))
  `(if (and sage-shell:process-buffer
            (get-buffer-process sage-shell:process-buffer))
       ,then-form
     ,else-form))

(defmacro sage:when-process-alive (&rest body)
  (declare (indent 0))
  `(sage:if-process-alive
     (progn
       ,@body)))

(defsubst sage:goto-line (n)
  (goto-char (point-min))
  (forward-line (1- n)))

(defvar sage:sage-modes '(sage-mode sage-shell-mode))

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
    "r"
    "singular"))

(defvar sage-shell:command nil)

(defun sage-shell:command ()
  (or sage-shell:command
      (setq sage-shell:command
            (if (stringp sage-shell:sage-root)
                (expand-file-name "sage" sage-shell:sage-root)
              "sage"))))


(defvar sage-shell:output-finished-regexp
  (concat (rx (group line-start
                     (or "sage:" "sage0:" ">>>" "....:"
                         "(Pdb)" "ipdb>" "(gdb)")
                     " "line-end))
          "\\|\\("
          (regexp-opt sage-shell-interfaces:other-interfaces)
          ": $\\)"))

(defvar sage-shell:process-buffer nil)
(make-variable-buffer-local 'sage-shell:process-buffer)

(defvar sage-shell:prompt-regexp
  (rx line-start
      (1+ (and (or "sage:" "sage0:" ">>>" "....:"
                   "(Pdb)" "ipdb>" "(gdb)") " ")))
  "Regular expression matching the Sage prompt.")

;; cache buffers
(defvar sage-shell-indent:indenting-buffer-name " *sage-indent*")
(defvar sage-shell-cpl:attribute-completion-buffer
  " *sage-attribute-completion*"
  "Buffer name used for collect candidates of attributes of a instance.")
(defvar sage-shell:output-buffer " *sage-output*")
(defvar sage-shell-ac:doc-arg-alist nil
  "Alist that consists of (objname-to-send . (arg-spec . doc))")
(make-variable-buffer-local 'sage-shell-ac:doc-arg-alist)

(defvar sage-shell:output-filter-finished-hook nil
  "Run after output finished.")
(make-variable-buffer-local 'sage-shell:output-filter-finished-hook)

(defvar sage-shell:redirect-filter-finished-hook nil
  "Run after redirect finished.")
(make-variable-buffer-local 'sage-shell:redirect-filter-finished-hook)


;;; Menu
(defun sage:delete-menu-from-map (keymap)
  (let ((map (symbol-value keymap)))
    (mapc (lambda (i) (when (eq (car-safe i) 'menu-bar)
                    (set keymap (delete i map))))
          map)))

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

(define-derived-mode sage-shell-mode comint-mode
  "Sage Interactive Shell" "Execute Sage commands interactively."

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
  (setq sage-shell-ac:doc-arg-alist nil)
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
  (add-hook 'comint-output-filter-functions
            'sage-shell:output-filter-hook-function nil t)

  (add-hook 'completion-at-point-functions
            'sage-shell:completion-at-point-func nil t)
  (unless sage-shell:menu-defined-p
    (sage:delete-menu-from-map 'sage-shell-mode-map)
    (easy-menu-define sage-shell-menu
      sage-shell-mode-map "sage-shell menu"
      sage-shell:menu-spec)
    (setq sage-shell:menu-defined-p t))

  ;; Run init functions after Sage loaded.
  (add-to-list 'sage-shell:output-filter-finished-hook
               (lambda () (sage-shell:after-init-function
                           sage-shell:process-buffer))))

(setq sage-shell-mode-syntax-table
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
        (modify-syntax-entry ?_ "w" table)
        table))

(defvar sage-shell-mode-hook nil "Hook run when entering Sage Shell mode.")

(sage:define-keys sage-shell-mode-map
  "TAB" 'sage-shell-tab-command
  "C-d" 'sage-shell:delchar-or-maybe-eof
  "RET" 'sage-shell:send-input
  "C-c C-i" 'sage-shell:complete
  "C-c C-h" 'sage-shell:help
  "C-c C-l" 'sage-shell:load-file
  "C-c M-o" 'sage-shell:clear-current-buffer)

(define-key sage-shell-mode-map [remap comint-next-input]
  'sage-shell:next-input)
(define-key sage-shell-mode-map [remap comint-delete-output]
  'sage-shell:delete-output)

(defun sage-shell:delchar-or-maybe-eof (arg)
  "Delete ARG characters forward or send an EOF to Sage process.
Sends an EOF only if point is at the end of the buffer and there is no input. "
  (interactive "p")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and proc (= (point) (marker-position (process-mark proc))))
        (sage-shell:send-eof)
      (delete-char arg))))

(defun sage-shell:send-eof ()
  "Send an EOF to the current buffer's process."
  (interactive)
  (sage-shell:comint-send-input t t)
  (process-send-eof)
  ;; kill cache buffes
  (loop for bufn in (list sage-shell:output-buffer
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



(defun sage-shell:send-command-sync
  (command &optional process-buffer output-buffer to-string)
  "internal function"
  (let ((out-buf
         (or output-buffer (get-buffer-create sage-shell:output-buffer)))
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
      (sage:with-current-buffer-safe out-buf
        (buffer-string)))))

(defun sage-shell:wait-for-redirection-to-complete
  (&optional msec process-buffer)
  (let ((msec (or msec 1))
        (proc-buf (or process-buffer sage-shell:process-buffer)))
    (sage:awhen (get-buffer proc-buf)
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
          (out-buf (or output-buffer
                       (get-buffer-create sage-shell:output-buffer))))
      (with-current-buffer out-buf (erase-buffer))
      (with-current-buffer proc-buf
        (sage-shell:wait-for-redirection-to-complete)
        (sage-shell:redirect-send-cmd-to-proc command out-buf proc-buf))
      (lexical-let ((out-buf out-buf))
        (lambda () (sage:with-current-buffer-safe out-buf
                    (buffer-string)))))))

(defun sage-shell:send-command-to-string (command &optional process-buffer)
  "Send process to command and return output as string."
  (sage-shell:send-command-sync command process-buffer nil t))

(defun sage-shell:current-branch-link ()
  "Return the current Sage branch link, i.e., the target of devel/sage."
  (let ((lst (split-string
              (shell-command-to-string
               (concat (sage-shell:command) " -branch")))))
    (if (= 1 (length lst)) (nth 0 lst) "main")))

(defvar sage-shell:run-history nil "sage command history.")

(defvar sage-shell:python-module "emacs_sage_shell"
  "Name of the python module.")

(defun sage-shell:py-mod-func (funcname)
  (format "%s.%s" sage-shell:python-module
          funcname))

(defvar sage-shell:python-script-directory
  (with-no-warnings
    (file-name-directory
     (locate-file sage-shell:python-module load-path '(".py")))))

(defvar sage-shell:init-command-list
  (list
   (format "sys.path = sys.path + ['%s']" sage-shell:python-script-directory)
   (format "import %s" sage-shell:python-module))
  "Sage command list evaluated after loading Sage.")

(defvar sage-shell:output-finished-p nil)
(make-variable-buffer-local 'sage-shell:output-finished-p)
(defun* sage-shell:output-finished-p
    (&optional (buffer sage-shell:process-buffer))
  (buffer-local-value 'sage-shell:output-finished-p buffer))

(defun sage-shell:start-sage-process (cmd buffer)
  (let ((cmdlist (split-string cmd)))
    (apply 'make-comint-in-buffer "Sage" buffer
           (car cmdlist) nil (cdr cmdlist))))

(defun sage-shell:after-init-function (buffer)
  "Runs after starting Sage"
  (sage-shell:send-command
   (mapconcat 'identity sage-shell:init-command-list "; ") buffer))

(defun sage-shell:get-branch-buffer (new)
  (let* ((buffer-base-name
          (sage:if-let* ((str (format "*Sage-%s*"
                                      (sage-shell:current-branch-link))))
              (string= str "*Sage-main*")
              "*Sage*" str)))
    (if new (generate-new-buffer buffer-base-name)
      (sage:aif (get-buffer buffer-base-name) it
                (generate-new-buffer buffer-base-name)))))

(defun sage-shell:read-command ()
  (let ((lst (split-string
              (read-from-minibuffer "Run sage (like this): "
                                    "sage" nil nil 'sage-shell:run-history
                                    "sage") " ")))
    (concat (sage:aif sage-shell:sage-root
                (expand-file-name (car lst) it)
              "sage")
            " "
            (mapconcat 'identity (cdr lst) " "))))

(defun* sage-shell:run (cmd new &optional
                            (switch-function 'switch-to-buffer))
  "Running Sage function internal.
SIWTCH-FUNCTION is 'no-switch, or a function with one
argument."
  (let ((buf (sage-shell:get-branch-buffer new)))
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
(defalias 'run-sage 'sage-shell:run-sage)

;;;###autoload
(defun sage-shell:run-new-sage (cmd)
  (interactive (list (sage-shell:read-command)))
  (sage-shell:run cmd t))
;;;###autoload
(defalias 'run-new-sage 'sage-shell:run-new-sage)

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

(defun sage-shell:in-this-func-p (funcname &optional pt)
  (let ((pt (or pt (point)))
        (regexp (concat "\\<" (regexp-opt (list funcname)) " *("))
        (cmbol (save-excursion (comint-bol))))
    (save-excursion
      (goto-char pt)
      (or (looking-back (concat regexp " *"))
          (when (re-search-backward regexp nil cmbol)
            (or (null (ignore-errors (forward-list)))
                (< pt (point))))))))

(defvar sage-shell:print-all-att-sage-fn
  (sage-shell:py-mod-func "print_all_attributes"))

(defvar sage-shell:print-all-commands-fn
  (sage-shell:py-mod-func "print_all_commands"))

(defvar sage-shell:dot-sage "~/.sage" "DOT_SAGE directory.")

(defun sage-shell:clear-current-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (sage:line-beginning-position))))

(defun sage-shell:redirect-finished-p ()
  (buffer-local-value 'comint-redirect-completed sage-shell:process-buffer))

(defun sage-shell:clear-command-cache ()
  (with-current-buffer sage-shell:process-buffer
    (sage-shell-cpl:set-cmd-lst "sage" nil)
    (setq sage-shell-ac:doc-arg-alist nil)))

(defun sage-shell:update-sage-commands ()
  (with-current-buffer sage-shell:process-buffer
    (setq sage-shell-ac:doc-arg-alist nil)
    (sage-shell-cpl:set-cmd-lst "sage" nil)
    (sage-shell-cpl:completion-init "sage" nil t)
    (sage-shell-cpl:candidates)
    (sage-shell-cpl:get-cmd-lst "sage")))

(defun sage-shell:load-file-base (command)
  (sage-edit:exec-command-base
   :command command
   :switch-p nil
   :display-function nil
   :pre-message "Loading the file to the Sage process..."
   :post-message "Loading the file to the Sage process... Done.")
  (sage-shell:clear-command-cache))

(defun sage-shell:load-file (filename)
  (interactive (list (sage-edit:read-script-file)))
  (sage-shell:load-file-base (format "load('%s')" filename)))

(defun sage-shell:attach-file (filename)
  (interactive (list (sage-edit:read-script-file)))
  (sage-shell:load-file-base (format "attach('%s')" filename)))

(defun sage-shell:send-magic-cmd-base (magic-command objname &optional async)
  "If `async' is nil, return the result as string, otherwise
returns a lamda function with no args to obtain the result."
  (let* ((cmd (format "%%%s %s" magic-command objname)))
    (cond (async (sage-shell:send-command cmd)
                 (lambda ()
                   (sage:with-current-buffer-safe sage-shell:output-buffer
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

(defmacro sage-shell:if-proc-and-anything-exists (then-form &optional else-form)
  (declare (indent 0))
  `(sage:if-process-alive
    (cond ((and (featurep 'anything)
                (featurep 'anything-match-plugin)
                (require 'anything-sage nil t))
           (with-no-warnings
             ,then-form))

          (t ,else-form))
    (error "Process is not alive.")))

(defun sage-shell:completion-at-point-func ()
  (list (sage-shell:word-at-pt-beg)
        (point)
        (sage-shell-cpl:candidates-sync)))

(defun sage-shell:complete ()
  (interactive)
  (sage:when-process-alive
   (cond
    ((functionp sage-shell:completion-function)
     (funcall sage-shell:completion-function))
     (t (let ((minibuffer-history nil))
          (completion-at-point))))))

(defvar sage-shell:help-completion-function 'sage-shell:help1
  "Completion function used for `sage-shell:help'.")

(defun sage-shell:help ()
  (interactive)
  (sage:when-process-alive
    (cond ((functionp sage-shell:help-completion-function)
           (funcall sage-shell:help-completion-function))
          (t (sage-shell:help1)))))

(defvar sage-shell:minibuffer-history nil)

(defun sage-shell:completing-read-commands ()
  (completing-read "Sage Objects: "
                   (sage-shell-cpl:candidates-sync)
                   nil nil (sage-shell:word-at-point)
                   sage-shell:minibuffer-history))

(defun sage-shell:source-file-and-line-num (obj)
  "Return (cons sourcefile line-number)"
  (let ((str (sage-shell:send-command-to-string
              (format
               (concat "print "
                       "sage.misc.sageinspect.sage_getfile(%s), '*', "
                       "sage.misc.sageinspect.sage_getsourcelines(%s)[-1]")
               obj obj))))
    (when (string-match (rx (group (1+ (not (syntax whitespace))))
                            (1+ " ") "*" (1+ " ")
                            (group (1+ num)))
                        str)
      (cons (match-string 1 str)
            (string-to-number (match-string 2 str))))))

(defun* sage-shell:find-source-in-view-mode
    (obj &optional (find-funct 'find-file-read-only-other-window) (offset 0))
  (let* ((src-line (sage-shell:source-file-and-line-num obj))
         (src (car-safe src-line))
         (line (cdr-safe src-line))
         (proc-buf sage-shell:process-buffer))
    (if (and src-line
             (file-readable-p src))
        (progn (funcall find-funct src)
               (sage:goto-line line)
               (recenter offset)
               (setq sage-shell:process-buffer proc-buf))
      (message "Source file not found."))))


;; comint functions
(defun sage-shell:nullify-ring (ring)
  (loop repeat (ring-size ring)
        do (ring-insert ring nil))
  ring)

(defvar sage-shell:output-ring
  (sage-shell:nullify-ring (make-ring 2))
  "An output from a Sage process is decomposed into parts.
This ring remebers the parts.")



(defun sage-shell:output-filter-hook-function (string)
  "A member of `comint-output-filter-functions' in Sage buffer."
  (unless (string= string "")
    ;; push output to `sage-shell:output-ring'
    (ring-insert sage-shell:output-ring string)

    (let ((output (concat (ring-ref sage-shell:output-ring 1)
                          (ring-ref sage-shell:output-ring 0))))
      (when (string-match sage-shell:output-finished-regexp output)
        (setq sage-shell:output-finished-p t)))

    ;; Comment out output if the syntax of a line does not looks like
    ;; python syntax.
    (sage-shell:comment-out-output)

    (sage-shell-indent:indent-function string)))

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
    (add-text-properties comint-last-output-start (point)
                         '(front-sticky
                           (field inhibit-line-move-field-capture)
                           rear-nonsticky t
                           field output
                           inhibit-line-move-field-capture t))
    (save-excursion
      (when comint-last-output-start
        (loop initially
              (goto-char comint-last-output-start)
              while
              (not (or (not (re-search-forward "^." nil t))
                       (save-excursion
                         (forward-line 0)
                         (looking-at sage-shell:output-finished-regexp))))
              do
              (let ((p (sage-shell:python-syntax-output-p
                        (buffer-substring (line-beginning-position)
                                          (line-end-position)))))
                (cond ((not p) (put-text-property  (1- (point)) (point)
                                                   'syntax-table (cons 11 nil)))
                      ((numberp p)
                       (setq p (+ p (line-beginning-position)))
                       (put-text-property  p (1+ p)
                                           'syntax-table (cons 11 nil))))))))))

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

(defun sage-shell:run-hook-and-remove (hook)
  (unwind-protect
      (loop for f in (nreverse (symbol-value hook)) do (funcall f))
    (set hook nil)))

(defvar sage-shell:output-filter-count 0)
(make-variable-buffer-local 'sage-shell:output-filter-count)
(defvar sage-shell:long-output-rdct-number 5)
(defvar sage-shell:redirect-last-prompt nil)
(defvar sage-shell:output-redirect-buffer "*Sage Redirect Output*")

(defvar sage-shell:redirect-long-output-p nil)

(defun sage-shell:output-filter (process string)
  (let ((oprocbuf (process-buffer process))
        (count sage-shell:long-output-rdct-number)
        (string (ansi-color-filter-apply string)))
    (sage:with-current-buffer-safe (and string oprocbuf)
      (cond
       (sage-shell:redirect-long-output-p
        (incf sage-shell:output-filter-count)
        (cond
         ((< sage-shell:output-filter-count count)
          (sage-shell:output-filter-no-rdct process string))

         ((equal sage-shell:output-filter-count count)
          (sage-shell:output-filter-rdct-lop-setup process)
          (sage-shell:output-filter-rdct-lop process string))

         ((> sage-shell:output-filter-count count)
          ;; (when (sage-shell:redirect-finished-p)
          ;;   ;; FIXME
          ;;   (message "THE OUTPUT MAY NOT BE CORRECT."))
          (sage-shell:output-filter-rdct-lop process string))))
       (t (sage-shell:output-filter-no-rdct process string))))))

(defun sage-shell:output-filter-rdct-lop-setup (process)
  (let ((o-buf (get-buffer-create sage-shell:output-redirect-buffer))
        (str (buffer-substring-no-properties
              comint-last-input-end (point))))
    (delete-region comint-last-input-end (point))
    (with-current-buffer o-buf
      (erase-buffer)
      (insert str))
    (sage-shell:prepare-for-redirect
     process sage-shell:output-redirect-buffer)
    (setq comint-redirect-original-filter-function (process-filter process))
    (sage-shell:output-filter-no-rdct process " *Output Redirected*")))

(defvar sage-shell:redirect-restore-filter-p t)
(make-variable-buffer-local 'sage-shell:redirect-restore-filter-p)
(defun sage-shell:output-redirect-cleanup ()
  (setq sage-shell:output-filter-count 0))

(defun sage-shell:output-filter-rdct-lop (process string)
  "Redirect long output"
  (let ((sage-shell:redirect-restore-filter-p nil))
    (sage-shell:redirect-filter process string)
    (when (sage-shell:redirect-finished-p)
      (with-current-buffer (process-buffer process)
        (sage-shell:output-filter-no-rdct
         process (concat "\n" sage-shell:redirect-last-prompt))
        (sage-shell:output-redirect-cleanup)
        (with-selected-window (display-buffer
                               (get-buffer sage-shell:output-redirect-buffer))
          (with-current-buffer sage-shell:output-redirect-buffer
            (goto-char (point-max)) (recenter -1)))))))

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
      (run-hook-with-args 'comint-output-filter-functions string)
      (set-marker saved-point (point))

      (goto-char (process-mark process)) ; in case a filter moved it

      (when sage-shell:output-finished-p
        (let ((lbp (sage:line-beginning-position)))
          (when (get-text-property lbp 'read-only)
            (delete-region lbp comint-last-output-start))
          ;; Highlight the prompt
          (sage-shell:highlight-prompt lbp)

          (sage-shell:run-hook-and-remove
           'sage-shell:output-filter-finished-hook)))
      (goto-char saved-point))))

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
       '(read-only t rear-nonsticky t front-sticky (read-only))))
    (unless (and (bolp) (null comint-last-prompt-overlay))
      ;; Need to create or move the prompt overlay (in the case
      ;; where there is no prompt ((bolp) == t), we still do
      ;; this if there's already an existing overlay).
      (if comint-last-prompt-overlay
          ;; Just move an existing overlay
          (move-overlay comint-last-prompt-overlay
                        prompt-start (point))
        ;; Need to create the overlay
        (setq comint-last-prompt-overlay
              (make-overlay prompt-start (point)))
        (overlay-put comint-last-prompt-overlay
                     'font-lock-face 'comint-highlight-prompt)))))

(defun sage-shell:delete-output ()
  "Delete all output from interpreter since last input.
Does not delete the prompt."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
        (replacement nil)
        (inhibit-read-only t))
    (save-excursion
      (let ((pmark (progn (goto-char (process-mark proc))
                          (forward-line 0)
                          (point-marker))))
        (delete-region comint-last-input-end pmark)
        (goto-char (process-mark proc))
        (setq replacement (buffer-substring pmark (point)))
        (delete-region pmark (point))))
    ;; Output message and put back prompt
    (sage-shell:output-filter proc replacement)))

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
          (input-string (ansi-color-filter-apply input-string)))
      (with-current-buffer proc-buf
        (let ((out-buf comint-redirect-output-buffer)
              (f-regexp sage-shell:output-finished-regexp))
          (set-buffer out-buf)
          ;; Send output to all registered buffers
          ;; Go to the end of the buffer
          (goto-char (point-max))
          ;; Insert the output
          (let ((inhibit-read-only comint-redirect-subvert-readonly))
            (insert input-string))

          ;; If we see the prompt, tidy up
          (when (save-excursion
                  (sage:aif sage-shell:redirect-last-point
                      (progn (goto-char it)
                             (forward-line -1))
                    (goto-char (point-min)))
                  (re-search-forward f-regexp nil t))
            (setq sage-shell:redirect-last-prompt (match-string 0))
            (replace-match "")
            (set-buffer proc-buf)
            (sage-shell:redirect-cleanup)
            (sage-shell:run-hook-and-remove
             'sage-shell:redirect-filter-finished-hook))
          (setq sage-shell:redirect-last-point (point)))))))

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
  (comint-redirect-setup
   output-buffer
   (current-buffer)                     ; Comint Buffer
   comint-redirect-finished-regexp      ; Finished Regexp
   nil)                                 ; Echo input

  (when filter
    ;; Set the filter
    (setq comint-redirect-original-filter-function ; Save the old filter
          (process-filter proc))
    (set-process-filter proc filter)))

(defun* sage-shell:redirect-cleanup ()
  (when sage-shell:redirect-restore-filter-p
    (set-process-filter (get-buffer-process (current-buffer))
                        comint-redirect-original-filter-function))
  ;; Restore the mode line
  (setq mode-line-process comint-redirect-original-mode-line-process)
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

        (comint-add-to-input-history history)

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
    (setq sage-shell:output-finished-p nil)
    (sage-shell:output-redirect-cleanup))

(defun sage-shell-update-sage-commands-p (line)
  (string-match (rx "from" (1+ nonl) "import") line))

(defun sage-shell:send-input ()
  "Send current line to Sage process.
This function has many side effects:
* Set `sage-shell:input-ring-index'.
* Set `sage-shell:output-finished-p'.
* Fill sage-shell:output-ring with nil.
* If current line is like '****?' then pop to the help buffer.
* Send current line to indenting buffer.
* (comint-send-input)"
  (interactive)
  (let ((line (buffer-substring (point-at-bol) (line-end-position)))
        (inhibit-read-only t)
        (at-tl-in-sage-p (sage-shell:at-top-level-and-in-sage-p)))

    (sage-shell:prepare-for-send)
    ;; Since comint-send-input sets comint-input-ring-index to nil,
    ;; restore its value
    (setq sage-shell:input-ring-index comint-input-ring-index)

    ;; If current line contains %gap, gap.console(), gap.interact(), %gp, ...
    ;; then create completion buffer
    (sage:awhen (sage-shell-cpl:switch-to-another-interface-p line)
      (sage-shell-cpl:completion-init it nil t))

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

     ;; send current line to indenting buffer and to process normally
     (t (sage-shell:send-line-to-indenting-buffer-and-indent line)
        (sage-shell:comint-send-input)))
    ;; If current line contains from ... import *, then update sage commands
    (when (sage-shell-update-sage-commands-p line)
      (sage-shell:update-sage-commands))))

(defun sage-shell:send-blank-line ()
  (with-current-buffer sage-shell:process-buffer
    (let ((comint-input-sender
           (lambda (proc str) (comint-simple-send proc ""))))
      (sage-shell:comint-send-input))))

(defun sage-shell:at-top-level-p ()
  (save-excursion
    (forward-line 0)
    (or (looking-at sage-shell:prompt1-regexp)
        (loop for i in sage-shell-interfaces:other-interfaces
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
  (rx (group bow (or "Base Class"
                     "Definition"
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
                     "Type"
                     "sage"
                     "Class Docstring"
                     "Constructor Docstring") ":")))

(defvar sage-shell-help:help-buffer-name "*Sage help*")

(define-derived-mode sage-help-mode help-mode "Sage help"
  "Help mode for Sage"
  (font-lock-mode 1)
  (view-mode 1)
  (font-lock-add-keywords
   nil `((,sage-shell-help:fontlock-keyword-regexp 1 font-lock-keyword-face))))

(sage:define-keys sage-help-mode-map
  "C-c C-j" 'sage-shell-help:send-current-line
  "C-c C-z" 'sage-edit:pop-to-process-buffer)

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
      (while (and (re-search-forward (rx bol (or "Source File"
                                                 "Loaded File"
                                                 "File") ":"
                                                 (1+ blank)) nil t)
                  (re-search-forward "[^ \n\t]+" nil t))
        (sage-shell-help:file-type-make-button
         0 (match-string 0) linenum)))))

(defun sage-shell-help:file-type-make-button (match-number file line)
  (unless (button-at (match-beginning match-number))
    (make-text-button
     (match-beginning match-number)
     (match-end match-number)
     'sage:file file 'sage:line line
     'action (lambda (button)
               (let ((linenum (button-get button 'sage:line)))
                 (find-file-other-window
                  (button-get button 'sage:file))
                 (when linenum
                   (goto-char (point-min))
                   (forward-line (1- linenum)))))
     'follow-link t)))


(defun sage-shell-help:find-symbols-line-num (symbol process-buffer)
  "return line number"
  (let* ((str (sage-shell:send-command-to-string
               (format
                (concat
                 "print "
                 "sage.misc.sageinspect.sage_getsourcelines(%s)[-1]")
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
    (labels ((insert-btn
              (text arg)
              (insert-text-button
               text
               'sage:help-history-arg arg
               'action
               (lambda (btn)
                 (sage-shell-help:forward-history
                  (button-get btn 'sage:help-history-arg)))
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
(define-key sage-help-mode-map
  [remap help-go-back] 'sage-shell-help:backward-history)
(define-key sage-help-mode-map
  [remap help-go-forward] 'sage-shell-help:forward-history)


(defun sage-shell-help:describe-symbol (symbol)
  "Describe symbol and pop to help buffer"
  (let ((buf (get-buffer-create sage-shell-help:help-buffer-name))
        (str (sage-shell:send-magic-cmd-base "pinfo" symbol))
        (proc (current-buffer)))
    (when (string-match sage-shell-help:symbol-not-found-regexp str)
      (error (format "Object %s not found." symbol)))

    (let ((inhibit-read-only t)
          (view-read-only nil)
          (help-window-select t))
      (with-help-window (buffer-name buf)
        (with-current-buffer buf
          (erase-buffer) (sage-help-mode)
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
    (sage:awhen line
      (sage-edit:exec-command-base :command it :insert-command-p t
                                   :display-function 'display-buffer))))



;;; sage-shell-interfaces

(defvar sage-shell-interfaces:optional-interfaces
  '("axiom" "gap3" "gnuplot" "kash" "magma"
    "maple" "matlab" "mathematica" "octave"
    "giac")
  "Interfaces not installed by default.")

(defun sage-shell-interfaces:current-interface ()
  (save-excursion
    ;; goto last prompt
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (forward-line 0)
    (sage:aif (loop for str in sage-shell-interfaces:other-interfaces
                    if (looking-at (concat str ": ")) return str)
        it
      (if (looking-at sage-shell:prompt-regexp)
          "sage"
        ;; otherwise
        (let ((lstline (ring-ref comint-input-ring 0)))
          (when (string-match
                 (rx (group (1+ alnum)) (or "." "_") "console")
                 lstline)
            (match-string-no-properties 1 lstline)))))))

;; Define many global variables
(loop for i in (append sage-shell-interfaces:other-interfaces '("sage"))
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
  (when (sage:in interface (cons "sage" sage-shell-interfaces:other-interfaces))
    (let ((alist (symbol-value
                  (intern (format "sage-shell-cpl:%s-info" interface)))))
      (loop for (att val) in (sage:group attributes-values)
            do
            (sage:aif (assoc att alist)
                (setcdr it val)
              (error (format "No such attribute %S" att)))
            finally return val))))

(defun sage-shell-interfaces:get (interface attribute)
  (when (sage:in interface (cons "sage" sage-shell-interfaces:other-interfaces))
    (let ((alist (symbol-value
                  (intern (format "sage-shell-cpl:%s-info" interface)))))
      (sage:aif (assoc attribute alist)
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
(loop for itf in (append '("sage") sage-shell-interfaces:other-interfaces)
      do (sage-shell-interfaces:set itf
                                    'completion-buffer
                                    (format " *sage-%s-compeltion*" itf)))

;; set verbose message and cache-file name
(loop for itf in '("maple" "maxima" "r")
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
(defvar sage-shell-cpl:info
  (list
   ;; name of the interface (string)
   (cons 'interface nil)
   ;; nil or the point of the beggining of completion
   (cons 'prefix nil)
   ;; nil or the base name of the variable name
   (cons 'var-base-name nil)
   ;; last completed object name, for example NumberField or
   ;; K.class_number, this is used for popup help.
   (cons 'last-cmpl-obj nil)
   ;; last completion time
   (cons 'last-cmpl-time nil)))

(make-variable-buffer-local 'sage-shell-cpl:info)

(defun sage-shell-cpl:get (attribute)
  (sage:aif (assoc attribute sage-shell-cpl:info)
      (cdr-safe it)
    (error (format "No such attribute %S" attribute))))

(defun sage-shell-cpl:set (&rest attributes-values)
  (loop for (att val) in (sage:group attributes-values)
        do
        (sage:aif (assoc att sage-shell-cpl:info)
            (setcdr it val)
          (error (format "No such attribute %S" att)))
        finally return val))

(defun sage-shell-cpl:completion-buffer-alist (interface var-base-name)
  "Return the alist of completion buffers. If cdr is nil, then it
is the buffer for the candidates of attribute."
  (let* ((other-interface-p
          (sage:in interface sage-shell-interfaces:other-interfaces))
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
    (loop with lst
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
        (sage:awhen (sage-shell-cpl:base-name-att-beg-rec var-chars)
          (let ((base-name (buffer-substring-no-properties it base-end)))
            ;; when base-name does not call any functions
            (unless (string-match (rx (or (group (or alnum "_")
                                                 (0+ whitespace)
                                                 "(")
                                          "%"))
                                  base-name)
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

(defun sage-shell-cpl:print-all-att
  (var-base-name output-buffer &optional process sync)
  (sage-shell:send-command
   (format "%s('%s')" sage-shell:print-all-att-sage-fn var-base-name)
   process output-buffer sync))

(defun sage-shell-cpl:print-all-commands
  (interface output-buffer &optional process sync)
  (sage-shell:send-command
   (format "%s('%s')" sage-shell:print-all-commands-fn interface)
   process output-buffer sync))

(defun sage-shell-cpl:prefix ()
  (when (and (get-buffer-process sage-shell:process-buffer)
             (sage-shell-interfaces:current-interface))
    (let* ((base-and-beg (sage-shell-cpl:var-base-name-and-att-start))
           (base-name (car-safe base-and-beg))
           (att-beg (cdr-safe base-and-beg))
           (cur-intf (sage-shell-interfaces:current-interface))
           (itfcs sage-shell-interfaces:other-interfaces)
           (intf (unless att-beg
                   (or (sage:in cur-intf itfcs)
                       (loop named interfaces for i in itfcs
                             do (loop for func in (mapcar
                                                   (lambda (x) (concat i x))
                                                   (list ".eval" ""))
                                      if (sage-shell:in-this-func-p func)
                                      do (return-from interfaces i))))))
           pfx)
      (cond
       ;; when the word at point is an attribute
       ((and att-beg (sage-shell:at-top-level-and-in-sage-p))
        (sage-shell-cpl:set 'var-base-name base-name)
        (if (sage:in base-name itfcs)
            (sage-shell-cpl:set 'interface base-name)
          (sage-shell-cpl:set 'interface "sage"))
        (sage-shell-cpl:set 'prefix att-beg))
       ;; when current interface is not sage or the point is
       ;; in a function one of gp.eval, gp, gap.eval, ...
       ((and intf (sage-shell-cpl:set 'interface intf)
             (setq pfx (sage-shell-interfaces:looking-back-var intf)))
        (sage-shell-cpl:set 'var-base-name nil
                            'prefix  pfx))
       ;; when current interface is sage
       ((equal cur-intf "sage")
        (sage-shell-cpl:set
         'interface "sage"
         'var-base-name nil
         'prefix
         (sage-shell-interfaces:looking-back-var "sage")))))))

(defun sage-shell-cpl:completion-init (interface var-base-name sync)
  (let* ((cmd-cmp-bufn (sage-shell-interfaces:get interface 'completion-buffer))
         (att-cmp-bufn sage-shell-cpl:attribute-completion-buffer)
         (verbose (sage-shell-interfaces:get interface 'verbose))
         (other-interface-p
          (sage:in interface sage-shell-interfaces:other-interfaces)))
    ;; when current line is not in a block and current interface is 'sage'
    (when (sage-shell:at-top-level-and-in-sage-p)
      (cond
       ;; when 'verbose' and 'interface' is installed and cache file
       ;; does not exit
       ((and (not (sage-shell-cpl:get-cmd-lst interface))
             verbose
             (not (file-exists-p (sage-shell-interfaces:get
                                  interface 'cache-file)))
             (or (not (sage:in interface
                               sage-shell-interfaces:optional-interfaces))
                 (executable-find interface)))
        (unless (get-buffer cmd-cmp-bufn)
          (get-buffer-create cmd-cmp-bufn)
          ;; show verbose message, print candidates and make a cache
          ;; file and print cadidates in completion buffer.
          (sage-shell-cpl:init-verbose interface verbose cmd-cmp-bufn sync)))
       (var-base-name
        ;; when var-base-name is non nil, print all attributes in buffer
        ;; `sage-shell-cpl:attribute-completion-buffer'
        (unless (get-buffer att-cmp-bufn) (get-buffer-create att-cmp-bufn))
        (cond ((boundp 'ac-auto-start)  ; case when auto-complete.el is loaded
               (when (or (null ac-auto-start)
                         (and (integerp ac-auto-start)
                              (<= (- (point) (sage-shell-cpl:get 'prefix))
                                  ac-auto-start)))
                 (sage-shell-cpl:print-all-att var-base-name
                                               att-cmp-bufn nil sync)))
              (t (sage-shell-cpl:print-all-att var-base-name
                                               att-cmp-bufn nil sync)))))
      (when (and (not (sage-shell-cpl:get-cmd-lst interface))
                 (or (not var-base-name) other-interface-p)
                 (not (get-buffer cmd-cmp-bufn)))
        (get-buffer-create cmd-cmp-bufn)
        (sage-shell-cpl:print-all-commands
         interface cmd-cmp-bufn nil sync)))))

(defun sage-shell-cpl:init-verbose
  (interface verbose output-buffer sync)
  (cond
   ((not (equal interface "magma"))
    (message verbose)
    (sage-shell-cpl:print-all-commands interface output-buffer nil sync))

   (t (let* ((tmp-file (make-temp-file "sage" nil ".sage"))
             (proc-name "sage-shell-magma-complete"))
        ;; if the interface is magma, start new process
        (with-temp-buffer
          (insert (format "%s.trait_names(verbose=False)" interface))
          (write-region (point-min) (point-max) tmp-file nil 'silent))
        (unless (loop for p in (process-list)
                      thereis
                      (equal (process-name p) proc-name))
          (message verbose)
          (lexical-let ((time (cadr (current-time)))
                        (proc (start-process proc-name nil
                                             (sage-shell:command) tmp-file)))
            (set-process-sentinel
             proc
             (lambda (proc event)
               (message
                "Scanning Magma types ... Done! (%d seconds)\n Saving cache to
'%s' for future instant use\n.  Delete the above file to force re-creation of the cache."
                (- (cadr (current-time)) time)
                (sage-shell-interfaces:get "magma" 'cache-file))))))))))

(defvar sage-shell-cpl:doc-mssg-time-dur 10)

(defun sage-shell-cpl:show-doc-message-p ()
  (let ((objname (sage-shell-cpl:get 'last-cmpl-obj))
        (last-time (sage-shell-cpl:get 'last-cmpl-time)))
    (and (eq major-mode 'sage-shell-mode)
         (get-buffer-process (current-buffer))
         (or
          ;; if 'objname pt ...'
          (looking-back
           (concat (regexp-opt (list objname)) " *"))
          ;; if 'objname (... pt ...' or 'objname (... pt ...)'
          (sage-shell:in-this-func-p objname))
         (< (nth 1 (current-time))
            (+ sage-shell-cpl:doc-mssg-time-dur
               last-time)))))

(defun sage-shell-cpl:switch-to-another-interface-p (line)
  "Returns non nil when LINE contains %gp, gp.console(), gp.interact(), ..."
  (loop for itf in sage-shell-interfaces:other-interfaces
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

(loop for i in sage-shell-interfaces:other-interfaces
      for sym = (sage-shell-cpl:cmds-symbol i)
      do (set sym nil))

(defun sage-shell-cpl:set-cmd-lst (intf lst)
  (if (sage:in intf (cons "sage" sage-shell-interfaces:other-interfaces))
      (sage:awhen (sage:aand sage-shell:process-buffer (get-buffer it))
        (with-current-buffer it
          (set (sage-shell-cpl:cmds-symbol intf) lst)))
    (error (format "No interface %s" intf))))

(defun sage-shell-cpl:get-cmd-lst (intf)
  (if (sage:in intf (cons "sage" sage-shell-interfaces:other-interfaces))
      (sage:awhen (sage:aand sage-shell:process-buffer (get-buffer it))
        (with-current-buffer it
          (symbol-value (sage-shell-cpl:cmds-symbol intf))))
      (error (format "No interface %s" intf))))

(defun sage-shell-cpl:to-objname-to-send (can)
  (let* ((var-base-name (sage-shell-cpl:get 'var-base-name))
         (interface (cond ((sage:in (sage-shell-interfaces:current-interface)
                                    sage-shell-interfaces:other-interfaces)
                           (sage-shell-interfaces:current-interface))
                          (t (sage-shell-cpl:get 'interface)))))
    (cond (var-base-name (concat var-base-name "." can))
          ((sage:in interface sage-shell-interfaces:other-interfaces)
           (concat interface "." can))
          (t can))))

(defun sage-shell-cpl:candidates (&optional regexp proc-buf)
  (with-current-buffer (or proc-buf sage-shell:process-buffer)
    (labels ((collect-cands
              (buf rgp)
              (when (get-buffer buf)
                (with-current-buffer buf
                  (loop initially
                        (goto-char (point-min))
                        while (re-search-forward (concat "^" rgp) nil t)
                        collect (match-string 0))))))
      (loop with candidates
            for (buf . intf) in (sage-shell-cpl:completion-buffer-alist
                                 (sage-shell-cpl:get 'interface)
                                 (sage-shell-cpl:get 'var-base-name))
            for rgp =
            (cond ((sage:in intf sage-shell-interfaces:other-interfaces)
                   (sage-shell-interfaces:get intf 'cmd-rxp))
                  (t (or regexp
                         (sage-shell-interfaces:get "sage" 'cmd-rxp))))
            for cmdlist = (sage:aif intf (sage-shell-cpl:get-cmd-lst it))
            do
            (setq candidates
                  (append
                   (cond ((and intf cmdlist) cmdlist)
                         ((and intf (sage-shell:redirect-finished-p))
                          (prog1 (sage-shell-cpl:set-cmd-lst
                                  intf (collect-cands buf rgp))
                            (when (get-buffer buf) (kill-buffer buf))))
                         (t (collect-cands buf rgp)))
                   candidates))
            finally (return candidates)))))

(defun sage-shell-cpl:candidates-sync (&optional regexp)
  (sage-shell-cpl:prefix)
  (let* ((cur-intf (sage-shell-interfaces:current-interface))
         (interface (cond ((equal cur-intf  "sage")
                           (sage-shell-cpl:get 'interface))
                          (t cur-intf))))
    ;; create candidates in some buffers
    (sage-shell-cpl:completion-init
     interface (sage-shell-cpl:get 'var-base-name) t)

    (sage-shell-cpl:candidates
     (or regexp (sage-shell-interfaces:get cur-intf 'cmd-rxp)))))






;;; sage-edit
(defun sage-edit:process-alist ()
  (or (sage:aif (get-buffer-process sage-shell:process-buffer)
          (list (cons it (process-name it))))
      (loop for proc in (process-list)
            for proc-name = (process-name proc)
            if (string-match "\\<sage\\>" proc-name)
            collect (cons proc-name proc))))

(defun* sage-edit:set-sage-proc-buf-internal (&optional (start-p t))
  "Set `sage-shell:process-buffer'"
  (or (get-buffer-process sage-shell:process-buffer)
      (let ((proc-alist (sage-edit:process-alist))
            (cur-buf (current-buffer)))
        (cond
         ;; if there are no processes
         ((and (null proc-alist) start-p)
          (if (y-or-n-p (concat "Threre are no Sage processes. "
                                "Start new process? "))
              (let ((proc-buf
                     (sage-shell:run (sage-shell:read-command) nil 'no-switch)))
                (with-current-buffer cur-buf
                  (setq sage-shell:process-buffer proc-buf)))))
         ((null proc-alist) t)
         ;; if there are multiple processes
         ((consp (cdr proc-alist))
          (let* ((proc-name
                  (completing-read
                   (concat "There are multiple Sage processes. "
                           "Please select process. ")
                   (loop for (proc-name . proc) in proc-alist
                         collect proc-name)))
                 (proc (cdr (assoc proc-name proc-alist))))
            (setq sage-shell:process-buffer (process-buffer proc))))
         ;; if there is exactly one process
         (t (setq sage-shell:process-buffer
                  (process-buffer (cdar proc-alist))))))))

(defvar sage-edit:display-function nil)
(defun* sage-edit:exec-command-base
    (&key command pre-message post-message switch-p
          (display-function nil) (insert-command-p nil))
  ;; set sage process buffer
  (sage-edit:set-sage-proc-buf-internal)

  (sage:awhen pre-message (message it))

  (lexical-let ((command command)
                (post-message post-message)
                (insert-command-p insert-command-p))

    (sage:as-soon-as (sage-shell:output-finished-p)
      (sage-edit:exec-cmd-internal command insert-command-p)
      (when post-message
        (sage-shell:after-output-finished
          (message post-message))))
    ;; display buffer
    (when display-function
      (funcall display-function sage-shell:process-buffer))
    (when switch-p (pop-to-buffer sage-shell:process-buffer))))

(defun sage-edit:exec-cmd-internal (command insert-command-p)
  (with-current-buffer sage-shell:process-buffer
    (save-excursion
      (end-of-line)
      (let* ((bol (comint-line-beginning-position))
             (eol (line-end-position))
             (line (buffer-substring-no-properties bol eol)))
        (delete-region bol eol)
        (cond (insert-command-p
               (goto-char (process-mark (get-buffer-process
                                         sage-shell:process-buffer)))
               (insert command)
               (sage-shell:send-input))
              (t (sage-shell:prepare-for-send)
                 (comint-send-string (get-buffer-process
                                      sage-shell:process-buffer)
                                     (concat command "\n"))))
        (insert line)))))

(defvar sage-edit:temp-file-base-name "sage_shell_mode_temp")
(defvar sage-edit:temp-directory
  (expand-file-name "sage_shell_mode" temporary-file-directory))
(unless (file-exists-p sage-edit:temp-directory)
  (make-directory sage-edit:temp-directory))

(defun sage-edit:temp-file (ext)
  (expand-file-name
   (concat sage-edit:temp-file-base-name "." ext)
   sage-edit:temp-directory))

(defvar sage-edit:temp-file-header "# -*- coding: utf-8 -*-\n")

(defun sage-edit:make-temp-file-from-region (start end)
  "Make temp file from region and return temp file name."
  (let* ((f (sage-edit:temp-file
             (sage:aif (buffer-file-name)
                 (file-name-extension it)
               "sage")))
         (orig-start (min start end))
         (buf-str (buffer-substring-no-properties start end))
         (offset (save-excursion
                   (goto-char orig-start)
                   (- (point) (line-beginning-position)))))
    (with-temp-buffer
      (insert sage-edit:temp-file-header)
      (insert (make-string offset (string-to-char " ")))
      (insert buf-str)
      (goto-char (point-min))
      (when (> (current-indentation) 0) ; need dummy block
        (insert "if True:\n"))
      (write-region nil nil  f nil 'nomsg))
    ;; return temp file name
    f))

(defun sage-edit:beg-of-defun-position ()
  (min (save-excursion
         (end-of-defun)
         (beginning-of-defun) (point))
       (save-excursion (beginning-of-defun)
                       (point))))

(defun sage-edit:end-of-defun-position ()
  (save-excursion
    (goto-char (sage-edit:beg-of-defun-position))
    (end-of-defun)
    (point)))

(defun sage-edit:block-name ()
  (let ((beg (sage-edit:beg-of-defun-position)))
    (save-excursion
      (buffer-substring
       (progn (goto-char beg)
         (skip-chars-forward " \t") (point))
       (progn (end-of-line)
              (skip-chars-backward " \t:")
              (point))))))

(eval-when-compile
  (defvar sage-edit:exec-command-base-alist
    (list (cons 'buffer (list :beg '(point-min)
                              :end '(point-max)
                              :name "buffer"))
          (cons 'region (list :beg 'beg :end 'end :name "region"
                              :interactive "r"
                              :args '(beg end)))
          (cons 'defun (list :beg '(sage-edit:beg-of-defun-position)
                             :end '(sage-edit:end-of-defun-position)
                             :name '(format "block: %s" (sage-edit:block-name))
                             :doc-name "def (or class)"))
          (cons 'line (list :beg '(line-beginning-position)
                            :end '(line-end-position)
                            :name "line")))))

(defmacro* sage-edit:send-obj-base
    (&key type switch-p (display-function 'sage-edit:display-function))
  (declare (debug t))
  (let* ((plst (assoc-default type sage-edit:exec-command-base-alist))
         (command `(format "load('%s')"
                           (sage-edit:make-temp-file-from-region
                            ,(plist-get plst :beg)
                            ,(plist-get plst :end)))))
    `(save-excursion
       (sage-edit:exec-command-base
        :command ,command
        :pre-message (format "Loading the %s to the Sage process..."
                             ,(plist-get plst :name))
        :post-message (format "Loading the %s to the Sage process... Done."
                              ,(plist-get plst :name))
        :switch-p ,switch-p
        :display-function ,display-function)
       (sage-shell:clear-command-cache))))

(defmacro sage-edit:defun-exec-commands ()
  (append '(progn)
   (loop for (type . plist) in sage-edit:exec-command-base-alist
         for func-name-base = (format "sage-edit:send-%s" type)
         for doc-string = (format "Send the current %S to the Sage process."
                                  type)
         for int = (sage:aif (plist-get plist :interactive)
                       `(interactive ,it)
                     '(interactive))
         for args = (sage:aif (plist-get plist :args) it '())
         for doc-name = (sage:aif (plist-get plist :doc-name)
                            it
                          (symbol-name type))
         append
         (loop for b in '(t nil)
               for func-name =  (cond (b (concat func-name-base "-and-go"))
                                      (t func-name-base))
               collect
               `(defun ,(intern func-name) ,args ,doc-string ,int
                  (sage-edit:send-obj-base :type ,type :switch-p ,b))))))

(sage-edit:defun-exec-commands)

(defun sage-edit:read-script-file ()
  (read-file-name "Load Sage file: "
                  nil
                  (sage:awhen (buffer-file-name) it)))


(defun* sage-edit:load-file-base
  (&key command switch-p (display-function nil))
  (sage-edit:exec-command-base
   :command command
   :switch-p switch-p
   :display-function display-function
   :pre-message "Loading the file to the Sage process..."
   :post-message "Loading the file to the Sage process... Done.")
  (sage-shell:clear-command-cache))

(defun sage-edit:load-file (file-name)
  "Load a Sage file FILE-NAME to the Sage process."
  (interactive (list (sage-edit:read-script-file)))
  (sage-edit:load-file-base
   :command (format "load('%s')" file-name)))

(defun sage-edit:attach-file (file-name)
  "Attach a Sage file FILE-NAME to the Sage process."
  (interactive (list (sage-edit:read-script-file)))
  (sage-edit:load-file-base
   :command (format "attach('%s')" file-name)))

(defun sage-edit:load-file-and-go (file-name)
  "Load a Sage file FILE-NAME to the Sage process."
  (interactive (list (sage-edit:read-script-file)))
  (sage-edit:load-file-base
   :command (format "load('%s')" file-name)
   :switch-p t))

(defun sage-edit:load-current-file ()
  "Load the current file to the Sage process."
  (interactive)
  (sage:aif (buffer-file-name) (sage-edit:load-file it)))

(defun sage-edit:load-current-file-and-go ()
  "Load the current file to the Sage process."
  (interactive)
  (sage:aif (buffer-file-name) (sage-edit:load-file-and-go it)))

(defun sage-edit:pop-to-process-buffer ()
  "Switch to the Sage process buffer."
  (interactive)
  (pop-to-buffer sage-shell:process-buffer))


;;; sage-mode
;;;###autoload
(define-derived-mode sage-mode python-mode "Sage")

(sage:define-keys sage-mode-map
  "C-c C-c" 'sage-edit:send-buffer
  "C-c C-r" 'sage-edit:send-region
  "C-M-x" 'sage-edit:send-defun
  "C-c C-l" 'sage-edit:load-file
  "C-c C-z" 'sage-edit:pop-to-process-buffer
  "C-c C-j" 'sage-edit:send-line)

(defvar sage-mode-hook nil "Hook run when entering Sage mode.")


;; (package-generate-autoloads "sage-shell" default-directory)

(provide 'sage-shell)
;;; sage-shell.el ends here
