(with-no-warnings (require 'cl))
(progn
  (with-no-warnings (require 'cl))

  (defun sage-install-read-sage-root ()
    (let ((d nil))
      (while (not (progn
                    (setq d (read-directory-name "Please specify Sage's root directory: "))
                    (file-executable-p
                     (expand-file-name "sage" d)))))
      d))

  (defun sage-install-sage-root ()
    (cond
     ((executable-find "sage") nil)
     ((and (eq system-type 'darwin)
           (yes-or-no-p "Cannot find command 'sage'. Guess Sage's root directory?: "))
      (or (sage-install-guess-sage-root)
          (sage-install-read-sage-root)))
     (t (sage-install-read-sage-root))))

  (defun sage-install-guess-sage-root ()
    (let ((sage-app (loop for f in (directory-files "/Applications/")
                          if (string-match (rx "Sage" (1+ nonl) ".app") f)
                          return f)))
      (when sage-app
        (expand-file-name "Contents/Resources/sage/"
                          (expand-file-name sage-app "/Applications")))))

  (defun sage-install-sage-root-config ()
    (let ((r (sage-install-sage-root)))
      (when r
        (format "(setq sage-shell:sage-root \"%s\")\n" r))))

  (defmacro sage-install-print-forms (&rest body)
    `(apply
      'concat
      ',(loop for s in body
              collect (let ((print-quoted t))
                        (concat (prin1-to-string s) "\n")))))

  (defun sage-install-ac-config ()
    (when (featurep 'auto-complete)
      (concat
       ";; Enable auto-complete-mode in Sage buffers\n"
       (sage-install-print-forms
        (setq ac-modes (append '(sage-mode sage-shell-mode) ac-modes))
        (add-hook 'sage-shell-mode-hook 'sage-shell-ac:add-sources)
        (add-hook 'sage-mode-hook 'sage-edit-ac:add-sources)))))

  (defun* sage-install-common-config
      (&optional (install-dir "~/.emacs.d/sage-shell"))
    (concat
     (format "(add-to-list 'load-path \"%s\")\n" install-dir)
     (sage-install-print-forms
      (require 'sage-shell-autoloads)
      (add-to-list 'auto-mode-alist (cons "\\.sage$" 'sage-mode)))))

  (defun sage-install-cpl-type-config ()
    (cond ((featurep 'anything-match-plugin)
           (concat
            ";; Use anything for completion\n"
            (sage-install-print-forms
             (setq sage-shell:completion-function
                   'anything-sage-shell)
             (setq sage-shell:help-completion-function
                   'anything-sage-shell-describe-object-at-point))))
          ((featurep 'helm-match-plugin)
           (concat
            ";; Use helm for completion\n"
            (sage-install-print-forms
             (setq sage-shell:completion-function
                   'helm-sage-shell)
             (setq sage-shell:help-completion-function
                   'helm-sage-shell-describe-object-at-point))))))

  (defun sage-install-insert-forms (&rest xs)
    (loop for x in xs
          if x
          do (insert x)))

  (defun* sage-install-insert-config
      (&optional (install-dir "~/.emacs.d/sage-shell"))
    (sage-install-insert-forms
     (sage-install-common-config install-dir)
     (sage-install-sage-root-config)
     (sage-install-ac-config)
     (sage-install-cpl-type-config)))

  (defun sage-install-download-files (url install-dir &rest files)
    (let ((command-fun
           (cond ((executable-find "wget")
                  (lambda (file)
                    (format "wget -q -O '%s' %s"
                            (expand-file-name file install-dir)
                            (concat url file))))
                 ((executable-find "curl")
                  (lambda (file)
                    (format "curl -s -o '%s' %s"
                            (expand-file-name file install-dir)
                            (concat url file))))
                 (t (error "Cannot find wget or curl.")))))
      (message "Downloading files...")
      (shell-command (mapconcat 'identity
                                (loop for f in files
                                      collect (funcall command-fun f))
                                "&&"))
      (message "Downloading files... Done.")))

  (defvar sage-install-files
    '("sage-shell.el"
      "anything-sage.el"
      "helm-sage.el"
      "auto-complete-sage.el"
      "sage-shell-autoloads.el"
      "emacs_sage_shell.py"))

  (defvar sage-install-dont-bytecompile-files
    (append
     (unless (require 'auto-complete nil t)
       '("auto-complete-sage.el"))
     (unless (require 'anything nil t)
       '("anything-sage.el"))
     (unless (require 'helm nil t)
       '("helm-sage.el"))
     '("sage-shell-autoloads.el"
       "emacs_sage_shell.py")))

  (defvar sage-install-url nil)

  (defvar sage-install-installation-directory nil)

  (unless (file-exists-p sage-install-installation-directory)
    (make-directory sage-install-installation-directory))

  (apply
   'sage-install-download-files
   sage-install-url
   sage-install-installation-directory
   sage-install-files)

  (add-to-list 'load-path sage-install-installation-directory)

  ;; Bytecompile files
  (loop for f in sage-install-files
        unless (member f sage-install-dont-bytecompile-files)
        do (ignore-errors
             (byte-compile-file
              (expand-file-name f sage-install-installation-directory))))


  (switch-to-buffer (get-buffer-create "*sage-shell-install*"))
  (insert (format ";; Put the following lines to %s \n" user-init-file))
  (newline)
  (sage-install-insert-config sage-install-installation-directory)
  (emacs-lisp-mode))
