;;; sage-shell-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (anything-sage-command-history anything-sage-shell-describe-object-at-point
;;;;;;  anything-sage-shell) "anything-sage" "anything-sage.el" (21428
;;;;;;  10776))
;;; Generated autoloads from anything-sage.el

(autoload 'anything-sage-shell "anything-sage" "\


\(fn)" t nil)

(autoload 'anything-sage-shell-describe-object-at-point "anything-sage" "\


\(fn)" t nil)

(autoload 'anything-sage-command-history "anything-sage" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (sage-edit-ac:add-sources sage-shell-ac:add-sources)
;;;;;;  "auto-complete-sage" "auto-complete-sage.el" (21428 10776))
;;; Generated autoloads from auto-complete-sage.el

(autoload 'sage-shell-ac:add-sources "auto-complete-sage" "\


\(fn)" nil nil)

(autoload 'sage-edit-ac:add-sources "auto-complete-sage" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads (helm-sage-command-history helm-sage-shell-describe-object-at-point
;;;;;;  helm-sage-shell) "helm-sage" "helm-sage.el" (21428 10776))
;;; Generated autoloads from helm-sage.el

(autoload 'helm-sage-shell "helm-sage" "\


\(fn)" t nil)

(autoload 'helm-sage-shell-describe-object-at-point "helm-sage" "\


\(fn)" t nil)

(autoload 'helm-sage-command-history "helm-sage" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (sagetex-load-file sage-mode sage-shell:run-new-sage
;;;;;;  sage-shell:run-sage) "sage-shell" "sage-shell.el" (21429
;;;;;;  5895))
;;; Generated autoloads from sage-shell.el

(autoload 'sage-shell:run-sage "sage-shell" "\


\(fn CMD)" t nil)

(defalias 'run-sage 'sage-shell:run-sage)

(autoload 'sage-shell:run-new-sage "sage-shell" "\


\(fn CMD)" t nil)

(defalias 'run-new-sage 'sage-shell:run-new-sage)

(autoload 'sage-mode "sage-shell" "\


\(fn)" t nil)

(autoload 'sagetex-load-file "sage-shell" "\
Compile a TeX file, execute this command and compile the TeX file again.

\(fn FILENAME)" t nil)

;;;***

;;;### (autoloads nil nil ("sage-install.el") (21429 5899 69139))

;;;***

(provide 'sage-shell-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sage-shell-autoloads.el ends here
