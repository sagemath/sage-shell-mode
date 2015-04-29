(require 'sage-shell-mode)
(require 'tramp)

(defcustom sage-shell-remote:sage-plists nil
  "plists"
  :group 'sage-shell)

(cl-defun sage-shell:run-remote
    (cmd new name-plist &optional (switch-function 'switch-to-buffer))
  "Running Sage function internal.
SIWTCH-FUNCTION is 'no-switch, or a function with one
argument."
  (let ((default-directory (sage-shell-remote:plist-to-file-name
                            (cdr name-plist))))
    (sage-shell:run cmd new switch-function (car name-plist))))

(defun sage-shell-remote:plist-to-file-name (plist)
  (tramp-make-tramp-file-name
   (plist-get plist :method)
   (plist-get plist :user)
   (plist-get plist :host)
   (plist-get plist :directory)))

(defun sage-shell-remote:run-sage (name)
  "Run remote Sage associated to NAME."
  (interactive (list (sage-shell-remote:-read-name)))
  (sage-shell:aif (assoc name sage-shell-remote:sage-plists)
      (sage-shell:run-remote "sage" nil it)
    (error (concat
            "Invalid name. Please set `sage-shell-remote:sage-plists'"
            " correctly."))))

(defun sage-shell-remote:-read-name ()
  (completing-read "Sage Remote: "
                   (mapcar #'car sage-shell-remote:sage-plists)))
