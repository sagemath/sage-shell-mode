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
  (let* ((plist (cdr name-plist))
         (default-directory (sage-shell-remote:-expand-fname
                             (or (plist-get plist :default-directory)
                                 "~/")
                             plist))
         ;; Spwan process
         (proc-buf (sage-shell:run cmd new switch-function (car name-plist))))
    (with-current-buffer proc-buf
      (setq sage-shell:remote-name-plist name-plist))))

(defun sage-shell-remote:-expand-fname (f plist)
  (tramp-make-tramp-file-name
   (plist-get plist :method)
   (plist-get plist :user)
   (plist-get plist :host)
   f))

(defun sage-shell-remote:run-sage (name)
  "Run remote Sage associated to NAME."
  (interactive (list (sage-shell-remote:-read-name)))
  (sage-shell:aif (assoc name sage-shell-remote:sage-plists)
      (sage-shell:run-remote
       (or (plist-get (cdr it) :executable) "sage") nil it)
    (error (concat
            "Invalid name. Please set `sage-shell-remote:sage-plists'"
            " correctly."))))

(defun sage-shell-remote:-read-name ()
  (completing-read "Sage Remote: "
                   (mapcar #'car sage-shell-remote:sage-plists)))
