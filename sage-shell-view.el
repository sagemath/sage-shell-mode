;;; sage-view.el --- Typeset Sage output on the fly -*- lexical-binding: t -*-

;; Copyright (C) 2008 ~ 2016  Matthias Meulien, Nick Alexander,
;; 2016 ~ Sho Takemori <stakemorii@gmail.com>

;; Author: Sho Takemori <stakemorii@gmail.com>
;; Keywords: sage, math, image
;; URL: https://github.com/sagemath/sage-shell-mode
;; Original authors: Matthias Meulien <matthias.meulien@xlim.fr>, Nick Alexander
;; <ncalexander@gmail.com>

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

;; This is a port of sage-view written by Matthias Meulien and Nick Alexander.

;; `sage-shell-view' typesets output in an `sage-shell-mode' buffer and displays
;; plots inline in an `sage-shell-mode'.  Inline displays are context
;; sensitive; by default, right-clicking brings up a context-menu.

;; Use `sage-shell-view' to enable the minor mode, and then
;; `sage-shell-view-enable-inline-output', `sage-shell-view-disable-inline-output' and
;; `sage-shell-view-enable-inline-plots', `sage-shell-view-disable-inline-plots' enable
;; and disable the relevant features.  You might add some of those functions
;; to `sage-shell-view-hook' to configure `sage-shell-view' to your liking.

;; You can customize `sage-shell-view' using the Emacs customize interface by M-x
;; customize-group RET sage-shell-view RET.  In particular you can customize
;; magnification and margins

;; This mode was inspired by doc-view.el by Tassilo Horn, preview.el
;; by David Kastrup, and imath.el by Yasuaki Honda.

;; The LaTeX style used by preview.el is mandatory to use
;; sage-shell-view.el.  It is shipped with AUCTeX.

;;; Code:

;;; Todo:
;; - Add a auto-reveal stuff to overlays
;; - Check that display is image capable
;; - Disabling sage-view mode should remove overlays
;; - Set color, center image, enlarge overlay to window full size
;; - Add zoom features to overlays
;; - Add horizontal scrolling

;; Bugs:
;; - Numpy output can be a text array... should not be inserted into
;;   $$ signs (hum... example?)

(require 'mouse)
(require 'sage-shell-mode)

(defgroup sage-shell-view nil "Typeset Sage output on the fly"
  :group 'sage-shell
  :prefix "sage-shell-view-")

(defcustom sage-shell-view-gs-command
  (if (eq system-type 'windows-nt)
      "GSWIN32C.EXE"
    "gs")
  "*Ghostscript command to convert from PDF to PNG.

See also `sage-shell-view-gs-options', `sage-shell-view-anti-aliasing-level'
and `sage-shell-view-scale'."
  :type 'string
  :group 'sage-shell-view)

(defcustom sage-shell-view-default-commands t
  "Determine what to enable when `sage-shell-view' is started.
If equal to the symbol `plots' then will start inline plotting.
If equal to the symbol `output' then will start typesetting output.
Otherwise, if non-nil will start both.

Each of these can be enabled or disabled later by calling
`sage-shell-view-enable-inline-plots', `sage-shell-view-disable-inline-plots',
`sage-shell-view-enable-inline-output', or `sage-shell-view-disable-inline-output'."
  :type '(choice (const :tag "Inline Plots" plots)
                 (const :tag "Typeset Output" output)
                 (const :tag "Both" t))
  :group 'sage-shell-view)

(defcustom sage-shell-view-latex-preamble
  "\\usepackage{amstext}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{amsfonts}
\\usepackage{graphicx}
\\usepackage{mathrsfs}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
% we need preview
\\usepackage[active, tightpage, pdftex, displaymath]{preview}
% macros sage uses
\\newcommand{\\ZZ}{\\Bold{Z}}
\\newcommand{\\NN}{\\Bold{N}}
\\newcommand{\\RR}{\\Bold{R}}
\\newcommand{\\CC}{\\Bold{C}}
\\newcommand{\\QQ}{\\Bold{Q}}
\\newcommand{\\QQbar}{\\overline{\\QQ}}
\\newcommand{\\GF}[1]{\\Bold{F}_{#1}}
\\newcommand{\\Zp}[1]{\\ZZ_{#1}}
\\newcommand{\\Qp}[1]{\\QQ_{#1}}
\\newcommand{\\Zmod}[1]{\\ZZ/#1\\ZZ}
\\newcommand{\\CDF}{\\Bold{C}}
\\newcommand{\\CIF}{\\Bold{C}}
\\newcommand{\\CLF}{\\Bold{C}}
\\newcommand{\\RDF}{\\Bold{R}}
\\newcommand{\\RIF}{\\Bold{I} \\Bold{R}}
\\newcommand{\\RLF}{\\Bold{R}}
\\newcommand{\\CFF}{\\Bold{CFF}}
"
  "The default LaTeX preamble."
  :type 'string
  :group 'sage-shell-view)

(defcustom sage-shell-latex-foreground-color nil
  "Foreground color used in LaTeX image as string.
If the value is `nil', then this variable is ignored."
  :type '(choice (const :tag "Not Specified" nil)
                 (string :tag "Color"))
  :group 'sage-shell-view)

(defcustom sage-shell-latex-background-color nil
  "Background color used in LaTeX image as string.
If the value is `nil', then this variable is ignored."
  :type '(choice (const :tag "Not Specified" nil)
                 (string :tag "Color"))
  :group 'sage-shell-view)

(defcustom sage-shell-view-gs-options
  '("-sDEVICE=png16m" "-dBATCH" "-dSAFER" "-q" "-dNOPAUSE")
  "*Options for Ghostscript when converting from PDF to PNG."
  :type 'list
  :group 'sage-shell-view)

(defcustom sage-shell-view-anti-aliasing-level 4
  "*Level of anti-aliasing used when converting from PDF to PNG. "
  :type '(choice (const :tag "No anti-aliasing" 1)
                 (const :tag "Some anti-aliasing" 2)
                 (const :tag "More anti-aliasing (slower, but prettier)." 4))
  :group 'sage-shell-view)

(defcustom sage-shell-view-margin '(1 . 1)
  "*Margin (in pixels or (pixels-x . pixels-y)) added around displayed images."
  :type '(choice integer (cons integer integer))
  :group 'sage-shell-view)

(defcustom sage-shell-view-scale-factor 0.2
  "*Factor used when zooming."
  :type 'number
  :group 'sage-shell-view)

(defcustom sage-shell-view-default-resolution 120
  "Resultion used when converting from PDF to PNG.
If it is `nil', then the function `sage-shell-view-compute-resolution'
computes the resolution automatically."
  :type 'number
  :group 'sage-shell-view)

(defvar sage-shell-view-scale 1.0
  "Scale used when converting from PDF/PS to PNG.")

(defun sage-shell-view-color-to-rgb (str)
  "Convert color name STR to rgb values understood by TeX."
  (mapcar (lambda (x) (/ x 65535.0)) (color-values str)))

(defun sage-shell-view-latex-str (math-expr)
  "LaTeX string to be inserted a tmp file."
  (format
   "\\documentclass{article}
\\usepackage{xcolor}
\\pagecolor[rgb]{%s}
%s
\\begin{document}
\\definecolor{mycolor}{rgb}{%s}
\\begin{preview}
\\begin{math}
\\color{mycolor}
%s
\\end{math}
\\end{preview}
\\end{document}
"
   (mapconcat #'number-to-string
              (sage-shell-view-color-to-rgb
               (or sage-shell-latex-background-color
                   (frame-parameter nil 'background-color)))
              ",")
   sage-shell-view-latex-preamble
   (mapconcat #'number-to-string
              (sage-shell-view-color-to-rgb
               (or sage-shell-latex-foreground-color
                   (frame-parameter nil 'foreground-color)))
              ",")
   math-expr))

(defun sage-shell-view-dir-name ()
  (sage-shell-edit--set-and-make-temp-dir)
  (buffer-local-value 'sage-shell-edit:temp-directory
                      sage-shell:process-buffer))

(defun sage-shell-view-overlay-activep (ov)
  "Check whether there is a valid image associated with OV."
  (equal (car (overlay-get ov 'display)) 'image))

(defun sage-shell-view-regenerate (ov)
  "Return zoom to normal and regenerate the overlay."
  (overlay-put ov 'scale sage-shell-view-scale)
  (sage-shell-view-process-overlay ov))

(defun sage-shell-view-zoom-in (ov &optional multiplier)
  "Internal function to zoom in on an overlay."
  (unless (numberp multiplier)
    (setq multiplier 1))
  (let ((scale (or (overlay-get ov 'scale) sage-shell-view-scale)))
    (overlay-put ov 'scale (+ scale (* multiplier sage-shell-view-scale-factor)))
    (message "Overlay's scale set to %s" scale)
    (sage-shell-view-process-overlay ov)))

(defun sage-shell-view-zoom-out (ov &optional multiplier)
  "Internal function to zoom out on an overlay."
  (unless (numberp multiplier)
    (setq multiplier 1))
  (let* ((scale (or (overlay-get ov 'scale) sage-shell-view-scale))
         (new-scale (- scale (* multiplier sage-shell-view-scale-factor))))
    ;; Ensure it's not too small (or negative)
    (when (< new-scale sage-shell-view-scale-factor)
      (setq new-scale sage-shell-view-scale-factor))
    (overlay-put ov 'scale new-scale)
    (message "Overlay's scale set to %s" scale)
    (sage-shell-view-process-overlay ov)))

(defmacro sage-shell-view--when-overlay-active (ov &rest body)
  (declare (indent 1))
  `(if (sage-shell-view-overlay-activep ,ov)
       (progn ,@body)
     (error "There is no valid image associated with the overlay.")))

(defun sage-shell-view-context-menu (ov ev)
  "Pop up a menu for OV at position EV."
  (popup-menu
   `("Sage View Mode"
     ["Regenerate" (lambda () (interactive) (sage-shell-view-regenerate ,ov))]
     ["Copy Text"  (lambda () (interactive) (sage-shell-view-copy-text ,ov))]
     ["Copy LaTeX" (lambda () (interactive) (sage-shell-view-copy-latex ,ov))]
     ["Save As..." (lambda () (interactive)
                     (sage-shell-view--when-overlay-active ,ov
                       (sage-shell-view-save-image ,ov)))]
     ["Zoom in" (lambda (multiplier) (interactive "p")
                  (sage-shell-view--when-overlay-active ,ov
                    (sage-shell-view-zoom-in ,ov multiplier)))]
     ["Zoom out" (lambda (multiplier)
                   (interactive "p")
                   (sage-shell-view--when-overlay-active ,ov
                     (sage-shell-view-zoom-out ,ov multiplier)))]
     "--"
     ["Customize Conversion Options"
      (lambda () (interactive)
        (sage-shell-view--when-overlay-active ,ov
          (customize-group 'sage-shell-view t)))])
   ev))

(defun sage-shell-view-cleanup-copied-text (str)
  "Remove some boilerplate text added by Sage to all LaTeX output."
  (replace-regexp-in-string
   (regexp-quote "\\newcommand{\\Bold}[1]{\\mathbf{#1}}")
   "" str))

(defun sage-shell-view-copy-text (ov)
  "Copy text source of OV into the kill buffer."
  (let ((text (overlay-get ov 'text)))
    (if text
        (kill-new text)
      (message "No text available"))))

(defun sage-shell-view-copy-latex (ov)
  "Copy LaTeX source of OV into the kill buffer."
  (let ((text (overlay-get ov 'math)))
    (if text
        (kill-new (sage-shell-view-cleanup-copied-text text))
      (message "No LaTeX code available"))))

(defvar sage-shell-view-plot-regex
  (rx "BEGIN_PNG:" (group (1+ nonl)) ":END_PNG")
  "Regular expression matching a plot output in Sage output.")

(defun sage-shell-view-save-image (ov)
  "Copy image file associated to OV.

Make sure that there is a valid image associated with OV with
`sage-shell-view-overlay-activep'."
  (let* ((spec (cdr (overlay-get ov 'display)))
         (file (plist-get spec :file))
         (name (when (and file (file-readable-p file))
                 (expand-file-name
                  (read-file-name "Write image to file: "
                                 default-directory
                                 "sage-shell-view.png")))))
    (if name
        (copy-file file name))))

(defun sage-shell-view-plot-context-menu (ov ev)
  "Pop up a menu for OV at position EV."
  (popup-menu
   (list
    "Sage View Mode"
    (vector
     "Save As..."
      (lambda () (interactive)
        (sage-shell-view--when-overlay-active ov
          (sage-shell-view-save-image ov)))))
   ev))

(defun sage-shell-view-compute-resolution (scale)
  (if (display-graphic-p)
      ;; In a terminal, display-mm-width returns nil and
      ;; display-pixel-width returns the number of characters.
      (let ((w (* scale (/ (* 25.4 1.4 (display-pixel-width))
                           (display-mm-width))))
            (h (* scale (/ (* 25.4 1.4 (display-pixel-height))
                           (display-mm-height)))))
        (concat (int-to-string w) "x" (int-to-string h)))
    "72x72"))

(defun sage-shell-view-process-overlay (ov)
  "Associate a LATEX document to OV and start conversion process
from LATEX to PDF."
  (let* ((base (expand-file-name
                (make-temp-name "sage-shell-view_") (sage-shell-view-dir-name))))
    (with-temp-file (concat base ".tex")
      (insert (sage-shell-view-latex-str
               (overlay-get ov 'math)))
      ;; The LaTeX created by Sage for MathJax (in some cases) isn't valid.
      ;; This is our attempt to work around it.
      (goto-char (point-min))
      (while (search-forward-regexp "\\verb!\\([^!]*\\)!"  nil t)
        (replace-match "\mathtt{\\1}")))

    (overlay-put ov 'file-sans-extension base)

    (deferred:$
      (apply #'deferred:process "latex" (sage-shell-view--latex-option base))

      (deferred:nextc it
        (lambda (_) (sage-shell-view--pdf-to-png ov))))))

(defun sage-shell-view--pdf-to-png (ov)
  (let ((base (overlay-get ov 'file-sans-extension)))
    (deferred:$
      (apply #'deferred:process
             sage-shell-view-gs-command
             (sage-shell-view--gs-option
              (overlay-get ov 'scale)
              base))
      (deferred:nextc it
        (lambda (_)
          (overlay-put ov 'display
                       (list 'image :type 'png :file (concat base ".png")
                             :margin sage-shell-view-margin)))))))

(defun sage-shell-view--latex-option (base)
  (list (concat "--output-directory=" (sage-shell-view-dir-name))
        (concat "-interaction=" "nonstopmode")
        (concat "-output-format=" "pdf")
        (concat base ".tex")))

(defun sage-shell-view--gs-option (scale base)
  (let* ((png (concat base ".png"))
         (pdf (concat base ".pdf"))
         (level (int-to-string sage-shell-view-anti-aliasing-level))
         (scale (or scale sage-shell-view-scale)))
    (append
     sage-shell-view-gs-options
     (list (concat "-dTextAlphaBits=" level)
           (concat "-dGraphicsAlphaBits=" level)
           (concat "-sOutputFile=" png)
           (format "-r%s" (if sage-shell-view-default-resolution
                              (sage-shell:->>
                               (* scale sage-shell-view-default-resolution)
                               round
                               int-to-string)
                            (sage-shell-view-compute-resolution
                             scale)))
           pdf))))

(defvar sage-shell-view-inline-plots-enabled nil)
(defvar sage-shell-view-inline-output-enabled nil)

(defun sage-shell-view-output-filter-process-inline-plots (_string)
  "Generate and place one overlay image for one inline plot,
found by looking for a particular png file in directory
`sage-shell-view-dir-name'.

This function expects the buffer to be narrowed to just the
current output; see `sage-shell-view-output-filter' for how to do
that."
  (goto-char (point-min))
  (while (re-search-forward
          sage-shell-view-plot-regex
          (point-max) t)
    (let* ((plot-beg (match-beginning 0))
           (plot-end (match-end 0))
           (pngname (match-string-no-properties 1))
           (base (expand-file-name (make-temp-name "sage-shell-view-plot_")
                                   (sage-shell-view-dir-name)))
           (pngname2 (concat base ".png")))
      (when (and pngname
                 (file-exists-p pngname)
                 (file-readable-p pngname))
        ;; the found branch
        (rename-file pngname pngname2 t)
        (goto-char comint-last-input-end)
        (let ((im (create-image pngname2 'png))
              (ov (make-overlay plot-beg plot-end
                                nil nil nil))
              (map (make-sparse-keymap)))
          (overlay-put ov 'display im)
          ;; help alignment as much as possible
          (overlay-put ov 'before-string "\n")
          (overlay-put ov 'sage-shell-view t)
          (define-key map [mouse-3]
            (lambda (event) (interactive "e")
              (sage-shell-view-plot-context-menu ov event)))
          (overlay-put ov 'keymap map))))))

(defvar sage-shell-view-output-regexp
  (rx "BEGIN_TEXT:"
      (group (minimal-match (0+ (or nonl "\n"))))
      ":END_TEXT\nBEGIN_LATEX:"
      (group (minimal-match (0+ (or nonl "\n"))))
      ":END_LATEX")
  "Regular expression matching typeset output from BackendEmacs.")

(defun sage-shell-view-output-filter-process-inline-output (_string)
  "Substitute overlays to inline output.

Each region delimited by `sage-shell-view-start-string' and
`sage-shell-view-final-string' is replaced by an overlay.

This function expects the buffer to be narrowed to the current
output. And should be wrapped in a `save-excursion' and
`save-restriction' call.

See also `sage-shell-view-output-filter'."
  (goto-char (point-min))
  (while (re-search-forward sage-shell-view-output-regexp (point-max) t)
    (let* ((full-beg (match-beginning 0))
           (full-end (match-end 0))
           (text-beg (match-beginning 1))
           (text-end (match-end 1))
           (latex-beg (match-beginning 2))
           (latex-end (match-end 2))
           (text (buffer-substring-no-properties text-beg text-end))
           (latex (buffer-substring-no-properties latex-beg latex-end))
           (ov (make-overlay full-beg full-end
                             nil nil nil))
           (map (make-sparse-keymap)))
      ;; Delete everything except the text
      (delete-region text-end full-end)
      (delete-region full-beg text-beg)
      ;; Populate the overlay
      (overlay-put ov 'help-echo "mouse-3: Open contextual menu")
      (overlay-put ov 'text text)
      (overlay-put ov 'math latex)
      (define-key map [mouse-3]
        `(lambda (event) (interactive "e")
           (sage-shell-view-context-menu ,ov event)))
      (overlay-put ov 'keymap map)
      (overlay-put ov 'sage-shell-view t)
      (sage-shell-view-process-overlay ov))))

(defun sage-shell-view-output-filter (string)
  "Generate and place overlay images for inline output and inline plots.

Function to be inserted in `comint-output-filter-functions'."
  (save-excursion
    (save-restriction
      (narrow-to-region comint-last-input-end
                        (process-mark (get-buffer-process (current-buffer))))
      (when sage-shell-view-inline-plots-enabled
        (sage-shell-view-output-filter-process-inline-plots string))
      (when sage-shell-view-inline-output-enabled
        (sage-shell-view-output-filter-process-inline-output string)))))

(defun sage-shell-view-update-modeline ()
  "Update modeline to include information about whether sage-shell-view is enabled."
  (when (eq major-mode 'sage-shell-mode)
    (let ((fmt (format "/%s%s"
                       (if sage-shell-view-inline-plots-enabled "p" "")
                       (if sage-shell-view-inline-output-enabled "t" "")))
          (bare-mode-name (if (string-match "\\(^[^/]*\\)/" mode-name)
                              (match-string 1 mode-name)
                            mode-name)))
      (setq mode-name
            (if (> (length fmt) 1)
                (concat bare-mode-name fmt)
              bare-mode-name))
      (force-mode-line-update))))

;;;###autoload
(defun sage-shell-view-enable-inline-output ()
  "Enable inline output pretty-printing, i.e. typeset output from sage in the `sage-shell-mode' buffer.
WARNING: this communicates with the sage process.  Only use this
when `sage-shell-view' mode is enabled and sage is running."
  (interactive)
  (sage-shell-view-set-backend
   t
   sage-shell-view-inline-plots-enabled
   (lambda () (setq sage-shell-view-inline-output-enabled t)))
  (sage-shell-view-update-modeline))

;;;###autoload
(defun sage-shell-view-disable-inline-output ()
  "Disable inline output pretty-printing, i.e. do not typeset output from sage in the `sage-shell-mode' buffer.
WARNING: this communicates with the sage process.  Only use this
when `sage-shell-view' mode is enabled and sage is running."
  (interactive)
  (sage-shell-view-set-backend
   nil
   sage-shell-view-inline-plots-enabled
   (lambda () (setq sage-shell-view-inline-output-enabled nil)))
  (sage-shell-view-update-modeline))

;;;###autoload
(defun sage-shell-view-enable-inline-plots ()
  "Enable inline plotting, i.e. display plots in the `sage-shell-mode' buffer and do not spawn an external viewer.
WARNING: this communicates with the sage process.  Only use this
when `sage-shell-view' mode is enabled and sage is running."
  (interactive)
  (sage-shell-view-set-backend
   sage-shell-view-inline-output-enabled
   t
   (lambda () (setq sage-shell-view-inline-plots-enabled t)))
  (sage-shell-view-update-modeline))

;;;###autoload
(defun sage-shell-view-disable-inline-plots ()
  "Disable inline plotting, i.e. do not display plots in the `sage-shell-mode' buffer and instead spawn an external viewer.
WARNING: this communicates with the sage process.  Only use this
when `sage-shell-view' mode is enabled and sage is running."
  (interactive)
  (sage-shell-view-set-backend
   sage-shell-view-inline-output-enabled
   nil
   (lambda () (setq sage-shell-view-inline-plots-enabled nil)))
  (sage-shell-view-update-modeline))

(defconst sage-shell-view--mod-name "_emacs_sage_shell_view")

(defun sage-shell-view--to-py-bool (a)
  (if a "True" "False"))

(defun sage-shell-view-set-backend (text plot success-callback)
  (cl-check-type success-callback function)
  (sage-shell-view--init)
  (sage-shell:run-cell
   (format "%s.%s(text=%s, plot=%s)"
           sage-shell-view--mod-name
           "set_backend"
           (sage-shell-view--to-py-bool text)
           (sage-shell-view--to-py-bool plot))
   :sync t
   :callback
   (lambda (res)
     (if (sage-shell:output-stct-success res)
         (funcall success-callback)
       (sage-shell--error-callback res)))))

(defvar sage-shell-view--init-completed-p nil)
(make-variable-buffer-local 'sage-shell-view--init-completed-p)

(defun sage-shell-view--init ()
  (unless sage-shell-view--init-completed-p
    (sage-shell:run-cell
     "import emacs_sage_shell_view as _emacs_sage_shell_view"
     :sync t
     :callback #'sage-shell--error-callback)
    (with-current-buffer sage-shell:process-buffer
      (setq sage-shell-view--init-completed-p t))))

;;;###autoload
(define-minor-mode sage-shell-view
  "Toggle automatic typesetting of Sage output.

Typesetting of math formulas is done by LATEX subprocesses and
PDF to PNG conversions." nil
  :group 'sage-shell-view
  :lighter " sage-shell-view"
  (if sage-shell-view
      (progn
        (cond
         ((eq sage-shell-view-default-commands 'plots)
          (sage-shell-view-enable-inline-plots))
         ((eq sage-shell-view-default-commands 'output)
          (sage-shell-view-enable-inline-output))
         (sage-shell-view-default-commands
          (sage-shell-view-enable-inline-plots)
          (sage-shell-view-enable-inline-output)))
        (add-hook 'comint-output-filter-functions
                  'sage-shell-view-output-filter nil t))
    (progn
      (sage-shell-view-disable-inline-output)
      (sage-shell-view-disable-inline-plots)
      (remove-hook 'comint-output-filter-functions 'sage-shell-view-output-filter
                   t))))

(provide 'sage-shell-view)
;;; sage-shell-view.el ends here