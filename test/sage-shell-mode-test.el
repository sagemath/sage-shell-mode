(require 'ert)
(require 'sage-shell-mode)
(require 'noflet)
(ert-deftest sage-shell-mode:sage-shell:development-version-test ()
  (should (string=
           (sage-shell:src-version
            (expand-file-name "local/lib/python2.7/site-packages/sage/rings/function_field/function_field.py"
                              (sage-shell:sage-root)))
           (expand-file-name "src/sage/rings/function_field/function_field.py"
                             (sage-shell:sage-root))))
  (should (string=
           (sage-shell:src-version
            (expand-file-name "local/lib/python2.7/site-packages/sage/misc/cachefunc.so"
                              (sage-shell:sage-root)))
           (expand-file-name "src/sage/misc/cachefunc.pyx"
                             (sage-shell:sage-root))))
  (should (string=
           (sage-shell:site-package-version
            (expand-file-name "src/sage/rings/function_field/function_field.py"
                              (sage-shell:sage-root)))
           (expand-file-name "local/lib/python2.7/site-packages/sage/rings/function_field/function_field.py"
                             (sage-shell:sage-root)))))
