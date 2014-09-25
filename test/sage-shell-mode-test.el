(require 'ert)
(require 'sage-shell-mode)
(require 'noflet)
(ert-deftest sage-shell-mode:sage-shell:development-version-test ()
  (noflet ((sage-shell:sage-root () "/usr/lib/sagemath/"))
          (should (string=
                   (sage-shell:development-version
                    "/usr/lib/sagemath/local/lib/python2.7/site-packages/sage/rings/function_field/function_field.py")
                   "/usr/lib/sagemath/src/sage/rings/function_field/function_field.py"))
          (should (string=
                   (sage-shell:development-version
                    "/usr/lib/sagemath/local/lib/python2.7/site-packages/sage/misc/cachefunc.so")
                   "/usr/lib/sagemath/src/sage/misc/cachefunc.pyx"))))
