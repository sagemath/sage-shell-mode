(require 'ert)
(require 'sage-shell-mode)

(ert-deftest sage-shell-mode:sage-shell:development-version-test ()
  (should (string=
           (sage-shell:development-version
            "/usr/lib/sagemath/local/lib/python2.7/site-packages/sage/rings/function_field/function_field.py")
           "/usr/lib/sagemath/src/sage/rings/function_field/function_field.py")))

