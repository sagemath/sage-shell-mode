(require 'ert)
(require 'sage-shell-mode)
(require 'noflet)
(ert-deftest sage-shell:development-version-test ()
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

(defun sage-shell-test:temp-state
    (code int &optional pt)
  (with-temp-buffer
    (insert code)
    (goto-char (or pt (point-max)))
    (sage-shell-cpl:parse-current-state int)))

(defmacro sage-shell-test:state-assert (state &rest args)
  (declare (indent 1) (debug t))
  (let ((test-fns '((prefix . =)
                    (var-base-name . equal)
                    (types . equal)
                    (interface . string=))))
    (cons 'and
          (cl-loop for (a b) in (sage-shell:group args)
                   collect
                   (list (assoc-default a test-fns)
                         `(sage-shell-cpl:get ,state ',a) b)))))

(ert-deftest sage-shell:parse-state-repl-attribute ()
  (should (let ((state (sage-shell-test:temp-state
                        "sage: abc.a[0].aaa" "sage")))
            (sage-shell-test:state-assert state
              prefix 16
              var-base-name "abc.a[0]"
              types '("attributes")
              interface "sage"))))

(ert-deftest sage-shell:parse-state-repl-funcall ()
    (should (let ((state (sage-shell-test:temp-state "sage: f().foo" "sage")))
            (sage-shell-test:state-assert state
              var-base-name nil
              types '("interface")
              interface "sage"))))

(ert-deftest sage-shell:parse-state-repl-intf ()
  (should (let ((state (sage-shell-test:temp-state "sage: gap.ev" "sage")))
            (sage-shell-test:state-assert state
              types '("interface" "attributes")
              interface "gap")))

  (should (let ((state (sage-shell-test:temp-state
                        "sage: gap.eval(\"Ab\")" "sage"
                        18)))
            (sage-shell-test:state-assert state
              prefix 17
              types '("interface")
              interface "gap"))))

(ert-deftest sage-shell:parse-state-repl-block ()
  (should (let ((state (sage-shell-test:temp-state "sage: def foo(x):
....:     if abc" "sage")))
            (sage-shell-test:state-assert state
              types '("interface")
              interface "sage"
              prefix 32))))
