;; -*- lexical-binding: t -*-

(require 'ert)
(require 'sage-shell-mode)

(ert-deftest sage-shell:development-version-test ()
  (should (or (string= (user-login-name) "travis")
              (string=
               (sage-shell:src-version
                (expand-file-name "local/lib/python2.7/site-packages/sage/rings/function_field/function_field.py"
                                  (sage-shell:sage-root)))
               (expand-file-name "src/sage/rings/function_field/function_field.py"
                                 (sage-shell:sage-root)))))
  (should (or
           (string= (user-login-name) "travis")
           (string=
            (sage-shell:src-version
             (expand-file-name "local/lib/python2.7/site-packages/sage/misc/cachefunc.so"
                               (sage-shell:sage-root)))
            (expand-file-name "src/sage/misc/cachefunc.pyx"
                              (sage-shell:sage-root)))))
  (should (or
           (string= (user-login-name) "travis")
           (string=
            (sage-shell:site-package-version
             (expand-file-name "src/sage/rings/function_field/function_field.py"
                               (sage-shell:sage-root)))
            (expand-file-name "local/lib/python2.7/site-packages/sage/rings/function_field/function_field.py"
                              (sage-shell:sage-root))))))

(defun sage-shell-test:temp-state
    (code int &optional pt)
  (with-temp-buffer
    (insert code)
    (goto-char (or pt (point-max)))
    (sage-shell-cpl:parse-current-state int)))

(defun sage-shell-test:sage-mode-temp-state (code &optional pt)
  (with-temp-buffer
    (insert code)
    (goto-char (or pt (point-max)))
    (sage-shell-edit:parse-current-state)))

(defmacro sage-shell-test:state-assert (state &rest args)
  (declare (indent 1) (debug t))
  (let ((test-fns '((prefix . =)
                    (var-base-name . equal)
                    (types . equal)
                    (interface . string=)
                    (module-name . equal))))
    (cons 'and
          (cl-loop for (a b) in (sage-shell:group args)
                   collect
                   (list (or (assoc-default a test-fns) #'equal)
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
              types nil
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

(ert-deftest sage-shell:parse-state-repl-other-int-gap ()
  (should (let ((state (sage-shell-test:temp-state "gap: " "gap")))
            (sage-shell-test:state-assert state
              types '("interface")
              interface "gap"))))

(ert-deftest sage-shell:parse-state-repl-other-int-gp ()
  (should (let ((state (sage-shell-test:temp-state "pari: " "gp")))
            (sage-shell-test:state-assert state
              types '("interface")
              interface "gp"))))

(ert-deftest sage-shell:parse-state-edit-from-top-level ()
  (should (let ((state (sage-shell-test:sage-mode-temp-state
                        "from foo")))
            (sage-shell-test:state-assert state
              types '("modules")
              module-name nil))))

(ert-deftest sage-shell:parse-state-edit-from-sub-module ()
  (should (let ((state (sage-shell-test:sage-mode-temp-state
                        "from foo.bar.baz")))
            (sage-shell-test:state-assert state
              types '("modules")
              module-name "foo.bar"))))

(ert-deftest sage-shell:parse-state-edit-from-sub-module-in-block ()
  (should (let ((state (sage-shell-test:sage-mode-temp-state
                        "def foo():
    from foo.bar.baz")))
            (sage-shell-test:state-assert state
              types '("modules")
              module-name "foo.bar"))))

(ert-deftest sage-shell:parse-state-edit-from-vars-in-module ()
  (should (let ((state (sage-shell-test:sage-mode-temp-state
                        "from foo.bar import (Foo,
                     Bar, B")))
            (sage-shell-test:state-assert state
              types '("vars-in-module")
              module-name "foo.bar"))))

(ert-deftest sage-shell:parse-state-func-call ()
  (should (let ((state (sage-shell-test:temp-state
                        "sage: foo()" "sage" 11)))
            (sage-shell-test:state-assert state
              types '("in-function-call" "interface")
              in-function-call "foo"
              in-function-call-end 10
              in-function-call-base-name nil))))

(ert-deftest sage-shell:parse-state-func-call-1 ()
  (should (let ((state (sage-shell-test:temp-state
                        "sage: foo(1, 2, ((2, 3, 4, [5, 6])))" "sage" 33)))
            (sage-shell-test:state-assert state
              types '("in-function-call" "interface")
              in-function-call "foo"
              in-function-call-end 10))))

(ert-deftest sage-shell:parse-state-func-call-1 ()
  (should (let ((state (sage-shell-test:temp-state
                        "sage: foo(1, 2, ((2, 3, 4, [5, 6])))" "sage" 33)))
            (sage-shell-test:state-assert state
              types '("in-function-call" "interface")
              in-function-call "foo"
              in-function-call-end 10))))

(ert-deftest sage-shell:parse-state-edit-func-call ()
  (should (let ((state (sage-shell-test:sage-mode-temp-state
                        "foo(1, 2,
((2, 3, 4, [5, 6])))" 25)))
            (sage-shell-test:state-assert state
              types '("interface")
              in-function-call "foo"
              in-function-call-end 4))))

(ert-deftest sage-shell:parse-state-edit-func-call-1 ()
  (should (let ((state (sage-shell-test:sage-mode-temp-state
                            "foo(1, 2,
((2, 3, 4, [5, 6])))")))
            (sage-shell-test:state-assert state
              types '("interface")
              in-function-call nil))))

(ert-deftest sage-shell:split-args ()
  (should (= (length
              (sage-shell:-eldoc-split-buffer-args "1, 2, ((2, 3, 4, [5,"))
             3)))

(ert-deftest sage-shell:split-args-1 ()
  (should (= (length
              (sage-shell:-eldoc-split-buffer-args
               "1,     2,3,4,
foo=bar(1, 2),baz=(1, 2"))
             6)))

(ert-deftest sage-shell:split-args-2 ()
  (should (= (length
              (sage-shell:-eldoc-split-buffer-args
               (concat "[foo(1, 2), ((3, 4), 5)],  "
               "
(((a, b), c))[0],
foo=bar(1, 2), baz=(1, 2")))
             4)))


(defvar sage-shell-test:eldoc-str
  "foo(a, b, bar=[0, (1, 2), (3, (5, 6)), '**kwds'], foo_bar='x, y, *args', **kwds)")

(ert-deftest sage-shell:eldoc-highlight ()
  (should (equal (sage-shell:-eldoc-highlight-beg-end
                  "foo" sage-shell-test:eldoc-str "a" nil)
                 (cons 4 5))))

(ert-deftest sage-shell:eldoc-highlight-1 ()
  (should (equal (sage-shell:-eldoc-highlight-beg-end
                  "foo" sage-shell-test:eldoc-str "c" nil)
                 (cons 73 79))))

(ert-deftest sage-shell:eldoc-highlight-2 ()
  (should (equal (sage-shell:-eldoc-highlight-beg-end
                  "foo" sage-shell-test:eldoc-str "bar" nil)
                 (cons 10 48))))

(ert-deftest sage-shell:eldoc-highlight-3 ()
  (should (equal (sage-shell:-eldoc-highlight-beg-end
                  "foo" "foo(a, b, *args, **kwds)" nil 5)
                 (cons 10 15))))

(ert-deftest sage-shell:eldoc-highlight-4 ()
  (should (equal (sage-shell:-eldoc-highlight-beg-end
                  "foo" "foo(a, b, *args, **kwds)" "bar" nil)
                 (cons 17 23))))

(defun sage-shell-test--start-sage-sync ()
  (let ((proc-buf (sage-shell:run-sage "sage")))
    (with-current-buffer proc-buf
      (while (null (sage-shell:output-finished-p))
        (accept-process-output nil 0 100))
      proc-buf)))

(when (executable-find "sage")

  (let ((proc-buf (sage-shell-test--start-sage-sync)))

    (let* ((rand-str (md5 (current-time-string)))
           (callback (sage-shell:send-command (format "print '%s'" rand-str)
                                              proc-buf)))
      (sage-shell:after-redirect-finished
        (ert-deftest sage-shell:test-send-command ()
          (should (equal (funcall callback) (format "%s\n" rand-str))))))


    (ert-deftest sage-shell:test-runcell-sync ()
      (equal (sage-shell:run-cell-raw-output "10.factorial()"
                                             :to-string t
                                             :process-buffer proc-buf)
             "3628800\n"))

    (let ((rand-str (md5 (current-time-string))))
      (sage-shell:run-cell
       (format "print '%s'" rand-str)
       :process-buffer proc-buf
       :call-back (lambda (res)
                    (ert-deftest sage-shell:test-run-cell-1 ()
                      (should (equal (sage-shell:output-stct-output res)
                                     (format "%s\n" rand-str)))
                      (should (equal (sage-shell:output-stct-success res) t))))))

    (sage-shell:run-cell
     "x/(x - x)"
     :process-buffer proc-buf
     :call-back (lambda (res)
                  (ert-deftest sage-shell:test-run-cell-2 ()
                    (should (equal (sage-shell:output-stct-success res) nil))
                    (should (string-match "ZeroDivisionError"
                                          (sage-shell:output-stct-output res))))))

    ;; Wait for evaliation completes
    (with-current-buffer proc-buf
      (while (null (sage-shell:redirect-finished-p))
        (accept-process-output nil 0 100)))))
