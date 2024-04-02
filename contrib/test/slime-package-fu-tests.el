(require 'slime-package-fu)
(require 'slime-tests)

(defun slime-setup-defpackage-buffer (clauses)
  (with-current-buffer (generate-new-buffer "defpackage")
    (insert "(defpackage foo\n")
    (insert clauses)
    (insert ")")
    (goto-char (point-min))
    (indent-region (point-min) (point-max))
    (current-buffer)))

(defun slime-defpackage-buffer-equalp (a b)
  (let ((a-bounds (with-current-buffer a
                    (cons (point-min) (point-max))))
        (b-bounds (with-current-buffer b
                    (cons (point-min) (point-max)))))
    (compare-buffer-substrings a (car a-bounds) (cdr a-bounds)
                               b (car b-bounds) (cdr b-bounds))))

(defmacro slime-check-defpackage-modification (before-clauses after-clauses modification msg)
  (declare (indent 2))
  (let ((before-buffer (gensym))
        (after-buffer (gensym)))
    `(let ((,before-buffer (slime-setup-defpackage-buffer ,before-clauses))
           (,after-buffer (slime-setup-defpackage-buffer ,after-clauses)))
       (unwind-protect
           (cl-letf (((symbol-function 'slime-goto-package-definition)
                      (lambda (&optional p) (goto-char (point-min)))))
             (with-current-buffer ,before-buffer
               ,modification
               (indent-region (point-min) (point-max)))
             (slime-check ,msg
               (slime-defpackage-buffer-equalp ,before-buffer
                                               ,after-buffer)))
         (kill-buffer ,after-buffer)
         (kill-buffer ,before-buffer)))))

(def-slime-test export-symbol
    (without symbol with)
    ""
    '(("(:export #:bar)"
       "baz"
       "(:export #:bar
         #:baz)")
      ("(:export \"BAR\")"
       "baz"
       "(:export \"BAR\"
         \"BAZ\")")
      (""
       "baz"
       "(:export #:baz)"))
  (slime-check-defpackage-modification without with
    (slime-export-symbol symbol)
    ("symbol %s did not export properly." symbol))
  (slime-check-defpackage-modification with without
    (slime-unexport-symbol symbol)
    ("symbol %s did not unexport properly." symbol)))

(def-slime-test import-symbol
    (without symbol package with)
    ""
    '(("(:import-from #:swank
              #:quit-lisp)"
       "from-string" "swank"
       "(:import-from #:swank
              #:quit-lisp
              #:from-string)")
      ("(:import-from swank
              quit-lisp)"
       "from-string" "swank"
       "(:import-from swank
              quit-lisp
              from-string)")
      (""
       "from-string" "swank"
       "(:import-from #:swank
              #:from-string)"))
  (slime-check-defpackage-modification without with
    (slime-import-symbol symbol package)
    ("importing %s from %s did not work properly." symbol package))
  (slime-check-defpackage-modification with without
    (slime-unimport-symbol symbol)
    ("unimporting %s did not work properly." symbol)))

(provide 'slime-package-fu-tests)
