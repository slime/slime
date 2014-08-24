(require 'sly-fuzzy)
(require 'sly-tests "lib/sly-tests")

(def-sly-test minimum-required-completions
    (prefix required-completions partial)
    "Find the completions of a symbol-name prefix."
    '(("cl:compile" ("cl:compile" "cl:compile-file" "cl:compile-file-pathname"
                     "cl:compiled-function" "cl:compiled-function-p" 
                     "cl:compiler-macro" "cl:compiler-macro-function")
       "cl:compile")
      ("cl:foobar" nil nil)
      ("swank::compile-file" ("swank::compile-file" 
                              "swank::compile-file-for-emacs"
                              "swank::compile-file-if-needed"
                              "swank::compile-file-output"
                              "swank::compile-file-pathname")
       "swank::compile-file")
      ("cl:m-v-l" ("cl:multiple-value-list" "cl:multiple-values-limit") "cl:multiple-value")
      ("common-lisp" ("common-lisp-user:" "common-lisp:") "common-lisp"))
  (let ((completions (sly-fuzzy-completions prefix)))
    (loop with head = (cl-subseq (car completions) 0
                                 (* 2.0 (length required-completions)))
          for required in required-completions
          unless (cl-find required head :key #'car :test #'string=)
          do (ert-fail (format "Expected to find \"%s\" in the group of %d first suggestions for completing \"%s\""
                               required
                               (length head)
                               prefix)))))

(provide 'sly-fuzzy-tests)
