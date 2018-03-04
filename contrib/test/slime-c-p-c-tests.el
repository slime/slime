(require 'slime-c-p-c)
(require 'slime-tests)

(def-slime-test complete-symbol*
    (prefix expected-completions)
    "Find the completions of a symbol-name prefix."
    '(("cl:compile" (("cl:compile" "cl:compile-file" "cl:compile-file-pathname"
                      "cl:compiled-function" "cl:compiled-function-p" 
                      "cl:compiler-macro" "cl:compiler-macro-function")
                     "cl:compile"))
      ("cl:foobar" nil)
      ("swank::compile-file" (("swank::compile-file" 
                               "swank::compile-file-for-emacs"
                               "swank::compile-file-if-needed"
                               "swank::compile-file-output"
                               "swank::compile-file-pathname")
                              "swank::compile-file"))
      ("cl:m-v-l" (("cl:multiple-value-list" "cl:multiple-values-limit") "cl:multiple-value"))
      ("common-lisp" (("common-lisp-user:" "common-lisp:") "common-lisp")))
  (let ((completions (slime-completions prefix)))
    (slime-test-expect "Completion set" expected-completions completions)))

(def-slime-test complete-form
    (buffer-sexpr wished-completion &optional skip-trailing-test-p)
    ""
    '(("(defmethod arglist-dispatch *HERE*"
       "(defmethod arglist-dispatch (operator arguments) body...)")
      ("(with-struct *HERE*"
       "(with-struct (conc-name names...) obj body...)")
      ("(with-struct *HERE*"
       "(with-struct (conc-name names...) obj body...)")
      ("(with-struct (*HERE*"
       "(with-struct (conc-name names...)" t)
      ("(with-struct (foo. bar baz *HERE*"
       "(with-struct (foo. bar baz names...)" t))
  (slime-check-top-level)
  (with-temp-buffer
    (lisp-mode)
    (setq slime-buffer-package "SWANK")
    (insert buffer-sexpr)
    (search-backward "*HERE*")
    (delete-region (match-beginning 0) (match-end 0))
    (slime-complete-form)
    (slime-check-completed-form buffer-sexpr wished-completion)

    ;; Now the same but with trailing `)' for paredit users...
    (unless skip-trailing-test-p
      (erase-buffer)
      (insert buffer-sexpr)
      (search-backward "*HERE*")
      (delete-region (match-beginning 0) (match-end 0))
      (insert ")") (backward-char)
      (slime-complete-form)
      (slime-check-completed-form (concat buffer-sexpr ")") wished-completion))
    ))

(defun slime-check-completed-form (buffer-sexpr wished-completion)
  (slime-test-expect (format "Completed form for `%s' is as expected"
                              buffer-sexpr)
                     wished-completion
                     (buffer-string)
                     'equal))

(provide 'slime-c-p-c-tests)
