;; -*- lexical-binding: t; -*-

(require 'slime-package-fu)
(require 'slime-tests)
(require 'subr-x)
(require 'rx)

(defmacro slime-define-package-fu-test (name running given gives)
  (declare (indent 1))
  `(define-slime-ert-test ,name ()
                          (with-temp-buffer
                            (lisp-mode)
                            (setq indent-tabs-mode nil)
                            (set-visited-file-name "my-package.lisp")
                            (insert ,given)
                            ,running
                            (should (equal ,gives (buffer-string))))))

(defmacro slime-package-fu-define-tests ()
  (with-temp-buffer
    (lisp-mode)
    (insert-file-contents (concat slime-path
                                  "/contrib/test/slime-package-fu-tests.txt"))
    (goto-char (point-min))
    (let (tests
          (test-sep (rx ";;; Test:" (* (any space)) (group (* not-newline))))
          (code-sep (rx line-start ";; gives" line-end))
          (test-code-regex (rx "do:" (* (any space)) (group (* not-newline)))))
      (while (re-search-forward test-sep nil t)
        (let* ((test-name (intern (match-string-no-properties 1)))
               (test-expr (progn (re-search-forward test-code-regex)
                                 (read (match-string-no-properties 1))))
               (start-of-input (match-end 0))
               (end-of-input (progn (re-search-forward code-sep)
                                    (match-beginning 0)))
               (start-of-expected (match-end 0))
               (end-of-test-spec (or (and (re-search-forward test-sep nil t)
                                          (match-beginning 0))
                                     (point-max))))
          (goto-char end-of-test-spec)
          (push
           `(slime-define-package-fu-test
             ,test-name
             ,test-expr
             ,(string-trim (buffer-substring start-of-input end-of-input))
             ,(string-trim (buffer-substring start-of-expected end-of-test-spec)))
           tests)))
      `(progn ,@tests))))

(slime-package-fu-define-tests)

(provide 'slime-package-fu-tests)
;;; slime-package-fu-tests.el ends here
