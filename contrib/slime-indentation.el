
(define-slime-contrib slime-indentation
  "Patched version of cl-indent.el as a slime-contrib module"
  (:swank-dependencies swank-indentation))

(load "slime-cl-indent.el")

(setq common-lisp-current-package-function 'slime-current-package)

(provide 'slime-indentation)
