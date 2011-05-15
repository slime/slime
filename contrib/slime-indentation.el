
(define-slime-contrib slime-indentation
  "Patched version of cl-indent.el as a slime-contrib module"
  (:swank-dependencies swank-indentation))

(load "slime-cl-indent.el")

(provide 'slime-indentation)
