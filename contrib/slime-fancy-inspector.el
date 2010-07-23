
(define-slime-contrib slime-fancy-inspector
  "Fancy inspector for CLOS objects."
  (:authors "Marco Baringer <mb@bese.it> and others")
  (:license "GPL")
  (:slime-dependencies slime-parse)
  (:swank-dependencies swank-fancy-inspector))

(defun slime-inspect-definition ()
  "Inspect definition at point"
  (interactive)
  (slime-inspect (slime-definition-at-point)))

(defun slime-disassemble-definition ()
  "Disassemble definition at point"
  (interactive)
  (slime-eval-describe `(swank:disassemble-form
                         ,(slime-definition-at-point t))))

(provide 'slime-fancy-inspector)
