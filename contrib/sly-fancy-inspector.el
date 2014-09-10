(eval-and-compile
  (require 'sly))

(define-sly-contrib sly-fancy-inspector
  "Fancy inspector for CLOS objects."
  (:authors "Marco Baringer <mb@bese.it> and others")
  (:license "GPL")
  (:sly-dependencies sly-parse)
  (:slynk-dependencies slynk-fancy-inspector))

(defun sly-inspect-definition ()
  "Inspect definition at point"
  (interactive)
  (sly-inspect (sly-definition-at-point)))

(defun sly-disassemble-definition ()
  "Disassemble definition at point"
  (interactive)
  (sly-eval-describe `(slynk:disassemble-form
                         ,(sly-definition-at-point t))))

(provide 'sly-fancy-inspector)
