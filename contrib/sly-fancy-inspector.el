(eval-and-compile
  (require 'sly))

(define-sly-contrib sly-fancy-inspector
  "Fancy inspector for CLOS objects."
  (:authors "Marco Baringer <mb@bese.it> and others")
  (:license "GPL")
  (:sly-dependencies sly-parse)
  (:swank-dependencies swank-fancy-inspector)
  (:on-load
   (add-hook 'sly-edit-definition-hooks 'sly-edit-inspector-part))
  (:on-unload
   (remove-hook 'sly-edit-definition-hooks 'sly-edit-inspector-part)))

(defun sly-inspect-definition ()
  "Inspect definition at point"
  (interactive)
  (sly-inspect (sly-definition-at-point)))

(defun sly-disassemble-definition ()
  "Disassemble definition at point"
  (interactive)
  (sly-eval-describe `(swank:disassemble-form
                         ,(sly-definition-at-point t))))

(defun sly-edit-inspector-part (name &optional where)
  (and (eq major-mode 'sly-inspector-mode)
       (cl-destructuring-bind (&optional property value)
           (sly-inspector-property-at-point)
         (when (eq property 'sly-part-number)
           (let ((location (sly-eval `(swank:find-definition-for-thing
                                         (swank:inspector-nth-part ,value))))
                 (name (format "Inspector part %s" value)))
             (when (and (consp location)
                        (not (eq (car location) :error)))
               (sly-edit-definition-cont
                (list (make-sly-xref :dspec `(,name)
                                       :location location))
                name
                where)))))))

(provide 'sly-fancy-inspector)
