(require 'slime)

(define-slime-contrib clos-class-graph
  "CLOS class graphing utility"
  (:authors "Jim Newton <jim.newton@lrde.eptia.edu>")
  (:license "BSD")
)

(defun class-graph (class-name)
  (interactive "P")
  (let* ((class-name (or class-name
			 (slime-symbol-at-point)))
	 (tmp-file (make-temp-file (format "%s-" class-name) nil ".png")))
    (slime-eval `(class-graph:class-graph-for-slime ,class-name ,tmp-file))
    (find-file tmp-file)
    (image-mode-fit-frame)))
