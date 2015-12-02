(require 'slime)

(define-slime-contrib clos-class-graph
  "CLOS class graphing utility"
  (:authors "Jim Newton <jim.newton@lrde.eptia.edu>")
  (:license "BSD")
)

(defun class-graph (class-name)
  (interactive "P")
  (let ((class-name (or class-name
			(slime-symbol-at-point)))
	(file-name "/tmp/graph.png"))
    (when (slime-interactive-eval (format "(class-graph:class-graph-for-slime '%s %S)" 
					  class-name file-name))
      (find-file file-name)
      (image-mode-fit-frame))
    ;; (when (slime-eval `(class-graph:class-graph-for-slime ',class-name ,file-name))
    ;;   (find-file file-name)
    ;;   (image-mode-fit-frame))
    ))

