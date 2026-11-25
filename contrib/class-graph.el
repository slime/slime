(require 'slime)
(require 'cl)

(define-slime-contrib clos-class-graph
  "CLOS class graphing utility"
  (:authors "Jim Newton <jim.newton@lrde.eptia.edu>")
  (:license "BSD")
)

(defvar class-graph-coordinate-data nil)
(defun class-graph (class-name)
  (interactive "P")
  (let* ((class-name (or class-name
			 (slime-symbol-at-point)))
	 (tmp-file (make-temp-file (format "%s-" class-name) nil ".png"))
	 (data (slime-eval `(class-graph:class-graph-for-slime ,class-name ,tmp-file))))

    (make-frame)
    (find-file tmp-file)
    (image-mode-fit-frame)
    (setq-local class-graph-coordinate-data data)
    (local-set-key [mouse-1] 'class-graph-from-click)))


(defun point-in-box-p (&rest args)
  ;; args = (list buffer-point buffer-bbox image-target-box image-bbox)
  (destructuring-bind ((target-point-x . target-point-y)
		       (buffer-left buffer-top buffer-right buffer-bottom)
		       (target-left target-top target-right target-bottom)
		       (image-left  image-top  image-right  image-bottom))
      args
    (labels ((normalize (min max value)
			(/ (- value min) (- max min))))
      (assert (< image-top image-bottom))
      (assert (< image-left image-right))
      (assert (< buffer-left buffer-right))
      (assert (< buffer-top buffer-bottom))
      (assert (< target-left target-right))
      (assert (< target-top target-bottom))
      (let ((normalized-target-left   (normalize image-left  image-right   target-left))
	    (normalized-target-top    (normalize image-top   image-bottom  target-top))
	    (normalized-target-right  (normalize image-left  image-right   target-right))
	    (normalized-target-bottom (normalize image-top   image-bottom  target-bottom))
	    (normalized-point-x       (normalize buffer-left buffer-right  target-point-x))
	    (normalized-point-y       (normalize buffer-top  buffer-bottom target-point-y)))
	(assert (< normalized-target-left
		   normalized-target-right) nil "1. expecting %d < %d" normalized-target-left normalized-target-right)
	(assert (< normalized-target-top
		   normalized-target-bottom) nil "2. expecting %d < %d" normalized-target-top normalized-target-bottom)
	(and (<= normalized-target-left
		 normalized-point-x
		 normalized-target-right)
	     (<= normalized-target-top
		 normalized-point-y
		 normalized-target-bottom))))))

(defun class-graph-from-click (event)
  (interactive "e")
  (let* ((position (event-start event))
	 (user-clicked (posn-x-y position)) ;; this is the x-y where the mouse was clicked-down
	 (user-x (car user-clicked))
	 (user-y (cdr user-clicked))
	 (object-xy (posn-object-x-y position))
	 (object-x (car object-xy))
	 (object-y (cdr object-xy))
	 (object-size (posn-object-width-height position))
	 (object-width (car object-size))
	 (object-height (cdr object-size))
	 ;;               left top right bottom
	 (buffer-bb (list object-x object-y
			  (+ object-x object-width) (+ object-y object-height))))
    ;;                                  class-graph-coordinate-data  is a buffer local variable
    (destructuring-bind (&rest _ &key ((:bb image-bb) nil) coords &allow-other-keys) class-graph-coordinate-data
      (dolist (coord coords)
	(destructuring-bind (&rest _ &key class ((:bb target-bb) nil) &allow-other-keys) coord
	  (when (point-in-box-p user-clicked
				buffer-bb
				target-bb
				image-bb)
	    (class-graph (format "%s" class))))))))
