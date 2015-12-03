(defpackage "CLASS-GRAPH"
  (:use :cl :c2mop)
  (:nicknames "GR")
  (:shadow "STANDARD-GENERIC-FUNCTION"
	   "DEFMETHOD"
	   "DEFGENERIC")
  (:export "CLASS-TO-DOT"
	   "CLASS-GRAPH-FOR-SLIME"))

(in-package :class-graph)

(defmacro while (test &rest body)
  `(loop :while ,test
	 :do (progn ,@body)))

(defun lconc (buf items)
  (cond
    ((null buf)
     (cons items (last items)))
    ((null items)
     buf)
    ((null (car buf))
     (setf (car buf) items)
     (setf (cdr buf) (last items))
     buf)
    (t
     (setf (cdr (cdr buf)) items)
     (setf (cdr buf) (last items))
     buf)))

(defun tconc (buf &rest items)
  (lconc buf items))

(defmacro print-vals (&rest args)
  (let* ((var (gensym))
	(content (loop :for arg :in args
			:for n = 1 :then (1+ n)
			:collect `(progn (format t "~2d/~d " ,n ,(length args))
					 (format t "~A --> ~A~%" ',arg (setf ,var ,arg))))))
    `(let ((,var nil))
       ,@content
       (finish-output)
       ,var)))

(cl:defgeneric class-to-dot (classes stream &rest args &key max-parents max-children open))

(cl:defmethod class-to-dot ((class-name symbol) dot-stream &rest args &key (max-parents 999) (max-children 999) (open nil))
  (declare (ignore max-parents max-children open))
  (apply #'class-to-dot (list (find-class class-name)) dot-stream args))

(cl:defmethod class-to-dot (classes (path pathname) &rest args &key (max-parents 999) (max-children 999) (open nil))
  (declare (ignore max-parents max-children))
  (cond ((string= "dot" (pathname-type path))
	 (with-open-file (stream path :direction :output :if-exists :rename)
	   (apply #'class-to-dot classes stream args)))
	((string= "png" (pathname-type path))
	 (let ((dotpath (merge-pathnames (make-pathname :type "dot")  path)))
	   (apply #'class-to-dot classes dotpath args)
	   ;; TODO need to take some action if "dot" is not found in the UNIX path
	   (sb-ext:run-program "dot" (list "-Tpng" (namestring dotpath) "-o" (namestring path)) :search t)
	   (when open
	     ;; TODO this only works on the MAC,
	     (sb-ext:run-program "open" (list "-n" (namestring path)) :search t))))
	(t
	 (error "invalid path ~A" path))))

(cl:defmethod class-to-dot (classes (dot-stream (eql nil)) &rest args &key (max-parents 999) (max-children 999) (open nil))
  (declare (ignore max-parents max-children open))
  (with-output-to-string (str)
    (apply #'class-to-dot classes str args)))

(cl:defmethod class-to-dot (classes (dot-stream (eql t)) &rest args &key (max-parents 999) (max-children 999) (open nil))
  (declare (ignore max-parents max-children open))
  (apply #'class-to-dot classes *STANDARD-OUTPUT* args))

(cl:defmethod class-to-dot ((classes list) (dot-stream stream) &rest args &key (max-parents 999) (max-children 999) (open nil))
  (declare (ignore open))
  (cond
    ((some #'symbolp classes)
     (apply #'class-to-dot (mapcar (lambda (class)
				     (cond ((symbolp class)
					    (find-class class)) ;; triggers an error if the symbol does not name a class
					   (t
					    class)))
				   classes)
	    dot-stream
	    args))
    (t
     (let* ((given-classes classes))

       (let (new-parent-classes
	     new-child-classes)
	 (dolist (class classes)
	   (setf new-parent-classes (union (set-difference (c2mop:compute-class-precedence-list class)
							   classes)
					   new-parent-classes)))
	 (setf new-parent-classes (last new-parent-classes max-parents))
	 (let ((buf (list nil nil)))
	   (lconc buf (copy-list given-classes))
	   (dolist (class (car buf))
	     (setf buf (lconc buf (copy-list (set-difference (set-difference (c2mop:class-direct-subclasses class)
									     classes)
							     (car buf))))))
	   (setf new-child-classes (last (car buf) max-children)))

	 (setf classes (union (union new-child-classes
				     new-parent-classes)
			      classes)))

       (format dot-stream "digraph G {~%")
       (format dot-stream "  rankdir=TB;~%")
       (format dot-stream "  graph [labeljust=l,nojustify=true];~%")

       (let ((hash (make-hash-table :test #'eq))
	     (index 0))
	 ;; assign labels to classes for the dot file
	 (dolist (class classes)
	   ;;(setf (gethash class hash) (format nil "C~D" (incf index)))
	   (setf (gethash class hash) (let ((*package* (find-package "KEYWORD")))
					(with-output-to-string (str)
					  (write (class-name class) :stream str :escape t))))
	   ;; output node definition in dot file
	   	     
	   (format dot-stream "  ~S [label=\"~A~%[~A]\" "
		   (gethash class hash)
		   (symbol-name (class-name class))
		   (package-name (symbol-package (class-name class))))
	   (format dot-stream "color=~A" (if (member class given-classes)
					     "green" "pink"))
	   (format dot-stream "]~%"))

	 ;; output arrows from child to parent
	 (dolist (super classes)
	   (dolist (sub classes)
	     (cond
	       ((eq super sub)
		nil)
	       ((member sub (c2mop:class-direct-subclasses super))
		(let ((label_super (gethash super hash))
		      (label_sub   (gethash sub hash)))
		  (format dot-stream "   ~S -> ~S~%" label_super label_sub)))))))
		      
       ;; close the diagraph g open brace
       (format dot-stream "}~%")))))

(defun class-graph-for-slime (class-name file-name)
  (cond
    ((stringp class-name)
     (class-graph-for-slime (swank::find-definitions-find-symbol-or-package class-name)
			    file-name))
    ((find-class class-name nil)
     (class-to-dot (list (find-class class-name)) (pathname file-name))
     file-name)
    (t
     (error "No such class ~A" class-name))))
  
