(in-package :swank)

;; A mechanism for printing to the slime repl so that the printed
;; result remembers what object it is associated with. Depends on the
;; ilisp bridge code being installed and ready to intercept messages
;; in the printed stream. We encode the information with a message
;; saying that we are starting to print an object corresponding to a
;; given id and another when we are done. The process filter notices these
;; and adds the necessary text properties to the output.

(defvar *can-print-presentation* nil 
  "set this to t in contexts where it is ok to print presentations at all")
 
(defvar *can-present-readable-objects* nil
  "set this to t in context where it is ok to automatically print presentations 
for some subset of readable objects, such as pathnames. Generally, this is unsafe
(since you might not be printing to the listener and expecting to read
them later) but can be appropriate in specific circumstances, such as
when you know your output is going to the listener, or where you know
you wouldn't be later reading the objects printed"
  )

(defvar *object-to-presentation-id* (make-hash-table :test 'eq #+openmcl :weak #+openmcl :key)
  "Store the mapping of objects to numeric identifiers")

(defvar *presentation-id-to-object* (make-hash-table :test 'eq #+openmcl :weak #+openmcl :value)
  "Store the mapping of numeric identifiers to objects")

(defvar *presentation-counter* 0 "identifier counter")

(defun clear-presentation-tables ()
  (clrhash *object-to-presentation-id*)
  (clrhash *presentation-id-to-object*)
  (setq *presentation-counter* 0)
  )

(defun lookup-presented-object (id)
  "Retrieve the object corresponding to id. :not-present returned if it isn't there"
  (gethash id *presentation-id-to-object* :not-present))

(defun save-presented-object (object)
  "If the object doesn't already have an id, save it and allocate
one. Otherwise return the old one"
  (or (gethash object *presentation-id-to-object*)
      (let ((newid (incf *presentation-counter*)))
	(setf (gethash newid *presentation-id-to-object*) object)
	(setf (gethash object *object-to-presentation-id*) newid)
	newid)))

(defmacro presenting-object (object stream &body body)
  "What you use in your code. Wrap this around some printing and that text will
be sensitive and remember what object it is in the repl"
  `(presenting-object-1 ,object ,stream #'(lambda () ,@body)))

(defmacro presenting-object-if (predicate object stream &body body)
  "What you use in your code. Wrap this around some printing and that text will
be sensitive and remember what object it is in the repl if predicate is true"
  (let ((continue (gensym)))
  `(let ((,continue #'(lambda () ,@body)))
    (if ,predicate
	(presenting-object-1 ,object ,stream ,continue)
	(funcall ,continue)))))

(defun can-present-readable-objects (&optional stream)
  (declare (ignore stream))
  *can-present-readable-objects*)

(defun presenting-object-1 (object stream continue)
  "Uses the bridge mechanism with two messages >id and <id. The first one
says that I am starting to print an object with this id. The second says I am finished"
  (if (and *record-repl-results* *can-print-presentation*)
      (let ((pid (swank::save-presented-object object)))
	(write-string "<" stream)
	(prin1 pid stream)
	(write-string "" stream)
	(funcall continue)
	(write-string ">" stream)
	(prin1 pid stream)
	(write-string "" stream))
      (funcall continue)))

;; enable presentations inside listener eval only
(defslimefun listener-eval (string)
  (clear-user-input)
  (with-buffer-syntax ()
    (let ((*slime-repl-suppress-output* :unset)
	  (*slime-repl-advance-history* :unset))
      (multiple-value-bind (values last-form) 
	  (let ((*can-print-presentation* t)) 
	    (eval-region string t))
	(unless (or (and (eq values nil) (eq last-form nil))
		    (eq *slime-repl-advance-history* nil))
	  (setq *** **  ** *  * (car values)
		/// //  // /  / values)
          (when *record-repl-results*
            (add-repl-result *current-id* *)))
	(setq +++ ++  ++ +  + last-form)
	(if (eq *slime-repl-suppress-output* t)
	    ""
	    (cond ((null values) "; No value")
		  (t
		   (format nil "~{~S~^~%~}" values))))))))

;; hook up previous implementation. Use negative ids for repl results so as to not conflict with
;; the ones for other printout
(defun add-repl-result (id val)
  (setf (gethash (- id) *presentation-id-to-object*) val)
  (save-presented-object val)
  t)

;; hook up previous implementation
(defslimefun clear-repl-results ()
  "Forget the results of all previous REPL evaluations."
  (clear-presentation-tables)
  t)

;; hook up previous implementation
(defslimefun get-repl-result (id)
  "Get the result of the previous REPL evaluation with ID."
  (let ((previous-output (lookup-presented-object id)))
    (when (eq previous-output :not-present)
      (if swank::*record-repl-results*
          (error "Attempt to access no longer existing result (number ~D)." id)
          (error "Attempt to access unrecorded result (number ~D). ~&See ~S."
                 id '*record-repl-results*)))
    previous-output))

;; Disable during backtrack, for now. For one thing, the filter isn't set up for the sldb
;; buffer. Also there is higher likelyhood of lossage due to dynamic extent objects.

(defslimefun backtrace (start end)
  "Return a list ((I FRAME) ...) of frames from START to END.
I is an integer describing and FRAME a string."
  (let ((*can-print-presentation* nil))
    (loop for frame in (compute-backtrace start end)
	  for i from start
	  collect (list i (frame-for-emacs i frame)))))

;; ditto inspector - isn't needed
(defslimefun init-inspector (string)
  (let ((*can-print-presentation* nil))
    (with-buffer-syntax ()
      (reset-inspector)
      (inspect-object (eval (read-from-string string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; menu protocol
;;
;; To define a menu for a type of object, define a method
;; menu-choices-for-presentation on that object type.  This function
;; should return a list of two element lists where the first element is
;; The name of the menu action and the second is a function that will be
;; called if the menu is chosen. The function will be called with 3
;; arguments:
;;
;; choice: The string naming the action from above
;;
;; object: The object 
;;
;; id: The presentation id of the object
;;
;; You might want append (when (next-method-p) (call-next-method)) to
;; pick up the Menu actions of superclasses.
;;
;; The function should return a form which will be evaluated on the emacs side.

(defvar *presentation-active-menu* nil)

(defun menu-choices-for-presentation-id (id)
  (let ((ob (lookup-presented-object id)))
    (if (eq ob :not-present)
	'not-present
	(let ((menu-and-actions (menu-choices-for-presentation ob)))
	  (setq *presentation-active-menu* (cons id menu-and-actions))
	  (mapcar 'car menu-and-actions)))))

(defun swank-ioify (thing)
  (cond ((keywordp thing) thing)
	((and (symbolp thing)(not (find #\: (symbol-name thing))))
	 (intern (symbol-name thing) 'swank-io-package))
	((consp thing) (cons (swank-ioify (car thing)) (swank-ioify (cdr thing))))
	(t thing)))

(defun execute-menu-choice-for-presentation-id (id count item)
  (let ((ob (lookup-presented-object id)))
    (assert (eql id (car *presentation-active-menu*)) () 
	    "Bug: Execute menu call for id ~a  but menu has id ~a"
	    id (car *presentation-active-menu*))
    (let ((action (second (nth (1- count) (cdr *presentation-active-menu*)))))
      (swank-ioify (funcall action item ob id)))))

;; Default method
(defmethod menu-choices-for-presentation (ob)
  (declare (ignore ob))
  (list 
   (list "Inspect" (lambda(choice object id) (declare (ignore choice object)) 
			  `(slime-inspect-presented-object ,id)))
   (list "Describe" (lambda(choice object id) (declare (ignore id choice)) 
			   (describe object) 
			   nil))
   (list "Copy to input" (lambda(choice object id) (declare (ignore choice object id)) 
			  `(slime-copy-presentation-at-point event)))))

;; Pathname
(defmethod menu-choices-for-presentation ((ob pathname))
  (let* ((file-exists (ignore-errors (probe-file ob)))
	 (source-file (and (not (equal (pathname-type ob) "lisp"))
			   (let ((source (merge-pathnames ".lisp" ob)))
			     (and (ignore-errors (probe-file source))
				  source)))))
    (remove nil 
	    (list*
	     (and file-exists
		  (list "Edit this file" 
			(lambda(choice object id) 
			  (declare (ignore choice id)) (ed object) nil)))
	     (and file-exists
		  (list "Dired containing directory"
			(lambda (choice object id)
			  (declare (ignore choice id))
			  `(dired ,(namestring (truename (merge-pathnames (make-pathname :name "" :type "") object)))))))
	     (and source-file 
		  (list "Edit lisp source file" 
			(lambda(choice object id) 
			  (declare (ignore choice id object)) (ed source-file) nil)))
	     (and (next-method-p) (call-next-method))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Example: Tell openmcl and cmucl to always present unreadable objects. try (describe 'class) 
#+openmcl
(in-package :ccl)
#+openmcl
(let ((*warn-if-redefine-kernel* nil)
      (*warn-if-redefine* nil))
  (defun %print-unreadable-object (object stream type id thunk)
    (cond ((null stream) (setq stream *standard-output*))
	  ((eq stream t) (setq stream *terminal-io*)))
    (swank::presenting-object object stream
      (write-unreadable-start object stream)
      (when type
	(princ (type-of object) stream)
	(stream-write-char stream #\space))
      (when thunk 
	(funcall thunk))
      (if id
	  (%write-address object stream #\>)
	  (pp-end-block stream ">"))
      nil))
  ;(defmethod print-object :around ((pathname pathname) stream)
  ;  (swank::presenting-object-if (swank::can-present-readable-objects stream) pathname stream (call-next-method)))
)

#+openmcl
(ccl::def-load-pointers clear-presentations ()
  (swank::clear-presentation-tables))

#+cmu
(in-package :lisp)

#+cmu
(ext:without-package-locks
 (defun %print-unreadable-object (object stream type identity body)
  (when *print-readably*
    (error 'print-not-readable :object object))
  (flet ((print-description ()
	   (when type
	     (write (type-of object) :stream stream :circle nil
		    :level nil :length nil)
	     (when (or body identity)
	       (write-char #\space stream)
	       (pprint-newline :fill stream)))
	     (when body
	       (funcall body))
	     (when identity
	       (when body
		 (write-char #\space stream)
		 (pprint-newline :fill stream))
	       (write-char #\{ stream)
	       (write (get-lisp-obj-address object) :stream stream
		      :radix nil :base 16)
	       (write-char #\} stream))))
   (swank::presenting-object object stream
    (cond ((and (pp:pretty-stream-p stream) *print-pretty*)
	   (pprint-logical-block (stream nil :prefix "#<" :suffix ">")
	     (print-description)))
	  (t
	   (write-string "#<" stream)
	   (print-description)
	   (write-char #\> stream))))
  nil)))