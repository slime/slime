(in-package :swank)

;; A mechanism for printing to the slime repl so that the printed
;; result remembers what object it is associated with. Depends on the
;; ilisp bridge code being installed and ready to intercept messages
;; in the printed stream. We encode the information with a message
;; saying that we are starting to print an object corresponding to a
;; given id and another when we are done. The process filter notices these
;; and adds the necessary text properties to the output.

;; We only do this if we know we are printing to a slime stream,
;; checked with the method slime-stream-p. Initially this checks for
;; the knows slime streams looking at *connections*. In cmucl and
;; openmcl it also checks if it is a pretty-printing stream which
;; ultimately prints to a slime stream.

;; Control
(defvar *enable-presenting-readable-objects* t
  "set this to enable automatically printing presentations for some
subset of readable objects, such as pathnames."  )

;; Saving presentations
(defvar *object-to-presentation-id* (make-hash-table :test 'eq #+openmcl :weak #+openmcl :key)
  "Store the mapping of objects to numeric identifiers")

(defvar *presentation-id-to-object* (make-hash-table :test 'eq #+openmcl :weak #+openmcl :value)
  "Store the mapping of numeric identifiers to objects")

(defvar *presentation-counter* 0 "identifier counter")

(defun clear-presentation-tables ()
  (clrhash *object-to-presentation-id*)
  (clrhash *presentation-id-to-object*)
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

;; doing it

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

(let ((last-stream nil)
      (last-answer nil))
  (defmethod slime-stream-p (stream)
    "Check if stream is one of the slime streams, since if it isn't we
don't want to present anything"
    (if (eq last-stream stream)
	last-answer
	(progn
	  (setq last-stream stream)
	  (if (eq stream t) 
	      (setq stream *standard-output*))
	  (setq last-answer 
		(or #+openmcl 
		    (and (typep stream 'ccl::xp-stream) 
					;(slime-stream-p (ccl::xp-base-stream (slot-value stream 'ccl::xp-structure)))
			 (slime-stream-p (ccl::%svref (slot-value stream 'ccl::xp-structure) 1)))
		    #+cmu
		    (and (typep stream 'pretty-print::pretty-stream)
			 (slime-stream-p (pretty-print::pretty-stream-target  stream)))
		    #+allegro
		    (and (typep stream 'excl:xp-simple-stream)
			 (slime-stream-p (excl::stream-output-handle stream)))
		    (loop for connection in *connections*
			  thereis (or (eq stream (connection.dedicated-output connection))
				      (eq stream (connection.socket-io connection))
				      (eq stream (connection.user-output connection))
				      (eq stream (connection.user-io connection))))))))))

(defun can-present-readable-objects (&optional stream)
  (declare (ignore stream))
  *enable-presenting-readable-objects*)

;; If we are printing to an XP (pretty printing) stream, printing the
;; escape sequences directly would mess up the layout because column
;; counting is disturbed.  Use "annotations" instead.
#+allegro
(defun write-annotation (stream function arg)
  (if (typep stream 'excl:xp-simple-stream)
      (excl::schedule-annotation stream function arg)
      (funcall function arg stream nil)))
#-allegro
(defun write-annotation (stream function arg)
  (funcall function arg stream nil))

(defstruct presentation-record 
  (id)
  (printed-p))

(defun presentation-start (record stream truncatep) 
  (unless truncatep
    ;; Don't start new presentations when nothing is going to be
    ;; printed due to *print-lines*.
    (let ((pid (presentation-record-id record)))
      (cond (*use-dedicated-output-stream* 
	     (write-string "<" stream)
	     (prin1 pid stream)
	     (write-string "" stream))
	    (t
	     (force-output stream)
	     (send-to-emacs `(:presentation-start ,pid)))))
    (setf (presentation-record-printed-p record) t)))
	   
(defun presentation-end (record stream truncatep)
  (declare (ignore truncatep))
  ;; Always end old presentations that were started.
  (when (presentation-record-printed-p record)
    (let ((pid (presentation-record-id record)))
      (cond (*use-dedicated-output-stream* 
	     (write-string ">" stream)
	     (prin1 pid stream)
	     (write-string "" stream))
	    (t
	     (force-output stream)
	     (send-to-emacs `(:presentation-end ,pid)))))))

(defun presenting-object-1 (object stream continue)
  "Uses the bridge mechanism with two messages >id and <id. The first one
says that I am starting to print an object with this id. The second says I am finished"
  (if (and *record-repl-results* *can-print-presentation*
	   (slime-stream-p stream))
      (let* ((pid (swank::save-presented-object object))
	     (record (make-presentation-record :id pid :printed-p nil)))
	(write-annotation stream #'presentation-start record)
	(multiple-value-prog1
	    (funcall continue)
	  (write-annotation stream #'presentation-end record)))
      (funcall continue)))

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
      (swank-ioify 
       (let ((*can-print-presentation* t))
	 (funcall action item ob id))))))

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
	 (source-file (and (not (member (pathname-type ob) '("lisp" "cl") :test 'equal))
			   (let ((source (merge-pathnames ".lisp" ob)))
			     (and (ignore-errors (probe-file source))
				  source))))
	 (fasl-file (and file-exists 
			 (equal (ignore-errors
				  (namestring
				   (truename
				    (compile-file-pathname
				     (merge-pathnames ".lisp" ob)))))
				(namestring (truename ob))))))
    (remove nil 
	    (list*
	     (and (and file-exists (not fasl-file))
		  (list "Edit this file" 
			(lambda(choice object id) 
			  (declare (ignore choice id))
			  `(find-file ,(namestring (truename object))))))
	     (and file-exists
		  (list "Dired containing directory"
			(lambda (choice object id)
			  (declare (ignore choice id))
			  `(dired ,(namestring 
				    (truename
				     (merge-pathnames
				      (make-pathname :name "" :type "") object)))))))
	     (and fasl-file
		  (list "Load this fasl file"
			(lambda (choice object id)
			  (declare (ignore choice id object)) 
			  (load ob)
			  nil)))
	     (and fasl-file
		  (list "Delete this fasl file"
			(lambda (choice object id)
			  (declare (ignore choice id object)) 
			  (let ((nt (namestring (truename ob))))
			    `(when (y-or-n-p ,(format nil "Delete ~a" nt))
			      (delete-file ,(namestring (truename ob))))
			    ))))
	     (and source-file 
		  (list "Edit lisp source file" 
			(lambda(choice object id) 
			  (declare (ignore choice id object)) 
			  `(find-file ,(namestring (truename source-file))))))
	     (and source-file 
		  (list "Load lisp source file" 
			(lambda(choice object id) 
			  (declare (ignore choice id object)) 
			  (load source-file)
			  nil)))
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
  (defmethod print-object :around ((pathname pathname) stream)
    (swank::presenting-object-if
	(swank::can-present-readable-objects stream)
	pathname stream (call-next-method))))

#+openmcl
(ccl::def-load-pointers clear-presentations ()
  (swank::clear-presentation-tables))

(in-package :swank)

#+cmu
(progn
  (fwrappers:define-fwrapper presenting-unreadable-wrapper (object stream type identity body)
    (presenting-object object stream
      (fwrappers:call-next-function)))

  (fwrappers:define-fwrapper presenting-pathname-wrapper (pathname stream depth)
    (presenting-object-if (can-present-readable-objects stream) pathname stream
      (fwrappers:call-next-function)))

  (fwrappers::fwrap 'lisp::%print-pathname  #'presenting-pathname-wrapper)
  (fwrappers::fwrap 'lisp::%print-unreadable-object  #'presenting-unreadable-wrapper)
  )

#+sbcl
(progn 
  (defvar *saved-%print-unreadable-object*
    (fdefinition 'sb-impl::%print-unreadable-object))
  (sb-ext:without-package-locks 
    (setf (fdefinition 'sb-impl::%print-unreadable-object)
	  (lambda (object stream type identity body)
	    (presenting-object object stream
	      (funcall *saved-%print-unreadable-object* 
		       object stream type identity body))))
    (defmethod print-object :around ((object pathname) stream)
      (presenting-object object stream
	(call-next-method)))))

#+allegro
(progn
  (excl:def-fwrapper presenting-unreadable-wrapper (object stream type identity continuation) 
    (swank::presenting-object object stream (excl:call-next-fwrapper)))
  (excl:def-fwrapper presenting-pathname-wrapper (pathname stream depth)
    (presenting-object-if (can-present-readable-objects stream) pathname stream
      (excl:call-next-fwrapper)))
  (excl:fwrap 'excl::print-unreadable-object-1 
	      'print-unreadable-present 'presenting-unreadable-wrapper)
  (excl:fwrap 'excl::pathname-printer 
	      'print-pathname-present 'presenting-pathname-wrapper))
