;;;; Source-paths

;;; CMUCL/SBCL use a data structure called "source-path" to locate
;;; subforms.  The compiler assigns a source-path to each form in a
;;; compilation unit.  Compiler notes usually contain the source-path
;;; of the error location.
;;;
;;; Compiled code objects don't contain source paths, only the
;;; "toplevel-form-number" and the (sub-) "form-number".  To get from
;;; the form-number to the source-path we need the entire toplevel-form
;;; (i.e. we have to read the source code).  CMUCL has already some
;;; utilities to do this translation, but we use some extended
;;; versions, because we need more exact position info.  Apparently
;;; Hemlock is happy with the position of the toplevel-form; we also
;;; need the position of subforms.
;;;
;;; We use a special readtable to get the positions of the subforms.
;;; The readtable stores the start and end position for each subform in
;;; hashtable for later retrieval.

;;; Taken from swank-cmucl.lisp, by Helmut Eller

(in-package :swank-backend)

(defun make-source-recorder (fn source-map)
  "Return a macro character function that does the same as FN, but
additionally stores the result together with the stream positions
before and after of calling FN in the hashtable SOURCE-MAP."
  (declare (type function fn))
  (lambda (stream char)
    (let ((start (file-position stream))
	  (values (multiple-value-list (funcall fn stream char)))
	  (end (file-position stream)))
      ;;(format t "[~D ~{~A~^, ~} ~D ~D]~%" start values end (char-code char))
      (unless (null values) 
	(push (cons start end) (gethash (car values) source-map)))
      (values-list values))))

#+sbcl
;; not sure why this should be the case, but SBCL 0.8.6 returns
;; #<FUNCTION "top level local call SB!IMPL::UNDEFINED-MACRO-CHAR">
;; for (get-macro-character) on characters that aren't macros.
;; As there's no way to detect the syntax of a character (only
;; to set it from another character) we have to compare against
;; this undefined-macro function to avoid turning everything into
;; a macro  -- Dan Barlow
;;
;; Just copy CMUCL's implementation, to get identical behavior.  The
;; SBCL implementation uses GET-RAW-CMT-ENTRY; we use
;; GET-COERCED-CMT-ENTRY, which seems to be what SET-MACRO-CHARACTER
;; expects. -- Helmut Eller
(defun cmucl-style-get-macro-character (char table)
  (let ((rt (or table sb-impl::*standard-readtable*)))
    (cond ((sb-impl::constituentp char)
	   (values (sb-impl::get-coerced-cmt-entry char rt) t))
	  ((sb-impl::terminating-macrop char)
	   (values (sb-impl::get-coerced-cmt-entry char rt) nil))
	  (t nil))))

#+cmu
(defun cmucl-style-get-macro-character (char table)
  (get-macro-character char table))

(defun make-source-recording-readtable (readtable source-map) 
  "Return a source position recording copy of READTABLE.
The source locations are stored in SOURCE-MAP."
  (let* ((tab (copy-readtable readtable))
	 (*readtable* tab))
    (dotimes (code char-code-limit)
      (let ((char (code-char code)))
	(multiple-value-bind (fn term) 
	    (cmucl-style-get-macro-character char tab)
	  (when fn
	    (set-macro-character char (make-source-recorder fn source-map) 
				 term tab)))))
    tab))

(defun make-source-map ()
  (make-hash-table :test #'eq))

(defvar *source-map* (make-source-map)
  "The hashtable table used for source position recording.")

(defvar *recording-readtable-cache* '()
  "An alist of (READTABLE . RECORDING-READTABLE) pairs.")

(defun lookup-recording-readtable (readtable)
  "Find a cached or create a new recording readtable for READTABLE."
  (or (cdr (assoc readtable *recording-readtable-cache*))
      (let ((table (make-source-recording-readtable readtable *source-map*)))
	(push (cons readtable table) *recording-readtable-cache*)
	table)))
			
(defun read-and-record-source-map (stream)
  "Read the next object from STREAM.
Return the object together with a hashtable that maps
subexpressions of the object to stream positions."
  (let ((*readtable* (lookup-recording-readtable *readtable*)))
    (clrhash *source-map*)
    (values (read stream) *source-map*)))
  
(defun source-path-stream-position (path stream)
  "Search the source-path PATH in STREAM and return its position."
  (destructuring-bind (tlf-number . path) path
    (let ((*read-suppress* t))
      (dotimes (i tlf-number) (read stream))
      (multiple-value-bind (form source-map)
	  (read-and-record-source-map stream)
	(source-path-source-position (cons 0 path) form source-map)))))

(defun source-path-string-position (path string)
  (with-input-from-string (s string)
    (source-path-stream-position path s)))

(defun source-path-file-position (path filename)
  (with-open-file (file filename)
    (source-path-stream-position path file)))

(defun source-path-source-position (path form source-map)
  "Return the start position of PATH from FORM and SOURCE-MAP.  All
subforms along the path are considered and the start and end position
of deepest (i.e. smallest) possible form is returned."
  ;; compute all subforms along path
  (let ((forms (loop for n in path
		     for f = form then (nth n f)
		     collect f)))
    ;; select the first subform present in source-map
    (loop for form in (reverse forms)
	  for positions = (gethash form source-map)
	  until (and positions (null (cdr positions)))
	  finally (destructuring-bind ((start . end)) positions
		    (return (values (1- start) end))))))

