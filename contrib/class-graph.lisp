(ql:quickload :yacc)
(ql:quickload :cl-ppcre)
(ql:quickload :closer-mop)

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
  "Debugging macro for printing exressions and their evaluated values."
  (let* ((var (gensym))
	(content (loop :for arg :in args
			:for n = 1 :then (1+ n)
			:collect `(progn (format t "~2d/~d " ,n ,(length args))
					 (format t "~A --> ~A~%" ',arg (setf ,var ,arg))))))
    `(let ((,var nil))
       ,@content
       (finish-output)
       ,var)))

(cl:defgeneric class-to-dot (classes stream &rest args &key max-parents max-children open)
  (:documentation "Function for creating a class graph in either the form of a .dot file or .png"))

(cl:defmethod class-to-dot ((class-name symbol) dot-stream &rest args &key (max-parents 999) (max-children 999) (open nil))
  "If called with symbol, call CLASS-TO-DOT recursively with the class it designates."
  (declare (ignore max-parents max-children open))
  (apply #'class-to-dot (list (find-class class-name)) dot-stream args))

(cl:defmethod class-to-dot (classes (path pathname) &rest args &key (max-parents 999) (max-children 999) (open nil))
  "Generate a graphical image (using graphviz) to represent the superclasses (up to T, shown) and the 
subclasses (down to NIL, not shown).
If the given pathname has extension 'dot' then populate the .dot file with the dot 
language instructions to draw the CLOS class graph.
If the given path name has the extension 'png' then generate the graphical image
corresponding to the CLOS class (or classes).
RETURNS data structure describing the image, its bounding box, and the bbox of all the
   classes which are graphed in the image."
  (declare (ignore max-parents max-children)
	   (notinline string=))
  (cond ((string= "dot" (pathname-type path))
	 (with-open-file (stream path :direction :output :if-exists :rename)
	   ;; populate the .dot file with graphviz.dot code
	   (apply #'class-to-dot classes stream args)))
	((string= "png" (pathname-type path))
	 (let ((vdotpath (merge-pathnames (make-pathname :type "v-dot") path)) ;verbose dotfile
	       (dotpath (merge-pathnames (make-pathname :type "dot") path)))
	   ;; populate the .dot file with graphviz.dot code
	   (apply #'class-to-dot classes dotpath args)
	   ;; TODO need to take some action if "dot" is not found in the UNIX path
	   (sb-ext:run-program "dot" (list "-Tpng" (namestring dotpath) "-o" (namestring path)) :search t)
	   (sb-ext:run-program "dot" (list "-Tdot" (namestring dotpath) "-o" (namestring vdotpath)) :search t)
	   (when open
	     ;; TODO this only works on the MAC,
	     (sb-ext:run-program "open" (list "-n" (namestring path)) :search t))
	   (extract-choords (read-dot vdotpath))))
	(t
	 (error "invalid path ~A" path))))

(cl:defmethod class-to-dot (classes (dot-stream (eql nil)) &rest args &key (max-parents 999) (max-children 999) (open nil))
  "If NIL is given as DOT-STREAM, generate a program in the dot language and return
it as a string.  The program when executed will draw a graph of the CLOS
classes designated."
  (declare (ignore max-parents max-children open))
  (with-output-to-string (str)
    (apply #'class-to-dot classes str args)))

(cl:defmethod class-to-dot (classes (dot-stream (eql t)) &rest args &key (max-parents 999) (max-children 999) (open nil))
  "If T is given as DOT-STREAM, generate a program in the dot language and print
it to *STANDARD-OUTPUT*.  The program when executed will draw a graph of the CLOS
classes designated."
  (declare (ignore max-parents max-children open))
  (apply #'class-to-dot classes *STANDARD-OUTPUT* args))

(cl:defmethod class-to-dot ((classes list) (dot-stream stream) &rest args &key (max-parents 999) (max-children 999) (open nil))
  "Generate a program in the dot language which when executed will draw a graph of the CLOS
class designated by CLASSES.  CLASSES is a list of class designators which are symbols or classes.
The dot program is printed to the given stream DOT-STREAM.
The resulting graph starts at the designated classes (shown in green).
The superclasses extend upward to T (in pink).
The subclasses extend downward (in pink)."
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
	     ;;(index 0)
	     )
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
		      
       ;; close the digraph g open brace
       (format dot-stream "}~%")))))

(cl:defgeneric read-dot (dot-file))

(cl:defmethod read-dot ((pathname pathname))
  (with-open-file (stream pathname :direction :input :if-does-not-exist :error)
    (read-dot stream)))

(defmacro consume (vars &rest body)
  (let* ((tmpvars (loop :for v :in vars :collect (gensym (symbol-name v))))
	 (sets (mapcar (lambda (v tmp)  `(setf ,v ,tmp)) vars tmpvars)))
    `(multiple-value-bind (success ,@tmpvars) (progn ,@body)
       (when success
	 ,@sets)
       success)))

;; Any string of alphabetic ([a-zA-Z\200-\377]) characters, underscores ('_')
;;     or digits ([0-9]), not beginning with a digit;
(defvar *dot-word*    (cl-ppcre:create-scanner "^[a-zA-Z_][a-zA-Z_0-9]*"))
(defvar *dot-numeral* (cl-ppcre:create-scanner "[-]?(([.][0-9]+)|([0-9]+(.[0-9]*)?))"))
(defvar *dot-reserved-words* '(|;| { } [ ] = |,| |:| |--| |->| node edge graph digraph subgraph strict))
(defvar *dot-whitespace* '(#\Space
				#\Newline
				#\Linefeed
				#\Return
				#\Tab))
(defun dot-lexer (stream)
  (declare (type stream stream) (notinline find-if))
  (let ((EOF stream))
    (labels ((split-string (string)
	       ;; return a list of characters which make up the string
	       (map 'list #'identity string))
	     (match-in-stream (chars)
	       (typecase chars
		 (null
		  nil)
		 (character
		  (and (match-in-stream (list chars))
		       chars))
		 (string
		  (and (match-in-stream (split-string chars))
		       chars))
		 (symbol
		  (and (match-in-stream (symbol-name chars))
		       chars))
		 (list			; list of characters
		  (let ((pos (file-position stream)))
		    (cond
		      ((every #'(lambda (c &aux (file-char (read-char stream nil EOF nil)))
				  (declare (notinline char-downcase))
				  (char= (char-downcase c)
					 (char-downcase file-char)))
			      chars)
		       chars)
		      (t
		       (file-position stream pos)
		       nil))))))
	     (read-to-whitespace (&aux (buf (list nil nil)))
	       ;; return the string to the next white space, but reset the file position back to the
	       ;; position before reading.	     
	       (let ((pos (file-position stream)))
		 (loop :while t
		       :do (let ((char (read-char stream nil EOF nil)))
			     (cond
			       ((or (member char *dot-whitespace* :test #'char=)
				    (eq EOF char))
				(file-position stream pos)
				(return-from read-to-whitespace
				  (concatenate 'string (car buf))))
			       (t
				(tconc buf char)))))))
	     (skip-white-space (&aux (char (read-char stream nil EOF nil)))
	       (while (and (not (eq EOF char))
			   (member char *dot-whitespace*
				   :test #'char=))
		      ;; read-again
		      (setf char (read-char stream nil EOF nil)))
	       char)
	     (read-quoted-string (&aux (buf (list nil nil)))
	       ;; read the rest of the string, assuming the open \" has already been read.
	       ;; including reading the close quote, and returning the string up to but not including the
	       ;; closing quote.
	       (loop :while t
		     :do (let ((char (read-char stream nil EOF nil)))
			   (case char
			     ((#\\)
			      (tconc buf char)
			      (tconc buf (read-char stream t)) ;; ERROR if there is a dangling \ at end of file
			      )
			     ((#\")
			      (return-from read-quoted-string
				(concatenate 'string (car buf))))
			     (t
			      (tconc buf char))))))
	     (parse-next (&aux it)
	       (cond
		 ;; |;| { } [ ] = |,| |:| |--| |->| node edge graph digraph subgraph strict
		 ((setf it (find-if #'match-in-stream *dot-reserved-words*))
		  (values it it))
	    
		 ;; any double-quoted string ("...") possibly containing escaped quotes (\");
		 ((match-in-stream #\")
		  (values 'ID (read-quoted-string)))
	    
		 ;; an HTML string (<...>).
		 ((match-in-stream #\<)
		  (error "HTML string not yet implemented."))
	    
		 ;; Any string of alphabetic ([a-zA-Z\200-\377]) characters, underscores ('_')
		 ;;     or digits ([0-9]), not beginning with a digit;
		 ((setf it (match-in-stream (cl-ppcre:scan-to-strings *dot-word* (read-to-whitespace))))
		  (values 'ID it))
	    
		 ;; a numeral [-]?(.[0-9]+ | [0-9]+(.[0-9]*)? );
		 ((setf it (match-in-stream (cl-ppcre:scan-to-strings *dot-numeral* (read-to-whitespace))))
		  (values 'ID it))
		 ;; else error in dot file
		 (t
		  (error "syntax error in dot file.  Next word is [~A]" (read-to-whitespace))))))
      #'(lambda (&aux (char (skip-white-space)))
	  (cond ((eq EOF char)
		 (values nil nil))
		(t
		 (unread-char char stream)
		 (parse-next)))))))

;; vgraph	:	[ strict ] (graph | digraph) [ ID ] '{' stmt_list '}'
;; stmt_list	:	[ stmt [ ';' ] [ stmt_list ] ]
;; stmt	        :	node_stmt
;;              |	edge_stmt
;;              |	attr_stmt
;;              |	ID '=' ID
;;              |	subgraph
;; attr_stmt	:	(graph | node | edge) attr_list
;; attr_list	:	'[' [ a_list ] ']' [ attr_list ]
;; a_list	:	ID '=' ID [ (';' | ',') ] [ a_list ]
;; edge_stmt	:	(node_id | subgraph) edgeRHS [ attr_list ]
;; edgeRHS	:	edgeop (node_id | subgraph) [ edgeRHS ]
;; node_stmt	:	node_id [ attr_list ]
;; node_id	:	ID [ port ]
;; port	        :	':' ID [ ':' compass_pt ]
;;              |	':' compass_pt
;; subgraph	:	[ subgraph [ ID ] ] '{' stmt_list '}'
;; compass_pt	:	(n | ne | e | se | s | sw | w | nw | c | _)
;; 
(yacc:define-parser *dot-parser*
  (:start-symbol vgraph)
  (:print-lookaheads nil)
  (:print-goto-graph nil)
  (:print-states nil)
  (:terminals (|;| { } [ ] = |,| |:| |--| |->| node edge graph digraph subgraph strict ID))
  (:precedence ())

  ;; vgraph	:	[ strict ] (graph | digraph) [ ID ] '{' stmt_list '}'
  (vgraph 
   (optional-strict graph-digraph optional-ID { stmt-list } (lambda (optional-strict graph-digraph optional-ID { stmt-list })
							      (declare (ignore { }))
							      `(:strict ,optional-strict
								:directed ,(eql 'digraph graph-digraph)
								:id ,optional-id
								:stmt-list ,stmt-list))))
  (optional-strict
   strict
   ())
  (graph-digraph
   graph
   digraph)
  (optional-ID
   ID
   ())

  ;; stmt_list	:	[ stmt [ ';' ] [ stmt_list ] ]
  ;; stmt_list	:	[ stmt [ ';' ] stmt_list ]
  (stmt-list
   (stmt |optional-;| stmt-list (lambda (stmt _ osl)
				  (declare (ignore _))
				  (cons stmt osl)))
   ())
  (|optional-;|
   |;|
   ())

  ;; stmt	  :	node_stmt
  ;;              |	edge_stmt
  ;;              |	attr_stmt
  ;;              |	ID '=' ID
  ;;              |	subgraph
  (stmt
   node-stmt
   edge-stmt
   attr-stmt
   (ID = ID (lambda (id1 = id2)
	      (declare (ignore =))
	      `(,id1 ,id2)))
   vsubgraph)

  ;; attr_stmt	:	(graph | node | edge) attr_list
  (attr-stmt
   (graph-node-edge attr-list))
  (graph-node-edge
   graph
   node
   edge)

  ;; attr_list	:	'[' [ a_list ] ']' [ attr_list ]
  (attr-list
   ([ optional-a-list ] optional-attr-list (lambda ([ optional-a-list ] optional-attr-list)
					     (declare (ignore [ ]))
					     (append optional-a-list optional-attr-list))))
  (optional-a-list
   a-list
   ())
  (optional-attr-list
   attr-list
   ())
  
  ;; ;; a_list	:	ID '=' ID [ (';' | ',') ] [ a_list ]
  (a-list
   (ID = ID optional-sep optional-a-list (lambda (id1 = id2 optional-sep optional-a-list)
					   (declare (ignore optional-sep =))
					   (cons `(,id1 ,id2) optional-a-list))))
  (optional-sep
   |;|
   |,|
   ())
  
  ;; ;; edge_stmt	:	(node_id | subgraph) edgeRHS [ attr_list ]
  (edge-stmt
   (node-id-subgraph edgeRHS optional-attr-list (lambda (node-id-subgraph edgeRHS optional-attr-list)
						  

						  `(,(car edgeRHS) ,node-id-subgraph ,@(cdr edgeRHS) ,optional-attr-list))))
  (node-id-subgraph
   node-id
   vsubgraph)
  
  ;; edgeRHS	:	edgeop (node_id | subgraph) [ edgeRHS ]
  (edgeRHS
   ;; -> X
   ;; -> X -> Y
   ;; -> X -> Y -> Z
   (edgeop node-id-subgraph optional-edgeRHS (lambda (edgeop node-id-subgraph optional-edgeRHS)
					       `(,edgeop ,node-id-subgraph ,@(cdr optional-edgeRHS)))))
  (edgeop
   ->
   --)
  (optional-edgeRHS
   edgeRHS
   ())

  ;; node_stmt	:	node_id [ attr_list ]
  (node-stmt
   (node-id optional-attr-list (lambda (node-id optional-attr-list)
				 `(,node-id
				   :attrs ,optional-attr-list))))

  ;; node_id	:	ID [ port ]
  (node-id
   (ID optional-port (lambda (ID optional-port)
		       (if optional-port
			   `(:node ,ID)
			   `(:node ,ID
			     :port ,optional-port)))))
  (optional-port
   port
   ())
  
  ;; port	  :	':' ID [ ':' compass_pt ]
  ;;              |	':' compass_pt
  (port
   (|:| ID |optional-:-compass-pt|)
   (|:| compass-pt))
  (|optional-:-compass-pt|
   (|:| compass-pt)
   ())

  ;; subgraph	:	[ subgraph [ ID ] ] '{' stmt_list '}'
  (vsubgraph
   (optional-subgraph-ID { stmt-list } ))
  (optional-subgraph-ID
   (subgraph optional-ID)
   ())

  ;; compass_pt	:	(n | ne | e | se | s | sw | w | nw | c | _)
  (compass-pt
   n
   ne
   e
   se
   s
   sw
   w
   nw
   c
   _)
  )

(cl:defmethod read-dot ((stream stream))
  "Read the stream which is a read stream of a .dot file.  
Returns a parse-tree generated by parsing the .dot file"
  (yacc:parse-with-lexer (dot-lexer stream) *dot-parser* ))

(defun extract-choords (parse-tree)
  "PARSE-TREE is a list returned from CLASS-GRAPH.
Returns a plist with fields
   :bb -- the bounding box of the image
   :coords - a list of plists, each with the fields
      :class -- symbol naming a class which is graphed in the image
      :bb -- bounding box of the rectangle representing the class"
  (flet ((read-prop (prop-name alist)
	   (cadr (assoc prop-name alist :test #'string=))))
    (let ((stmt-list (getf parse-tree :stmt-list))
	  (data (list :bb nil
		      :coords nil)))

      (dolist (stmt stmt-list)
	(cond ((eq 'graph (car stmt))
	       (destructuring-bind (_ attrs) stmt
		 (declare (ignore _))
		 (setf (getf data :bb)
		       (mapcar #'read-from-string (cl-ppcre:split "," (read-prop "bb" attrs))))))
	      ((and (consp (car stmt))
		    (eq :node (caar stmt))
		    (getf (cdr stmt) :attrs))
	       (destructuring-bind (node-plist &rest _ &key attrs) stmt
		 (declare (ignore _))
		 (let* ((pos (mapcar #'read-from-string (cl-ppcre:split "," (read-prop "pos" attrs))))
			(x (car pos))
			(y (cadr pos))
			(width  (read-from-string (read-prop "width" attrs)))
			(height (read-from-string (read-prop "height" attrs))))
		   (push (list :class (read-from-string (getf node-plist :node)) ;; class name
			       :pos pos
			       :width width
			       :height height
			       :bb `(,x
				     ,y
				     ,(+ x width)
				     ,(+ y height)))
			 (getf data :coords)))))))
      data)))

(defun class-graph-for-slime (class-name file-name)
  "Function called from slime. CLASS-NAME is a string which was the symbol-at-point.
FILE-NAME is the name of a tmp file which CL is allowed to write to.
CLASS-GRAPH-FOR-SLIME calls CLASS-TO-DOT to populate the designated file with
the PNG format image of a CLOS class graph representing the class hierarchy.
The slime function, class-graph, calls this function and then displays the image
in an emacs buffer."
  (cond
    ((stringp class-name)
     (class-graph-for-slime (swank::find-definitions-find-symbol-or-package class-name)
			    file-name))
    ((find-class class-name nil)
     (class-to-dot (list (find-class class-name)) (pathname file-name)))
    (t
     (error "No such class ~A" class-name))))
  
