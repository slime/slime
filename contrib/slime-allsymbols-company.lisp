(in-package :swank)


(defun shortest-package-name (package)
  (if (null (package-nicknames package))
      (package-name package)
      (swank::shortest-package-nickname package)))
    
(defun format-allsymbols-completion-set (symbols package-name)
  "Format a set of completion strings. Returns a list of strings with package qualifiers if needed."
  (let ((this (find-package (read-from-string package-name))))
    (mapcar (lambda (symbol)
	      (multiple-value-bind (sym internal-p) (and this (find-symbol (string symbol) this))
		(string-downcase 
		 (if (or (eq sym symbol) (eq (symbol-package sym) (symbol-package symbol)))
		     (string symbol)
		     (if (keywordp symbol)
			 (cat ":" (string symbol))
			 (untokenize-symbol (shortest-package-nickname (symbol-package symbol)) t (string symbol)))))))
	    symbols)))

(defslimefun allsymbol-completions (string package-name &aux default-package-name)
  (destructuring-bind (maybe-package-name symbol-name)
      (let ((colon (position #\: string)))
  	(if colon
  	    (list (subseq string 0 (position #\: string)) (subseq string (1+ (position #\: string :from-end t))))
  	    (list package-name string)))
    (when  (and maybe-package-name (find-package (string-upcase maybe-package-name)))
      (setq default-package-name (string-upcase maybe-package-name)))
    (setq string symbol-name))
  ;(cl-user::print-db package-name string)
  (let ((results 
	  #+:abcl (abcl-find-candidates string package-name)
	  #-:abcl (not-abcl-find-candidates string package-name)
	  ))
    (let ((default-package (or (find-package default-package-name) *package*)))
      (flet ((relative-importance (a b)
	       (if (and (eq (symbol-package a) default-package)
			(eq (symbol-package b) default-package))
		   (or (boundp a) (fboundp a))
		   (if (and (eq (symbol-package a) default-package)
			    (not (eq (symbol-package b) default-package)))
		       t
		       (string-lessp (String a) (string b))))))
	(list (format-allsymbols-completion-set 
	       (sort results #'relative-importance)
	       package-name)
	      string)))))

#+:abcl
(defun abcl-find-candidates (string package-name &aux results)
  (declare (ignore package-name))
  (declare (optimize (speed 3) (safety 0)))
  (let ((pattern (java:jstatic "compile" (java:jclass "java.util.regex.Pattern") (concatenate 'string "(?i)^" (java:jstatic "quote" (java:jclass "java.util.regex.Pattern") string) ".*"))))
    (let ((matcher (java::jmethod  "java.util.regex.Pattern" "matcher" "java.lang.CharSequence"))
	  (matches (java::jmethod "java.util.regex.Matcher" "matches" )))
      (do-all-symbols (s)
	(when (java::jcall matches (java::jcall matcher pattern (string s)))
	  (push s results))))
    results))

#-:abcl
(defun not-abcl-find-candidates (string package-name &aux results)
  (declare (ignore package-name))
  (do-all-symbols (s)
    (when (eql (search string (string s) :test 'char-equal) 0)
      (push s results)))
  results)

(provide 'slime-allsymbols-company)
