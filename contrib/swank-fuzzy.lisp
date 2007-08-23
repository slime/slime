;;; swank-fuzzy.lisp --- fuzzy symbol completion
;;
;; Author: Brian Downing <bdowning@lavos.net> and others
;; License: Public Domain
;;


(in-package :swank)

;;; For nomenclature of the fuzzy completion section, please read
;;; through the following docstring.

(defslimefun fuzzy-completions (string default-package-name &key limit time-limit-in-msec)
"Returns a list of two values:

  An (optionally limited to LIMIT best results) list of fuzzy
  completions for a symbol designator STRING. The list will be
  sorted by score, most likely match first.

  A flag that indicates whether or not TIME-LIMIT-IN-MSEC has
  been exhausted during computation. If that parameter's value is
  NIL or 0, no time limit is assumed.

The main result is a list of completion objects, where a completion
object is:

    (COMPLETED-STRING SCORE (&rest CHUNKS) FLAGS)

where a CHUNK is a description of a matched substring:

    (OFFSET SUBSTRING)

and FLAGS is a list of keywords describing properties of the 
symbol (see CLASSIFY-SYMBOL).

E.g., completing \"mvb\" in a package that uses COMMON-LISP would
return something like:

    ((\"multiple-value-bind\" 26.588236 ((0 \"m\") (9 \"v\") (15 \"b\"))
     (:FBOUNDP :MACRO))
     ...)

If STRING is package qualified the result list will also be
qualified.  If string is non-qualified the result strings are
also not qualified and are considered relative to
DEFAULT-PACKAGE-NAME.

Which symbols are candidates for matching depends on the symbol
designator's format. The cases are as follows:
  FOO      - Symbols accessible in the buffer package.
  PKG:FOO  - Symbols external in package PKG.
  PKG::FOO - Symbols accessible in package PKG."
  ;; For Emacs we allow both NIL and 0 as value of TIME-LIMIT-IN-MSEC
  ;; to denote an infinite time limit. Internally, we only use NIL for
  ;; that purpose, to be able to distinguish between "no time limit
  ;; alltogether" and "current time limit already exhausted." So we've
  ;; got to canonicalize its value at first:
  (let* ((no-time-limit-p (or (not time-limit-in-msec) (zerop time-limit-in-msec)))
         (time-limit (if no-time-limit-p nil time-limit-in-msec)))
    (multiple-value-bind (completion-set interrupted-p)
        (fuzzy-completion-set string default-package-name :limit limit
                              :time-limit-in-msec time-limit)
      ;; We may send this as elisp [] arrays to spare a coerce here,
      ;; but then the network serialization were slower by handling arrays.
      ;; Instead we limit the number of completions that is transferred
      ;; (the limit is set from Emacs.)
      (list (coerce completion-set 'list) interrupted-p))))


;;; A Fuzzy Matching -- Not to be confused with a fuzzy completion
;;; object that will be sent back to Emacs, as described above.

(defstruct (fuzzy-matching (:conc-name   fuzzy-matching.)
			   (:predicate   fuzzy-matching-p)
			   (:constructor %make-fuzzy-matching))
  symbol	    ; The symbol that has been found to match. 
  score	            ; The higher the better symbol is a match.
  package-chunks    ; Chunks pertaining to the package identifier of the symbol.
  symbol-chunks)    ; Chunks pertaining to the symbol's name.

(defun make-fuzzy-matching (symbol score package-chunks symbol-chunks)
  (declare (inline %make-fuzzy-matching))
  (%make-fuzzy-matching :symbol symbol :score score
			:package-chunks package-chunks
			:symbol-chunks symbol-chunks))


(defun fuzzy-convert-matching-for-emacs (fuzzy-matching converter
					 internal-p package-name)
  "Converts a result from the fuzzy completion core into
something that emacs is expecting.  Converts symbols to strings,
fixes case issues, and adds information describing if the symbol
is :bound, :fbound, a :class, a :macro, a :generic-function,
a :special-operator, or a :package."
  (with-struct (fuzzy-matching. symbol score package-chunks symbol-chunks) fuzzy-matching
    (multiple-value-bind (name added-length)
        (format-completion-result
          (funcall (or converter #'identity) (symbol-name symbol))
          internal-p package-name)
      (list name
            score
            (append package-chunks
		    (mapcar #'(lambda (chunk)
				;; Fix up chunk positions to account for possible
				;; added package identifier.
				(let ((offset (first chunk)) (string (second chunk)))
				  (list (+ added-length offset) string))) 
			    symbol-chunks))
            (classify-symbol symbol)))))

(defun classify-symbol (symbol)
  "Returns a list of classifiers that classify SYMBOL according
to its underneath objects (e.g. :BOUNDP if SYMBOL constitutes a
special variable.) The list may contain the following classification
keywords: :BOUNDP, :FBOUNDP, :GENERIC-FUNCTION, :CLASS, :MACRO, 
:SPECIAL-OPERATOR, and/or :PACKAGE"
  (check-type symbol symbol)
  (let (result)
    (when (boundp symbol)             (push :boundp result))
    (when (fboundp symbol)            (push :fboundp result))
    (when (find-class symbol nil)     (push :class result))
    (when (macro-function symbol)     (push :macro result))
    (when (special-operator-p symbol) (push :special-operator result))
    (when (find-package symbol)       (push :package result))
    (when (typep (ignore-errors (fdefinition symbol))
                 'generic-function)
      (push :generic-function result))
    result))

(defun symbol-classification->string (flags)
  (format nil "~A~A~A~A~A~A~A"
          (if (member :boundp flags) "b" "-")
          (if (member :fboundp flags) "f" "-")
          (if (member :generic-function flags) "g" "-")
          (if (member :class flags) "c" "-")
          (if (member :macro flags) "m" "-")
          (if (member :special-operator flags) "s" "-")
          (if (member :package flags) "p" "-")))


(defun fuzzy-completion-set (string default-package-name &key limit time-limit-in-msec)
  "Returns two values: an array of completion objects, sorted by
their score, that is how well they are a match for STRING
according to the fuzzy completion algorithm.  If LIMIT is set,
only the top LIMIT results will be returned. Additionally, a flag
is returned that indicates whether or not TIME-LIMIT-IN-MSEC was
exhausted."
  (check-type limit (or null (integer 0 #.(1- most-positive-fixnum))))
  (check-type time-limit-in-msec (or null (integer 0 #.(1- most-positive-fixnum))))
  (multiple-value-bind (completion-set interrupted-p)
      (fuzzy-create-completion-set string default-package-name
                                   time-limit-in-msec)
    (when (and limit
               (> limit 0)
               (< limit (length completion-set)))
      (if (array-has-fill-pointer-p completion-set)
          (setf (fill-pointer completion-set) limit)
          (setf completion-set (make-array limit :displaced-to completion-set))))
    (values completion-set interrupted-p)))


(defun fuzzy-create-completion-set (string default-package-name time-limit-in-msec)
  "Does all the hard work for FUZZY-COMPLETION-SET. If
TIME-LIMIT-IN-MSEC is NIL, an infinite time limit is assumed."
  (multiple-value-bind (parsed-name parsed-package-name package internal-p)
      (parse-completion-arguments string default-package-name)
    (flet ((convert (matchings package-name &optional converter)
	     ;; Converts MATCHINGS to completion objects for Emacs.
	     ;; PACKAGE-NAME is the package identifier that's used as prefix
	     ;; during formatting. If NIL, the identifier is omitted.
	     (map-into matchings
		       #'(lambda (m)
			   (fuzzy-convert-matching-for-emacs m converter
							     internal-p
							     package-name))
		       matchings))
	   (fix-up (matchings parent-package-matching)
	     ;; The components of each matching in MATCHINGS have been computed
	     ;; relatively to PARENT-PACKAGE-MATCHING. Make them absolute.
	     (let* ((p parent-package-matching)
		    (p.score  (fuzzy-matching.score p))
		    (p.chunks (fuzzy-matching.package-chunks p)))
	       (map-into matchings
			 #'(lambda (m)
			     (let ((m.score (fuzzy-matching.score m)))
			       (setf (fuzzy-matching.package-chunks m) p.chunks)
			       (setf (fuzzy-matching.score m)
				     (if (string= parsed-name "")
					 ;; (Make package matchings be sorted before all the
                                         ;; relative symbol matchings while preserving over
					 ;; all orderness.)
					 (/ p.score 100)        
					 (+ p.score m.score)))
			       m))
			 matchings)))
	   (find-symbols (designator package time-limit)
	     (fuzzy-find-matching-symbols designator package
					  :time-limit-in-msec time-limit
					  :external-only (not internal-p)))
           (find-packages (designator time-limit)
             (fuzzy-find-matching-packages designator :time-limit-in-msec time-limit)))
      (let ((symbol-normalizer  (completion-output-symbol-converter string))
	    (package-normalizer #'(lambda (package-name)
				    (let ((converter (completion-output-package-converter string)))
				      ;; Present packages with a trailing colon for maximum convenience!
				      (concatenate 'string (funcall converter package-name) ":"))))
            (time-limit time-limit-in-msec) (symbols) (packages) (results))
	(cond ((not parsed-package-name)        ; E.g. STRING = "asd"
	       ;; We don't know if user is searching for a package or a symbol
	       ;; within his current package. So we try to find either.
	       (setf (values packages time-limit) (find-packages parsed-name time-limit))
               (setf (values symbols  time-limit) (find-symbols parsed-name package time-limit))
               (setf symbols  (convert symbols nil symbol-normalizer))
               (setf packages (convert packages nil package-normalizer)))
	      ((string= parsed-package-name "") ; E.g. STRING = ":" or ":foo"
	       (setf (values symbols time-limit) (find-symbols parsed-name package time-limit))
               (setf symbols (convert symbols "" symbol-normalizer)))
	      (t	                        ; E.g. STRING = "asd:" or "asd:foo"
	       ;; Find fuzzy matchings of the denoted package identifier part.
	       ;; After that, find matchings for the denoted symbol identifier
	       ;; relative to all the packages found.
               (multiple-value-bind (found-packages rest-time-limit)
                   (find-packages parsed-package-name time-limit-in-msec)
                 (loop
                    for package-matching across found-packages
                    for package-sym  = (fuzzy-matching.symbol package-matching)
                    for package-name = (funcall symbol-normalizer (symbol-name package-sym))
                    for package      = (find-package package-sym)
                    while (or (not time-limit) (> rest-time-limit 0)) do
                      (multiple-value-bind (matchings remaining-time)
                          (find-symbols parsed-name package rest-time-limit)
                        (setf matchings (fix-up matchings package-matching))
                        (setf matchings (convert matchings package-name symbol-normalizer))
                        (setf symbols   (concatenate 'vector symbols matchings))
                        (setf rest-time-limit remaining-time))
                    finally ; CONVERT is destructive. So we have to do this at last.
                      (setf time-limit rest-time-limit)
                      (setf packages (when (string= parsed-name "")
                                       (convert found-packages nil package-normalizer)))))))
	;; Sort alphabetically before sorting by score. (Especially useful when
	;; PARSED-NAME is empty, and all possible completions are to be returned.)
	(setf results (concatenate 'vector symbols packages))
	(setf results (sort results #'string< :key #'first))  ; SORT + #'STRING-LESSP
	(setf results (stable-sort results #'> :key #'second));  conses on at least SBCL 0.9.18.
	(values results (and time-limit (<= time-limit 0)))))))


(defun get-real-time-in-msecs ()
  (let ((units-per-msec (max 1 (floor internal-time-units-per-second 1000))))
    (values (floor (get-internal-real-time) units-per-msec)))) ; return just one value!


(defun fuzzy-find-matching-symbols (string package &key external-only time-limit-in-msec)
  "Returns two values: a vector of fuzzy matchings for matching
symbols in PACKAGE, using the fuzzy completion algorithm; the
remaining time limit. 

If EXTERNAL-ONLY is true, only external symbols are considered. A
TIME-LIMIT-IN-MSEC of NIL is considered no limit; if it's zero or
negative, perform a NOP."
  (let ((time-limit-p (and time-limit-in-msec t))
        (time-limit (or time-limit-in-msec 0))
        (rtime-at-start (get-real-time-in-msecs))
        (count 0))
    (declare (type boolean time-limit-p))
    (declare (type integer time-limit rtime-at-start))
    (declare (type (integer 0 #.(1- most-positive-fixnum)) count))

    (flet ((recompute-remaining-time (old-remaining-time)
             (cond ((not time-limit-p)
                    (values nil nil)) ; propagate NIL back as infinite time limit.
                   ((> count 0)       ; ease up on getting internal time like crazy.
                    (setf count (mod (1+ count) 128))
                    (values nil old-remaining-time))
                   (t (let* ((elapsed-time (- (get-real-time-in-msecs) rtime-at-start))
                             (remaining (- time-limit elapsed-time)))
                        (values (<= remaining 0) remaining)))))
           (perform-fuzzy-match (string symbol-name)
             (let* ((converter (completion-output-symbol-converter string))
                    (converted-symbol-name (funcall converter symbol-name)))
               (compute-highest-scoring-completion string converted-symbol-name))))
      (let ((completions (make-array 256 :adjustable t :fill-pointer 0))
            (rest-time-limit time-limit))
        (block loop
          (do-symbols* (symbol package)
            (multiple-value-bind (exhausted? remaining-time)
                (recompute-remaining-time rest-time-limit)
              (setf rest-time-limit remaining-time)
              (cond (exhausted? (return-from loop))
                    ((or (not external-only) (symbol-external-p symbol package))
                     (if (string= "" string) ; "" matchs always
                         (vector-push-extend (make-fuzzy-matching symbol 0.0 '() '())
                                             completions)
                         (multiple-value-bind (match-result score)
                             (perform-fuzzy-match string (symbol-name symbol))
                           (when match-result
                             (vector-push-extend
                              (make-fuzzy-matching symbol score '() match-result)
                              completions)))))))))
        (values completions rest-time-limit)))))


(defun fuzzy-find-matching-packages (name &key time-limit-in-msec)
  "Returns a vector of fuzzy matchings for each package that is
similiar to NAME, and the remaining time limit. 
Cf. FUZZY-FIND-MATCHING-SYMBOLS."
  (let ((time-limit-p (and time-limit-in-msec t))
        (time-limit (or time-limit-in-msec 0))
        (rtime-at-start (get-real-time-in-msecs))
        (converter (completion-output-package-converter name))
        (completions (make-array 32 :adjustable t :fill-pointer 0)))
    (declare (type boolean time-limit-p))
    (declare (type integer time-limit rtime-at-start))
    (declare (type function converter))
    (if (and time-limit-p (<= time-limit 0))
        (values #() time-limit)
        (loop for package-name in (mapcan #'package-names (list-all-packages))
              for converted-name = (funcall converter package-name)
              for package-symbol = (or (find-symbol package-name)
                                        (make-symbol package-name)) ; no INTERN
              do (multiple-value-bind (result score)
                     (compute-highest-scoring-completion name converted-name)
                   (when result
                     (vector-push-extend (make-fuzzy-matching package-symbol score result '())
                                         completions)))
              finally
                (return
                  (values completions
                          (and time-limit-p
                               (let ((elapsed-time (- (get-real-time-in-msecs) rtime-at-start)))
                                 (- time-limit elapsed-time)))))))))


(defslimefun fuzzy-completion-selected (original-string completion)
  "This function is called by Slime when a fuzzy completion is
selected by the user.  It is for future expansion to make
testing, say, a machine learning algorithm for completion scoring
easier.

ORIGINAL-STRING is the string the user completed from, and
COMPLETION is the completion object (see docstring for
SWANK:FUZZY-COMPLETIONS) corresponding to the completion that the
user selected."
  (declare (ignore original-string completion))
  nil)


;;;;; Fuzzy completion core

(defparameter *fuzzy-recursion-soft-limit* 30
  "This is a soft limit for recursion in
RECURSIVELY-COMPUTE-MOST-COMPLETIONS.  Without this limit,
completing a string such as \"ZZZZZZ\" with a symbol named
\"ZZZZZZZZZZZZZZZZZZZZZZZ\" will result in explosive recursion to
find all the ways it can match.

Most natural language searches and symbols do not have this
problem -- this is only here as a safeguard.")
(declaim (fixnum *fuzzy-recursion-soft-limit*))

(defun compute-highest-scoring-completion (short full)
  "Finds the highest scoring way to complete the abbreviation
SHORT onto the string FULL, using CHAR= as a equality function for
letters.  Returns two values:  The first being the completion
chunks of the highest scorer, and the second being the score."
  (let* ((scored-results
          (mapcar #'(lambda (result)
                      (cons (score-completion result short full) result))
                  (compute-most-completions short full)))
         (winner (first (sort scored-results #'> :key #'first))))
    (values (rest winner) (first winner))))

(defun compute-most-completions (short full)
  "Finds most possible ways to complete FULL with the letters in SHORT.
Calls RECURSIVELY-COMPUTE-MOST-COMPLETIONS recursively.  Returns
a list of (&rest CHUNKS), where each CHUNKS is a description of
how a completion matches."
  (let ((*all-chunks* nil))
    (declare (special *all-chunks*))
    (recursively-compute-most-completions short full 0 0 nil nil nil t)
    *all-chunks*))

(defun recursively-compute-most-completions 
    (short full 
     short-index initial-full-index 
     chunks current-chunk current-chunk-pos 
     recurse-p)
  "Recursively (if RECURSE-P is true) find /most/ possible ways
to fuzzily map the letters in SHORT onto FULL, using CHAR= to
determine if two letters match.

A chunk is a list of elements that have matched consecutively.
When consecutive matches stop, it is coerced into a string,
paired with the starting position of the chunk, and pushed onto
CHUNKS.

Whenever a letter matches, if RECURSE-P is true,
RECURSIVELY-COMPUTE-MOST-COMPLETIONS calls itself with a position
one index ahead, to find other possibly higher scoring
possibilities.  If there are less than
*FUZZY-RECURSION-SOFT-LIMIT* results in *ALL-CHUNKS* currently,
this call will also recurse.

Once a word has been completely matched, the chunks are pushed
onto the special variable *ALL-CHUNKS* and the function returns."
  (declare ;(optimize speed)
           (fixnum short-index initial-full-index)
           (simple-string short full)
           (special *all-chunks*))
  (flet ((short-cur () 
           "Returns the next letter from the abbreviation, or NIL
            if all have been used."
           (if (= short-index (length short))
               nil
               (aref short short-index)))
         (add-to-chunk (char pos)
           "Adds the CHAR at POS in FULL to the current chunk,
            marking the start position if it is empty."
           (unless current-chunk
             (setf current-chunk-pos pos))
           (push char current-chunk))
         (collect-chunk ()
           "Collects the current chunk to CHUNKS and prepares for
            a new chunk."
           (when current-chunk
             (push (list current-chunk-pos
                         (coerce (reverse current-chunk) 'string)) chunks)
             (setf current-chunk nil
                   current-chunk-pos nil))))
    ;; If there's an outstanding chunk coming in collect it.  Since
    ;; we're recursively called on skipping an input character, the
    ;; chunk can't possibly continue on.
    (when current-chunk (collect-chunk))
    (do ((pos initial-full-index (1+ pos)))
        ((= pos (length full)))
      (let ((cur-char (aref full pos)))
        (if (and (short-cur) 
                 (char= cur-char (short-cur)))
            (progn
              (when recurse-p
                ;; Try other possibilities, limiting insanely deep
                ;; recursion somewhat.
                (recursively-compute-most-completions 
                 short full short-index (1+ pos) 
                 chunks current-chunk current-chunk-pos
                 (not (> (length *all-chunks*) 
                         *fuzzy-recursion-soft-limit*))))
              (incf short-index)
              (add-to-chunk cur-char pos))
            (collect-chunk))))
    (collect-chunk)
    ;; If we've exhausted the short characters we have a match.
    (if (short-cur)
        nil
        (let ((rev-chunks (reverse chunks)))
          (push rev-chunks *all-chunks*)
          rev-chunks))))


;;;;; Fuzzy completion scoring

(defparameter *fuzzy-completion-symbol-prefixes* "*+-%&?<"
  "Letters that are likely to be at the beginning of a symbol.
Letters found after one of these prefixes will be scored as if
they were at the beginning of ths symbol.")
(defparameter *fuzzy-completion-symbol-suffixes* "*+->"
  "Letters that are likely to be at the end of a symbol.
Letters found before one of these suffixes will be scored as if
they were at the end of the symbol.")
(defparameter *fuzzy-completion-word-separators* "-/."
  "Letters that separate different words in symbols.  Letters
after one of these symbols will be scores more highly than other
letters.")

(defun score-completion (completion short full)
  "Scores the completion chunks COMPLETION as a completion from
the abbreviation SHORT to the full string FULL.  COMPLETION is a
list like:
    ((0 \"mul\") (9 \"v\") (15 \"b\"))
Which, if SHORT were \"mulvb\" and full were \"multiple-value-bind\", 
would indicate that it completed as such (completed letters
capitalized):
    MULtiple-Value-Bind

Letters are given scores based on their position in the string.
Letters at the beginning of a string or after a prefix letter at
the beginning of a string are scored highest.  Letters after a
word separator such as #\- are scored next highest.  Letters at
the end of a string or before a suffix letter at the end of a
string are scored medium, and letters anywhere else are scored
low.

If a letter is directly after another matched letter, and its
intrinsic value in that position is less than a percentage of the
previous letter's value, it will use that percentage instead.

Finally, a small scaling factor is applied to favor shorter
matches, all other things being equal."
  (labels ((at-beginning-p (pos) 
             (= pos 0))
           (after-prefix-p (pos) 
             (and (= pos 1) 
                  (find (aref full 0) *fuzzy-completion-symbol-prefixes*)))
           (word-separator-p (pos)
             (find (aref full pos) *fuzzy-completion-word-separators*))
           (after-word-separator-p (pos)
             (find (aref full (1- pos)) *fuzzy-completion-word-separators*))
           (at-end-p (pos)
             (= pos (1- (length full))))
           (before-suffix-p (pos)
             (and (= pos (- (length full) 2))
                  (find (aref full (1- (length full)))
                        *fuzzy-completion-symbol-suffixes*)))
           (score-or-percentage-of-previous (base-score pos chunk-pos)
             (if (zerop chunk-pos) 
                 base-score 
                 (max base-score 
                      (+ (* (score-char (1- pos) (1- chunk-pos)) 0.85)
                         (expt 1.2 chunk-pos)))))
           (score-char (pos chunk-pos)
             (score-or-percentage-of-previous
              (cond ((at-beginning-p pos)         10)
                    ((after-prefix-p pos)         10)
                    ((word-separator-p pos)       1)
                    ((after-word-separator-p pos) 8)
                    ((at-end-p pos)               6)
                    ((before-suffix-p pos)        6)
                    (t                            1))
              pos chunk-pos))
           (score-chunk (chunk)
             (loop for chunk-pos below (length (second chunk))
                   for pos from (first chunk) 
                   summing (score-char pos chunk-pos))))
    (let* ((chunk-scores (mapcar #'score-chunk completion))
           (length-score (/ 10.0 (1+ (- (length full) (length short))))))
      (values
       (+ (reduce #'+ chunk-scores) length-score)
       (list (mapcar #'list chunk-scores completion) length-score)))))

(defun highlight-completion (completion full)
  "Given a chunk definition COMPLETION and the string FULL,
HIGHLIGHT-COMPLETION will create a string that demonstrates where
the completion matched in the string.  Matches will be
capitalized, while the rest of the string will be lower-case."
  (let ((highlit (nstring-downcase (copy-seq full))))
    (dolist (chunk completion)
      (setf highlit (nstring-upcase highlit 
                                    :start (first chunk)
                                    :end (+ (first chunk) 
                                            (length (second chunk))))))
    highlit))

(defun format-fuzzy-completion-set (winners)
  "Given a list of completion objects such as on returned by
FUZZY-COMPLETION-SET, format the list into user-readable output
for interactive debugging purpose."
  (let ((max-len 
         (loop for winner in winners maximizing (length (first winner)))))
    (loop for (sym score result) in winners do
          (format t "~&~VA  score ~8,2F  ~A"
                  max-len (highlight-completion result sym) score result))))

(provide :swank-fuzzy)