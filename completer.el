;;; -*- Mode: Emacs-Lisp -*-
;;;%Header
;;;
;;; Rcs_Info: completer.el,v 3.23 1993/09/03 02:05:07 ivan Rel $
;;;
;;; Partial completion mechanism for GNU Emacs and XEmacs.  Version 3.05
;;; Copyright (C) 1990, 1991, 1992 Chris McConnell, ccm@cs.cmu.edu.
;;; Copyright (C) 2000 Ben Wing.
;;; Copyright (C) 2002 Marco Antoniotti and the ILISP Maintainers
;;;
;;; Author: Chris Mcconnell <chrimc@microsoft.com>
;;; Latest XEmacs Author: Ben Wing
;;; Maintainer: The ILISP Maintainers
;;; Keywords: minibuffer, completion

;;; Thanks to Bjorn Victor for suggestions, testing, and patches for
;;; file completion. 

;;; This file should be part of GNU Emacs and XEmacs.

;;; GNU Emacs and XEmacs are distributed in the hope that they will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.
;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs and XEmacs, but only under the conditions described in the
;;; GNU Emacs and XEmacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs or XEmacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.  Among other things, the copyright notice
;;; and this notice must be preserved on all copies.

;;; When loaded, this file extends the standard completion mechanisms
;;; so that they perform pattern matching completions.  There is also
;;; an interface that allows it to be used by other programs.  The
;;; completion rules are:
;;;
;;; 1) If what has been typed matches any possibility, do normal
;;; completion. 
;;;
;;; 2) Otherwise, generate a regular expression such that
;;; completer-words delimit words and generate all possible matches.
;;; The variable completer-any-delimiter can be set to a character
;;; that matches any delimiter.  If it were " ", then "by  d" would be 
;;; byte-recompile-directory.  If completer-use-words is T, a match is
;;; unique if it is the only one with the same number of words.  If
;;; completer-use-words is NIL, a match is unique if it is the only
;;; possibility.  If you ask the completer to use its best guess, it
;;; will be the shortest match of the possibilities unless
;;; completer-exact is T.
;;;
;;; 3) For filenames, if completer-complete-filenames is T, each
;;; pathname component will be individually completed, otherwise only
;;; the final component will be completed.  If you are using a
;;; distributed file system like afs, you may want to set up a
;;; symbolic link in your home directory or add pathname components to
;;; completer-file-skip so that the pathname components that go across
;;; machines do not get expanded.
;;;
;;; SPACE, TAB, LFD, RET, and ? do normal completion if possible
;;; otherwise they do partial completion.  In addition, C-DEL will
;;; undo the last partial expansion or contraction.  M-RET will always
;;; complete to the current match before returning.  This is useful
;;; when any string is possible, but you want to complete to a string
;;; as when calling find-file.  The bindings can be changed by using
;;; completer-load-hook.
;;;
;;; Modes that use comint-dynamic-complete (like cmushell and ilisp)
;;; will also do partial completion as will M-tab in Emacs LISP.
;;;
;;; Examples:
;;; a-f     auto-fill-mode
;;; b--d    *beginning-of-defun or byte-recompile-directory
;;; by  d   *byte-recompile-directory if completer-any-delimiter is " "
;;; ~/i.e   *~/ilisp.el or ~/il-el.el or ~/ilisp.elc
;;; /u/mi/  /usr/misc/
;;;


(require 'cl)

;;;%Globals
;;;%%Switches
(defvar completer-load-hook nil
  "Hook called when minibuffer partial completion is loaded.")

(defvar completer-disable nil
  "*If T, turn off partial completion.  Use the command
\\[completer-toggle] to set this.")

(defvar completer-complete-filenames t
  "*If T, then each component of a filename will be completed,
otherwise just the final component will be completed.")

(defvar completer-use-words nil ; jwz: this is HATEFUL!
  "*If T, then prefer completions with the same number of words as the
pattern.")

(defvar completer-words "---. <" 
  "*Delimiters used in partial completions.  It should be a set of
characters suitable for inclusion in a [] regular expression.")

(defvar completer-any-delimiter nil
  "*If a character, then a delimiter in the pattern that matches the
character will match any delimiter in completer-words.")

(defvar completer-file-skip "^cs/$\\|@sys\\|.edu/$\\|.gov/$\\|.com/$\\|:/$"
  "*Regular expression for pathname components to not complete.")

(defvar completer-exact nil
  "*If T, then you must have an exact match.  Otherwise, the shortest
string that matches the pattern will be used.")

(defvar completer-cache-size 100
  "*Size of cache to use for partially completed pathnames.")

(defvar completer-use-cache t
  "*Set to nil to disable the partially completed pathname cache.")

;;;%%Internal
(defvar completer-last-pattern ""
  "The last pattern expanded.")

(defvar completer-message nil
  "T if temporary message was just displayed.")

(defvar completer-path-cache nil
  "Cache of (path . choices) for completer.")

(defvar completer-path-separator-string
  (if (eq system-type 'windows-nt) "\\" "/"))

(defvar completer-path-separator-regexp
  (if (eq system-type 'windows-nt) "[/\\]" "/"))

(defvar completer-path-delimiter-list
  (if (eq system-type 'windows-nt) '(?\\ ?/) '(?/)))

(defvar completer-path-separator-regexp-inside-brackets
  (if (eq system-type 'windows-nt) "/\\" "/"))

(defvar completer-dot-dot-list
  (if (eq system-type 'windows-nt) '("../" "..\\") '("../")))

(defvar completer-string nil "Last completer string.")
(defvar completer-table nil "Last completer table.")
(defvar completer-pred nil "Last completer pred.")
(defvar completer-mode nil "Last completer mode.")
(defvar completer-result nil "Last completer result.")

(eval-when (eval load compile)
  (if (not (fboundp 'completion-display-completion-list-function))
      (setf completion-display-completion-list-function
	    'display-completion-list)))


(unless (fboundp 'minibuffer-prompt-end)
  (defun minibuffer-prompt-end ()
    "Return the buffer position of the end of the minibuffer prompt.
Return (point-min) if current buffer is not a mini-buffer."
    (point-min)))

;;;%Utilities
(defun completer-message (message &optional point)
  "Display MESSAGE at optional POINT for two seconds."
  (setq point (or point (point-max))
	completer-message t)
  (let ((end
	 (save-excursion
	   (goto-char point)
	   (insert message)
	   (point)))
	(inhibit-quit t))
    (sit-for 2)
    (delete-region point end)
    (if (and quit-flag 
	     ;; (not (eq 'lucid-19 ilisp-emacs-version-id))
	     ;; (not (string-match "Lucid" emacs-version))
	     (not (memq +ilisp-emacs-version-id+
			'(xemacs lucid-19 lucid-19-new)))
	     )
	(setq quit-flag nil
	      unread-command-char 7))))

;;;
(defun completer-deleter (regexp choices &optional keep)
  "Destructively remove strings that match REGEXP in CHOICES.
Return the modified list.  If optional KEEP, then keep entries that
match regexp."
  (let* ((choiceb choices)
	 choicep)
    (if keep
	(progn
	  (while (and choiceb (not (string-match regexp (car choiceb))))
	    (setq choiceb (cdr choiceb)))
	  (setq choicep choiceb)
	  (while (cdr choicep)
	    (if (string-match regexp (car (cdr choicep)))
		(setq choicep (cdr choicep))
		(rplacd choicep (cdr (cdr choicep))))))
	(while (and choiceb (string-match regexp (car choiceb)))
	  (setq choiceb (cdr choiceb)))
	(setq choicep choiceb)
	(while (cdr choicep)
	  (if (string-match regexp (car (cdr choicep)))
	      (rplacd choicep (cdr (cdr choicep)))
	      (setq choicep (cdr choicep)))))
    choiceb))

;;;%%Regexp
(defun completer-regexp (string delimiters any)
  "Convert STRING into a regexp with words delimited by chars in DELIMITERS.
Any delimiter in STRING that is the same as ANY will match any delimiter."
  (let* ((delimiter-reg (concat "[" delimiters "]"))
	 (limit (length string))
	 (pos 0)
	 (regexp "^"))
    (while (and (< pos limit) (string-match delimiter-reg string pos))
      (let* ((begin (match-beginning 0))
	     (end (match-end 0))
	     (delimiter (substring string begin end))
	     (anyp (eq (elt string begin) any)))
	(setq regexp 
	      (format "%s%s[^%s]*%s" 
		      regexp
		      (regexp-quote (substring string pos begin))
		      (if anyp delimiters delimiter)
		      (if anyp delimiter-reg (regexp-quote delimiter)))
	      pos end)))
    (if (<= pos limit)
	(setq regexp (concat regexp 
			     (regexp-quote (substring string pos limit)))))))

;;;
(defun completer-words (regexp string &optional limit)
  "Return the number of words matching REGEXP in STRING up to LIMIT."
  (setq limit (or limit 1000))
  (let ((count 1)
	(pos 0))
    (while (and (string-match regexp string pos) (<= count limit))
      (setq count (1+ count)
	    pos (match-end 0)))
    count))

;;;%Matcher
(defun completer-matches (string choices delimiters any)
    "Return STRING's matches in CHOICES.
DELIMITERS and the wildcard ANY are used  to segment the strings."
    (let* ((regexp (concat "[" delimiters "]"))
	   (from nil)
	   (to 0)
	   (pattern nil)
	   (len (length string))
	   (matches nil)
	   sub sublen choice word wordlen pat)
      ;; Segment pattern
      (while (< (or from 0) len)
	(setq to (or (string-match regexp string (if from (1+ from))) len))
	(if (eq (elt string (or from 0)) completer-any-delimiter)
	    (setq sub (substring string (if from (1+ from) 0) to)
		  sublen (- (length sub)))
	    (setq sub (substring string (or from 0) to)
		  sublen (length sub)))
	(setq pattern (cons (cons sub sublen) pattern)
	      from to))
      (setq pattern (reverse pattern))
      ;; Find choices that match patterns
      (setq regexp (concat "[" delimiters "]"))
      (while choices
	(setq choice (car choices)
	      word pattern 
	      from 0)
	(while (and word from
		    (let* (begin end)
		      (if (< (setq wordlen (cdr (setq pat (car word)))) 0)
			  (setq begin (1+ from)
				end (+ begin (- wordlen)))
			  (setq begin from
				end (+ begin wordlen)))
		      (and (<= end (length choice))
			   (or (zerop wordlen)
			       (string-equal 
				(car pat)
				(substring choice begin end))))))
	  (setq from (string-match regexp choice 
				   (if (and (zerop from) (zerop wordlen))
				       from
				       (1+ from)))
		word (cdr word)))
	(if (not word) (setq matches (cons choice matches)))
	(setq choices (cdr choices)))
      matches))

;;;
(defun completer-choice (string choices delimiters use-words)
  "Return a list with best match of STRING in CHOICES and T if it is unique.
DELIMITERS are used to separate words.  A match is unique if it is the only
possibility or when USE-WORDS the only possibility with the same
number of words.  The shortest string of multiple possibilities will be
the best match."
  (or (if (null (cdr choices)) (cons (car choices) t))
      (let* ((regexp (concat "[^" delimiters "]*[" delimiters "]"))
	     (words (if use-words (completer-words regexp string)))
	     (choice choices)
	     (unique-p nil)
	     (match nil)
	     (match-count nil)
	     (match-len 1000))
	(while choice
	  (let* ((current (car choice))
		 (length (length current)))
	    (if match-count
		(if (= (completer-words regexp current words) words)
		    (progn
		      (setq unique-p nil)
		      (if (< length match-len)
			  (setq match current
				match-len length))))
		(if (and use-words 
			 (= (completer-words regexp current words) words))
		    (setq match current
			  match-len length
			  match-count t
			  unique-p t)
		    (if (< length match-len)
			(setq match current
			      match-len length)))))
	  (setq choice (cdr choice)))
	(cons match unique-p))))

;;;%Completer
;;;%%Utilities
(defun completer-region (delimiters)
  "Return the completion region bounded by characters in DELIMITERS.
The search is for the current buffer assuming that point is in it."
  (cons (save-excursion (skip-chars-backward delimiters (minibuffer-prompt-end))
                        (point))
	(save-excursion (skip-chars-forward delimiters)
                        (point))))
	 
;;;
(defun completer-last-component (string)
  "Return the start of the last filename component in STRING."
  (let ((last (1- (length string)))
	(match 0)
	(end 0))
    (while (and (setq match (string-match completer-path-separator-regexp string end))
		(< match last))
      (setq end (1+ match)))
    end))

;;;
(defun completer-match-record (string matches delimiters any dir mode)
  "Return (match lcs choices unique) for STRING in MATCHES.
DELIMITERS or ANY wildcards and DIR if a filename when in MODE."
  (let ((pattern (if dir
		     (substring string (completer-last-component string))
		     string))
	match)
    (setq matches (completer-matches pattern matches delimiters any)
	  match (try-completion pattern (mapcar 'list matches)))
    ;; If try-completion produced an exact match for an element in 'matches',
    ;; then remove any partial matches from 'matches' and set the unique
    ;; match flag.
    (and (stringp match) (member match matches) (setq matches (list match)))
    (if (cdr matches)
	(let ((lcs (concat dir (try-completion "" (mapcar 'list matches)))))
	  (setq match (if (not completer-exact)
			  (completer-choice
			   pattern matches delimiters completer-use-words)))
	  (list (if match (concat dir (car match)))
		lcs
		matches
		(cdr match)))
      (if matches
	  (progn (setq match (concat dir (car matches)))
		 (list match match matches t))
	(list nil nil nil nil)))))

;;;%%Complete file
(defun completer-extension-regexp (extensions)
  "Return a regexp that matches a string ending with any string in EXTENSIONS."
  (concat "\\(" (mapconcat 'regexp-quote extensions "\\|") "\\)\\'"))

;;;
(defun completer-flush ()
  "Flush completer's pathname cache."
  (interactive)
  (setq completer-path-cache nil))

;;;
(defun completer-cache (path pred words any mode)
  "Check to see if PATH is in path cache with PRED, WORDS, ANY and MODE."
  (let* ((last nil)
	 (ptr completer-path-cache)
	 (size 0) 
	 (result nil))
    (if completer-use-cache
	(while ptr
	  (let ((current (car (car ptr))))
	    (if (string-equal current path)
		(progn
		  (if last
		      (progn
			(rplacd last (cdr ptr))
			(rplacd ptr completer-path-cache)
			(setq completer-path-cache ptr)))
		  (setq result (cdr (car ptr))
			ptr nil))
	      (if (cdr ptr) (setq last ptr))
	      (setq size (1+ size)
		    ptr (cdr ptr))))))
    (or result
	(let* ((choices 
		(completer path 'read-file-name-internal pred words any
			   mode t)))
	  (if (and (or (car (cdr (cdr (cdr choices))))
		       (string= path (car choices)))
		   (memq (elt (car choices) (1- (length (car choices))))
			 completer-path-delimiter-list))
	      (progn 
		(if (>= size completer-cache-size) (rplacd last nil))
		(setq completer-path-cache 
		      (cons (cons path choices) completer-path-cache))))
	  choices))))


(defun completer-file (string pred words any mode)
  "Return (match common-substring matches unique-p) for STRING.
It uses 'READ-FILE-NAME-INTERNAL' for choices that pass PRED using WORDS to
delimit words.  Optional ANY is a delimiter that matches any of the
delimiters in WORD.  If optional MODE is nil or 'help then possible
matches will always be returned."
  ;; Canonicalize slashes under windows-nt for proper completion
  (when (and (eq system-type 'windows-nt) (featurep 'xemacs))
    (setq string (replace-in-string string "/" "\\\\")))
  (let* ((case-fold-search completion-ignore-case)
	 (last (and (eq mode 'exit-ok) (completer-last-component string)))
	 (position

	  ;; Original
	  ;; Special hack for CMU RFS filenames
	  ;; (if (string-match "^/\\.\\./[^/]*/" string)
	  ;;    (match-end 0)
	  ;;  (string-match "[^~/]" string))

	  ;; 2002-05-23
	  ;; New by Ben Wing
	  ;; Find beginning of first directory component.
	  (cond ((string-match "^/\\.\\./[^/]*/" string)
		 ;; CMU RFS filenames like /../computername/foo/bar.c
		 (match-end 0))

		((and (memq system-type '(windows-nt cygwin32))
		      (string-match "[/\\][/\\][^/\\]*[/\\]" string))
		 ;; windows-nt filenames like \\computername\foo\bar.c, or
		 ;; cygwin filenames like //d/foo/bar.c
		 (match-end 0))

		((and (eq system-type 'windows-nt)
		      (string-match "[A-Za-z]:[/\\]?" string))
		 ;; windows-nt filenames like c:\foo\bar.c or c:bar.c
		 (match-end 0))

		(t
		 ;; normal absolute or relative names, or names beginning
		 ;; with ~/
		 (string-match
		  (concat "[^~" completer-path-separator-regexp-inside-brackets
			  "]") string)))
	 )
	 (new (substring string 0 position))
	 (user (if (string= new "~")
		   (setq new (file-name-directory (expand-file-name new)))))
	 (words (concat words completer-path-separator-regexp-inside-brackets))
	 (len (length string))
	 (choices nil)
	 (end nil)
	 (old-choices (list nil nil nil nil)))
    (while position
      (let* ((begin (string-match completer-path-separator-regexp
				  string
				  position))
	     (exact-p nil))
	(setq end (when begin (match-end 0))
	      choices
	      ;; Ends with a /, so check files in directory
	      (if (and (memq mode '(nil help)) (= position len))
		  (completer-match-record 
		   ""
		   ;; This assumes that .. and . come at the end
		   (let* ((choices
			   (all-completions new 'read-file-name-internal))
			  (choicep choices))
		     (if (member* (first choicep) completer-dot-dot-list
				  :test #'string=)
			 (cdr (cdr choicep))
		       (while (cdr choicep)
			 (if (member* (second choicep) completer-dot-dot-list
				      :test #'string=)
			     (rplacd choicep nil))
			 (setq choicep (cdr choicep)))
		       choices))
		   words any new mode)
		(if (eq position last)
		    (let ((new (concat new (substring string position))))
		      (list new new nil t))
		  (let ((component (substring string position end)))
		    (if (and end
			     (string-match completer-file-skip component))
			;; Assume component is complete
			(list (concat new component) 
			      (concat new component)
			      nil t)
		      (completer-cache
		       (concat new component)
		       pred words any mode))))))
	;; Keep going if unique or we match exactly
	(if (or (car (cdr (cdr (cdr choices))))
		(setq exact-p
		      (string= (concat new (substring string position end))
			       (car choices))))
	    (setq old-choices
		  (let* ((lcs (car (cdr choices)))
			 (matches (car (cdr (cdr choices))))
			 ;; (slash (and lcs (string-match "/$" lcs))))
			 (slash
			  (and lcs
			       (string-match
				(concat completer-path-separator-regexp "$")
				lcs))))
 
		    (list nil
			  (if slash (substring lcs 0 slash) lcs)
			  (if (and (cdr matches) 
				   (or (eq mode 'help) (not exact-p)))
			      matches)
			  nil))
		  new (car choices)
		  position end)
	  ;; Its ok to not match user names because they may be in
	  ;; different root directories
	  (if (and (= position 1) (= (elt string 0) ?~))
	      (setq new (substring string 0 end)
		    choices (list new new (list new) t)
		    user nil
		    position end)
	    (setq position nil)))))
    (if (not (car choices))
	(setq choices old-choices))
    (if (and (car choices)
	     (not (eq mode 'help))
	     (not (car (cdr (cdr (cdr choices))))))
	;; Try removing completion ignored extensions
	(let* ((extensions
		(completer-extension-regexp completion-ignored-extensions))
	       (choiceb (car (cdr (cdr choices))))
	       (choicep choiceb)
	       (isext nil)
	       (noext nil))
	  (while choicep
	    (if (string-match extensions (car choicep))
		(setq isext t)
	      (setq noext t))
	    (if (and isext noext)
		;; There are matches besides extensions
		(setq choiceb (completer-deleter extensions choiceb)
		      choicep nil)
	      (setq choicep (cdr choicep))))
	  (if (and isext noext)
	      (setq choices
		    (completer-match-record 
		     (if end (substring string end) "")
		     choiceb words any
		     (file-name-directory (car (cdr choices)))
		     mode)))))
    (if user
	(let ((match (car choices))
	      (lcs (car (cdr choices)))
	      (len (length user)))
	  (setq choices
		(cons (if match (concat "~" (substring match len)))
		      (cons (if lcs (concat "~" (substring lcs len)))
			    (cdr (cdr choices)))))))
    choices))

;;;%Exported program interface
;;;%%Completer
(defun completer (string table pred words
			 &optional any mode file-p)
  "Return (match common-substring matches unique-p) for STRING in TABLE.
The choices must also pass PRED using WORDS to delimit words.  If the
flag 'COMPLETER-COMPLETE-FILENAMES' is T and the table is
'READ-FILE-NAME-INTERNAL', then filename components will be individually
expanded.  Optional ANY is a delimiter that can match any delimiter in
WORDS.  Optional MODE is nil for complete, 'help for help and 'exit
for exit."
  (if (and (stringp completer-string) 
	   (string= string completer-string)
	   (eq table completer-table)
	   (eq pred completer-pred)
	   (not file-p)
	   (or (eq mode completer-mode)
	       (not (memq table '(read-file-name-internal
				  read-directory-name-internal)))))
      completer-result
      (setq 
       completer-string ""
       completer-table table
       completer-pred pred
       completer-mode mode
       completer-result
       (if (and completer-complete-filenames
		(not file-p)
		(memq table '(read-file-name-internal
			      read-directory-name-internal)))
	   (completer-file string pred words any mode)
	   (let* ((file-p (or file-p
			      (memq table
				    '(read-file-name-internal
				      read-directory-name-internal))))
		  (case-fold-search completion-ignore-case)
		  (pattern (concat "[" words "]"))
		  (component (if file-p (completer-last-component string)))
		  (dir (if component (substring string 0 component)))
		  (string (if dir (substring string component) string))
		  (has-words (or (string-match pattern string)
				 (length string))))
	     (if (and file-p (string-match "^\\$" string))
		 ;; Handle environment variables
		 (let ((match
			(getenv (substring string 1 
					   ;; (string-match "/" string)))) ; old
					   (string-match
					    completer-path-separator-regexp
					    string))))
		       )
		   ;; (if match (setq match (concat match "/"))) ; old
		   (when match
		     (setq match
			   (concat match
				   completer-path-separator-string)))
 
		   (list match match (list match) match))
		 (let* ((choices
			 (all-completions 
			  (concat dir (substring string 0 has-words))
			  table pred))
			(regexp (completer-regexp string words any)))
		   (if choices
		       (completer-match-record 
			string 
			(completer-deleter regexp choices t) 
			words any dir mode)
		       (list nil nil nil nil))))))
       completer-string string)
      completer-result))

;;;%%Display choices
(defun completer-display-choices (choices &optional match message end
					  display)
  "Display the list of possible CHOICES.
MATCH, MESSAGE, END and DISPLAY are used optionally.  If MATCH is
non-nil, it will be flagged as the best guess.  If there are no
choices, display MESSAGE.  END is where to put temporary messages.  If
DISPLAY is present then it will be called on each possible completion
and should return a string."

  (if choices
      (with-output-to-temp-buffer "*Completions*"
	(if (cdr choices) 
	    (funcall completion-display-completion-list-function
	     (sort
	      (if display
		  (let ((old choices)
			(new nil))
		    (while old
		      (setq new (cons (funcall display (car old)) new)
			    old (cdr old)))
		    new)
		(copy-sequence choices))
	      (function (lambda (x y)
			  (string-lessp (or (car-safe x) x)
					(or (car-safe y) y)))))))
	(if match
	    (save-excursion
	      (set-buffer "*Completions*")
	      (goto-char (point-min))
	      (let ((buffer-read-only nil))
		(insert "Guess = " match (if (cdr choices) ", " "") "\n")))))
      (beep)
      (completer-message (or message " (No completions)") end)))

;;;%%Goto
(defun completer-goto (match lcs choices unique delimiters words 
			     &optional mode display)
  "Go to the part of the string that disambiguates CHOICES.
MATCH is the best match, LCS is the longest common substring of all
of the matches.  CHOICES is a list of the possibilities, UNIQUE
indicates if MATCH is unique.  DELIMITERS are possible bounding
characters for the completion region.  WORDS are the characters that
delimit the words for partial matches.  Replace the region bounded by
delimiters with the match if unique and the lcs otherwise unless
optional MODE is 'help.  Then go to the part of the string that
disambiguates CHOICES using WORDS to separate words and display the
possibilities if the string was not extended.  If optional DISPLAY is
present then it will be called on each possible completion and should
return a string."
  (setq completer-message nil)
  (let* ((region (completer-region delimiters))
	 (start (car region))
	 (end (cdr region))
	 (string (buffer-substring start end))
	 ;; (file-p (string-match "[^ ]*\\(~\\|/\\|$\\)" string))
	 (file-p (string-match (if (eq system-type 'windows-nt)
				   "[^ ]*\\(~\\|/\\|\\\\\\|\\|$\\)"
				 "[^ ]*\\(~\\|/\\|$\\)")
			       string))
	 (no-insert (eq mode 'help))
	 (message t)
	 (new (not (string= (buffer-substring start (point)) lcs))))
    (if unique
	(if no-insert
	    (progn
	      (goto-char end)
	      (completer-display-choices choices match nil end display))
	    (if (string= string match)
		(if (not file-p) 
		    (progn (goto-char end)
			   (completer-message " (Sole completion)" end)))
		(completer-insert match delimiters)))
	;;Not unique
	(if lcs
	    (let* ((regexp 
		    ;; (concat "[" words (if file-p "/") "]")
		    (concat "["
			    words
			    (and file-p completer-path-separator-regexp-inside-brackets)
			    "]")
		    )
		   (words (completer-words regexp lcs))
		   (point nil))
	      ;; Go to where its ambiguous
	      (goto-char start)
	      (unless no-insert
		(insert lcs)
		(setq completer-last-pattern 
		      (list string delimiters (current-buffer) start)
		      start (point)
		      end (+ end (length lcs))))
	      ;; Skip to the first delimiter in the original string
	      ;; beyond the ambiguous point and keep from there on
	      (if (re-search-forward regexp end 'move words)
		  (progn
		    (if (and (not no-insert) match)
			(let ((delimiter
			       (progn
				 (string-match (regexp-quote lcs) match)
				 (substring match (match-end 0)
					    (1+ (match-end 0))))))
			  (if (string-match regexp delimiter)
			      (insert delimiter))))
		    (forward-char -1)))
	      (unless no-insert
		(setq end (- end (- (point) start)))
		(delete-region start (point)))))
	(if choices
	    (when (or no-insert (not new))
	      (completer-display-choices choices match nil end display))
	    (when file-p
	      (when (not (= (point) end)) (forward-char 1))
	      (unless (save-excursion (re-search-forward completer-path-separator-regexp end t))
		(goto-char end)))
	    (when message
	      (beep)
	      (completer-message (if no-insert 
				     " (No completions)"
				   " (No match)")
				 end))))))

;;;%Exported buffer interface
;;;%%Complete and go
(defun completer-complete-goto (delimiters words table pred 
					   &optional no-insert display)
  "Complete the string bound by DELIMITERS using WORDS to bound words
for partial matches in TABLE with PRED and then insert the longest
common substring unless optional NO-INSERT and go to the point of
ambiguity.  If optional DISPLAY, it will be called on each match when
possible completions are shown and should return a string."
  (let* ((region (completer-region delimiters)))
    (apply 'completer-goto 
	   (append (completer (buffer-substring (car region) (cdr region))
			      table pred words completer-any-delimiter
			      no-insert)
		  (list delimiters words no-insert display)))))

;;;%%Undo
(defun completer-insert (match delimiters &optional buffer undo)
  "Replace the region bounded with characters in DELIMITERS by MATCH.
Then save it so that it can be restored by completer-undo."
  (let* ((region (completer-region delimiters))
	 (start (car region))
	 (end (cdr region)))
    (if (and undo (or (not (= start undo)) 
		      (not (eq (current-buffer) buffer))))
	(error "No previous pattern")
	(setq completer-last-pattern (list (buffer-substring start end) 
					   delimiters
					   (current-buffer)
					   start))
	(delete-region start end)
	(goto-char start)
	(insert match))))

;;;
(defun completer-undo ()
  "Swap the last expansion and the last match pattern."
  (interactive)
  (if completer-last-pattern
      (apply 'completer-insert completer-last-pattern)
      (error "No previous pattern")))

;;;%Minibuffer specific code
;;;%%Utilities
(defun completer-minibuf-string ()
  "Remove dead filename specs from the minibuffer.
Dead filename should be delimited by // or ~ or $ and return the
resulting string."
  (save-excursion
    (goto-char (point-max))
    (if (and (memq minibuffer-completion-table
		   '(read-file-name-internal read-directory-name-internal))
	     (re-search-backward
	      ;; "//\\|/~\\|.\\$"
	      (if (memq system-type '(windows-nt cygwin32))
		  ;; // is meaningful
		  "/~\\|.\\$"
		"//\\|/~\\|.\\$")
	      (minibuffer-prompt-end)
	      t))
	(delete-region (minibuffer-prompt-end) (1+ (point))))
    (buffer-substring (minibuffer-prompt-end) (point-max))))

;;;
(defun completer-minibuf-exit ()
  "Exit the minibuffer and clear completer-last-pattern."
  (interactive)
  (setq completer-last-pattern nil)
  (exit-minibuffer))

;;;
(defun completer-new-cmd (cmd)
  "Return T if we can't execute the old minibuffer version of CMD."
  (if (or completer-disable
	  (let ((string (completer-minibuf-string)))
	    (or
	     (not (string-match
		   (concat "["
			   completer-words
			   completer-path-separator-regexp-inside-brackets
			   "~]")
		   string))
	      (condition-case ()
		  (let ((completion
			 (try-completion string
					 minibuffer-completion-table
					 minibuffer-completion-predicate)))
		    (if (memq minibuffer-completion-table
			      '(read-file-name-internal
				read-directory-name-internal))
			;; Directories complete as themselves
			(and completion
			     (or (not (string= string completion))
				 (file-exists-p completion)))
			completion))
		(error nil)))))
      (progn
	(funcall cmd)
	nil)
      t))

;;;
(defun completer-minibuf (&optional mode)
  "Partial completion of minibuffer expressions.
Optional MODE is (quote help) for help and (quote exit) for exit.

If what has been typed so far matches any possibility normal
completion will be done.  Otherwise, the string is considered to be a
pattern with words delimited by the characters in
completer-words.  If completer-exact is T, the best match will be
the shortest one with the same number of words as the pattern if
possible and otherwise the shortest matching expression.  If called
with a prefix, caching will be temporarily disabled.

Examples:
a-f     auto-fill-mode
r-e     rmail-expunge
b--d    *beginning-of-defun or byte-recompile-directory
by  d   *byte-recompile-directory if completer-any-delimiter is \" \"
~/i.e   *~/ilisp.el or ~/il-el.el or ~/ilisp.elc
/u/mi/  /usr/misc/"
  (interactive)
  (append
   (let ((completer-use-cache (not (or (not completer-use-cache)
				       current-prefix-arg))))
     (completer (completer-minibuf-string)
		minibuffer-completion-table
		minibuffer-completion-predicate
		completer-words
		completer-any-delimiter
		mode))
   (list "^" completer-words mode)))

;;;%%Commands
(defun completer-toggle ()
  "Turn partial completion on or off."
  (interactive)
  (setq completer-disable (not completer-disable))
  (message (if completer-disable 
	       "Partial completion OFF"
	       "Partial completion ON")))

;;;
(defvar completer-old-help
  (lookup-key minibuffer-local-must-match-map "?")
  "Old binding of ? in minibuffer completion map.")
(defun completer-help ()
  "Partial completion minibuffer-completion-help.  
See completer-minibuf for more information."
  (interactive)
  (if (completer-new-cmd completer-old-help)
      (apply 'completer-goto (completer-minibuf 'help))))

;;;
(defvar completer-old-completer
  (lookup-key minibuffer-local-must-match-map "\t")
  "Old binding of TAB in minibuffer completion map.")

(defun completer-complete ()
  "Partial completion minibuffer-complete.
See completer-minibuf for more information."
  (interactive)
  (if (completer-new-cmd completer-old-completer)
      (apply 'completer-goto (completer-minibuf))))

;;;
(defvar completer-old-word
  (lookup-key minibuffer-local-must-match-map " ")
  "Old binding of SPACE in minibuffer completion map.")
(defun completer-word ()
  "Partial completion minibuffer-complete.
See completer-minibuf for more information."
  (interactive)
  (if (eq completer-any-delimiter ?\ )
      (insert ?\ )
      (if (completer-new-cmd completer-old-word)
	  (apply 'completer-goto (completer-minibuf)))))

;;; 
(defvar completer-old-exit
  (lookup-key minibuffer-local-must-match-map "\n")
  "Old binding of RET in minibuffer completion map.")
(defun completer-exit ()
  "Partial completion minibuffer-complete-and-exit.
See completer-minibuf for more information."
  (interactive)
  (if (completer-new-cmd completer-old-exit)
      (let* ((completions (completer-minibuf 'exit))
	     (match (car completions))
	     (unique-p (car (cdr (cdr (cdr completions))))))
	(apply 'completer-goto completions)
	(if unique-p
	    (completer-minibuf-exit)
	    (if match
		(progn (completer-insert match "^")
		       (if minibuffer-completion-confirm
			   (completer-message " (Confirm)")
			   (completer-minibuf-exit)))
		(if (not completer-message) (beep)))))))

;;;
(defun completer-match-exit ()
  "Exit the minibuffer with the current best match."
  (interactive)
  (let* ((completions (completer-minibuf 'exit))
	 (guess (car completions)))
    (if (not guess) 
	;; OK if last filename component doesn't match
	(setq completions (completer-minibuf 'exit-ok)
	      guess (car completions)))
    (if guess
	(progn
	  (goto-char (minibuffer-prompt-end))
	  (insert guess)
	  (delete-region (point) (point-max))
	  (exit-minibuffer))
	(apply 'completer-goto completions))))

;;;%%Keymaps
;this interferes with normal undo.
;(define-key minibuffer-local-completion-map "\C-_"  'completer-undo)
(define-key minibuffer-local-completion-map "\t"    'completer-complete)
(define-key minibuffer-local-completion-map " "     'completer-word)
(define-key minibuffer-local-completion-map "?"     'completer-help)
(define-key minibuffer-local-completion-map "\n"    'completer-minibuf-exit)
(define-key minibuffer-local-completion-map "\r"    'completer-minibuf-exit)
(define-key minibuffer-local-completion-map "\M-\n" 'completer-match-exit)
(define-key minibuffer-local-completion-map "\M-\r" 'completer-match-exit)

;this interferes with normal undo.
;(define-key minibuffer-local-must-match-map "\C-_"  'completer-undo)
(define-key minibuffer-local-must-match-map "\t"    'completer-complete)
(define-key minibuffer-local-must-match-map " "     'completer-word)
(define-key minibuffer-local-must-match-map "\n"    'completer-exit)
(define-key minibuffer-local-must-match-map "\r"    'completer-exit)
(define-key minibuffer-local-must-match-map "?"     'completer-help)
(define-key minibuffer-local-must-match-map "\M-\n" 'completer-match-exit)
(define-key minibuffer-local-must-match-map "\M-\r" 'completer-match-exit)

;;;%comint 
(defun completer-comint-dynamic-list-completions (completions)
  "List in help buffer sorted COMPLETIONS.
Typing SPC flushes the help buffer."
  (completer-comint-dynamic-complete-1 nil 'help))

(defun completer-comint-dynamic-complete-filename ()
  "Dynamically complete the filename at point."
  (interactive)
  (completer-comint-dynamic-complete-1 nil t))

;;;
(defun completer-comint-dynamic-complete-1 (&optional undo mode)
  "Complete the previous filename or display possibilities if done
twice in a row.  If called with a prefix, undo the last completion."
  (interactive "P")
  (if undo
      (completer-undo)
    ;; added by jwz: don't cache completions in shell buffer!
    (setq completer-string nil)
    (let ((conf (current-window-configuration)));; lemacs change
      (completer-complete-goto "^ \t\n\""
			       completer-words
			       'read-file-name-internal
			       default-directory
			       mode)
      ;; lemacs change
      (when (eq mode 'help) (comint-restore-window-config conf))
      )))

;(fset 'comint-dynamic-complete 'completer-comint-dynamic-complete)
(fset 'comint-dynamic-complete-filename
      'completer-comint-dynamic-complete-filename)
(fset 'comint-dynamic-list-completions 
      'completer-comint-dynamic-list-completions)

;;; Set the functions again if comint is loaded.
(setq comint-load-hook 
      (cons (function (lambda ()
			;; (fset 'comint-dynamic-complete 
			;;       'completer-comint-dynamic-complete)
			(fset 'comint-dynamic-complete-filename
			      'completer-comint-dynamic-complete-filename)
			(fset 'comint-dynamic-list-completions 
			      'completer-comint-dynamic-list-completions)))
	    (when (and (boundp 'comint-load-hook) comint-load-hook)
	      (if (consp comint-load-hook)
		  (if (eq (car comint-load-hook) 'lambda)
		      (list comint-load-hook)
		    comint-load-hook)
		(list comint-load-hook)))))

;;;%lisp-complete-symbol
(defun lisp-complete-symbol (&optional mode)
  "Perform partial completion on Lisp symbol preceding point.
That symbol is compared against the symbols that exist and any additional
characters determined by what is there are inserted.  If the symbol
starts just after an open-parenthesis, only symbols with function
definitions are considered.  Otherwise, all symbols with function
definitions, values or properties are considered.  If called with a
negative prefix, the last completion will be undone."
  (interactive "P")
  (if (< (prefix-numeric-value mode) 0)
      (completer-undo)
      (let* ((end (save-excursion (skip-chars-forward "^ \t\n)]}\"") (point)))
	     (beg (save-excursion
		    (backward-sexp 1)
		    (while (= (char-syntax (following-char)) ?\')
		      (forward-char 1))
		    (point)))
	     (pattern (buffer-substring beg end))
	     (predicate
	      (if (eq (char-after (1- beg)) ?\()
		  'fboundp
		  (function (lambda (sym)
		    (or (boundp sym) (fboundp sym)
			(symbol-plist sym))))))
	     (completion (try-completion pattern obarray predicate)))
	   (cond ((eq completion t))
	      ((null completion)
	       (completer-complete-goto
		"^ \t\n\(\)[]{}'`" completer-words
		obarray predicate 
		nil
		(if (not (eq predicate 'fboundp))
		    (function (lambda (choice)
		      (if (fboundp (intern choice))
			  (list choice " <f>")
			  choice))))))
	      ((not (string= pattern completion))
	       (delete-region beg end)
	       (insert completion))
	      (t
	       (message "Making completion list...")
	       (let ((list (all-completions pattern obarray predicate)))
		 (or (eq predicate 'fboundp)
		     (let (new)
		       (while list
			 (setq new (cons (if (fboundp (intern (car list)))
					     (list (car list) " <f>")
					     (car list))
					 new))
			 (setq list (cdr list)))
		       (setq list (nreverse new))))
		 (with-output-to-temp-buffer "*Help*"
		   (funcall completion-display-completion-list-function
		    (sort list (function (lambda (x y)
					   (string-lessp
					    (or (car-safe x) x)
					    (or (car-safe y) y))))))))
	       (message "Making completion list...%s" "done"))))))

;;;%Hooks
(provide 'completer)
(run-hooks 'completer-load-hook)

;;; end of file -- completer.el --
