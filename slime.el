;; -*- mode: emacs-lisp; mode: outline-minor; outline-regexp: ";;;;*" -*-
;;; slime.el -- Superior Lisp Interaction Mode, Extended
;;
;; Originally slim.el by Eric Marsden <emarsden@laas.fr>
;; Hacked some more by Luke Gorrie <luke@bluetail.com> (maintainer)
;;
;;     Copyright (C) 2003  Eric Marsden
;;     Copyright (C) 2003  Luke Gorrie
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;     
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.


;;; Commentary:

;; This minor mode extends Lisp-Mode with CMUCL-specific features.
;; The features can be summarised thusly:
;;
;;   Separate control channel (WIRE) for communication with CMUCL,
;;   similar to Hemlock. This is used to implement other features,
;;   rather than talking to the Lisp listener "in-band".
;;
;;   Associates compiler notes/warnings with sexps in the source
;;   buffer. The text of each compiler note is annotated on the
;;   appropriate sexp in the buffer and the sexp is visually
;;   highlighted. Commands exist for navigating between compiler
;;   notes.
;;
;;   Comforts familiar from ILISP: completion of symbols, automatic
;;   display of arglists in function calls, and TAGS-like definition
;;   finding.
;;
;; The goal is to make Emacs support CMU Common Lisp as well as it
;; supports Emacs Lisp. The strategy is to take maximum advantage of
;; all CMUCL features and hooks, portability be damned to hades.
;;
;; Compatibility with other Common Lisps is not a goal.
;;
;; Compatibility with GNU Emacs 20 and 21 and XEmacs 21 is a goal. It
;; is not achieved yet, but will be required for all public
;; releases. Meanwhile we support GNU Emacs 21.


;;; Declarations of dependencies, constants, and variables.
(require 'inf-lisp)
(require 'cmucl-wire)

(when (featurep 'xemacs)
  (require 'overlay)
  (defun next-single-char-property-change (&rest args)
    (or (apply 'next-single-property-change args)
        (point-max)))
  (defun previous-single-char-property-change (&rest args)
    (or (apply 'previous-single-property-change args)
        (point-min))))

(unless (fboundp 'define-minor-mode)
  (require 'easy-mmode)
  (defalias 'define-minor-mode 'easy-mmode-define-minor-mode))

(unless (fboundp 'next-single-char-property-change)
  (defun next-single-char-property-change (position prop &optional
						    object limit)
    (if (stringp object)
	(or (next-single-property-change position prop object limit)
	    limit 
	    (length object))
      (with-current-buffer (or object (current-buffer))
	(let ((initial-value (get-char-property position prop object))
	      (limit (or limit (point-max))))
	  (loop for pos = position then (next-char-property-change pos limit)
		if (>= pos limit) return limit
		if (not (eq initial-value 
			    (get-char-property pos prop object))) 
		return pos))))))

(unless (fboundp 'previous-single-char-property-change)
  (defun previous-single-char-property-change (position prop &optional
							object limit)
    (if (stringp object)
	(or (previous-single-property-change position prop object limit)
	    limit 
	    (length object))
      (with-current-buffer (or object (current-buffer))
	(let ((initial-value (get-char-property position prop object))
	      (limit (or limit (point-min))))
	  (if (<= position limit)
	      limit
	    (loop for pos = position then 
		  (previous-char-property-change pos limit)
		  if (<= pos limit) return limit
		  if (not (eq initial-value 
			      (get-char-property pos prop object))) 
		  return pos)))))))

(defconst slime-swank-port 4004
  "TCP port number for the Lisp Swank server.")

(defvar slime-wire nil
  "Process (socket) connected to the Swank server.")

(defvar slime-path
  (let ((path (locate-library "slime")))
    (and path (file-name-directory path)))
  "Directory containing the Slime package.
This is used to load the supporting Common Lisp library, Swank.
The default value is automatically computed from the location of the
Emacs Lisp package.")

(defvar slime-swank-connection-retries 10
  "Number of times to try connecting to the Swank server before aborting.")

(defvar slime-cmucl-binary-extension ".x86f")

(defconst slime-wire-success-code 42)
(defconst slime-wire-condition-code 57)
(defconst slime-wire-internal-error-code 56)

(make-variable-buffer-local
 (defvar slime-buffer-package nil
   "The Lisp package associated with the current buffer.
Don't access this value directly in a program. Call the function with
the same name instead."))


;;; Custom.

(defgroup slime nil
  "Interfaction with the Superior Lisp Environment."
  :prefix "slime-"
  :group 'applications)

(defface slime-error-face
  '((((class color) (background light))
     (:underline "red"))
    (((class color) (background dark))
     (:underline "red"))
    (t (:underline t)))
  "Face for errors from the compiler."
  :group 'slime)

(defface slime-warning-face
  '((((class color) (background light))
     (:underline "orange"))
    (((class color) (background dark))
     (:underline "coral"))
    (t (:underline t)))
  "Face for warnings from the compiler."
  :group 'slime)

(defface slime-note-face
  '((((class color) (background light))
     (:underline "brown"))
    (((class color) (background dark))
     (:underline "gold"))
    (t (:underline t)))
  "Face for notes from the compiler."
  :group 'slime)

(defface slime-highlight-face
  '((t
     (:inherit highlight)
     (:underline nil)))
  "Face for compiler notes while selected."
  :group 'slime)


;;; Minor mode.

(define-minor-mode slime-mode
    "\\<slime-mode-map>\
The Superior Lisp Interaction Mode, Extended (minor-mode).

Compilation commands compile the current buffer's source file and
visually highlight any resulting compiler notes and warnings:
\\[slime-compile-and-load-file]	- Compile and load the current buffer's file.
\\[slime-compile-file]	- Compile (but not load) the current buffer's file.
\\[slime-compile-defun]	- Compile the top-level form at point.

Commands for visiting compiler notes:
\\[slime-next-note]	- Goto the next form with a compiler note.
\\[slime-previous-note]	- Goto the previous form with a compiler note.
\\[slime-remove-notes]	- Remove compiler-note annotations in buffer.

Finding definitions:
\\[slime-edit-fdefinition]	- Edit the definition of the function called at point.
\\[slime-pop-find-definition-stack]	- Pop the definition stack to go back from a definition.

Programming aids:
\\[slime-complete-symbol]	- Complete the Lisp symbol at point. (Also M-TAB.)
\\[slime-macroexpand-1]	- Macroexpand once.
\\[slime-macroexpand-all]	- Macroexpand all.

Documentation commands:
\\[slime-describe-symbol]:	Describe symbol.
\\[slime-apropos]:	Apropos search.
\\[slime-diassemble]: Disassemble a function.

Evaluation commands:
\\[slime-eval-defun]	- Evaluate top-level from containing point.
\\[slime-eval-last-expression]	- Evaluate sexp before point.
\\[slime-pprint-eval-list-expression]	- Evaluate sexp before point, pretty-print result.

\\{slime-mode-map}"
  nil
  nil
  '((" "        . slime-space)
    ("\M-p"     . slime-previous-note)
    ("\M-n"     . slime-next-note)
    ("\C-c\M-c" . slime-remove-notes)
    ("\C-c\C-k" . slime-compile-and-load-file)
    ("\C-c\M-k" . slime-compile-file)
    ("\C-c\C-c" . slime-compile-defun)
    ;; Multiple bindings for completion, since M-TAB is often taken by
    ;; the window manager.
    ("\M-\C-i"  . slime-complete-symbol)
    ("\C-c\C-i" . slime-complete-symbol)
    ("\M-."     . slime-edit-fdefinition)
    ("\M-,"     . slime-pop-find-definition-stack)
    ("\C-x\C-e" . slime-eval-last-expression)
    ("\C-c\C-p" . slime-pprint-eval-last-expression)
    ("\M-\C-x"  . slime-eval-defun)
    ("\C-c\C-d" . slime-describe-symbol)
    ("\C-c\M-d" . slime-disassemble-symbol)
    ("\C-c\C-a" . slime-apropos)
    ([(control c) (control m)] . slime-macroexpand-1)
    ([(control c) (meta m)]    . slime-macroexpand-all)
    ))

;; Setup the mode-line to say when we're in slime-mode, and which CL
;; package we think the current buffer belongs to.
(add-to-list 'minor-mode-alist
             '(slime-mode
               (" Slime" (slime-buffer-package
                          (":" slime-buffer-package)
                          ""))))


;;; Setup initial `slime-mode' hooks.

(make-variable-buffer-local
 (defvar slime-pre-command-actions nil
   "List of functions to execute before the next Emacs command.
This list of flushed between commands."))

(defun slime-pre-command-hook ()
  "Execute all functions in `slime-pre-command-actions', then NIL it."
  (dolist (undo-fn slime-pre-command-actions)
    (ignore-errors (funcall undo-fn)))
  (setq slime-pre-command-actions nil))

(defun slime-setup-pre-command-hook ()
  "Setup a buffer-local `pre-command-hook' to call `slime-pre-command-hook'."
  (make-local-variable 'pre-command-hook)
  (add-hook 'pre-command-hook 'slime-pre-command-hook))

(add-hook 'slime-mode-hook 'slime-setup-pre-command-hook)
(add-hook 'slime-mode-hook 'slime-buffer-package)



;;; CMUCL Setup: compiling and connecting to Swank.

;; SLIME -- command
;;
(defun slime ()
  "Start an inferior^_superior Lisp and connect to its Swank server."
  (interactive)
  (call-interactively 'inferior-lisp)
  (slime-start-swank-server)
  (slime-connect "localhost" slime-swank-port))

;; SLIME-CONNECT -- command
;;
(defun slime-connect (host port &optional retries)
  "Connect to a running Swank server."
  (interactive "sHost: \nnPort: ")
  (setq retries (or retries slime-swank-connection-retries))
  (if (zerop retries)
      (error "Unable to contact Swank server.")
      (condition-case ()
          (progn 
	    (setq slime-wire (wire-connect-to-remote-server 
			      host port 
			      'slime-take-input 
			      'slime-wire-error))
	    (slime-init-dispatcher)
	    (message "Connected to Swank on %s:%S. %s"
		     host port (slime-random-words-of-encouragement)))
        (wire-error (message "Connecting to Swank (%S attempts remaining)." 
                             retries)
                    (sit-for 1)
                    (slime-connect host port (1- retries))))))

(defun slime-start-swank-server ()
  "Start a Swank server on the inferior lisp."
  (slime-maybe-compile-swank)
  (comint-proc-query (inferior-lisp-proc)
                     (format "(load %S)\n" (concat slime-path "swank")))
  (comint-proc-query (inferior-lisp-proc)
                     "(swank:start-server)\n"))

(defun slime-maybe-compile-swank ()
  (let ((source (concat slime-path "swank.lisp"))
        (binary (concat slime-path "swank" slime-cmucl-binary-extension)))
    (flet ((compile-swank ()
             (comint-proc-query (inferior-lisp-proc)
                                (format "(compile-file %S)\n" source))))
      (when (or (and (not (file-exists-p binary))
                     (y-or-n-p "\
The CMUCL support library (Swank) is not compiled. Compile now? "))
                (and (file-newer-than-file-p source binary)
                     (y-or-n-p "\
Your Swank binary is older than the source. Recompile now? ")))
        (compile-swank)))))

(defvar slime-words-of-encouragement
  '("Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    "Your hacking starts... NOW!"
    "May the source be with you!")
  "Scientifically-proven optimal words of hackerish encouragement.")

(defun slime-random-words-of-encouragement ()
  "Return a string of hackerish encouragement."
  (nth (random (length slime-words-of-encouragement))
       slime-words-of-encouragement))


;;; Evaluation mechanics.

(defmacro destructure-case (value &rest patterns)
  (let ((operator (gensym "op-"))
	(operands (gensym "rand-"))
	(tmp (gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (case ,operator
	 ,@(mapcar (lambda (clause)
		     (destructuring-bind ((op &rest rands) &rest body) clause
		       `(,op (destructuring-bind ,rands ,operands
			       . ,body))))
		   patterns)
	 (t (error "destructure-case failed: %S" ,tmp))))))

(defvar slime-saved-state)
(defvar slime-input-queue)
(defvar slime-accepting-input-p)

(defun slime-wire-error (condition)
  (apply #'signal condition))

(defun slime-init-dispatcher ()
  (setq slime-saved-state (list '() 0))
  (setq slime-input-queue '())
  (setq slime-accepting-input-p t))

(defun slime-take-input (object)
  (setq slime-input-queue (nconc slime-input-queue (list object)))
  (slime-process-input))

(defun slime-process-input ()
  (cond (slime-accepting-input-p
	 (setq slime-accepting-input-p nil)
	 (apply #'slime-dispatch-on-input
		(pop slime-input-queue) 
		slime-saved-state))
	(t
	 (message "queuing input from wire"))))

(defun slime-wait-for-input (state)
  (setq slime-accepting-input-p t)
  (setq slime-saved-state state)
  (when slime-input-queue
    (slime-process-input)))

(put 'destructure-case 'lisp-indent-function 1)

(defun slime-dispatch-on-input (object alist counter)
  (destructure-case object
    ((:CALL-CONTINUATION id result)
     (slime-call-continuation id result alist counter))
    ((:DEBUGGER-HOOK old-level)
     (slime-enter-debugger old-level)
     (slime-wait-for-input (list alist counter)))
    ((eval-string-async string package cont)
     (wire-output-funcall 
      slime-wire 'SWANK:EVAL-STRING-ASYNC string package counter)
     (slime-wait-for-input (list (acons counter cont alist) 
				 (1+ counter))))
    ((t &rest message)
     (message "Unkown message: %S" object)
     (slime-wait-for-input (list alist counter)))))

(defun slime-call-continuation (id result alist counter)
  (let ((probe (assq id alist)))
    (funcall (cdr probe) result)
    (slime-wait-for-input (list (delete probe alist) counter))))

(defun slime-enter-debugger (old-level)
  (pop-to-buffer inferior-lisp-buffer t t)
  (enlarge-window (- (/ (frame-height) 2) (window-height)))
  (goto-char (point-max))
  (slime-eval-async 
   '(CL:LET ((CL:*DEBUGGER-HOOK* CL:NIL))
	    (CL:INVOKE-DEBUGGER SWANK::*SWANK-DEBUGGER-CONDITION*))
   nil
   'slime-leave-debugger))

(add-hook 'inferior-lisp-mode-hook
	  (lambda ()
	    (setq comint-scroll-to-bottom-on-input t)
	    (setq comint-scroll-show-maximum-output t)))

(defun slime-leave-debugger (reply)
  (destructure-case reply
    (((:OK :ABORTED) &rest _) (delete-windows-on inferior-lisp-buffer))))

(defun slime-eval-string-async (string package cont)
  (slime-take-input `(eval-string-async ,string ,package ,cont)))

(defun slime-eval-async (sexp package cont)
  "Evaluate EXPR on the superior Lisp and call CONT with the result."
  (slime-eval-string-async (prin1-to-string sexp) package cont))

(defun slime-eval (sexp &optional package)
  "Evaluate EXPR on the superior Lisp and return the result."
  (assert slime-accepting-input-p)
  (lexical-let ((done nil)
		(error nil)
		value)
    (slime-eval-async
     sexp (or package (slime-buffer-package))
     (lambda (reply)
       (setq done t)
       (destructure-case reply
	 ((:OK result) (setq value result))
	 ((:ABORTED) (setq error 'aborted))
	 ((t &rest _) (setq error "Unexpected reply: %S" reply)))))
    (let ((debug-on-quit t))
      (while (not done)
	(accept-process-output)))
    (when error
      (error "slime-eval failed: %S" error))
    value))

(defun slime-downcase-symbols (x)
  "Convert all symbols in the term X to lowercase."
  (cond ((symbolp x) (intern (downcase (symbol-name x))))
        ((consp x)   (mapcar 'slime-downcase-symbols x))
        (t           x)))

(defun slime-buffer-package (&optional dont-cache)
  "Return the Common Lisp package associated with the current buffer.
This is heuristically determined by a text search of the buffer.
The result is cached and returned on subsequent calls unless
DONT-CACHE is non-nil."
  (or (and (not dont-cache) slime-buffer-package)
      (and (setq slime-buffer-package (slime-find-buffer-package))
           (progn (force-mode-line-update) slime-buffer-package))
      "CL-USER"))

(defun slime-find-buffer-package ()
  "Figure out which Lisp package the current buffer is associated with."
  (save-excursion
    (when (let ((case-fold-search t))
	    (re-search-backward "^(\\(cl:\\|common-lisp:\\)?in-package\\>" 
				nil t))
      (goto-char (match-end 0))
      (skip-chars-forward " \n\t\f\r#:")
      (let ((pkg (read (current-buffer))))
	(cond ((stringp pkg)
	       pkg)
	      ((symbolp pkg)
	       (symbol-name pkg)))))))


;;; Compilation and the creation of compiler-note annotations.

;; SLIME-COMPILE-AND-LOAD-FILE -- command
;;
(defun slime-compile-and-load-file ()
  "Compile and load the buffer's file and highlight compiler notes.

Each source location that is the subject of a compiler note is
underlined and annotated with the relevant information. The commands
`slime-next-note' and `slime-previous-note' can be used to navigate
between compiler notes and to display their full details."
  (interactive)
  (slime-compile-file t))

;; SLIME-COMPILE-FILE -- command
;;
(defun slime-compile-file (&optional load)
  "Compile current buffer's file and highlight resulting compiler notes.

See `slime-compile-and-load-file' for further details."
  (interactive)
  (unless (eq major-mode 'lisp-mode)
    (error "Only valid in lisp-mode"))
  (save-some-buffers)
  (slime-eval-async 
   `(CL:PROGN 
     (SWANK:CLEAR-NOTES ,(buffer-file-name))
     (CL:LIST
      (CL:PRIN1-TO-STRING
       (CL:LET ((EXT:*ERROR-PRINT-LENGTH* 8)
		(EXT:*ERROR-PRINT-LEVEL* 6))
	       (CL:COMPILE-FILE
		,(buffer-file-name) :LOAD ,(if load 'CL:T 'CL:NIL))))
      (SWANK:LOOKUP-NOTES ,(buffer-file-name))))
   (slime-buffer-package)
   (lexical-let ((buffer (current-buffer)))
     (lambda (reply) 
       (with-current-buffer buffer
	 (slime-compilation-finished reply)))))
  (message "Compiling %s.." (buffer-file-name)))

(defun slime-defun-at-point ()
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (buffer-substring-no-properties (point) end))))

(defun slime-compile-defun ()
  (interactive)
  (slime-interactive-eval
   (prin1-to-string
   `(CL:LET ((EXT:*ERROR-PRINT-LENGTH* 8)
	     (EXT:*ERROR-PRINT-LEVEL* 6))
	    (SWANK:SWANK-COMPILE-STRING ,(slime-defun-at-point)
					,(buffer-file-name)
					,(save-excursion
					   (beginning-of-defun)
					   (point)))))))

(defun slime-compilation-finished (reply)
  (destructure-case reply
    ((:OK (filename notes))
     (loop for (location severity message context) in notes 
	   count (eq :ERROR severity) into errors
	   count (eq :WARNING severity) into warnings
	   count (eq :NOTE severity) into notes
	   finally 
	   (message 
	    "Compilation finished: %s errors  %s warnings  %s notes"
	    errors warnings notes) ; filename)
	   )
     (slime-highlight-notes notes))
    ((:ABORTED) (message "Compilation aborted"))))

(defun slime-display-buffer-other-window (buffer &optional not-this-window)
  "Display BUFFER in some other window.
Like `display-buffer', but ignores `same-window-buffer-names'."
  (let ((same-window-buffer-names nil))
    (display-buffer buffer not-this-window)))

(defun slime-highlight-notes (notes)
  "Highlight compiler notes, warnings, and errors in the buffer."
  (interactive (list (slime-compiler-notes)))
  (save-excursion
    (slime-remove-old-overlays)
    (dolist (w notes)
      (apply 'slime-overlay-note w))))

(defun slime-compiler-notes ()
  "Return all compiler notes, warnings, and errors."
  (slime-eval `(SWANK:LOOKUP-NOTES ,(buffer-file-name))))

(defun slime-remove-old-overlays ()
  "Delete the existing Slime overlays in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (goto-char (next-overlay-change (point)))
      (dolist (o (overlays-at (point)))
        (when (overlay-get o 'slime)
          (delete-overlay o))))))


;;;;; Adding a single compiler note.

(defun slime-overlay-note (location severity message context)
  "Add a compiler note to the buffer as an overlay.
If an appropriate overlay for a compiler note in the same location
already exists then the new information is merged into it. Otherwise a
new overlay is created."
  (multiple-value-bind (start end) (slime-choose-overlay-region location)
    (goto-char start)
    (let ((appropriate-overlay (slime-note-at-point)))
      (if appropriate-overlay
	  (slime-merge-note-into-overlay appropriate-overlay severity message)
	  (slime-create-note-overlay location start end severity message)))))

(defun slime-create-note-overlay (location start end severity message)
  "Create an overlay representing a compiler note.
The overlay has several properties:
  FACE       - to underline the relevant text.
  SEVERITY   - for future reference, :NOTE, :WARNING, or :ERROR.
  MOUSE-FACE - highlight the note when the mouse passes over.
  HELP-ECHO  - a string describing the note, both for future reference
               and for display as a tooltip (due to the special
               property name)."
  (let ((overlay (make-overlay start end)))
    (flet ((putp (name value) (overlay-put overlay name value)))
      (putp 'slime location)
      (putp 'priority (slime-sexp-depth start))
      (putp 'face (slime-severity-face severity))
      (putp 'severity severity)
      (putp 'mouse-face 'highlight)
      (putp 'help-echo message)
      overlay)))

(defun slime-sexp-depth (position)
  "Return the number of sexps containing POSITION."
  (let ((n 0))
    (save-excursion
      (goto-char position)
      (ignore-errors
        (while t
          (backward-up-list 1)
          (incf n))))
    n))

(defun slime-merge-note-into-overlay (overlay severity message)
  "Merge another compiler note into an existing overlay.
The help text describes both notes, and the highest of the severities
is kept."
  (flet ((putp (name value) (overlay-put overlay name value))
	 (getp (name)       (overlay-get overlay name)))
    (putp 'severity (slime-most-severe severity (getp 'severity)))
    (putp 'face (slime-severity-face (getp 'severity)))
    (putp 'help-echo (concat (getp 'help-echo) "\n;;\n" message))))

(defun slime-choose-overlay-region (location)
  "Choose the start and end points for an overlay over LOCATION.
If the location's sexp is a list spanning multiple lines, then the
region around the first element is used."
  (slime-goto-location location)
  (let ((start (point)))
    (ignore-errors (backward-up-list 1))
    (slime-forward-sexp)
    (when (eq (char-before) ?\)) (backward-char))
    (values start
	    (if (slime-same-line-p start (point))
		(point)
		(progn (goto-char start)
		       (forward-sexp)
		       (point))))))

(defun slime-same-line-p (start end)
  "Return true if buffer positions START and END are on the same line."
  (save-excursion (goto-char start)
                  (not (search-forward "\n" end t))))

(defun slime-severity-face (severity)
  "Return the name of the font-lock face representing SEVERITY."
  (ecase severity
    (:ERROR   'slime-error-face)
    (:WARNING 'slime-warning-face)
    (:NOTE    'slime-note-face)))

(defun slime-most-severe (sev1 sev2)
  "Return the most servere of two conditions.
Severity is ordered as :NOTE < :WARNING < :ERROR."
  (if (or (eq sev1 :error)              ; Well, not exactly Smullyan..
          (and (eq sev1 :warning)
               (not (eq sev2 :error))))
      sev1
      sev2))

(defun slime-goto-location (location)
  "Goto the source location by position or source path.
A source-path is a list of the form (1 2 3 4), which indicates a
position in a file in terms of sexp positions. The first number
identifies the top-level form that contains the position that we wish
to move to: the first top-level form has number 0. The second number
in the source-path identifies the containing sexp within that
top-level form, etc."
  (interactive)
  (cond ((integerp location)            ; it's a character position
         (goto-char location)
         (backward-sexp)
         (forward-char))
        ((listp location)               ; it's a source-form
         (goto-char (point-min))
         (dolist (sexps location)
           (slime-forward-sexp sexps)
           (ignore-errors (slime-forward-sexp))
           (backward-sexp)
           (when (looking-at "(")
             (forward-char))))
        (t
         (error "Unknown location type %s" location))))

(defun slime-forward-sexp (&optional count)
  "Like `forward-sexp', but steps over reader-conditionals (#- and #+)."
  (dotimes (i (or count 1))
    (forward-sexp)
    (backward-sexp)
    (when (or (looking-at "#+")
              (looking-at "#-"))
      (forward-sexp))
    (forward-sexp)))


;;;;; Visiting and navigating the overlays of compiler notes.

;; SLIME-NEXT-NOTE -- command
;;
(defun slime-next-note ()
  "Go to and describe the next compiler note in the buffer."
  (interactive)
  (slime-find-next-note)
  (if (slime-note-at-point)
      (slime-show-note (slime-note-at-point))
      (message "No next note.")))

;; SLIME-PREVIOUS-NOTE -- command
;;
(defun slime-previous-note ()
  "Go to and describe the previous compiler note in the buffer."
  (interactive)
  (slime-find-previous-note)
  (if (slime-note-at-point)
      (slime-show-note (slime-note-at-point))
      (message "No previous note.")))

;; SLIME-REMOVE-NOTES -- command
;;
(defun slime-remove-notes ()
  "Remove compiler-note annotations from the current buffer."
  (interactive)
  (slime-remove-old-overlays))

(defun slime-show-note (overlay)
  "Present the details of a compiler note to the user."
  (slime-temporarily-highlight-note overlay)
  (slime-message (get-char-property (point) 'help-echo)))

(if (or (featurep 'xemacs)
	(= emacs-major-version 20))
    ;; XEmacs truncates multi-line messages in the echo area.
    (defun slime-message (fmt &rest args)
      (slime-display-message-or-view (format fmt args) "*CMUCL Note*"))
    (defun slime-message (fmt &rest args)
      (apply 'message fmt args)))


(defun slime-display-message-or-view (msg bufname &optional select)
  "Like `display-buffer-or-message', but with `view-buffer-other-window'.
That is, if a buffer pops up it will be in view mode, and pressing q
will get rid of it.

Only uses the echo area for single-line messages - or more accurately,
messages without embedded newlines. They may still need to wrap or
truncate to fit on the screen."
  (if (string-match "\n.*[^\\s-]" msg)
      ;; Contains a newline with actual text after it, so display as a
      ;; buffer
      (with-current-buffer (get-buffer-create bufname)
	(setq buffer-read-only t)
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (insert msg)
	  (goto-char (point-min))
	  (let ((win (display-buffer (current-buffer))))
	    (slime-display-buffer-region (current-buffer) 
					 (point-min) (point-max))
	    (when select (select-window win)))))
    ;; Print only the part before the newline (if there is
    ;; one). Newlines in messages are displayed as "^J" in emacs20,
    ;; which is ugly
    (string-match "[^\r\n]*" msg)
    (message (match-string 0 msg))))

(defun slime-temporarily-highlight-note (overlay)
  "Temporarily highlight a compiler note's overlay.
The highlighting is designed to both make the relevant source more
visible, and to highlight any further notes that are nested inside the
current one.

The highlighting is automatically undone before the next Emacs command."
  (let ((old-face (overlay-get overlay 'face)))
    (push `(lambda () (overlay-put ,overlay 'face ',old-face))
	  slime-pre-command-actions)
    (overlay-put overlay 'face 'slime-highlight-face)))


;;;;; Overlay lookup operations.

(defun slime-note-at-point ()
  "Return the overlay for a note starting at point, otherwise NIL."
  (find (point) (slime-note-overlays-at-point)
	:key 'overlay-start))

(defun slime-note-overlay-p (overlay)
  "Return true if OVERLAY represents a compiler note."
  (overlay-get overlay 'slime))

(defun slime-note-overlays-at-point ()
  "Return a list of all note overlays that are under the point."
  (remove-if-not 'slime-note-overlay-p (overlays-at (point))))

(defun slime-find-next-note ()
  "Go to the next position with the `slime-note' text property.
Retuns true if such a position is found."
  (slime-find-note 'next-single-char-property-change))

(defun slime-find-previous-note ()
  "Go to the next position with the `slime' text property.
Returns true if such a position is found."
  (slime-find-note 'previous-single-char-property-change))

(defun slime-find-note (next-candidate-fn)
  "Seek out the beginning of a note.
NEXT-CANDIDATE-FN is called to find each new position for consideration."
  (let ((origin (point)))
    (loop do (goto-char (funcall next-candidate-fn (point) 'slime))
	  until (or (slime-note-at-point)
		    (eobp)
		    (bobp)))
    (unless (slime-note-at-point)
      (goto-char origin))))


;;; Documentation.

;; SLIME-SPACE -- command
;;
(defun slime-space ()
  "Insert a space and print some relevant information (function arglist).
Designed to be bound to the SPC key."
  (interactive)
  (insert " ")
  (when (slime-function-called-at-point/line)
    (slime-arglist (symbol-name (slime-function-called-at-point/line)))))

(defun slime-arglist (symbol-name)
  "Show the argument list for the nearest function call, if any."
  (interactive (list (read-string "Arglist of: "
                                  (let ((sym (symbol-at-point)))
                                    (and sym (symbol-name sym))))))
  (slime-eval-async 
   `(SWANK:ARGLIST-STRING ',(intern symbol-name))
   (slime-buffer-package)
   (lexical-let ((symbol-name symbol-name))
     (lambda (result)
       (destructure-case result
	 ((:OK arglist) 
	  (message "(%s %s)" symbol-name (substring arglist 1 -1)))
	 ((t &rest error) (message "slime error: %S" error)))))))

(defun slime-function-called-at-point/line ()
  "Return the name of the function being called at point, provided the
function call starts on the same line at the point itself."
  (and (ignore-errors
         (slime-same-line-p (save-excursion (backward-up-list 1) (point))
                            (point)))
       (slime-function-called-at-point)))

;; What a tragedy!
;;
;; Twenty-nine whole lines of code copy-and-yanked from help.el's
;; `function-called-at-point', all because it only considers symbols
;; that are fbound in Emacs!
(defun slime-function-called-at-point ()
  "Return a function around point or else called by the list containing point.
If that doesn't give a function, return nil."
  (or (condition-case ()
	  (save-excursion
	    (or (not (zerop (skip-syntax-backward "_w")))
		(eq (char-syntax (following-char)) ?w)
		(eq (char-syntax (following-char)) ?_)
		(forward-sexp -1))
	    (skip-chars-forward "'")
	    (let ((obj (read (current-buffer))))
              ;; FIXME: fboundp?
	      (and (symbolp obj) (fboundp obj) obj)))
	(error nil))
      (condition-case ()
	  (save-excursion
	    (save-restriction
	      (narrow-to-region (max (point-min)
				     (- (point) 1000)) (point-max))
	      ;; Move up to surrounding paren, then after the open.
	      (backward-up-list 1)
	      (forward-char 1)
	      ;; If there is space here, this is probably something
	      ;; other than a real Lisp function call, so ignore it.
	      (if (looking-at "[ \t]")
		  (error "Probably not a Lisp function call"))
	      (let ((obj (read (current-buffer))))
		(and (symbolp obj) obj))))
	(error nil))))


;;; Completion.

(defun slime-complete-symbol ()
  "Complete the symbol at point.
If the symbol lacks an explicit package prefix, the current buffer's
package is used."
  ;; NB: It is only the name part of the symbol that we actually want
  ;; to complete -- the package prefix, if given, is just context.
  (interactive)
  (let* ((end (point))
         (beg (slime-symbol-start-pos))
         ;; Beginning of symbol name (i.e. after package if present).
         (name-beg (save-excursion
                     (if (search-backward ":" beg t)
                         (1+ (point))
                         beg)))
         (whole-prefix (buffer-substring-no-properties beg end))
         (name-prefix  (buffer-substring-no-properties name-beg end))
         (completions (slime-completions whole-prefix))
         (completions-alist (slime-bogus-completion-alist completions))
         (completion (try-completion name-prefix completions-alist nil)))
    (cond ((eq completion t))
          ((null completion)
           (message "Can't find completion for \"%s\"" whole-prefix)
           (ding))
          ((not (string= name-prefix completion))
           (delete-region name-beg end)
           (insert completion))
          (t
           (message "Making completion list...")
           (let ((list (all-completions name-prefix completions-alist nil)))
             (with-output-to-temp-buffer "*Completions*"
               (display-completion-list list)))
           (message "Making completion list...done")))))

(defun slime-symbol-start-pos ()
  "Return the starting position of the symbol under point.
The result is unspecified if there isn't a symbol under the point."
  (save-excursion
    (backward-sexp 1)
    (skip-syntax-forward "'")
    (point)))

(defun slime-bogus-completion-alist (list)
  "Make an alist out of list.
The same elements go in the CAR, and nil in the CDR. To support the
apparently very stupid `try-completions' interface, that wants an
alist but ignores CDRs."
  (mapcar (lambda (x) (cons x nil)) list))

(defun slime-completions (prefix)
  (when (stringp prefix)
    (setq prefix (intern prefix)))
  (let ((package (upcase (wire-symbol-package prefix (slime-buffer-package))))
        (name (upcase (wire-symbol-name prefix)))
        (external-ref (wire-symbol-external-ref-p prefix))
        (has-upcase (let ((case-fold-search nil))
                      (string-match "[A-Z]" (symbol-name prefix)))))
    (mapcar (if has-upcase 'upcase 'downcase)
            (slime-eval `(swank:completions ,name ,package ,external-ref)))))


;;; Edit definition

(defvar slime-find-definition-history-ring (make-ring 20)
  "History ring recording the definition-finding \"stack\".")

(defun slime-inferior-lisp-marker-position ()
  (marker-position (process-mark (get-buffer-process inferior-lisp-buffer))))

(defun slime-edit-fdefinition (name)
  "Lookup the definition of the function called at point.
If no function call is recognised, or a prefix argument is given, then
the function name is prompted."
  (interactive (list (let ((called (slime-function-called-at-point)))
                       (if (and called (null current-prefix-arg))
                           (symbol-name called)
                         (read-string "Function name: ")))))
  (let* ((package (upcase (wire-symbol-package name (slime-buffer-package))))
         (file (slime-eval `(swank:find-fdefinition ,name ,package))))
    (if (null file)
        (message "Cannot locate definition of %S" name)
      (ring-insert-at-beginning slime-find-definition-history-ring
                                (point-marker))
      (find-file file)
      (goto-char (point-min))
      (let ((regexp (format "(\\(defun\\|defmacro\\)\\s *%s\\s "
                            (regexp-quote (wire-symbol-name name)))))
        (if (re-search-forward regexp nil t)
            (progn (beginning-of-line)
                   (unless (pos-visible-in-window-p)
                     (recenter 4)))
          (message "Unable to find definition by searching."))))))

(defun slime-pop-find-definition-stack ()
  (interactive)
  (unless (ring-empty-p slime-find-definition-history-ring)
    (let* ((marker (ring-remove slime-find-definition-history-ring))
	   (buffer (marker-buffer marker)))
      (if (buffer-live-p buffer)
	  (progn (switch-to-buffer buffer)
		 (goto-char (marker-position marker)))
        ;; If this buffer was deleted, recurse to try the next one
        (slime-pop-find-definition-stack)))))


;;; Interactive evaluation.

(defun slime-interactive-eval (string)
  (with-current-buffer inferior-lisp-buffer
    (goto-char (point-max))
    (insert "\n--------------------------------------------------------\n")
    (set-marker (process-mark (get-buffer-process (current-buffer))) (point)))
  (delete-windows-on inferior-lisp-buffer)
  (slime-eval-async
   `(CL:FORMAT CL:NIL "~{~S~^, ~}"
	       (CL:MULTIPLE-VALUE-LIST
		(CL:EVAL (CL:READ-FROM-STRING ,string))))
   (slime-buffer-package t)
   (lexical-let ((output-start (slime-inferior-lisp-marker-position)))
     (lambda (reply)
       (slime-show-evaluation-result output-start reply)))))

(defun slime-display-buffer-region (buffer start end &optional border)
  (let ((border (or border 0)))
    (with-current-buffer buffer
      (save-selected-window
	(save-excursion
	  (unless (get-buffer-window buffer)
	    (display-buffer buffer t))
	  (goto-char start)
	  (when (eolp) 
	    (forward-char))
	  (beginning-of-line)
	  (let ((win (get-buffer-window buffer)))
	    ;; set start before select to force update.
	    ;; (set-window-start sets a "modified" flag, but only if the
	    ;; window is not selected.)
	    (set-window-start win (point))
	    (let* ((lines (max (count-lines (point) end) 1))
		   (new-height (1+ (min (/ (frame-height) 2)
					(+ border lines))))
		   (diff (- new-height (window-height win))))
	      (let ((window-min-height 1))
		(select-window win)
		(enlarge-window diff)))))))))

(defun slime-show-evaluation-result (output-start reply)
  (destructure-case reply
    ((:OK value) 
     (message "=> %s" value)
     (when (< output-start (slime-inferior-lisp-marker-position))
       (slime-display-buffer-region 
	inferior-lisp-buffer 
	output-start (slime-inferior-lisp-marker-position) 1)))
    ((:ABORTED) (message "Evalution aborted"))
    ((:READ-FAILED message) (message "%s" message))))

(defun slime-last-expression ()
  (buffer-substring-no-properties (save-excursion (backward-sexp) (point))
				  (point)))

(defun slime-eval-last-expression ()
  (interactive)
  (slime-interactive-eval (slime-last-expression)))

(defun slime-eval-defun ()
  (interactive)
  (slime-interactive-eval (slime-defun-at-point)))

(defun slime-pprint-eval-last-expression ()
  (interactive)
  (slime-eval-async 
   `(CL:LET ((CL:*PRINT-PRETTY* CL:T)
	     (CL:*PRINT-CIRCLE* CL:T)
	     (CL:*PRINT-LEVEL* CL:NIL)
	     (CL:*PRINT-LENGTH* CL:NIL)
	     (EXT:*GC-VERBOSE* CL:NIL))
	    (CL:LET ((SWANK::value 
		      (CL:EVAL (CL:READ-FROM-STRING
				,(slime-last-expression)))))
		    (CL:WITH-OUTPUT-TO-STRING 
		     (CL:*STANDARD-OUTPUT*)
		     (CL:PPRINT SWANK::value))))
   (slime-buffer-package t)
   'slime-show-string-result))

;;; 

(defun slime-show-description (string)
  (save-current-buffer
    (with-output-to-temp-buffer "*Help*"
      (princ string))))

(defun slime-show-string-result (reply)
  (destructure-case reply
    ((:OK string) (slime-show-description string))
    ((t &rest _) (message "Unexpected reply: %S" reply))))
  
(eval-and-compile 
  (if (fboundp 'substring-no-properties)
      (defalias 'slime-substring-no-properties 'substring-no-properties)
    (defun slime-substring-no-properties (string &optional start end)
      (let* ((start (or start 0))
	     (end (or end (length string)))
	     (string (substring string start end)))
	(set-text-properties start end nil string)
	string))))

(defun slime-eval-describe (form package)
  (slime-eval-async
   `(CL:WITH-OUTPUT-TO-STRING 
     (CL:STREAM) 
     (CL:DESCRIBE ,form CL:STREAM))
   (or package (slime-buffer-package t))
   'slime-show-string-result))
  
(defun slime-describe-symbol (symbol-name &optional package)
  (interactive (lisp-symprompt "Describe" (thing-at-point 'symbol)))
  (slime-eval-describe `(CL:READ-FROM-STRING 
			 ,(slime-substring-no-properties symbol-name))
		       package))

(defun slime-disassemble-symbol (symbol-name)
  (interactive (list (slime-substring-no-properties (thing-at-point 'symbol))))
  (slime-eval-async 
   `(CL:WITH-OUTPUT-TO-STRING 
     (CL:*STANDARD-OUTPUT*)
     (CL:DISASSEMBLE (CL:READ-FROM-STRING ,symbol-name)))
   (slime-buffer-package t)
   'slime-show-string-result))

;;(defun slime-apropos (string)
;;  (interactive "sSlime apropos: ")
;;  (slime-eval-async
;;   `(CL:WITH-OUTPUT-TO-STRING (CL:*STANDARD-OUTPUT*) (CL:APROPOS ,string))
;;   (slime-buffer-package t)
;;   'slime-show-string-result))

(defun slime-apropos (string)
  (interactive "sSlime apropos: ")
  (slime-eval-async
   `(SWANK::APROPOS-LIST-FOR-EMACS ,string)
   (slime-buffer-package t)
   'slime-show-apropos))

;; Buffer-local variable. Holds the name of *package* when apropos was
;; apropos was called.
(defvar slime-apropos-package)

(defun slime-show-apropos (reply)
  (destructure-case reply
    ((:OK (package plists))
     (save-current-buffer
       (with-output-to-temp-buffer "*CMUCL Apropos*"
	 (set-buffer standard-output)
	 (apropos-mode)
	 (make-local-variable 'slime-apropos-package)
	 (setq slime-apropos-package package)
         (set (make-local-variable 'truncate-lines) t)
	 (slime-print-apropos plists))))))

(defun slime-princ-propertized (string props)
  (let ((start (point)))
    (princ string)
    (add-text-properties start (point) props)))

(defun slime-print-apropos (plists)
  (dolist (plist plists)
    (let ((designator (plist-get plist :DESIGNATOR)))
      (slime-princ-propertized designator 
			       (list 'face apropos-symbol-face
				     'item designator
				     'action 'slime-apropos-describe-symbol)))
    (terpri)
    (let ((apropos-label-properties 
	   (cond ((and (boundp 'apropos-label-properties) 
		       apropos-label-properties))
		 ((boundp 'apropos-label-face)
		  (typecase apropos-label-face
		    (symbol `(face ,(or apropos-label-face 'italic)
				   mouse-face highlight))
		    (list apropos-label-face))))))
      (loop for (prop namespace action) 
	    in '((:VARIABLE "Variable" slime-describe-symbol)
		 (:FUNCTION "Function" slime-describe-function)
		 (:SETF "Setf" slime-describe-setf-function)
		 (:TYPE "Type" slime-describe-type)
		 (:CLASS "Class" slime-describe-class))
	    do
	    (let ((value (plist-get plist prop))
		  (start (point)))
	      (when value
		(princ "  ") 
		(slime-princ-propertized namespace apropos-label-properties)
		(princ ": ")
		(princ (etypecase value
			 (string value)
			 ((member :NOT-DOCUMENTED) "(not documented)")))
		(put-text-property start (point) 'real-action action)
		(put-text-property start (point) 'action 
				   'slime-apropos-call-action-with-package)
		(terpri)))))))

(defun slime-apropos-describe-symbol (item)
  (slime-describe-symbol item slime-apropos-package))

(defun slime-apropos-call-action-with-package (item)
  (funcall (get-text-property (point) 'real-action) 
	   item slime-apropos-package))

(defun slime-describe-function (name &optional package)
  (slime-eval-describe `(CL:SYMBOL-FUNCTION (CL:READ-FROM-STRING ,name))
		       package))

(defun slime-describe-setf-function (name &optional package)
  (slime-eval-describe 
   `(CL:OR (EXT:INFO SETF INVERSE (CL:READ-FROM-STRING ,name))
	   (EXT:INFO SETF EXPANDER (CL:READ-FROM-STRING ,name)))
   package))

(defun slime-describe-type (name &optional package)
  (slime-eval-describe
   `(KERNEL:VALUES-SPECIFIER-TYPE (CL:READ-FROM-STRING ,name))
   package))

(defun slime-describe-class (name &optional package)
  (slime-eval-describe 
   `(CL:FIND-CLASS (CL:READ-FROM-STRING ,name))
   package))


;;; Macroexpansion

(defun slime-eval-macroexpand (expander)
  (let ((string (slime-substring-no-properties (thing-at-point 'sexp))))
    (slime-eval-async
     `(CL:LET ((CL:*PRINT-PRETTY* CL:T)
	       (CL:*PRINT-LENGTH* 20)
	       (CL:*PRINT-LEVEL* 20))
	      (CL:PRIN1-TO-STRING (,expander (CL:READ-FROM-STRING ,string))))
     (slime-buffer-package t)
     'slime-show-string-result)))

(defun slime-macroexpand-1 (&optional repeatedly)
  (interactive "P")
  (slime-eval-macroexpand
   (if repeatedly 'CL:MACROEXPAND 'CL:MACROEXPAND-1)))

(defun slime-macroexpand-all ()
  (interactive)
  (slime-eval-macroexpand 'WALKER:MACROEXPAND-ALL))

(run-hooks 'slime-load-hook)

(provide 'slime)

;;; Test suite.

(defvar slime-tests '()
  "Names of test functions.")

(defun slime-run-tests ()
  (interactive)
  (save-window-excursion
    (loop for (function inputs) in slime-tests
          do (dolist (input inputs)
               (apply function input))))
  (message "Done."))


(defmacro def-slime-test (name args doc inputs &rest body)
  (let ((fname (intern (format "slime-test-%s" name))))
    `(progn
      (defun ,fname ,args
        ,doc
        ,@body)
      (setq slime-tests (append (remove* ',fname slime-tests :key 'car)
                                (list (list ',fname ,inputs)))))))

(defmacro slime-check (test-name &rest body)
  `(unless (progn ,@body)
     (message "Test failed: %S" ',test-name)
     (debug)))

(def-slime-test find-definition (name expected-filename)
    "Find the definition of a function or macro."
    '((list "list.lisp")
      (loop "loop.lisp")
      (aref "array.lisp"))
  (let ((orig-buffer (current-buffer))
        (orig-pos (point)))
    (slime-edit-fdefinition (symbol-name name))
    ;; Postconditions
    (slime-check correct-file
      (string= (file-name-nondirectory (buffer-file-name))
               expected-filename))
    (slime-check looking-at-definition
      (looking-at (format "(\\(defun\\|defmacro\\)\\s *%s\\s " name)))
    (slime-pop-find-definition-stack)
    (slime-check return-from-definition
      (and (eq orig-buffer (current-buffer))
           (= orig-pos (point))))))

(def-slime-test complete-symbol (prefix expected-completions)
    "Find the completions of a symbol-name prefix."
    '(("cl:compile" ("compile" "compile-file" "compile-file-pathname"
                     "compiled-function" "compiled-function-p"
                     "compiler-macro" "compiler-macro-function"))
      ("cl:foobar" nil)
      ("cl::compile-file" ("compile-file" "compile-file-pathname")))
  (let ((completions (slime-completions prefix)))
    (slime-check expected-completions
      (equal expected-completions (sort completions 'string<)))))

(put 'def-slime-test 'lisp-indent-function 4)
(put 'slime-check 'lisp-indent-function 1)

;;; slime.el ends here
