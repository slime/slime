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
;;   Comforts familiar from ILISP: completion of symbols, and
;;   automatic display of arglists in function calls.
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

(defvar slime-swank-connection-retries 5
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
    "
The Superior Lisp Interaction Mode, Extended (minor-mode).

Compilation commands compile the current buffer's source file and
visually highlight any resulting compiler notes and warnings:
\\[slime-compile-and-load-file]	- Compile and load the current buffer's file.
\\[slime-compile-file]	- Compile (but not load) the current buffer's file.

Commands for visiting compiler notes:
\\[slime-next-note]	- Goto the next form with a compiler note.
\\[slime-previous-note]	- Goto the previous form with a compiler note.
\\[slime-remove-notes]	- Get rid of any compiler-note annotations in the buffer.

Other commands:
\\[slime-complete-symbol]       - Complete the Lisp symbol at point. (Also M-TAB.)
"
  nil
  nil
  '((" "        . slime-space)
    ("\M-p"     . slime-previous-note)
    ("\M-n"     . slime-next-note)
    ("\C-c\C-k" . slime-compile-and-load-file)
    ("\C-c\M-k" . slime-compile-file)
    ;; Multiple bindings for completion, since M-TAB is often taken by
    ;; the window manager.
    ("\M-\C-i"  . slime-complete-symbol)
    ("\C-c\C-i" . slime-complete-symbol)))

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
          (progn (setq slime-wire (wire-connect-to-remote-server host port))
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

(defun slime-eval (expr &optional package)
  "Evaluate EXPR on the superior Lisp and return the result."
  (when (null package)
    (setq package (slime-buffer-package)))
  (multiple-value-bind (status condition result)
      (wire-remote-eval slime-wire (prin1-to-string expr) package)
    (if (eql status slime-wire-success-code)
        (slime-downcase-symbols (car (read-from-string result)))
        (error "slime-eval failed: %S" condition))))

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
           (prog1 t (force-mode-line-update)))
      "CL-USER"))

(defun slime-find-buffer-package ()
  "Figure out which Lisp package the current buffer is associated with."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^(in-package\\>" nil t)
      (ignore-errors
        (let ((pkg (read (current-buffer))))
          (cond ((stringp pkg)
                 pkg)
                ((keywordp pkg)
                 (upcase (substring (symbol-name pkg) 1)))))))))


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
  (wire-output-funcall slime-wire
		       'SWANK:SWANK-COMPILE-FILE
		       (buffer-file-name) (prin1-to-string load))
  (message "Compiling %s.." (buffer-file-name))
  (let ((status    (wire-get-object slime-wire))
        (condition (wire-get-object slime-wire))
        (result    (wire-get-object slime-wire)))
    (slime-highlight-notes)
    (cond ((eql status slime-wire-success-code)
	   (message "OK: %s" (caar (read-from-string result))))
	  ((eql status slime-wire-condition-code)
	   (message "Failed: %s" condition))
	  ((eql status slime-wire-internal-error-code)
	   (slime-display-buffer-other-window inferior-lisp-buffer t)
	   (message "\
Internal Swank error! (Who's the swanker that wrote this code, anyway?)"
		    (buffer-file-name))))))

(defun slime-display-buffer-other-window (buffer &optional not-this-window)
  "Display BUFFER in some other window.
Like `display-buffer', but ignores `same-window-buffer-names'."
  (let ((same-window-buffer-names nil))
    (display-buffer buffer not-this-window)))

(defun slime-highlight-notes ()
  "Highlight compiler notes, warnings, and errors in the buffer."
  (interactive)
  (save-excursion
    (slime-remove-old-overlays)
    (dolist (w (slime-compiler-notes))
      (apply 'slime-overlay-note w))))

(defun slime-compiler-notes ()
  "Return all compiler notes, warnings, and errors."
  (slime-eval `(swank:lookup-notes ,(buffer-file-name))))

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
	  (slime-create-note-overlay start end severity message)))))

(defun slime-create-note-overlay (start end severity message)
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
          (backward-up-list)
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
    (ignore-errors (backward-up-list))
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
    (:error   'slime-error-face)
    (:warning 'slime-warning-face)
    (:note    'slime-note-face)))

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

(if (featurep 'xemacs)
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
  (let ((arglist (slime-eval `(swank:arglist-string ',(intern symbol-name)))))
    (when arglist
      (message "(%s %s)" symbol-name (substring arglist 1 -1)))))

(defun slime-function-called-at-point/line ()
  "Return the name of the function being called at point, provided the
function call starts on the same line at the point itself."
  (and (ignore-errors
         (slime-same-line-p (save-excursion (backward-up-list) (point))
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
  (with-syntax-table emacs-lisp-mode-syntax-table
    (or (condition-case ()
	    (save-excursion
	      (or (not (zerop (skip-syntax-backward "_w")))
		  (eq (char-syntax (following-char)) ?w)
		  (eq (char-syntax (following-char)) ?_)
		  (forward-sexp -1))
	      (skip-chars-forward "'")
	      (let ((obj (read (current-buffer))))
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
	  (error nil)))))

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
        (has-upcase (let ((case-fold-search nil))
                      (string-match "[A-Z]" (symbol-name prefix)))))
    (mapcar (if has-upcase 'upcase 'downcase)
            (slime-eval `(swank:completions ,name ,package)))))


(provide 'slime)

;;; slime.el ends here
