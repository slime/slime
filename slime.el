;; -*- mode: emacs-lisp; mode: outline-minor; outline-regexp: ";;;;*" -*-
;; slime.el -- Superior Lisp Interaction Mode, Extended
;;; License
;;     Copyright (C) 2003  Eric Marsden, Luke Gorrie, Helmut Eller
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


;;; Commentary

;; This minor mode extends Lisp-Mode with CMUCL-specific features.
;; The features can be summarised thusly:
;;
;;   Separate control channel for communication with CMUCL, similar to
;;   Hemlock. This is used to implement other features, rather than
;;   talking to the Lisp listener "in-band".
;;
;;   Compiler notes and warnings are visually annotated in the source
;;   buffer. Commands are provided for inspecting and navigating
;;   between notes.
;;
;;   Comforts familiar from ILISP and Emacs Lisp: completion of
;;   symbols, automatic display of arglists in function calls,
;;   TAGS-like definition finding, Lisp evaluation, online
;;   documentation, etc.
;;
;;   Common Lisp debugger interface.
;;
;; The goal is to make Emacs support CMU Common Lisp as well as it
;; supports Emacs Lisp. The strategy is to take maximum advantage of
;; all CMUCL features and hooks, portability be damned to hades.
;;
;; Compatibility with other Common Lisps is not an immediate
;; goal. SBCL support would be highly desirable in the future.
;;
;; SLIME is compatible with GNU Emacs 20 and 21, and XEmacs 21.
;; Development copies may have temporary have portability bugs.


;;; Dependencies, major global variables and constants

(require 'inf-lisp)
(require 'cl)
(require 'hyperspec)
(when (featurep 'xemacs)
  (require 'overlay))

(defconst slime-swank-port 4005
  "TCP port number for the Lisp Swank server.")

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

(make-variable-buffer-local
 (defvar slime-buffer-package nil
   "The Lisp package associated with the current buffer.
Don't access this value directly in a program. Call the function with
the same name instead."))


;;; Customize group

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


;;; Minor mode

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
\\[slime-disassemble-symbol]:	Disassemble a function.

Evaluation commands:
\\[slime-eval-defun]	- Evaluate top-level from containing point.
\\[slime-eval-last-expression]	- Evaluate sexp before point.
\\[slime-pprint-eval-last-expression]	- Evaluate sexp before point, pretty-print result.

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
    ("\C-c\C-l" . slime-load-file)
    ;; Multiple bindings for completion, since M-TAB is often taken by
    ;; the window manager.
    ("\M-\C-i"  . slime-complete-symbol)
    ("\C-c\C-i" . slime-complete-symbol)
    ("\M-."     . slime-edit-fdefinition)
    ("\M-,"     . slime-pop-find-definition-stack)
    ("\C-x\C-e" . slime-eval-last-expression)
    ("\C-c\C-p" . slime-pprint-eval-last-expression)
    ("\M-\C-x"  . slime-eval-defun)
    ("\C-c:"    . slime-interactive-eval)
    ("\C-c\C-d" . slime-describe-symbol)
    ("\C-c\M-d" . slime-disassemble-symbol)
    ("\C-c\C-t" . slime-toggle-trace-fdefinition)
    ("\C-c\C-a" . slime-apropos)
    ("\C-c\M-a" . slime-apropos-all)
    ([(control c) (control m)] . slime-macroexpand-1)
    ([(control c) (meta m)]    . slime-macroexpand-all)
    ("\C-c\C-g" . slime-interrupt)
    ("\C-c\M-g" . slime-quit)
    ("\C-c\M-0" . slime-restore-window-configuration)
    ("\C-c\C-h" . hyperspec-lookup)
    ("\C-c\C-wc" . slime-who-calls)
    ("\C-c\C-wr" . slime-who-references)
    ("\C-c\C-wb" . slime-who-binds)
    ("\C-c\C-ws" . slime-who-sets)
    ("\C-c\C-wm" . slime-who-macroexpands)
    ))

;; Setup the mode-line to say when we're in slime-mode, and which CL
;; package we think the current buffer belongs to.
(add-to-list 'minor-mode-alist
             '(slime-mode
               (" Slime"
		((slime-buffer-package (":" slime-buffer-package) "")
		 ("|" slime-state-name)))))


;;; Setup initial `slime-mode' hooks

(make-variable-buffer-local
 (defvar slime-pre-command-actions nil
   "List of functions to execute before the next Emacs command.
This list of flushed between commands."))

(defun slime-pre-command-hook ()
  "Execute all functions in `slime-pre-command-actions', then NIL it."
  (dolist (undo-fn slime-pre-command-actions)
    (ignore-errors (funcall undo-fn)))
  (setq slime-pre-command-actions nil))

(defun slime-post-command-hook ()
  (slime-process-available-input))

(defun slime-setup-command-hooks ()
  "Setup a buffer-local `pre-command-hook' to call `slime-pre-command-hook'."
  (make-local-variable 'pre-command-hook)
  (make-local-variable 'post-command-hook)
  (add-hook 'pre-command-hook 'slime-pre-command-hook)
  (add-hook 'post-command-hook 'slime-post-command-hook))

(add-hook 'slime-mode-hook 'slime-setup-command-hooks)
(add-hook 'slime-mode-hook 'slime-buffer-package)

;;; Common utility functions

(defun slime-display-buffer-other-window (buffer &optional not-this-window)
  "Display BUFFER in some other window.
Like `display-buffer', but ignores `same-window-buffer-names'."
  (let ((same-window-buffer-names nil))
    (display-buffer buffer not-this-window)))

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
    (string-match "^[^\r\n]*" msg)
    (message (match-string 0 msg))))

;; defun slime-message
(if (or (featurep 'xemacs)
	(= emacs-major-version 20))
    ;; XEmacs truncates multi-line messages in the echo area.
    (defun slime-message (fmt &rest args)
      (slime-display-message-or-view (apply #'format fmt args) "*CMUCL Note*"))
    (defun slime-message (fmt &rest args)
      (apply 'message fmt args)))

(defun slime-defun-at-point ()
  "Return the text of the defun at point."
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (buffer-substring-no-properties (point) end))))

(defun slime-symbol-at-point ()
  "Return the symbol at point, otherwise nil."
  (let ((string (thing-at-point 'symbol)))
    (if string (intern (slime-substring-no-properties string)) nil)))

(defun slime-symbol-name-at-point ()
  "Return the name of the symbol at point, otherwise nil."
  (let ((sym (slime-symbol-at-point)))
    (and sym (symbol-name sym))))

(defun slime-sexp-at-point ()
  "Return the sexp at point, otherwise nil."
  (let ((string (thing-at-point 'sexp)))
    (if string (slime-substring-no-properties string) nil)))

(defun slime-function-called-at-point/line ()
  "Return the name of the function being called at point, provided the
function call starts on the same line at the point itself."
  (and (ignore-errors
         (slime-same-line-p (save-excursion (backward-up-list 1) (point))
                            (point)))
       (slime-function-called-at-point)))

(defvar slime-saved-window-configuration nil
  "Window configuration before the last changes SLIME made.")

(defun slime-save-window-configuration ()
  "Save the current window configuration.
This should be called before modifying the user's window configuration.

`slime-restore-window-configuration' restores the saved configuration."
  (setq slime-saved-window-configuration (current-window-configuration)))

(defun slime-restore-window-configuration ()
  "Restore the most recently saved window configuration."
  (interactive)
  (when slime-saved-window-configuration
    (set-window-configuration slime-saved-window-configuration)
    (setq slime-saved-window-configuration nil)))

(defmacro slime-with-output-to-temp-buffer (&rest args)
  "Like `with-output-to-temp-buffer', but saves the window configuration."
  `(progn (slime-save-window-configuration)
          (with-output-to-temp-buffer ,@args)))

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
	      (and (symbolp obj) obj)))
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


;;; CMUCL Setup: compiling and connecting to Swank

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
  (interactive (list (read-string "Host: " "localhost")
		     (let ((port
			    (read-string "Port: " 
					 (number-to-string slime-swank-port))))
		       (or (ignore-errors (string-to-number port)) port))))
  (setq retries (or retries slime-swank-connection-retries))
  (if (zerop retries)
      (error "Unable to contact Swank server.")
    (if (slime-net-connect host port)
        (progn (slime-init-dispatcher)
               (message "Connected to Swank on %s:%S. %s"
                        host port (slime-random-words-of-encouragement)))
      (message "Connecting to Swank (%S attempts remaining)." 
               retries)
      (sit-for 1)
      (slime-connect host port (1- retries)))))

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


;;; Networking

(defvar slime-net-process nil
  "The process (socket) connected to CMUCL.")

(defun slime-net-connect (host port)
  "Establish a connection with CMUCL."
  (condition-case nil
      (progn
        (setq slime-net-process (open-network-stream "CMUCL" nil host port))
        (let ((buffer (slime-make-net-buffer "*cmucl-connection*")))
          (set-process-buffer slime-net-process buffer)
          (set-process-filter slime-net-process 'slime-net-filter)
          (set-process-sentinel slime-net-process 'slime-net-sentinel)
	  (set-process-coding-system slime-net-process 
				     'no-conversion 'no-conversion))
	slime-net-process)
      (file-error () nil)))
    
(defun slime-make-net-buffer (name)
  "Make a buffer suitable for a network process."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (when (fboundp 'set-buffer-multibyte)
	(set-buffer-multibyte nil))
      (buffer-disable-undo))
    buffer))

(defun slime-net-output-funcall (fun &rest args)
  "Send a request for FUN to be applied to ARGS."
  (slime-net-send `(,fun ,@args)))

(defun slime-net-send (sexp)
  "Send a SEXP to CMUCL.
This is the lowest level of communication. The sexp will be READ and
EVAL'd by Lisp."
  (let* ((msg (format "%S\n" sexp))
	 (string (concat (slime-net-enc3 (length msg)) msg)))
    (process-send-string slime-net-process (string-make-unibyte string))))

(defun slime-net-sentinel (process message)
  (message "wire sentinel: %s" message))

(defun slime-net-filter (process string)
  "Accept output from the socket and input all complete messages."
  (with-current-buffer (process-buffer slime-net-process)
    (save-excursion
      (goto-char (point-max))
      (insert string))
    (slime-process-available-input)))

(defun slime-process-available-input ()
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer slime-net-process)
    (while (slime-net-have-input-p)
      (let ((input (slime-net-read))
	    (aborted t))
	(unwind-protect
	    (save-current-buffer
	      (slime-take-input input)
	      (setq aborted nil))
	  (when (and aborted
		     (slime-net-have-input-p))
	    (slime-process-available-input)))))))

(defun slime-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 3)
       (>= (buffer-size) (slime-net-read3))))

(defun slime-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (slime-net-read3))
         (start (+ 3 (point)))
         (end (+ start length)))
    (let ((string (buffer-substring start end)))
      (prog1 (read string)
        (delete-region (point-min) end)))))

(defun slime-net-read3 ()
  "Read a 24-bit big-endian integer from buffer."
  (save-excursion
    (logior (prog1 (ash (char-after) 16) (forward-char 1))
            (prog1 (ash (char-after) 8)  (forward-char 1))
            (char-after))))

(defun slime-net-enc3 (n)
  "Encode an integer into a 24-bit big-endian string."
  (string (logand (ash n -16) 255)
          (logand (ash n -8) 255)
          (logand n 255)))


;;; Evaluation mechanics

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

(put 'destructure-case 'lisp-indent-function 1)

(defvar slime-state-name "??")
(defvar slime-current-state)

(defun slime-move-to-state (state)
  (let ((fn (car state)))
    (setq slime-state-name (case fn
			     (slime-idle-state "idle")
			     (slime-eval-async-state "eval-async")
			     (sldb-state (format "sldb[%d]" sldb-level))
			     (t (prin1-to-string fn))))
    (setq slime-current-state state)))

(defun slime-make-state (function saved-values)
  (cons function saved-values))

(defmacro slime-state (function saved-values)
  `(slime-make-state (function ,function) (list ,@saved-values)))

(defun slime-init-dispatcher ()
  (setq sldb-level 0)
  (slime-move-to-state (slime-state slime-idle-state ())))

(defun slime-take-input (object)
  (slime-check-connected)
  (apply (car slime-current-state) object (cdr slime-current-state)))

(defun slime-idle-state (message)
  (destructure-case message
    ((send-eval-string string package next-state)
     (slime-move-to-state next-state)
     (slime-net-send `(swank:eval-string ,string ,package)))))

(defun slime-ping ()
  (interactive)
  (slime-eval `(swank:ping ,sldb-level) nil)
  (message "Connection ok."))

(defun slime-check-connected ()
  (unless (and slime-net-process
               (eq (process-status slime-net-process) 'open))
    (error "Not connected. Use `M-x slime' to start a Lisp.")))

(defun slime-eval-string-async (string package next-state)
  (slime-take-input `(send-eval-string ,string ,package ,next-state)))

(defconst +slime-sigint+ 2)

(defun slime-send-sigint ()
  (signal-process (process-id (inferior-lisp-proc)) +slime-sigint+))

(defun slime-eval-async-state (reply cont next-state)
  (destructure-case reply
    ((:ok result)
     (slime-move-to-state next-state)
     (funcall cont result))
    ((:aborted)
     (slime-move-to-state next-state)
     (message "Evaluation aborted"))
    ((:debugger-hook)
     (slime-debugger-hook))
    ((interrupt)
     (slime-send-sigint))
    ((quit)
     (slime-send-sigint)
     (slime-move-to-state
      (slime-state
       (lambda (message state)
	 (destructure-case message
	   ((:debugger-hook)
	    (slime-move-to-state state)
	    (slime-net-send '(swank:throw-to-toplevel))
	    (pop-to-buffer inferior-lisp-buffer))))
       (slime-current-state))))))

(defun slime-eval-async (sexp package cont)
  "Evaluate EXPR on the superior Lisp and call CONT with the result."
  (slime-check-connected)
  (slime-eval-string-async (prin1-to-string sexp) package
			   (slime-state slime-eval-async-state
					(cont slime-current-state))))

(defun slime-eval (sexp &optional package)
  "Evaluate EXPR on the superior Lisp and return the result."
  (slime-check-connected)
  (lexical-let ((el-level (recursion-depth))
		(cl-level sldb-level)
		(done nil)
		(error nil)
		value)
    (slime-eval-string-async
     (prin1-to-string sexp)
     package
     (slime-state
      (lambda (message next-state)
	(destructure-case message
	  ((:ok result)
	   (slime-move-to-state next-state)
	   (setq done t)
	   (setq value result))
	  ((:aborted)
	   (slime-move-to-state next-state)
	   (setq done t)
	   (setq error "slime-eval aborted"))
	  ((:sldb-prompt l) 
	   (assert (= sldb-level l)))
	  ((:debugger-hook)
	   (slime-move-to-state (slime-state
				 (lambda (m here)
				   (assert (= cl-level sldb-level))
				   (slime-move-to-state here)
				   (slime-take-input m)
				   (when (> (recursion-depth) el-level)
				     (throw 'el-quit nil)))
				 (slime-current-state)))
	   (slime-debugger-hook)
	   (unwind-protect
	       (catch 'el-quit
		 (save-current-buffer
		   (debug)))
	     (when (> sldb-level cl-level)
	       (slime-take-input '(abort-from-el)))))))
      (slime-current-state)))
    (let ((debug-on-quit t))
      (while (not done)
	(accept-process-output))
      (when error
	(error error))
      value)))

(defun slime-sync ()
  "Block until all asynchronous commands are completed."
  (while (eq (car slime-current-state) 'slime-eval-async-state)
    (accept-process-output slime-net-process)))

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


;;; Compilation and the creation of compiler-note annotations

(defun slime-compile-and-load-file ()
  "Compile and load the buffer's file and highlight compiler notes.

Each source location that is the subject of a compiler note is
underlined and annotated with the relevant information. The commands
`slime-next-note' and `slime-previous-note' can be used to navigate
between compiler notes and to display their full details."
  (interactive)
  (slime-compile-file t))

(defun slime-compile-file (&optional load)
  "Compile current buffer's file and highlight resulting compiler notes.

See `slime-compile-and-load-file' for further details."
  (interactive)
  (unless (eq major-mode 'lisp-mode)
    (error "Only valid in lisp-mode"))
  (save-some-buffers)
  (slime-eval-async
   `(swank:swank-compile-file ,(buffer-file-name) ,(if load t nil))
   nil
   (slime-compilation-finished-continuation))
  (message "Compiling %s.." (buffer-file-name)))

(defun slime-compile-defun ()
  (interactive)
  (slime-eval-async 
   `(swank:swank-compile-string ,(slime-defun-at-point) 
				,(buffer-name)
				,(save-excursion
                                   (end-of-defun)
				   (beginning-of-defun)
				   (point)))
   (slime-buffer-package)
   (slime-compilation-finished-continuation)))

(defun slime-show-note-counts (notes)
  (loop for note in notes 
	for severity = (plist-get note :severity)
	count (eq :error severity) into errors
	count (eq :warning severity) into warnings
	count (eq :note severity) into notes
	finally 
	(message 
	 "Compilation finished: %s errors  %s warnings  %s notes"
	 errors warnings notes)))

(defun slime-compilation-finished (result buffer)
  (with-current-buffer buffer
    (let ((notes (slime-compiler-notes)))
      (slime-show-note-counts notes)
      (slime-highlight-notes notes))))

(defun slime-compilation-finished-continuation ()
  (lexical-let ((buffer (current-buffer)))
    (lambda (result) 
      (slime-compilation-finished result buffer))))

(defun slime-highlight-notes (notes)
  "Highlight compiler notes, warnings, and errors in the buffer."
  (interactive (list (slime-compiler-notes-for-file (buffer-file-name))))
  (save-excursion
    (slime-remove-old-overlays)
    (mapc #'slime-overlay-note notes)))

(defun slime-compiler-notes ()
  "Return all compiler notes, warnings, and errors."
  (slime-eval `(swank:compiler-notes-for-emacs)))

(defun slime-remove-old-overlays ()
  "Delete the existing Slime overlays in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (goto-char (next-overlay-change (point)))
      (dolist (o (overlays-at (point)))
        (when (overlay-get o 'slime)
          (delete-overlay o))))))


;;;; Adding a single compiler note

(defun slime-overlay-note (note)
  "Add a compiler note to the buffer as an overlay.
If an appropriate overlay for a compiler note in the same location
already exists then the new information is merged into it. Otherwise a
new overlay is created."
  (multiple-value-bind (start end) (slime-choose-overlay-region note)
    (goto-char start)
    (let ((severity (plist-get note :severity))
	  (message (plist-get note :message))
	  (appropriate-overlay (slime-note-at-point)))
      (if appropriate-overlay
	  (slime-merge-note-into-overlay appropriate-overlay severity message)
	(slime-create-note-overlay note start end severity message)))))

(defun slime-create-note-overlay (note start end severity message)
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
      (putp 'slime note)
      (putp 'priority (slime-sexp-depth start))
      (putp 'face (slime-severity-face severity))
      (putp 'severity severity)
      (unless (emacs-20-p)
	(putp 'mouse-face 'highlight))
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

(defun slime-choose-overlay-region (note)
  "Choose the start and end points for an overlay over NOTE.
If the location's sexp is a list spanning multiple lines, then the
region around the first element is used."
  (slime-goto-location note)
  (let ((start (point)))
    (slime-forward-sexp)
    (if (slime-same-line-p start (point))
        (values start (point))
      (values (1+ start)
              (progn (goto-char (1+ start))
                     (forward-sexp 1)
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


(defun slime-visit-source-path (source-path)
  "Visit a full source path including the top-level form."
  (ignore-errors
    (goto-char (point-min))
    (forward-sexp (car source-path))
    (slime-forward-source-path (cdr source-path))))

(defun slime-forward-source-path (source-path)
  (let ((origin (point)))
    (cond ((null source-path)
	   (or (ignore-errors (forward-sexp) (backward-sexp) t)
	       (goto-char origin)))
	  (t 
	   (or (ignore-errors (down-list 1)
			      (forward-sexp (car source-path))
			      (slime-forward-source-path (cdr source-path)))
	       (goto-char origin))))))

(defun slime-goto-location (note)
  "Move to the location fiven with the note NOTE.

NOTE's :position property contains the byte offset of the toplevel
form we are searching.  NOTE's :source-path property the path to the
subexpression.

A source-path is a list of the form (1 2 3 4), which indicates a
position in a file in terms of sexp positions. The first number
identifies the top-level form that contains the position that we wish
to move to: the first top-level form has number 0. The second number
in the source-path identifies the containing sexp within that
top-level form, etc."
  (interactive)
  (cond ((not (plist-get note :context))
	 ;; no context available. hmm... move the the first sexp
	 (cond ((plist-get note :buffername)
		(goto-char (plist-get note :buffer-offset)))
	       (t
		(goto-char (point-min))))
	 (forward-sexp)
	 (backward-sexp))
	((stringp (plist-get note :filename))
	 ;; Jump to the offset given with the :position property (and avoid
	 ;; most of the reader issues)
	 (goto-char (plist-get note ':position))
	 ;; Drop the the toplevel form from the source-path and go the
	 ;; expression.
	 (slime-forward-source-path (cdr (plist-get note ':source-path))))
	((stringp (plist-get note :buffername))
	 (assert (string= (buffer-name) (plist-get note :buffername)))
	 (goto-char (plist-get note :buffer-offset))
	 (slime-forward-source-path (cdr (plist-get note ':source-path))))
	(t
	 (error "Unsupported location type %s" note))))

(defun slime-forward-sexp (&optional count)
  "Like `forward-sexp', but steps over reader-conditionals (#- and #+)."
  (dotimes (i (or count 1))
    (forward-sexp)
    (backward-sexp)
    (when (or (looking-at "#+")
		 (looking-at "#-"))
	 (forward-sexp))
    (forward-sexp)))


;;;; Visiting and navigating the overlays of compiler notes

(defun slime-next-note ()
  "Go to and describe the next compiler note in the buffer."
  (interactive)
  (slime-find-next-note)
  (if (slime-note-at-point)
      (slime-show-note (slime-note-at-point))
      (message "No next note.")))

(defun slime-previous-note ()
  "Go to and describe the previous compiler note in the buffer."
  (interactive)
  (slime-find-previous-note)
  (if (slime-note-at-point)
      (slime-show-note (slime-note-at-point))
      (message "No previous note.")))

(defun slime-remove-notes ()
  "Remove compiler-note annotations from the current buffer."
  (interactive)
  (slime-remove-old-overlays))

(defun slime-show-note (overlay)
  "Present the details of a compiler note to the user."
  (slime-temporarily-highlight-note overlay)
  (slime-message "%s" (get-char-property (point) 'help-echo)))

(defun slime-temporarily-highlight-note (overlay)
  "Temporarily highlight a compiler note's overlay.
The highlighting is designed to both make the relevant source more
visible, and to highlight any further notes that are nested inside the
current one.

The highlighting is automatically undone before the next Emacs command."
  (lexical-let ((old-face (overlay-get overlay 'face))
                (overlay overlay))
    (push (lambda () (overlay-put overlay 'face old-face))
	  slime-pre-command-actions)
    (overlay-put overlay 'face 'slime-highlight-face)))


;;;; Overlay lookup operations

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


;;; Arglist Display

(defun slime-space ()
  "Insert a space and print some relevant information (function arglist).
Designed to be bound to the SPC key."
  (interactive)
  (insert " ")
  (when (memq (car slime-current-state) '(slime-idle-state sldb-state))
    (when (slime-function-called-at-point/line)
      (slime-arglist (symbol-name (slime-function-called-at-point/line))))))

(defun slime-arglist (symbol-name)
  "Show the argument list for the nearest function call, if any."
  (interactive (list (read-string "Arglist of: "
                                  (let ((sym (symbol-at-point)))
                                    (and sym (symbol-name sym))))))
  (slime-eval-async 
   `(swank:arglist-string ',symbol-name)
   (slime-buffer-package)
   (lexical-let ((symbol-name symbol-name))
     (lambda (arglist)
       (message "(%s %s)" symbol-name (substring arglist 1 -1))))))


;;; Completion

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
             (slime-with-output-to-temp-buffer "*Completions*"
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
  (let ((package (upcase (slime-cl-symbol-package prefix (slime-buffer-package))))
        (name (upcase (slime-cl-symbol-name prefix)))
        (external-ref (slime-cl-symbol-external-ref-p prefix))
        (has-upcase (let ((case-fold-search nil))
                      (string-match "[A-Z]" (symbol-name prefix)))))
    (mapcar (if has-upcase 'upcase 'downcase)
            (slime-eval `(swank:completions ,name ,package ,external-ref)))))


(defun slime-cl-symbol-name (symbol)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match ":\\([^:]*\\)$" n)
	(match-string 1 n)
	n)))

(defun slime-cl-symbol-package (symbol &optional default)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match "^\\([^:]*\\):" n)
	(match-string 1 n)
	default)))

(defun slime-cl-symbol-external-ref-p (symbol)
  "Does SYMBOL refer to an external symbol?
FOO:BAR is an external reference.
FOO::BAR is not, and nor is BAR."
  (let ((name (if (stringp symbol) symbol (symbol-name symbol))))
    (and (string-match ":" name)
         (not (string-match "::" name)))))


;;; Edit definition

(defvar slime-find-definition-history-ring (make-ring 20)
  "History ring recording the definition-finding \"stack\".")

(defun slime-edit-fdefinition (name)
  "Lookup the definition of the function called at point.
If no function call is recognised, or a prefix argument is given, then
the function name is prompted."
  (interactive (list (let ((called (slime-symbol-at-point)))
                       (if (and called (null current-prefix-arg))
                           (symbol-name called)
                         (read-string "Function name: ")))))
  (let* ((package (upcase (slime-cl-symbol-package name
						   (slime-buffer-package))))
         (file (slime-eval `(swank:find-fdefinition ,name ,package))))
    (if (null file)
        (message "Cannot locate definition of %S" name)
      (slime-push-definition-stack)
      (find-file file)
      (goto-char (point-min))
      (let ((regexp (format "(\\(defun\\|defmacro\\)\\s *%s\\s "
                            (regexp-quote (slime-cl-symbol-name name)))))
        (if (re-search-forward regexp nil t)
            (progn (beginning-of-line)
                   (unless (pos-visible-in-window-p)
                     (recenter 4)))
          (message "Unable to find definition by searching."))))))

(defun slime-push-definition-stack (&optional marker)
  "Add MARKER to the edit-definition history stack.
If MARKER is nil, use the point."
  (ring-insert-at-beginning slime-find-definition-history-ring
                            (or marker (point-marker))))

(defun slime-pop-find-definition-stack ()
  "Pop the edit-definition stack and goto the location."
  (interactive)
  (unless (ring-empty-p slime-find-definition-history-ring)
    (let* ((marker (ring-remove slime-find-definition-history-ring))
	   (buffer (marker-buffer marker)))
      (if (buffer-live-p buffer)
	  (progn (switch-to-buffer buffer)
		 (goto-char (marker-position marker)))
        ;; If this buffer was deleted, recurse to try the next one
        (slime-pop-find-definition-stack)))))

(defun slime-edit-symbol-fdefinition (name)
  "Lookup the function definition of the symbol at point."
  (interactive (list (cond (current-prefix-arg
			    (read-string "Edit fdefinition: "))
			   (t (slime-symbol-name-at-point)))))
  (let ((origin (point-marker))
	(source-location
	 (slime-eval `(swank:function-source-location-for-emacs ,name)
		     (slime-buffer-package))))
    (slime-goto-source-location source-location)
    (ring-insert-at-beginning slime-find-definition-history-ring origin)))
    

;;; Interactive evaluation.

(defun slime-use-inf-lisp-p ()
  (and inferior-lisp-buffer (get-buffer inferior-lisp-buffer)))

(defun slime-insert-transcript-delimiter (string)
  (when (slime-use-inf-lisp-p)
    (delete-windows-on inferior-lisp-buffer)
    (with-current-buffer inferior-lisp-buffer
      (goto-char (point-max))
      (insert "\n;;;; " 
	      (subst-char-in-string ?\n ?\ 
				    (substring string 0 
					       (min 60 (length string))))
	      " ...\n")
      (set-marker 
       (process-mark (get-buffer-process (current-buffer))) (point)))))

(defun slime-inferior-lisp-marker-position ()
  (when (slime-use-inf-lisp-p)
    (marker-position 
     (process-mark (get-buffer-process inferior-lisp-buffer)))))
    
(defun slime-inferior-lisp-show-last-output (output-start)
  (when (slime-use-inf-lisp-p)
    (when (< output-start (slime-inferior-lisp-marker-position))
      (slime-display-buffer-region 
       inferior-lisp-buffer 
       output-start (slime-inferior-lisp-marker-position) 1))))

(defun slime-interactive-eval (string)
  (interactive "sSlime Eval: ")
  (slime-insert-transcript-delimiter string)
  (slime-eval-async 
   `(swank:interactive-eval ,string)
   (slime-buffer-package t)
   (slime-show-evaluation-result-continuation)))

(defun slime-display-buffer-region (buffer start end &optional border)
  (slime-save-window-configuration)
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

(defun slime-show-evaluation-result (output-start value)
  (message "=> %s" value)
  (slime-inferior-lisp-show-last-output output-start))

(defun slime-show-evaluation-result-continuation ()
  (lexical-let ((output-start (slime-inferior-lisp-marker-position)))
     (lambda (value)
       (slime-show-evaluation-result output-start value))))
  
(defun slime-last-expression ()
  (buffer-substring-no-properties (save-excursion (backward-sexp) (point))
				  (point)))

(defun slime-eval-last-expression ()
  (interactive)
  (slime-interactive-eval (slime-last-expression)))

(defun slime-eval-defun ()
  (interactive)
  (slime-interactive-eval (slime-defun-at-point)))

(defun slime-eval-region (start end)
  (interactive "r")
  (slime-eval-async
   `(swank:interactive-eval-region ,(buffer-substring-no-properties start end))
   (slime-buffer-package)
   (slime-show-evaluation-result-continuation)))

(defun slime-eval-buffer ()
  (interactive)
  (slime-eval-region (point-min) (point-max)))

(defun slime-re-evaluate-defvar ()
  "Force the re-evaluaton of the defvar form before point.  

First make the variable unbound, then evaluate the entire form."
  (interactive)
  (slime-eval-async `(swank:re-evaluate-defvar ,(slime-last-expression))
		    (slime-buffer-package)
		    (slime-show-evaluation-result-continuation)))

(defun slime-pprint-eval-last-expression ()
  (interactive)
  (slime-eval-describe `(swank:pprint-eval ,(slime-last-expression))))

(defun slime-toggle-trace-fdefinition (fname-string)
  (interactive (lisp-symprompt "(Un)trace" (slime-symbol-name-at-point)))
  (message "%s" (slime-eval `(swank:toggle-trace-fdefinition ,fname-string)
			    (slime-buffer-package t))))

(defun slime-disassemble-symbol (symbol-name)
  (interactive (list (slime-symbol-name-at-point)))
  (slime-eval-describe `(swank:disassemble-symbol ,symbol-name)))

(defun slime-load-file (filename)
  (interactive "fLoad file: ")
  (slime-eval-async 
   `(swank:load-file ,(expand-file-name filename)) nil 
   (slime-show-evaluation-result-continuation)))


;;; Documentation

(defun slime-show-description (string package)
  (save-current-buffer
    (let* ((slime-package-for-help-mode package)
	   (temp-buffer-show-hook 
	    (cons (lambda ()
		    (setq slime-buffer-package slime-package-for-help-mode)
		    (set-syntax-table lisp-mode-syntax-table)
		    (slime-mode t))
		  temp-buffer-show-hook)))
      (slime-with-output-to-temp-buffer "*Help*"
	(princ string)))))

(eval-and-compile 
  (if (fboundp 'substring-no-properties)
      (defalias 'slime-substring-no-properties 'substring-no-properties)
    (defun slime-substring-no-properties (string &optional start end)
      (let* ((start (or start 0))
	     (end (or end (length string)))
	     (string (substring string start end)))
	(set-text-properties start end nil string)
	string))))

(defun slime-eval-describe (form)
  (let ((package (slime-buffer-package)))
    (slime-eval-async 
     form package
     (lexical-let ((package package))
       (lambda (string) (slime-show-description string package))))))

(defun slime-describe-symbol (symbol-name)
  (interactive
   (list (cond (current-prefix-arg 
		(read-string "Describe symbol: "))
	       ((slime-symbol-name-at-point))
	       (t (read-string "Describe symbol: ")))))
  (when (not symbol-name)
    (error "No symbol given"))
  (slime-eval-describe `(swank:describe-symbol ,symbol-name)))

(defun slime-read-package-name (prompt)
  (completing-read prompt (mapcar (lambda (x) (cons x x))
				  (slime-eval 
				   `(swank:list-all-package-names)))))

(defun slime-apropos (string &optional only-external-p package)
  (interactive
   (if current-prefix-arg
       (list (read-string "SLIME Apropos: ")
             (y-or-n-p "External symbols only? ")
             (let ((pkg (slime-read-package-name "Package: ")))
               (if (string= pkg "") nil pkg)))
     (list (read-string "SLIME Apropos: ") t nil)))
  (slime-eval-async
   `(swank:apropos-list-for-emacs ,string ,only-external-p ,package)
   (slime-buffer-package t)
   (lexical-let ((string string)
                 (package package))
     (lambda (r) (slime-show-apropos r string package)))))

(defun slime-apropos-all ()
  "Shortcut for (slime-apropos <pattern> nil nil)"
  (interactive)
  (slime-apropos (read-string "SLIME Apropos: ") nil nil))

(defun slime-show-apropos (plists string package)
  (if (null plists)
      (message "No apropos matches for %S" string)
    (save-current-buffer
      (slime-with-output-to-temp-buffer "*CMUCL Apropos*"
        (set-buffer standard-output)
        (apropos-mode)
        (set-syntax-table lisp-mode-syntax-table)
        (slime-mode t)
        (setq slime-buffer-package package)
        (set (make-local-variable 'truncate-lines) t)
        (slime-print-apropos plists)))))

(defun slime-princ-propertized (string props)
  (with-current-buffer standard-output
    (let ((start (point)))
      (princ string)
      (add-text-properties start (point) props))))

(autoload 'apropos-mode "apropos")
(defvar apropos-label-properties)

(defun slime-print-apropos (plists)
  (dolist (plist plists)
    (let ((designator (plist-get plist :designator)))
      (slime-princ-propertized designator 
			       (list 'face apropos-symbol-face
				     'item designator
				     'action 'slime-describe-symbol)))
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
	    in '((:variable "Variable" swank:describe-symbol)
		 (:function "Function" swank:describe-function)
		 (:setf "Setf" swank:describe-setf-function)
		 (:type "Type" swank:describe-type)
		 (:class "Class" swank:describe-class))
	    do
	    (let ((value (plist-get plist prop))
		  (start (point)))
	      (when value
		(princ "  ") 
		(slime-princ-propertized namespace apropos-label-properties)
		(princ ": ")
		(princ (etypecase value
			 (string value)
			 ((member :not-documented) "(not documented)")))
		(put-text-property start (point) 'describer action)
		(put-text-property start (point) 'action 'slime-call-describer)
		(terpri)))))))

(defun slime-call-describer (item)
  (slime-eval-describe `(,(get-text-property (point) 'describer) ,item)))


;;; Cross-referencing

(defun slime-who-calls (symbol)
  "Show all known callers of the function SYMBOL."
  (interactive (list (slime-read-symbol "Who calls: ")))
  (slime-xref 'swank:who-calls))

(defun slime-who-references (symbol)
  "Show all known referrers of the global variable SYMBOL."
  (interactive (list (slime-read-symbol "Who references: ")))
  (slime-xref 'swank:who-references))

(defun slime-who-binds (symbol)
  "Show all known binders of the global variable SYMBOL."
  (interactive (list (slime-read-symbol "Who binds: ")))
  (slime-xref 'swank:who-binds))

(defun slime-who-sets (symbol)
  "Show all known setters of the global variable SYMBOL."
  (interactive (list (slime-read-symbol "Who sets: ")))
  (slime-xref 'swank:who-sets))

(defun slime-who-macroexpands (symbol)
  "Show all known expanders of the macro SYMBOL."
  (interactive (list (slime-read-symbol "Who macroexpands: ")))
  (slime-xref 'swank:who-macroexpands))

(defun slime-read-symbol (prompt)
  (or (and (not current-prefix-arg) (slime-symbol-at-point))
      (intern (read-string prompt))))

(defun slime-xref (swank-function)
  (slime-eval-async
   `(,swank-function ',symbol)
   (slime-buffer-package t)
   'slime-show-xrefs))
  

(defun slime-show-xrefs (file-referrers)
  (if (null file-referrers)
      (message "No references found.")
    (slime-save-window-configuration)
    (save-selected-window
      (view-buffer-other-window (get-buffer-create "*CMUCL xref*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (set-syntax-table lisp-mode-syntax-table)
      (slime-mode t)
      (set (make-local-variable 'truncate-lines) t)
      (dolist (ref file-referrers)
        (apply #'slime-insert-xrefs ref))
      (goto-char (point-min))
      (forward-line 1)
      (set-window-text-height (selected-window)
                              (min (1+ (count-lines (point-min) (point-max)))
                                   (window-text-height))))))

(defun slime-xref-init-keys ()
  (use-local-map (make-sparse-keymap))
  (define-key (current-local-map) "\C-m"   'slime-goto-xref)
  (define-key (current-local-map) [return] 'slime-goto-xref))

(defun slime-insert-xrefs (filename refs)
  (unless (bobp) (insert "\n"))
  (insert (format "In %s:\n" filename))
  (dolist (ref refs)
    (destructuring-bind (referrer source-path) ref
      (slime-insert-propertized (list 'slime-xref-file filename
                                      'slime-xref-source-path source-path
                                      'face 'font-lock-function-name-face)
                                (format "%s\n" referrer)))))

(defun slime-goto-xref ()
  (interactive)
  (let ((file (get-text-property (point) 'slime-xref-file))
        (path (get-text-property (point) 'slime-xref-source-path)))
    (unless (and file path)
      (error "No reference at point."))
    (find-file-other-window file)
    (goto-char (point-min))
    (slime-visit-source-path path)))


;;; Macroexpansion

(defun slime-eval-macroexpand (expander)
  (let ((string (slime-sexp-at-point)))
    (slime-eval-describe `(,expander ,string))))

(defun slime-macroexpand-1 (&optional repeatedly)
  (interactive "P")
  (slime-eval-macroexpand
   (if repeatedly 'swank:swank-macroexpand 'swank:swank-macroexpand-1)))

(defun slime-macroexpand-all ()
  (interactive)
  (slime-eval-macroexpand 'swank:swank-macroexpand-all))


;;; Subprocess control

(defun slime-interrupt ()
  (interactive)
  (slime-take-input '(interrupt)))

(defun slime-quit ()
  (interactive)
  (slime-take-input '(quit)))


;;; Debugger

(defvar slime-use-tty-debugger nil
  "*")

(defvar sldb-condition)
(defvar sldb-restarts)
(defvar sldb-backtrace-length)
(defvar sldb-level-in-buffer)
(defvar sldb-backtrace-start-marker)
(defvar sldb-mode-map)

(defun slime-debugger-hook ()
  (if slime-use-tty-debugger
      (slime-enter-tty-debugger)
    (slime-enter-sldb)))

(defun slime-enter-tty-debugger ()
  (slime-move-to-state (slime-state slime-tty-debugger-state 
				    (slime-current-state)))
  (pop-to-buffer inferior-lisp-buffer t t)
  (enlarge-window (- (/ (frame-height) 2) (window-height)))
  (goto-char (point-max))
  (slime-net-send
   `(swank:eval-string
     ,(prin1-to-string '(let ((*debugger-hook* nil))
			  (invoke-debugger swank::*swank-debugger-condition*)))
     nil)))

(defun slime-tty-debugger-state (event next-state)
  (destructure-case event
    ((:ok)
     (slime-move-to-state next-state)
     (delete-windows-on inferior-lisp-buffer))
    ((:aborted)
     (slime-move-to-state next-state)
     (delete-windows-on inferior-lisp-buffer))
    ((:debugger-hook)
     (slime-debugger-hook))))

(add-hook 'inferior-lisp-mode-hook
	     (lambda ()
	       (setq comint-scroll-to-bottom-on-input t)
	       (setq comint-scroll-show-maximum-output t)))

(defun slime-enter-sldb ()
  (slime-move-to-state (slime-state sldb-state (slime-current-state)))
  (incf sldb-level)
  (slime-net-send `(swank:sldb-loop)))

(defun sldb-state (message previous-state)
  (destructure-case message
    ((:sldb-prompt l)
     (assert (= l sldb-level))
     (let ((buffer (get-buffer "*sldb*")))
       (when (or (not buffer)
		 (with-current-buffer (get-buffer "*sldb*")
		   (/= sldb-level-in-buffer sldb-level)))
	 (sldb-setup)))
     ;;(unless (get-buffer-window "*sldb*")
     ;;	 (pop-to-buffer (get-buffer "*sldb*")))
     )
    ((:sldb-abort l)
     (assert (= l sldb-level))
     (when (get-buffer "*sldb*")
       (sldb-cleanup (get-buffer "*sldb*")))
     (decf sldb-level)
     (slime-move-to-state previous-state))
    ((:debug-condition message)
     (message "%s" message))
    ((abort-from-el)
     (sldb-abort))
    ((send-eval-string string package next-state)
     (slime-idle-state `(send-eval-string ,string ,package ,next-state)))))

(defun sldb-setup ()
  (with-current-buffer (get-buffer-create "*sldb*")
    (setq buffer-read-only nil)
    (sldb-mode)
    (setq buffer-read-only t)
    (add-hook (make-local-variable 'kill-buffer-hook) 'sldb-delete-overlays)
    (pop-to-buffer (current-buffer))))

(defun slime-insert-propertized (props &rest args)
  (let ((start (point)))
    (apply #'insert args)
    (add-text-properties start (point) props)))


(define-derived-mode sldb-mode fundamental-mode "sldb" 
  "Superior lisp debugger mode

\\{sldb-mode-map}"
  (erase-buffer)
  (set-syntax-table lisp-mode-syntax-table)
  (mapc #'make-local-variable '(sldb-condition 
				sldb-restarts
				sldb-backtrace-length
				sldb-level-in-buffer
				sldb-backtrace-start-marker))
  (setq sldb-level-in-buffer sldb-level)
  (setq mode-name (format "sldb[%d]" sldb-level))
  (destructuring-bind (condition restarts length frames)
      (slime-eval `(swank:debugger-info-for-emacs 0 1))
    (setq sldb-condition condition)
    (setq sldb-restarts restarts)
    (setq sldb-backtrace-length length)
    (insert condition "\n" "\nRestarts:\n")
    (loop for (name string) in restarts
	  for number from 0 
	  do (slime-insert-propertized
	      `(face bold 
		     restart-number ,number
		     sldb-default-action sldb-invoke-restart
		     mouse-face highlight)
	      "  " (number-to-string number) ": ["  name "] " string "\n"))
    (insert "\nBacktrace:\n")
    (setq sldb-backtrace-start-marker (point-marker))
    (sldb-insert-frames frames)))

(defun sldb-insert-frames (frames)
  (save-excursion
    (loop for frame in frames
	  for (number string) = frame
	  do (slime-insert-propertized `(frame ,frame) string "\n"))
    (let ((number (sldb-previous-frame-number)))
      (cond ((= sldb-backtrace-length (1+ number)))
	    (t
	     (slime-insert-propertized 
	      '(sldb-default-action 
		sldb-fetch-more-frames
		point-entered sldb-fetch-more-frames)
	      "   --more--"))))))

(defun sldb-fetch-more-frames (&optional start end)
  (let ((inhibit-point-motion-hooks t))
    (let ((previous (sldb-previous-frame-number)))
      (let ((inhibit-read-only t))
	(beginning-of-line)
	(let ((start (point)))
	  (end-of-buffer)
	  (delete-region start (point)))
	(sldb-insert-frames 
	 (slime-eval `(swank:backtrace-for-emacs 
		       ,(1+ previous)
		       ,(+ previous 40))))))))

(defun sldb-default-action ()
  (interactive)
  (let ((fn (get-text-property (point) 'sldb-default-action)))
    (if fn (funcall fn))))

(defvar sldb-overlays '())

(defun sldb-delete-overlays ()
  (mapc #'delete-overlay sldb-overlays)
  (setq sldb-overlays '()))
  
(defun sldb-highlight-sexp (&optional start end)
  "Highlight the first sexp after point."
  (sldb-delete-overlays)
  (let ((start (or start (point)))
	(end (or end (save-excursion (forward-sexp) (point)))))
    (push (make-overlay start (1+ start)) sldb-overlays)
    (push (make-overlay (1- end) end) sldb-overlays)
    (dolist (overlay sldb-overlays)
      (overlay-put overlay 'face 'secondary-selection))))

(defun sldb-frame-number-at-point ()
  (let ((frame (get-text-property (point) 'frame)))
    (cond (frame (car frame))
	  (t (error "No frame at point")))))

(defun sldb-previous-frame-number ()
  (save-excursion
    (sldb-backward-frame)
    (sldb-frame-number-at-point)))

(defun slime-goto-source-location (source-location &optional other-window)
  (let ((error (plist-get source-location :error)))
    (when error
      (error "Cannot locate source: %s" error))
    (case (plist-get source-location :from)
      (:file
       (funcall (if other-window #'find-file-other-window #'find-file)
		(plist-get source-location :filename))
	 (goto-char (plist-get source-location :position)))
      (:stream
       (let ((info (plist-get source-location :info)))
	 (cond ((and (consp info) (eq :emacs-buffer (car info)))
		(let ((buffer (plist-get info :emacs-buffer))
		      (offset (plist-get info :emacs-buffer-offset)))
		  (funcall (if other-window 
			       #'switch-to-buffer-other-window 
			     #'switch-to-buffer)
			   (get-buffer buffer))
		  (goto-char offset)
		  (slime-forward-source-path 
		   (cdr (plist-get source-location :path)))))
	       (t
		(error "Cannot locate source from stream: %s"
		       source-location)))))
      (t
       (slime-message "Source Form:\n%s" 
		      (plist-get source-location :source-form))))))
	   
(defun sldb-show-source ()
  (interactive)
  (sldb-delete-overlays)
  (let* ((number (sldb-frame-number-at-point))
	 (source-location (slime-eval
			   `(swank:frame-source-location-for-emacs ,number))))
    (save-selected-window
      (slime-goto-source-location source-location t)
      (sldb-highlight-sexp))))

(defun sldb-frame-details-visible-p ()
  (and (get-text-property (point) 'frame)
       (get-text-property (point) 'details-visible-p)))

(defun sldb-toggle-details (&optional on)
  (interactive)
  (sldb-frame-number-at-point)
  (let ((inhibit-read-only t))
    (if (or on (not (sldb-frame-details-visible-p)))
	(sldb-show-frame-details)
      (sldb-hide-frame-details))))

(defmacro* sldb-propertize-region (props &body body)
  (let ((start (gensym)))
    `(let ((,start (point)))
      (prog1 (progn ,@body)
	(add-text-properties ,start (point) ,props)))))

(put 'sldb-propertize-region 'lisp-indent-function 1)

(defun sldb-frame-region ()
  (save-excursion
    (goto-char (next-single-property-change (point) 'frame nil (point-max)))
    (backward-char)
    (values (previous-single-property-change (point) 'frame)
	    (next-single-property-change (point) 'frame nil (point-max)))))

(defun sldb-show-frame-details ()
  (multiple-value-bind (start end) (sldb-frame-region)
    (let ((end
	   (save-excursion
	     (let* ((props (text-properties-at (point)))
		    (frame (plist-get props 'frame))
		    (frame-number (car frame))
		    (standard-output (current-buffer)))
	       (goto-char start)
	       (delete-region start end)
	       (sldb-propertize-region (plist-put props 'details-visible-p t)
		 (insert (second frame) "\n"
			 "   Locals:\n")
		 (sldb-princ-locals frame-number "       ")
		 (let ((catchers (sldb-catch-tags frame-number)))
		   (cond ((null catchers)
			  (princ "   [No catch-tags]\n"))
			 (t
			  (princ "   Catch-tags:\n")
			  (loop for (tag . location) in catchers
				do (slime-insert-propertized  
				    '(catch-tag ,tag)
				    (format "      %S\n" tag)))))
		   (terpri)
		   (point)))))))
    ;;(sldb-maybe-recenter-region start end)
    )))

(defun sldb-maybe-recenter-region (start end)
  (sit-for 0 1)
  (cond ((and (< (window-start) start)
	      (< end (window-end))))
	(t
	 (let ((lines (count-lines start end)))
	   (cond ((< lines (window-height))
		  (recenter (max (- (window-height) lines 4) 0)))
		 (t (recenter 1)))))))

(defun sldb-hide-frame-details ()
  (save-excursion
    (multiple-value-bind (start end) (sldb-frame-region)
      (let* ((props (text-properties-at (point)))
	     (frame (plist-get props 'frame)))
	(goto-char start)
	(delete-region start end)
	(sldb-propertize-region (plist-put props 'details-visible-p nil)
	  (insert (second frame) "\n"))))))
  
(defvar sldb-eval-expression-history '())

(defun sldb-eval-in-frame (string)
  (interactive (list (read-from-minibuffer 
		      "Eval in frame: " 
		      nil nil nil 'sldb-eval-expression-history)))
  (let* ((number (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:eval-string-in-frame ,string ,number)
		      nil
		      (lambda (reply) (slime-message "==> %s" reply)))))

(defun sldb-forward-frame ()
  (goto-char (next-single-char-property-change (point) 'frame)))

(defun sldb-backward-frame ()
  (goto-char (previous-single-char-property-change
	      (point) 'frame 
	      nil sldb-backtrace-start-marker)))

(defun sldb-down ()
  (interactive)
  (sldb-forward-frame))

(defun sldb-up ()
  (interactive)
  (sldb-backward-frame)
  (when (= (point) sldb-backtrace-start-marker)
    (recenter (1+ (count-lines (point-min) (point))))))

(defun sldb-sugar-move (move-fn)
  (let ((inhibit-read-only t))
    (when (sldb-frame-details-visible-p)
      (sldb-hide-frame-details))
    (funcall move-fn)
    (sldb-toggle-details t)
    (sldb-show-source)))
  
(defun sldb-details-up ()
  (interactive)
  (sldb-sugar-move 'sldb-up))

(defun sldb-details-down ()
  (interactive)
  (sldb-sugar-move 'sldb-down))

(defun sldb-frame-locals (frame)
  (slime-eval `(swank:frame-locals ,frame)))

(defun sldb-princ-locals (frame prefix)
  (dolist (l (sldb-frame-locals frame))
    (princ prefix)
    (princ (plist-get l :symbol))
    (let ((id (plist-get l :id)))
      (unless (zerop id) (princ "#") (princ id)))
    (princ " = ")
    (cond ((eq :valid (plist-get l :validity))
	   (princ (plist-get l :value-string)))
	  (t
	   (princ "<not-available>")))
    (terpri)))

(defun sldb-list-locals ()
  (interactive)
  (let ((string (with-output-to-string
		  (sldb-princ-locals (sldb-frame-number-at-point) ""))))
    (slime-message "%s" string)))

(defun sldb-catch-tags (frame)
  (slime-eval `(swank:frame-catch-tags ,frame)))

(defun sldb-list-catch-tags ()
  (interactive)
  (slime-message "%S" (sldb-catch-tags (sldb-frame-number-at-point))))
  
(defun sldb-cleanup (buffer)
  (delete-windows-on buffer)
  (kill-buffer buffer))

(defun sldb-quit ()
  (interactive)
  (slime-eval-async '(swank:throw-to-toplevel) nil (lambda ())))

(defun sldb-continue ()
  (interactive)
  (slime-eval-async '(swank:sldb-continue)
		    nil
		    (lambda (foo)
		      (message "No restart named continue") 
		      (ding))))

(defun sldb-abort ()
  (interactive)
  (slime-eval-async '(swank:sldb-abort) nil (lambda ())))

(defun sldb-invoke-restart (&optional number)
  (interactive)
  (let ((restart (or number
                     (sldb-restart-at-point)
                     (error "No restart at point"))))
    (slime-eval-async `(swank:invoke-nth-restart ,restart) nil (lambda ()))))

(defun sldb-restart-at-point ()
  (get-text-property (point) 'restart-number))

(define-key sldb-mode-map "v" 'sldb-show-source)
(define-key sldb-mode-map (kbd "RET") 'sldb-default-action)
(define-key sldb-mode-map "e" 'sldb-eval-in-frame)
(define-key sldb-mode-map "d" 'sldb-down)
(define-key sldb-mode-map "u" 'sldb-up)
(define-key sldb-mode-map "\M-n" 'sldb-details-down)
(define-key sldb-mode-map "\M-p" 'sldb-details-up)
(define-key sldb-mode-map "l" 'sldb-list-locals)
(define-key sldb-mode-map "t" 'sldb-toggle-details)
(define-key sldb-mode-map "c" 'sldb-continue)
(define-key sldb-mode-map "a" 'sldb-abort)
(define-key sldb-mode-map "r" 'sldb-invoke-restart)
(define-key sldb-mode-map "q" 'sldb-quit)

;; Keys 0-9 are shortcuts to invoke particular restarts.
(defun def-sldb-invoke-restart (n key)
  ;; I don't like using EVAL, but what's the right way to do this? -luke
  (let ((fname (intern (format "sldb-invoke-restart-%S" (eval n)))))
    (eval
     `(progn
        (defun ,fname ()
          (interactive)
          (sldb-invoke-restart ,n))
        (define-key sldb-mode-map ,key ',fname)))))

;; (NB: XEmacs has a separate char type)
(loop for c across "0123456789"
      for n from 0 to 9
      do (def-sldb-invoke-restart n (string c)))


;;; Test suite

(defvar slime-tests '()
  "Names of test functions.")

(defvar slime-test-debug-on-error nil
  "*When non-nil debug errors in test cases.")

(defvar slime-test-verbose-p nil
  "*When non-nil do not display the results of individual checks.")

(defvar slime-total-tests nil
  "Total number of tests executed during a test run.")

(defvar slime-failed-tests nil
  "Total number of failed tests during a test run.")

(defun slime-run-tests ()
  (interactive)
  (slime-with-output-to-temp-buffer "*Tests*"
    (with-current-buffer standard-output
      (set (make-local-variable 'truncate-lines) t))
    (slime-execute-tests)))

(defun slime-execute-tests ()
  (save-window-excursion
    (let ((slime-total-tests 0)
          (slime-failed-tests 0))
      (loop for (name function inputs) in slime-tests
            do (dolist (input inputs)
                 (incf slime-total-tests)
                 (princ (format "%s: %S\n" name input))
                 (condition-case err
                     (apply function input)
                   (error (incf slime-failed-tests)
                          (slime-print-check-error err)))))
      (if (zerop slime-failed-tests)
          (message "All %S tests completed successfully." slime-total-tests)
        (message "Failed on %S of %S tests."
                 slime-failed-tests slime-total-tests)))))

(defun slime-batch-test ()
  "Run the test suite in batch-mode."
  (let ((standard-output t)
        (slime-test-debug-on-error nil))
    (slime)
    (slime-run-tests)))

(defmacro def-slime-test (name args doc inputs &rest body)
  "Define a test case.
NAME is a symbol naming the test.
ARGS is a lambda-list.
DOC is a docstring.
INPUTS is a list of argument lists, each tested separately.
BODY is the test case. The body can use `slime-check' to test
conditions (assertions)."
  (let ((fname (intern (format "slime-test-%s" name))))
    `(progn
       (defun ,fname ,args
         ,doc
         ,@body)
       (setq slime-tests (append (remove* ',name slime-tests :key 'car)
                                 (list (list ',name ',fname ,inputs)))))))

(defmacro slime-check (test-name &rest body)
  `(if (progn ,@body)
       (slime-print-check-ok ',test-name)
     (incf slime-failed-tests)
     (slime-print-check-failed ',test-name)
     (when slime-test-debug-on-error
       (debug (format "Check failed: %S" ',test-name)))))

(defun slime-print-check-ok (test-name)
  (when slime-test-verbose-p
    (princ (format "        ok:     %s\n" test-name))))

(defun slime-print-check-failed (test-name)
  (slime-princ-propertized (format "        FAILED: %s\n" test-name)
                           '(face font-lock-warning-face)))

(defun slime-print-check-error (reason)
  (slime-princ-propertized (format "        ERROR:  %S\n" reason)
                           '(face font-lock-warning-face)))

(def-slime-test find-definition
    (name expected-filename)
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

(def-slime-test complete-symbol
    (prefix expected-completions)
    "Find the completions of a symbol-name prefix."
    '(("cl:compile" ("compile" "compile-file" "compile-file-pathname"
                     "compiled-function" "compiled-function-p"
                     "compiler-macro" "compiler-macro-function"))
      ("cl:foobar" nil)
      ("cl::compile-file" ("compile-file" "compile-file-pathname")))
  (let ((completions (slime-completions prefix)))
    (slime-check expected-completions
      (equal expected-completions (sort completions 'string<)))))

(def-slime-test arglist
    (symbol expected-arglist)
    "Lookup the argument list for SYMBOL.
Confirm that EXPECTED-ARGLIST is displayed."
    '(("list" "(list &rest args)")
      ("defun" "(defun &whole source name lambda-list &parse-body (body decls doc))")
      ("cl::defun" "(cl::defun &whole source name lambda-list &parse-body (body decls doc))"))
  (slime-arglist symbol)
  (slime-sync)
  (slime-check expected-arglist
    (string= expected-arglist (current-message))))

(def-slime-test compile-defun 
    (program subform)
    "Compile PROGRAM containing errors.
Confirm that SUBFORM is correclty located."
    '(("(defun :foo () (:bar))" (:bar))
      ("(defun :foo () 
         #\\space
         ;;Sdf              
         (:bar))"
       (:bar))
      ;; this fails
      ("(defun :foo () 
            #| |#
            (:bar))"
       (:bar))
      )
  (with-temp-buffer 
    (lisp-mode)
    (insert program)
    (slime-compile-defun)
    (slime-sync)
    (slime-previous-note)
    (slime-check error-location-correct
      (equal (read (current-buffer))
	     subform))))

;; This one currently fails.
(def-slime-test concurrent-async-requests
    (n)
    "Test making concurrent asynchronous requests."
    '((1) (2) (5))
  (lexical-let ((replies 0))
    (dotimes (i n)
      (slime-eval-async t "CL-USER" (lambda (r) (incf replies))))
    (slime-sync)
    (slime-check continuations-called
      (= replies n))))

(put 'def-slime-test 'lisp-indent-function 4)
(put 'slime-check 'lisp-indent-function 1)


;;; Portability library

(when (featurep 'xemacs)
  (require 'overlay)
  (defun next-single-char-property-change (&rest args)
    (or (apply 'next-single-property-change args)
        (point-max)))
  (defun previous-single-char-property-change (&rest args)
    (or (apply 'previous-single-property-change args)
        (point-min)))
  (unless (fboundp 'string-make-unibyte)
    (defalias 'string-make-unibyte #'identity))
  )

(unless (fboundp 'define-minor-mode)
  (require 'easy-mmode)
  (defalias 'define-minor-mode 'easy-mmode-define-minor-mode))

(unless (fboundp 'next-single-char-property-change)
  (defun next-single-char-property-change (position prop &optional
						    object limit)
    (let ((limit (typecase limit
		   (null nil)
		   (marker (marker-position limit))
		   (t limit))))
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
		return pos)))))))

(unless (fboundp 'previous-single-char-property-change)
  (defun previous-single-char-property-change (position prop &optional
							object limit)
    (let ((limit (typecase limit
		   (null nil)
		   (marker (marker-position limit))
		   (t limit))))
    (if (stringp object)
	(or (previous-single-property-change position prop object limit)
	    limit 
	    (length object))
      (with-current-buffer (or object (current-buffer))
	(let ((initial-value (get-char-property (1- position) prop object))
	      (limit (or limit (point-min))))
	  (if (<= position limit)
	      limit
	    (loop for pos = position then 
		  (previous-char-property-change pos limit)
		  if (<= pos limit) return limit
		  if (not (eq initial-value 
			      (get-char-property (1- pos) prop object))) 
		  return pos))))))))

(defun emacs-20-p ()
  (and (not (featurep 'xemacs))
       (= emacs-major-version 20)))

;;; Finishing up

(run-hooks 'slime-load-hook)

(provide 'slime)

;;; slime.el ends here
