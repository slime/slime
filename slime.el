;; -*- mode: emacs-lisp; mode: outline-minor; outline-regexp: ";;;;*"; indent-tabs-mode: nil -*-
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
(require 'pp)
(require 'hideshow)
(require 'hyperspec)
(require 'font-lock)
(when (featurep 'xemacs)
  (require 'overlay))
(eval-when (compile load eval)
  (unless (fboundp 'define-minor-mode)
    (require 'easy-mmode)
    (defalias 'define-minor-mode 'easy-mmode-define-minor-mode)))

(defvar slime-path
  (let ((path (locate-library "slime")))
    (and path (file-name-directory path)))
  "Directory containing the Slime package.
This is used to load the supporting Common Lisp library, Swank.
The default value is automatically computed from the location of the
Emacs Lisp package.")

(defvar slime-swank-connection-retries nil
  "Number of times to try connecting to the Swank server before aborting.
Nil means never give up.")

(defvar slime-backend "swank-loader"
  "The name of the Lisp file implementing the Swank server.")

(make-variable-buffer-local
 (defvar slime-buffer-package nil
   "The Lisp package associated with the current buffer.
Don't access this value directly in a program. Call the function with
the same name instead."))

(defvar slime-lisp-features nil
  "The symbol names in the *FEATURES* list of the Superior lisp.
This is needed to READ Common Lisp expressions adequately.")

(defvar slime-lisp-preferred-package-nicknames
  '(("COMMON-LISP-USER" . "CL-USER")
    ("COMMON-LISP" . "CL"))
  "Association list mapping package names onto their preferred nicknames.
This determines which name appears in the REPL prompt.")

(defvar slime-default-lisp-package
  (or (cdr (assoc "COMMON-LISP-USER" slime-lisp-preferred-package-nicknames))
      "COMMON-LISP-USER")
  "The default and initial package for the REPL.")

(defvar slime-lisp-package
  slime-default-lisp-package
  "The current package name of the Superior lisp.
This is automatically synchronized from Lisp.")

(defvar slime-pid nil
  "The process id of the Lisp process.")

(defvar slime-dont-prompt nil
  "When true, don't prompt the user for input during startup.
This is used for batch-mode testing.")

(defvar slime-truncate-lines t
  "When true, set `truncate-lines' in certain popup buffers.
This applies to buffers that present lines as rows of data, such as
debugger backtraces and apropos listings.")


;;; Customize group

(defgroup slime nil
  "Interfaction with the Superior Lisp Environment."
  :prefix "slime-"
  :group 'applications)

;; XEmacs wants underline to be a boolean.
(defun slime-underline-color (underline)
  (cond ((featurep 'xemacs) (if underline t nil))
        (t underline)))
    
(defface slime-error-face
  `((((class color) (background light))
     (:underline ,(slime-underline-color "red")))
    (((class color) (background dark))
     (:underline ,(slime-underline-color "red")))
    (t (:underline t)))
  "Face for errors from the compiler."
  :group 'slime)

(defface slime-warning-face
  `((((class color) (background light))
     (:underline ,(slime-underline-color "orange")))
    (((class color) (background dark))
     (:underline ,(slime-underline-color "coral")))
    (t (:underline t)))
  "Face for warnings from the compiler."
  :group 'slime)

(defface slime-style-warning-face
  `((((class color) (background light))
     (:underline ,(slime-underline-color "brown")))
    (((class color) (background dark))
     (:underline ,(slime-underline-color "gold")))
    (t (:underline t)))
  "Face for style-warnings from the compiler."
  :group 'slime)

(defface slime-note-face
  `((((class color) (background light))
     (:underline ,(slime-underline-color "brown4")))
    (((class color) (background dark))
     (:underline ,(slime-underline-color "light goldenrod")))
    (t (:underline t)))
  "Face for notes from the compiler."
  :group 'slime)

(defun slime-face-inheritance-possible-p ()
  (assq :inherit custom-face-attributes))

(defface slime-highlight-face
  (cond ((slime-face-inheritance-possible-p)
         '((t (:inherit highlight :underline nil))))
        (t 
         '((((class color) (background light))
            (:background "darkseagreen2"))
           (((class color) (background dark))
            (:background "darkolivegreen"))
           (t (:inverse-video t)))))
  "Face for compiler notes while selected."
  :group 'slime)

(defface slime-repl-output-face
  `((t (:inherit font-lock-string-face)))
  "Face for Lisp output in the SLIME REPL."
  :group 'slime)

(defface slime-repl-input-face
  '((t (:bold t)))
  "Face for previous input in the SLIME REPL."
  :group 'slime)


;;; Minor modes

(define-minor-mode slime-mode
  "\\<slime-mode-map>
SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode).

Commands to compile the current buffer's source file and visually
highlight any resulting compiler notes and warnings:
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

Cross-referencing (see CMUCL manual):
\\[slime-who-calls]	- WHO-CALLS a function.
\\[slime-who-references]	- WHO-REFERENCES a global variable.
\\[slime-who-sets]	- WHO-SETS a global variable.
\\[slime-who-binds]	- WHO-BINDS a global variable.
\\[slime-who-macroexpands]	- WHO-MACROEXPANDS a macro.
C-M-.		- Goto the next reference source location. (Also C-c C-SPC)

Documentation commands:
\\[slime-describe-symbol]	- Describe symbol.
\\[slime-apropos]	- Apropos search.
\\[slime-disassemble-symbol]	- Disassemble a function.

Evaluation commands:
\\[slime-eval-defun]	- Evaluate top-level from containing point.
\\[slime-eval-last-expression]	- Evaluate sexp before point.
\\[slime-pprint-eval-last-expression]	- Evaluate sexp before point, pretty-print result.

\\{slime-mode-map}"
  nil
  nil
  ;; Fake binding to coax `define-minor-mode' to create the keymap
  '((" " 'undefined)))


;;;; inferior-slime-mode
(define-minor-mode inferior-slime-mode
  "\\<slime-mode-map>
Inferior SLIME mode: The Inferior Superior Lisp Mode for Emacs.

This mode is intended for use with `inferior-lisp-mode'. It provides a
subset of the bindings from `slime-mode'.

\\{inferior-slime-mode-map}"
  nil
  nil
  ;; Fake binding to coax `define-minor-mode' to create the keymap
  '((" " 'undefined)))

;; Setup the mode-line to say when we're in slime-mode, and which CL
;; package we think the current buffer belongs to.
(add-to-list 'minor-mode-alist
             '(slime-mode
               (" Slime"
		((slime-buffer-package (":" slime-buffer-package) "")
		 slime-state-name))))

(add-to-list 'minor-mode-alist
             '(inferior-slime-mode
               (" Inf-Slime"
		((slime-buffer-package (":" slime-buffer-package) "")
		 slime-state-name))))

(defun inferior-slime-return ()
  "Handle the return key in the inferior-lisp buffer.
The current input should only be sent if a whole expression has been
entered, i.e. the parenthesis are matched.

A prefix argument disables this behaviour."
  (interactive)
  (if (or current-prefix-arg (inferior-slime-input-complete-p))
      (comint-send-input)
    (insert "\n")
    (inferior-slime-indent-line)))

(defun inferior-slime-indent-line ()
  "Indent the current line, ignoring everything before the prompt."
  (interactive)
  (save-restriction
    (let ((indent-start
           (save-excursion
             (goto-char (process-mark (get-buffer-process (current-buffer))))
             (let ((inhibit-field-text-motion t))
               (beginning-of-line 1))
             (point))))
      (narrow-to-region indent-start (point-max)))
    (lisp-indent-line)))

(defun slime-input-complete-p (start end)
  "Return t if the region from START to END contains a complete sexp."
  (ignore-errors
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char start)
        ;; Keep stepping over blanks and sexps until the end of buffer
        ;; is reached or an error occurs
        (loop do (or (skip-chars-forward " \t\r\n")
                     (looking-at ")"))  ; tollerate extra close parens
              until (eobp)
              do (slime-forward-sexp))
        t))))

(defun inferior-slime-input-complete-p ()
  "Return true if the input is complete in the inferior lisp buffer."
  (slime-input-complete-p (process-mark (get-buffer-process (current-buffer)))
                          (point-max)))

(defun inferior-slime-closing-return ()
  "Send the current expression to Lisp after closing any open lists."
  (interactive)
  (goto-char (point-max))
  (save-restriction
    (narrow-to-region (process-mark (get-buffer-process (current-buffer)))
                      (point-max))
    (while (ignore-errors (save-excursion (backward-up-list 1) t))
      (insert ")")))
  (comint-send-input))


;;;; Key bindings

;; See `slime-define-key' below for keyword meanings.
(defvar slime-keys
  '(;; Compiler notes
    ("\M-p" slime-previous-note)
    ("\M-n" slime-next-note)
    ("\M-c" slime-remove-notes :prefixed t)
    ("\C-k" slime-compile-and-load-file :prefixed t)
    ("\M-k" slime-compile-file :prefixed t)
    ("\C-c" slime-compile-defun :prefixed t)
    ("\C-l" slime-load-file :prefixed t)
    ;; Editing/navigating
    ;; NB: Existing `slime-inspect' binding of \C-c\C-i (i.e. C-TAB)
    ;; clashes with completion! Need a new key for one of them.
    ("\M-\C-i" slime-complete-symbol :inferior t)
    ("\C-i" slime-complete-symbol :prefixed t :inferior t)
    ("\M-." slime-edit-fdefinition :inferior t :sldb t)
    ("\M-," slime-pop-find-definition-stack :inferior t :sldb t)
    ;; Evaluating
    ("\C-x\C-e" slime-eval-last-expression :inferior t)
    ("\C-p" slime-pprint-eval-last-expression :prefixed t :inferior t)
    ("\C-\M-x" slime-eval-defun)
    (":" slime-interactive-eval :prefixed t :sldb t)
    ("\C-z" slime-switch-to-output-buffer :prefixed t :sldb t)
    ("\C-g" slime-interrupt :prefixed t :inferior t :sldb t)
    ;; NB: XEmacs dosn't like \C-g.  Use \C-b as "break" key.
    ("\C-b" slime-interrupt :prefixed t :inferior t :sldb t)
    ("\M-g" slime-quit :prefixed t :inferior t :sldb t)
    ;; Documentation
    (" " slime-space :inferior t)
    ("\C-d" slime-describe-symbol :prefixed t :inferior t :sldb t)
    ("\M-d" slime-disassemble-symbol :prefixed t :inferior t :sldb t)
    ("\C-t" slime-toggle-trace-fdefinition :prefixed t :sldb t)
    ("\C-a" slime-apropos :prefixed t :inferior t :sldb t)
    ("\M-a" slime-apropos-all :prefixed t :inferior t :sldb t)
    ("\C-m" slime-macroexpand-1 :prefixed t :inferior t)
    ("\M-m" slime-macroexpand-all :prefixed t :inferior t)
    ("\M-0" slime-restore-window-configuration :prefixed t :inferior t)
    ("\C-h" hyperspec-lookup :prefixed t :inferior t :sldb t)
    ([(control meta ?\.)] slime-next-location :inferior t)
    ;; Emacs20 on LinuxPPC signals a 
    ;; "Invalid character: 400000040, 2147479172, 0xffffffd8"
    ;; for "\C- ".
    ;; ("\C- " slime-next-location :prefixed t :inferior t)
    ("~" slime-sync-package-and-default-directory :prefixed t :inferior t)
    ;; Cross reference
    ("\C-wc" slime-who-calls :prefixed t :inferior t :sldb t)
    ("\C-wr" slime-who-references :prefixed t :inferior t :sldb t)
    ("\C-wb" slime-who-binds :prefixed t :inferior t :sldb t)
    ("\C-ws" slime-who-sets :prefixed t :inferior t :sldb t)
    ("\C-wm" slime-who-macroexpands :prefixed t :inferior t :sldb t)
    ("<" slime-list-callers :prefixed t :inferior t :sldb t)
    (">" slime-list-callees :prefixed t :inferior t :sldb t)))

;; Maybe a good idea, maybe not..
(defvar slime-prefix-key "\C-c"
  "The prefix key to use in SLIME keybinding sequences.")

(defun* slime-define-key (key command &key prefixed inferior)
  "Define a keybinding of KEY for COMMAND.
If PREFIXED is non-nil, `slime-prefix-key' is prepended to KEY.
If INFERIOR is non-nil, the key is also bound for `inferior-slime-mode'."
  (when prefixed
    (setq key (concat slime-prefix-key key)))
  (define-key slime-mode-map key command)
  (when inferior
    (define-key inferior-slime-mode-map key command)))

(defun slime-init-keymaps ()
  "(Re)initialize the keymaps for `slime-mode' and `inferior-slime-mode'."
  (interactive)
  (loop for (key command . keys) in slime-keys
        do (apply #'slime-define-key key command :allow-other-keys t keys))
  ;; Extras..
  (define-key inferior-slime-mode-map [return] 'inferior-slime-return)
  (define-key inferior-slime-mode-map
    [(control return)] 'inferior-slime-closing-return)
  (define-key inferior-slime-mode-map
    [(meta control ?m)] 'inferior-slime-closing-return))

(slime-init-keymaps)


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
  (when slime-mode
    (when (slime-connected-p)
      (slime-process-available-input))
    (when slime-autodoc-mode
      (slime-autodoc-post-command-hook))))

(defun slime-setup-command-hooks ()
  "Setup a buffer-local `pre-command-hook' to call `slime-pre-command-hook'."
  (make-local-hook 'pre-command-hook)
  (make-local-hook 'post-command-hook)
  (add-hook 'pre-command-hook 'slime-pre-command-hook)
  (add-hook 'post-command-hook 'slime-post-command-hook))

(add-hook 'slime-mode-hook 'slime-setup-command-hooks)
(add-hook 'slime-mode-hook 'slime-buffer-package)
(add-hook 'inferior-lisp-mode-hook 
          (lambda ()
            (add-to-list
             (make-local-variable 'comint-output-filter-functions)
             (lambda (string)
               (unless (get-buffer-window (current-buffer))
                 (display-buffer (current-buffer) t))
               (comint-postoutput-scroll-to-bottom string)))))


;;; Common utility functions and macros

(defmacro* when-let ((var value) &rest body)
  "Evaluate VALUE, and if the result is non-nil bind it to VAR and
evaluate BODY.

\(when-let (VAR VALUE) &rest BODY)"
  `(let ((,var ,value))
     (when ,var ,@body)))

(put 'when-let 'lisp-indent-function 1)

(defmacro with-lexical-bindings (variables &rest body)
  "Execute BODY with VARIABLES in lexical scope."
  `(lexical-let ,(mapcar (lambda (variable) (list variable variable))
                         variables)
     ,@body))

(put 'with-lexical-bindings 'lisp-indent-function 1)

(defmacro destructure-case (value &rest patterns)
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (gensym "op-"))
	(operands (gensym "rand-"))
	(tmp (gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (case ,operator
	 ,@(mapcar (lambda (clause)
                     (if (eq (car clause) t)
                         `(t ,@(cdr clause))
                       (destructuring-bind ((op &rest rands) &rest body) clause
                         `(,op (destructuring-bind ,rands ,operands
                                 . ,body)))))
		   patterns)
	 ,@(if (eq (caar (last patterns)) t)
	       '()
	     `((t (error "destructure-case failed: %S" ,tmp))))))))

(put 'destructure-case 'lisp-indent-function 1)

(defmacro slime-define-keys (keymap &rest key-command)
  `(progn . ,(mapcar (lambda (k-c) `(define-key ,keymap . ,k-c))
		     key-command)))

(put 'slime-define-keys 'lisp-indent-function 1)

(defun slime-buffer-package (&optional dont-cache)
  "Return the Common Lisp package associated with the current buffer.
This is heuristically determined by a text search of the buffer.  The
result is stored in `slime-buffer-package' unless DONT-CACHE is
non-nil.  If the current package cannot be determined fall back to
slime-buffer-package (which may also be nil).

The REPL buffer is a special case: it's package is `slime-lisp-package'."
  (or (and (eq major-mode 'slime-repl-mode) slime-lisp-package)
      (let ((string (slime-find-buffer-package)))
        (cond (string
               (cond (dont-cache)
                     ((equal string slime-buffer-package))
                     (t 
                      (setq slime-buffer-package string)
                      (force-mode-line-update)))
               string)
              (t slime-buffer-package)))))

(defun slime-find-buffer-package ()
  "Figure out which Lisp package the current buffer is associated with."
  (save-excursion
    (when (let ((case-fold-search t)
                (regexp "^(\\(cl:\\|common-lisp:\\)?in-package\\>"))
	    (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t)))
      (goto-char (match-end 0))
      (skip-chars-forward " \n\t\f\r#:")
      (let ((pkg (condition-case nil (read (current-buffer)) (error nil ))))
	(cond ((stringp pkg)
	       pkg)
	      ((symbolp pkg)
	       (symbol-name pkg)))))))

(defun slime-display-buffer-other-window (buffer &optional not-this-window)
  "Display BUFFER in some other window.
Like `display-buffer', but ignores `same-window-buffer-names'."
  (let ((same-window-buffer-names nil))
    (display-buffer buffer not-this-window)))

(defun slime-create-message-window ()
  (let ((previous (previous-window (minibuffer-window))))
    (when (<= (window-height previous) (* 2 window-min-height))
      (save-selected-window 
        (select-window previous)
        (enlarge-window (- (1+ (* 2 window-min-height))
                           (window-height previous)))))
    (split-window previous)))
  
(defun slime-display-message (message buffer-name) 
  "Display MESSAGE in the echo area or in BUFFER-NAME.  Use the echo
area if MESSAGE needs only a single line.  If the MESSAGE requires
more than one line display it in BUFFER-NAME and add a hook to
`slime-pre-command-actions' to remove the window before the next
command."
  (when (get-buffer-window buffer-name) (delete-windows-on buffer-name))
  (cond ((or (string-match "\n" message)
             (> (length message) (1- (frame-width))))
         (lexical-let ((buffer (get-buffer-create buffer-name)))
           (with-current-buffer buffer
             (erase-buffer)
             (insert message)
             (goto-char (point-min))
             (let ((win (slime-create-message-window)))
               (set-window-buffer win (current-buffer))
               (shrink-window-if-larger-than-buffer
                (display-buffer (current-buffer)))))
           (push (lambda () (delete-windows-on buffer) (bury-buffer buffer))
                 slime-pre-command-actions)))
        (t (message "%s" message))))

;; defun slime-message
(if (or (featurep 'xemacs)
	(= emacs-major-version 20))
    ;; XEmacs truncates multi-line messages in the echo area.
    (defun slime-message (fmt &rest args)
      (slime-display-message (apply #'format fmt args) "*SLIME Note*"))
  (defun slime-message (fmt &rest args)
    (apply 'message fmt args)))

(defun slime-set-truncate-lines ()
  "Set `truncate-lines' in the current buffer if
`slime-truncate-lines' is non-nil."
  (when slime-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

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
    (if string (intern (substring-no-properties string)) nil)))

(defun slime-symbol-name-at-point ()
  "Return the name of the symbol at point, otherwise nil."
  (let ((sym (slime-symbol-at-point)))
    (and sym (symbol-name sym))))

(defun slime-sexp-at-point ()
  "Return the sexp at point, otherwise nil."
  (let ((string (thing-at-point 'sexp)))
    (if string (substring-no-properties string) nil)))

(defun slime-function-called-at-point/line ()
  "Return the name of the function being called at point, provided the
function call starts on the same line at the point itself."
  (and (ignore-errors
         (slime-same-line-p (save-excursion (backward-up-list 1) (point))
                            (point)))
       (slime-function-called-at-point)))

(defun slime-read-symbol-name (prompt)
  "Either read a symbol name or choose the one at point.
The user is prompted if a prefix argument is in effect or there is no
symbol at point."
  (or (and (not current-prefix-arg) (slime-symbol-name-at-point))
      (slime-completing-read-symbol-name prompt)))

(defun slime-read-symbol (prompt)
  "Either read a symbol or choose the one at point.
The user is prompted if a prefix argument is in effect or there is no
symbol at point."
  (intern (slime-read-symbol-name prompt)))

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

(put 'slime-with-output-to-temp-buffer 'lisp-indent-function 1)

(defun slime-function-called-at-point ()
  "Return a function around point or else called by the list containing point.
If that doesn't give a function, return nil."
  (ignore-errors
    (save-excursion
      (save-restriction
        (narrow-to-region (max (point-min) (- (point) 1000))
                          (point-max))
        ;; Move up to surrounding paren, then after the open.
        (backward-up-list 1)
        (when (or (ignore-errors
                    ;; "((foo" is probably not a function call
                    (save-excursion (backward-up-list 1)
                                    (looking-at "(\\s *(")))
                  ;; nor is "( foo"
                  (looking-at "([ \t]"))
          (error "Probably not a Lisp function call"))
        (forward-char 1)
        (let ((obj (read (current-buffer))))
          (and (symbolp obj) obj))))))

(defun slime-read-package-name (prompt &optional initial-value)
  (let ((completion-ignore-case t))
    (completing-read prompt (mapcar (lambda (x) (cons x x))
				    (slime-eval 
				     `(swank:list-all-package-names)))
		     nil nil initial-value)))

(defun slime-lisp-package ()
  "Return the name of the current REPL package."
  (or (cdr (assoc slime-lisp-package slime-lisp-preferred-package-nicknames))
      slime-lisp-package))

(defmacro slime-propertize-region (props &rest body)
  (let ((start (gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
	 (add-text-properties ,start (point) ,props)))))

(put 'slime-propertize-region 'lisp-indent-function 1)

(defsubst slime-insert-propertized (props &rest args)
  "Insert all ARGS and then add text-PROPS to the inserted text."
  (slime-propertize-region props (apply #'insert args)))


;;; Inferior CL Setup: compiling and connecting to Swank

(defvar slime-startup-retry-timer nil
  "Timer object while waiting for an inferior-lisp to start.")

(defun slime ()
  "Start an inferior^_superior Lisp and connect to its Swank server."
  (interactive)
  (when (slime-connected-p)
    (slime-disconnect))
  (slime-maybe-start-lisp)
  (setq slime-lisp-package slime-default-lisp-package)
  (slime-read-port-and-connect))

(defun slime-maybe-start-lisp ()
  "Start an inferior lisp unless one is already running."
  (unless (get-buffer-process (get-buffer "*inferior-lisp*"))
    (call-interactively 'inferior-lisp)
    (comint-proc-query (inferior-lisp-proc)
                       (format "(load %S)\n"
                               (concat slime-path slime-backend)))))

(defun slime-start-swank-server ()
  "Start a Swank server on the inferior lisp."
  (comint-proc-query (inferior-lisp-proc)
                     (format "(swank:start-server %S)\n"
                             (slime-swank-port-file))))

(defun slime-swank-port-file ()
  "Filename where the SWANK server writes its TCP port number."
  (format "/tmp/slime.%S" (emacs-pid)))

(defun slime-read-swank-port ()
  "Read the Swank server port number from the `slime-swank-port-file'."
  (save-excursion
    (with-temp-buffer
      (insert-file-contents (slime-swank-port-file))
      (goto-char (point-min))
      (let ((port (read (current-buffer))))
        (assert (integerp port))
        port))))

(defun slime-read-port-and-connect (&optional retries)
  "Connect to a running Swank server."
  (slime-start-swank-server)
  (lexical-let ((retries (or retries slime-swank-connection-retries))
                (attempt 0))
    (labels
        ;; A small one-state machine to attempt a connection with
        ;; timer-based retries.
        ((attempt-connection
          ()
          (unless (active-minibuffer-window)
            (message "\
Polling %S.. (Abort with `M-x slime-disconnect'.)"
                     (slime-swank-port-file)))
          (setq slime-state-name (format "[polling:%S]" (incf attempt)))
          (force-mode-line-update)
          (when slime-startup-retry-timer
            (cancel-timer slime-startup-retry-timer))
          (setq slime-startup-retry-timer nil) ; remove old timer
          (cond ((file-exists-p (slime-swank-port-file))
                 (let ((port (slime-read-swank-port)))
                   (delete-file (slime-swank-port-file))
                   (slime-connect "localhost" port)))
                ((and retries (zerop retries))
                 (message "Failed to connect to Swank."))
                (t
                 (when retries (decf retries))
                 (setq slime-startup-retry-timer
                       (run-with-timer 1 nil #'attempt-connection))))))
      (attempt-connection))))

(defun slime-connect (host port)
  "Connect to a running Swank server"
  (interactive (list (read-from-minibuffer "Host: " "localhost")
                     (read-from-minibuffer "Port: " "4005" nil t)))
  (message "Connecting to Swank on port %S.." port)
  (slime-net-connect "localhost" port)
  (slime-init-connection)
  (pop-to-buffer (slime-output-buffer))
  (message "Connected to Swank server on port %S. %s"
           port (slime-random-words-of-encouragement)))

(defun slime-disconnect ()
  "Disconnect from the Swank server."
  (interactive)
  (cond ((slime-connected-p)
         (kill-buffer (process-buffer slime-net-process))
         (delete-process slime-net-process)
         (message "Disconnected."))
        (slime-startup-retry-timer
         (cancel-timer slime-startup-retry-timer)
         (message "Cancelled connection attempt."))
        (t
         (message "Not connected."))))

(defun slime-init-connection ()
  (slime-init-dispatcher)
  (setq slime-pid (slime-eval '(swank:getpid))))

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
  "The process (socket) connected to the CL.")

(defun slime-net-connect (host port)
  "Establish a connection with a CL."
  (setq slime-net-process
        (open-network-stream "SLIME Lisp" nil host port))
  (let ((buffer (slime-make-net-buffer " *cl-connection*")))
    (set-process-buffer slime-net-process buffer)
    (set-process-filter slime-net-process 'slime-net-filter)
    (set-process-sentinel slime-net-process 'slime-net-sentinel)
    (when (fboundp 'set-process-coding-system)
      (set-process-coding-system slime-net-process 
                                 'no-conversion 'no-conversion)))
  slime-net-process)
    
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
  "Send a SEXP to inferior CL.
This is the lowest level of communication. The sexp will be READ and
EVAL'd by Lisp."
  (let* ((msg (format "%S\n" sexp))
	 (string (concat (slime-net-enc3 (length msg)) msg)))
    (process-send-string slime-net-process (string-make-unibyte string))))

(defun slime-net-sentinel (process message)
  (when (ignore-errors (eq (process-status (inferior-lisp-proc)) 'open))
    (message "Lisp connection closed unexpectedly: %s" message))
  (setq slime-state-name "[not connected]")
  (force-mode-line-update)
  (ignore-errors (kill-buffer (process-buffer slime-net-process))))

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
    (unwind-protect
        (while (slime-net-have-input-p)
          (let ((event (condition-case error
                           (slime-net-read)
                         (error (slime-state/event-panic error)))))
            (save-current-buffer (slime-dispatch-event event))))
      (when (slime-net-have-input-p)
        (run-at-time 0 nil 'slime-process-available-input)))))

(defun slime-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 3)
       (>= (- (buffer-size) 3) (slime-net-read3))))

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

;; The SLIME protocol is implemented with a small state machine. That
;; means the program uses "state" data structures to remember where
;; it's up to -- whether it's idle, or waiting for an evaluation
;; request from Lisp, whether it's debugging, and so on.
;;
;; The state machine has a stack for putting states that are only
;; partially complete, i.e. it is a "push-down automaton" like they
;; use in parsers. This design works well because the SLIME protocol
;; can be described as a context-free grammar, loosely:
;;
;;   CONVERSATION ::= <EXCHANGE>*
;;   EXCHANGE     ::= request reply
;;                or  request <DEBUG> reply
;;   DEBUG        ::= enter-debugger <CONVERSATION> debug-return
;;
;; Or, in plain english, in the simplest case Emacs asks Lisp to
;; evaluate something and Lisp sends the result. But it's also
;; possible that Lisp signals a condition and enters the debugger
;; while computing the reply. In that case both sides enter a
;; debugging state, and can have arbitrary nested conversations until
;; a restart makes the debugger return.
;;
;; The state machine's stack represents the interesting parts of the
;; remote Lisp stack. Each Emacs state on the stack corresponds to a
;; particular Lisp stack frame. When that frame returns it sends a
;; message to Emacs delivering a result, which Emacs delivers to the
;; state and pops its stack. So the stacks are kept synchronized.
;;
;; The format of events is lists whose CAR is a symbol identifying the
;; type of event and whose CDR contains any extra arguments. We treat
;; events created by Emacs the same as events sent by Lisp, but by
;; convention use "emacs-" as a prefix on the names of events
;; originating locally in Emacs.
;;
;; There are also certain "out of band" messages which are handled by
;; a special function instead of reaching the state machine.


;;;; Basic state machine framework

(defvar slime-state-stack '()
  "Stack of machine states. The state at the top is the current state.")

(defvar slime-state-name "[??]"
  "The name of the current state, for display in the modeline.")

(defun slime-push-state (state)
  "Push into a new state, saving the current state on the stack.
This may be called by a state machine to cause a state change."
  (push state slime-state-stack)
  (slime-activate-state))

(defun slime-pop-state ()
  "Pop back to the previous state from the stack.
This may be called by a state machine to finish its current state."
  (pop slime-state-stack)
  (slime-activate-state))

(defun slime-current-state ()
  "The current state."
  (car slime-state-stack))

(defun slime-init-dispatcher ()
  "Initialize the stack machine."
  (setq sldb-level 0)
  (setq slime-state-stack (list (slime-idle-state)))
  (sldb-cleanup))

(defun slime-activate-state ()
  "Activate the current state.
This delivers an (activate) event to the state function, and updates
the state name for the modeline."
  (let ((state (slime-current-state)))
    (setq slime-state-name
          (ecase (slime-state-name state)
            (slime-idle-state "")
            (slime-evaluating-state "[eval...]")
            (slime-debugging-state "[debug]")
            (slime-read-string-state "[read]")))
    (force-mode-line-update)
    (slime-dispatch-event '(activate))))

;; state datastructure
(defun slime-make-state (name function)
  "Make a state object called NAME that handles events with FUNCTION."
  (list 'slime-state name function))

(defun slime-state-name (state)
  "Return the name of STATE."
  (second state))

(defun slime-state-function (state)
  "Return STATE's event-handler function."
  (third state))


;;;;; Event dispatching.

(defun slime-dispatch-event (event)
  "Dispatch an event to the current state.
Certain \"out of band\" events are handled specially instead of going
into the state machine."
  (slime-log-event event)
  (or (slime-handle-oob event)
      (funcall (slime-state-function (slime-current-state)) event)))

(defun slime-handle-oob (event)
  "Handle out-of-band events.
Return true if the event is recognised and handled."
  (destructure-case event
    ((:read-output output)
     (slime-output-string output)
     t)
    ((:new-package package)
     (setq slime-lisp-package package)
     t)
    ((:new-features features)
     (setq slime-lisp-features features)
     t)
    (t nil)))

(defun slime-state/event-panic (event)
  "Signal the error that we received EVENT in a state that can't handle it.
When this happens it is due to a bug in SLIME.

The connection to Lisp is dropped, the user is presented with some
debugging information, and an error is signaled."
  (with-output-to-temp-buffer "*SLIME bug*"
    (princ (format "\
You have encountered a bug in SLIME.

The communication state machine received an event that was illegal for
its current state, which means that the communication between Emacs
and Lisp has lost synchronization. The connection to Lisp has
therefore been closed.

You can open a fresh connection with `M-x slime'.

Please report this problem to your friendly neighbourhood SLIME
hacker, or the mailing list at slime-devel@common-lisp.net. Please
include in your report:

  A description of what you were doing when the problem occured,
  the version of SLIME, Emacs, and Lisp that you are using,
  the Lisp backtrace, if one was printed,
  and the information printed below:

The event was:
%s

The state stack was:
%s

The content of the *slime-events* buffer:
%s

The content of the *cl-connection* buffer:
%s

"
                   (pp-to-string event)
                   (pp-to-string (mapcar 'slime-state-name
                                         slime-state-stack))
                   (cond ((get-buffer "*slime-events*")
                          (with-current-buffer "*slime-events*"
                            (buffer-string)))
                         (t "<no *slime-event* buffer>"))
                   (cond ((process-buffer slime-net-process)
                          (with-current-buffer 
                              (process-buffer slime-net-process)
                            (buffer-string)))
                         (t "<no *cl-connection*>"))
                   )))
  (slime-disconnect)
  (delete-other-windows (get-buffer-window "*SLIME bug*"))
  (unwind-protect (unwind-protect (sit-for 2)
                    (display-buffer "*SLIME bug*")
                    (delete-other-windows (get-buffer-window "*SLIME bug*")))
    (error "The SLIME protocol reached an inconsistent state.")))


;;;;; Event logging to *slime-events*
(defun slime-log-event (event)
  (with-current-buffer (slime-events-buffer)
    ;; trim?
    (when (> (buffer-size) 100000)
      (goto-char (/ (buffer-size) 2))
      (beginning-of-defun)
      (delete-region (point-min) (point)))
    (goto-char (point-max))
    (save-excursion
      (pp event (current-buffer))
      (when (equal event '(activate))
        (backward-char 1)
        (insert (format " ; %s" (slime-state-name (slime-current-state))))))
    (hs-hide-block-at-point)
    (goto-char (point-max))))

(defun slime-events-buffer ()
  (or (get-buffer "*slime-events*")
      (let ((buffer (get-buffer-create "*slime-events*")))
        (with-current-buffer buffer
          (lisp-mode)
          (hs-minor-mode)
          (set (make-local-variable 'hs-block-start-regexp) "^(")
          (setq font-lock-defaults nil)
          (current-buffer)))))


;;;; Upper layer macros for defining states

(eval-when (compile eval)
  (defun slime-make-state-function (arglist clauses)
    "Build the function that implements a state.
The state's variables are moved into lexical bindings."
    (let ((event-var (gensym "event-")))
      `(lexical-let ,(mapcar* #'list arglist arglist)
	 (lambda (,event-var)
	   (destructure-case ,event-var
	     ,@clauses
	     ;; Every state can handle the event (activate). By default
	     ;; it does nothing.
	     ,@(if (member* '(activate) clauses :key #'car :test #'equal)
		   '()
		 '( ((activate) nil)) )
	     (t
              ;; Illegal event for current state. This is a BUG!
              (slime-state/event-panic ,event-var))))))))

(defmacro slime-defstate (name variables doc &rest events)
  "Define a state called NAME and comprised of VARIABLES.
DOC is a documentation string.
EVENTS is a set of event-handler patterns for matching events with
their actions. The pattern syntax is the same as `destructure-case'."
  `(defun ,name ,variables
     ,doc
     (slime-make-state ',name ,(slime-make-state-function variables events))))

(put 'slime-defstate 'lisp-indent-function 2)


;;;; The SLIME state machine definition

(defvar sldb-level 0
  "Current debug level, or 0 when not debugging.")

(defvar sldb-level-in-buffer nil
  "Buffer local variable in sldb buffer.")

(defvar slime-stack-eval-tags nil
  "List of stack-tags of continuations waiting on the stack.")

(slime-defstate slime-idle-state ()
  "Idle state. The only event allowed is to make a request."
  ((activate)
   (assert (= sldb-level 0))
   (slime-repl-maybe-prompt))
  ((:emacs-evaluate form-string package-name continuation)
   (slime-output-evaluate-request form-string package-name)
   (slime-push-state (slime-evaluating-state continuation))))

(defvar slime-evaluating-state-activation-hook nil
  "Hook called when the evaluating state is actived.")

(slime-defstate slime-evaluating-state (continuation)
  "Evaluting state.
We have asked Lisp to evaluate a form, and when the result arrives we
will pass it to CONTINUATION."
  ((activate)
   (run-hooks 'slime-evaluating-state-activation-hook))
  ((:ok result)
   (slime-pop-state)
   (destructure-case continuation
     ((:function f)
      (funcall f result))
     ((:catch-tag tag)
      (when (member tag slime-stack-eval-tags)
	(throw tag `(:ok ,result))))))
  ((:aborted)
   (destructure-case continuation
     ((:function f)
      (message "Evaluation aborted.")
      (slime-pop-state))
     ((:catch-tag tag)
      (slime-pop-state)
      (when (member tag slime-stack-eval-tags)
	(throw tag `(:aborted))))))
  ((:debug level condition restarts frames)
   (slime-push-state
    (slime-debugging-state level condition restarts frames
                           (current-window-configuration))))
  ((:emacs-interrupt)
   (slime-send-sigint))
  ((:emacs-quit)
   ;; To discard the state would break our synchronization.
   ;; Instead, just cancel the continuation.
   (setq continuation (lambda (value) t)))
  ((:read-string tag)
   (slime-push-state (slime-read-string-state tag))))

(slime-defstate slime-debugging-state (level condition restarts frames
                                             saved-window-configuration)
  "Debugging state.
Lisp entered the debugger while handling one of our requests. This
state interacts with it until it is coaxed into returning."
  ((activate)
   (setq sldb-level level)
   (let ((sldb-buffer (get-buffer "*sldb*")))
     (when (or (not sldb-buffer)
               (with-current-buffer sldb-buffer
                 (/= sldb-level-in-buffer level)))
       (sldb-setup condition restarts frames))))
  ((:debug-return level)
   (assert (= level sldb-level))
   (sldb-cleanup)
   (decf sldb-level)
   (set-window-configuration saved-window-configuration)
   (slime-pop-state))
  ((:emacs-evaluate form-string package-name continuation)
   ;; recursive evaluation request
   (slime-output-evaluate-request form-string package-name)
   (slime-push-state (slime-evaluating-state continuation))))

(slime-defstate slime-read-string-state (tag)
  "Reading state.
Lisp waits for input from Emacs."
  ((activate)
   (slime-repl-read-string))
  ((:emacs-return-string code)
   (slime-net-send `(swank:take-input ,tag ,code))
   (slime-pop-state))
  ((:emacs-evaluate form-string package-name continuation)
   (slime-output-evaluate-request form-string package-name)
   (slime-push-state (slime-evaluating-state continuation)))
  ((:read-aborted)
   (slime-repl-abort-read)
   (slime-pop-state)))


;;;; Utilities

(defun slime-output-evaluate-request (form-string package-name)
  "Send a request for LISP to read and evaluate FORM-STRING in PACKAGE-NAME."
  (slime-net-send `(swank:eval-string ,form-string ,package-name)))

(defun slime-check-connected ()
  (unless (slime-connected-p)
    (error "Not connected. Use `M-x slime' to start a Lisp.")))

(defun slime-connected-p ()
  "Return true if the Swank connection is open."
  (and slime-net-process
       (eq (process-status slime-net-process) 'open)))

(defun slime-eval-string-async (string package continuation)
  (when (slime-busy-p)
    (error "Lisp is already busy evaluating a request."))
  (slime-dispatch-event `(:emacs-evaluate ,string ,package ,continuation)))

(defconst +slime-sigint+ 2)

(defun slime-send-sigint ()
  (signal-process slime-pid +slime-sigint+))


;;;; Emacs Lisp programming interface

(defun slime-eval (sexp &optional package)
  "Evaluate EXPR on the superior Lisp and return the result."
  (slime-check-connected)
  (let* ((tag (gensym "slime-result-"))
	 (slime-stack-eval-tags (cons tag slime-stack-eval-tags)))
    (destructure-case
	(catch tag (slime-do-eval sexp package `(:catch-tag ,tag)))
      ((:ok value)
       value)
      ((:aborted)
       (error "Lisp Evaluation aborted.")))))

(defun slime-do-eval (sexp package continuation)
  "Perform an evaluation synchronously.
Loops until the result is thrown to our caller, or the user aborts."
  (slime-eval-string-async (prin1-to-string sexp) package continuation)
  (let ((debug-on-quit t)
        (inhibit-quit nil))
    (while (slime-busy-p)
      (accept-process-output)))
  ;; No longer busy, but result not delivered. That means we have
  ;; entered the debugger.
  (recursive-edit)
  ;; If we get here, the user completed the recursive edit without
  ;; coaxing the debugger into returning. We abort.
  (error "Evaluation aborted."))

(defun slime-eval-async (sexp package cont)
  "Evaluate EXPR on the superior Lisp and call CONT with the result."
  (slime-check-connected)
  (slime-eval-string-async (prin1-to-string sexp) package `(:function ,cont)))

(defun slime-sync ()
  "Block until any asynchronous command has completed."
  (while (slime-busy-p)
    (accept-process-output slime-net-process)))

(defun slime-busy-p ()
  "Return true if Lisp is busy processing a request."
  (eq (slime-state-name (slime-current-state)) 'slime-evaluating-state))

(defun slime-evaluating-p ()
  "Return true if Lisp is evaluating a request for Emacs."
  (slime-busy-p))

(defun slime-idle-p ()
  "Return true if Lisp is idle."
  (eq (slime-state-name (slime-current-state)) 'slime-idle-state))

(defun slime-reading-p ()
  "Return true if Lisp waits for input from Emacs."
  (eq (slime-state-name (slime-current-state)) 'slime-read-string-state))

(defun slime-debugging-p ()
  "Return true if Lisp is in the debugger."
  (eq (slime-state-name (slime-current-state)) 'slime-debugging-state))

(defun slime-ping ()
  "Check that communication works."
  (interactive)
  (message "%s" (slime-eval "PONG")))


;;; Stream output

(defvar slime-last-output-start (make-marker)
  "Marker for the start of the output for the evaluation.")

(defun slime-output-buffer ()
  "Return the output buffer, create it if necessary."
  (or (get-buffer "*slime-repl*")
      (with-current-buffer (get-buffer-create "*slime-repl*")
	(slime-repl-mode)
        (slime-repl-insert-prompt)
	(current-buffer))))

(defun slime-insert-transcript-delimiter (string)
  (with-current-buffer (slime-output-buffer)
    (goto-char (point-max))
    (slime-repl-maybe-insert-output-separator)
    (slime-insert-propertized
     '(slime-transcript-delimiter t)
     ";;;; " 
     (subst-char-in-string ?\n ?\ 
			   (substring string 0 
				      (min 60 (length string))))
     " ...\n")
    (set-marker slime-last-output-start (point) (current-buffer))))

(defun slime-show-last-output ()
  (with-current-buffer (slime-output-buffer)
    (let ((start slime-last-output-start)
          (end slime-repl-prompt-start-mark))
      (when (< start end)
        (slime-display-buffer-region (current-buffer) start 
                                     slime-repl-input-start-mark)))))

(defun slime-with-output-at-eob (fn)
  "Call FN at the eob.  In a save-excursion block if we are not at
eob."
  (cond ((eobp) (funcall fn) 
         (when-let (w (get-buffer-window (current-buffer)))
           (set-window-point w (point))))
        (t (save-excursion 
             (goto-char (point-max))
             (funcall fn)))))

(defun slime-output-string (string)
  (with-current-buffer (slime-output-buffer)
    (slime-with-output-at-eob
     (lambda ()
       (slime-repl-maybe-insert-output-separator)
       (slime-propertize-region '(face slime-repl-output-face)
         (insert string))))))

(defun slime-switch-to-output-buffer ()
  "Select the output buffer, preferably in a different window."
  (interactive)
  (set-buffer (slime-output-buffer))
  (unless (eq (current-buffer) (window-buffer))
    (pop-to-buffer (current-buffer) t))
  (goto-char (point-max)))

(defun slime-show-output-buffer ()
  (slime-show-last-output)
  (with-current-buffer (slime-output-buffer)
    (display-buffer (slime-output-buffer) t)))


;;; REPL

(defvar slime-repl-input-history '()
  "History list of strings read from the REPL buffer.")

(defvar slime-repl-input-history-position 0)
(defvar slime-repl-mode-map)

(defvar slime-repl-prompt-start-mark (make-marker))
(defvar slime-repl-input-start-mark (make-marker))
(defvar slime-repl-input-end-mark (let ((m (make-marker)))
                                    (set-marker-insertion-type m t)
                                    m))

(defun slime-repl-mode () 
  "Major mode for interacting with a superior Lisp.
\\{slime-repl-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'slime-repl-mode)
  (use-local-map slime-repl-mode-map)
  (lisp-mode-variables t)
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function)
  (setq font-lock-defaults nil)
  (setq mode-name "REPL")
  (set (make-local-variable 'scroll-conservatively) 20)
  (set (make-local-variable 'scroll-margin) 0)
  (slime-setup-command-hooks)
  (run-hooks 'slime-repl-mode-hook))

(defun slime-repl-insert-prompt ()
  (unless (bolp) (insert "\n"))
  (let ((start (point)))
    (slime-propertize-region
        '(face font-lock-keyword-face 
               read-only t
               intangible t
               ;; emacs stuff
               rear-nonsticky (slime-repl-prompt read-only face intangible)
               ;; xemacs stuff
               start-open t end-open t)
      (insert (slime-lisp-package) "> "))
    (set-marker slime-repl-prompt-start-mark start (current-buffer))
    (set-marker slime-repl-input-start-mark (point) (current-buffer))
    (set-marker slime-repl-input-end-mark (point) (current-buffer))))

(defun slime-repl-maybe-prompt ()
  "Insert a prompt if there is none."
  (with-current-buffer (slime-output-buffer)
    (unless (= (point-max) slime-repl-input-end-mark)
      (slime-with-output-at-eob
       (lambda ()
         (slime-repl-insert-prompt))))))

(defun slime-repl-current-input ()
  "Return the current input as string.  The input is the region from
after the last prompt to the end of buffer."
  (buffer-substring-no-properties slime-repl-input-start-mark
                                  slime-repl-input-end-mark))

(defun slime-repl-add-to-input-history (string)
  (unless (equal string (car slime-repl-input-history))
    (push string slime-repl-input-history))
  (setq slime-repl-input-history-position -1))
  
(defun slime-repl-eval-string (string)
  (slime-eval-async `(swank:listener-eval ,string)
                    slime-lisp-package
                    (slime-repl-show-result-continutation)))

(defun slime-repl-send-string (string)
  (slime-repl-add-to-input-history string)
  (ecase (slime-state-name (slime-current-state))
    (slime-idle-state (slime-repl-eval-string string))
    (slime-read-string-state (slime-repl-return-string (concat string "\n")))))

(defun slime-repl-show-result-continutation ()
  ;; This is called _after_ the idle state is activated.  This means
  ;; the prompt is already printed.
  (lambda (result)
    (with-current-buffer (slime-output-buffer)
      (save-excursion
        (goto-char slime-repl-prompt-start-mark)
        (insert result "\n")))))

(defun slime-repl-maybe-insert-output-separator ()
  "Insert a newline at point, if we are the end of the input."
  (when (= (point) slime-repl-input-end-mark)
    (insert "\n")
    (set-marker slime-repl-input-end-mark (1- (point)) (current-buffer))
    (set-marker slime-last-output-start (point))))

(defun slime-repl-bol ()
  "Go to the beginning of line or the prompt."
  (interactive)
  (if (and (>= (point) slime-repl-input-start-mark)
           (slime-same-line-p (point) slime-repl-input-start-mark))
      (goto-char slime-repl-input-start-mark)
    (beginning-of-line 1)))
        
(defun slime-repl-return ()
  "Evaluate the current input string, or insert a newline.  
Send the current input ony if a whole expression has been entered,
i.e. the parenthesis are matched. 

With prefix argument send the input even if the parenthesis are not
balanced."
  (interactive)
  (unless (or (slime-idle-p)
              (slime-reading-p))
    (error "Lisp is not ready for requests from the REPL."))
  (if (or current-prefix-arg
          (slime-input-complete-p slime-repl-input-start-mark 
                                  slime-repl-input-end-mark))
      (slime-repl-send-input)
    (slime-repl-newline-and-indent)))

(defun slime-repl-send-input ()
  "Goto to the end of the input and send the current input."
  (let ((input (slime-repl-current-input)))
    (goto-char slime-repl-input-end-mark)
    (slime-repl-maybe-insert-output-separator)
    (add-text-properties slime-repl-input-start-mark
                         slime-repl-input-end-mark
                         '(face slime-repl-input-face))
    (slime-repl-send-string input)))

(defun slime-repl-closing-return ()
  "Evaluate the current input string after closing all open lists."
  (interactive)
  (goto-char (point-max))
  (save-restriction
    (narrow-to-region slime-repl-input-start-mark (point))
    (while (ignore-errors (save-excursion (backward-up-list 1)) t)
      (insert ")")))
  (slime-repl-return))

(defun slime-repl-newline-and-indent ()
  "Insert a newline, then indent the next line.
Restrict the buffer from the prompt for indentation, to avoid being
confused by strange characters (like unmatched quotes) appearing
earlier in the buffer."
  (interactive)
  (save-restriction
    (narrow-to-region slime-repl-prompt-start-mark (point-max))
    (insert "\n")
    (lisp-indent-line)))

(defun slime-repl-delete-current-input ()
  (delete-region slime-repl-input-start-mark slime-repl-input-end-mark))

(defun slime-repl-replace-input (string)
  (slime-repl-delete-current-input)
  (insert-and-inherit string))

;;; Scratch

(defvar slime-scratch-mode-map)
(setq slime-scratch-mode-map (make-sparse-keymap))
(set-keymap-parent slime-scratch-mode-map lisp-mode-map)

(defun slime-scratch-buffer ()
  "Return the scratch buffer, create it if necessary."
  (or (get-buffer "*slime-scratch*")
      (with-current-buffer (get-buffer-create "*slime-scratch*")
	(lisp-mode)
	(use-local-map slime-scratch-mode-map)
	(slime-mode t)
	(current-buffer))))

(defun slime-switch-to-scratch-buffer ()
  (set-buffer (slime-scratch-buffer))
  (unless (eq (current-buffer) (window-buffer))
    (pop-to-buffer (current-buffer) t)))

(defun slime-scratch ()
  (interactive)
  (slime-switch-to-scratch-buffer))

(slime-define-keys slime-scratch-mode-map
  ("\C-j" 'slime-eval-print-last-expression))


;;;; History

(defvar slime-repl-history-pattern nil
  "The regexp most recently used for finding input history.")

(defun slime-repl-history-replace (direction regexp)
  "Replace the current input with the next line in DIRECTION matching REGEXP.
DIRECTION is 'forward' or 'backward' (in the history list)."
  (let* ((step (ecase direction
                 (forward -1)
                 (backward 1)))
         (history-pos0 slime-repl-input-history-position))
    (setq slime-repl-history-pattern regexp)
    ;; Loop through the history list looking for a matching line
    (loop for pos = (+ history-pos0 step) then (+ pos step)
          while (and (<= 0 pos)
                     (< pos (length slime-repl-input-history)))
          do (let ((string (nth pos slime-repl-input-history)))
               (when (and (string-match regexp string)
                          (not (string= string (slime-repl-current-input))))
                 (slime-repl-replace-input string)
                 (setq slime-repl-input-history-position pos)
                 (return)))
          finally (message "End of history; no matching item"))))

(defun slime-repl-matching-input-regexp ()
  (if (memq last-command
            '(slime-repl-previous-input slime-repl-next-input))
      slime-repl-history-pattern
    (concat "^" (regexp-quote (slime-repl-current-input)))))

(defun slime-repl-previous-input ()
  (interactive)
  (slime-repl-history-replace 'backward (slime-repl-matching-input-regexp)))

(defun slime-repl-next-input ()
  (interactive)
  (slime-repl-history-replace 'forward (slime-repl-matching-input-regexp)))

(defun slime-repl-previous-matching-input (regexp)
  (interactive "sPrevious element matching (regexp): ")
  (slime-repl-history-replace 'backward regexp))

(defun slime-repl-next-matching-input (regexp)
  (interactive "sNext element matching (regexp): ")
  (slime-repl-history-replace 'forward regexp))

(defun slime-repl ()
  (interactive)
  (slime-switch-to-output-buffer))

(setq slime-repl-mode-map (make-sparse-keymap))
(set-keymap-parent slime-repl-mode-map lisp-mode-map)

(dolist (spec slime-keys)
  (destructuring-bind (key command &key inferior prefixed 
                           &allow-other-keys) spec
    (when inferior
      (let ((key (if prefixed (concat slime-prefix-key key) key)))
        (define-key slime-repl-mode-map key command)))))

(slime-define-keys slime-repl-mode-map
  ("\C-m" 'slime-repl-return)
  ("\C-j" 'slime-repl-newline-and-indent)
  ("\C-\M-m" 'slime-repl-closing-return)
  ([(control return)] 'slime-repl-closing-return)
  ("\C-a" 'slime-repl-bol)
  ("\M-p" 'slime-repl-previous-input)
  ("\M-n" 'slime-repl-next-input)
  ("\M-r" 'slime-repl-previous-matching-input)
  ("\M-s" 'slime-repl-next-matching-input)
  ("\C-c\C-c" 'slime-interrupt)
  ("\C-c\C-g" 'slime-interrupt)
  ("\t"   'slime-complete-symbol)
  (" "    'slime-space)
  ("\C-\M-x" 'slime-eval-defun)
  )

(define-minor-mode slime-repl-read-mode 
  "Mode the read input from Emacs
\\{slime-repl-read-mode-map}"
  nil
  "[read]"
  '(("\C-m" . slime-repl-return)
    ("\C-c\C-b" . slime-repl-read-break)
    ("\C-c\C-c" . slime-repl-read-break)
    ("\C-c\C-g" . slime-repl-read-break)))

(defun slime-repl-read-string ()
  (slime-switch-to-output-buffer)
  (set-marker slime-repl-input-start-mark (point) (current-buffer))
  (set-marker slime-repl-input-end-mark (point) (current-buffer))
  (slime-repl-read-mode t))

(defun slime-repl-return-string (string)
  (slime-dispatch-event `(:emacs-return-string ,string))
  (slime-repl-read-mode nil))

(defun slime-repl-read-break ()
  (interactive)
  (slime-eval-async `(cl:break) nil (lambda (_))))

(defun slime-repl-abort-read ()
  (with-current-buffer (slime-output-buffer)
    (slime-repl-read-mode nil)
    (slime-repl-maybe-insert-output-separator)
    (message "Read aborted")))


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
  (message "Compiling %s.." (buffer-file-name))
  (with-current-buffer (slime-output-buffer)
    (goto-char (point-max))
    (display-buffer (current-buffer) t)))

(defun slime-compile-defun ()
  "Compile the current toplevel form."
  (interactive)
  (slime-compile-string (slime-defun-at-point)
                        (save-excursion 
                          (end-of-defun)
                          (beginning-of-defun)
                          (point))))

(defun slime-compile-region (start end)
  "Compile the region."
  (interactive "r")
  (slime-compile-string (buffer-substring-no-properties start end) start))

(defun slime-compile-string (string start-offset)
  (slime-eval-async 
   `(swank:swank-compile-string ,string ,(buffer-name) ,start-offset)
   (slime-buffer-package)
   (slime-compilation-finished-continuation)))

(defvar slime-hide-style-warning-count-if-zero t)

(defun slime-note-count-string (severity count &optional suppress-if-zero)
  (cond ((and (zerop count) suppress-if-zero)
         "")
        (t (format "%2d %s%s " count severity (if (= count 1) "" "s")))))

(defun slime-show-note-counts (notes &optional secs)
  (loop for note in notes 
	for severity = (plist-get note :severity)
	count (eq :error severity) into errors
	count (eq :warning severity) into warnings
        count (eq :style-warning severity) into style-warnings
	count (eq :note severity) into notes
	finally 
	(message 
	 "Compilation finished:%s%s%s%s%s"
         (slime-note-count-string "error" errors)
         (slime-note-count-string "warning" warnings)
         (slime-note-count-string "style-warning" style-warnings 
                                  slime-hide-style-warning-count-if-zero)
         (slime-note-count-string "note" notes)
         (if secs (format "[%s secs]" secs) ""))))

(defun slime-compilation-finished (result buffer)
  (with-current-buffer buffer
    (multiple-value-bind (result secs) result
      (let ((notes (slime-compiler-notes)))
	(slime-show-note-counts notes secs)
	(slime-highlight-notes notes)))))

(defun slime-compilation-finished-continuation ()
  (lexical-let ((buffer (current-buffer)))
    (lambda (result) 
      (slime-compilation-finished result buffer))))

(defun slime-highlight-notes (notes)
  "Highlight compiler notes, warnings, and errors in the buffer."
  (interactive (list (slime-compiler-notes)))
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
  SEVERITY   - for future reference, :NOTE, :STYLE-WARNING, :WARNING, or :ERROR.
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
    (putp 'help-echo (concat (getp 'help-echo) "\n" message))))

(defun slime-choose-overlay-region (note)
  "Choose the start and end points for an overlay over NOTE.
If the location's sexp is a list spanning multiple lines, then the
region around the first element is used."
  (let ((location (getf note :location)))
    (unless (equal location '(:null))
      (slime-goto-source-location location)))
  (let ((start (point)))
    (slime-forward-sexp)
    (if (slime-same-line-p start (point))
        (values start (point))
      (values (1+ start)
              (progn (goto-char (1+ start))
                     (forward-sexp 1)
                     (point))))))

(defun slime-same-line-p (pos1 pos2)
  "Return true if buffer positions PoS1 and POS2 are on the same line."
  (save-excursion (goto-char (min pos1 pos2))
                  (not (search-forward "\n" (max pos1 pos2) t))))

(defun slime-severity-face (severity)
  "Return the name of the font-lock face representing SEVERITY."
  (ecase severity
    (:error         'slime-error-face)
    (:warning       'slime-warning-face)
    (:style-warning 'slime-style-warning-face)
    (:note          'slime-note-face)))

(defun slime-most-severe (sev1 sev2)
  "Return the most servere of two conditions.
Severity is ordered as :NOTE < :STYLE-WARNING < :WARNING < :ERROR."
                                        ; Well, not exactly Smullyan..
  (let ((order '(:note :style-warning :warning :error)))
    (if (>= (position sev1 order) 
            (position sev2 order))
        sev1
      sev2)))

(defun slime-visit-source-path (source-path)
  "Visit a full source path including the top-level form."
  (goto-char (point-min))
  (slime-forward-source-path source-path))

(defun slime-forward-positioned-source-path (source-path)
  "Move forward through a sourcepath from a fixed position.
The point is assumed to already be at the outermost sexp, making the
first element of the source-path redundant."
  (ignore-errors (down-list 1))
  (slime-forward-source-path (cdr source-path)))

(defun slime-forward-source-path (source-path)
  (let ((origin (point)))
    (condition-case nil
        (progn
          (loop for (count . more) on source-path
                do (progn
                     (slime-forward-sexp count)
                     (when more (down-list 1))))
          ;; Align at beginning
          (slime-forward-sexp)
          (beginning-of-sexp))
      (error (goto-char origin)))))

(defun slime-goto-source-location (location)
  "Move to the source location LOCATION.

LOCATION is a plist and defines a position in a buffer.  Several kinds
of locations are supported:

 (:file ,filename ,position)             -- A position in a file.
 (:emacs-buffer ,buffername ,position)   -- A position in a buffer.
 (:sexp ,string)                         -- A sexp where no file is available."
  (destructure-case location
    ((:file filename position)
     (set-buffer (find-file-noselect filename t))
     (goto-char position))
    ((:emacs-buffer buffer position)
     (set-buffer buffer)
     (goto-char position))
    ((:sexp string)
     (with-output-to-temp-buffer "*SLIME SEXP*"
       (princ string)))
    ((:openmcl filename function-name)
     (set-buffer (find-file-noselect filename t))
     (ignore-errors
       (goto-char (point-min))
       (re-search-forward (format "^(def\\w+\\s +%s\\s +" function-name)
       (beginning-of-line))))
    ((:sbcl 
      &key from buffername buffer-offset 
      filename position info source-path path source-form function-name)
     (cond (function-name
            (ignore-errors
              (goto-char (point-min))
              (re-search-forward (format "^(def\\w+\\s +%s\\s +"
                                         function-name))
              (beginning-of-line)))
           ((and (eq filename :lisp) (not buffername))
            (beginning-of-defun))
           (t
            (cond (buffername
                   (set-buffer buffername) (goto-char buffer-offset))
                  (filename
                   (set-buffer (find-file-noselect filename))
                   (when position (goto-char position))))
            (cond (path 
                   (slime-forward-source-path (cdr path)))
                  (source-path
                   (slime-forward-positioned-source-path source-path))
                  (t
                   (forward-sexp) (backward-sexp))))))))

(defmacro slime-point-moves-p (&rest body)
  "Execute BODY and return true if the current buffer's point moved."
  (let ((pointvar (gensym "point-")))
    `(let ((,pointvar (point)))
       (save-current-buffer ,@body)
       (/= ,pointvar (point)))))

(put 'slime-point-moves-p 'lisp-indent-function 0)

(defun slime-forward-sexp (&optional count)
  "Like `forward-sexp', but understands reader-conditionals (#- and #+)."
  (dotimes (i (or count 1))
    (while (slime-point-moves-p (slime-forward-blanks)
                                (slime-forward-reader-comment)
                                (slime-forward-reader-conditional)))
    (forward-sexp)))

(defun slime-forward-blanks ()
  "Move forward over all whitespace and newlines at point."
  (while (slime-point-moves-p
           (skip-syntax-forward " ")
           ;; newlines aren't in lisp-mode's whitespace syntax class
           (when (eolp) (forward-char)))))

;; Emacs 21's forward-sexp understands #| |# comments in lisp-mode
;; buffers, but (at least) Emacs 20's doesn't, so here it is.
(defun slime-forward-reader-comment ()
  "Move forward over #|...|# reader comments. The comments may be nested."
  (when (looking-at "#|")
    (goto-char (match-end 0))
    (while (not (looking-at "|#"))
      (re-search-forward (regexp-opt '("|#" "#|")))
      (goto-char (match-beginning 0))
      (when (looking-at "#|")           ; nested comment
        (slime-forward-reader-comment)))
    (goto-char (match-end 0))))

(defun slime-forward-reader-conditional ()
  "Move past any reader conditional (#+ or #-) at point."
  (when (or (looking-at "#\\+")
            (looking-at "#-"))
    (goto-char (match-end 0))
    (let* ((plus-conditional-p (eq (char-before) ?+))
           (result (slime-eval-feature-conditional (read (current-buffer)))))
      (unless (if plus-conditional-p result (not result))
        ;; skip this sexp
        (slime-forward-sexp)))))

(defun slime-eval-feature-conditional (e)
  "Interpret a reader conditional expression."
  (if (symbolp e)
      (member* (symbol-name e) slime-lisp-features :test #'equalp)
    (funcall (ecase (car e)
               (and #'every)
               (or  #'some)
               (not (lambda (f l) (not (apply f l)))))
             #'slime-eval-feature-conditional
             (cdr e))))


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
  (let ((message (get-char-property (point) 'help-echo)))
    (slime-message "%s" (if (zerop (length message)) "\"\"" message))))

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

(defun slime-space (n)
  "Insert a space and print some relevant information (function arglist).
Designed to be bound to the SPC key.  Prefix argument can be used to insert
more than one space."
  (interactive "p")
  (self-insert-command n)
  (when (and (slime-connected-p)
	     (or (slime-idle-p) (slime-debugging-p))
	     (slime-function-called-at-point/line))
    (slime-arglist (symbol-name (slime-function-called-at-point/line)))))

(defun slime-arglist (symbol-name &optional show-fn)
  "Show the argument list for the nearest function call, if any.
If SHOW-FN is non-nil, it is funcall'd with the result instead of
printing a message."
  (interactive (list (slime-read-symbol "Arglist of: ")))
  (slime-eval-async 
   `(swank:arglist-string ,symbol-name)
   (slime-buffer-package)
   (with-lexical-bindings (show-fn symbol-name)
     (lambda (arglist)
       (if show-fn
           (funcall show-fn arglist)
         (message "%s" (slime-format-arglist symbol-name arglist)))))))

(defun slime-get-arglist (symbol-name)
  "Return the argument list for SYMBOL-NAME."
  (slime-format-arglist symbol-name
                        (slime-eval `(swank:arglist-string ,symbol-name))))

(defun slime-format-arglist (symbol-name arglist)
  (format "(%s %s)" symbol-name (substring arglist 1 -1)))


;;; Autodocs (automatic context-sensitive help)

(defvar slime-autodoc-mode nil
  "*When non-nil, print documentation about symbols as the point moves.")

(defvar slime-autodoc-cache-type 'last
  "*Cache policy for automatically fetched documentation.
Possible values are:
 nil  - none.
 last - cache only the most recently-looked-at symbol's documentation.
        The values are stored in the variable `slime-autodoc-cache'.
 all  - cache all symbol documentation.
        The values are stored on the `slime-autodoc-cache' property
        of the respective Elisp symbols.

More caching means fewer calls to the Lisp process, but at the risk of
using outdated information.")

(defvar slime-autodoc-cache nil
  "Cache variable for when `slime-autodoc-cache-type' is 'last'.
The value is (SYMBOL-NAME . DOCUMENTATION).")

(defun slime-autodoc ()
  "Print some apropos information about the code at point, if applicable."
  (when-let (sym (slime-function-called-at-point/line))
    (let ((name (symbol-name sym))
          (cache-key (slime-qualify-cl-symbol-name sym)))
      (or (when-let (documentation (slime-get-cached-autodoc cache-key))
            (message documentation)
            t)
          ;; Asynchronously fetch, cache, and display arglist
          (slime-arglist
           name
           (with-lexical-bindings (cache-key name)
             (lambda (arglist)
               ;; FIXME: better detection of "no documentation available"
               (unless (string-match "<Unknown-Function>" arglist)
                 (setq arglist (slime-format-arglist name arglist))
                 (slime-update-autodoc-cache cache-key arglist)
                 (message arglist)))))))))

(defun slime-get-cached-autodoc (symbol-name)
  "Return the cached autodoc documentation for SYMBOL-NAME, or nil."
  (ecase slime-autodoc-cache-type
    ((nil) nil)
    ((last)
     (when (equal (car slime-autodoc-cache) symbol-name)
       (cdr slime-autodoc-cache)))
    ((all)
     (when-let (symbol (intern-soft symbol-name))
       (get symbol 'slime-autodoc-cache)))))

(defun slime-update-autodoc-cache (symbol-name documentation)
  "Update the autodoc cache for SYMBOL with DOCUMENTATION.
Return DOCUMENTATION."
  (ecase slime-autodoc-cache-type
    ((nil) nil)
    ((last)
     (setq slime-autodoc-cache (cons symbol-name documentation)))
    ((all)
     (put (intern symbol-name) 'slime-autodoc-cache documentation)))
  documentation)

(defun slime-autodoc-post-command-hook ()
  "Function to be called after each Emacs command in a slime-mode buffer.
When `slime-autodoc-mode' is non-nil, print apropos information about
the symbol at point if applicable."
  (assert slime-mode)
  (when (and (slime-connected-p) (not (slime-busy-p)))
    (condition-case err
        (slime-autodoc)
      (error
       (setq slime-autodoc-mode nil)
       (message "Error: %S; slime-autodoc-mode now disabled." err)))))


;;; Completion

(defvar slime-completions-buffer-name "*Completions*")

(defvar slime-complete-saved-window-configuration nil
  "Window configuration before we show the *Completions* buffer.\n\
This is buffer local in the buffer where the complition is
perfermed.")

(defun slime-complete-maybe-save-window-configuration ()
  (make-local-variable 'slime-complete-saved-window-configuration)
  (unless slime-complete-saved-window-configuration
    (setq slime-complete-saved-window-configuration
          (current-window-configuration))))

(defun slime-complete-delay-restoration ()
  (add-hook (make-local-hook 'pre-command-hook)
            'slime-complete-maybe-restore-window-confguration))

(defun slime-complete-forget-window-configuration ()
  (setq slime-complete-saved-window-configuration nil))

(defun slime-complete-restore-window-configuration ()
  "Restore the window config if available."
  (remove-hook 'pre-command-hook
               'slime-complete-maybe-restore-window-confguration)
  (when slime-complete-saved-window-configuration
    (set-window-configuration slime-complete-saved-window-configuration)
    (setq slime-complete-saved-window-configuration nil))
  (when (get-buffer slime-completions-buffer-name)
    (bury-buffer slime-completions-buffer-name)))

(defun slime-complete-maybe-restore-window-confguration ()
  "Restore the window configuration, if the following command
terminates a current completion."
  (remove-hook 'pre-command-hook
               'slime-complete-maybe-restore-window-confguration)
  (condition-case err
      (cond ((find last-command-char "()\"'`,# \r\n:")
             (slime-complete-restore-window-configuration))
            ((memq this-command '(self-insert-command
                                  slime-complete-symbol
                                  backward-delete-char-untabify
                              backward-delete-char
                              scroll-other-window))
             (slime-complete-delay-restoration))
            (t 
             (slime-complete-forget-window-configuration)))
    (error
     ;; Because this is called on the pre-command-hook, we mustn't let
     ;; errors propagate.
     (message "Error in slime-complete-forget-window-configuration: %S" err))))
  
(defun slime-complete-symbol ()
  "Complete the symbol at point.
If the symbol lacks an explicit package prefix, the current buffer's
package is used."
  ;; NB: It is only the name part of the symbol that we actually want
  ;; to complete -- the package prefix, if given, is just context.
  (interactive)
  (let* ((end (point))
         (beg (slime-symbol-start-pos))
         (prefix (buffer-substring-no-properties beg end))
         (completions (slime-completions prefix))
         (completions-alist (slime-bogus-completion-alist completions))
         (completion (try-completion prefix completions-alist nil)))
    (cond ((eq completion t)
           (message "[Sole completion]")
           (slime-complete-restore-window-configuration))
          ((null completion)
           (message "Can't find completion for \"%s\"" prefix)
           (ding)
           (slime-complete-restore-window-configuration))
          ((not (string= prefix completion))
           (delete-region beg end)
           (insert-and-inherit completion)
           (cond ((null (cdr completions))
                  (slime-complete-restore-window-configuration))
                 (t (slime-complete-delay-restoration))))
          (t
           (message "Making completion list...")
           (let ((list (all-completions prefix completions-alist nil)))
             (slime-complete-maybe-save-window-configuration)
             (slime-with-output-to-temp-buffer "*Completions*"
               (display-completion-list list))
             (slime-complete-delay-restoration))
           (message "Making completion list...done")))))

(defun slime-completing-read-internal (string default-package flag)
  ;; We misuse the predicate argument to pass the default-package.
  ;; That's needed because slime-completing-read-internal is called in
  ;; the minibuffer.
  (ecase flag
    ((nil) 
     (let* ((completions (slime-completions string default-package)))
       (try-completion string
		       (slime-bogus-completion-alist completions))))
    ((t)
     (slime-completions string default-package))
    ((lambda)
     (member string (slime-completions string default-package)))))

(defun slime-completing-read-symbol-name (prompt &optional initial-value)
  "Read the name of a CL symbol, with completion.  
The \"name\" may include a package prefix."
  (completing-read prompt 
		   'slime-completing-read-internal
		   (slime-buffer-package)
		   nil
		   initial-value))

(defvar slime-read-expression-map (make-sparse-keymap)
  "Minibuffer keymap used for reading CL expressions.")

(set-keymap-parent slime-read-expression-map minibuffer-local-map)

(define-key slime-read-expression-map "\t" 'slime-complete-symbol)
(define-key slime-read-expression-map "\M-\t" 'slime-complete-symbol)

(defvar slime-read-expression-history '()
  "History list of expressions read from the minibuffer.")
 
(defun slime-read-from-minibuffer (prompt &optional initial-value)
  "Read a string from the minibuffer, prompting with PROMPT.  
If INITIAL-VALUE is non-nil, it is inserted into the minibuffer before
reading input.  The result is a string (\"\" if no input was given)."
  (let ((minibuffer-setup-hook 
	 (cons (lexical-let ((package (slime-buffer-package)))
		 (lambda ()
		   (setq slime-buffer-package package)
		   (set-syntax-table lisp-mode-syntax-table)))
	       minibuffer-setup-hook)))
    (read-from-minibuffer prompt initial-value slime-read-expression-map
			  nil 'slime-read-expression-history)))

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

(defun slime-completions (prefix &optional default-package)
  (let ((prefix (etypecase prefix
		  (symbol (symbol-name prefix))
		  (string prefix))))
    (slime-eval `(swank:completions ,prefix 
				    ,(or default-package
					 (slime-find-buffer-package)
					 (slime-buffer-package))))))


;;; Interpreting Elisp symbols as CL symbols (package qualifiers)

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

(defun slime-qualify-cl-symbol (symbol-or-name)
  "Like `slime-qualify-cl-symbol-name', but interns the result."
  (intern (slime-qualify-cl-symbol-name symbol-or-name)))

(defun slime-qualify-cl-symbol-name (symbol-or-name)
  "Return a package-qualified symbol-name that indicates the CL symbol
SYMBOL. If SYMBOL doesn't already have a package prefix, the buffer
package is used."
  (let ((s (if (stringp symbol-or-name)
               symbol-or-name
             (symbol-name symbol-or-name))))
    (if (slime-cl-symbol-package s)
        s
      (format "%s::%s"
              (slime-buffer-package)
              (slime-cl-symbol-name s)))))


;;; Edit definition

(defvar slime-find-definition-history-ring (make-ring 20)
  "History ring recording the definition-finding \"stack\".")

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

(defun slime-edit-fdefinition (name)
  "Lookup the definition of the symbol at point.  
If there's no symbol at point, or a prefix argument is given, then the
function name is prompted."
  (interactive (list (slime-read-symbol-name "Function name: ")))
  (let ((origin (point-marker))
	(source-location
	 (slime-eval `(swank:function-source-location-for-emacs ,name)
		     (slime-buffer-package))))
    (cond ((or (null source-location) (equal source-location '(:null)))
           (message "No definition found: %s" name))
          ((equal (car source-location) :error)
           (slime-message "%s" (cadr source-location)))
          (t
           (slime-goto-source-location source-location)
           (switch-to-buffer (current-buffer))
           (ring-insert-at-beginning 
	    slime-find-definition-history-ring origin)))))


;;; Interactive evaluation.

(defun slime-interactive-eval (string)
  "Read and evaluate STRING and print value in minibuffer. "
  (interactive (list (slime-read-from-minibuffer "Slime Eval: ")))
  (slime-insert-transcript-delimiter string)
  (slime-eval-async 
   `(swank:interactive-eval ,string)
   (slime-buffer-package t)
   (slime-show-evaluation-result-continuation)))

(defun slime-display-buffer-region (buffer start end &optional other-window)
  "Like `display-buffer', but only display the specified region."
  (let ((window-min-height 1))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (goto-char start)
          (beginning-of-line)
          (narrow-to-region (point) end)
          (let ((window (display-buffer buffer other-window)))
            (set-window-start window (point))
            (unless (or (one-window-p t)
                        (/= (frame-width) (window-width)))
              (set-window-text-height window (/ (1- (frame-height)) 2)))
            (shrink-window-if-larger-than-buffer window)
            window))))))

(defun slime-show-evaluation-result (value)
  (slime-show-last-output)
  (slime-message "=> %s" value))

(defun slime-show-evaluation-result-continuation ()
  (lexical-let ((buffer (current-buffer)))
    (lambda (value)
      (with-current-buffer buffer
        (slime-show-evaluation-result value)))))
  
(defun slime-last-expression ()
  (buffer-substring-no-properties (save-excursion (backward-sexp) (point))
				  (point)))

(defun slime-eval-last-expression ()
  "Evaluate the expression preceding point."
  (interactive)
  (slime-interactive-eval (slime-last-expression)))
  
(defun slime-eval-defun ()
  "Evaluate the current toplevel form."
  (interactive)
  (slime-interactive-eval (slime-defun-at-point)))

(defun slime-eval-region (start end)
  "Evalute region."
  (interactive "r")
  (slime-eval-async
   `(swank:interactive-eval-region ,(buffer-substring-no-properties start end))
   (slime-buffer-package)
   (slime-show-evaluation-result-continuation)))

(defun slime-eval-buffer ()
  "Evalute the current buffer.
The value is printed in the echo area."
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
  "Evalute the form before point; pprint the value in a buffer."
  (interactive)
  (slime-eval-describe `(swank:pprint-eval ,(slime-last-expression))))

(defun slime-eval-print-last-expression (string)
  "Evalute sexp before point; print value into the current buffer"
  (interactive (list (slime-last-expression)))
  (slime-insert-transcript-delimiter string)
  (insert "\n")
  (slime-eval-async 
   `(swank:interactive-eval ,string)
   (slime-buffer-package t)
   (lexical-let ((buffer (current-buffer)))
     (lambda (result)
       (with-current-buffer buffer
         (slime-show-last-output)
         (princ result buffer)
         (insert "\n"))))))

(defun slime-toggle-trace-fdefinition (fname-string)
  "Toggle trace for FNAME-STRING."
  (interactive (list (slime-completing-read-symbol-name 
		      "(Un)trace: " (slime-symbol-name-at-point))))
  (message "%s" (slime-eval `(swank:toggle-trace-fdefinition ,fname-string)
			    (slime-buffer-package t))))

(defun slime-untrace-all ()
  "Untrace all functions."
  (interactive)
  (slime-eval `(swank:untrace-all)))

(defun slime-disassemble-symbol (symbol-name)
  "Display the disassembly for SYMBOL-NAME."
  (interactive (list (slime-read-symbol-name "Disassemble: ")))
  (slime-eval-describe `(swank:disassemble-symbol ,symbol-name)))

(defun slime-load-file (filename)
  "Load the Lisp file FILENAME."
  (interactive (list 
		(read-file-name "Load file: " nil nil
				nil (file-name-sans-extension
				     (file-name-nondirectory 
				      (buffer-file-name))))))
  (slime-eval-async 
   `(swank:load-file ,(expand-file-name filename)) nil 
   (slime-show-evaluation-result-continuation)))


;;; Documentation

(defun slime-show-description (string package)
  (slime-save-window-configuration)
  (save-current-buffer
    (slime-with-output-to-temp-buffer "*Help*"
      (princ string))
    (with-current-buffer "*Help*"
      (setq slime-buffer-package package)
      (set-syntax-table lisp-mode-syntax-table)
      (slime-mode t))))

(defun slime-eval-describe (form)
  (let ((package (slime-buffer-package)))
    (slime-eval-async 
     form package
     (lexical-let ((package package))
       (lambda (string) (slime-show-description string package))))))

(defun slime-describe-symbol (symbol-name)
  (interactive (list (slime-read-symbol-name "Describe symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (slime-eval-describe `(swank:describe-symbol ,symbol-name)))

(defun slime-apropos (string &optional only-external-p package)
  (interactive
   (if current-prefix-arg
       (list (read-string "SLIME Apropos: ")
             (y-or-n-p "External symbols only? ")
             (let ((pkg (slime-read-package-name "Package: ")))
               (if (string= pkg "") nil pkg)))
     (list (read-string "SLIME Apropos: ") t nil)))
  (let ((buffer-package (slime-buffer-package t)))
    (slime-eval-async
     `(swank:apropos-list-for-emacs ,string ,only-external-p ,package)
     buffer-package
     (lexical-let ((string string)
		   (package (or package buffer-package)))
       (lambda (r) (slime-show-apropos r string package))))))

(defun slime-apropos-all ()
  "Shortcut for (slime-apropos <pattern> nil nil)"
  (interactive)
  (slime-apropos (read-string "SLIME Apropos: ") nil nil))

(defun slime-show-apropos (plists string package)
  (if (null plists)
      (message "No apropos matches for %S" string)
    (save-current-buffer
      (slime-with-output-to-temp-buffer "*SLIME Apropos*"
	(set-buffer standard-output)
	(apropos-mode)
	(set-syntax-table lisp-mode-syntax-table)
	(slime-mode t)
	(setq slime-buffer-package package)
        (slime-set-truncate-lines)
	(slime-print-apropos plists)))))

(defvar slime-apropos-label-properties
  (progn
    (require 'apropos)
    (cond ((and (boundp 'apropos-label-properties) 
                (symbol-value 'apropos-label-properties)))
          ((boundp 'apropos-label-face)
           (typecase (symbol-value 'apropos-label-face)
             (symbol `(face ,(or (symbol-value 'apropos-label-face)
                                 'italic)
                            mouse-face highlight))
             (list (symbol-value 'apropos-label-face)))))))

(defun slime-print-apropos (plists)
  (dolist (plist plists)
    (let ((designator (plist-get plist :designator)))
      (slime-insert-propertized (list 'face apropos-symbol-face
                                      'item designator
                                      'action 'slime-describe-symbol)
                                designator))
    (terpri)
    (let ((apropos-label-properties slime-apropos-label-properties))
      (loop for (prop namespace action) 
	    in '((:variable "Variable" swank:describe-symbol)
		 (:function "Function" swank:describe-function)
		 (:setf "Setf" swank:describe-setf-function)
		 (:type "Type" swank:describe-type)
		 (:class "Class" swank:describe-class)
                 (:alien-type "Alien type" swank:describe-alien-type)
                 (:alien-struct "Alien struct" swank:describe-alien-struct)
                 (:alien-union "Alien type" swank:describe-alien-union)
                 (:alien-enum "Alien enum" swank:describe-alien-enum)
                 )
	    do
	    (let ((value (plist-get plist prop))
		  (start (point)))
	      (when value
		(princ "  ") 
		(slime-insert-propertized apropos-label-properties namespace)
		(princ ": ")
		(princ (etypecase value
			 (string value)
			 ((member :not-documented) "(not documented)")))
		(put-text-property start (point) 'describer action)
		(put-text-property start (point) 'action 'slime-call-describer)
		(terpri)))))))

(defun slime-call-describer (item)
  (slime-eval-describe `(,(get-text-property (point) 'describer) ,item)))


;;; XREF: cross-referencing

(defvar slime-xref-summary nil
  "Summary of a cross reference list, for the mode line.")

(define-minor-mode slime-xref-mode
  "\\<slime-xref-mode-map>"
  nil
  nil
  '(("RET"  . slime-goto-xref)
    ("\C-m" . slime-goto-xref)
    ))

;; Setup the mode-line to say when we're in slime-mode, and which CL
;; package we think the current buffer belongs to.
(add-to-list 'minor-mode-alist '(slime-xref-mode slime-xref-summary))

(defun slime-who-calls (symbol)
  "Show all known callers of the function SYMBOL."
  (interactive (list (slime-read-symbol "Who calls: ")))
  (slime-xref 'calls symbol))

(defun slime-who-references (symbol)
  "Show all known referrers of the global variable SYMBOL."
  (interactive (list (slime-read-symbol "Who references: ")))
  (slime-xref 'references symbol))

(defun slime-who-binds (symbol)
  "Show all known binders of the global variable SYMBOL."
  (interactive (list (slime-read-symbol "Who binds: ")))
  (slime-xref 'binds symbol))

(defun slime-who-sets (symbol)
  "Show all known setters of the global variable SYMBOL."
  (interactive (list (slime-read-symbol "Who sets: ")))
  (slime-xref 'sets symbol))

(defun slime-who-macroexpands (symbol)
  "Show all known expanders of the macro SYMBOL."
  (interactive (list (slime-read-symbol "Who macroexpands: ")))
  (slime-xref 'macroexpands symbol))

(defun slime-xref (type symbol)
  "Make an XREF request to Lisp."
  (slime-eval-async
   `(,(intern (format "swank:who-%s" type)) ',symbol)
   (slime-buffer-package t)
   (lexical-let ((type type)
                 (symbol symbol)
                 (package (slime-buffer-package)))
     (lambda (result)
       (slime-show-xrefs result type symbol package)))))

(defun slime-show-xrefs (file-referrers type symbol package)
  "Show the results of an XREF query."
  (if (null file-referrers)
      (message "No references found for %s." symbol)
    (slime-save-window-configuration)
    (setq slime-next-location-function 'slime-goto-next-xref)
    (with-current-buffer (slime-xref-buffer t)
      (slime-init-xref-buffer package type symbol)
      (dolist (ref file-referrers)
        (apply #'slime-insert-xrefs ref))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (save-selected-window
        (delete-windows-on (slime-xref-buffer))
        (slime-display-xref-buffer)))))

(defun slime-insert-xrefs (filename refs)
  "Insert the cross-references for a file.
Each cross-reference line contains these text properties:
 slime-xref:             a unique object
 slime-file:             filename of reference
 slime-xref-source-path: source-path of reference
 slime-xref-complete:    true iff both file and source-path are known."
  (unless (bobp) (insert "\n"))
  (insert (format "In %s:\n" (or filename "unidentified files")))
  (loop for (referrer source-path) in refs
        do (let ((complete (and filename source-path)))
             (slime-insert-propertized
              (list 'slime-xref (make-symbol "#:unique-ref")
                    'slime-xref-complete complete
                    'slime-xref-file filename
                    'slime-xref-source-path source-path
                    'face (if complete
                              'font-lock-function-name-face
                            'font-lock-comment-face))
              (format "%s\n" referrer)))))


;;;; XREF results buffer and window management

(defun slime-xref-buffer (&optional create)
  "Return the XREF results buffer.
If CREATE is non-nil, create it if necessary."
  (if create
      (get-buffer-create "*CMUCL xref*")
    (or (get-buffer "*CMUCL xref*")
        (error "No XREF buffer"))))

(defun slime-init-xref-buffer (package ref-type symbol)
  "Initialize the current buffer for displaying XREF information."
  (slime-xref-mode t)
  (setq buffer-read-only nil)
  (erase-buffer)
  (set-syntax-table lisp-mode-syntax-table)
  (slime-mode t)
  (setq slime-buffer-package package)
  (slime-set-truncate-lines)
  (setq slime-xref-summary
        (format " XREF[%s: %s]" ref-type symbol)))

(defun slime-display-xref-buffer ()
  "Display the XREF results buffer in a window and select it."
  (let* ((buffer (slime-xref-buffer))
         (window (get-buffer-window buffer)))
    (if (and window (window-live-p window))
        (select-window window)
      (select-window (display-buffer buffer t))
      (set-window-text-height (selected-window)
                              (min (count-lines (point-min) (point-max))
                                   (window-text-height))))))


;;;; XREF navigation
(defun slime-goto-xref ()
  "Goto the cross-referenced location at point."
  (interactive)
  (let ((file (get-text-property (point) 'slime-xref-file))
        (path (get-text-property (point) 'slime-xref-source-path)))
    (unless (and file path)
      (error "No reference at point."))
    (find-file-other-window file)
    (goto-char (point-min))
    (slime-visit-source-path path)))

(defun slime-goto-next-xref ()
  "Goto the next cross-reference location."
  (save-selected-window
    (slime-display-xref-buffer)
    (loop do (goto-char (next-single-char-property-change (point) 'slime-xref))
          until (or (get-text-property (point) 'slime-xref-complete)
                    (eobp)))
    (if (not (eobp))
        (slime-goto-xref)
      (forward-line -1)
      (message "No more xrefs."))))

(defvar slime-next-location-function nil
  "Function to call for going to the next location.")

(defun slime-next-location ()
  "Go to the next location, depending on context.
When displaying XREF information, this goes to the next reference."
  (interactive)
  (when (null slime-next-location-function)
    (error "No context for finding locations."))
  (funcall slime-next-location-function))


;;; List callers/callees

(defvar slime-select-mode-map)
(defvar slime-previous-selected-line)
(defvar slime-select-finish)
(defvar slime-select-follow)
(defvar slime-select-saved-window-configuration)

(defun slime-list-callers (symbol-name)
  (interactive (list (slime-read-symbol-name "List callers: ")))
  (slime-eval-select-function-list `(swank:list-callers ,symbol-name)))

(defun slime-list-callees (symbol-name)
  (interactive (list (slime-read-symbol-name "List callees: ")))
  (slime-eval-select-function-list `(swank:list-callees ,symbol-name)))

(defun slime-eval-select-function-list (sexp)
  (lexical-let ((package (slime-buffer-package)))
    (slime-eval-async sexp package
		      (lambda (names) 
			(slime-select-function names package)))
    (slime-save-window-configuration)))

(defun slime-select-function (function-names package)
  (if (null function-names)
      (message "No callers")
    (with-lexical-bindings (function-names package)
      (slime-select 
       function-names
       (lambda (index)
         (slime-eval-async `(swank:function-source-location-for-emacs 
                             ,(nth index function-names))
                           package
                           (lambda (loc)
                             (let ((pop-up-windows nil))
                               (slime-carefully-show-source-location loc)))))
       (lambda (index))))))

(defun slime-carefully-show-source-location (location)
  (condition-case e
      (slime-show-source-location location)
    (error (message "%s" (error-message-string e))
	   (ding))))

(defvar slime-select-split-window-vectically nil)

(defun slime-get-select-window (labels)
  (cond (slime-select-split-window-vectically
         (split-window (selected-window)
                       (- (frame-width) 
                          (min (1+ (max 
                                    (loop for l in labels maximize (length l))
                                    window-min-width))
                               25))
                       t))
        (t
         (cond ((one-window-p)
                (split-window (selected-window)))
               (t (next-window))))))

(defun slime-select-pop-to-window (buffer labels)
  (let ((window (slime-get-select-window labels)))
    (set-window-buffer window (current-buffer))
    (select-window window)
    (shrink-window-if-larger-than-buffer window)))

(defun slime-select (labels follow finish)
  "Select an item form the list LABELS.

The list is displayed in a new buffer. FOLLOW is called with the
current index whenever a new line is selected.  FINISH is called with
the current index when the selection is completed."
    (set-buffer (get-buffer-create "*SLIME Select*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (loop for (label . r) on labels
	  do (progn (insert label)
		    (when r (insert "\n"))))
    (goto-char (point-min))
    (slime-select-mode)
    (setq slime-select-follow follow)
    (setq slime-select-finish finish)
    (setq buffer-read-only t)
    (setq slime-select-saved-window-configuration 
	  (current-window-configuration))
    (slime-select-pop-to-window (current-buffer) labels)
    (slime-select-post-command-hook))

(defun slime-selected-line ()
  (count-lines (point-min) (save-excursion (beginning-of-line) (point))))

(define-derived-mode slime-select-mode fundamental-mode "SLIME-Select"
  "Mode to select an item from a list."
  (mapc #'make-variable-buffer-local
	'(slime-previous-selected-line
	  slime-select-follow
	  slime-select-finish
	  slime-select-saved-window-configuration))
  (setq slime-previous-selected-line -1)
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'slime-select-post-command-hook nil t)
  (add-hook (make-local-variable 'kill-buffer-hook) 'sldb-delete-overlays)
  (slime-mode t))

(defun slime-select-post-command-hook ()
  (unless (eq slime-previous-selected-line (slime-selected-line))
    (let ((line (slime-selected-line)))
      (setq slime-previous-selected-line line)
      (ignore-errors (funcall slime-select-follow line)))))

(defun slime-select-done ()
  (interactive)
  (save-current-buffer
    (funcall slime-select-finish (slime-selected-line)))
  (slime-select-cleanup))

(defun slime-select-cleanup ()
  (let ((buffer (current-buffer)))
    (delete-windows-on buffer)
    (kill-buffer buffer)))
  
(defun slime-select-quit ()
  (interactive)
  (set-window-configuration slime-select-saved-window-configuration)
  (slime-select-cleanup))

(slime-define-keys slime-select-mode-map
  ([return] 'slime-select-done)
  ("q" 'slime-select-quit))


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
  (if (slime-evaluating-p)
      (slime-dispatch-event '(:emacs-interrupt))
    (error "Not evaluating - nothing to interrupt.")))

(defun slime-quit ()
  (interactive)
  (if (slime-evaluating-p)
      (slime-dispatch-event '(:emacs-quit))
    (error "Not evaluating - nothing to quit.")))

(defun slime-set-package (package)
  (interactive (list (slime-read-package-name "Package: " 
					      (slime-find-buffer-package))))
  (message "*package*: %s" (slime-eval `(swank:set-package ,package))))

(defun slime-set-default-directory (directory)
  (interactive (list (read-file-name "Directory: " nil default-directory t)))
  (message "default-directory: %s" 
	   (slime-eval `(swank:set-default-directory 
			 ,(expand-file-name directory)))))

(defun slime-sync-package-and-default-directory ()
  (interactive)
  (let ((package (slime-eval `(swank:set-package 
			       ,(slime-find-buffer-package))))
	(directory (slime-eval `(swank:set-default-directory 
				 ,(expand-file-name default-directory)))))
    (message "package: %s  default-directory: %s" package directory)))
	

;;; Debugger

(defvar sldb-condition)
(defvar sldb-restarts)
(defvar sldb-level-in-buffer)
(defvar sldb-backtrace-start-marker)
(defvar sldb-mode-map)

(defvar sldb-hook nil
  "Hook run on entry to the debugger.")

(defun sldb-setup (condition restarts frames)
  (with-current-buffer (get-buffer-create "*sldb*")
    (setq buffer-read-only nil)
    (sldb-mode)
    (slime-set-truncate-lines)
    (add-hook (make-local-variable 'kill-buffer-hook) 'sldb-delete-overlays)
    (setq sldb-condition condition)
    (setq sldb-restarts restarts)
    (insert condition "\n" "\nRestarts:\n")
    (loop for (name string) in restarts
	  for number from 0 
	  do (progn
	       (slime-insert-propertized
		`(face bold 
		       restart-number ,number
		       sldb-default-action sldb-invoke-restart
		       mouse-face highlight)
		"  " (number-to-string number) ": ["  name "] " string)
	       (insert "\n")))
    (insert "\nBacktrace:\n")
    (setq sldb-backtrace-start-marker (point-marker))
    (sldb-insert-frames frames 1)
    (setq buffer-read-only t)
    (pop-to-buffer (current-buffer))
    (run-hooks 'sldb-hook)))

(define-derived-mode sldb-mode fundamental-mode "sldb" 
  "Superior lisp debugger mode

\\{sldb-mode-map}"
  (erase-buffer)
  (set-syntax-table lisp-mode-syntax-table)
  (mapc #'make-local-variable '(sldb-condition 
				sldb-restarts
				sldb-level-in-buffer
				sldb-backtrace-start-marker))
  (setq sldb-level-in-buffer sldb-level)
  (setq mode-name (format "sldb[%d]" sldb-level)))

(defun sldb-insert-frames (frames maximum-length)
  (assert (<= (length frames) maximum-length))
  (save-excursion
    (loop for frame in frames
	  for (number string) = frame
	  do (slime-insert-propertized `(frame ,frame) string "\n"))
    (let ((number (sldb-previous-frame-number)))
      (cond ((< (length frames) maximum-length))
	    (t
	     (slime-insert-propertized 
	      `(sldb-default-action 
		sldb-fetch-more-frames
		point-entered sldb-fetch-more-frames
		sldb-previous-frame-number ,number)
	      " --more--\n"))))))

(defun sldb-fetch-more-frames (&optional start end)
  (let ((inhibit-point-motion-hooks t))
    (let ((inhibit-read-only t))
      (let ((previous (get-text-property (point) 
					 'sldb-previous-frame-number)))
	(when previous
	  (beginning-of-line)
	  (let ((start (point)))
	    (end-of-buffer)
	    (delete-region start (point)))
           (let ((start (1+ previous))
                 (end (+ previous 40)))
             (sldb-insert-frames 
              (slime-eval `(swank:backtrace-for-emacs ,start ,end))
              (- end start))))))))

(defun sldb-default-action/mouse (event)
  (interactive "e")
  (destructuring-bind (mouse-1 (w pos &rest _)) event
    (save-excursion
      (goto-char pos)
      (let ((fn (get-text-property (point) 'sldb-default-action)))
	(if fn (funcall fn))))))

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
	   
(defun sldb-show-source ()
  (interactive)
  (sldb-delete-overlays)
  (let* ((number (sldb-frame-number-at-point))
	 (source-location (slime-eval
			   `(swank:frame-source-location-for-emacs ,number))))
    (slime-show-source-location source-location)))

(defun slime-show-source-location (source-location)
  (save-selected-window
    (slime-goto-source-location source-location)
    (sldb-highlight-sexp)
    (display-buffer (current-buffer) t)
    (save-excursion
      (beginning-of-line -4)
      (set-window-start (get-buffer-window (current-buffer)) (point)))))

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

(defun sldb-frame-region ()
  (save-excursion
    (goto-char (next-single-property-change (point) 'frame nil (point-max)))
    (backward-char)
    (values (previous-single-property-change (point) 'frame)
	    (next-single-property-change (point) 'frame nil (point-max)))))

(defun sldb-show-frame-details ()
  (multiple-value-bind (start end) (sldb-frame-region)
    (save-excursion
      (let* ((props (text-properties-at (point)))
	     (frame (plist-get props 'frame))
	     (frame-number (car frame))
	     (standard-output (current-buffer))
             (indent1 "   ")
             (indent2 "      "))
	(goto-char start)
	(delete-region start end)
	(slime-propertize-region (plist-put props 'details-visible-p t)
	  (insert (second frame) "\n"
                  indent1 "Locals:\n")
	  (sldb-princ-locals frame-number indent2)
          (let ((catchers (sldb-catch-tags frame-number)))
            (cond ((null catchers)
                   (insert indent1 "[No catch-tags]\n"))
                  (t
                   (insert indent1 "Catch-tags:\n")
                   (loop for (tag . location) in catchers
                         do (slime-insert-propertized  
                             '(catch-tag ,tag)
                             indent2 (format "%S\n" tag))))))

	  (terpri)
	  (point)))))
  (apply #'sldb-maybe-recenter-region (sldb-frame-region)))

(defun sldb-maybe-recenter-region (start end)
  (sit-for 0 nil)
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
	(slime-propertize-region (plist-put props 'details-visible-p nil)
	  (insert (second frame) "\n"))))))

(defun sldb-eval-in-frame (string)
  (interactive (list (slime-read-from-minibuffer "Eval in frame: ")))
  (let* ((number (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:eval-string-in-frame ,string ,number)
		      (slime-buffer-package)
		      (lambda (reply) (slime-message "==> %s" reply)))))

(defun sldb-pprint-eval-in-frame (string)
  (interactive (list (slime-read-from-minibuffer "Eval in frame: ")))
  (let* ((number (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:pprint-eval-string-in-frame ,string ,number)
		      nil
		      (lambda (result)
			(slime-show-description result nil)))))

(defun sldb-inspect-in-frame (string)
  (interactive (list (slime-read-from-minibuffer 
                      "Inspect in frame (evaluated): ")))
  (let ((number (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:inspect-in-frame ,string ,number)
                      (slime-buffer-package)
                      'slime-open-inspector)))

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
    (princ (plist-get l :value-string))
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

(defun sldb-cleanup ()
  (let ((sldb-buffer (get-buffer "*sldb*")))
    (when sldb-buffer
      (if (> sldb-level 1)
          (with-current-buffer sldb-buffer
            (let ((inhibit-read-only t))
              (erase-buffer)))
        (kill-buffer sldb-buffer)))))
      
(defun sldb-quit ()
  (interactive)
  (slime-eval-async '(swank:throw-to-toplevel) nil (lambda (_))))

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
            
(slime-define-keys sldb-mode-map 
  ("v"    'sldb-show-source)
  ((kbd "RET") 'sldb-default-action)
  ("\C-m"      'sldb-default-action)
  ([mouse-2]  'sldb-default-action/mouse)
  ("e"    'sldb-eval-in-frame)
  ("p"    'sldb-pprint-eval-in-frame)
  ("i"    'sldb-inspect-in-frame)
  ("d"    'sldb-down)
  ("u"    'sldb-up)
  ("\M-n" 'sldb-details-down)
  ("\M-p" 'sldb-details-up)
  ("l"    'sldb-list-locals)
  ("t"    'sldb-toggle-details)
  ("c"    'sldb-continue)
  ("a"    'sldb-abort)
  ("r"    'sldb-invoke-restart)
  ("q"    'sldb-quit)
  (":"    'slime-interactive-eval))

(dolist (spec slime-keys)
  (destructuring-bind (key command &key sldb prefixed &allow-other-keys) spec
    (when sldb
      (let ((key (if prefixed (concat slime-prefix-key key) key)))
        (define-key sldb-mode-map key command)))))

;; Keys 0-9 are shortcuts to invoke particular restarts.
(defmacro define-sldb-invoke-restart-key (number key)
  (let ((fname (intern (format "sldb-invoke-restart-%S" number))))
    `(progn
       (defun ,fname ()
	 (interactive)
	 (sldb-invoke-restart ,number))
       (define-key sldb-mode-map ,key ',fname))))

(defmacro define-sldb-invoke-restart-keys (from to)
  `(progn
     ,@(loop for n from from to to
	     collect `(define-sldb-invoke-restart-key ,n 
			,(number-to-string n)))))

(define-sldb-invoke-restart-keys 0 9)


;;; Inspector

(defvar slime-inspector-mark-stack '())

(defun slime-inspect (string)
  (interactive 
   (list (slime-read-from-minibuffer "Inspect value (evaluated): "
				     (slime-sexp-at-point))))
  (slime-eval-async `(swank:init-inspector ,string) (slime-buffer-package)
		    'slime-open-inspector))

(define-derived-mode slime-inspector-mode fundamental-mode "Slime-Inspector"
  (set-syntax-table lisp-mode-syntax-table)
  (slime-set-truncate-lines)
  (slime-mode t)
  (setq buffer-read-only t))

(defun slime-inspector-buffer ()
  (or (get-buffer "*Slime Inspector*")
      (with-current-buffer (get-buffer-create "*Slime Inspector*")
	(setq slime-inspector-mark-stack '())
	(slime-inspector-mode)
	(current-buffer))))

(defun slime-open-inspector (inspected-parts &optional point)
  (with-current-buffer (slime-inspector-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (getf inspected-parts :text))
      (while (eq (char-before) ?\n) (backward-delete-char 1))
      (insert "\n"
	      "   [type: " (getf inspected-parts :type) "]\n"
	      "   " (getf inspected-parts :primitive-type) "\n"
	      "\n"
	      "Slots:\n")
      (save-excursion
	(loop for (label . value) in (getf inspected-parts :parts)
	      for i from 0
	      do (slime-propertize-region `(slime-part-number ,i)
		   (insert label ": " value "\n"))))
      (pop-to-buffer (current-buffer))
      (when point (goto-char point)))))

(defun slime-inspector-object-at-point ()
  (or (get-text-property (point) 'slime-part-number)
      (error "No part at point")))

(defun slime-inspector-inspect-object-at-point (number)
  (interactive (list (slime-inspector-object-at-point)))
  (slime-eval-async `(swank:inspect-nth-part ,number) nil
		    'slime-open-inspector)
  (push (point) slime-inspector-mark-stack))

(defun slime-inspector-pop ()
  (interactive)
  (slime-eval-async 
   `(swank:inspector-pop) nil 
   (lambda (result)
     (cond (result
	    (slime-open-inspector result (pop slime-inspector-mark-stack)))
	   (t 
	    (message "No previous object")
	    (ding))))))

(defun slime-inspector-next ()
  (interactive)
  (let ((result (slime-eval `(swank:inspector-next) nil)))
    (cond (result 
	   (push (point) slime-inspector-mark-stack)
	   (slime-open-inspector result))
	  (t (message "No next object")
	     (ding)))))
  
(defun slime-inspector-quit ()
  (interactive)
  (slime-eval-async `(swank:quit-inspector) nil (lambda (_)))
  (kill-buffer (current-buffer)))

(defun slime-inspector-describe ()
  (interactive)
  (slime-eval-describe `(swank:describe-inspectee)))

(slime-define-keys slime-inspector-mode-map
  ([return] 'slime-inspector-inspect-object-at-point)
  ("l" 'slime-inspector-pop)
  ("n" 'slime-inspector-next)
  ("d" 'slime-inspector-describe)
  ("q" 'slime-inspector-quit))


;;; `Select'

(defvar slime-selector-methods nil
  "List of buffer-selection methods for the `slime-select' command.
Each element is a list (KEY DESCRIPTION FUNCTION).
DESCRIPTION is a one-line description of what the key selects.")

(defun slime-selector ()
  "Select a new buffer by type, indicated by a single character.
The user is prompted for a single character indicating the method by
which to choose a new buffer. The `?' character describes the
available methods.

See `def-slime-selector-method' for defining new methods."
  (interactive)
  (message "Select [%s]: " 
           (apply #'string (mapcar #'car slime-selector-methods)))
  (let* ((ch (save-window-excursion
               (select-window (minibuffer-window))
               (read-char)))
         (method (find ch slime-selector-methods :key #'car)))
    (cond ((null method)
           (message "No method for character: ?\\%c" ch)
           (ding)
           (sit-for 1)
           (slime-selector))
          (t
           (funcall (third method))))))

(defmacro def-slime-selector-method (key description &rest body)
  "Define a new `slime-select' buffer selection method.
KEY is the key the user will enter to choose this method.
DESCRIPTION is a one-line sentence describing how the method selects a
buffer.
BODY is a series of forms which must return the buffer to be selected."
  `(setq slime-selector-methods
         (sort* (cons (list ,key ,description
                            (lambda () (switch-to-buffer (progn ,@body))))
                      (remove* ,key slime-selector-methods :key #'car))
                #'< :key #'car)))


(def-slime-selector-method ?? "the Select help buffer."
  (ignore-errors (kill-buffer "*Select Help*"))
  (with-current-buffer (get-buffer-create "*Select Help*")
    (insert "Select Methods:\n\n")
    (loop for (key line function) in slime-selector-methods
          do (insert (format "%c:\t%s\n" key line)))
    (help-mode)
    (display-buffer (current-buffer) t)
    (shrink-window-if-larger-than-buffer 
     (get-buffer-window (current-buffer))))
  (slime-selector)
  (current-buffer))

(def-slime-selector-method ?r
  "the SLIME Read-Eval-Print-Loop."
  (slime-output-buffer))

(def-slime-selector-method ?i
  "the *inferior-lisp* buffer."
  "*inferior-lisp*")

(def-slime-selector-method ?v
  "the *slime-events* buffer."
  "*slime-events*")

(def-slime-selector-method ?l
  "the most recently visited lisp-mode buffer."
  (slime-recently-visited-buffer 'lisp-mode))

(def-slime-selector-method ?d
  "the *sldb* buffer buffer"
  (unless (get-buffer "*sldb*")
    (error "No debugger buffer"))
  "*sldb*")

(def-slime-selector-method ?e
  "the most recently visited emacs-lisp-mode buffer."
  (slime-recently-visited-buffer 'emacs-lisp-mode))

(defun slime-recently-visited-buffer (mode)
  "Return the most recently visited buffer whose major-mode is MODE.
Only considers buffers that are not already visible."
  (loop for buffer in (buffer-list)
        when (and (with-current-buffer buffer (eq major-mode mode))
                  (null (get-buffer-window buffer 'visible)))
        return buffer
        finally (error "Can't find unshown buffer in %S" mode)))


;;; Test suite

(defvar slime-tests '()
  "Names of test functions.")

(defvar slime-test-debug-on-error nil
  "*When non-nil debug errors in test cases.")

(defvar slime-total-tests nil
  "Total number of tests executed during a test run.")

(defvar slime-failed-tests nil
  "Total number of failed tests during a test run.")

(defvar slime-test-buffer-name "*Tests*"
  "The name of the buffer used to display test results.")


;;;; Execution engine

(defun slime-run-tests ()
  "Run the test suite.
The results are presented in an outline-mode buffer, with the tests
that succeeded initially folded away."
  (interactive)
  (slime-create-test-results-buffer)
  (unwind-protect
      (slime-execute-tests)
    (pop-to-buffer slime-test-buffer-name)
    (goto-char (point-min))
    (hide-body)
    ;; Expose failed tests
    (dolist (o (overlays-in (point-min) (point-max)))
      (when (overlay-get o 'slime-failed-test)
        (goto-char (overlay-start o))
        (show-subtree)))))

(defun slime-execute-tests ()
  "Execute each test case with each input.
Return the number of failed tests."
  (save-window-excursion
    (let ((slime-total-tests 0)
          (slime-failed-tests 0))
      (loop for (name function inputs) in slime-tests
            do (progn
                 (slime-test-heading 1 "%s" name)
                 (dolist (input inputs)
                   (incf slime-total-tests)
                   (slime-test-heading 2 "input: %s" input)
                   (condition-case err
                       (apply function input)
                     (error (incf slime-failed-tests)
                            (slime-print-check-error err))))))
      (let ((summary (if (zerop slime-failed-tests)
                         (format "All %S tests completed successfully."
                                 slime-total-tests)
                       (format "Failed on %S of %S tests."
                               slime-failed-tests slime-total-tests))))
        (save-excursion
          (with-current-buffer slime-test-buffer-name
            (goto-char (point-min))
            (insert summary "\n\n")))
        (message summary)
        slime-failed-tests))))

(defun slime-batch-test (results-file)
  "Run the test suite in batch-mode.
Exits Emacs when finished. The exit code is the number of failed tests."
  (let ((slime-dont-prompt t)
        (slime-swank-port 4006)         ; different port than interactive use
        (slime-test-debug-on-error nil))
    (slime)
    ;; Block until we are up and running.
    (slime-sync-state-stack '(slime-idle-state) 120)
    (switch-to-buffer "*scratch*")
    (let ((failed-tests (slime-run-tests)))
      (with-current-buffer slime-test-buffer-name
        (slime-delete-hidden-outline-text)
        (goto-char (point-min))
        (insert "-*- outline -*-\n\n")
        (write-file results-file))
      (kill-emacs failed-tests))))


;;;; Results buffer creation and output

(defun slime-create-test-results-buffer ()
  "Create and initialize the buffer for test suite results."
  (ignore-errors (kill-buffer slime-test-buffer-name))
  (with-current-buffer (get-buffer-create slime-test-buffer-name)
    (erase-buffer)
    (outline-mode)
    (set (make-local-variable 'outline-regexp) "\\*+")
    (slime-set-truncate-lines)))

(defun slime-delete-hidden-outline-text ()
  "Delete the hidden parts of an outline-mode buffer."
  (loop do (when (eq (get-char-property (point) 'invisible) 'outline)
             (delete-region (point)
                            (next-single-char-property-change (point)
                                                              'invisible)))
        until (eobp)
        do (goto-char (next-single-char-property-change (point) 'invisible))))

(defun slime-test-heading (level format &rest args)
  "Output a test suite heading.
LEVEL gives the depth of nesting: 1 for top-level, 2 for a subheading, etc."
  (with-current-buffer slime-test-buffer-name
    (goto-char (point-max))
    (insert (make-string level ?*)
            " "
            (apply 'format format args)
            "\n")))

(defun slime-test-failure (keyword string)
  "Output a failure message from the test suite.
KEYWORD names the type of failure and STRING describes the reason."
  (with-current-buffer slime-test-buffer-name
    (goto-char (point-max))
    (let ((start (point)))
      (insert keyword ": ")
      (let ((overlay (make-overlay start (point))))
        (overlay-put overlay 'slime-failed-test t)
        (overlay-put overlay 'face 'bold)))
    (insert string "\n")))

(defun slime-test-message (string)
  "Output a message from the test suite."
  (with-current-buffer slime-test-buffer-name
    (goto-char (point-max))
    (insert string "\n")))


;;;; Macros for defining test cases

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
         (slime-sync)
         ,@body)
       (setq slime-tests (append (remove* ',name slime-tests :key 'car)
                                 (list (list ',name ',fname ,inputs)))))))

(defmacro slime-check (test-name &rest body)
  "Check a condition (assertion.)
TEST-NAME can be a symbol, a string, or a (FORMAT-STRING . ARGS) list.
BODY returns true if the check succeeds."
  (let ((check-name (gensym "check-name-")))
    `(let ((,check-name ,(typecase test-name
                           (symbol (symbol-name test-name))
                           (string test-name)
                           (cons `(format ,@test-name)))))
       (if (progn ,@body)
           (slime-print-check-ok ,check-name)
         (incf slime-failed-tests)
         (slime-print-check-failed ,check-name)
         (when slime-test-debug-on-error
           (debug (format "Check failed: %S" ,check-name)))))))

(defun slime-print-check-ok (test-name)
  (slime-test-message test-name))

(defun slime-print-check-failed (test-name)
  (slime-test-failure "FAILED" test-name))

(defun slime-print-check-error (reason)
  (slime-test-failure "ERROR" (format "%S" reason)))

(put 'def-slime-test 'lisp-indent-function 4)
(put 'slime-check 'lisp-indent-function 1)


;;;; Test case definitions

;; Clear out old tests.
(setq slime-tests nil)

(def-slime-test find-definition
    (name buffer-package)
    "Find the definition of a function or macro in swank.lisp."
    '((read-from-emacs "SWANK")
      (swank::read-from-emacs "CL-USER")
      (swank:start-server "CL-USER"))
  (switch-to-buffer "*scratch*")        ; not buffer of definition
  (let ((orig-buffer (current-buffer))
        (orig-pos (point))
        (enable-local-variables nil)    ; don't get stuck on -*- eval: -*-
        (slime-buffer-package buffer-package))
    (slime-edit-fdefinition (symbol-name name))
    ;; Postconditions
    (slime-check ("Definition of `%S' is in swank.lisp." name)
      (string= (file-name-nondirectory (buffer-file-name))
               "swank.lisp"))
    (slime-check "Definition now at point."
      (looking-at (format "(\\(defun\\|defmacro\\)\\s *%s\\s "
                          (slime-cl-symbol-name name))))
    (slime-pop-find-definition-stack)
    (slime-check "Returning from definition restores original buffer/position."
      (and (eq orig-buffer (current-buffer))
           (= orig-pos (point))))))

(def-slime-test complete-symbol
    (prefix expected-completions)
    "Find the completions of a symbol-name prefix."
    '(("cl:compile" ("cl:compile" "cl:compile-file" "cl:compile-file-pathname"
                     "cl:compiled-function" "cl:compiled-function-p"
                     "cl:compiler-macro" "cl:compiler-macro-function"))
      ("cl:foobar" nil)
      ("cl::compile-file" ("cl::compile-file" "cl::compile-file-pathname")))
  (let ((completions (slime-completions prefix)))
    (slime-check "Completion set is as expected."
      (equal expected-completions (sort completions 'string<)))))

(def-slime-test arglist
    (function-name expected-arglist)
    "Lookup the argument list for FUNCTION-NAME.
Confirm that EXPECTED-ARGLIST is displayed."
    '(("swank:start-server"
       "(swank:start-server port-file-namestring)")
      ("swank::string-prefix-p"
       "(swank::string-prefix-p s1 s2)"))
  (let ((arglist (slime-get-arglist function-name))) ;
    (slime-check ("Argument list %S is as expected." arglist)
      (string= expected-arglist arglist))))

(def-slime-test compile-defun 
    (program subform)
    "Compile PROGRAM containing errors.
Confirm that SUBFORM is correctly located."
    '(("(defun :foo () (:bar))" (:bar))
      ("(defun :foo () 
          #\\space
          ;;Sdf              
          (:bar))"
       (:bar))
      ("(defun :foo () 
             #+(or)skipped
             #| #||#
                #||# |#
             (:bar))"
       (:bar))
      ("(defun :foo () (list `(1 ,(random 10) 2 ,@(random 10) 3 ,(:bar))))"
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

(def-slime-test async-eval-debugging (depth)
  "Test recursive debugging of asynchronous evaluation requests."
  '((1) (2) (3))
  (slime-check "Automaton initially in idle state."
    ;; We expect to be at the top-level when the test starts.
    (slime-test-state-stack '(slime-idle-state)))
  (lexical-let ((depth depth)
                (debug-hook-max-depth 0))
    (let ((debug-hook
           (lambda ()
             (when (> sldb-level debug-hook-max-depth)
               (setq debug-hook-max-depth sldb-level)
               (slime-check
                   ("Automaton stack reflects debug level %S." sldb-level)
                 ;; Inspect the stack to ensure we are debugging at the
                 ;; expected recursion depth.
                 (let ((expected-stack '(slime-idle-state)))
                   (dotimes (i sldb-level)
                     (push 'slime-evaluating-state expected-stack)
                     (push 'slime-debugging-state expected-stack))
                   (slime-test-state-stack expected-stack)))
               (if (= sldb-level depth)
                   ;; We're at maximum recursion - time to unwind
                   (sldb-quit)
                 ;; Going down - enter another recursive debug
                 ;; Recursively debug.
                 (slime-eval-async 'no-such-variable nil (lambda (_) nil))
                 (slime-sync))))))
      (let ((sldb-hook (cons debug-hook sldb-hook)))
        (slime-eval-async 'no-such-variable nil (lambda (_) nil))
        (slime-sync)
        ;; Now the hook should have run once for each ,
        (slime-check ("Maximum depth reached (%S) is %S."
                      debug-hook-max-depth depth)
          (= debug-hook-max-depth depth))
        ;; FIXME: synchronize properly somehow. We are expecting Lisp
        ;; to perform a restart and unwind our stack.  and the restart
        ;; should have put us back at the top level.
        (slime-sync-state-stack '(slime-idle-state) 5)
        (slime-check "Automaton is back in idle state."
          (slime-test-state-stack '(slime-idle-state)))))))

(defun slime-test-state-stack (states)
  "True if STATES describes the current stack of states."
  (equal states (mapcar #'slime-state-name slime-state-stack)))

(defun slime-sync-state-stack (state-stack timeout)
  "Wait until the machine's stack is STATE-STACK or the timeout \
expires.\nThe timeout is given in seconds (a floating point number)."
  (let ((end (time-add (current-time) (seconds-to-time timeout))))
    (loop until (or (slime-test-state-stack state-stack)
                    (time-less-p end (current-time)))
          do (accept-process-output nil 0 100000))))

(def-slime-test loop-interrupt-quit ()
   "Test interrupting a loop."
   '(())
   (slime-check "Automaton initially in idle state."
     (slime-test-state-stack '(slime-idle-state)))
   (slime-eval-async '(cl:loop) "CL-USER" (lambda (_) ))
   (let ((sldb-hook
          (lambda ()
            (slime-check "First interrupt."
              (and (slime-test-state-stack '(slime-debugging-state
                                             slime-evaluating-state
                                            slime-idle-state))
                   (get-buffer "*sldb*")))
            (sldb-quit))))
     (accept-process-output nil 1)
     (slime-check "In eval state."
       (slime-test-state-stack '(slime-evaluating-state slime-idle-state)))
     (slime-interrupt)
     (slime-sync-state-stack '(slime-idle-state) 5)
     (slime-check "Automaton is back in idle state."
       (slime-test-state-stack '(slime-idle-state)))))

(def-slime-test loop-interrupt-continue-interrupt-quit ()
   "Test interrupting a previously interrupted but continued loop."
   '(())
   (slime-check "Automaton initially in idle state."
     (slime-test-state-stack '(slime-idle-state)))
   (slime-eval-async '(cl:loop) "CL-USER" (lambda (_) ))
   (let ((sldb-hook
          (lambda ()
            (slime-check "First interrupt."
              (and (slime-test-state-stack '(slime-debugging-state
                                             slime-evaluating-state
                                            slime-idle-state))
                   (get-buffer "*sldb*")))
            (let ((slime-evaluating-state-activation-hook 
                   (lambda ()
                     (when (slime-test-state-stack '(slime-evaluating-state 
                                                     slime-idle-state))
                       (setq slime-evaluating-state-activation-hook nil)
                       (slime-check "No sldb buffer."
                         (not (get-buffer "*sldb*")))
                       (let ((sldb-hook
                              (lambda ()
                                (slime-check "Second interrupt."
                                  (and (slime-test-state-stack 
                                        '(slime-debugging-state
                                          slime-evaluating-state
                                          slime-idle-state))
                                       (get-buffer "*sldb*")))
                                (sldb-quit))))
                         (accept-process-output nil 1)
                         (slime-check "In eval state."
                           (slime-test-state-stack 
                            '(slime-evaluating-state slime-idle-state)))
                         (slime-interrupt)
                         (slime-sync-state-stack '(slime-idle-state) 5))))))
              (sldb-continue)
              (slime-sync-state-stack '(slime-idle-state) 5)))))
     (accept-process-output nil 1)
     (slime-check "In eval state."
       (slime-test-state-stack '(slime-evaluating-state slime-idle-state)))
     (slime-interrupt)
     (slime-sync-state-stack '(slime-idle-state) 5)
     (slime-check "Automaton is back in idle state."
       (slime-test-state-stack '(slime-idle-state)))))

(def-slime-test interrupt-bubbling-idiot ()
   "Test interrupting a loop that sends a lot of output to Emacs."
   '(())
  (slime-check "Automaton initially in idle state."
    (slime-test-state-stack '(slime-idle-state)))
  (slime-eval-async '(cl:loop :for i :from 0 :do (cl:progn (cl:print i) 
                                                           (cl:force-output)))
                    "CL-USER" (lambda (_) ))
  (let ((sldb-hook
          (lambda ()
            (slime-check "First interrupt."
              (and (slime-test-state-stack '(slime-debugging-state
                                             slime-evaluating-state
                                            slime-idle-state))
                   (get-buffer "*sldb*")))
            (sldb-quit))))
     (accept-process-output nil 1)
     (slime-check "In eval state."
       (slime-test-state-stack '(slime-evaluating-state slime-idle-state)))
     (slime-interrupt)
     (slime-sync-state-stack '(slime-idle-state) 15)
     (slime-check "Automaton is back in idle state."
       (slime-test-state-stack '(slime-idle-state)))))

(def-slime-test interactive-eval ()
   "Test interactive eval and continuing from the debugger."
   '(())
  (let ((sldb-hook (lambda () (sldb-continue))))
    (slime-interactive-eval 
     "(progn(cerror \"foo\" \"restart\")(cerror \"bar\" \"restart\")(+ 1 2))")
    (slime-sync-state-stack '(slime-idle-state) 5)
    (slime-check "Automaton is back in idle state."
      (slime-test-state-stack '(slime-idle-state)))
    (let ((message (current-message)))
      (slime-check "Minibuffer contains: \"=> 3\""
        (equal "=> 3" message)))))


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

(eval-when (compile eval)
  (defmacro defun-if-undefined (name &rest rest)
    `(unless (fboundp ',name)
       (defun ,name ,@rest))))

(defun-if-undefined next-single-char-property-change
  (position prop &optional object limit)
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
		return pos))))))

(defun-if-undefined previous-single-char-property-change 
  (position prop &optional object limit)
  (let ((limit (typecase limit
		 (null nil)
		 (marker (marker-position limit))
		 (t limit))))
    (if (stringp object)
	(or (previous-single-property-change position prop object limit)
	    limit 
	    (length object))
      (with-current-buffer (or object (current-buffer))
	(let ((limit (or limit (point-min))))
	  (if (<= position limit)
	      limit
            (let ((initial-value (get-char-property (1- position)
                                                    prop object)))
              (loop for pos = position then 
                    (previous-char-property-change pos limit)
                    if (<= pos limit) return limit
                    if (not (eq initial-value 
                                (get-char-property (1- pos) prop object))) 
                    return pos))))))))

(defun-if-undefined substring-no-properties (string &optional start end)
  (let* ((start (or start 0))
	 (end (or end (length string)))
	 (string (substring string start end)))
    (set-text-properties start end nil string)
    string))

(defun-if-undefined set-window-text-height (window height)
  (let ((delta (- height (window-text-height window))))
    (unless (zerop delta)
      (let ((window-min-height 1))
	(if (and window (not (eq window (selected-window))))
	    (save-selected-window
	      (select-window window)
	      (enlarge-window delta))
	  (enlarge-window delta))))))

(defun-if-undefined window-text-height (&optional window)
  (1- (window-height window)))

(defun-if-undefined subst-char-in-string (fromchar tochar string 
						   &optional inplace)
  "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
  (let ((i (length string))
	(newstr (if inplace string (copy-sequence string))))
    (while (> i 0)
      (setq i (1- i))
      (if (eq (aref newstr i) fromchar)
	  (aset newstr i tochar)))
    newstr))

(defun-if-undefined count-screen-lines 
  (&optional beg end count-final-newline window)
  (unless beg
    (setq beg (point-min)))
  (unless end
    (setq end (point-max)))
  (if (= beg end)
      0
    (save-excursion
      (save-restriction
        (widen)
        (narrow-to-region (min beg end)
                          (if (and (not count-final-newline)
                                   (= ?\n (char-before (max beg end))))
                              (1- (max beg end))
                            (max beg end)))
        (goto-char (point-min))
        ;; XXX make this xemacs compatible
        (1+ (vertical-motion (buffer-size) window))))))

(defun-if-undefined seconds-to-time (seconds)
  "Convert SECONDS (a floating point number) to a time value."
  (list (floor seconds 65536)
	(floor (mod seconds 65536))
	(floor (* (- seconds (ffloor seconds)) 1000000))))

(defun-if-undefined time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(defun-if-undefined time-add (t1 t2)
  "Add two time values.  One should represent a time difference."
  (let ((high (car t1))
	(low (if (consp (cdr t1)) (nth 1 t1) (cdr t1)))
	(micro (if (numberp (car-safe (cdr-safe (cdr t1))))
		   (nth 2 t1)
		 0))
	(high2 (car t2))
	(low2 (if (consp (cdr t2)) (nth 1 t2) (cdr t2)))
	(micro2 (if (numberp (car-safe (cdr-safe (cdr t2))))
		    (nth 2 t2)
		  0)))
    ;; Add
    (setq micro (+ micro micro2))
    (setq low (+ low low2))
    (setq high (+ high high2))

    ;; Normalize
    ;; `/' rounds towards zero while `mod' returns a positive number,
    ;; so we can't rely on (= a (+ (* 100 (/ a 100)) (mod a 100))).
    (setq low (+ low (/ micro 1000000) (if (< micro 0) -1 0)))
    (setq micro (mod micro 1000000))
    (setq high (+ high (/ low 65536) (if (< low 0) -1 0)))
    (setq low (logand low 65535))

    (list high low micro)))

(defun emacs-20-p ()
  (and (not (featurep 'xemacs))
       (= emacs-major-version 20)))

(when (featurep 'xemacs)
  (add-hook 'sldb-hook 'sldb-xemacs-emulate-point-entered-hook))

(defun sldb-xemacs-emulate-point-entered-hook ()
  (add-hook (make-local-variable 'post-command-hook)
            'sldb-xemacs-post-command-hook))

(defun sldb-xemacs-post-command-hook ()
  (when (get-text-property (point) 'point-entered)
    (funcall (get-text-property (point) 'point-entered))))


;;; Finishing up

(mapc #'byte-compile
      '(slime-handle-oob 
        slime-log-event
        slime-events-buffer
        slime-output-string 
        slime-output-buffer
        slime-with-output-at-eob
        slime-process-available-input 
        slime-dispatch-event 
        slime-net-filter 
        slime-net-have-input-p
        slime-net-read3
        slime-net-read
        slime-print-apropos
        slime-insert-propertized))

(run-hooks 'slime-load-hook)

(provide 'slime)

;;; slime.el ends here
