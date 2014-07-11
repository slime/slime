;;; sly.el ---Superior Lisp Interaction Mode for Emacs-*-lexical-binding:t-*-
;;; 
;; Version: 1.0
;; URL: https://github.com/sly/sly
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: languages, lisp, sly

;;;; License
;;     Copyright (C) 2003  Eric Marsden, Luke Gorrie, Helmut Eller
;;     Copyright (C) 2004,2005,2006  Luke Gorrie, Helmut Eller
;;     Copyright (C) 2007,2008,2009  Helmut Eller, Tobias C. Rittweiler
;;
;;     For a detailed list of contributors, see the manual.
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


;;;; Commentary
;;
;; This file contains extensions for programming in Common Lisp. The
;; main features are:
;;
;;   A socket-based communication/RPC interface between Emacs and
;;   Lisp, enabling introspection and remote development.
;;
;;   The `sly-mode' minor-mode complementing `lisp-mode'. This new
;;   mode includes many commands for interacting with the Common Lisp
;;   process.
;;
;;   A Common Lisp debugger written in Emacs Lisp. The debugger pops up
;;   an Emacs buffer similar to the Emacs/Elisp debugger.
;;
;;   A Common Lisp inspector to interactively look at run-time data.
;;
;;   Trapping compiler messages and creating annotations in the source
;;   file on the appropriate forms.
;;
;; SLY works with Emacs >= 24.3+
;;
;; In order to run SLY, a supporting Lisp server called Swank is
;; required. Swank is distributed with sly.el and will automatically
;; be started in a normal installation.


;;;; Dependencies and setup
(require 'cl-lib)

(eval-when-compile (require 'cl)) ; defsetf, lexical-let

(eval-and-compile
  (if (version< emacs-version "24.3")
      (error "Sly requires at least Emacs 24.3")))

(require 'hyperspec "lib/hyperspec")
(require 'thingatpt)
(require 'comint)
(require 'pp)
(require 'easymenu)
(require 'outline)
(require 'arc-mode)
(require 'etags)

(eval-when-compile
  (require 'apropos)
  (require 'compile)
  (require 'gud))

(eval-and-compile
  (defvar sly-path
    (let ((path (or (locate-library "sly") load-file-name)))
      (and path (file-name-directory path)))
    "Directory containing the SLY package.
This is used to load the supporting Common Lisp library, Swank.
The default value is automatically computed from the location of the
Emacs Lisp package."))

(defvar sly-contribs nil
  "A list of contrib packages to load with SLY.")
(define-obsolete-variable-alias 'sly-setup-contribs
'sly-contribs "2.3.2")

(defun sly-setup (&optional contribs)
  "Setup Emacs so that lisp-mode buffers always use SLY.
CONTRIBS is a list of contrib packages to load. If `nil', use
`sly-contribs'. "
  (interactive)
  (add-hook 'lisp-mode-hook 'sly-editing-mode)
  (when contribs
    (setq sly-contribs contribs))
  (sly--setup-contribs))

(defvar sly-required-modules '()
  "Alist of MODULE . WHERE for swank-provided features.

MODULE is a symbol naming a specific Swank feature and WHERE is
the full pathname to the directory where the file(s)
providing the feature are found.")

(defun sly--setup-contribs ()
  "Load and initialize contribs."
  (when sly-contribs
    (add-to-list 'load-path (expand-file-name "contrib" sly-path))
    (dolist (c sly-contribs)
      (unless (and (featurep c)
                   (assq c sly-required-modules))
        (require c)
        (let ((init (intern (format "%s-init" c))))
          (when (fboundp init)
            (funcall init)))))))

(eval-and-compile
  (defun sly-version (&optional interactive)
    "Read SLY's version of its own sly.el file."
    (interactive "p")
    (let ((version
           (with-temp-buffer
             (insert-file-contents-literally
              (expand-file-name "sly.el" sly-path)
              nil 0 200)
             (and (search-forward-regexp
                   ";;[[:space:]]*Version:[[:space:]]*\\(.*\\)$" nil t)
                  (match-string 1)))))
      (if interactive
          (message "SLY %s" version)
        version))))

(defvar sly-protocol-version nil)
(setq sly-protocol-version
      (eval-when-compile (sly-version)))


;;;; Customize groups
;;
;;;;; sly

(defgroup sly nil
  "Interaction with the Superior Lisp Environment."
  :prefix "sly-"
  :group 'applications)

;;;;; sly-ui

(defgroup sly-ui nil
  "Interaction with the Superior Lisp Environment."
  :prefix "sly-"
  :group 'sly)

(defcustom sly-truncate-lines t
  "Set `truncate-lines' in popup buffers.
This applies to buffers that present lines as rows of data, such as
debugger backtraces and apropos listings."
  :type 'boolean
  :group 'sly-ui)

(defcustom sly-kill-without-query-p nil
  "If non-nil, kill SLY processes without query when quitting Emacs.
This applies to the *inferior-lisp* buffer and the network connections."
  :type 'boolean
  :group 'sly-ui)

;;;;; sly-lisp

(defgroup sly-lisp nil
  "Lisp server configuration."
  :prefix "sly-"
  :group 'sly)

(defcustom sly-init-function 'sly-init-using-asdf
  "Function bootstrapping swank on the remote.

Value is a function of two arguments: SWANK-PORTFILE and an
ingored argument for backward compatibility. Function should
return a string issuing very first commands issued by Sly to
the remote-connection process. Some time after this there should
be a port number ready in SWANK-PORTFILE."
  :type '(choice (const :tag "Use ASDF"
                        sly-init-using-asdf)
                 (const :tag "Use legacy swank-loader.lisp"
                        sly-init-using-swank-loader))
  :group 'sly-lisp)

(defcustom sly-swank-loader-backend "swank-loader.lisp"
  "The name of the swank-loader that loads the Swank server.
Only applicable if `sly-init-function' is set to
`sly-init-using-swank-loader'. This name is interpreted
relative to the directory containing sly.el, but could also be
set to an absolute filename."
  :type 'string
  :group 'sly-lisp)

(define-obsolete-variable-alias 'sly-backend
'sly-swank-loader-backend "3.0")

(defcustom sly-connected-hook nil
  "List of functions to call when SLY connects to Lisp."
  :type 'hook
  :group 'sly-lisp)

(defcustom sly-enable-evaluate-in-emacs nil
  "*If non-nil, the inferior Lisp can evaluate arbitrary forms in Emacs.
The default is nil, as this feature can be a security risk."
  :type '(boolean)
  :group 'sly-lisp)

(defcustom sly-lisp-host "127.0.0.1"
  "The default hostname (or IP address) to connect to."
  :type 'string
  :group 'sly-lisp)

(defcustom sly-port 4005
  "Port to use as the default for `sly-connect'."
  :type 'integer
  :group 'sly-lisp)

(defvar sly-connect-host-history (list sly-lisp-host))
(defvar sly-connect-port-history (list (prin1-to-string sly-port)))

(defvar sly-net-valid-coding-systems
  '((iso-latin-1-unix nil "iso-latin-1-unix")
    (iso-8859-1-unix  nil "iso-latin-1-unix")
    (binary           nil "iso-latin-1-unix")
    (utf-8-unix       t   "utf-8-unix")
    (emacs-mule-unix  t   "emacs-mule-unix")
    (euc-jp-unix      t   "euc-jp-unix"))
  "A list of valid coding systems.
Each element is of the form: (NAME MULTIBYTEP CL-NAME)")

(defun sly-find-coding-system (name)
  "Return the coding system for the symbol NAME.
The result is either an element in `sly-net-valid-coding-systems'
of nil."
  (let ((probe (assq name sly-net-valid-coding-systems)))
    (when (and probe (if (fboundp 'check-coding-system)
                         (ignore-errors (check-coding-system (car probe)))
                       (eq (car probe) 'binary)))
      probe)))

(defcustom sly-net-coding-system
  (car (cl-find-if 'sly-find-coding-system
                   sly-net-valid-coding-systems :key 'car))
  "Coding system used for network connections.
See also `sly-net-valid-coding-systems'."
  :type (cons 'choice
              (mapcar (lambda (x)
                        (list 'const (car x)))
                      sly-net-valid-coding-systems))
  :group 'sly-lisp)

;;;;; sly-mode

(defgroup sly-mode nil
  "Settings for sly-mode Lisp source buffers."
  :prefix "sly-"
  :group 'sly)

(defcustom sly-find-definitions-function 'sly-find-definitions-rpc
  "Function to find definitions for a name.
The function is called with the definition name, a string, as its
argument."
  :type 'function
  :group 'sly-mode
  :options '(sly-find-definitions-rpc
             sly-etags-definitions
             (lambda (name)
               (append (sly-find-definitions-rpc name)
                       (sly-etags-definitions name)))
             (lambda (name)
               (or (sly-find-definitions-rpc name)
                   (and tags-table-list
                        (sly-etags-definitions name))))))

(defcustom sly-complete-symbol-function 'sly-simple-complete-symbol
  "*Function to perform symbol completion."
  :group 'sly-mode
  :type '(choice (const :tag "Simple" sly-simple-complete-symbol)
                 (const :tag "Compound" sly-complete-symbol*)
                 (const :tag "Fuzzy" sly-fuzzy-complete-symbol)))

;;;;; sly-mode-faces

(defgroup sly-mode-faces nil
  "Faces in sly-mode source code buffers."
  :prefix "sly-"
  :group 'sly-mode)

(defface sly-error-face
  `((((class color) (background light))
     (:underline "red"))
    (((class color) (background dark))
     (:underline "red"))
    (t (:underline t)))
  "Face for errors from the compiler."
  :group 'sly-mode-faces)

(defface sly-warning-face
  `((((class color) (background light))
     (:underline "orange"))
    (((class color) (background dark))
     (:underline "coral"))
    (t (:underline t)))
  "Face for warnings from the compiler."
  :group 'sly-mode-faces)

(defface sly-style-warning-face
  `((((class color) (background light))
     (:underline "brown"))
    (((class color) (background dark))
     (:underline "gold"))
    (t (:underline t)))
  "Face for style-warnings from the compiler."
  :group 'sly-mode-faces)

(defface sly-note-face
  `((((class color) (background light))
     (:underline "brown4"))
    (((class color) (background dark))
     (:underline "light goldenrod"))
    (t (:underline t)))
  "Face for notes from the compiler."
  :group 'sly-mode-faces)

(defface sly-highlight-face
  '((t (:inherit highlight :underline nil)))
  "Face for compiler notes while selected."
  :group 'sly-mode-faces)

(defface sly-inspectable-value-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for things which can themselves be inspected."
  :group 'sly-mode-faces)

;;;;; sldb

(defgroup sly-debugger nil
  "Backtrace options and fontification."
  :prefix "sldb-"
  :group 'sly)

(defmacro define-sldb-faces (&rest faces)
  "Define the set of SLDB faces.
Each face specifiation is (NAME DESCRIPTION &optional PROPERTIES).
NAME is a symbol; the face will be called sldb-NAME-face.
DESCRIPTION is a one-liner for the customization buffer.
PROPERTIES specifies any default face properties."
  `(progn ,@(cl-loop for face in faces
                     collect `(define-sldb-face ,@face))))

(defmacro define-sldb-face (name description &optional default)
  (let ((facename (intern (format "sldb-%s-face" (symbol-name name)))))
    `(defface ,facename
       (list (list t ,default))
       ,(format "Face for %s." description)
       :group 'sly-debugger)))

(define-sldb-faces
  (topline        "the top line describing the error")
  (condition      "the condition class")
  (section        "the labels of major sections in the debugger buffer")
  (frame-label    "backtrace frame numbers")
  (restart-type   "restart names."
                  '(:inherit font-lock-keyword-face))
  (restart        "restart descriptions")
  (restart-number "restart numbers (correspond to keystrokes to invoke)"
                  '(:bold t))
  (frame-line     "function names and arguments in the backtrace")
  (restartable-frame-line
   "frames which are surely restartable"
   '(:inherit font-lock-function-name-face))
  (non-restartable-frame-line
   "frames which are surely not restartable")
  (detailed-frame-line
   "function names and arguments in a detailed (expanded) frame")
  (local-name     "local variable names")
  (local-value    "local variable values"
                  '(:inherit sly-inspectable-value-face))
  (catch-tag      "catch tags"))


;;;;; Key bindings
(defvar sly-doc-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") 'sly-apropos)
    (define-key map (kbd "C-z") 'sly-apropos-all)
    (define-key map (kbd "C-p") 'sly-apropos-package)
    (define-key map (kbd "C-d") 'sly-describe-symbol)
    (define-key map (kbd "C-f") 'sly-describe-function)
    (define-key map (kbd "C-h") 'sly-documentation-lookup)
    (define-key map (kbd "C-~") 'common-lisp-hyperspec-format)
    (define-key map (kbd "C-#") 'common-lisp-hyperspec-lookup-reader-macro)
    map))

(defvar sly-who-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c") 'sly-who-calls)
    (define-key map (kbd "C-w") 'sly-calls-who)
    (define-key map (kbd "C-r") 'sly-who-references)
    (define-key map (kbd "C-b") 'sly-who-binds)
    (define-key map (kbd "C-s") 'sly-who-sets)
    (define-key map (kbd "C-m") 'sly-who-macroexpands)
    (define-key map (kbd "C-a") 'sly-who-specializes)
    map))

(defvar sly-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-r")   'sly-eval-region)
    (define-key map (kbd ":")     'sly-interactive-eval)
    (define-key map (kbd "C-e")   'sly-interactive-eval)
    (define-key map (kbd "E")     'sly-edit-value)
    (define-key map (kbd "C-l")   'sly-load-file)
    (define-key map (kbd "C-b")   'sly-interrupt)
    (define-key map (kbd "M-d")   'sly-disassemble-symbol)
    (define-key map (kbd "C-t")   'sly-toggle-trace-fdefinition)
    (define-key map (kbd "I")     'sly-inspect)
    (define-key map (kbd "C-x t") 'sly-list-threads)
    (define-key map (kbd "C-x n") 'sly-cycle-connections)
    (define-key map (kbd "C-x c") 'sly-list-connections)
    (define-key map (kbd "<")     'sly-list-callers)
    (define-key map (kbd ">")     'sly-list-callees)
    ;; Include DOC keys...
    (define-key map (kbd "C-d")  sly-doc-map)
    ;; Include XREF WHO-FOO keys...
    (define-key map (kbd "C-w")  sly-who-map)
    map))

(defvar sly-mode-map
  (let ((map (make-sparse-keymap)))
    ;; These used to be a `sly-parent-map'
    (define-key map (kbd "M-.")     'sly-edit-definition)
    (define-key map (kbd "M-,")     'sly-pop-find-definition-stack)
    (define-key map (kbd "M-_")     'sly-edit-uses)    ; for German layout
    (define-key map (kbd "M-?")     'sly-edit-uses)    ; for USian layout
    (define-key map (kbd "C-x 4 .") 'sly-edit-definition-other-window)
    (define-key map (kbd "C-x 5 .") 'sly-edit-definition-other-frame)
    (define-key map (kbd "C-x C-e") 'sly-eval-last-expression)
    (define-key map (kbd "C-M-x")   'sly-eval-defun)
    ;; Include PREFIX keys...
    (define-key map (kbd "C-c")     sly-prefix-map)
    ;; Completion
    (define-key map (kbd "C-c TAB") 'sly-complete-symbol)
    ;; Evaluating
    (define-key map (kbd "C-c C-p") 'sly-pprint-eval-last-expression)
    ;; Macroexpand
    (define-key map (kbd "C-c C-m") 'sly-expand-1)
    (define-key map (kbd "C-c M-m") 'sly-macroexpand-all)
    ;; Misc
    (define-key map (kbd "C-c C-u") 'sly-undefine-function)
    (define-key map (kbd "C-M-.") 'sly-next-location)
    (define-key map (kbd "C-M-,") 'sly-previous-location)
    map))

(defvar sly-editing-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-p")     'sly-previous-note)
    (define-key map (kbd "M-n")     'sly-next-note)
    (define-key map (kbd "C-c M-c") 'sly-remove-notes)
    (define-key map (kbd "C-c C-k") 'sly-compile-and-load-file)
    (define-key map (kbd "C-c M-k") 'sly-compile-file)
    (define-key map (kbd "C-c C-c") 'sly-compile-defun)
    map))



;;;; Minor modes

;;;;; sly-mode
(defvar sly-buffer-connection)
(defvar sly-dispatching-connection)
(defvar sly-current-thread)

(define-minor-mode sly-mode
  "Minor mode for horizontal SLY functionality."
  nil nil nil)

(define-minor-mode sly-editing-mode
  "Minor mode for editing `lisp-mode' buffers."
  nil nil nil
  (sly-mode 1)
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function))


;;;;;; Modeline
(add-to-list 'minor-mode-alist
             `(sly-mode (:eval (sly-modeline-string))))

(defun sly-modeline-string ()
  "Return the string to display in the modeline.
\"SLY\" only appears if we aren't connected.  If connected,
include package-name, connection-name, and possibly some state
information."
  (let ((conn (sly-current-connection)))
    ;; Bail out early in case there's no connection, so we won't
    ;; implicitly invoke `sly-connection' which may query the user.
    (if (not conn)
        (and (symbol-value 'sly-mode) " SLY")
      (let ((pkg   (sly-current-package)))
        (concat " "
                (if pkg
                    (concat (sly-pretty-package-name pkg) " "))
                ;; ignore errors for closed connections
                (ignore-errors (sly-connection-name conn))
                (sly-modeline-state-string conn))))))

(defun sly-pretty-package-name (name)
  "Return a pretty version of a package name NAME."
  (cond ((string-match "^#?:\\(.*\\)$" name)
         (match-string 1 name))
        ((string-match "^\"\\(.*\\)\"$" name)
         (match-string 1 name))
        (t name)))

(defun sly-modeline-state-string (conn)
  "Return a string possibly describing CONN's state."
  (cond ((not (eq (process-status conn) 'open))
         (format " %s" (process-status conn)))
        ((let ((pending (length (sly-rex-continuations conn)))
               (sldbs (length (sldb-buffers conn))))
           (cond ((and (zerop sldbs) (zerop pending)) nil)
                 ((zerop sldbs) (format " %s" pending))
                 (t (format " %s/%s" pending sldbs)))))))


;;;; Framework'ey bits
;;;
;;; This section contains some standard SLY idioms: basic macros,
;;; ways of showing messages to the user, etc. All the code in this
;;; file should use these functions when applicable.
;;;
;;;;; Syntactic sugar

(cl-defmacro when-let ((var value) &rest body)
  "Evaluate VALUE, if the result is non-nil bind it to VAR and eval BODY.

\(fn (VAR VALUE) &rest BODY)"
  (declare (indent 1))
  `(let ((,var ,value))
     (when ,var ,@body)))

(defmacro destructure-case (value &rest patterns)
  (declare (indent 1)
           (debug (sexp &rest (sexp &rest form))))
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (cl-gensym "op-"))
	(operands (cl-gensym "rand-"))
	(tmp (cl-gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (cl-case ,operator
	 ,@(mapcar (lambda (clause)
                     (if (eq (car clause) t)
                         `(t ,@(cdr clause))
                       (cl-destructuring-bind ((op &rest rands) &rest body)
                           clause
                         `(,op (cl-destructuring-bind ,rands ,operands
                                 . ,(or body
                                        '((ignore)) ; suppress some warnings
                                        ))))))
		   patterns)
	 ,@(if (eq (caar (last patterns)) t)
	       '()
	     `((t (error "Elisp destructure-case failed: %S" ,tmp))))))))

(defmacro sly-define-keys (keymap &rest key-command)
  "Define keys in KEYMAP. Each KEY-COMMAND is a list of (KEY COMMAND)."
  (declare (indent 1))
  `(progn . ,(mapcar (lambda (k-c) `(define-key ,keymap . ,k-c))
		     key-command)))

(cl-defmacro with-struct ((conc-name &rest slots) struct &body body)
  "Like with-slots but works only for structs.
\(fn (CONC-NAME &rest SLOTS) STRUCT &body BODY)"
  (declare (indent 2)
           (debug (sexp sexp &rest form)))
  (let ((struct-var (cl-gensym "struct"))
        (reader (lambda (slot)
                  (intern (concat (symbol-name conc-name)
                                  (symbol-name slot))))))
    `(let ((,struct-var ,struct))
       (cl-symbol-macrolet
           ,(mapcar (lambda (slot)
                      (cl-etypecase slot
                        (symbol `(,slot (,(funcall reader slot) ,struct-var)))
                        (cons `(,(cl-first slot)
                                (,(funcall reader (cl-second slot))
                                 ,struct-var)))))
                    slots)
         . ,body))))

;;;;; Very-commonly-used functions

(defvar sly-message-function 'message)

;; Interface
(defun sly-buffer-name (type &optional hidden)
  (cl-assert (keywordp type))
  (concat (if hidden " " "")
          (format "*sly-%s*" (substring (symbol-name type) 1))))

;; Interface
(defun sly-message (format &rest args)
  "Like `message' but with special support for multi-line messages.
Single-line messages use the echo area."
  (apply sly-message-function format args))

(defun sly-display-warning (message &rest args)
  (display-warning '(sly warning) (apply #'format message args)))

(defvar sly-background-message-function 'sly-display-oneliner)

;; Interface
(defun sly-background-message (format-string &rest format-args)
  "Display a message in passing.
This is like `sly-message', but less distracting because it
will never pop up a buffer or display multi-line messages.
It should be used for \"background\" messages such as argument lists."
  (apply sly-background-message-function format-string format-args))

(defun sly-display-oneliner (format-string &rest format-args)
  (let* ((msg (apply #'format format-string format-args)))
    (unless (minibuffer-window-active-p (minibuffer-window))
      (message  "%s" (sly-oneliner msg)))))

(defun sly-oneliner (string)
  "Return STRING truncated to fit in a single echo-area line."
  (substring string 0 (min (length string)
                           (or (cl-position ?\n string) most-positive-fixnum)
                           (1- (window-width (minibuffer-window))))))

(defun sly-recenter (target)
  "Try to make the region between point and TARGET visible.
Minimize point motion if possible."
  (let ((window-height (window-text-height))
        (height-diff (abs (- (line-number-at-pos target)
                             (line-number-at-pos (point))))))
    (when (or (> height-diff window-height)
              (not (pos-visible-in-window-p target)))
      (recenter (if (< target (point))
                  (min (- window-height 2) height-diff)
                (max 0 (- window-height height-diff 1)))))))

;; Interface
(defun sly-set-truncate-lines ()
  "Apply `sly-truncate-lines' to the current buffer."
  (when sly-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

;; Interface
(defun sly-read-package-name (prompt &optional initial-value)
  "Read a package name from the minibuffer, prompting with PROMPT."
  (let ((completion-ignore-case t))
    (completing-read prompt (sly-bogus-completion-alist
                             (sly-eval
                              `(swank:list-all-package-names t)))
		     nil t initial-value)))

;; Interface
(defun sly-read-symbol-name (prompt &optional query)
  "Either read a symbol name or choose the one at point.
The user is prompted if a prefix argument is in effect, if there is no
symbol at point, or if QUERY is non-nil."
  (cond ((or current-prefix-arg query (not (sly-symbol-at-point)))
         (sly-read-from-minibuffer prompt (sly-symbol-at-point)))
        (t (sly-symbol-at-point))))

;; Interface
(defmacro sly-propertize-region (props &rest body)
  "Execute BODY and add PROPS to all the text it inserts.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY."
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((start (cl-gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
	 (add-text-properties ,start (point) ,props)))))

(defun sly-add-face (face string)
  (declare (indent 1))
  (add-text-properties 0 (length string) (list 'face face) string)
  string)

;; Interface
(defsubst sly-insert-propertized (props &rest args)
  "Insert all ARGS and then add text-PROPS to the inserted text."
  (sly-propertize-region props (apply #'insert args)))

(defmacro sly-with-rigid-indentation (level &rest body)
  "Execute BODY and then rigidly indent its text insertions.
Assumes all insertions are made at point."
  (declare (indent 1))
  (let ((start (cl-gensym)) (l (cl-gensym)))
    `(let ((,start (point)) (,l ,(or level '(current-column))))
       (prog1 (progn ,@body)
         (sly-indent-rigidly ,start (point) ,l)))))

(defun sly-indent-rigidly (start end column)
  ;; Similar to `indent-rigidly' but doesn't inherit text props.
  (let ((indent (make-string column ?\ )))
    (save-excursion
      (goto-char end)
      (beginning-of-line)
      (while (and (<= start (point))
                  (progn
                    (insert-before-markers indent)
                    (zerop (forward-line -1))))))))

(defun sly-insert-indented (&rest strings)
  "Insert all arguments rigidly indented."
  (sly-with-rigid-indentation nil
    (apply #'insert strings)))

(defun sly-property-bounds (prop)
  "Return two the positions of the previous and next changes to PROP.
PROP is the name of a text property."
  (cl-assert (get-text-property (point) prop))
  (let ((end (next-single-char-property-change (point) prop)))
    (list (previous-single-char-property-change end prop) end)))

(defun sly-curry (fun &rest args)
  "Partially apply FUN to ARGS.  The result is a new function.
This idiom is preferred over `lexical-let'."
  `(lambda (&rest more) (apply ',fun (append ',args more))))

(defun sly-rcurry (fun &rest args)
  "Like `sly-curry' but ARGS on the right are applied."
  `(lambda (&rest more) (apply ',fun (append more ',args))))


;;;;; Temporary popup buffers

;; keep compiler quiet
(defvar sly-buffer-package)
(defvar sly-buffer-connection)

;; Interface
(cl-defmacro sly-with-popup-buffer ((name &key package connection select
                                            mode)
                                      &body body)
  "Similar to `with-output-to-temp-buffer'.
Bind standard-output and initialize some buffer-local variables.
Restore window configuration when closed.

NAME is the name of the buffer to be created.
PACKAGE is the value `sly-buffer-package'.
CONNECTION is the value for `sly-buffer-connection',
 if nil, no explicit connection is associated with
 the buffer.  If t, the current connection is taken.
MODE is the name of a major mode which will be enabled.
"
  (declare (indent 1))
  (let ((package-sym (cl-gensym "package-"))
        (connection-sym (cl-gensym "connection-")))
    `(let ((,package-sym ,(if (eq package t)
                              `(sly-current-package)
                            package))
           (,connection-sym ,(if (eq connection t)
                                 `(sly-current-connection)
                               connection)))
       (with-current-buffer (get-buffer-create ,name)
         (let ((inhibit-read-only t)
               (standard-output (current-buffer)))
           (erase-buffer)
           (funcall (or ,mode 'fundamental-mode))
           (setq sly-buffer-package ,package-sym
                 sly-buffer-connection ,connection-sym)
           (set-syntax-table lisp-mode-syntax-table)
           ,@body
           (sly-popup-buffer-mode 1)
           (funcall (if ,select 'pop-to-buffer 'display-buffer)
                    (current-buffer))
           (current-buffer))))))

(defvar sly-popup-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    ;;("\C-c\C-z" . sly-switch-to-output-buffer)
    (define-key map (kbd "M-.") 'sly-edit-definition)
    map))

(define-minor-mode sly-popup-buffer-mode
  "Mode for displaying read only stuff"
  nil nil nil
  (sly-mode 1)
  (setq buffer-read-only t))

(add-to-list 'minor-mode-alist
             `(sly-popup-buffer-mode
               (:eval (unless sly-mode
                        (sly-modeline-string)))))

;;;;; Filename translation
;;;
;;; Filenames passed between Emacs and Lisp should be translated using
;;; these functions. This way users who run Emacs and Lisp on separate
;;; machines have a chance to integrate file operations somehow.

(defvar sly-to-lisp-filename-function #'convert-standard-filename
  "Function to translate Emacs filenames to CL namestrings.")
(defvar sly-from-lisp-filename-function #'identity
  "Function to translate CL namestrings to Emacs filenames.")

(defun sly-to-lisp-filename (filename)
  "Translate the string FILENAME to a Lisp filename."
  (funcall sly-to-lisp-filename-function filename))

(defun sly-from-lisp-filename (filename)
  "Translate the Lisp filename FILENAME to an Emacs filename."
  (funcall sly-from-lisp-filename-function filename))


;;;; Starting SLY
;;;
;;; This section covers starting an inferior-lisp, compiling and
;;; starting the server, initiating a network connection.

;;;;; Entry points

;; We no longer load inf-lisp, but we use this variable for backward
;; compatibility.
(defvar inferior-lisp-program "lisp"
  "*Program name for invoking an inferior Lisp with for Inferior Lisp mode.")

(defvar sly-lisp-implementations nil
  "*A list of known Lisp implementations.
The list should have the form:
  ((NAME (PROGRAM PROGRAM-ARGS...) &key KEYWORD-ARGS) ...)

NAME is a symbol for the implementation.
PROGRAM and PROGRAM-ARGS are strings used to start the Lisp process.
For KEYWORD-ARGS see `sly-start'.

Here's an example:
 ((cmucl (\"/opt/cmucl/bin/lisp\" \"-quiet\") :init sly-init-command)
  (acl (\"acl7\") :coding-system emacs-mule))")

(defvar sly-default-lisp nil
  "*The name of the default Lisp implementation.
See `sly-lisp-implementations'")

;; dummy definitions for the compiler
(defvar sly-net-processes)
(defvar sly-default-connection)

(defun sly (&optional command coding-system)
  "Start an inferior^_superior Lisp and connect to its Swank server."
  (interactive)
  (let ((inferior-lisp-program (or command inferior-lisp-program))
        (sly-net-coding-system (or coding-system sly-net-coding-system)))
    (sly-start* (cond ((and command (symbolp command))
                         (sly-lisp-options command))
                        (t (sly-read-interactive-args))))))

(defvar sly-inferior-lisp-program-history '()
  "History list of command strings.  Used by `sly'.")

(defun sly-read-interactive-args ()
  "Return the list of args which should be passed to `sly-start'.

The rules for selecting the arguments are rather complicated:

- In the most common case, i.e. if there's no prefix-arg in
  effect and if `sly-lisp-implementations' is nil, use
  `inferior-lisp-program' as fallback.

- If the table `sly-lisp-implementations' is non-nil use the
  implementation with name `sly-default-lisp' or if that's nil
  the first entry in the table.

- If the prefix-arg is `-', prompt for one of the registered
  lisps.

- If the prefix-arg is positive, read the command to start the
  process."
  (let ((table sly-lisp-implementations))
    (cond ((not current-prefix-arg) (sly-lisp-options))
          ((eq current-prefix-arg '-)
           (let ((key (completing-read
                       "Lisp name: " (mapcar (lambda (x)
                                               (list (symbol-name (car x))))
                                             table)
                       nil t)))
             (sly-lookup-lisp-implementation table (intern key))))
          (t
           (cl-destructuring-bind (program &rest program-args)
               (split-string-and-unquote
                (read-shell-command "Run lisp: " inferior-lisp-program
                                    'sly-inferior-lisp-program-history))
             (let ((coding-system
                    (if (eq 16 (prefix-numeric-value current-prefix-arg))
                        (read-coding-system "set sly-coding-system: "
                                            sly-net-coding-system)
                      sly-net-coding-system)))
               (list :program program :program-args program-args
                     :coding-system coding-system)))))))

(defun sly-lisp-options (&optional name)
  (let ((table sly-lisp-implementations))
    (cl-assert (or (not name) table))
    (cond (table (sly-lookup-lisp-implementation sly-lisp-implementations
                                                   (or name sly-default-lisp
                                                       (car (car table)))))
          (t (cl-destructuring-bind (program &rest args)
                 (split-string inferior-lisp-program)
               (list :program program :program-args args))))))

(defun sly-lookup-lisp-implementation (table name)
  (let ((arguments (cl-rest (assoc name table))))
    (unless arguments
      (error "Could not find lisp implementation with the name '%S'" name))
    (when (and (= (length arguments) 1)
               (functionp (cl-first arguments)))
      (setf arguments (funcall (cl-first arguments))))
    (cl-destructuring-bind ((prog &rest args) &rest keys) arguments
      (cl-list* :name name :program prog :program-args args keys))))

(cl-defun sly-start (&key (program inferior-lisp-program) program-args
                          directory
                          (coding-system sly-net-coding-system)
                          (init sly-init-function)
                          name
                          (buffer "*inferior-lisp*")
                          init-function
                          env)
  "Start a Lisp process and connect to it.
This function is intended for programmatic use if `sly' is not
flexible enough.

PROGRAM and PROGRAM-ARGS are the filename and argument strings
  for the subprocess.
INIT is a function that should return a string to load and start
  Swank. The function will be called with the PORT-FILENAME and ENCODING as
  arguments.  INIT defaults to `sly-init-function'.
CODING-SYSTEM a symbol for the coding system. The default is
  sly-net-coding-system
ENV environment variables for the subprocess (see `process-environment').
INIT-FUNCTION function to call right after the connection is established.
BUFFER the name of the buffer to use for the subprocess.
NAME a symbol to describe the Lisp implementation
DIRECTORY change to this directory before starting the process.
"
  (let ((args (list :program program :program-args program-args :buffer buffer
                    :coding-system coding-system :init init :name name
                    :init-function init-function :env env)))
    (sly-check-coding-system coding-system)
    (when (sly-bytecode-stale-p)
      (sly-urge-bytecode-recompile))
    (let ((proc (sly-maybe-start-lisp program program-args env
                                        directory buffer)))
      (sly-inferior-connect proc args)
      (pop-to-buffer (process-buffer proc)))))

(defun sly-start* (options)
  (apply #'sly-start options))

(defun sly-connect (host port &optional _coding-system interactive-p)
  "Connect to a running Swank server. Return the connection."
  (interactive (list (read-from-minibuffer
                      "Host: " (cl-first sly-connect-host-history)
                      nil nil '(sly-connect-host-history . 1))
                     (string-to-number
                      (read-from-minibuffer
                       "Port: " (cl-first sly-connect-port-history)
                       nil nil '(sly-connect-port-history . 1)))
                     nil t))
  (when (and interactive-p
             sly-net-processes
             (y-or-n-p "Close old connections first? "))
    (sly-disconnect-all))
  (message "Connecting to Swank on port %S.." port)
  (let* ((process (sly-net-connect host port))
         (sly-dispatching-connection process))
    (sly-setup-connection process)))

;; FIXME: seems redundant
(defun sly-start-and-init (options fun)
  (let* ((rest (plist-get options :init-function))
         (init (cond (rest `(lambda () (funcall ',rest) (funcall ',fun)))
                     (t fun))))
    (sly-start* (plist-put (cl-copy-list options) :init-function init))))

;;;;; Start inferior lisp
;;;
;;; Here is the protocol for starting SLY:
;;;
;;;   0. Emacs recompiles/reloads sly.elc if it exists and is stale.
;;;   1. Emacs starts an inferior Lisp process.
;;;   2. Emacs tells Lisp (via stdio) to load and start Swank.
;;;   3. Lisp recompiles the Swank if needed.
;;;   4. Lisp starts the Swank server and writes its TCP port to a temp file.
;;;   5. Emacs reads the temp file to get the port and then connects.
;;;   6. Emacs prints a message of warm encouragement for the hacking ahead.
;;;
;;; Between steps 2-5 Emacs polls for the creation of the temp file so
;;; that it can make the connection. This polling may continue for a
;;; fair while if Swank needs recompilation.

(defvar sly-connect-retry-timer nil
  "Timer object while waiting for an inferior-lisp to start.")

;;; Recompiling bytecode:

(defun sly-bytecode-stale-p ()
  "Return true if sly.elc is older than sly.el."
  (when-let (libfile (locate-library "sly"))
    (let* ((basename (file-name-sans-extension libfile))
           (sourcefile (concat basename ".el"))
           (bytefile (concat basename ".elc")))
      (and (file-exists-p bytefile)
           (file-newer-than-file-p sourcefile bytefile)))))

(defun sly-recompile-bytecode ()
  "Recompile and reload sly."
  (interactive)
  (let ((sourcefile (concat (file-name-sans-extension (locate-library "sly"))
                            ".el")))
    (byte-compile-file sourcefile t)))

(defun sly-urge-bytecode-recompile ()
  "Urge the user to recompile sly.elc.
Return true if we have been given permission to continue."
  (when (y-or-n-p "sly.elc is older than source.  Recompile first? ")
    (sly-recompile-bytecode)))

(defun sly-abort-connection ()
  "Abort connection the current connection attempt."
  (interactive)
  (cond (sly-connect-retry-timer
         (sly-cancel-connect-retry-timer)
         (message "Cancelled connection attempt."))
        (t (error "Not connecting"))))

;;; Starting the inferior Lisp and loading Swank:

(defun sly-maybe-start-lisp (program program-args env directory buffer)
  "Return a new or existing inferior lisp process."
  (cond ((not (comint-check-proc buffer))
         (sly-start-lisp program program-args env directory buffer))
        ((sly-reinitialize-inferior-lisp-p program program-args env buffer)
         (when-let (conn (cl-find (get-buffer-process buffer)
                                  sly-net-processes
                                  :key #'sly-inferior-process))
           (sly-net-close conn))
         (get-buffer-process buffer))
        (t (sly-start-lisp program program-args env directory
                             (generate-new-buffer-name buffer)))))

(defun sly-reinitialize-inferior-lisp-p (program program-args env buffer)
  (let ((args (sly-inferior-lisp-args (get-buffer-process buffer))))
    (and (equal (plist-get args :program) program)
         (equal (plist-get args :program-args) program-args)
         (equal (plist-get args :env) env)
         (not (y-or-n-p "Create an additional *inferior-lisp*? ")))))

(defvar sly-inferior-process-start-hook nil
  "Hook called whenever a new process gets started.")

(defun sly-start-lisp (program program-args env directory buffer)
  "Does the same as `inferior-lisp' but less ugly.
Return the created process."
  (with-current-buffer (get-buffer-create buffer)
    (when directory
      (cd (expand-file-name directory)))
    (comint-mode)
    (let ((process-environment (append env process-environment))
          (process-connection-type nil))
      (comint-exec (current-buffer) "inferior-lisp" program nil program-args))
    (lisp-mode-variables t)
    (let ((proc (get-buffer-process (current-buffer))))
      (set-process-query-on-exit-flag proc (not sly-kill-without-query-p))
      (run-hooks 'sly-inferior-process-start-hook)
      proc)))

(defun sly-inferior-connect (process args)
  "Start a Swank server in the inferior Lisp and connect."
  (sly-delete-swank-port-file 'quiet)
  (sly-start-swank-server process args)
  (sly-read-port-and-connect process))

(defvar sly-inferior-lisp-args nil
  "A buffer local variable in the inferior proccess.
See `sly-start'.")

(defun sly-start-swank-server (process args)
  "Start a Swank server on the inferior lisp."
  (cl-destructuring-bind (&key coding-system init &allow-other-keys) args
    (with-current-buffer (process-buffer process)
      (make-local-variable 'sly-inferior-lisp-args)
      (setq sly-inferior-lisp-args args)
      (let ((str (funcall init (sly-swank-port-file) coding-system)))
        (goto-char (process-mark process))
        (insert-before-markers str)
        (process-send-string process str)))))

(defun sly-inferior-lisp-args (process)
  "Return the initial process arguments.
See `sly-start'."
  (with-current-buffer (process-buffer process)
    sly-inferior-lisp-args))

(defun sly-init-using-asdf (port-filename _coding-system)
  "Return a string to initialize Lisp using ASDF.

Fall back to `sly-init-using-swank-loader' if ASDF fails."
  (pp-to-string
   `(cond ((ignore-errors
             (funcall 'require "asdf")
             (funcall (read-from-string "asdf:version-satisfies")
                      (funcall (read-from-string "asdf:asdf-version"))
                      "2.019"))
           (push (pathname ,(sly-to-lisp-filename sly-path))
                 (symbol-value
                  (read-from-string "asdf:*central-registry*")))
           (funcall
            (read-from-string "asdf:load-system")
            :swank)
           (funcall
            (read-from-string "swank:start-server")
            ,port-filename))
          (t
           ,(read (sly-init-using-swank-loader port-filename _coding-system))))))

;; XXX load-server & start-server used to be separated. maybe that was  better.
(defun sly-init-using-swank-loader (port-filename _coding-system)
  "Return a string to initialize Lisp."
  (let ((loader (sly-to-lisp-filename
                 (expand-file-name sly-swank-loader-backend sly-path))))
    ;; Return a single form to avoid problems with buffered input.
    (format "%S\n\n"
            `(progn
               (load ,loader :verbose t)
               (funcall (read-from-string "swank-loader:init"))
               (funcall (read-from-string "swank:start-server")
                        ,port-filename)))))

(defun sly-swank-port-file ()
  "Filename where the SWANK server writes its TCP port number."
  (expand-file-name (format "sly.%S" (emacs-pid)) (sly-temp-directory)))

(defun sly-temp-directory ()
  (cond ((fboundp 'temp-directory) (temp-directory))
        ((boundp 'temporary-file-directory) temporary-file-directory)
        (t "/tmp/")))

(defun sly-delete-swank-port-file (&optional quiet)
  (condition-case data
      (delete-file (sly-swank-port-file))
    (error
     (cl-ecase quiet
       ((nil) (signal (car data) (cdr data)))
       (quiet)
       (message (message "Unable to delete swank port file %S"
                         (sly-swank-port-file)))))))

(defun sly-read-port-and-connect (inferior-process)
  (sly-attempt-connection inferior-process nil 1))

(defun sly-attempt-connection (process retries attempt)
  ;; A small one-state machine to attempt a connection with
  ;; timer-based retries.
  (sly-cancel-connect-retry-timer)
  (let ((file (sly-swank-port-file)))
    (unless (active-minibuffer-window)
      (message "Polling %S .. %d (Abort with `M-x sly-abort-connection'.)"
               file attempt))
    (cond ((and (file-exists-p file)
                (> (nth 7 (file-attributes file)) 0)) ; file size
           (let ((port (sly-read-swank-port))
                 (args (sly-inferior-lisp-args process)))
             (sly-delete-swank-port-file 'message)
             (let ((c (sly-connect sly-lisp-host port
                                   (plist-get args :coding-system))))
               (sly-set-inferior-process c process))))
          ((and retries (zerop retries))
           (message "Gave up connecting to Swank after %d attempts." attempt))
          ((eq (process-status process) 'exit)
           (message "Failed to connect to Swank: inferior process exited."))
          (t
           (when (and (file-exists-p file)
                      (zerop (nth 7 (file-attributes file))))
             (message "(Zero length port file)")
             ;; the file may be in the filesystem but not yet written
             (unless retries (setq retries 3)))
           (cl-assert (not sly-connect-retry-timer))
           (setq sly-connect-retry-timer
                 (run-with-timer
                  0.3 nil
                  #'sly-timer-call #'sly-attempt-connection
                  process (and retries (1- retries))
                  (1+ attempt)))))))

(defun sly-timer-call (fun &rest args)
  "Call function FUN with ARGS, reporting all errors.

The default condition handler for timer functions (see
`timer-event-handler') ignores errors."
  (condition-case data
      (apply fun args)
    ((debug error)
     (debug nil (list "Error in timer" fun args data)))))

(defun sly-cancel-connect-retry-timer ()
  (when sly-connect-retry-timer
    (cancel-timer sly-connect-retry-timer)
    (setq sly-connect-retry-timer nil)))

(defun sly-read-swank-port ()
  "Read the Swank server port number from the `sly-swank-port-file'."
  (save-excursion
    (with-temp-buffer
      (insert-file-contents (sly-swank-port-file))
      (goto-char (point-min))
      (let ((port (read (current-buffer))))
        (cl-assert (integerp port))
        port))))

(defun sly-toggle-debug-on-swank-error ()
  (interactive)
  (if (sly-eval `(swank:toggle-debug-on-swank-error))
      (message "Debug on SWANK error enabled.")
    (message "Debug on SWANK error disabled.")))

;;; Words of encouragement

(defun sly-user-first-name ()
  (let ((name (if (string= (user-full-name) "")
                  (user-login-name)
                (user-full-name))))
    (string-match "^[^ ]*" name)
    (capitalize (match-string 0 name))))

(defvar sly-words-of-encouragement
  `("Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    "Your hacking starts... NOW!"
    "May the source be with you!"
    "Take this REPL, brother, and may it serve you well."
    "Lemonodor-fame is but a hack away!"
    ,(format "%s, this could be the start of a beautiful program."
             (sly-user-first-name)))
  "Scientifically-proven optimal words of hackerish encouragement.")

(defun sly-random-words-of-encouragement ()
  "Return a string of hackerish encouragement."
  (eval (nth (random (length sly-words-of-encouragement))
             sly-words-of-encouragement)))


;;;; Networking
;;;
;;; This section covers the low-level networking: establishing
;;; connections and encoding/decoding protocol messages.
;;;
;;; Each SLY protocol message beings with a 6-byte header followed
;;; by an S-expression as text. The sexp must be readable both by
;;; Emacs and by Common Lisp, so if it contains any embedded code
;;; fragments they should be sent as strings:
;;;
;;; The set of meaningful protocol messages are not specified
;;; here. They are defined elsewhere by the event-dispatching
;;; functions in this file and in swank.lisp.

(defvar sly-net-processes nil
  "List of processes (sockets) connected to Lisps.")

(defvar sly-net-process-close-hooks '()
  "List of functions called when a sly network connection closes.
The functions are called with the process as their argument.")

(defun sly-secret ()
  "Find the magic secret from the user's home directory.
Return nil if the file doesn't exist or is empty; otherwise the
first line of the file."
  (condition-case _err
      (with-temp-buffer
	(insert-file-contents "~/.sly-secret")
	(goto-char (point-min))
	(buffer-substring (point-min) (line-end-position)))
    (file-error nil)))

;;; Interface
(defvar sly--net-connect-counter 0)
(defun sly-net-connect (host port)
  "Establish a connection with a CL."
  (let* ((inhibit-quit nil)
         (name (format "sly-%s" (incf sly--net-connect-counter)))
         (proc (open-network-stream name nil host port))
         (buffer (sly-make-net-buffer (format " *%s*" name))))
    (push proc sly-net-processes)
    (set-process-plist proc `(sly--net-connect-counter ,sly--net-connect-counter))
    (set-process-buffer proc buffer)
    (set-process-filter proc 'sly-net-filter)
    (set-process-sentinel proc 'sly-net-sentinel)
    (set-process-query-on-exit-flag proc (not sly-kill-without-query-p))
    (when (fboundp 'set-process-coding-system)
      (set-process-coding-system proc 'binary 'binary))
    (when-let (secret (sly-secret))
      (sly-net-send secret proc))
    proc))

(defun sly-make-net-buffer (name)
  "Make a buffer suitable for a network process."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (set (make-local-variable 'kill-buffer-query-functions) nil))
    buffer))

;;;;; Coding system madness

(defun sly-check-coding-system (coding-system)
  "Signal an error if CODING-SYSTEM isn't a valid coding system."
  (interactive)
  (let ((props (sly-find-coding-system coding-system)))
    (unless props
      (error "Invalid sly-net-coding-system: %s. %s"
             coding-system (mapcar #'car sly-net-valid-coding-systems)))
    (when (and (cl-second props) (boundp 'default-enable-multibyte-characters))
      (cl-assert default-enable-multibyte-characters))
    t))

(defun sly-coding-system-mulibyte-p (coding-system)
  (cl-second (sly-find-coding-system coding-system)))

(defun sly-coding-system-cl-name (coding-system)
  (cl-third (sly-find-coding-system coding-system)))

;;; Interface
(defun sly-net-send (sexp proc)
  "Send a SEXP to Lisp over the socket PROC.
This is the lowest level of communication. The sexp will be READ and
EVAL'd by Lisp."
  (let* ((payload (encode-coding-string
                   (concat (sly-prin1-to-string sexp) "\n")
                   'utf-8-unix))
         (string (concat (sly-net-encode-length (length payload))
                         payload)))
    (sly-log-event sexp)
    (process-send-string proc string)))

(defun sly-safe-encoding-p (coding-system string)
  "Return true iff CODING-SYSTEM can safely encode STRING."
  (or (let ((candidates (find-coding-systems-string string))
            (base (coding-system-base coding-system)))
        (or (equal candidates '(undecided))
            (memq base candidates)))
      (and (not (multibyte-string-p string))
           (not (sly-coding-system-mulibyte-p coding-system)))))

(defun sly-net-close (process &optional debug)
  (setq sly-net-processes (remove process sly-net-processes))
  (when (eq process sly-default-connection)
    (setq sly-default-connection nil))
  (cond (debug
         (set-process-sentinel process 'ignore)
         (set-process-filter process 'ignore)
         (delete-process process))
        (t
         (run-hook-with-args 'sly-net-process-close-hooks process)
         ;; killing the buffer also closes the socket
         (kill-buffer (process-buffer process)))))

(defun sly-net-sentinel (process message)
  (message "Lisp connection closed unexpectedly: %s" message)
  (sly-net-close process))

;;; Socket input is handled by `sly-net-filter', which decodes any
;;; complete messages and hands them off to the event dispatcher.

(defun sly-net-filter (process string)
  "Accept output from the socket and process all complete messages."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  (sly-process-available-input process))

(defun sly-process-available-input (process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (sly-net-have-input-p)
      (let ((event (sly-net-read-or-lose process))
            (ok nil))
        (sly-log-event event)
        (unwind-protect
            (save-current-buffer
              (sly-dispatch-event event process)
              (setq ok t))
          (unless ok
            (sly-run-when-idle 'sly-process-available-input process)))))))

(defun sly-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (sly-net-decode-length))))

(defun sly-run-when-idle (function &rest args)
  "Call FUNCTION as soon as Emacs is idle."
  (apply #'run-at-time 0 nil function args))

(defun sly-handle-net-read-error (error)
  (let ((packet (buffer-string)))
    (sly-with-popup-buffer ((sly-buffer-name :error))
      (princ (format "%s\nin packet:\n%s" (error-message-string error) packet))
      (goto-char (point-min)))
    (cond ((y-or-n-p "Skip this packet? ")
           `(:emacs-skipped-packet ,packet))
          (t
           (when (y-or-n-p "Enter debugger instead? ")
             (debug 'error error))
           (signal (car error) (cdr error))))))

(defun sly-net-read-or-lose (process)
  (condition-case error
      (sly-net-read)
    (error
     (sly-net-close process t)
     (error "net-read error: %S" error))))

(defun sly-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (sly-net-decode-length))
         (start (+ (point) 6))
         (end (+ start length)))
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (condition-case error
                 (progn
                   (decode-coding-region start end 'utf-8-unix)
                   (setq end (point-max))
                   (read (current-buffer)))
               (error
                (sly-handle-net-read-error error))))
      (delete-region (point-min) end))))

(defun sly-net-decode-length ()
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6))
                    16))

(defun sly-net-encode-length (n)
  (format "%06x" n))

(defun sly-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (let (print-escape-nonascii
        print-escape-newlines
        print-length
        print-level)
    (prin1-to-string sexp)))


;;;; Connections
;;;
;;; "Connections" are the high-level Emacs<->Lisp networking concept.
;;;
;;; Emacs has a connection to each Lisp process that it's interacting
;;; with. Typically there would only be one, but a user can choose to
;;; connect to many Lisps simultaneously.
;;;
;;; A connection consists of a control socket, optionally an extra
;;; socket dedicated to receiving Lisp output (an optimization), and a
;;; set of connection-local state variables.
;;;
;;; The state variables are stored as buffer-local variables in the
;;; control socket's process-buffer and are used via accessor
;;; functions. These variables include things like the *FEATURES* list
;;; and Unix Pid of the Lisp process.
;;;
;;; One connection is "current" at any given time. This is:
;;;   `sly-dispatching-connection' if dynamically bound, or
;;;   `sly-buffer-connection' if this is set buffer-local, or
;;;   `sly-default-connection' otherwise.
;;;
;;; When you're invoking commands in your source files you'll be using
;;; `sly-default-connection'. This connection can be interactively
;;; reassigned via the connection-list buffer.
;;;
;;; When a command creates a new buffer it will set
;;; `sly-buffer-connection' so that commands in the new buffer will
;;; use the connection that the buffer originated from. For example,
;;; the apropos command creates the *Apropos* buffer and any command
;;; in that buffer (e.g. `M-.') will go to the same Lisp that did the
;;; apropos search. REPL buffers are similarly tied to their
;;; respective connections.
;;;
;;; When Emacs is dispatching some network message that arrived from a
;;; connection it will dynamically bind `sly-dispatching-connection'
;;; so that the event will be processed in the context of that
;;; connection.
;;;
;;; This is mostly transparent. The user should be aware that he can
;;; set the default connection to pick which Lisp handles commands in
;;; Lisp-mode source buffers, and sly hackers should be aware that
;;; they can tie a buffer to a specific connection. The rest takes
;;; care of itself.

(defvar sly-dispatching-connection nil
  "Network process currently executing.
This is dynamically bound while handling messages from Lisp; it
overrides `sly-buffer-connection' and `sly-default-connection'.")

(make-variable-buffer-local
 (defvar sly-buffer-connection nil
   "Network connection to use in the current buffer.
This overrides `sly-default-connection'."))

(defvar sly-default-connection nil
  "Network connection to use by default.
Used for all Lisp communication, except when overridden by
`sly-dispatching-connection' or `sly-buffer-connection'.")

(defun sly-current-connection ()
  "Return the connection to use for Lisp interaction.
Return nil if there's no connection."
  (or sly-dispatching-connection
      sly-buffer-connection
      sly-default-connection))

(defun sly-connection ()
  "Return the connection to use for Lisp interaction.
Signal an error if there's no connection."
  (let ((conn (sly-current-connection)))
    (cond ((and (not conn) sly-net-processes)
           (or (sly-auto-select-connection)
               (error "No default connection selected.")))
          ((not conn)
           (or (sly-auto-start)
               (error "Not connected.")))
          ((not (eq (process-status conn) 'open))
           (error "Connection closed."))
          (t conn))))

(define-obsolete-variable-alias 'sly-auto-connect
'sly-auto-start "2.5")
(defcustom sly-auto-start 'never
  "Controls auto connection when information from lisp process is needed.
This doesn't mean it will connect right after SLY is loaded."
  :group 'sly-mode
  :type '(choice (const never)
                 (const always)
                 (const ask)))

(defun sly-auto-start ()
  (cond ((or (eq sly-auto-start 'always)
             (and (eq sly-auto-start 'ask)
                  (y-or-n-p "No connection.  Start SLY? ")))
         (save-window-excursion
           (sly)
           (while (not (sly-current-connection))
             (sleep-for 1))
           (sly-connection)))
        (t nil)))

(defcustom sly-auto-select-connection 'ask
  "Controls auto selection after the default connection was closed."
  :group 'sly-mode
  :type '(choice (const never)
                 (const always)
                 (const ask)))

(defun sly-auto-select-connection ()
  (let* ((c0 (car sly-net-processes))
         (c (cond ((eq sly-auto-select-connection 'always) c0)
                  ((and (eq sly-auto-select-connection 'ask)
                        (y-or-n-p
                         (format "No default connection selected.  %s %s? "
                                 "Switch to" (sly-connection-name c0))))
                   c0))))
    (when c
      (sly-select-connection c)
      (message "Switching to connection: %s" (sly-connection-name c))
      c)))

(defun sly-select-connection (process)
  "Make PROCESS the default connection."
  (setq sly-default-connection process))

(defvar sly-cycle-connections-hook nil)

(defun sly-cycle-connections ()
  "Change current sly connection, cycling through all connections."
  (interactive)
  (let* ((tail (or (cdr (member (sly-current-connection)
                                sly-net-processes))
                   sly-net-processes))
         (p (car tail)))
    (sly-select-connection p)
    (run-hooks 'sly-cycle-connections-hook)
    (message "Lisp: %s %s" (sly-connection-name p) (process-contact p))))

(cl-defmacro sly-with-connection-buffer ((&optional process) &rest body)
  "Execute BODY in the process-buffer of PROCESS.
If PROCESS is not specified, `sly-connection' is used.

\(fn (&optional PROCESS) &body BODY))"
  (declare (indent 1))
  `(with-current-buffer
       (process-buffer (or ,process (sly-connection)
                           (error "No connection")))
     ,@body))

;;; Connection-local variables:

(defmacro sly-def-connection-var (varname &rest initial-value-and-doc)
  "Define a connection-local variable.
The value of the variable can be read by calling the function of the
same name (it must not be accessed directly). The accessor function is
setf-able.

The actual variable bindings are stored buffer-local in the
process-buffers of connections. The accessor function refers to
the binding for `sly-connection'."
  (declare (indent 2))
  (let ((real-var (intern (format "%s:connlocal" varname))))
    `(progn
       ;; Variable
       (make-variable-buffer-local
        (defvar ,real-var ,@initial-value-and-doc))
       ;; Accessor
       (defun ,varname (&optional process)
         (sly-with-connection-buffer (process) ,real-var))
       ;; Setf
       (defsetf ,varname (&optional process) (store)
         `(sly-with-connection-buffer (,process)
            (setq (\, (quote (\, real-var))) (\, store))))
       '(\, varname))))

(sly-def-connection-var sly-connection-number nil
  "Serial number of a connection.
Bound in the connection's process-buffer.")

(sly-def-connection-var sly-lisp-features '()
  "The symbol-names of Lisp's *FEATURES*.
This is automatically synchronized from Lisp.")

(sly-def-connection-var sly-lisp-modules '()
  "The strings of Lisp's *MODULES*.")

(sly-def-connection-var sly-pid nil
  "The process id of the Lisp process.")

(sly-def-connection-var sly-lisp-implementation-type nil
  "The implementation type of the Lisp process.")

(sly-def-connection-var sly-lisp-implementation-version nil
  "The implementation type of the Lisp process.")

(sly-def-connection-var sly-lisp-implementation-name nil
  "The short name for the Lisp implementation.")

(sly-def-connection-var sly-lisp-implementation-program nil
  "The argv[0] of the process running the Lisp implementation.")

(sly-def-connection-var sly-connection-name nil
  "The short name for connection.")

(sly-def-connection-var sly-inferior-process nil
  "The inferior process for the connection if any.")

(sly-def-connection-var sly-communication-style nil
  "The communication style.")

(sly-def-connection-var sly-machine-instance nil
  "The name of the (remote) machine running the Lisp process.")

(sly-def-connection-var sly-connection-coding-systems nil
  "Coding systems supported by the Lisp process.")

;;;;; Connection setup

(defvar sly-connection-counter 0
  "The number of SLY connections made. For generating serial numbers.")

;;; Interface
(defun sly-setup-connection (process)
  "Make a connection out of PROCESS."
  (let ((sly-dispatching-connection process))
    (sly-init-connection-state process)
    (sly-select-connection process)
    process))

(defun sly-init-connection-state (proc)
  "Initialize connection state in the process-buffer of PROC."
  ;; To make life simpler for the user: if this is the only open
  ;; connection then reset the connection counter.
  (when (equal sly-net-processes (list proc))
    (setq sly-connection-counter 0))
  (sly-with-connection-buffer ()
    (setq sly-buffer-connection proc))
  (setf (sly-connection-number proc) (cl-incf sly-connection-counter))
  ;; We do the rest of our initialization asynchronously. The current
  ;; function may be called from a timer, and if we setup the REPL
  ;; from a timer then it mysteriously uses the wrong keymap for the
  ;; first command.
  (let ((sly-current-thread t))
    (sly-eval-async '(swank:connection-info)
      (sly-curry #'sly-set-connection-info proc))))

(defun sly-set-connection-info (connection info)
  "Initialize CONNECTION with INFO received from Lisp."
  (let ((sly-dispatching-connection connection)
        (sly-current-thread t))
    (cl-destructuring-bind (&key pid style lisp-implementation machine
                                 features version modules encoding
                                 &allow-other-keys) info
      (sly-check-version version connection)
      (setf (sly-pid) pid
            (sly-communication-style) style
            (sly-lisp-features) features
            (sly-lisp-modules) modules)
      (cl-destructuring-bind (&key type name version program)
          lisp-implementation
        (setf (sly-lisp-implementation-type) type
              (sly-lisp-implementation-version) version
              (sly-lisp-implementation-name) name
              (sly-lisp-implementation-program) program
              (sly-connection-name) (sly-generate-connection-name name)))
      (cl-destructuring-bind (&key instance ((:type _)) ((:version _))) machine
        (setf (sly-machine-instance) instance))
      (cl-destructuring-bind (&key coding-systems) encoding
        (setf (sly-connection-coding-systems) coding-systems)))
    (let ((args (when-let (p (sly-inferior-process))
                  (sly-inferior-lisp-args p))))
      (when-let (name (plist-get args ':name))
        (unless (string= (sly-lisp-implementation-name) name)
          (setf (sly-connection-name)
                (sly-generate-connection-name (symbol-name name)))))
      (sly-load-contribs)
      (run-hooks 'sly-connected-hook)
      (when-let (fun (plist-get args ':init-function))
        (funcall fun)))
    (message "Connected. %s" (sly-random-words-of-encouragement))))

(defun sly-check-version (version conn)
  (or (equal version sly-protocol-version)
      (equal sly-protocol-version 'ignore)
      (y-or-n-p
       (format "Versions differ: %s (sly) vs. %s (swank). Continue? "
               sly-protocol-version version))
      (sly-net-close conn)
      (top-level)))

(defun sly-generate-connection-name (lisp-name)
  (when (file-exists-p lisp-name)
      (setq lisp-name (file-name-nondirectory lisp-name)))
  (cl-loop for i from 1
           for name = lisp-name then (format "%s<%d>" lisp-name i)
           while (cl-find name sly-net-processes
                          :key #'sly-connection-name :test #'equal)
           finally (cl-return name)))

(defun sly-connection-close-hook (process)
  (when (eq process sly-default-connection)
    (when sly-net-processes
      (sly-select-connection (car sly-net-processes))
      (message "Default connection closed; switched to #%S (%S)"
               (sly-connection-number)
               (sly-connection-name)))))

(add-hook 'sly-net-process-close-hooks 'sly-connection-close-hook)

;;;;; Commands on connections

(defun sly-disconnect ()
  "Close the current connection."
  (interactive)
  (sly-net-close (sly-connection)))

(defun sly-disconnect-all ()
  "Disconnect all connections."
  (interactive)
  (mapc #'sly-net-close sly-net-processes))

(defun sly-connection-port (connection)
  "Return the remote port number of CONNECTION."
  (cadr (process-contact connection)))

(defun sly-process (&optional connection)
  "Return the Lisp process for CONNECTION (default `sly-connection').
Return nil if there's no process object for the connection."
  (let ((proc (sly-inferior-process connection)))
    (if (and proc
             (memq (process-status proc) '(run stop)))
        proc)))

;; Non-macro version to keep the file byte-compilable.
(defun sly-set-inferior-process (connection process)
  (setf (sly-inferior-process connection) process))

(defun sly-use-sigint-for-interrupt (&optional connection)
  (let ((c (or connection (sly-connection))))
    (cl-ecase (sly-communication-style c)
      ((:fd-handler nil) t)
      ((:spawn :sigio) nil))))

(defvar sly-inhibit-pipelining t
  "*If true, don't send background requests if Lisp is already busy.")

(defun sly-background-activities-enabled-p ()
  (and (let ((con (sly-current-connection)))
         (and con
              (eq (process-status con) 'open)))
       (or (not (sly-busy-p))
           (not sly-inhibit-pipelining))))


;;;; Communication protocol

;;;;; Emacs Lisp programming interface
;;;
;;; The programming interface for writing Emacs commands is based on
;;; remote procedure calls (RPCs). The basic operation is to ask Lisp
;;; to apply a named Lisp function to some arguments, then to do
;;; something with the result.
;;;
;;; Requests can be either synchronous (blocking) or asynchronous
;;; (with the result passed to a callback/continuation function).  If
;;; an error occurs during the request then the debugger is entered
;;; before the result arrives -- for synchronous evaluations this
;;; requires a recursive edit.
;;;
;;; You should use asynchronous evaluations (`sly-eval-async') for
;;; most things. Reserve synchronous evaluations (`sly-eval') for
;;; the cases where blocking Emacs is really appropriate (like
;;; completion) and that shouldn't trigger errors (e.g. not evaluate
;;; user-entered code).
;;;
;;; We have the concept of the "current Lisp package". RPC requests
;;; always say what package the user is making them from and the Lisp
;;; side binds that package to *BUFFER-PACKAGE* to use as it sees
;;; fit. The current package is defined as the buffer-local value of
;;; `sly-buffer-package' if set, and otherwise the package named by
;;; the nearest IN-PACKAGE as found by text search (cl-first backwards,
;;; then forwards).
;;;
;;; Similarly we have the concept of the current thread, i.e. which
;;; thread in the Lisp process should handle the request. The current
;;; thread is determined solely by the buffer-local value of
;;; `sly-current-thread'. This is usually bound to t meaning "no
;;; particular thread", but can also be used to nominate a specific
;;; thread. The REPL and the debugger both use this feature to deal
;;; with specific threads.

(make-variable-buffer-local
 (defvar sly-current-thread t
   "The id of the current thread on the Lisp side.
t means the \"current\" thread;
:repl-thread the thread that executes REPL requests;
fixnum a specific thread."))

(make-variable-buffer-local
 (defvar sly-buffer-package nil
   "The Lisp package associated with the current buffer.
This is set only in buffers bound to specific packages."))

;;; `sly-rex' is the RPC primitive which is used to implement both
;;; `sly-eval' and `sly-eval-async'. You can use it directly if
;;; you need to, but the others are usually more convenient.

(cl-defmacro sly-rex ((&rest saved-vars)
                        (sexp &optional
                              (package '(sly-current-package))
                              (thread 'sly-current-thread))
                        &rest continuations)
  "(sly-rex (VAR ...) (SEXP &optional PACKAGE THREAD) CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each
VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the princed version is sent to Lisp.

PACKAGE is evaluated and Lisp binds *BUFFER-PACKAGE* to this package.
The default value is (sly-current-package).

CLAUSES is a list of patterns with same syntax as
`destructure-case'.  The result of the evaluation of SEXP is
dispatched on CLAUSES.  The result is either a sexp of the
form (:ok VALUE) or (:abort CONDITION).  CLAUSES is executed
asynchronously.

Note: don't use backquote syntax for SEXP, because various Emacs
versions cannot deal with that."
  (declare (indent 2)
           (debug (sexp (form &optional sexp sexp)
                        &rest (sexp &rest form))))
  (let ((result (cl-gensym)))
    `(lexical-let ,(cl-loop for var in saved-vars
                            collect (cl-etypecase var
                                      (symbol (list var var))
                                      (cons var)))
       (sly-dispatch-event
        (list :emacs-rex ,sexp ,package ,thread
              (lambda (,result)
                (destructure-case ,result
                  ,@continuations)))))))

;;; Interface
(defun sly-current-package ()
  "Return the Common Lisp package in the current context.
If `sly-buffer-package' has a value then return that, otherwise
search for and read an `in-package' form."
  (or sly-buffer-package
      (save-restriction
        (widen)
        (sly-find-buffer-package))))

(defvar sly-find-buffer-package-function 'sly-search-buffer-package
  "*Function to use for `sly-find-buffer-package'.
The result should be the package-name (a string)
or nil if nothing suitable can be found.")

(defun sly-find-buffer-package ()
  "Figure out which Lisp package the current buffer is associated with."
  (funcall sly-find-buffer-package-function))

(make-variable-buffer-local
 (defvar sly-package-cache nil
   "Cons of the form (buffer-modified-tick . package)"))

;; When modifing this code consider cases like:
;;  (in-package #.*foo*)
;;  (in-package #:cl)
;;  (in-package :cl)
;;  (in-package "CL")
;;  (in-package |CL|)
;;  (in-package #+ansi-cl :cl #-ansi-cl 'lisp)

(defun sly-search-buffer-package ()
  (let ((case-fold-search t)
        (regexp (concat "^(\\(cl:\\|common-lisp:\\)?in-package\\>[ \t']*"
                        "\\([^)]+\\)[ \t]*)")))
    (save-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (match-string-no-properties 2)))))

;;; Synchronous requests are implemented in terms of asynchronous
;;; ones. We make an asynchronous request with a continuation function
;;; that `throw's its result up to a `catch' and then enter a loop of
;;; handling I/O until that happens.

(defvar sly-stack-eval-tags nil
  "List of stack-tags of continuations waiting on the stack.")

(defun sly-eval (sexp &optional package)
  "Evaluate EXPR on the superior Lisp and return the result."
  (when (null package) (setq package (sly-current-package)))
  (let* ((tag (cl-gensym (format "sly-result-%d-"
                                 (1+ (sly-continuation-counter)))))
	 (sly-stack-eval-tags (cons tag sly-stack-eval-tags)))
    (apply
     #'funcall
     (catch tag
       (sly-rex (tag sexp)
           (sexp package)
         ((:ok value)
          (unless (member tag sly-stack-eval-tags)
            (error "Reply to canceled synchronous eval request tag=%S sexp=%S"
                   tag sexp))
          (throw tag (list #'identity value)))
         ((:abort _condition)
          (throw tag (list #'error "Synchronous Lisp Evaluation aborted"))))
       (let ((debug-on-quit t)
             (inhibit-quit nil)
             (conn (sly-connection)))
         (while t
           (unless (eq (process-status conn) 'open)
             (error "Lisp connection closed unexpectedly"))
           (accept-process-output nil 0.01)))))))

(defun sly-eval-async (sexp &optional cont package)
  "Evaluate EXPR on the superior Lisp and call CONT with the result."
  (declare (indent 1))
  (sly-rex (cont (buffer (current-buffer)))
      (sexp (or package (sly-current-package)))
    ((:ok result)
     (when cont
       (set-buffer buffer)
       (funcall cont result)))
    ((:abort condition)
     (message "Evaluation aborted on %s." condition)))
  ;; Guard against arbitrary return values which once upon a time
  ;; showed up in the minibuffer spuriously (due to a bug in
  ;; sly-autodoc.)  If this ever happens again, returning the
  ;; following will make debugging much easier:
  :sly-eval-async)

;;; These functions can be handy too:

(defun sly-connected-p ()
  "Return true if the Swank connection is open."
  (not (null sly-net-processes)))

(defun sly-check-connected ()
  "Signal an error if we are not connected to Lisp."
  (unless (sly-connected-p)
    (error "Not connected. Use `%s' to start a Lisp."
           (substitute-command-keys "\\[sly]"))))

;; UNUSED
(defun sly-debugged-connection-p (conn)
  ;; This previously was (AND (SLDB-DEBUGGED-CONTINUATIONS CONN) T),
  ;; but an SLDB buffer may exist without having continuations
  ;; attached to it, e.g. the one resulting from `sly-interrupt'.
  (cl-loop for b in (sldb-buffers)
           thereis (with-current-buffer b
                     (eq sly-buffer-connection conn))))

(defun sly-busy-p (&optional conn)
  "True if Lisp has outstanding requests.
Debugged requests are ignored."
  (let ((debugged (sldb-debugged-continuations (or conn (sly-connection)))))
    (cl-remove-if (lambda (id)
                    (memq id debugged))
                  (sly-rex-continuations)
                  :key #'car)))

(defun sly-sync ()
  "Block until the most recent request has finished."
  (when (sly-rex-continuations)
    (let ((tag (caar (sly-rex-continuations))))
      (while (cl-find tag (sly-rex-continuations) :key #'car)
        (accept-process-output nil 0.1)))))

(defun sly-ping ()
  "Check that communication works."
  (interactive)
  (message "%s" (sly-eval "PONG")))

;;;;; Protocol event handler (cl-the guts)
;;;
;;; This is the protocol in all its glory. The input to this function
;;; is a protocol event that either originates within Emacs or arrived
;;; over the network from Lisp.
;;;
;;; Each event is a list beginning with a keyword and followed by
;;; arguments. The keyword identifies the type of event. Events
;;; originating from Emacs have names starting with :emacs- and events
;;; from Lisp don't.

(sly-def-connection-var sly-rex-continuations '()
  "List of (ID . FUNCTION) continuations waiting for RPC results.")

(sly-def-connection-var sly-continuation-counter 0
  "Continuation serial number counter.")

(defvar sly-event-hooks)

(defun sly-dispatch-event (event &optional process)
  (let ((sly-dispatching-connection (or process (sly-connection))))
    (or (run-hook-with-args-until-success 'sly-event-hooks event)
        (destructure-case event
          ((:emacs-rex form package thread continuation)
           (when (and (sly-use-sigint-for-interrupt) (sly-busy-p))
             (sly-display-oneliner "; pipelined request... %S" form))
           (let ((id (cl-incf (sly-continuation-counter))))
             (sly-send `(:emacs-rex ,form ,package ,thread ,id))
             (push (cons id continuation) (sly-rex-continuations))))
          ((:return value id)
           (let ((rec (assq id (sly-rex-continuations))))
             (cond (rec (setf (sly-rex-continuations)
                              (remove rec (sly-rex-continuations)))
                        (funcall (cdr rec) value))
                   (t
                    (error "Unexpected reply: %S %S" id value)))))
          ((:debug-activate thread level &optional select)
           (cl-assert thread)
           (sldb-activate thread level select))
          ((:debug thread level condition restarts frames conts)
           (cl-assert thread)
           (sldb-setup thread level condition restarts frames conts))
          ((:debug-return thread level stepping)
           (cl-assert thread)
           (sldb-exit thread level stepping))
          ((:emacs-interrupt thread)
           (sly-send `(:emacs-interrupt ,thread)))
          ((:read-from-minibuffer thread tag prompt initial-value)
           (sly-read-from-minibuffer-for-swank thread tag prompt
                                               initial-value))
          ((:y-or-n-p thread tag question)
           (sly-y-or-n-p thread tag question))
          ((:emacs-return-string thread tag string)
           (sly-send `(:emacs-return-string ,thread ,tag ,string)))
          ((:new-features features)
           (setf (sly-lisp-features) features))
          ((:indentation-update info)
           (sly-handle-indentation-update info))
          ((:eval-no-wait form)
           (sly-check-eval-in-emacs-enabled)
           (eval (read form)))
          ((:eval thread tag form-string)
           (sly-check-eval-in-emacs-enabled)
           (sly-eval-for-lisp thread tag form-string))
          ((:emacs-return thread tag value)
           (sly-send `(:emacs-return ,thread ,tag ,value)))
          ((:ed what)
           (sly-ed what))
          ((:inspect what thread tag)
           (let ((hook (when (and thread tag)
                         (sly-curry #'sly-send
                                    `(:emacs-return ,thread ,tag nil)))))
             (sly-open-inspector what nil hook)))
          ((:background-message message)
           (sly-background-message "%s" message))
          ((:debug-condition thread message)
           (cl-assert thread)
           (message "%s" message))
          ((:ping thread tag)
           (sly-send `(:emacs-pong ,thread ,tag)))
          ((:reader-error packet condition)
           (sly-with-popup-buffer ((sly-buffer-name :error))
             (princ (format "Invalid protocol message:\n%s\n\n%s"
                            condition packet))
             (goto-char (point-min)))
           (error "Invalid protocol message"))
          ((:invalid-rpc id message)
           (setf (sly-rex-continuations)
                 (cl-remove id (sly-rex-continuations) :key #'car))
           (error "Invalid rpc: %s" message))
          ((:emacs-skipped-packet _pkg))
          ((:test-delay seconds) ; for testing only
           (sit-for seconds))
          ((:channel-send id msg)
           (sly-channel-send (or (sly-find-channel id)
                                 (error "Invalid channel id: %S %S" id msg))
                             msg))
          ((:emacs-channel-send id msg)
           (sly-send `(:emacs-channel-send ,id ,msg)))
          ((:invalid-channel channel-id reason)
           (error "Invalid remote channel %d: %s" channel-id reason))))))

(defun sly-send (sexp)
  "Send SEXP directly over the wire on the current connection."
  (sly-net-send sexp (sly-connection)))

(defun sly-reset ()
  "Clear all pending continuations and erase connection buffer."
  (interactive)
  (setf (sly-rex-continuations) '())
  (mapc #'kill-buffer (sldb-buffers))
  (sly-with-connection-buffer ()
    (erase-buffer)))

(defun sly-send-sigint ()
  (interactive)
  (signal-process (sly-pid) 'SIGINT))

;;;;; Channels

;;; A channel implements a set of operations.  Those operations can be
;;; invoked by sending messages to the channel.  Channels are used for
;;; protocols which can't be expressed naturally with RPCs, e.g. for
;;; streaming data over the wire.
;;;
;;; A channel can be "remote" or "local".  Remote channels are
;;; represented by integers.  Local channels are structures.  Messages
;;; sent to a closed (remote) channel are ignored.

(sly-def-connection-var sly-channels '()
  "Alist of the form (ID . CHANNEL).")

(sly-def-connection-var sly-channels-counter 0
  "Channel serial number counter.")

(cl-defstruct (sly-channel (:conc-name sly-channel.)
                             (:constructor
                              sly-make-channel% (operations name id plist)))
  operations name id plist)

(defun sly-make-channel (operations &optional name)
  (let* ((id (cl-incf (sly-channels-counter)))
         (ch (sly-make-channel% operations name id nil)))
    (push (cons id ch) (sly-channels))
    ch))

(defun sly-close-channel (channel)
  (setf (sly-channel.operations channel) 'closed-channel)
  (let ((probe (assq (sly-channel.id channel)
                     (and (sly-current-connection)
                          (sly-channels)))))
    (cond (probe (setf (sly-channels) (delete probe (sly-channels))))
          (t (error "Can't close invalid channel: %s" channel)))))

(defun sly-find-channel (id)
  (cdr (assq id (sly-channels))))

(defun sly-channel-send (channel message)
  (apply (or (gethash (car message) (sly-channel.operations channel))
             (error "Unsupported operation: %S %S" message channel))
         channel (cdr message)))

(defun sly-channel-put (channel prop value)
  (setf (sly-channel.plist channel)
        (plist-put (sly-channel.plist channel) prop value)))

(defun sly-channel-get (channel prop)
  (plist-get (sly-channel.plist channel) prop))

(eval-and-compile
  (defun sly-channel-method-table-name (type)
    (intern (format "sly-%s-channel-methods" type))))

(defmacro sly-define-channel-type (name)
  (declare (indent defun))
  (let ((tab (sly-channel-method-table-name name)))
    `(defvar ,tab (make-hash-table :size 10))))

(defmacro sly-define-channel-method (type method args &rest body)
  (declare (indent 3) (debug (&define name sexp lambda-list
                                      def-body)))
  `(puthash ',method
            (lambda (self . ,args) ,@body)
            ,(sly-channel-method-table-name type)))

(defun sly-send-to-remote-channel (channel-id msg)
  (sly-dispatch-event `(:emacs-channel-send ,channel-id ,msg)))

;;;;; Event logging to *sly-events*
;;;
;;; The *sly-events* buffer logs all protocol messages for debugging
;;; purposes. Optionally you can enable outline-mode in that buffer,
;;; which is convenient but slows things down significantly.

(defvar sly-log-events t
  "*Log protocol events to the *sly-events* buffer.")

(defvar sly-outline-mode-in-events-buffer nil
  "*Non-nil means use outline-mode in *sly-events*.")

(defvar sly-event-buffer-name (sly-buffer-name :events)
  "The name of the sly event buffer.")

(defun sly-log-event (event)
  "Record the fact that EVENT occurred."
  (when sly-log-events
    (with-current-buffer (sly-events-buffer)
      ;; trim?
      (when (> (buffer-size) 100000)
        (goto-char (/ (buffer-size) 2))
        (re-search-forward "^(" nil t)
        (delete-region (point-min) (point)))
      (goto-char (point-max))
      (save-excursion
        (sly-pprint-event event (current-buffer)))
      (when (and (boundp 'outline-minor-mode)
                 outline-minor-mode)
        (hide-entry))
      (goto-char (point-max)))))

(defun sly-pprint-event (event buffer)
  "Pretty print EVENT in BUFFER with limited depth and width."
  (let ((print-length 20)
	(print-level 6)
	(pp-escape-newlines t))
    (pp event buffer)))

(defun sly-events-buffer ()
  "Return or create the event log buffer."
  (or (get-buffer sly-event-buffer-name)
      (let ((buffer (get-buffer-create sly-event-buffer-name)))
        (with-current-buffer buffer
          (buffer-disable-undo)
          (set (make-local-variable 'outline-regexp) "^(")
          (set (make-local-variable 'comment-start) ";")
          (set (make-local-variable 'comment-end) "")
          (when sly-outline-mode-in-events-buffer
            (outline-minor-mode)))
        buffer)))


;;;;; Cleanup after a quit

(defun sly-restart-inferior-lisp ()
  "Kill and restart the Lisp subprocess."
  (interactive)
  (cl-assert (sly-inferior-process) () "No inferior lisp process")
  (sly-quit-lisp-internal (sly-connection) 'sly-restart-sentinel t))

(defun sly-restart-sentinel (process _message)
  "Restart the inferior lisp process.
Also rearrange windows."
  (cl-assert (process-status process) 'closed)
  (let* ((proc (sly-inferior-process process))
         (args (sly-inferior-lisp-args proc))
         (buffer (buffer-name (process-buffer proc)))
         ;;(buffer-window (get-buffer-window buffer))
         (new-proc (sly-start-lisp (plist-get args :program)
                                     (plist-get args :program-args)
                                     (plist-get args :env)
                                     nil
                                     buffer)))
    (sly-net-close process)
    (sly-inferior-connect new-proc args)
    (switch-to-buffer buffer)
    (goto-char (point-max))))


;;;; Compilation and the creation of compiler-note annotations

(defvar sly-highlight-compiler-notes t
  "*When non-nil annotate buffers with compilation notes etc.")

(defvar sly-before-compile-functions nil
  "A list of function called before compiling a buffer or region.
The function receive two arguments: the beginning and the end of the
region that will be compiled.")

;; FIXME: remove some of the options
(defcustom sly-compilation-finished-hook 'sly-maybe-show-compilation-log
  "Hook called with a list of compiler notes after a compilation."
  :group 'sly-mode
  :type 'hook
  :options '(sly-maybe-show-compilation-log
             sly-create-compilation-log
             sly-show-compilation-log
             sly-maybe-list-compiler-notes
             sly-list-compiler-notes
             sly-maybe-show-xrefs-for-notes
             sly-goto-first-note))

;; FIXME: I doubt that anybody uses this directly and it seems to be
;; only an ugly way to pass arguments.
(defvar sly-compilation-policy nil
  "When non-nil compile with these optimization settings.")

(defun sly-compute-policy (arg)
  "Return the policy for the prefix argument ARG."
  (let ((between (lambda (min n max)
                   (cond ((< n min) min)
                         ((> n max) max)
                         (t n)))))
    (let ((n (prefix-numeric-value arg)))
      (cond ((not arg)   sly-compilation-policy)
            ((cl-plusp n)   `((cl:debug . ,(funcall between 0 n 3))))
            ((eq arg '-) `((cl:speed . 3)))
            (t           `((cl:speed . ,(funcall between 0 (abs n) 3))))))))

(cl-defstruct (sly-compilation-result
               (:type list)
               (:conc-name sly-compilation-result.)
               (:constructor nil)
               (:copier nil))
  tag notes successp duration loadp faslfile)

(defvar sly-last-compilation-result nil
  "The result of the most recently issued compilation.")

(defun sly-compiler-notes ()
  "Return all compiler notes, warnings, and errors."
  (sly-compilation-result.notes sly-last-compilation-result))

(defun sly-compile-and-load-file (&optional policy)
  "Compile and load the buffer's file and highlight compiler notes.

With (positive) prefix argument the file is compiled with maximal
debug settings (`C-u'). With negative prefix argument it is compiled for
speed (`M--'). If a numeric argument is passed set debug or speed settings
to it depending on its sign.

Each source location that is the subject of a compiler note is
underlined and annotated with the relevant information. The commands
`sly-next-note' and `sly-previous-note' can be used to navigate
between compiler notes and to display their full details."
  (interactive "P")
  (sly-compile-file t (sly-compute-policy policy)))

(defcustom sly-compile-file-options '()
  "Plist of additional options that C-c C-k should pass to Lisp.
Currently only :fasl-directory is supported."
  :group 'sly-lisp
  :type '(plist :key-type symbol :value-type (file :must-match t)))

(defun sly-compile-file (&optional load policy)
  "Compile current buffer's file and highlight resulting compiler notes.

See `sly-compile-and-load-file' for further details."
  (interactive)
  (unless buffer-file-name
    (error "Buffer %s is not associated with a file." (buffer-name)))
  (check-parens)
  (when (and (buffer-modified-p)
             (y-or-n-p (format "Save file %s? " (buffer-file-name))))
    (save-buffer))
  (run-hook-with-args 'sly-before-compile-functions (point-min) (point-max))
  (let ((file (sly-to-lisp-filename (buffer-file-name)))
        (options (sly-simplify-plist `(,@sly-compile-file-options
                                         :policy ,policy))))
    (sly-eval-async
        `(swank:compile-file-for-emacs ,file ,(if load t nil)
                                       . ,(sly-hack-quotes options))
      #'sly-compilation-finished)
    (message "Compiling %s..." file)))

(defun sly-hack-quotes (arglist)
  ;; eval is the wrong primitive, we really want funcall
  (cl-loop for arg in arglist collect `(quote ,arg)))

(defun sly-simplify-plist (plist)
  (cl-loop for (key val) on plist by #'cddr
           append (cond ((null val) '())
                        (t (list key val)))))

(defun sly-compile-defun (&optional raw-prefix-arg)
  "Compile the current toplevel form.

With (positive) prefix argument the form is compiled with maximal
debug settings (`C-u'). With negative prefix argument it is compiled for
speed (`M--'). If a numeric argument is passed set debug or speed settings
to it depending on its sign."
  (interactive "P")
  (let ((sly-compilation-policy (sly-compute-policy raw-prefix-arg)))
    (if (use-region-p)
        (sly-compile-region (region-beginning) (region-end))
      (apply #'sly-compile-region (sly-region-for-defun-at-point)))))

(defun sly-compile-region (start end)
  "Compile the region."
  (interactive "r")
  ;; Check connection before running hooks things like
  ;; sly-flash-region don't make much sense if there's no connection
  (sly-connection)
  (sly-flash-region start end)
  (run-hook-with-args 'sly-before-compile-functions start end)
  (sly-compile-string (buffer-substring-no-properties start end) start))

(defun sly-flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun sly-compile-string (string start-offset)
  (let* ((line (save-excursion
                 (goto-char start-offset)
                 (list (line-number-at-pos) (1+ (current-column)))))
         (position `((:position ,start-offset) (:line ,@line))))
    (sly-eval-async
        `(swank:compile-string-for-emacs
          ,string
          ,(buffer-name)
          ',position
          ,(if (buffer-file-name) (sly-to-lisp-filename (buffer-file-name)))
          ',sly-compilation-policy)
      #'(lambda (result)
          (sly-compilation-finished result t)))))

(defcustom sly-load-failed-fasl 'ask
  "Which action to take when COMPILE-FILE set FAILURE-P to T.
NEVER doesn't load the fasl
ALWAYS loads the fasl
ASK asks the user."
  :type '(choice (const never)
                 (const always)
                 (const ask)))

(defun sly-load-failed-fasl-p ()
  (cl-ecase sly-load-failed-fasl
    (never nil)
    (always t)
    (ask (y-or-n-p "Compilation failed.  Load fasl file anyway? "))))

(defun sly-compilation-finished (result &optional stringp)
  (with-struct (sly-compilation-result. notes duration successp
                                          loadp faslfile) result
    (setf sly-last-compilation-result result)
    (sly-show-note-counts notes duration (cond ((not loadp) successp)
                                               (t (and faslfile successp)))
                          (or stringp loadp))
    (when sly-highlight-compiler-notes
      (sly-highlight-notes notes))
    (run-hook-with-args 'sly-compilation-finished-hook notes)
    (when (and loadp faslfile
               (or successp
                   (sly-load-failed-fasl-p)))
      (sly-eval-async `(swank:load-file ,faslfile)))))

(defun sly-show-note-counts (notes secs successp loadp)
  (message (concat
            (cond ((and successp loadp)
                   "Compiled and loaded")
                  (successp "Compilation finished")
                  (t (sly-add-face 'font-lock-warning-face
                       "Compilation failed")))
            (if (null notes) ". (No warnings)" ": ")
            (mapconcat
             (lambda (messages)
               (cl-destructuring-bind (sev . notes) messages
                 (let ((len (length notes)))
                   (format "%d %s%s" len (sly-severity-label sev)
                           (if (= len 1) "" "s")))))
             (sort (sly-alistify notes #'sly-note.severity #'eq)
                   (lambda (x y) (sly-severity< (car y) (car x))))
             "  ")
            (if secs (format "  [%.2f secs]" secs)))))

(defun sly-highlight-notes (notes)
  "Highlight compiler notes, warnings, and errors in the buffer."
  (interactive (list (sly-compiler-notes)))
  (with-temp-message "Highlighting notes..."
    (save-excursion
      (save-restriction
        (widen)                  ; highlight notes on the whole buffer
        (sly-remove-old-overlays)
        (mapc #'sly-overlay-note (sly-merge-notes-for-display notes))))))

(defvar sly-note-overlays '()
  "List of overlays created by `sly-make-note-overlay'")

(defun sly-remove-old-overlays ()
  "Delete the existing note overlays."
  (mapc #'delete-overlay sly-note-overlays)
  (setq sly-note-overlays '()))

(defun sly-filter-buffers (predicate)
  "Return a list of where PREDICATE returns true.
PREDICATE is executed in the buffer to test."
  (cl-remove-if-not (lambda (%buffer)
                      (with-current-buffer %buffer
                        (funcall predicate)))
                    (buffer-list)))

;;;;; Recompilation.

;; FIXME: This whole idea is questionable since it depends so
;; crucially on precise source-locs.

(defun sly-recompile-location (location)
  (save-excursion
    (sly-goto-source-location location)
    (sly-compile-defun)))

(defun sly-recompile-locations (locations cont)
  (sly-eval-async
      `(swank:compile-multiple-strings-for-emacs
        ',(cl-loop for loc in locations collect
                   (save-excursion
                     (sly-goto-source-location loc)
                     (cl-destructuring-bind (start end)
                         (sly-region-for-defun-at-point)
                       (list (buffer-substring-no-properties start end)
                             (buffer-name)
                             (sly-current-package)
                             start
                             (if (buffer-file-name)
                                 (file-name-directory (buffer-file-name))
                               nil)))))
        ',sly-compilation-policy)
    cont))


;;;;; Merging together compiler notes in the same location.

(defun sly-merge-notes-for-display (notes)
  "Merge together notes that refer to the same location.
This operation is \"lossy\" in the broad sense but not for display purposes."
  (mapcar #'sly-merge-notes
          (sly-group-similar 'sly-notes-in-same-location-p notes)))

(defun sly-merge-notes (notes)
  "Merge NOTES together. Keep the highest severity, concatenate the messages."
  (let* ((new-severity (cl-reduce #'sly-most-severe notes
                                  :key #'sly-note.severity))
         (new-message (mapconcat #'sly-note.message notes "\n")))
    (let ((new-note (cl-copy-list (car notes))))
      (setf (cl-getf new-note :message) new-message)
      (setf (cl-getf new-note :severity) new-severity)
      new-note)))

(defun sly-notes-in-same-location-p (a b)
  (equal (sly-note.location a) (sly-note.location b)))


;;;;; Compiler notes list

(defun sly-one-line-ify (string)
  "Return a single-line version of STRING.
Each newlines and following indentation is replaced by a single space."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "\n[\n \t]*" nil t)
      (replace-match " "))
    (buffer-string)))

(defun sly-xrefs-for-notes (notes)
  (let ((xrefs))
    (dolist (note notes)
      (let* ((location (cl-getf note :location))
             (fn (cadr (assq :file (cdr location))))
             (file (assoc fn xrefs))
             (node
              (list (format "%s: %s"
                            (cl-getf note :severity)
                            (sly-one-line-ify (cl-getf note :message)))
                    location)))
        (when fn
          (if file
              (push node (cdr file))
            (setf xrefs (cl-acons fn (list node) xrefs))))))
    xrefs))

(defun sly-maybe-show-xrefs-for-notes (notes)
  "Show the compiler notes NOTES if they come from more than one file."
  (let ((xrefs (sly-xrefs-for-notes notes)))
    (when (sly-length> xrefs 1)          ; >1 file
      (sly-show-xrefs
       xrefs 'definition "Compiler notes" (sly-current-package)))))

(defun sly-note-has-location-p (note)
  (not (eq ':error (car (sly-note.location note)))))

(defun sly-redefinition-note-p (note)
  (eq (sly-note.severity note) :redefinition))

(defun sly-create-compilation-log (notes)
  "Create a buffer for `next-error' to use."
  (with-current-buffer (get-buffer-create (sly-buffer-name :compilation))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (sly-insert-compilation-log notes)
    (compilation-mode)))

(defun sly-maybe-show-compilation-log (notes)
  "Display the log on failed compilations or if NOTES is non-nil."
  (sly-create-compilation-log notes)
  (with-struct (sly-compilation-result. notes duration successp)
      sly-last-compilation-result
    (unless successp
      (with-current-buffer (sly-buffer-name :compilation)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "Compilation " (if successp "succeeded." "failed."))
          (goto-char (point-min))
          (display-buffer (current-buffer)))))))

(defun sly-show-compilation-log (notes)
  "Create and display the compilation log buffer."
  (interactive (list (sly-compiler-notes)))
  (sly-with-popup-buffer ((sly-buffer-name :compilation)
                            :mode 'compilation-mode)
    (sly-insert-compilation-log notes)))

(defun sly-insert-compilation-log (notes)
  "Insert NOTES in format suitable for `compilation-mode'."
  (cl-multiple-value-bind (grouped-notes canonicalized-locs-table)
      (sly-group-and-sort-notes notes)
    (with-temp-message "Preparing compilation log..."
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t)) ; inefficient font-lock-hook
        (insert (format "cd %s\n%d compiler notes:\n\n"
                        default-directory (length notes)))
        (dolist (notes grouped-notes)
          (let ((loc (gethash (cl-first notes) canonicalized-locs-table))
                (start (point)))
            (insert (sly-canonicalized-location-to-string loc) ":")
            (sly-insert-note-group notes)
            (insert "\n")
            (sly-make-note-overlay (cl-first notes) start (1- (point))))))
      (set (make-local-variable 'compilation-skip-threshold) 0)
      (setq next-error-last-buffer (current-buffer)))))

(defun sly-insert-note-group (notes)
  "Insert a group of compiler messages."
  (insert "\n")
  (dolist (note notes)
    (insert "  " (sly-severity-label (sly-note.severity note)) ": ")
    (let ((start (point)))
      (insert (sly-note.message note))
      (let ((ctx (sly-note.source-context note)))
        (if ctx (insert "\n" ctx)))
      (sly-indent-block start 4))
    (insert "\n")))

(defun sly-indent-block (start column)
  "If the region back to START isn't a one-liner indent it."
  (when (< start (line-beginning-position))
    (save-excursion
      (goto-char start)
      (insert "\n"))
    (sly-indent-rigidly start (point) column)))

(defun sly-canonicalized-location (location)
  "Return a list (FILE LINE COLUMN) for sly-location LOCATION.
This is quite an expensive operation so use carefully."
  (save-excursion
    (sly-goto-location-buffer (sly-location.buffer location))
    (save-excursion
      (sly-goto-source-location location)
      (list (or (buffer-file-name) (buffer-name))
            (save-restriction
              (widen)
              (line-number-at-pos))
            (1+ (current-column))))))

(defun sly-canonicalized-location-to-string (loc)
  (if loc
      (cl-destructuring-bind (filename line col) loc
        (format "%s:%d:%d"
                (cond ((not filename) "")
                      ((let ((rel (file-relative-name filename)))
                         (if (< (length rel) (length filename))
                             rel)))
                      (t filename))
                line col))
    (format "Unknown location")))

(defun sly-goto-note-in-compilation-log (note)
  "Find `note' in the compilation log and display it."
  (with-current-buffer (get-buffer (sly-buffer-name :compilation))
    (let ((pos
           (save-excursion
             (goto-char (point-min))
             (cl-loop for overlay = (sly-find-next-note)
                      while overlay
                      for other-note = (overlay-get overlay 'sly-note)
                      when (sly-notes-in-same-location-p note other-note)
                      return (overlay-start overlay)))))
      (when pos
        (with-selected-window (display-buffer (current-buffer) t)
          (goto-char pos)
          (recenter 0))))))

(defun sly-group-and-sort-notes (notes)
  "First sort, then group NOTES according to their canonicalized locs."
  (let ((locs (make-hash-table :test #'eq)))
    (mapc (lambda (note)
            (let ((loc (sly-note.location note)))
              (when (sly-location-p loc)
                (puthash note (sly-canonicalized-location loc) locs))))
          notes)
    (cl-values (sly-group-similar
                (lambda (n1 n2)
                  (equal (gethash n1 locs nil) (gethash n2 locs t)))
                (let* ((bottom most-negative-fixnum)
                       (+default+ (list "" bottom bottom)))
                  (sort notes
                        (lambda (n1 n2)
                          (cl-destructuring-bind (filename1 line1 col1)
                              (gethash n1 locs +default+)
                            (cl-destructuring-bind (filename2 line2 col2)
                                (gethash n2 locs +default+)
                              (cond ((string-lessp filename1 filename2) t)
                                    ((string-lessp filename2 filename1) nil)
                                    ((< line1 line2) t)
                                    ((> line1 line2) nil)
                                    (t (< col1 col2)))))))))
               locs)))

(defun sly-note.severity (note)
  (plist-get note :severity))

(defun sly-note.message (note)
  (plist-get note :message))

(defun sly-note.source-context (note)
  (plist-get note :source-context))

(defun sly-note.location (note)
  (plist-get note :location))

(defun sly-severity-label (severity)
  (cl-subseq (symbol-name severity) 1))


;;;;; Adding a single compiler note

(defun sly-overlay-note (note)
  "Add a compiler note to the buffer as an overlay.
If an appropriate overlay for a compiler note in the same location
already exists then the new information is merged into it. Otherwise a
new overlay is created."
  (cl-multiple-value-bind (start end) (sly-choose-overlay-region note)
    (when start
      (goto-char start)
      (let ((severity (plist-get note :severity))
            (message (plist-get note :message))
            (overlay (sly-note-at-point)))
        (if overlay
            (sly-merge-note-into-overlay overlay severity message)
          (sly-create-note-overlay note start end severity message))))))

(defun sly-make-note-overlay (note start end)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'sly-note note)
    (push overlay sly-note-overlays)
    overlay))

(defun sly-create-note-overlay (note start end severity message)
  "Create an overlay representing a compiler note.
The overlay has several properties:
  FACE       - to underline the relevant text.
  SEVERITY   - for future reference :NOTE, :STYLE-WARNING, :WARNING, or :ERROR.
  MOUSE-FACE - highlight the note when the mouse passes over.
  HELP-ECHO  - a string describing the note, both for future reference
               and for display as a tooltip (due to the special
               property name)."
  (let ((overlay (sly-make-note-overlay note start end)))
    (cl-macrolet ((putp (name value) `(overlay-put overlay ,name ,value)))
      (putp 'face (sly-severity-face severity))
      (putp 'severity severity)
      (putp 'mouse-face 'highlight)
      (putp 'help-echo message)
      overlay)))

;; XXX Obsolete due to `sly-merge-notes-for-display' doing the
;; work already -- unless we decide to put several sets of notes on a
;; buffer without clearing in between, which only this handles.
(defun sly-merge-note-into-overlay (overlay severity message)
  "Merge another compiler note into an existing overlay.
The help text describes both notes, and the highest of the severities
is kept."
  (cl-macrolet ((putp (name value) `(overlay-put overlay ,name ,value))
                (getp (name)       `(overlay-get overlay ,name)))
    (putp 'severity (sly-most-severe severity (getp 'severity)))
    (putp 'face (sly-severity-face (getp 'severity)))
    (putp 'help-echo (concat (getp 'help-echo) "\n" message))))

(defun sly-choose-overlay-region (note)
  "Choose the start and end points for an overlay over NOTE.
If the location's sexp is a list spanning multiple lines, then the
region around the first element is used.
Return nil if there's no useful source location."
  (let ((location (sly-note.location note)))
    (when location
      (destructure-case location
        ((:error _))                 ; do nothing
        ((:location file pos _hints)
         (cond ((eq (car file) ':source-form) nil)
               ((eq (sly-note.severity note) :read-error)
                (sly-choose-overlay-for-read-error location))
               ((equal pos '(:eof))
                (cl-values (1- (point-max)) (point-max)))
               (t
                (sly-choose-overlay-for-sexp location))))))))

(defun sly-choose-overlay-for-read-error (location)
  (let ((pos (sly-location-offset location)))
    (save-excursion
      (goto-char pos)
      (cond ((sly-symbol-at-point)
             ;; package not found, &c.
             (cl-values (sly-symbol-start-pos) (sly-symbol-end-pos)))
            (t
             (cl-values pos (1+ pos)))))))

(defun sly-choose-overlay-for-sexp (location)
  (sly-goto-source-location location)
  (skip-chars-forward "'#`")
  (let ((start (point)))
    (ignore-errors (sly-forward-sexp))
    (if (sly-same-line-p start (point))
        (cl-values start (point))
      (cl-values (1+ start)
                 (progn (goto-char (1+ start))
                        (ignore-errors (forward-sexp 1))
                        (point))))))

(defun sly-same-line-p (pos1 pos2)
  "Return t if buffer positions POS1 and POS2 are on the same line."
  (save-excursion (goto-char (min pos1 pos2))
                  (<= (max pos1 pos2) (line-end-position))))

(defvar sly-severity-face-plist
  '(:error         sly-error-face
                   :read-error    sly-error-face
                   :warning       sly-warning-face
                   :redefinition  sly-style-warning-face
                   :style-warning sly-style-warning-face
                   :note          sly-note-face))

(defun sly-severity-face (severity)
  "Return the name of the font-lock face representing SEVERITY."
  (or (plist-get sly-severity-face-plist severity)
      (error "No face for: %S" severity)))

(defvar sly-severity-order
  '(:note :style-warning :redefinition :warning :error :read-error))

(defun sly-severity< (sev1 sev2)
  "Return true if SEV1 is less severe than SEV2."
  (< (cl-position sev1 sly-severity-order)
     (cl-position sev2 sly-severity-order)))

(defun sly-most-severe (sev1 sev2)
  "Return the most servere of two conditions."
  (if (sly-severity< sev1 sev2) sev2 sev1))

;; XXX: unused function
(defun sly-visit-source-path (source-path)
  "Visit a full source path including the top-level form."
  (goto-char (point-min))
  (sly-forward-source-path source-path))

(defun sly-forward-positioned-source-path (source-path)
  "Move forward through a sourcepath from a fixed position.
The point is assumed to already be at the outermost sexp, making the
first element of the source-path redundant."
  (ignore-errors
    (sly-forward-sexp)
    (beginning-of-defun))
  (when-let (source-path (cdr source-path))
    (down-list 1)
    (sly-forward-source-path source-path)))

(defun sly-forward-source-path (source-path)
  (let ((origin (point)))
    (condition-case nil
        (progn
          (cl-loop for (count . more) on source-path
                   do (progn
                        (sly-forward-sexp count)
                        (when more (down-list 1))))
          ;; Align at beginning
          (sly-forward-sexp)
          (beginning-of-sexp))
      (error (goto-char origin)))))


;; FIXME: really fix this mess
;; FIXME: the check shouln't be done here anyway but by M-. itself.

(defun sly-filesystem-toplevel-directory ()
  ;; Windows doesn't have a true toplevel root directory, and all
  ;; filenames look like "c:/foo/bar/quux.baz" from an Emacs
  ;; perspective anyway.
  (if (memq system-type '(ms-dos windows-nt))
      ""
    (file-name-as-directory "/")))

(defun sly-file-name-merge-source-root (target-filename buffer-filename)
  "Returns a filename where the source root directory of TARGET-FILENAME
is replaced with the source root directory of BUFFER-FILENAME.

If no common source root could be determined, return NIL.

E.g. (sly-file-name-merge-source-root
       \"/usr/local/src/joe/upstream/sbcl/code/late-extensions.lisp\"
       \"/usr/local/src/joe/hacked/sbcl/compiler/deftype.lisp\")

        ==> \"/usr/local/src/joe/hacked/sbcl/code/late-extensions.lisp\"
"
  (let ((target-dirs (split-string (file-name-directory target-filename)
                                   "/" t))
        (buffer-dirs (split-string (file-name-directory buffer-filename)
                                   "/" t)))
    ;; Starting from the end, we look if one of the TARGET-DIRS exists
    ;; in BUFFER-FILENAME---if so, it and everything left from that dirname
    ;; is considered to be the source root directory of BUFFER-FILENAME.
    (cl-loop with target-suffix-dirs = nil
             with buffer-dirs* = (reverse buffer-dirs)
             with target-dirs* = (reverse target-dirs)
             for target-dir in target-dirs*
             do (let  ((concat-dirs (lambda (dirs)
                                      (apply #'concat
                                             (mapcar #'file-name-as-directory
                                                     dirs))))
                       (pos (cl-position target-dir buffer-dirs*
                                         :test #'equal)))
                  (if (not pos)    ; TARGET-DIR not in BUFFER-FILENAME?
                      (push target-dir target-suffix-dirs)
                    (let* ((target-suffix
                                        ; PUSH reversed for us!
                            (funcall concat-dirs target-suffix-dirs))
                           (buffer-root
                            (funcall concat-dirs
                                     (reverse (nthcdr pos buffer-dirs*)))))
                      (cl-return (concat (sly-filesystem-toplevel-directory)
                                         buffer-root
                                         target-suffix
                                         (file-name-nondirectory
                                          target-filename)))))))))

(defun sly-highlight-differences-in-dirname (base-dirname contrast-dirname)
  "Returns a copy of BASE-DIRNAME where all differences between
BASE-DIRNAME and CONTRAST-DIRNAME are propertized with a
highlighting face."
  (setq base-dirname (file-name-as-directory base-dirname))
  (setq contrast-dirname (file-name-as-directory contrast-dirname))
  (let ((base-dirs (split-string base-dirname "/" t))
        (contrast-dirs (split-string contrast-dirname "/" t)))
    (with-temp-buffer
      (cl-loop initially (insert (sly-filesystem-toplevel-directory))
               for base-dir in base-dirs do
               (let ((pos (cl-position base-dir contrast-dirs :test #'equal)))
                 (cond ((not pos)
                        (sly-insert-propertized '(face highlight) base-dir)
                        (insert "/"))
                       (t
                        (insert (file-name-as-directory base-dir))
                        (setq contrast-dirs
                              (nthcdr (1+ pos) contrast-dirs))))))
      (buffer-substring (point-min) (point-max)))))

(defvar sly-warn-when-possibly-tricked-by-M-. t
  "When working on multiple source trees simultaneously, the way
`sly-edit-definition' (M-.) works can sometimes be confusing:

`M-.' visits locations that are present in the current Lisp image,
which works perfectly well as long as the image reflects the source
tree that one is currently looking at.

In the other case, however, one can easily end up visiting a file
in a different source root directory (cl-the one corresponding to
the Lisp image), and is thus easily tricked to modify the wrong
source files---which can lead to quite some stressfull cursing.

If this variable is T, a warning message is issued to raise the
user's attention whenever `M-.' is about opening a file in a
different source root that also exists in the source root
directory of the user's current buffer.

There's no guarantee that all possible cases are covered, but
if you encounter such a warning, it's a strong indication that
you should check twice before modifying.")

(defun sly-maybe-warn-for-different-source-root (target-filename
                                                   buffer-filename)
  (let ((guessed-target (sly-file-name-merge-source-root target-filename
                                                           buffer-filename)))
    (when (and guessed-target
               (not (equal guessed-target target-filename))
               (file-exists-p guessed-target))
      (sly-message "Attention: This is `%s'."
                     (concat (sly-highlight-differences-in-dirname
                              (file-name-directory target-filename)
                              (file-name-directory guessed-target))
                             (file-name-nondirectory target-filename))))))

(defun sly-check-location-filename-sanity (filename)
  (when sly-warn-when-possibly-tricked-by-M-.
    (cl-macrolet ((truename-safe (file) `(and ,file (file-truename ,file))))
      (let ((target-filename (truename-safe filename))
            (buffer-filename (truename-safe (buffer-file-name))))
        (when (and target-filename
                   buffer-filename)
          (sly-maybe-warn-for-different-source-root
           target-filename buffer-filename))))))

(defun sly-check-location-buffer-name-sanity (buffer-name)
  (sly-check-location-filename-sanity
   (buffer-file-name (get-buffer buffer-name))))



(defun sly-goto-location-buffer (buffer)
  (destructure-case buffer
    ((:file filename)
     (let ((filename (sly-from-lisp-filename filename)))
       (sly-check-location-filename-sanity filename)
       (set-buffer (or (get-file-buffer filename)
                       (let ((find-file-suppress-same-file-warnings t))
                         (find-file-noselect filename))))))
    ((:buffer buffer-name)
     (sly-check-location-buffer-name-sanity buffer-name)
     (set-buffer buffer-name))
    ((:buffer-and-file buffer filename)
     (sly-goto-location-buffer
      (if (get-buffer buffer)
          (list :buffer buffer)
        (list :file filename))))
    ((:source-form string)
     (set-buffer (get-buffer-create (sly-buffer-name :source)))
     (erase-buffer)
     (lisp-mode)
     (insert string)
     (goto-char (point-min)))
    ((:zip file entry)
     (require 'arc-mode)
     (set-buffer (find-file-noselect file t))
     (goto-char (point-min))
     (re-search-forward (concat "  " entry "$"))
     (let ((buffer (save-window-excursion
                     (archive-extract)
                     (current-buffer))))
       (set-buffer buffer)
       (goto-char (point-min))))))

(defun sly-goto-location-position (position)
  (destructure-case position
    ((:position pos)
     (goto-char 1)
     (forward-char (- (1- pos) (sly-eol-conversion-fixup (1- pos)))))
    ((:offset start offset)
     (goto-char start)
     (forward-char offset))
    ((:line start &optional column)
     (goto-char (point-min))
     (beginning-of-line start)
     (cond (column (move-to-column column))
           (t (skip-chars-forward " \t"))))
    ((:function-name name)
     (let ((case-fold-search t)
           (name (regexp-quote name)))
       (goto-char (point-min))
       (when (or
              (re-search-forward
               (format "\\s *(def\\(\\s_\\|\\sw\\)*\\s +(*%s\\S_"
                       (regexp-quote name)) nil t)
              (re-search-forward
               (format "[( \t]%s\\>\\(\\s \\|$\\)" name) nil t))
         (goto-char (match-beginning 0)))))
    ((:method name specializers &rest qualifiers)
     (sly-search-method-location name specializers qualifiers))
    ((:source-path source-path start-position)
     (cond (start-position
            (goto-char start-position)
            (sly-forward-positioned-source-path source-path))
           (t
            (sly-forward-source-path source-path))))
    ((:eof)
     (goto-char (point-max)))))

(defun sly-eol-conversion-fixup (n)
  ;; Return the number of \r\n eol markers that we need to cross when
  ;; moving N chars forward.  N is the number of chars but \r\n are
  ;; counted as 2 separate chars.
  (cl-case (coding-system-eol-type buffer-file-coding-system)
    ((1)
     (save-excursion
       (cl-do ((pos (+ (point) n))
               (count 0 (1+ count)))
           ((>= (point) pos) (1- count))
         (forward-line)
         (cl-decf pos))))
    (t 0)))

(defun sly-search-method-location (name specializers qualifiers)
  ;; Look for a sequence of words (def<something> method name
  ;; qualifers specializers don't look for "T" since it isn't requires
  ;; (arg without t) as class is taken as such.
  (let* ((case-fold-search t)
         (name (regexp-quote name))
         (qualifiers (mapconcat (lambda (el) (concat ".+?\\<" el "\\>"))
                                qualifiers ""))
         (specializers (mapconcat
                        (lambda (el)
                          (if (eql (aref el 0) ?\()
                              (let ((spec (read el)))
                                (if (eq (car spec) 'EQL)
                                    (concat
                                     ".*?\\n\\{0,1\\}.*?(EQL.*?'\\{0,1\\}"
                                     (format "%s" (cl-second spec)) ")")
                                  (error "don't understand specializer: %s,%s"
                                         el (car spec))))
                            (concat ".+?\n\\{0,1\\}.+?\\<" el "\\>")))
                        (remove "T" specializers) ""))
         (regexp (format "\\s *(def\\(\\s_\\|\\sw\\)*\\s +%s\\s +%s%s" name
                         qualifiers specializers)))
    (or (and (re-search-forward regexp  nil t)
             (goto-char (match-beginning 0)))
        ;;	(sly-goto-location-position `(:function-name ,name))
        )))

(defun sly-search-call-site (fname)
  "Move to the place where FNAME called.
Don't move if there are multiple or no calls in the current defun."
  (save-restriction
    (narrow-to-defun)
    (let ((start (point))
          (regexp (concat "(" fname "[)\n \t]"))
          (case-fold-search t))
      (cond ((and (re-search-forward regexp nil t)
                  (not (re-search-forward regexp nil t)))
             (goto-char (match-beginning 0)))
            (t (goto-char start))))))

(defun sly-search-edit-path (edit-path)
  "Move to EDIT-PATH starting at the current toplevel form."
  (when edit-path
    (unless (and (= (current-column) 0)
                 (looking-at "("))
      (beginning-of-defun))
    (sly-forward-source-path edit-path)))

(defun sly-goto-source-location (location &optional noerror)
  "Move to the source location LOCATION.  Several kinds of locations
are supported:

<location> ::= (:location <buffer> <position> <hints>)
             | (:error <message>)

<buffer>   ::= (:file <filename>)
             | (:buffer <buffername>)
             | (:buffer-and-file <buffername> <filename>)
             | (:source-form <string>)
             | (:zip <file> <entry>)

<position> ::= (:position <fixnum>) ; 1 based (for files)
             | (:offset <start> <offset>) ; start+offset (for C-c C-c)
             | (:line <line> [<column>])
             | (:function-name <string>)
             | (:source-path <list> <start-position>)
             | (:method <name string> <specializers> . <qualifiers>)"
  (destructure-case location
    ((:location buffer _position _hints)
     (sly-goto-location-buffer buffer)
     (let ((pos (sly-location-offset location)))
       (cond ((and (<= (point-min) pos) (<= pos (point-max))))
             (widen-automatically (widen))
             (t
              (error "Location is outside accessible part of buffer")))
       (goto-char pos)))
    ((:error message)
     (if noerror
         (sly-message "%s" message)
       (error "%s" message)))))

(defun sly-location-offset (location)
  "Return the position, as character number, of LOCATION."
  (save-restriction
    (widen)
    (condition-case nil
        (sly-goto-location-position
         (sly-location.position location))
      (error (goto-char 0)))
    (let ((hints (sly-location.hints location)))
      (when-let (snippet (cl-getf hints :snippet))
        (sly-isearch snippet))
      (when-let (snippet (cl-getf hints :edit-path))
        (sly-search-edit-path snippet))
      (when-let (fname (cl-getf hints :call-site))
        (sly-search-call-site fname))
      (when (cl-getf hints :align)
        (sly-forward-sexp)
        (beginning-of-sexp)))
    (point)))


;;;;; Incremental search
;;
;; Search for the longest match of a string in either direction.
;;
;; This is for locating text that is expected to be near the point and
;; may have been modified (but hopefully not near the beginning!)

(defun sly-isearch (string)
  "Find the longest occurence of STRING either backwards of forwards.
If multiple matches exist the choose the one nearest to point."
  (goto-char
   (let* ((start (point))
          (len1 (sly-isearch-with-function 'search-forward string))
          (pos1 (point)))
     (goto-char start)
     (let* ((len2 (sly-isearch-with-function 'search-backward string))
            (pos2 (point)))
       (cond ((and len1 len2)
              ;; Have a match in both directions
              (cond ((= len1 len2)
                     ;; Both are full matches -- choose the nearest.
                     (if (< (abs (- start pos1))
                            (abs (- start pos2)))
                         pos1 pos2))
                    ((> len1 len2) pos1)
                    ((> len2 len1) pos2)))
             (len1 pos1)
             (len2 pos2)
             (t start))))))

(defun sly-isearch-with-function (search-fn string)
  "Search for the longest substring of STRING using SEARCH-FN.
SEARCH-FN is either the symbol `search-forward' or `search-backward'."
  (unless (string= string "")
    (cl-loop for i from 1 to (length string)
             while (funcall search-fn (substring string 0 i) nil t)
             for match-data = (match-data)
             do (cl-case search-fn
                  (search-forward  (goto-char (match-beginning 0)))
                  (search-backward (goto-char (1+ (match-end 0)))))
             finally (cl-return (if (null match-data)
                                    nil
                                  ;; Finish based on the last successful match
                                  (store-match-data match-data)
                                  (goto-char (match-beginning 0))
                                  (- (match-end 0) (match-beginning 0)))))))


;;;;; Visiting and navigating the overlays of compiler notes

(defun sly-next-note ()
  "Go to and describe the next compiler note in the buffer."
  (interactive)
  (let ((here (point))
        (note (sly-find-next-note)))
    (if note
        (sly-show-note note)
      (goto-char here)
      (message "No next note."))))

(defun sly-previous-note ()
  "Go to and describe the previous compiler note in the buffer."
  (interactive)
  (let ((here (point))
        (note (sly-find-previous-note)))
    (if note
        (sly-show-note note)
      (goto-char here)
      (message "No previous note."))))

(defun sly-goto-first-note (&rest _)
  "Go to the first note in the buffer."
  (let ((point (point)))
    (goto-char (point-min))
    (cond ((sly-find-next-note)
           (sly-show-note (sly-note-at-point)))
          (t (goto-char point)))))

(defun sly-remove-notes ()
  "Remove compiler-note annotations from the current buffer."
  (interactive)
  (sly-remove-old-overlays))

(defun sly-show-note (overlay)
  "Present the details of a compiler note to the user."
  (sly-temporarily-highlight-note overlay)
  (if (get-buffer-window (sly-buffer-name :compilation) t)
      (sly-goto-note-in-compilation-log (overlay-get overlay 'sly-note)))
  (let ((message (get-char-property (point) 'help-echo)))
      (sly-message "%s" (if (zerop (length message)) "\"\"" message))))

;; FIXME: could probably use flash region
(defun sly-temporarily-highlight-note (overlay)
  "Temporarily highlight a compiler note's overlay.
The highlighting is designed to both make the relevant source more
visible, and to highlight any further notes that are nested inside the
current one.

The highlighting is automatically undone with a timer."
  (run-with-timer 0.2 nil
                  #'overlay-put overlay 'face (overlay-get overlay 'face))
  (overlay-put overlay 'face 'sly-highlight-face))


;;;;; Overlay lookup operations

(defun sly-note-at-point ()
  "Return the overlay for a note starting at point, otherwise NIL."
  (cl-find (point) (sly-note-overlays-at-point)
           :key 'overlay-start))

(defun sly-note-overlay-p (overlay)
  "Return true if OVERLAY represents a compiler note."
  (overlay-get overlay 'sly-note))

(defun sly-note-overlays-at-point ()
  "Return a list of all note overlays that are under the point."
  (cl-remove-if-not 'sly-note-overlay-p (overlays-at (point))))

(defun sly-find-next-note ()
  "Go to the next position with the `sly-note' text property.
Retuns the note overlay if such a position is found, otherwise nil."
  (sly-search-property 'sly-note nil #'sly-note-at-point))

(defun sly-find-previous-note ()
  "Go to the next position with the `sly-note' text property.
Retuns the note overlay if such a position is found, otherwise nil."
  (sly-search-property 'sly-note t #'sly-note-at-point))


;;;; Completion

;; XXX those long names are ugly to read; long names an indicator for
;; bad factoring?

(defvar sly-completions-buffer-name "*Completions*")

;; FIXME: can probably use quit-window instead
(make-variable-buffer-local
 (defvar sly-complete-saved-window-configuration nil
   "Window configuration before we show the *Completions* buffer.
This is buffer local in the buffer where the completion is
performed."))

(make-variable-buffer-local
 (defvar sly-completions-window nil
   "The window displaying *Completions* after saving window configuration.
If this window is no longer active or displaying the completions
buffer then we can ignore `sly-complete-saved-window-configuration'."))

(defun sly-complete-maybe-save-window-configuration ()
  "Maybe save the current window configuration.
Return true if the configuration was saved."
  (unless (or sly-complete-saved-window-configuration
              (get-buffer-window sly-completions-buffer-name))
    (setq sly-complete-saved-window-configuration
          (current-window-configuration))
    t))

(defun sly-complete-delay-restoration ()
  (add-hook 'pre-command-hook
            'sly-complete-maybe-restore-window-configuration
            'append
            'local))

(defun sly-complete-forget-window-configuration ()
  (setq sly-complete-saved-window-configuration nil)
  (setq sly-completions-window nil))

(defun sly-complete-restore-window-configuration ()
  "Restore the window config if available."
  (remove-hook 'pre-command-hook
               'sly-complete-maybe-restore-window-configuration)
  (when (and sly-complete-saved-window-configuration
             (sly-completion-window-active-p))
    (save-excursion (set-window-configuration
                     sly-complete-saved-window-configuration))
    (setq sly-complete-saved-window-configuration nil)
    (when (buffer-live-p sly-completions-buffer-name)
      (kill-buffer sly-completions-buffer-name))))

(defun sly-complete-maybe-restore-window-configuration ()
  "Restore the window configuration, if the following command
terminates a current completion."
  (remove-hook 'pre-command-hook
               'sly-complete-maybe-restore-window-configuration)
  (condition-case err
      (cond ((cl-find last-command-event "()\"'`,# \r\n:")
             (sly-complete-restore-window-configuration))
            ((not (sly-completion-window-active-p))
             (sly-complete-forget-window-configuration))
            (t
             (sly-complete-delay-restoration)))
    (error
     ;; Because this is called on the pre-command-hook, we mustn't let
     ;; errors propagate.
     (message "Error in sly-complete-restore-window-configuration: %S"
              err))))

(defun sly-completion-window-active-p ()
  "Is the completion window currently active?"
  (and (window-live-p sly-completions-window)
       (equal (buffer-name (window-buffer sly-completions-window))
              sly-completions-buffer-name)))

(defun sly-display-completion-list (completions base)
  (let ((savedp (sly-complete-maybe-save-window-configuration)))
    (with-output-to-temp-buffer sly-completions-buffer-name
      (display-completion-list completions)
      (let ((offset (- (point) 1 (length base))))
        (with-current-buffer standard-output
          (setq completion-base-position offset)
          (set-syntax-table lisp-mode-syntax-table))))
    (when savedp
      (setq sly-completions-window
            (get-buffer-window sly-completions-buffer-name)))))

(defun sly-display-or-scroll-completions (completions base)
  (cond ((and (eq last-command this-command)
              (sly-completion-window-active-p))
         (sly-scroll-completions))
        (t
         (sly-display-completion-list completions base)))
  (sly-complete-delay-restoration))

(defun sly-scroll-completions ()
  (let ((window sly-completions-window))
    (with-current-buffer (window-buffer window)
      (if (pos-visible-in-window-p (point-max) window)
          (set-window-start window (point-min))
        (save-selected-window
          (select-window window)
          (scroll-up))))))

(defun sly-complete-symbol ()
  "Complete the symbol at point.

Completion is performed by `sly-complete-symbol-function'."
  (interactive)
  (funcall sly-complete-symbol-function))

(defun sly-simple-complete-symbol ()
  "Complete the symbol at point.
Perform completion more similar to Emacs' complete-symbol."
  (or (sly-maybe-complete-as-filename)
      (let* ((end (point))
             (beg (sly-symbol-start-pos))
             (prefix (buffer-substring-no-properties beg end))
             (result (sly-simple-completions prefix)))
        (cl-destructuring-bind (completions partial) result
          (if (null completions)
              (progn (sly-minibuffer-respecting-message
                      "Can't find completion for \"%s\"" prefix)
                     (ding)
                     (sly-complete-restore-window-configuration))
            (insert-and-inherit (substring partial (length prefix)))
            (cond ((sly-length= completions 1)
                   (sly-minibuffer-respecting-message "Sole completion")
                   (sly-complete-restore-window-configuration))
                  ;; Incomplete
                  (t
                   (when (member partial completions)
                     (sly-minibuffer-respecting-message
                      "Complete but not unique"))
                   (sly-display-or-scroll-completions completions
                                                        partial))))))))

(defun sly-maybe-complete-as-filename ()
  "If point is at a string starting with \", complete it as filename.
Return nil if point is not at filename."
  (when (save-excursion (re-search-backward "\"[^ \t\n]+\\="
                                            (max (point-min)
                                                 (- (point) 1000)) t))
    (let ((comint-completion-addsuffix '("/" . "\"")))
      (comint-replace-by-expanded-filename)
      t)))

(defun sly-minibuffer-respecting-message (format &rest format-args)
  "Display TEXT as a message, without hiding any minibuffer contents."
  (let ((text (format " [%s]" (apply #'format format format-args))))
    (if (minibuffer-window-active-p (minibuffer-window))
        (minibuffer-message text)
      (message "%s" text))))

(defun sly-show-arglist ()
  (let ((op (ignore-errors
              (save-excursion
                (backward-up-list 1)
                (down-list 1)
                (sly-symbol-at-point)))))
    (when op
      (sly-eval-async `(swank:operator-arglist ,op ,(sly-current-package))
        (lambda (arglist)
          (when arglist
            (sly-message "%s" arglist)))))))

(defun sly-indent-and-complete-symbol ()
  "Indent the current line and perform symbol completion.
First indent the line. If indenting doesn't move point, complete
the symbol. If there's no symbol at the point, show the arglist
for the most recently enclosed macro or function."
  (interactive)
  (let ((pos (point)))
    (unless (get-text-property (line-beginning-position) 'sly-repl-prompt)
      (lisp-indent-line))
    (when (= pos (point))
      (cond ((save-excursion (re-search-backward "[^() \n\t\r]+\\=" nil t))
             (sly-complete-symbol))
            ((memq (char-before) '(?\t ?\ ))
             (sly-show-arglist))))))

(defvar sly-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\t" 'sly-complete-symbol)
    map)
  "Minibuffer keymap used for reading CL expressions.")

(defvar sly-minibuffer-history '()
  "History list of expressions read from the minibuffer.")

(defun sly-minibuffer-setup-hook ()
  (cons (lexical-let ((package (sly-current-package))
                      (connection (sly-connection)))
          (lambda ()
            (setq sly-buffer-package package)
            (setq sly-buffer-connection connection)
            (set-syntax-table lisp-mode-syntax-table)))
        minibuffer-setup-hook))

(defun sly-read-from-minibuffer (prompt &optional initial-value history)
  "Read a string from the minibuffer, prompting with PROMPT.
If INITIAL-VALUE is non-nil, it is inserted into the minibuffer before
reading input.  The result is a string (\"\" if no input was given)."
  (let ((minibuffer-setup-hook (sly-minibuffer-setup-hook)))
    (read-from-minibuffer prompt initial-value sly-minibuffer-map
			  nil (or history 'sly-minibuffer-history))))

(defun sly-bogus-completion-alist (list)
  "Make an alist out of list.
The same elements go in the CAR, and nil in the CDR. To support the
apparently very stupid `try-completions' interface, that wants an
alist but ignores CDRs."
  (mapcar (lambda (x) (cons x nil)) list))

(defun sly-simple-completions (prefix)
  (let ((sly-current-thread t))
    (sly-eval
     `(swank:simple-completions ,prefix ',(sly-current-package)))))


;;;; Edit definition

(defun sly-push-definition-stack ()
  "Add point to find-tag-marker-ring."
  (require 'etags)
  (ring-insert find-tag-marker-ring (point-marker)))

(defun sly-pop-find-definition-stack ()
  "Pop the edit-definition stack and goto the location."
  (interactive)
  (pop-tag-mark))

(cl-defstruct (sly-xref (:conc-name sly-xref.) (:type list))
  dspec location)

(cl-defstruct (sly-location (:conc-name sly-location.) (:type list)
                              (:constructor nil)
                              (:copier nil))
  tag buffer position hints)

(defun sly-location-p (o) (and (consp o) (eq (car o) :location)))

(defun sly-xref-has-location-p (xref)
  (sly-location-p (sly-xref.location xref)))

(defun make-sly-buffer-location (buffer-name position &optional hints)
  `(:location (:buffer ,buffer-name) (:position ,position)
              ,(when hints `(:hints ,hints))))

(defun make-sly-file-location (file-name position &optional hints)
  `(:location (:file ,file-name) (:position ,position)
              ,(when hints `(:hints ,hints))))

;;; The hooks are tried in order until one succeeds, otherwise the
;;; default implementation involving `sly-find-definitions-function'
;;; is used. The hooks are called with the same arguments as
;;; `sly-edit-definition'.
(defvar sly-edit-definition-hooks)

(defun sly-edit-definition (&optional name where)
  "Lookup the definition of the name at point.
If there's no name at point, or a prefix argument is given, then the
function name is prompted."
  (interactive (list (or (and (not current-prefix-arg)
                              (sly-symbol-at-point))
                         (sly-read-symbol-name "Edit Definition of: "))))
  ;; The hooks might search for a name in a different manner, so don't
  ;; ask the user if it's missing before the hooks are run
  (or (run-hook-with-args-until-success 'sly-edit-definition-hooks
                                        name where)
      (sly-edit-definition-cont (sly-find-definitions name)
                                  name where)))

(defun sly-edit-definition-cont (xrefs name where)
  (cl-destructuring-bind (1loc file-alist) (sly-analyze-xrefs xrefs)
    (cond ((null xrefs)
           (error "No known definition for: %s (in %s)"
                  name (sly-current-package)))
          (1loc
           (sly-push-definition-stack)
           (sly-pop-to-location (sly-xref.location (car xrefs)) where))
          ((sly-length= xrefs 1)      ; ((:error "..."))
           (error "%s" (cadr (sly-xref.location (car xrefs)))))
          (t
           (sly-push-definition-stack)
           (sly-show-xrefs file-alist 'definition name
                             (sly-current-package))))))

(defvar sly-edit-uses-xrefs
  '(:calls :macroexpands :binds :references :sets :specializes))

;;; FIXME. TODO: Would be nice to group the symbols (in each
;;;              type-group) by their home-package.
(defun sly-edit-uses (symbol)
  "Lookup all the uses of SYMBOL."
  (interactive (list (sly-read-symbol-name "Edit Uses of: ")))
  (sly-xrefs sly-edit-uses-xrefs
               symbol
               (lambda (xrefs type symbol package)
                 (cond
                  ((null xrefs)
                   (message "No xref information found for %s." symbol))
                  ((and (sly-length= xrefs 1)          ; one group
                        (sly-length= (cdar  xrefs) 1)) ; one ref in group
                   (cl-destructuring-bind (_ (_ loc)) (cl-first xrefs)
                     (sly-push-definition-stack)
                     (sly-pop-to-location loc)))
                  (t
                   (sly-push-definition-stack)
                   (sly-show-xref-buffer xrefs type symbol package))))))

(defun sly-analyze-xrefs (xrefs)
  "Find common filenames in XREFS.
Return a list (SINGLE-LOCATION FILE-ALIST).
SINGLE-LOCATION is true if all xrefs point to the same location.
FILE-ALIST is an alist of the form ((FILENAME . (XREF ...)) ...)."
  (list (and xrefs
             (let ((loc (sly-xref.location (car xrefs))))
               (and (sly-location-p loc)
                    (cl-every (lambda (x) (equal (sly-xref.location x) loc))
                              (cdr xrefs)))))
        (sly-alistify xrefs #'sly-xref-group #'equal)))

(defun sly-xref-group (xref)
  (cond ((sly-xref-has-location-p xref)
         (destructure-case (sly-location.buffer (sly-xref.location xref))
           ((:file filename) filename)
           ((:buffer bufname)
            (let ((buffer (get-buffer bufname)))
              (if buffer
                  (format "%S" buffer) ; "#<buffer foo.lisp>"
                (format "%s (previously existing buffer)" bufname))))
           ((:buffer-and-file _buffer filename) filename)
           ((:source-form _) "(S-Exp)")
           ((:zip _zip entry) entry)))
        (t
         "(No location)")))

(defun sly-pop-to-location (location &optional where)
  (sly-goto-source-location location)
  (cl-ecase where
    ((nil)     (switch-to-buffer (current-buffer)))
    (window    (pop-to-buffer (current-buffer) t))
    (frame     (let ((pop-up-frames t)) (pop-to-buffer (current-buffer) t)))))

(defun sly-postprocess-xref (original-xref)
  "Process (for normalization purposes) an Xref comming directly
from SWANK before the rest of SLY sees it. In particular,
convert ETAGS based xrefs to actual file+position based
locations."
  (if (not (sly-xref-has-location-p original-xref))
      (list original-xref)
    (let ((loc (sly-xref.location original-xref)))
      (destructure-case (sly-location.buffer loc)
        ((:etags-file tags-file)
         (destructure-case (sly-location.position loc)
           ((:tag &rest tags)
            (visit-tags-table tags-file)
            (mapcar (lambda (xref)
                      (let ((old-dspec (sly-xref.dspec original-xref))
                            (new-dspec (sly-xref.dspec xref)))
                        (setf (sly-xref.dspec xref)
                              (format "%s: %s" old-dspec new-dspec))
                        xref))
                    (cl-mapcan #'sly-etags-definitions tags)))))
        (t
         (list original-xref))))))

(defun sly-postprocess-xrefs (xrefs)
  (cl-mapcan #'sly-postprocess-xref xrefs))

(defun sly-find-definitions (name)
  "Find definitions for NAME."
  (sly-postprocess-xrefs (funcall sly-find-definitions-function name)))

(defun sly-find-definitions-rpc (name)
  (sly-eval `(swank:find-definitions-for-emacs ,name)))

(defun sly-edit-definition-other-window (name)
  "Like `sly-edit-definition' but switch to the other window."
  (interactive (list (sly-read-symbol-name "Symbol: ")))
  (sly-edit-definition name 'window))

(defun sly-edit-definition-other-frame (name)
  "Like `sly-edit-definition' but switch to the other window."
  (interactive (list (sly-read-symbol-name "Symbol: ")))
  (sly-edit-definition name 'frame))

(defun sly-edit-definition-with-etags (name)
  (interactive (list (sly-read-symbol-name "Symbol: ")))
  (let ((xrefs (sly-etags-definitions name)))
    (cond (xrefs
           (message "Using tag file...")
           (sly-edit-definition-cont xrefs name nil))
          (t
           (error "No known definition for: %s" name)))))

(defun sly-etags-to-locations (name)
  "Search for definitions matching `name' in the currently active
tags table. Return a possibly empty list of sly-locations."
  (let ((locs '()))
    (save-excursion
      (let ((first-time t))
        (while (visit-tags-table-buffer (not first-time))
          (setq first-time nil)
          (goto-char (point-min))
          (while (search-forward name nil t)
            (beginning-of-line)
            (cl-destructuring-bind (hint line &rest pos) (etags-snarf-tag)
              (unless (eq hint t) ; hint==t if we are in a filename line
                (push `(:location (:file ,(expand-file-name (file-of-tag)))
                                  (:line ,line)
                                  (:snippet ,hint))
                      locs))))))
      (nreverse locs))))

(defun sly-etags-definitions (name)
  "Search definitions matching NAME in the tags file.
The result is a (possibly empty) list of definitions."
  (mapcar (lambda (loc)
            (make-sly-xref :dspec (cl-second (sly-location.hints loc))
                             :location loc))
          (sly-etags-to-locations name)))

;;;;; first-change-hook

(defun sly-first-change-hook ()
  "Notify Lisp that a source file's buffer has been modified."
  ;; Be careful not to disturb anything!
  ;; In particular if we muck up the match-data then query-replace
  ;; breaks. -luke (26/Jul/2004)
  (save-excursion
    (save-match-data
      (when (and (buffer-file-name)
                 (file-exists-p (buffer-file-name))
                 (sly-background-activities-enabled-p))
        (let ((filename (sly-to-lisp-filename (buffer-file-name))))
          (sly-eval-async `(swank:buffer-first-change ,filename)))))))

(defun sly-setup-first-change-hook ()
  (add-hook (make-local-variable 'first-change-hook)
            'sly-first-change-hook))

(add-hook 'sly-mode-hook 'sly-setup-first-change-hook)


;;;; Eval for Lisp

(defun sly-eval-for-lisp (thread tag form-string)
  (let ((ok nil)
        (value nil)
        (error nil)
        (c (sly-connection)))
    (unwind-protect
        (condition-case err
            (progn
              (sly-check-eval-in-emacs-enabled)
              (setq value (eval (read form-string)))
              (sly-check-eval-in-emacs-result value)
              (setq ok t))
          ((debug error)
           (setq error err)))
      (let ((result (cond (ok `(:ok ,value))
                          (error `(:error ,(symbol-name (car error))
                                          . ,(mapcar #'prin1-to-string
                                                     (cdr error))))
                          (t `(:abort)))))
        (sly-dispatch-event `(:emacs-return ,thread ,tag ,result) c)))))

(defun sly-check-eval-in-emacs-result (x)
  "Raise an error if X can't be marshaled."
  (or (stringp x)
      (memq x '(nil t))
      (integerp x)
      (keywordp x)
      (and (consp x)
           (let ((l x))
             (while (consp l)
               (sly-check-eval-in-emacs-result (car x))
               (setq l (cdr l)))
             (sly-check-eval-in-emacs-result l)))
      (error "Non-serializable return value: %S" x)))

(defun sly-check-eval-in-emacs-enabled ()
  "Raise an error if `sly-enable-evaluate-in-emacs' isn't true."
  (unless sly-enable-evaluate-in-emacs
    (error (concat "sly-eval-in-emacs disabled for security."
                   "Set sly-enable-evaluate-in-emacs true to enable it."))))


;;;; `ED'

(defvar sly-ed-frame nil
  "The frame used by `sly-ed'.")

(defcustom sly-ed-use-dedicated-frame t
  "*When non-nil, `sly-ed' will create and reuse a dedicated frame."
  :type 'boolean
  :group 'sly-mode)

(defun sly-ed (what)
  "Edit WHAT.

WHAT can be:
  A filename (string),
  A list (:filename FILENAME &key LINE COLUMN POSITION),
  A function name (:function-name STRING)
  nil.

This is for use in the implementation of COMMON-LISP:ED."
  (when sly-ed-use-dedicated-frame
    (unless (and sly-ed-frame (frame-live-p sly-ed-frame))
      (setq sly-ed-frame (make-frame)))
    (select-frame sly-ed-frame))
  (when what
    (destructure-case what
      ((:filename file &key line column position bytep)
       (find-file (sly-from-lisp-filename file))
       (when line (sly-goto-line line))
       (when column (move-to-column column))
       (when position
         (goto-char (if bytep
                        (byte-to-position position)
                      position))))
      ((:function-name name)
       (sly-edit-definition name)))))

(defun sly-goto-line (line-number)
  "Move to line LINE-NUMBER (1-based).
This is similar to `goto-line' but without pushing the mark and
the display stuff that we neither need nor want."
  (cl-assert (= (buffer-size) (- (point-max) (point-min))) ()
             "sly-goto-line in narrowed buffer")
  (goto-char (point-min))
  (forward-line (1- line-number)))

(defun sly-y-or-n-p (thread tag question)
  (sly-dispatch-event `(:emacs-return ,thread ,tag ,(y-or-n-p question))))

(defun sly-read-from-minibuffer-for-swank (thread tag prompt initial-value)
  (let ((answer (condition-case nil
                    (sly-read-from-minibuffer prompt initial-value)
                  (quit nil))))
    (sly-dispatch-event `(:emacs-return ,thread ,tag ,answer))))

;;;; Interactive evaluation.

(defun sly-interactive-eval (string)
  "Read and evaluate STRING and print value in minibuffer.

Note: If a prefix argument is in effect then the result will be
inserted in the current buffer."
  (interactive (list (sly-read-from-minibuffer "SLY Eval: ")))
  (cl-case current-prefix-arg
    ((nil)
     (sly-eval-with-transcript `(swank:interactive-eval ,string)))
    ((-)
     (sly-eval-save string))
    (t
     (sly-eval-print string))))

(defvar sly-transcript-start-hook nil
  "Hook run before start an evalution.")
(defvar sly-transcript-stop-hook nil
  "Hook run after finishing a evalution.")

(defun sly-display-eval-result (value)
  (sly-message "%s" value))

(defun sly-eval-with-transcript (form)
  "Eval FORM in Lisp.  Display output, if any."
  (run-hooks 'sly-transcript-start-hook)
  (sly-rex () (form)
    ((:ok value)
     (run-hooks 'sly-transcript-stop-hook)
     (sly-display-eval-result value))
    ((:abort condition)
     (run-hooks 'sly-transcript-stop-hook)
     (message "Evaluation aborted on %s." condition))))

(defun sly-eval-print (string)
  "Eval STRING in Lisp; insert any output and the result at point."
  (sly-eval-async `(swank:eval-and-grab-output ,string)
    (lambda (result)
      (cl-destructuring-bind (output value) result
        (push-mark)
        (insert output value)))))

(defun sly-eval-save (string)
  "Evaluate STRING in Lisp and save the result in the kill ring."
  (sly-eval-async `(swank:eval-and-grab-output ,string)
    (lambda (result)
      (cl-destructuring-bind (output value) result
        (let ((string (concat output value)))
          (kill-new string)
          (message "Evaluation finished; pushed result to kill ring."))))))

(defun sly-eval-describe (form)
  "Evaluate FORM in Lisp and display the result in a new buffer."
  (sly-eval-async form (sly-rcurry #'sly-show-description
                                       (sly-current-package))))

(defvar sly-description-autofocus nil
  "If non-nil select description windows on display.")

(defun sly-show-description (string package)
  ;; So we can have one description buffer open per connection. Useful
  ;; for comparing the output of DISASSEMBLE across implementations.
  ;; FIXME: could easily be achieved with M-x rename-buffer
  (let ((bufname (sly-buffer-name :description)))
    (sly-with-popup-buffer (bufname :package package
                                      :connection t
                                      :select sly-description-autofocus)
      (princ string)
      (goto-char (point-min)))))

(defun sly-last-expression ()
  (buffer-substring-no-properties
   (save-excursion (backward-sexp) (point))
   (point)))

(defun sly-eval-last-expression ()
  "Evaluate the expression preceding point."
  (interactive)
  (sly-interactive-eval (sly-last-expression)))

(defun sly-eval-defun ()
  "Evaluate the current toplevel form.
Use `sly-re-evaluate-defvar' if the from starts with '(defvar'"
  (interactive)
  (let ((form (sly-defun-at-point)))
    (cond ((string-match "^(defvar " form)
           (sly-re-evaluate-defvar form))
          (t
           (sly-interactive-eval form)))))

(defun sly-eval-region (start end)
  "Evaluate region."
  (interactive "r")
  (sly-eval-with-transcript
   `(swank:interactive-eval-region
     ,(buffer-substring-no-properties start end))))

(defun sly-pprint-eval-region (start end)
  "Evaluate region; pprint the value in a buffer."
  (interactive "r")
  (sly-eval-describe
   `(swank:pprint-eval
     ,(buffer-substring-no-properties start end))))

(defun sly-eval-buffer ()
  "Evaluate the current buffer.
The value is printed in the echo area."
  (interactive)
  (sly-eval-region (point-min) (point-max)))

(defun sly-re-evaluate-defvar (form)
  "Force the re-evaluaton of the defvar form before point.

First make the variable unbound, then evaluate the entire form."
  (interactive (list (sly-last-expression)))
  (sly-eval-with-transcript `(swank:re-evaluate-defvar ,form)))

(defun sly-pprint-eval-last-expression ()
  "Evaluate the form before point; pprint the value in a buffer."
  (interactive)
  (sly-eval-describe `(swank:pprint-eval ,(sly-last-expression))))

(defun sly-eval-print-last-expression (string)
  "Evaluate sexp before point; print value into the current buffer"
  (interactive (list (sly-last-expression)))
  (insert "\n")
  (sly-eval-print string))

;;;; Edit Lisp value
;;;
(defun sly-edit-value (form-string)
  "\\<sly-edit-value-mode-map>\
Edit the value of a setf'able form in a new buffer.
The value is inserted into a temporary buffer for editing and then set
in Lisp when committed with \\[sly-edit-value-commit]."
  (interactive
   (list (sly-read-from-minibuffer "Edit value (evaluated): "
				     (sly-sexp-at-point))))
  (sly-eval-async `(swank:value-for-editing ,form-string)
    (lexical-let ((form-string form-string)
                  (package (sly-current-package)))
      (lambda (result)
        (sly-edit-value-callback form-string result
                                   package)))))

(make-variable-buffer-local
 (defvar sly-edit-form-string nil
   "The form being edited by `sly-edit-value'."))

(define-minor-mode sly-edit-value-mode
  "Mode for editing a Lisp value."
  nil
  " Edit-Value"
  '(("\C-c\C-c" . sly-edit-value-commit)))

(defun sly-edit-value-callback (form-string current-value package)
  (let* ((name (generate-new-buffer-name (format "*Edit %s*" form-string)))
         (buffer (sly-with-popup-buffer (name :package package
                                                :connection t
                                                :select t
                                                :mode 'lisp-mode)
                   (sly-popup-buffer-mode -1) ; don't want binding of 'q'
                   (sly-mode 1)
                   (sly-edit-value-mode 1)
                   (setq sly-edit-form-string form-string)
                   (insert current-value)
                   (current-buffer))))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (message "Type C-c C-c when done"))))

(defun sly-edit-value-commit ()
  "Commit the edited value to the Lisp image.
\\(See `sly-edit-value'.)"
  (interactive)
  (if (null sly-edit-form-string)
      (error "Not editing a value.")
    (let ((value (buffer-substring-no-properties (point-min) (point-max))))
      (lexical-let ((buffer (current-buffer)))
        (sly-eval-async `(swank:commit-edited-value ,sly-edit-form-string
                                                      ,value)
          (lambda (_)
            (with-current-buffer buffer
              (quit-window t))))))))

;;;; Tracing

(defun sly-untrace-all ()
  "Untrace all functions."
  (interactive)
  (sly-eval `(swank:untrace-all)))

(defun sly-toggle-trace-fdefinition (spec)
  "Toggle trace."
  (interactive (list (sly-read-from-minibuffer
                      "(Un)trace: " (sly-symbol-at-point))))
  (message "%s" (sly-eval `(swank:swank-toggle-trace ,spec))))



(defun sly-disassemble-symbol (symbol-name)
  "Display the disassembly for SYMBOL-NAME."
  (interactive (list (sly-read-symbol-name "Disassemble: ")))
  (sly-eval-describe `(swank:disassemble-form ,(concat "'" symbol-name))))

(defun sly-undefine-function (symbol-name)
  "Unbind the function slot of SYMBOL-NAME."
  (interactive (list (sly-read-symbol-name "fmakunbound: " t)))
  (sly-eval-async `(swank:undefine-function ,symbol-name)
    (lambda (result) (message "%s" result))))

(defun sly-unintern-symbol (symbol-name package)
  "Unintern the symbol given with SYMBOL-NAME PACKAGE."
  (interactive (list (sly-read-symbol-name "Unintern symbol: " t)
                     (sly-read-package-name "from package: "
                                              (sly-current-package))))
  (sly-eval-async `(swank:unintern-symbol ,symbol-name ,package)
    (lambda (result) (message "%s" result))))

(defun sly-delete-package (package-name)
  "Delete the package with name PACKAGE-NAME."
  (interactive (list (sly-read-package-name "Delete package: "
                                              (sly-current-package))))
  (sly-eval-async `(cl:delete-package
                      (swank::guess-package ,package-name))))

(defun sly-load-file (filename)
  "Load the Lisp file FILENAME."
  (interactive (list
		(read-file-name "Load file: " nil nil
				nil (if (buffer-file-name)
                                        (file-name-nondirectory
                                         (buffer-file-name))))))
  (let ((lisp-filename (sly-to-lisp-filename (expand-file-name filename))))
    (sly-eval-with-transcript `(swank:load-file ,lisp-filename))))

(defvar sly-change-directory-hooks nil
  "Hook run by `sly-change-directory'.
The functions are called with the new (absolute) directory.")

(defun sly-change-directory (directory)
  "Make DIRECTORY become Lisp's current directory.
Return whatever swank:set-default-directory returns."
  (let ((dir (expand-file-name directory)))
    (prog1 (sly-eval `(swank:set-default-directory
                         ,(sly-to-lisp-filename dir)))
      (sly-with-connection-buffer nil (cd-absolute dir))
      (run-hook-with-args 'sly-change-directory-hooks dir))))

(defun sly-cd (directory)
  "Make DIRECTORY become Lisp's current directory.
Return whatever swank:set-default-directory returns."
  (interactive (list (read-directory-name "Directory: " nil nil t)))
  (message "default-directory: %s" (sly-change-directory directory)))

(defun sly-pwd ()
  "Show Lisp's default directory."
  (interactive)
  (message "Directory %s" (sly-eval `(swank:default-directory))))


;;;; Profiling

(defun sly-toggle-profile-fdefinition (fname-string)
  "Toggle profiling for FNAME-STRING."
  (interactive (list (sly-read-from-minibuffer
                      "(Un)Profile: "
                      (sly-symbol-at-point))))
  (sly-eval-async `(swank:toggle-profile-fdefinition ,fname-string)
    (lambda (r) (message "%s" r))))

(defun sly-unprofile-all ()
  "Unprofile all functions."
  (interactive)
  (sly-eval-async '(swank:unprofile-all)
    (lambda (r) (message "%s" r))))

(defun sly-profile-report ()
  "Print profile report."
  (interactive)
  (sly-eval-with-transcript '(swank:profile-report)))

(defun sly-profile-reset ()
  "Reset profile counters."
  (interactive)
  (sly-eval-async (sly-eval `(swank:profile-reset))
    (lambda (r) (message "%s" r))))

(defun sly-profiled-functions ()
  "Return list of names of currently profiled functions."
  (interactive)
  (sly-eval-async `(swank:profiled-functions)
    (lambda (r) (message "%s" r))))

(defun sly-profile-package (package callers methods)
  "Profile all functions in PACKAGE.
If CALLER is non-nil names have counts of the most common calling
functions recorded.
If METHODS is non-nil, profile all methods of all generic function
having names in the given package."
  (interactive (list (sly-read-package-name "Package: ")
                     (y-or-n-p "Record the most common callers? ")
                     (y-or-n-p "Profile methods? ")))
  (sly-eval-async `(swank:swank-profile-package ,package ,callers ,methods)
    (lambda (r) (message "%s" r))))

(defun sly-profile-by-substring (substring &optional package)
  "Profile all functions which names contain SUBSTRING.
If PACKAGE is NIL, then search in all packages."
  (interactive (list
                (sly-read-from-minibuffer
                 "Profile by matching substring: "
                 (sly-symbol-at-point))
                (sly-read-package-name "Package (RET for all packages): ")))
  (let ((package (unless (equal package "") package)))
    (sly-eval-async `(swank:profile-by-substring ,substring ,package)
      (lambda (r) (message "%s" r)) )))

;;;; Documentation

(defvar sly-documentation-lookup-function
  'sly-hyperspec-lookup)

(defun sly-documentation-lookup ()
  "Generalized documentation lookup. Defaults to hyperspec lookup."
  (interactive)
  (call-interactively sly-documentation-lookup-function))

(defun sly-hyperspec-lookup (symbol-name)
  "A wrapper for `hyperspec-lookup'"
  (interactive (list (common-lisp-hyperspec-read-symbol-name
                      (sly-symbol-at-point))))
  (hyperspec-lookup symbol-name))

(defun sly-describe-symbol (symbol-name)
  "Describe the symbol at point."
  (interactive (list (sly-read-symbol-name "Describe symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (sly-eval-describe `(swank:describe-symbol ,symbol-name)))

(defun sly-documentation (symbol-name)
  "Display function- or symbol-documentation for SYMBOL-NAME."
  (interactive (list (sly-read-symbol-name "Documentation for symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (sly-eval-describe
   `(swank:documentation-symbol ,symbol-name)))

(defun sly-describe-function (symbol-name)
  (interactive (list (sly-read-symbol-name "Describe symbol's function: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (sly-eval-describe `(swank:describe-function ,symbol-name)))

(defface sly-apropos-symbol
  '((t (:inherit bold)))
  "Face for the symbol name in Apropos output."
  :group 'sly)

(defface sly-apropos-label
  '((t (:inherit italic)))
  "Face for label (`Function', `Variable' ...) in Apropos output."
  :group 'sly)

(defun sly-apropos-summary (string case-sensitive-p package only-external-p)
  "Return a short description for the performed apropos search."
  (concat (if case-sensitive-p "Case-sensitive " "")
          "Apropos for "
          (format "%S" string)
          (if package (format " in package %S" package) "")
          (if only-external-p " (external symbols only)" "")))

(defun sly-apropos (string &optional only-external-p package
                             case-sensitive-p)
  "Show all bound symbols whose names match STRING. With prefix
arg, you're interactively asked for parameters of the search."
  (interactive
   (if current-prefix-arg
       (list (read-string "SLY Apropos: ")
             (y-or-n-p "External symbols only? ")
             (let ((pkg (sly-read-package-name "Package: ")))
               (if (string= pkg "") nil pkg))
             (y-or-n-p "Case-sensitive? "))
     (list (read-string "SLY Apropos: ") t nil nil)))
  (let ((buffer-package (or package (sly-current-package))))
    (sly-eval-async
        `(swank:apropos-list-for-emacs ,string ,only-external-p
                                       ,case-sensitive-p ',package)
      (sly-rcurry #'sly-show-apropos string buffer-package
                  (sly-apropos-summary string case-sensitive-p
                                       buffer-package only-external-p)))))

(defun sly-apropos-all ()
  "Shortcut for (sly-apropos <string> nil nil)"
  (interactive)
  (sly-apropos (read-string "SLY Apropos: ") nil nil))

(defun sly-apropos-package (package &optional internal)
  "Show apropos listing for symbols in PACKAGE.
With prefix argument include internal symbols."
  (interactive (list (let ((pkg (sly-read-package-name "Package: ")))
                       (if (string= pkg "") (sly-current-package) pkg))
                     current-prefix-arg))
  (sly-apropos "" (not internal) package))

(autoload 'apropos-mode "apropos")
(defun sly-show-apropos (plists string package summary)
  (if (null plists)
      (message "No apropos matches for %S" string)
    (sly-with-popup-buffer ((sly-buffer-name :apropos)
                              :package package :connection t
                              :mode 'apropos-mode)
      (if (boundp 'header-line-format)
          (setq header-line-format summary)
        (insert summary "\n\n"))
      (sly-set-truncate-lines)
      (sly-print-apropos plists)
      (set-syntax-table lisp-mode-syntax-table)
      (goto-char (point-min)))))

(defvar sly-apropos-namespaces
  '((:variable "Variable")
    (:function "Function")
    (:generic-function "Generic Function")
    (:macro "Macro")
    (:special-operator "Special Operator")
    (:setf "Setf")
    (:type "Type")
    (:class "Class")
    (:alien-type "Alien type")
    (:alien-struct "Alien struct")
    (:alien-union "Alien type")
    (:alien-enum "Alien enum")))

(defun sly-print-apropos (plists)
  (dolist (plist plists)
    (let ((designator (plist-get plist :designator)))
      (cl-assert designator)
      (sly-insert-propertized `(face sly-apropos-symbol) designator))
    (terpri)
    (cl-loop for (prop value) on plist by #'cddr
             unless (eq prop :designator) do
             (let ((namespace (cadr (or (assq prop sly-apropos-namespaces)
                                        (error "Unknown property: %S" prop))))
                   (start (point)))
               (princ "  ")
               (sly-insert-propertized `(face sly-apropos-label) namespace)
               (princ ": ")
               (princ (cl-etypecase value
                        (string value)
                        ((member nil :not-documented) "(not documented)")))
               (add-text-properties
                start (point)
                (list 'type prop 'action 'sly-call-describer
                      'button t 'apropos-label namespace
                      'item (plist-get plist :designator)))
               (terpri)))))

(defun sly-call-describer (arg)
  (let* ((pos (if (markerp arg) arg (point)))
         (type (get-text-property pos 'type))
         (item (get-text-property pos 'item)))
    (sly-eval-describe `(swank:describe-definition-for-emacs ,item ,type))))

(defun sly-info ()
  "Open SLY manual"
  (interactive)
  (let ((file (expand-file-name "doc/sly.info" sly-path)))
    (if (file-exists-p file)
        (info file)
      (message "No sly.info, run `make sly.info' in %s"
               (expand-file-name "doc/" sly-path)))))


;;;; XREF: cross-referencing

(defvar sly-xref-mode-map)

(define-derived-mode sly-xref-mode lisp-mode "Xref"
  "sly-xref-mode: Major mode for cross-referencing.
\\<sly-xref-mode-map>\
The most important commands:
\\[sly-xref-quit]	- Dismiss buffer.
\\[sly-show-xref]	- Display referenced source and keep xref window.
\\[sly-goto-xref]	- Jump to referenced source and dismiss xref window.

\\{sly-xref-mode-map}
\\{sly-popup-buffer-mode-map}
"
  (sly-popup-buffer-mode)
  (setq font-lock-defaults nil)
  (setq delayed-mode-hooks nil))

(sly-define-keys sly-xref-mode-map
  ((kbd "RET") 'sly-goto-xref)
  ((kbd "SPC") 'sly-goto-xref)
  ("v" 'sly-show-xref)
  ("n" 'sly-xref-next-line)
  ("p" 'sly-xref-prev-line)
  ("\C-c\C-c" 'sly-recompile-xref)
  ("\C-c\C-k" 'sly-recompile-all-xrefs)
  ("\M-," 'sly-xref-retract)
  ([remap next-line] 'sly-xref-next-line)
  ([remap previous-line] 'sly-xref-prev-line)
  )

(defun sly-next-line/not-add-newlines ()
  (interactive)
  (let ((next-line-add-newlines nil))
    (forward-line 1)))


;;;;; XREF results buffer and window management

(cl-defmacro sly-with-xref-buffer ((_xref-type _symbol &optional package)
                                     &body body)
  "Execute BODY in a xref buffer, then show that buffer."
  (declare (indent 1))
  `(sly-with-popup-buffer ((sly-buffer-name :xref)
                             :package ,package
                             :connection t
                             :select t
                             :mode 'sly-xref-mode)
     (sly-set-truncate-lines)
     ,@body))

(defun sly-insert-xrefs (xref-alist)
  "Insert XREF-ALIST in the current-buffer.
XREF-ALIST is of the form ((GROUP . ((LABEL LOCATION) ...)) ...).
GROUP and LABEL are for decoration purposes.  LOCATION is a
source-location."
  (cl-loop for (group . refs) in xref-alist do
           (sly-insert-propertized '(face bold) group "\n")
           (cl-loop for (label location) in refs do
                    (sly-insert-propertized
                     (list 'sly-location location
                           'face 'font-lock-keyword-face)
                     "  " (sly-one-line-ify label) "\n")))
  ;; Remove the final newline to prevent accidental window-scrolling
  (backward-delete-char 1))

(defun sly-xref-next-line ()
  (interactive)
  (sly-xref-show-location (sly-search-property 'sly-location)))

(defun sly-xref-prev-line ()
  (interactive)
  (sly-xref-show-location (sly-search-property 'sly-location t)))

(defun sly-xref-show-location (loc)
  (cl-ecase (car loc)
    (:location (sly-show-source-location loc t))
    (:error (message "%s" (cadr loc)))
    ((nil))))

(defvar sly-next-location-function nil
  "Function to call for going to the next location.")

(defvar sly-previous-location-function nil
  "Function to call for going to the previous location.")

(defvar sly-xref-last-buffer nil
  "The most recent XREF results buffer.
This is used by `sly-goto-next-xref'")

(defun sly-show-xref-buffer (xrefs _type _symbol package)
  (sly-with-xref-buffer (_type _symbol package)
    (sly-insert-xrefs xrefs)
    (setq sly-next-location-function 'sly-goto-next-xref)
    (setq sly-previous-location-function 'sly-goto-previous-xref)
    (setq sly-xref-last-buffer (current-buffer))
    (goto-char (point-min))))

(defun sly-show-xrefs (xrefs type symbol package)
  "Show the results of an XREF query."
  (if (null xrefs)
      (message "No references found for %s." symbol)
    (sly-show-xref-buffer xrefs type symbol package)))


;;;;; XREF commands

(defun sly-who-calls (symbol)
  "Show all known callers of the function SYMBOL."
  (interactive (list (sly-read-symbol-name "Who calls: " t)))
  (sly-xref :calls symbol))

(defun sly-calls-who (symbol)
  "Show all known functions called by the function SYMBOL."
  (interactive (list (sly-read-symbol-name "Who calls: " t)))
  (sly-xref :calls-who symbol))

(defun sly-who-references (symbol)
  "Show all known referrers of the global variable SYMBOL."
  (interactive (list (sly-read-symbol-name "Who references: " t)))
  (sly-xref :references symbol))

(defun sly-who-binds (symbol)
  "Show all known binders of the global variable SYMBOL."
  (interactive (list (sly-read-symbol-name "Who binds: " t)))
  (sly-xref :binds symbol))

(defun sly-who-sets (symbol)
  "Show all known setters of the global variable SYMBOL."
  (interactive (list (sly-read-symbol-name "Who sets: " t)))
  (sly-xref :sets symbol))

(defun sly-who-macroexpands (symbol)
  "Show all known expanders of the macro SYMBOL."
  (interactive (list (sly-read-symbol-name "Who macroexpands: " t)))
  (sly-xref :macroexpands symbol))

(defun sly-who-specializes (symbol)
  "Show all known methods specialized on class SYMBOL."
  (interactive (list (sly-read-symbol-name "Who specializes: " t)))
  (sly-xref :specializes symbol))

(defun sly-list-callers (symbol-name)
  "List the callers of SYMBOL-NAME in a xref window."
  (interactive (list (sly-read-symbol-name "List callers: ")))
  (sly-xref :callers symbol-name))

(defun sly-list-callees (symbol-name)
  "List the callees of SYMBOL-NAME in a xref window."
  (interactive (list (sly-read-symbol-name "List callees: ")))
  (sly-xref :callees symbol-name))

;; FIXME: whats the call (sly-postprocess-xrefs result) good for?
(defun sly-xref (type symbol &optional continuation)
  "Make an XREF request to Lisp."
  (sly-eval-async
      `(swank:xref ',type ',symbol)
    (sly-rcurry (lambda (result type symbol package cont)
                    (sly-check-xref-implemented type result)
                    (let* ((_xrefs (sly-postprocess-xrefs result))
                           (file-alist (cadr (sly-analyze-xrefs result))))
                      (funcall (or cont 'sly-show-xrefs)
                               file-alist type symbol package)))
                  type
                  symbol
                  (sly-current-package)
                  continuation)))

(defun sly-check-xref-implemented (type xrefs)
  (when (eq xrefs :not-implemented)
    (error "%s is not implemented yet on %s."
           (sly-xref-type type)
           (sly-lisp-implementation-name))))

(defun sly-xref-type (type)
  (format "who-%s" (sly-cl-symbol-name type)))

(defun sly-xrefs (types symbol &optional continuation)
  "Make multiple XREF requests at once."
  (sly-eval-async
      `(swank:xrefs ',types ',symbol)
    #'(lambda (result)
        (funcall (or continuation
                     #'sly-show-xrefs)
                 (cl-loop for (key . val) in result
                          collect (cons (sly-xref-type key) val))
                 types symbol (sly-current-package)))))


;;;;; XREF navigation

(defun sly-xref-location-at-point ()
  (save-excursion
    ;; When the end of the last line is at (point-max) we can't find
    ;; the text property there. Going to bol avoids this problem.
    (beginning-of-line 1)
    (or (get-text-property (point) 'sly-location)
        (error "No reference at point."))))

(defun sly-xref-dspec-at-point ()
  (save-excursion
    (beginning-of-line 1)
    (with-syntax-table lisp-mode-syntax-table
      (forward-sexp)                    ; skip initial whitespaces
      (backward-sexp)
      (sly-sexp-at-point))))

(defun sly-all-xrefs ()
  (let ((xrefs nil))
    (save-excursion
      (goto-char (point-min))
      (while (ignore-errors (sly-next-line/not-add-newlines) t)
        (when-let (loc (get-text-property (point) 'sly-location))
          (let* ((dspec (sly-xref-dspec-at-point))
                 (xref  (make-sly-xref :dspec dspec :location loc)))
            (push xref xrefs)))))
    (nreverse xrefs)))

(defun sly-goto-xref ()
  "Goto the cross-referenced location at point."
  (interactive)
  (sly-show-xref)
  (quit-window))

(defun sly-show-xref ()
  "Display the xref at point in the other window."
  (interactive)
  (let ((location (sly-xref-location-at-point)))
    (sly-show-source-location location)))

(defun sly-goto-next-xref (&optional backward)
  "Goto the next cross-reference location."
  (if (not (buffer-live-p sly-xref-last-buffer))
      (error "No XREF buffer alive.")
    (cl-multiple-value-bind (location pos)
        (with-current-buffer sly-xref-last-buffer
          (cl-values (sly-search-property 'sly-location backward)
                     (point)))
      (cond ((sly-location-p location)
             (sly-pop-to-location location)
             ;; We do this here because changing the location can take
             ;; a while when Emacs needs to read a file from disk.
             (with-current-buffer sly-xref-last-buffer
               (goto-char pos)
               (sly-highlight-line 0.35)))
            ((null location)
             (message (if backward "No previous xref" "No next xref.")))
            (t ; error location
             (sly-goto-next-xref backward))))))

(defun sly-goto-previous-xref ()
  "Goto the previous cross-reference location."
  (sly-goto-next-xref t))

(defun sly-search-property (prop &optional backward prop-value-fn)
  "Search the next text range where PROP is non-nil.
Return the value of PROP.
If BACKWARD is non-nil, search backward.
If PROP-VALUE-FN is non-nil use it to extract PROP's value."
  (let ((next-candidate (if backward
                            #'previous-single-char-property-change
                          #'next-single-char-property-change))
        (prop-value-fn  (or prop-value-fn
                            (lambda ()
                              (get-text-property (point) prop))))
        (start (point))
        (prop-value))
    (while (progn
             (goto-char (funcall next-candidate (point) prop))
             (not (or (setq prop-value (funcall prop-value-fn))
                      (eobp)
                      (bobp)))))
    (cond (prop-value)
          (t (goto-char start) nil))))

(defun sly-next-location ()
  "Go to the next location, depending on context.
When displaying XREF information, this goes to the next reference."
  (interactive)
  (when (null sly-next-location-function)
    (error "No context for finding locations."))
  (funcall sly-next-location-function))

(defun sly-previous-location ()
  "Go to the previous location, depending on context.
When displaying XREF information, this goes to the previous reference."
  (interactive)
  (when (null sly-previous-location-function)
    (error "No context for finding locations."))
  (funcall sly-previous-location-function))

(defun sly-recompile-xref (&optional raw-prefix-arg)
  (interactive "P")
  (let ((sly-compilation-policy (sly-compute-policy raw-prefix-arg)))
    (let ((location (sly-xref-location-at-point))
          (dspec    (sly-xref-dspec-at-point)))
      (sly-recompile-locations
       (list location)
       (sly-rcurry #'sly-xref-recompilation-cont
                     (list dspec) (current-buffer))))))

(defun sly-recompile-all-xrefs (&optional raw-prefix-arg)
  (interactive "P")
  (let ((sly-compilation-policy (sly-compute-policy raw-prefix-arg)))
    (let ((dspecs) (locations))
      (dolist (xref (sly-all-xrefs))
        (when (sly-xref-has-location-p xref)
          (push (sly-xref.dspec xref) dspecs)
          (push (sly-xref.location xref) locations)))
      (sly-recompile-locations
       locations
       (sly-rcurry #'sly-xref-recompilation-cont
                     dspecs (current-buffer))))))

(defun sly-xref-recompilation-cont (results dspecs buffer)
  ;; Extreme long-windedness to insert status of recompilation;
  ;; sometimes Elisp resembles more of an Ewwlisp.

  ;; FIXME: Should probably throw out the whole recompilation cruft
  ;; anyway.  -- helmut
  ;; TODO: next iteration of fixme cleanup this is going in a contrib -- jt
  (with-current-buffer buffer
    (sly-compilation-finished (sly-aggregate-compilation-results results))
    (save-excursion
      (sly-xref-insert-recompilation-flags
       dspecs (cl-loop for r in results collect
                       (or (sly-compilation-result.successp r)
                           (and (sly-compilation-result.notes r)
                                :complained)))))))

(defun sly-aggregate-compilation-results (results)
  `(:compilation-result
    ,(cl-reduce #'append (mapcar #'sly-compilation-result.notes results))
    ,(cl-every #'sly-compilation-result.successp results)
    ,(cl-reduce #'+ (mapcar #'sly-compilation-result.duration results))))

(defun sly-xref-insert-recompilation-flags (dspecs compilation-results)
  (let* ((buffer-read-only nil)
         (max-column (sly-column-max)))
    (goto-char (point-min))
    (cl-loop for dspec in dspecs
             for result in compilation-results
             do (save-excursion
                  (cl-loop for dspec2 = (progn (search-forward dspec)
                                               (sly-xref-dspec-at-point))
                           until (equal dspec2 dspec))
                  (end-of-line) ; skip old status information.
                  (insert-char ?\  (1+ (- max-column (current-column))))
                  (insert (format "[%s]"
                                  (cl-case result
                                    ((t)   :success)
                                    ((nil) :failure)
                                    (t     result))))))))


;;;; Macroexpansion

(define-minor-mode sly-macroexpansion-minor-mode
  "SLY mode for macroexpansion"
  nil
  " Macroexpand"
  '(("g" . sly-macroexpand-again)))

(cl-macrolet ((remap (from to)
                     `(dolist (mapping
                               (where-is-internal ,from sly-mode-map))
                        (define-key sly-macroexpansion-minor-mode-map
                                    mapping ,to))))
  (remap 'sly-macroexpand-1 'sly-macroexpand-1-inplace)
  (remap 'sly-macroexpand-all 'sly-macroexpand-all-inplace)
  (remap 'sly-compiler-macroexpand-1 'sly-compiler-macroexpand-1-inplace)
  (remap 'sly-expand-1
         'sly-expand-1-inplace)
  (remap 'advertised-undo 'sly-macroexpand-undo)
  (remap 'undo 'sly-macroexpand-undo))

(defun sly-macroexpand-undo (&optional arg)
  (interactive)
  ;; Emacs 22.x introduced `undo-only' which
  ;; works by binding `undo-no-redo' to t. We do
  ;; it this way so we don't break prior Emacs
  ;; versions.
  (cl-macrolet ((undo-only (arg) `(let ((undo-no-redo t)) (undo ,arg))))
    (let ((inhibit-read-only t))
      (when (fboundp 'sly-remove-edits)
        (sly-remove-edits (point-min) (point-max)))
      (undo-only arg))))

(defvar sly-eval-macroexpand-expression nil
  "Specifies the last macroexpansion preformed.
This variable specifies both what was expanded and how.")

(defun sly-eval-macroexpand (expander &optional string)
  (let ((string (or string (sly-sexp-at-point))))
    (setq sly-eval-macroexpand-expression `(,expander ,string))
    (sly-eval-async sly-eval-macroexpand-expression
      #'sly-initialize-macroexpansion-buffer)))

(defun sly-macroexpand-again ()
  "Reperform the last macroexpansion."
  (interactive)
  (sly-eval-async sly-eval-macroexpand-expression
    (sly-rcurry #'sly-initialize-macroexpansion-buffer
                  (current-buffer))))

(defun sly-initialize-macroexpansion-buffer (expansion &optional buffer)
  (pop-to-buffer (or buffer (sly-create-macroexpansion-buffer)))
  (setq buffer-undo-list nil) ; Get rid of undo information from
                                        ; previous expansions.
  (let ((inhibit-read-only t)
        (buffer-undo-list t)) ; Make the initial insertion not be undoable.
    (erase-buffer)
    (insert expansion)
    (goto-char (point-min))
    (font-lock-fontify-buffer)))

(defun sly-create-macroexpansion-buffer ()
  (let ((name (sly-buffer-name :macroexpansion)))
    (sly-with-popup-buffer (name :package t :connection t
                                   :mode 'lisp-mode)
      (sly-macroexpansion-minor-mode 1)
      (setq font-lock-keywords-case-fold-search t)
      (current-buffer))))

(defun sly-eval-macroexpand-inplace (expander)
  "Substitute the sexp at point with its macroexpansion.

NB: Does not affect sly-eval-macroexpand-expression"
  (interactive)
  (let* ((bounds (or (sly-bounds-of-sexp-at-point)
                     (error "No sexp at point"))))
    (lexical-let* ((start (copy-marker (car bounds)))
                   (end (copy-marker (cdr bounds)))
                   (point (point))
                   (package (sly-current-package))
                   (buffer (current-buffer)))
      (sly-eval-async
          `(,expander ,(buffer-substring-no-properties start end))
        (lambda (expansion)
          (with-current-buffer buffer
            (let ((buffer-read-only nil))
              (when (fboundp 'sly-remove-edits)
                (sly-remove-edits (point-min) (point-max)))
              (goto-char start)
              (delete-region start end)
              (sly-insert-indented expansion)
              (goto-char point))))))))

(defun sly-macroexpand-1 (&optional repeatedly)
  "Display the macro expansion of the form at point.
The form is expanded with CL:MACROEXPAND-1 or, if a prefix
argument is given, with CL:MACROEXPAND."
  (interactive "P")
  (sly-eval-macroexpand
   (if repeatedly 'swank:swank-macroexpand 'swank:swank-macroexpand-1)))

(defun sly-macroexpand-1-inplace (&optional repeatedly)
  (interactive "P")
  (sly-eval-macroexpand-inplace
   (if repeatedly 'swank:swank-macroexpand 'swank:swank-macroexpand-1)))

(defun sly-macroexpand-all ()
  "Display the recursively macro expanded sexp at point."
  (interactive)
  (sly-eval-macroexpand 'swank:swank-macroexpand-all))

(defun sly-macroexpand-all-inplace ()
  "Display the recursively macro expanded sexp at point."
  (interactive)
  (sly-eval-macroexpand-inplace 'swank:swank-macroexpand-all))

(defun sly-compiler-macroexpand-1 (&optional repeatedly)
  "Display the compiler-macro expansion of sexp at point."
  (interactive "P")
  (sly-eval-macroexpand
   (if repeatedly
       'swank:swank-compiler-macroexpand
     'swank:swank-compiler-macroexpand-1)))

(defun sly-compiler-macroexpand-1-inplace (&optional repeatedly)
  "Display the compiler-macro expansion of sexp at point."
  (interactive "P")
  (sly-eval-macroexpand-inplace
   (if repeatedly
       'swank:swank-compiler-macroexpand
     'swank:swank-compiler-macroexpand-1)))

(defun sly-expand-1 (&optional repeatedly)
  "Display the macro expansion of the form at point.
The form is expanded with CL:MACROEXPAND-1 or, if a prefix
argument is given, with CL:MACROEXPAND."
  (interactive "P")
  (sly-eval-macroexpand
   (if repeatedly
       'swank:swank-expand
     'swank:swank-expand-1)))

(defun sly-expand-1-inplace (&optional repeatedly)
  "Display the macro expansion of the form at point.
The form is expanded with CL:MACROEXPAND-1 or, if a prefix
argument is given, with CL:MACROEXPAND."
  (interactive "P")
  (sly-eval-macroexpand-inplace
   (if repeatedly
       'swank:swank-expand
     'swank:swank-expand-1)))

(defun sly-format-string-expand ()
  "Expand the format-string at point and display it."
  (interactive)
  (sly-eval-macroexpand 'swank:swank-format-string-expand
                          (sly-string-at-point-or-error)))


;;;; Subprocess control

(defun sly-interrupt ()
  "Interrupt Lisp."
  (interactive)
  (cond ((sly-use-sigint-for-interrupt) (sly-send-sigint))
        (t (sly-dispatch-event `(:emacs-interrupt ,sly-current-thread)))))

(defun sly-quit ()
  (error "Not implemented properly.  Use `sly-interrupt' instead."))

(defun sly-quit-lisp (&optional kill)
  "Quit lisp, kill the inferior process and associated buffers."
  (interactive "P")
  (sly-quit-lisp-internal (sly-connection) 'sly-quit-sentinel kill))

(defun sly-quit-lisp-internal (connection sentinel kill)
  (let ((sly-dispatching-connection connection))
    (sly-eval-async '(swank:quit-lisp))
    (let* ((process (sly-inferior-process connection)))
      (set-process-filter connection  nil)
      (set-process-sentinel connection sentinel)
      (when (and kill process)
        (sleep-for 0.2)
        (unless (memq (process-status process) '(exit signal))
          (kill-process process))))))

(defun sly-quit-sentinel (process _message)
  (cl-assert (process-status process) 'closed)
  (let* ((inferior (sly-inferior-process process))
         (inferior-buffer (if inferior (process-buffer inferior))))
    (when inferior (delete-process inferior))
    (when inferior-buffer (kill-buffer inferior-buffer))
    (sly-net-close process)
    (message "Connection closed.")))


;;;; Debugger (SLDB)

(defvar sldb-hook nil
  "Hook run on entry to the debugger.")

(defcustom sldb-initial-restart-limit 6
  "Maximum number of restarts to display initially."
  :group 'sly-debugger
  :type 'integer)


;;;;; Local variables in the debugger buffer

;; Small helper.
(defun sly-make-variables-buffer-local (&rest variables)
  (mapcar #'make-variable-buffer-local variables))

(sly-make-variables-buffer-local
 (defvar sldb-condition nil
   "A list (DESCRIPTION TYPE) describing the condition being debugged.")

 (defvar sldb-restarts nil
   "List of (NAME DESCRIPTION) for each available restart.")

 (defvar sldb-level nil
   "Current debug level (recursion depth) displayed in buffer.")

 (defvar sldb-backtrace-start-marker nil
   "Marker placed at the first frame of the backtrace.")

 (defvar sldb-restart-list-start-marker nil
   "Marker placed at the first restart in the restart list.")

 (defvar sldb-continuations nil
   "List of ids for pending continuation."))

;;;;; SLDB macros

;; some macros that we need to define before the first use

(defmacro sldb-in-face (name string)
  "Return STRING propertised with face sldb-NAME-face."
  (declare (indent 1))
  (let ((facename (intern (format "sldb-%s-face" (symbol-name name))))
	(var (cl-gensym "string")))
    `(let ((,var ,string))
       (sly-add-face ',facename ,var)
       ,var)))


;;;;; sldb-mode

(defvar sldb-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    ;; We give < and > parenthesis syntax, so that #< ... > is treated
    ;; as a balanced expression.  This enables autodoc-mode to match
    ;; #<unreadable> actual arguments in the backtraces with formal
    ;; arguments of the function.  (For Lisp mode, this is not
    ;; desirable, since we do not wish to get a mismatched paren
    ;; highlighted everytime we type < or >.)
    (modify-syntax-entry ?< "(" table)
    (modify-syntax-entry ?> ")" table)
    table)
  "Syntax table for SLDB mode.")

(define-derived-mode sldb-mode fundamental-mode "sldb"
  "Superior lisp debugger mode.
In addition to ordinary SLY commands, the following are
available:\\<sldb-mode-map>

Commands to examine the selected frame:
   \\[sldb-toggle-details]   - toggle details (local bindings, CATCH tags)
   \\[sldb-show-source]   - view source for the frame
   \\[sldb-eval-in-frame]   - eval in frame
   \\[sldb-pprint-eval-in-frame]   - eval in frame, pretty-print result
   \\[sldb-disassemble]   - disassemble
   \\[sldb-inspect-in-frame]   - inspect

Commands to invoke restarts:
   \\[sldb-quit]   - quit
   \\[sldb-abort]   - abort
   \\[sldb-continue]   - continue
   \\[sldb-invoke-restart-0]-\\[sldb-invoke-restart-9] - restart shortcuts
   \\[sldb-invoke-restart-by-name]   - invoke restart by name

Commands to navigate frames:
   \\[sldb-down]   - down
   \\[sldb-up]   - up
   \\[sldb-details-down] - down, with details
   \\[sldb-details-up] - up, with details
   \\[sldb-cycle] - cycle between restarts & backtrace
   \\[sldb-beginning-of-backtrace]   - beginning of backtrace
   \\[sldb-end-of-backtrace]   - end of backtrace

Miscellaneous commands:
   \\[sldb-restart-frame]   - restart frame
   \\[sldb-return-from-frame]   - return from frame
   \\[sldb-step]   - step
   \\[sldb-break-with-default-debugger]   - switch to native debugger
   \\[sldb-break-with-system-debugger]   - switch to system debugger (gdb)
   \\[sly-interactive-eval]   - eval
   \\[sldb-inspect-condition]   - inspect signalled condition

Full list of commands:

\\{sldb-mode-map}"
  (erase-buffer)
  (set-syntax-table sldb-mode-syntax-table)
  (sly-set-truncate-lines)
  ;; Make original sly-connection "sticky" for SLDB commands in this buffer
  (setq sly-buffer-connection (sly-connection))
  (sly-mode))

(sly-define-keys sldb-mode-map
  ((kbd "RET") 'sldb-default-action)
  ("\C-m"      'sldb-default-action)
  ([return] 'sldb-default-action)
  ([mouse-2]  'sldb-default-action/mouse)
  ([follow-link] 'mouse-face)
  ("\C-i" 'sldb-cycle)
  ("h"    'describe-mode)
  ("v"    'sldb-show-source)
  ("e"    'sldb-eval-in-frame)
  ("d"    'sldb-pprint-eval-in-frame)
  ("D"    'sldb-disassemble)
  ("i"    'sldb-inspect-in-frame)
  ("n"    'sldb-down)
  ("p"    'sldb-up)
  ("\M-n" 'sldb-details-down)
  ("\M-p" 'sldb-details-up)
  ("<"    'sldb-beginning-of-backtrace)
  (">"    'sldb-end-of-backtrace)
  ("t"    'sldb-toggle-details)
  ("r"    'sldb-restart-frame)
  ("I"    'sldb-invoke-restart-by-name)
  ("R"    'sldb-return-from-frame)
  ("c"    'sldb-continue)
  ("s"    'sldb-step)
  ("x"    'sldb-next)
  ("o"    'sldb-out)
  ("b"    'sldb-break-on-return)
  ("a"    'sldb-abort)
  ("q"    'sldb-quit)
  ("A"    'sldb-break-with-system-debugger)
  ("B"    'sldb-break-with-default-debugger)
  ("P"    'sldb-print-condition)
  ("C"    'sldb-inspect-condition)
  (":"    'sly-interactive-eval)
  ("\C-c\C-c" 'sldb-recompile-frame-source))

;; Keys 0-9 are shortcuts to invoke particular restarts.
(dotimes (number 10)
  (let ((fname (intern (format "sldb-invoke-restart-%S" number)))
        (docstring (format "Invoke restart numbered %S." number)))
    (eval `(defun ,fname ()
             ,docstring
             (interactive)
             (sldb-invoke-restart ,number)))
    (define-key sldb-mode-map (number-to-string number) fname)))


;;;;; SLDB buffer creation & update

(defun sldb-buffers (&optional connection)
  "Return a list of all sldb buffers (belonging to CONNECTION.)"
  (if connection
      (sly-filter-buffers (lambda ()
                              (and (eq sly-buffer-connection connection)
                                   (eq major-mode 'sldb-mode))))
    (sly-filter-buffers (lambda () (eq major-mode 'sldb-mode)))))

(defun sldb-find-buffer (thread &optional connection)
  (let ((connection (or connection (sly-connection))))
    (cl-find-if (lambda (buffer)
                  (with-current-buffer buffer
                    (and (eq sly-buffer-connection connection)
                         (eq sly-current-thread thread))))
                (sldb-buffers))))

(defun sldb-get-default-buffer ()
  "Get a sldb buffer.
The chosen buffer the default connection's it if exists."
  (car (sldb-buffers sly-default-connection)))

(defun sldb-get-buffer (thread &optional connection)
  "Find or create a sldb-buffer for THREAD."
  (let ((connection (or connection (sly-connection))))
    (or (sldb-find-buffer thread connection)
        (let ((name (format "*sldb %s/%s*" (sly-connection-name) thread)))
          (with-current-buffer (generate-new-buffer name)
            (setq sly-buffer-connection connection
                  sly-current-thread thread)
            (current-buffer))))))

(defun sldb-debugged-continuations (connection)
  "Return the all debugged continuations for CONNECTION across SLDB buffers."
  (cl-loop for b in (sldb-buffers)
           append (with-current-buffer b
                    (and (eq sly-buffer-connection connection)
                         sldb-continuations))))

(defun sldb-setup (thread level condition restarts frames conts)
  "Setup a new SLDB buffer.
CONDITION is a string describing the condition to debug.
RESTARTS is a list of strings (NAME DESCRIPTION) for each available restart.
FRAMES is a list (NUMBER DESCRIPTION &optional PLIST) describing the initial
portion of the backtrace. Frames are numbered from 0.
CONTS is a list of pending Emacs continuations."
  (with-current-buffer (sldb-get-buffer thread)
    (cl-assert (if (equal sldb-level level)
                   (equal sldb-condition condition)
                 t)
               () "Bug: sldb-level is equal but condition differs\n%s\n%s"
               sldb-condition condition)
    (unless (equal sldb-level level)
      (setq buffer-read-only nil)
      (sldb-mode)
      (setq sly-current-thread thread)
      (setq sldb-level level)
      (setq mode-name (format "sldb[%d]" sldb-level))
      (setq sldb-condition condition)
      (setq sldb-restarts restarts)
      (setq sldb-continuations conts)
      (sldb-insert-condition condition)
      (insert "\n\n" (sldb-in-face section "Restarts:") "\n")
      (setq sldb-restart-list-start-marker (point-marker))
      (sldb-insert-restarts restarts 0 sldb-initial-restart-limit)
      (insert "\n" (sldb-in-face section "Backtrace:") "\n")
      (setq sldb-backtrace-start-marker (point-marker))
      (save-excursion
        (if frames
            (sldb-insert-frames (sldb-prune-initial-frames frames) t)
          (insert "[No backtrace]")))
      (run-hooks 'sldb-hook)
      (set-syntax-table lisp-mode-syntax-table))
    (pop-to-buffer (current-buffer) '(sldb--display-in-prev-sldb-window))
    (set-window-parameter (selected-window) 'sldb (current-buffer))
    (sly-recenter (point-min))
    (setq buffer-read-only t)
    (when (and sly-stack-eval-tags
               ;; (y-or-n-p "Enter recursive edit? ")
               )
      (message "Entering recursive edit..")
      (recursive-edit))))

(defun sldb--display-in-prev-sldb-window (buffer _alist)
  (let ((window
         (get-window-with-predicate
          #'(lambda (w)
              (let ((value (window-parameter w 'sldb)))
                (and value
                     (not (buffer-live-p value))))))))
    (when window
      (display-buffer-record-window 'reuse window buffer)
      (set-window-buffer window buffer)
      window)))

(defun sldb-activate (thread level select)
  "Display the debugger buffer for THREAD.
If LEVEL isn't the same as in the buffer reinitialize the buffer."
  (or (let ((buffer (sldb-find-buffer thread)))
        (when buffer
          (with-current-buffer buffer
            (when (equal sldb-level level)
              (when select (pop-to-buffer (current-buffer)))
              t))))
      (sldb-reinitialize thread level)))

(defun sldb-reinitialize (thread level)
  (sly-rex (thread level)
      ('(swank:debugger-info-for-emacs 0 10)
       nil thread)
    ((:ok result)
     (apply #'sldb-setup thread level result))))

(defun sldb-exit (thread _level &optional stepping)
  "Exit from the debug level LEVEL."
  (when-let (sldb (sldb-find-buffer thread))
    (with-current-buffer sldb
      (cond (stepping
             (setq sldb-level nil)
             (run-with-timer 0.4 nil 'sldb-close-step-buffer sldb))
            ((not (eq sldb (window-buffer (selected-window))))
             ;; A different window selection means an indirect,
             ;; non-interactive exit, we just kill the sldb buffer.
             (kill-buffer))
            (t
             (quit-window t))))))

(defun sldb-close-step-buffer (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (not sldb-level)
        (quit-window t)))))


;;;;;; SLDB buffer insertion

(defun sldb-insert-condition (condition)
  "Insert the text for CONDITION.
CONDITION should be a list (MESSAGE TYPE EXTRAS).
EXTRAS is currently used for the stepper."
  (cl-destructuring-bind (message type extras) condition
    (sly-insert-propertized '(sldb-default-action sldb-inspect-condition)
                              (sldb-in-face topline message)
                              "\n"
                              (sldb-in-face condition type))
    (sldb-dispatch-extras extras)))

(defvar sldb-extras-hooks)

(defun sldb-dispatch-extras (extras)
  ;; this is (mis-)used for the stepper
  (dolist (extra extras)
    (destructure-case extra
      ((:show-frame-source n)
       (sldb-show-frame-source n))
      (t
       (or (run-hook-with-args-until-success 'sldb-extras-hooks extra)
           ;;(error "Unhandled extra element:" extra)
           )))))

(defun sldb-insert-restarts (restarts start count)
  "Insert RESTARTS and add the needed text props
RESTARTS should be a list ((NAME DESCRIPTION) ...)."
  (let* ((len (length restarts))
         (end (if count (min (+ start count) len) len)))
    (cl-loop for (name string) in (cl-subseq restarts start end)
             for number from start
             do (sly-insert-propertized
                 `(,@nil restart ,number
                         sldb-default-action sldb-invoke-restart
                         mouse-face highlight)
                 " " (sldb-in-face restart-number (number-to-string number))
                 ": ["  (sldb-in-face restart-type name) "] "
                 (sldb-in-face restart string))
             (insert "\n"))
    (when (< end len)
      (let ((pos (point)))
        (sly-insert-propertized
         (list 'sldb-default-action
               (sly-rcurry #'sldb-insert-more-restarts restarts pos end))
         " --more--\n")))))

(defun sldb-insert-more-restarts (restarts position start)
  (goto-char position)
  (let ((inhibit-read-only t))
    (delete-region position (1+ (line-end-position)))
    (sldb-insert-restarts restarts start nil)))

(defun sldb-frame.string (frame)
  (cl-destructuring-bind (_ str &optional _) frame str))

(defun sldb-frame.number (frame)
  (cl-destructuring-bind (n _ &optional _) frame n))

(defun sldb-frame.plist (frame)
  (cl-destructuring-bind (_ _ &optional plist) frame plist))

(defun sldb-frame-restartable-p (frame)
  (and (plist-get (sldb-frame.plist frame) :restartable) t))

(defun sldb-prune-initial-frames (frames)
  "Return the prefix of FRAMES to initially present to the user.
Regexp heuristics are used to avoid showing SWANK-internal frames."
  (let* ((case-fold-search t)
         (rx "^\\([() ]\\|lambda\\)*swank\\>"))
    (or (cl-loop for frame in frames
                 until (string-match rx (sldb-frame.string frame))
                 collect frame)
        frames)))

(defun sldb-insert-frames (frames more)
  "Insert FRAMES into buffer.
If MORE is non-nil, more frames are on the Lisp stack."
  (mapc #'sldb-insert-frame frames)
  (when more
    (sly-insert-propertized
     `(,@nil sldb-default-action sldb-fetch-more-frames
             sldb-previous-frame-number
             ,(sldb-frame.number (cl-first (last frames)))
             point-entered sldb-fetch-more-frames
             start-open t
             face sldb-section-face
             mouse-face highlight)
     " --more--")
    (insert "\n")))

(defun sldb-compute-frame-face (frame)
  (if (sldb-frame-restartable-p frame)
      'sldb-restartable-frame-line-face
    'sldb-frame-line-face))

(defun sldb-insert-frame (frame &optional face)
  "Insert FRAME with FACE at point.
If FACE is nil, `sldb-compute-frame-face' is used to determine the face."
  (setq face (or face (sldb-compute-frame-face frame)))
  (let ((number (sldb-frame.number frame))
        (string (sldb-frame.string frame))
        (props `(frame ,frame sldb-default-action sldb-toggle-details)))
    (sly-propertize-region props
      (sly-propertize-region '(mouse-face highlight)
        (insert " " (sldb-in-face frame-label (format "%2d:" number)) " ")
        (sly-insert-indented
         (sly-add-face face string)))
      (insert "\n"))))

(defun sldb-fetch-more-frames (&rest _)
  "Fetch more backtrace frames.
Called on the `point-entered' text-property hook."
  (let ((inhibit-point-motion-hooks t)
        (inhibit-read-only t)
        (prev (get-text-property (point) 'sldb-previous-frame-number)))
    ;; we may be called twice, PREV is nil the second time
    (when prev
      (let* ((count 40)
             (from (1+ prev))
             (to (+ from count))
             (frames (sly-eval `(swank:backtrace ,from ,to)))
             (more (sly-length= frames count))
             (pos (point)))
        (delete-region (line-beginning-position) (point-max))
        (sldb-insert-frames frames more)
        (goto-char pos)))))


;;;;;; SLDB examining text props

(defun sldb-restart-at-point ()
  (or (get-text-property (point) 'restart)
      (error "No restart at point")))

(defun sldb-frame-number-at-point ()
  (let ((frame (get-text-property (point) 'frame)))
    (cond (frame (car frame))
	  (t (error "No frame at point")))))

(defun sldb-var-number-at-point ()
  (let ((var (get-text-property (point) 'var)))
    (cond (var var)
	  (t (error "No variable at point")))))

(defun sldb-previous-frame-number ()
  (save-excursion
    (sldb-backward-frame)
    (sldb-frame-number-at-point)))

(defun sldb-frame-details-visible-p ()
  (and (get-text-property (point) 'frame)
       (get-text-property (point) 'details-visible-p)))

(defun sldb-frame-region ()
  (sly-property-bounds 'frame))

(defun sldb-forward-frame ()
  (goto-char (next-single-char-property-change (point) 'frame)))

(defun sldb-backward-frame ()
  (when (> (point) sldb-backtrace-start-marker)
    (goto-char (previous-single-char-property-change
                (if (get-text-property (point) 'frame)
                    (car (sldb-frame-region))
                  (point))
                'frame
                nil sldb-backtrace-start-marker))))

(defun sldb-goto-last-frame ()
  (goto-char (point-max))
  (while (not (get-text-property (point) 'frame))
    (goto-char (previous-single-property-change (point) 'frame))
    ;; Recenter to bottom of the window; -2 to account for the
    ;; empty last line displayed in sldb buffers.
    (recenter -2)))

(defun sldb-beginning-of-backtrace ()
  "Goto the first frame."
  (interactive)
  (goto-char sldb-backtrace-start-marker))


;;;;;; SLDB recenter & redisplay
;; not sure yet, whether this is a good idea.
;;
;; jt: seconded. Only `sldb-show-frame-details' and
;; `sldb-hide-frame-details' use this. They could avoid it by not
;; removing and reinserting the frame's name line.
(defmacro sly-save-coordinates (origin &rest body)
  "Restore line and column relative to ORIGIN, after executing BODY.

This is useful if BODY deletes and inserts some text but we want to
preserve the current row and column as closely as possible."
  (let ((base (make-symbol "base"))
        (goal (make-symbol "goal"))
        (mark (make-symbol "mark")))
    `(let* ((,base ,origin)
            (,goal (sly-coordinates ,base))
            (,mark (point-marker)))
       (set-marker-insertion-type ,mark t)
       (prog1 (save-excursion ,@body)
         (sly-restore-coordinate ,base ,goal ,mark)))))

(put 'sly-save-coordinates 'lisp-indent-function 1)

(defun sly-coordinates (origin)
  ;; Return a pair (X . Y) for the column and line distance to ORIGIN.
  (let ((y (sly-count-lines origin (point)))
        (x (save-excursion
             (- (current-column)
                (progn (goto-char origin) (current-column))))))
    (cons x y)))

(defun sly-restore-coordinate (base goal limit)
  ;; Move point to GOAL. Coordinates are relative to BASE.
  ;; Don't move beyond LIMIT.
  (save-restriction
    (narrow-to-region base limit)
    (goto-char (point-min))
    (let ((col (current-column)))
      (forward-line (cdr goal))
      (when (and (eobp) (bolp) (not (bobp)))
        (backward-char))
      (move-to-column (+ col (car goal))))))

(defun sly-count-lines (start end)
  "Return the number of lines between START and END.
This is 0 if START and END at the same line."
  (- (count-lines start end)
     (if (save-excursion (goto-char end) (bolp)) 0 1)))


;;;;; SLDB commands

(defun sldb-default-action ()
  "Invoke the action at point."
  (interactive)
  (let ((fn (get-text-property (point) 'sldb-default-action)))
    (if fn (funcall fn))))

(defun sldb-default-action/mouse (event)
  "Invoke the action pointed at by the mouse."
  (interactive "e")
  (cl-destructuring-bind (_mouse-1 (_w pos &rest ignore)) event
    (save-excursion
      (goto-char pos)
      (let ((fn (get-text-property (point) 'sldb-default-action)))
	(if fn (funcall fn))))))

(defun sldb-cycle ()
  "Cycle between restart list and backtrace."
  (interactive)
  (let ((pt (point)))
    (cond ((< pt sldb-restart-list-start-marker)
           (goto-char sldb-restart-list-start-marker))
          ((< pt sldb-backtrace-start-marker)
           (goto-char sldb-backtrace-start-marker))
          (t
           (goto-char sldb-restart-list-start-marker)))))

(defun sldb-end-of-backtrace ()
  "Fetch the entire backtrace and go to the last frame."
  (interactive)
  (sldb-fetch-all-frames)
  (sldb-goto-last-frame))

(defun sldb-fetch-all-frames ()
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (sldb-goto-last-frame)
    (let ((last (sldb-frame-number-at-point)))
      (goto-char (next-single-char-property-change (point) 'frame))
      (delete-region (point) (point-max))
      (save-excursion
        (sldb-insert-frames (sly-eval `(swank:backtrace ,(1+ last) nil))
                            nil)))))


;;;;;; SLDB show source

(defun sldb-show-source ()
  "Highlight the frame at point's expression in a source code buffer."
  (interactive)
  (sldb-show-frame-source (sldb-frame-number-at-point)))

(defun sldb-show-frame-source (frame-number)
  (sly-eval-async
      `(swank:frame-source-location ,frame-number)
    (lambda (source-location)
      (destructure-case source-location
        ((:error message)
         (message "%s" message)
         (ding))
        (t
         (sly-show-source-location source-location))))))

(defun sly-show-source-location (source-location &optional no-highlight-p)
  (sly-goto-source-location source-location)
  (let ((pos (point))) ; show the location, but don't hijack focus.
    (with-selected-window (display-buffer (current-buffer) t)
      (goto-char pos)
      (recenter (if (= (current-column) 0) 1))
      (unless no-highlight-p (sly-highlight-sexp)))))

(defun sly-highlight-sexp (&optional start end)
  "Highlight the first sexp after point."
  (let ((start (or start (point)))
	(end (or end (save-excursion (ignore-errors (forward-sexp)) (point)))))
    (sly-flash-region start end)))

(defun sly-highlight-line (&optional timeout)
  (sly-flash-region (+ (line-beginning-position) (current-indentation))
                      (line-end-position)
                      timeout))


;;;;;; SLDB toggle details

(defun sldb-toggle-details (&optional on)
  "Toggle display of details for the current frame.
The details include local variable bindings and CATCH-tags."
  (interactive)
  (cl-assert (sldb-frame-number-at-point))
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (if (or on (not (sldb-frame-details-visible-p)))
	(sldb-show-frame-details)
      (sldb-hide-frame-details))))

(defun sldb-show-frame-details ()
  ;; fetch and display info about local variables and catch tags
  (cl-destructuring-bind (start end frame locals catches) (sldb-frame-details)
    (sly-save-coordinates start
      (delete-region start end)
      (sly-propertize-region `(frame ,frame details-visible-p t)
        (sldb-insert-frame frame (if (sldb-frame-restartable-p frame)
                                     'sldb-restartable-frame-line-face
                                   ;; FIXME: can we somehow merge the two?
                                   'sldb-detailed-frame-line-face))
        (let ((indent1 "      ")
              (indent2 "        "))
          (insert indent1 (sldb-in-face section
                            (if locals "Locals:" "[No Locals]")) "\n")
          (sldb-insert-locals locals indent2 frame)
          (when catches
            (insert indent1 (sldb-in-face section "Catch-tags:") "\n")
            (dolist (tag catches)
              (sly-propertize-region `(catch-tag ,tag)
                (insert indent2 (sldb-in-face catch-tag (format "%s" tag))
                        "\n"))))
          (setq end (point)))))
    (sly-recenter end)))

(defun sldb-frame-details ()
  ;; Return a list (START END FRAME LOCALS CATCHES) for frame at point.
  (let* ((frame (get-text-property (point) 'frame))
         (num (car frame)))
    (cl-destructuring-bind (start end) (sldb-frame-region)
      (cl-list* start end frame
                (sly-eval `(swank:frame-locals-and-catch-tags ,num))))))

(defvar sldb-insert-frame-variable-value-function
  'sldb-insert-frame-variable-value)

(defun sldb-insert-locals (vars prefix frame)
  "Insert VARS and add PREFIX at the beginning of each inserted line.
VAR should be a plist with the keys :name, :id, and :value."
  (cl-loop for i from 0
           for var in vars do
           (cl-destructuring-bind (&key name id value) var
             (sly-propertize-region
                 (list 'sldb-default-action 'sldb-inspect-var 'var i)
               (insert prefix
                       (sldb-in-face local-name
                         (concat name (if (zerop id) "" (format "#%d" id))))
                       " = ")
               (funcall sldb-insert-frame-variable-value-function
                        value frame i)
               (insert "\n")))))

(defun sldb-insert-frame-variable-value (value _frame _index)
  (insert (sldb-in-face local-value value)))

(defun sldb-hide-frame-details ()
  ;; delete locals and catch tags, but keep the function name and args.
  (cl-destructuring-bind (start end) (sldb-frame-region)
    (let ((frame (get-text-property (point) 'frame)))
      (sly-save-coordinates start
        (delete-region start end)
        (sly-propertize-region '(details-visible-p nil)
          (sldb-insert-frame frame))))))

(defun sldb-disassemble ()
  "Disassemble the code for the current frame."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (sly-eval-async `(swank:sldb-disassemble ,frame)
      (lambda (result)
        (sly-show-description result nil)))))


;;;;;; SLDB eval and inspect

(defun sldb-eval-in-frame (frame string package)
  "Prompt for an expression and evaluate it in the selected frame."
  (interactive (sldb-read-form-for-frame "Eval in frame (%s)> "))
  (sly-eval-async `(swank:eval-string-in-frame ,string ,frame ,package)
    (if current-prefix-arg
        'sly-write-string
      'sly-display-eval-result)))

(defun sldb-pprint-eval-in-frame (frame string package)
  "Prompt for an expression, evaluate in selected frame, pretty-print result."
  (interactive (sldb-read-form-for-frame "Eval in frame (%s)> "))
  (sly-eval-async
      `(swank:pprint-eval-string-in-frame ,string ,frame ,package)
    (lambda (result)
      (sly-show-description result nil))))

(defun sldb-read-form-for-frame (fstring)
  (let* ((frame (sldb-frame-number-at-point))
         (pkg (sly-eval `(swank:frame-package-name ,frame))))
    (list frame
          (let ((sly-buffer-package pkg))
            (sly-read-from-minibuffer (format fstring pkg)))
          pkg)))

(defun sldb-inspect-in-frame (string)
  "Prompt for an expression and inspect it in the selected frame."
  (interactive (list (sly-read-from-minibuffer
                      "Inspect in frame (evaluated): "
                      (sly-sexp-at-point))))
  (let ((number (sldb-frame-number-at-point)))
    (sly-eval-async `(swank:inspect-in-frame ,string ,number)
      'sly-open-inspector)))

(defun sldb-inspect-var ()
  (let ((frame (sldb-frame-number-at-point))
        (var (sldb-var-number-at-point)))
    (sly-eval-async `(swank:inspect-frame-var ,frame ,var)
      'sly-open-inspector)))

(defun sldb-inspect-condition ()
  "Inspect the current debugger condition."
  (interactive)
  (sly-eval-async '(swank:inspect-current-condition)
    'sly-open-inspector))

(defun sldb-print-condition ()
  (interactive)
  (sly-eval-describe `(swank:sdlb-print-condition)))


;;;;;; SLDB movement

(defun sldb-down ()
  "Select next frame."
  (interactive)
  (sldb-forward-frame))

(defun sldb-up ()
  "Select previous frame."
  (interactive)
  (sldb-backward-frame)
  (when (= (point) sldb-backtrace-start-marker)
    (recenter (1+ (count-lines (point-min) (point))))))

(defun sldb-sugar-move (move-fn)
  (let ((inhibit-read-only t))
    (when (sldb-frame-details-visible-p) (sldb-hide-frame-details))
    (funcall move-fn)
    (sldb-show-source)
    (sldb-toggle-details t)))

(defun sldb-details-up ()
  "Select previous frame and show details."
  (interactive)
  (sldb-sugar-move 'sldb-up))

(defun sldb-details-down ()
  "Select next frame and show details."
  (interactive)
  (sldb-sugar-move 'sldb-down))


;;;;;; SLDB restarts

(defun sldb-quit ()
  "Quit to toplevel."
  (interactive)
  (cl-assert sldb-restarts () "sldb-quit called outside of sldb buffer")
  (sly-rex () ('(swank:throw-to-toplevel))
    ((:ok x) (error "sldb-quit returned [%s]" x))
    ((:abort _))))

(defun sldb-continue ()
  "Invoke the \"continue\" restart."
  (interactive)
  (cl-assert sldb-restarts () "sldb-continue called outside of sldb buffer")
  (sly-rex ()
      ('(swank:sldb-continue))
    ((:ok _)
     (message "No restart named continue")
     (ding))
    ((:abort _))))

(defun sldb-abort ()
  "Invoke the \"abort\" restart."
  (interactive)
  (sly-eval-async '(swank:sldb-abort)
    (lambda (v) (message "Restart returned: %S" v))))

(defun sldb-invoke-restart (&optional number)
  "Invoke a restart.
Optional NUMBER (index into `sldb-restarts') specifies the
restart to invoke, otherwise use the restart at point."
  (interactive)
  (let ((restart (or number (sldb-restart-at-point))))
    (sly-rex ()
        ((list 'swank:invoke-nth-restart-for-emacs sldb-level restart))
      ((:ok value) (message "Restart returned: %s" value))
      ((:abort _)))))

(defun sldb-invoke-restart-by-name (restart-name)
  (interactive (list (let ((completion-ignore-case t))
                       (completing-read "Restart: " sldb-restarts nil t
                                        ""
                                        'sldb-invoke-restart-by-name))))
  (sldb-invoke-restart (cl-position restart-name sldb-restarts
                                    :test 'string= :key 'first)))

(defun sldb-break-with-default-debugger (&optional dont-unwind)
  "Enter default debugger."
  (interactive "P")
  (sly-rex ()
      ((list 'swank:sldb-break-with-default-debugger
             (not (not dont-unwind)))
       nil sly-current-thread)
    ((:abort _))))

(defun sldb-break-with-system-debugger (&optional lightweight)
  "Enter system debugger (gdb)."
  (interactive "P")
  (sly-attach-gdb sly-buffer-connection lightweight))

(defun sly-attach-gdb (connection &optional lightweight)
  "Run `gud-gdb'on the connection with PID `pid'.

If `lightweight' is given, do not send any request to the
inferior Lisp (e.g. to obtain default gdb config) but only
operate from the Emacs side; intended for cases where the Lisp is
truly screwed up."
  (interactive
   (list (sly-read-connection "Attach gdb to: " (sly-connection)) "P"))
  (let ((pid  (sly-pid connection))
        (file (sly-lisp-implementation-program connection))
        (commands (unless lightweight
                    (let ((sly-dispatching-connection connection))
                      (sly-eval `(swank:gdb-initial-commands))))))
    (gud-gdb (format "gdb -p %d %s" pid (or file "")))
    (with-current-buffer gud-comint-buffer
      (dolist (cmd commands)
        ;; First wait until gdb was initialized, then wait until current
        ;; command was processed.
        (while (not (looking-back comint-prompt-regexp))
          (sit-for 0.01))
        ;; We do not use `gud-call' because we want the initial commands
        ;; to be displayed by the user so he knows what he's got.
        (insert cmd)
        (comint-send-input)))))

(defun sly-read-connection (prompt &optional initial-value)
  "Read a connection from the minibuffer.
Return the net process, or nil."
  (cl-assert (memq initial-value sly-net-processes))
  (let* ((to-string (lambda (p)
                      (format "%s (pid %d)"
                              (sly-connection-name p) (sly-pid p))))
         (candidates (mapcar (lambda (p) (cons (funcall to-string p) p))
                             sly-net-processes)))
    (cdr (assoc (completing-read prompt candidates
                                 nil t (funcall to-string initial-value))
                candidates))))

(defun sldb-step ()
  "Step to next basic-block boundary."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (sly-eval-async `(swank:sldb-step ,frame))))

(defun sldb-next ()
  "Step over call."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (sly-eval-async `(swank:sldb-next ,frame))))

(defun sldb-out ()
  "Resume stepping after returning from this function."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (sly-eval-async `(swank:sldb-out ,frame))))

(defun sldb-break-on-return ()
  "Set a breakpoint at the current frame.
The debugger is entered when the frame exits."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (sly-eval-async `(swank:sldb-break-on-return ,frame)
      (lambda (msg) (message "%s" msg)))))

(defun sldb-break (name)
  "Set a breakpoint at the start of the function NAME."
  (interactive (list (sly-read-symbol-name "Function: " t)))
  (sly-eval-async `(swank:sldb-break ,name)
    (lambda (msg) (message "%s" msg))))

(defun sldb-return-from-frame (string)
  "Reads an expression in the minibuffer and causes the function to
return that value, evaluated in the context of the frame."
  (interactive (list (sly-read-from-minibuffer "Return from frame: ")))
  (let* ((number (sldb-frame-number-at-point)))
    (sly-rex ()
        ((list 'swank:sldb-return-from-frame number string))
      ((:ok value) (message "%s" value))
      ((:abort _)))))

(defun sldb-restart-frame ()
  "Causes the frame to restart execution with the same arguments as it
was called originally."
  (interactive)
  (let* ((number (sldb-frame-number-at-point)))
    (sly-rex ()
        ((list 'swank:restart-frame number))
      ((:ok value) (message "%s" value))
      ((:abort _)))))

(defun sly-toggle-break-on-signals ()
  "Toggle the value of *break-on-signals*."
  (interactive)
  (sly-eval-async `(swank:toggle-break-on-signals)
    (lambda (msg) (message "%s" msg))))


;;;;;; SLDB recompilation commands

(defun sldb-recompile-frame-source (&optional raw-prefix-arg)
  (interactive "P")
  (sly-eval-async
      `(swank:frame-source-location ,(sldb-frame-number-at-point))
    (lexical-let ((policy (sly-compute-policy raw-prefix-arg)))
      (lambda (source-location)
        (destructure-case source-location
          ((:error message)
           (message "%s" message)
           (ding))
          (t
           (let ((sly-compilation-policy policy))
             (sly-recompile-location source-location))))))))


;;;; Thread control panel

(defvar sly-threads-buffer-name (sly-buffer-name :threads))
(defvar sly-threads-buffer-timer nil)

(defcustom sly-threads-update-interval nil
  "Interval at which the list of threads will be updated."
  :type '(choice
          (number :value 0.5)
          (const nil))
  :group 'sly-ui)

(defun sly-list-threads ()
  "Display a list of threads."
  (interactive)
  (let ((name sly-threads-buffer-name))
    (sly-with-popup-buffer (name :connection t
                                   :mode 'sly-thread-control-mode)
      (sly-update-threads-buffer)
      (goto-char (point-min))
      (when sly-threads-update-interval
        (when sly-threads-buffer-timer
          (cancel-timer sly-threads-buffer-timer))
        (setq sly-threads-buffer-timer
              (run-with-timer
               sly-threads-update-interval
               sly-threads-update-interval
               'sly-update-threads-buffer))))))

(defun sly-quit-threads-buffer ()
  (when sly-threads-buffer-timer
    (cancel-timer sly-threads-buffer-timer))
  (quit-window t)
  (sly-eval-async `(swank:quit-thread-browser)))

(defun sly-update-threads-buffer ()
  (interactive)
  (with-current-buffer sly-threads-buffer-name
    (sly-eval-async '(swank:list-threads)
      'sly-display-threads)))

(defun sly-move-point (position)
  "Move point in the current buffer and in the window the buffer is displayed."
  (let ((window (get-buffer-window (current-buffer) t)))
    (goto-char position)
    (when window
      (set-window-point window position))))

(defun sly-display-threads (threads)
  (with-current-buffer sly-threads-buffer-name
    (let* ((inhibit-read-only t)
           (old-thread-id (get-text-property (point) 'thread-id))
           (old-line (line-number-at-pos))
           (old-column (current-column)))
      (erase-buffer)
      (sly-insert-threads threads)
      (let ((new-line (cl-position old-thread-id (cdr threads)
                                   :key #'car :test #'equal)))
        (goto-char (point-min))
        (forward-line (or new-line old-line))
        (move-to-column old-column)
        (sly-move-point (point))))))

(defun sly-transpose-lists (list-of-lists)
  (let ((ncols (length (car list-of-lists))))
    (cl-loop for col-index below ncols
             collect (cl-loop for row in list-of-lists
                              collect (elt row col-index)))))

(defun sly-insert-table-row (line line-props col-props col-widths)
  (sly-propertize-region line-props
    (cl-loop for string in line
             for col-prop in col-props
             for width in col-widths do
             (sly-insert-propertized col-prop string)
             (insert-char ?\ (- width (length string))))))

(defun sly-insert-table (rows header row-properties column-properties)
  "Insert a \"table\" so that the columns are nicely aligned."
  (let* ((ncols (length header))
         (lines (cons header rows))
         (widths (cl-loop for columns in (sly-transpose-lists lines)
                          collect (1+ (cl-loop for cell in columns
                                               maximize (length cell)))))
         (header-line (with-temp-buffer
                        (sly-insert-table-row
                         header nil (make-list ncols nil) widths)
                        (buffer-string))))
    (cond ((boundp 'header-line-format)
           (setq header-line-format header-line))
          (t (insert header-line "\n")))
    (cl-loop for line in rows  for line-props in row-properties do
             (sly-insert-table-row line line-props column-properties widths)
             (insert "\n"))))

(defvar sly-threads-table-properties
  '(nil (face bold)))

(defun sly-insert-threads (threads)
  (let* ((labels (car threads))
         (threads (cdr threads))
         (header (cl-loop for label in labels collect
                          (capitalize (substring (symbol-name label) 1))))
         (rows (cl-loop for thread in threads collect
                        (cl-loop for prop in thread collect
                                 (format "%s" prop))))
         (line-props (cl-loop for (id) in threads for i from 0
                              collect `(thread-index ,i thread-id ,id)))
         (col-props (cl-loop for nil in labels for i from 0 collect
                             (nth i sly-threads-table-properties))))
    (sly-insert-table rows header line-props col-props)))


;;;;; Major mode

(define-derived-mode sly-thread-control-mode fundamental-mode
  "Threads"
  "SLY Thread Control Panel Mode.

\\{sly-thread-control-mode-map}
\\{sly-popup-buffer-mode-map}"
  (when sly-truncate-lines
    (set (make-local-variable 'truncate-lines) t))
  (setq buffer-undo-list t))

(sly-define-keys sly-thread-control-mode-map
  ("a" 'sly-thread-attach)
  ("d" 'sly-thread-debug)
  ("g" 'sly-update-threads-buffer)
  ("k" 'sly-thread-kill)
  ("q" 'sly-quit-threads-buffer))

(defun sly-thread-kill ()
  (interactive)
  (sly-eval `(cl:mapc 'swank:kill-nth-thread
                        ',(sly-get-properties 'thread-index)))
  (call-interactively 'sly-update-threads-buffer))

(defun sly-get-region-properties (prop start end)
  (cl-loop for position = (if (get-text-property start prop)
                              start
                            (next-single-property-change start prop))
           then (next-single-property-change position prop)
           while (<= position end)
           collect (get-text-property position prop)))

(defun sly-get-properties (prop)
  (if (use-region-p)
      (sly-get-region-properties prop
                                   (region-beginning)
                                   (region-end))
    (let ((value (get-text-property (point) prop)))
      (when value
        (list value)))))

(defun sly-thread-attach ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-index))
        (file (sly-swank-port-file)))
    (sly-eval-async `(swank:start-swank-server-in-thread ,id ,file)))
  (sly-read-port-and-connect nil))

(defun sly-thread-debug ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-index)))
    (sly-eval-async `(swank:debug-nth-thread ,id))))


;;;;; Connection listing

(define-derived-mode sly-connection-list-mode fundamental-mode
  "SLY-Connections"
  "SLY Connection List Mode.

\\{sly-connection-list-mode-map}
\\{sly-popup-buffer-mode-map}"
  (when sly-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

(sly-define-keys sly-connection-list-mode-map
  ("d"         'sly-connection-list-make-default)
  ("g"         'sly-update-connection-list)
  ((kbd "C-k") 'sly-quit-connection-at-point)
  ("R"         'sly-restart-connection-at-point))

(defun sly-connection-at-point ()
  (or (get-text-property (point) 'sly-connection)
      (error "No connection at point")))

(defun sly-quit-connection-at-point (connection)
  (interactive (list (sly-connection-at-point)))
  (let ((sly-dispatching-connection connection)
        (end (time-add (current-time) (seconds-to-time 3))))
    (sly-quit-lisp t)
    (while (memq connection sly-net-processes)
      (when (time-less-p end (current-time))
        (message "Quit timeout expired.  Disconnecting.")
        (delete-process connection))
      (sit-for 0 100)))
  (sly-update-connection-list))

(defun sly-restart-connection-at-point (connection)
  (interactive (list (sly-connection-at-point)))
  (let ((sly-dispatching-connection connection))
    (sly-restart-inferior-lisp)))

(defun sly-connection-list-make-default ()
  "Make the connection at point the default connection."
  (interactive)
  (sly-select-connection (sly-connection-at-point))
  (sly-update-connection-list))

(defvar sly-connections-buffer-name (sly-buffer-name :connections))

(defun sly-list-connections ()
  "Display a list of all connections."
  (interactive)
  (sly-with-popup-buffer (sly-connections-buffer-name
                            :mode 'sly-connection-list-mode)
    (sly-draw-connection-list)))

(defun sly-update-connection-list ()
  "Display a list of all connections."
  (interactive)
  (let ((pos (point))
        (inhibit-read-only t))
    (erase-buffer)
    (sly-draw-connection-list)
    (goto-char pos)))

(defun sly-draw-connection-list ()
  (let ((default-pos nil)
        (default sly-default-connection)
        (fstring "%s%2s  %-10s  %-17s  %-7s %-s\n"))
    (insert (format fstring " " "Nr" "Name" "Port" "Pid" "Type")
            (format fstring " " "--" "----" "----" "---" "----"))
    (dolist (p (reverse sly-net-processes))
      (when (eq default p) (setf default-pos (point)))
      (sly-insert-propertized
       (list 'sly-connection p)
       (format fstring
               (if (eq default p) "*" " ")
               (sly-connection-number p)
               (sly-connection-name p)
               (or (process-id p) (process-contact p))
               (sly-pid p)
               (sly-lisp-implementation-type p))))
    (when default-pos
      (goto-char default-pos))))


;;;; Inspector

(defgroup sly-inspector nil
  "Inspector faces."
  :prefix "sly-inspector-"
  :group 'sly)

(defface sly-inspector-topline-face
  '((t ()))
  "Face for top line describing object."
  :group 'sly-inspector)

(defface sly-inspector-label-face
  '((t (:inherit font-lock-constant-face)))
  "Face for labels in the inspector."
  :group 'sly-inspector)

(defface sly-inspector-action-face
  '((t (:inherit font-lock-warning-face)))
  "Face for labels of inspector actions."
  :group 'sly-inspector)

(defface sly-inspector-type-face
  '((t (:inherit font-lock-type-face)))
  "Face for type description in inspector."
  :group 'sly-inspector)

(defvar sly-inspector-mark-stack '())

(defun sly-inspect (string)
  "Eval an expression and inspect the result."
  (interactive
   (list (sly-read-from-minibuffer "Inspect value (evaluated): "
				     (sly-sexp-at-point))))
  (sly-eval-async `(swank:init-inspector ,string) 'sly-open-inspector))

(define-derived-mode sly-inspector-mode fundamental-mode
  "SLY-Inspector"
  "
\\{sly-inspector-mode-map}
\\{sly-popup-buffer-mode-map}"
  (set-syntax-table lisp-mode-syntax-table)
  (sly-set-truncate-lines)
  (setq buffer-read-only t))

(defun sly-inspector-buffer ()
  (or (get-buffer (sly-buffer-name :inspector))
      (sly-with-popup-buffer ((sly-buffer-name :inspector)
                                :mode 'sly-inspector-mode)
        (setq sly-inspector-mark-stack '())
        (buffer-disable-undo)
        (current-buffer))))

(defmacro sly-inspector-fontify (face string)
  `(sly-add-face ',(intern (format "sly-inspector-%s-face" face)) ,string))

(defvar sly-inspector-insert-ispec-function 'sly-inspector-insert-ispec)

(defun sly-open-inspector (inspected-parts &optional point hook)
  "Display INSPECTED-PARTS in a new inspector window.
Optionally set point to POINT. If HOOK is provided, it is added to local
KILL-BUFFER hooks for the inspector buffer."
  (with-current-buffer (sly-inspector-buffer)
    (when hook
      (add-hook 'kill-buffer-hook hook t t))
    (setq sly-buffer-connection (sly-current-connection))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (pop-to-buffer (current-buffer))
      (cl-destructuring-bind (&key id title content) inspected-parts
        (cl-macrolet ((fontify (face string)
                               `(sly-inspector-fontify ,face ,string)))
          (sly-propertize-region
              (list 'sly-part-number id
                    'mouse-face 'highlight
                    'face 'sly-inspectable-value-face)
            (insert title))
          (while (eq (char-before) ?\n)
            (backward-delete-char 1))
          (insert "\n" (fontify label "--------------------") "\n")
          (save-excursion
            (sly-inspector-insert-content content))
          (when point
            (cl-check-type point cons)
            (ignore-errors
              (goto-char (point-min))
              (forward-line (1- (car point)))
              (move-to-column (cdr point)))))))))

(defvar sly-inspector-limit 500)

(defun sly-inspector-insert-content (content)
  (sly-inspector-fetch-chunk
   content nil
   (lambda (chunk)
     (let ((inhibit-read-only t))
       (sly-inspector-insert-chunk chunk t t)))))

(defun sly-inspector-insert-chunk (chunk prev next)
  "Insert CHUNK at point.
If PREV resp. NEXT are true insert more-buttons as needed."
  (cl-destructuring-bind (ispecs len start end) chunk
    (when (and prev (> start 0))
      (sly-inspector-insert-more-button start t))
    (mapc sly-inspector-insert-ispec-function ispecs)
    (when (and next (< end len))
      (sly-inspector-insert-more-button end nil))))

(defun sly-inspector-insert-ispec (ispec)
  (if (stringp ispec)
      (insert ispec)
    (destructure-case ispec
      ((:value string id)
       (sly-propertize-region
           (list 'sly-part-number id
                 'mouse-face 'highlight
                 'face 'sly-inspectable-value-face)
         (insert string)))
      ((:label string)
       (insert (sly-inspector-fontify label string)))
      ((:action string id)
       (sly-insert-propertized (list 'sly-action-number id
                                       'mouse-face 'highlight
                                       'face 'sly-inspector-action-face)
                                 string)))))

(defun sly-inspector-position ()
  "Return a pair (Y-POSITION X-POSITION) representing the
position of point in the current buffer."
  ;; We make sure we return absolute coordinates even if the user has
  ;; narrowed the buffer.
  ;; FIXME: why would somebody narrow the buffer?
  (save-restriction
    (widen)
    (cons (line-number-at-pos)
          (current-column))))

(defun sly-inspector-property-at-point ()
  (let* ((properties '(sly-part-number sly-range-button
                                         sly-action-number))
         (find-property
          (lambda (point)
            (cl-loop for property in properties
                     for value = (get-text-property point property)
                     when value
                     return (list property value)))))
    (or (funcall find-property (point))
        (funcall find-property (1- (point))))))

(defun sly-inspector-operate-on-point ()
  "Invoke the command for the text at point.
1. If point is on a value then recursivly call the inspector on
that value.
2. If point is on an action then call that action.
3. If point is on a range-button fetch and insert the range."
  (interactive)
  (let ((opener (lexical-let ((point (sly-inspector-position)))
                  (lambda (parts)
                    (when parts
                      (sly-open-inspector parts point)))))
        (new-opener (lambda (parts)
                      (when parts
                        (sly-open-inspector parts)))))
    (cl-destructuring-bind (&optional property value)
        (sly-inspector-property-at-point)
      (cl-case property
        (sly-part-number
         (sly-eval-async `(swank:inspect-nth-part ,value)
           new-opener)
         (push (sly-inspector-position) sly-inspector-mark-stack))
        (sly-range-button
         (sly-inspector-fetch-more value))
        (sly-action-number
         (sly-eval-async `(swank::inspector-call-nth-action ,value)
           opener))
        (t (error "No object at point"))))))

(defun sly-inspector-operate-on-click (event)
  "Move to events' position and operate the part."
  (interactive "@e")
  (let ((point (posn-point (event-end event))))
    (cond ((and point
                (or (get-text-property point 'sly-part-number)
                    (get-text-property point 'sly-range-button)
                    (get-text-property point 'sly-action-number)))
           (goto-char point)
           (sly-inspector-operate-on-point))
          (t
           (error "No clickable part here")))))

(defun sly-inspector-pop ()
  "Reinspect the previous object."
  (interactive)
  (sly-eval-async
      `(swank:inspector-pop)
    (lambda (result)
      (cond (result
             (sly-open-inspector result (pop sly-inspector-mark-stack)))
            (t
             (message "No previous object")
             (ding))))))

(defun sly-inspector-next ()
  "Inspect the next object in the history."
  (interactive)
  (let ((result (sly-eval `(swank:inspector-next))))
    (cond (result
	   (push (sly-inspector-position) sly-inspector-mark-stack)
	   (sly-open-inspector result))
	  (t (message "No next object")
	     (ding)))))

(defun sly-inspector-quit ()
  "Quit the inspector and kill the buffer."
  (interactive)
  (sly-eval-async `(swank:quit-inspector))
  (quit-window t))

;; FIXME: first return value is just point.
;; FIXME: could probably use sly-search-property.
(defun sly-find-inspectable-object (direction limit)
  "Find the next/previous inspectable object.
DIRECTION can be either 'next or 'prev.
LIMIT is the maximum or minimum position in the current buffer.

Return a list of two values: If an object could be found, the
starting position of the found object and T is returned;
otherwise LIMIT and NIL is returned."
  (let ((finder (cl-ecase direction
                  (next 'next-single-property-change)
                  (prev 'previous-single-property-change))))
    (let ((prop nil) (curpos (point)))
      (while (and (not prop) (not (= curpos limit)))
        (let ((newpos (funcall finder curpos 'sly-part-number nil limit)))
          (setq prop (get-text-property newpos 'sly-part-number))
          (setq curpos newpos)))
      (list curpos (and prop t)))))

(defun sly-inspector-next-inspectable-object (arg)
  "Move point to the next inspectable object.
With optional ARG, move across that many objects.
If ARG is negative, move backwards."
  (interactive "p")
  (let ((maxpos (point-max)) (minpos (point-min))
        (previously-wrapped-p nil))
    ;; Forward.
    (while (> arg 0)
      (cl-destructuring-bind (pos foundp)
          (sly-find-inspectable-object 'next maxpos)
        (if foundp
            (progn (goto-char pos) (setq arg (1- arg))
                   (setq previously-wrapped-p nil))
          (if (not previously-wrapped-p) ; cycle detection
              (progn (goto-char minpos) (setq previously-wrapped-p t))
            (error "No inspectable objects")))))
    ;; Backward.
    (while (< arg 0)
      (cl-destructuring-bind (pos foundp)
          (sly-find-inspectable-object 'prev minpos)
        ;; SLY-OPEN-INSPECTOR inserts the title of an inspector page
        ;; as a presentation at the beginning of the buffer; skip
        ;; that.  (Notice how this problem can not arise in ``Forward.'')
        (if (and foundp (/= pos minpos))
            (progn (goto-char pos) (setq arg (1+ arg))
                   (setq previously-wrapped-p nil))
          (if (not previously-wrapped-p) ; cycle detection
              (progn (goto-char maxpos) (setq previously-wrapped-p t))
            (error "No inspectable objects")))))))

(defun sly-inspector-previous-inspectable-object (arg)
  "Move point to the previous inspectable object.
With optional ARG, move across that many objects.
If ARG is negative, move forwards."
  (interactive "p")
  (sly-inspector-next-inspectable-object (- arg)))

(defun sly-inspector-describe ()
  (interactive)
  (sly-eval-describe `(swank:describe-inspectee)))

(defun sly-inspector-pprint (part)
  (interactive (list (or (get-text-property (point) 'sly-part-number)
                         (error "No part at point"))))
  (sly-eval-describe `(swank:pprint-inspector-part ,part)))

(defun sly-inspector-eval (string)
  "Eval an expression in the context of the inspected object."
  (interactive (list (sly-read-from-minibuffer "Inspector eval: ")))
  (sly-eval-with-transcript `(swank:inspector-eval ,string)))

(defun sly-inspector-history ()
  "Show the previously inspected objects."
  (interactive)
  (sly-eval-describe `(swank:inspector-history)))

(defun sly-inspector-show-source (part)
  (interactive (list (or (get-text-property (point) 'sly-part-number)
                         (error "No part at point"))))
  (sly-eval-async
      `(swank:find-source-location-for-emacs '(:inspector ,part))
    #'sly-show-source-location))

(defun sly-inspector-reinspect ()
  (interactive)
  (sly-eval-async `(swank:inspector-reinspect)
    (lexical-let ((point (sly-inspector-position)))
      (lambda (parts)
        (sly-open-inspector parts point)))))

(defun sly-inspector-toggle-verbose ()
  (interactive)
  (sly-eval-async `(swank:inspector-toggle-verbose)
    (lexical-let ((point (sly-inspector-position)))
      (lambda (parts)
        (sly-open-inspector parts point)))))

(defun sly-inspector-insert-more-button (index previous)
  (sly-insert-propertized
   (list 'sly-range-button (list index previous)
         'mouse-face 'highlight
         'face 'sly-inspector-action-face)
   (if previous " [--more--]\n" " [--more--]")))

(defun sly-inspector-fetch-all ()
  "Fetch all inspector contents and go to the end."
  (interactive)
  (goto-char (1- (point-max)))
  (let ((button (get-text-property (point) 'sly-range-button)))
    (when button
      (let (sly-inspector-limit)
        (sly-inspector-fetch-more button)))))

(defun sly-inspector-fetch-more (button)
  (cl-destructuring-bind (index prev) button
    (sly-inspector-fetch-chunk
     (list '() (1+ index) index index) prev
     (sly-rcurry
      (lambda (chunk prev)
        (let ((inhibit-read-only t))
          (apply #'delete-region (sly-property-bounds 'sly-range-button))
          (sly-inspector-insert-chunk chunk prev (not prev))))
      prev))))

(defun sly-inspector-fetch-chunk (chunk prev cont)
  (sly-inspector-fetch chunk sly-inspector-limit prev cont))

(defun sly-inspector-fetch (chunk limit prev cont)
  (cl-destructuring-bind (from to)
      (sly-inspector-next-range chunk limit prev)
    (cond ((and from to)
           (sly-eval-async
               `(swank:inspector-range ,from ,to)
             (sly-rcurry (lambda (chunk2 chunk1 limit prev cont)
                             (sly-inspector-fetch
                              (sly-inspector-join-chunks chunk1 chunk2)
                              limit prev cont))
                           chunk limit prev cont)))
          (t (funcall cont chunk)))))

(defun sly-inspector-next-range (chunk limit prev)
  (cl-destructuring-bind (_ len start end) chunk
    (let ((count (- end start)))
      (cond ((and prev (< 0 start) (or (not limit) (< count limit)))
             (list (if limit (max (- end limit) 0) 0) start))
            ((and (not prev) (< end len) (or (not limit) (< count limit)))
             (list end (if limit (+ start limit) most-positive-fixnum)))
            (t '(nil nil))))))

(defun sly-inspector-join-chunks (chunk1 chunk2)
  (cl-destructuring-bind (i1 _l1 s1 e1) chunk1
    (cl-destructuring-bind (i2 l2 s2 e2) chunk2
      (cond ((= e1 s2)
             (list (append i1 i2) l2 s1 e2))
            ((= e2 s1)
             (list (append i2 i1) l2 s2 e1))
            (t (error "Invalid chunks"))))))


(sly-define-keys sly-inspector-mode-map
  ([return] 'sly-inspector-operate-on-point)
  ("\C-m"   'sly-inspector-operate-on-point)
  ([mouse-2] 'sly-inspector-operate-on-click)
  ("l" 'sly-inspector-pop)
  ("n" 'sly-inspector-next)
  (" " 'sly-inspector-next)
  ("d" 'sly-inspector-describe)
  ("p" 'sly-inspector-pprint)
  ("e" 'sly-inspector-eval)
  ("h" 'sly-inspector-history)
  ("g" 'sly-inspector-reinspect)
  ("v" 'sly-inspector-show-source)
  ("\C-i" 'sly-inspector-next-inspectable-object)
  ([(shift tab)]
   'sly-inspector-previous-inspectable-object) ; Emacs translates S-TAB
  ([backtab] 'sly-inspector-previous-inspectable-object) ; to BACKTAB on X.
  (">" 'sly-inspector-fetch-all)
  ("q" 'sly-inspector-quit))


;;;; Buffer selector

(defvar sly-selector-methods nil
  "List of buffer-selection methods for the `sly-select' command.
Each element is a list (KEY DESCRIPTION FUNCTION).
DESCRIPTION is a one-line description of what the key selects.")

(defvar sly-selector-other-window nil
  "If non-nil use switch-to-buffer-other-window.")

(defun sly-selector (&optional other-window)
  "Select a new buffer by type, indicated by a single character.
The user is prompted for a single character indicating the method by
which to choose a new buffer. The `?' character describes the
available methods.

See `def-sly-selector-method' for defining new methods."
  (interactive)
  (message "Select [%s]: "
           (apply #'string (mapcar #'car sly-selector-methods)))
  (let* ((sly-selector-other-window other-window)
         (ch (save-window-excursion
               (select-window (minibuffer-window))
               (read-char)))
         (method (cl-find ch sly-selector-methods :key #'car)))
    (cond (method
           (funcall (cl-third method)))
          (t
           (message "No method for character: ?\\%c" ch)
           (ding)
           (sleep-for 1)
           (discard-input)
           (sly-selector)))))

(defmacro def-sly-selector-method (key description &rest body)
  "Define a new `sly-select' buffer selection method.

KEY is the key the user will enter to choose this method.

DESCRIPTION is a one-line sentence describing how the method
selects a buffer.

BODY is a series of forms which are evaluated when the selector
is chosen. The returned buffer is selected with
switch-to-buffer."
  (let ((method `(lambda ()
                   (let ((buffer (progn ,@body)))
                     (cond ((not (get-buffer buffer))
                            (message "No such buffer: %S" buffer)
                            (ding))
                           ((get-buffer-window buffer)
                            (select-window (get-buffer-window buffer)))
                           (sly-selector-other-window
                            (switch-to-buffer-other-window buffer))
                           (t
                            (switch-to-buffer buffer)))))))
    `(setq sly-selector-methods
           (cl-sort (cons (list ,key ,description ,method)
                          (cl-remove ,key sly-selector-methods :key #'car))
                    #'< :key #'car))))

(def-sly-selector-method ?? "Selector help buffer."
  (ignore-errors (kill-buffer "*Select Help*"))
  (with-current-buffer (get-buffer-create "*Select Help*")
    (insert "Select Methods:\n\n")
    (cl-loop for (key line nil) in sly-selector-methods
             do (insert (format "%c:\t%s\n" key line)))
    (goto-char (point-min))
    (help-mode)
    (display-buffer (current-buffer) t))
  (sly-selector)
  (current-buffer))

(cl-pushnew (list ?4 "Select in other window" (lambda () (sly-selector t)))
            sly-selector-methods :key #'car)

(def-sly-selector-method ?q "Abort."
  (top-level))

(def-sly-selector-method ?i
  "*inferior-lisp* buffer."
  (cond ((and (sly-connected-p) (sly-process))
         (process-buffer (sly-process)))
        (t
         "*inferior-lisp*")))

(def-sly-selector-method ?v
  "*sly-events* buffer."
  sly-event-buffer-name)

(def-sly-selector-method ?l
  "most recently visited lisp-mode buffer."
  (sly-recently-visited-buffer 'lisp-mode))

(def-sly-selector-method ?d
  "*sldb* buffer for the current connection."
  (or (sldb-get-default-buffer)
      (error "No debugger buffer")))

(def-sly-selector-method ?e
  "most recently visited emacs-lisp-mode buffer."
  (sly-recently-visited-buffer 'emacs-lisp-mode))

(def-sly-selector-method ?c
  "SLY connections buffer."
  (sly-list-connections)
  sly-connections-buffer-name)

(def-sly-selector-method ?n
  "Cycle to the next Lisp connection."
  (sly-cycle-connections)
  (concat "*sly-repl "
          (sly-connection-name (sly-current-connection))
          "*"))

(def-sly-selector-method ?t
  "SLY threads buffer."
  (sly-list-threads)
  sly-threads-buffer-name)

(defun sly-recently-visited-buffer (mode)
  "Return the most recently visited buffer whose major-mode is MODE.
Only considers buffers that are not already visible."
  (cl-loop for buffer in (buffer-list)
           when (and (with-current-buffer buffer (eq major-mode mode))
                     (not (string-match "^ " (buffer-name buffer)))
                     (null (get-buffer-window buffer 'visible)))
           return buffer
           finally (error "Can't find unshown buffer in %S" mode)))


;;;; Indentation

(defun sly-update-indentation ()
  "Update indentation for all macros defined in the Lisp system."
  (interactive)
  (sly-eval-async '(swank:update-indentation-information)))

(defvar sly-indentation-update-hooks)

(defun sly-intern-indentation-spec (spec)
  (cond ((consp spec)
         (cons (sly-intern-indentation-spec (car spec))
               (sly-intern-indentation-spec (cdr spec))))
        ((stringp spec)
         (intern spec))
        (t
         spec)))

;; FIXME: restore the old version without per-package
;; stuff. sly-indentation.el should be able tho disable the simple
;; version if needed.
(defun sly-handle-indentation-update (alist)
  "Update Lisp indent information.

ALIST is a list of (SYMBOL-NAME . INDENT-SPEC) of proposed indentation
settings for `common-lisp-indent-function'. The appropriate property
is setup, unless the user already set one explicitly."
  (dolist (info alist)
    (let ((symbol (intern (car info)))
          (indent (sly-intern-indentation-spec (cl-second info)))
          (packages (cl-third info)))
      (if (and (boundp 'common-lisp-system-indentation)
               (fboundp 'sly-update-system-indentation))
          ;; A table provided by sly-cl-indent.el.
          (funcall #'sly-update-system-indentation symbol indent packages)
        ;; Does the symbol have an indentation value that we set?
        (when (equal (get symbol 'common-lisp-indent-function)
                     (get symbol 'sly-indent))
          (put symbol 'common-lisp-indent-function indent)
          (put symbol 'sly-indent indent)))
      (run-hook-with-args 'sly-indentation-update-hooks
                          symbol indent packages))))


;;;; Contrib modules

(defun sly-load-contribs ()
  (let ((needed (cl-remove-if (lambda (s)
                                (cl-find (symbol-name s)
                                         (sly-lisp-modules)
                                         :key #'downcase
                                         :test #'string=))
                              sly-required-modules
                              :key #'car)))
    (when needed
      ;; No asynchronous request because with :SPAWN that could result
      ;; in the attempt to load modules concurrently which may not be
      ;; supported by the host Lisp.
      (sly-eval `(swank:swank-add-load-paths ',(cl-remove-duplicates
                                                  (mapcar #'cdr needed)
                                                  :test #'string=)))
      (setf (sly-lisp-modules)
            (sly-eval `(swank:swank-require
                          ',(mapcar #'symbol-name (mapcar #'car needed))))))))

(cl-defstruct sly-contrib
  name
  sly-dependencies
  swank-dependencies
  enable
  disable
  authors
  license)

(defmacro define-sly-contrib (name _docstring &rest clauses)
  (declare (indent 1))
  (cl-destructuring-bind (&key sly-dependencies
                               swank-dependencies
                               on-load
                               on-unload
                               authors
                               license)
      (cl-loop for (key . value) in clauses append `(,key ,value))
    (cl-labels
        ((enable-fn (c) (intern (concat (symbol-name c) "-init")))
         (disable-fn (c) (intern (concat (symbol-name c) "-unload")))
         (path-sym (c) (intern (concat (symbol-name c) "--path"))))
      `(progn
         (defvar ,(path-sym name))
         (setq ,(path-sym name) (and load-file-name
                                     (file-name-directory load-file-name)))
         ,@(mapcar (lambda (d) `(require ',d)) sly-dependencies)
         (defun ,(enable-fn name) ()
           (mapc #'funcall ',(mapcar
                              #'enable-fn
                              sly-dependencies))
           (cl-loop for dep in ',swank-dependencies
                    do (cl-pushnew (cons dep ,(path-sym name))
                                   sly-required-modules
                                   :key #'car))
           (when (sly-connected-p)
             (sly-load-contribs))
           ,@on-load)
         (defun ,(disable-fn name) ()
           ,@on-unload
           (mapc #'funcall ',(mapcar
                              #'disable-fn
                              sly-dependencies)))
         (put 'sly-contribs ',name
              (make-sly-contrib
               :name ',name :authors ',authors :license ',license
               :sly-dependencies ',sly-dependencies
               :swank-dependencies ',swank-dependencies
               :enable ',(enable-fn name) :disable ',(disable-fn name)))))))

(defun sly-all-contribs ()
  (cl-loop for (nil val) on (symbol-plist 'sly-contribs) by #'cddr
           when (sly-contrib-p val)
           collect val))

(defun sly-contrib-all-dependencies (contrib)
  "List all contribs recursively needed by CONTRIB, including self."
  (cons contrib
        (cl-mapcan #'sly-contrib-all-dependencies
                   (sly-contrib-sly-dependencies
                    (sly-find-contrib contrib)))))

(defun sly-find-contrib (name)
  (get 'sly-contribs name))

(defun sly-read-contrib-name ()
  (let ((names (cl-loop for c in (sly-all-contribs) collect
                        (symbol-name (sly-contrib-name c)))))
    (intern (completing-read "Contrib: " names nil t))))

(defun sly-enable-contrib (name)
  (interactive (list (sly-read-contrib-name)))
  (let ((c (or (sly-find-contrib name)
               (error "Unknown contrib: %S" name))))
    (funcall (sly-contrib-enable c))))

(defun sly-disable-contrib (name)
  (interactive (list (sly-read-contrib-name)))
  (let ((c (or (sly-find-contrib name)
               (error "Unknown contrib: %S" name))))
    (funcall (sly-contrib-disable c))))


;;;;; Pull-down menu

(defvar sly-easy-menu
  (let ((C '(sly-connected-p)))
    `("SLY"
      [ "Edit Definition..."       sly-edit-definition ,C ]
      [ "Return From Definition"   sly-pop-find-definition-stack ,C ]
      [ "Complete Symbol"          sly-complete-symbol ,C ]
      "--"
      ("Evaluation"
       [ "Eval Defun"              sly-eval-defun ,C ]
       [ "Eval Last Expression"    sly-eval-last-expression ,C ]
       [ "Eval And Pretty-Print"   sly-pprint-eval-last-expression ,C ]
       [ "Eval Region"             sly-eval-region ,C ]
       [ "Eval Region And Pretty-Print" sly-pprint-eval-region ,C ]
       [ "Interactive Eval..."     sly-interactive-eval ,C ]
       [ "Edit Lisp Value..."      sly-edit-value ,C ]
       [ "Call Defun"              sly-call-defun ,C ])
      ("Debugging"
       [ "Macroexpand Once..."     sly-macroexpand-1 ,C ]
       [ "Macroexpand All..."      sly-macroexpand-all ,C ]
       [ "Create Trace Buffer"     sly-redirect-trace-output ,C ]
       [ "Toggle Trace..."         sly-toggle-trace-fdefinition ,C ]
       [ "Untrace All"             sly-untrace-all ,C]
       [ "Disassemble..."          sly-disassemble-symbol ,C ]
       [ "Inspect..."              sly-inspect ,C ])
      ("Compilation"
       [ "Compile Defun"           sly-compile-defun ,C ]
       [ "Compile/Load File"       sly-compile-and-load-file ,C ]
       [ "Compile File"            sly-compile-file ,C ]
       [ "Compile Region"          sly-compile-region ,C ]
       "--"
       [ "Next Note"               sly-next-note t ]
       [ "Previous Note"           sly-previous-note t ]
       [ "Remove Notes"            sly-remove-notes t ]
       [ "List Notes"              sly-list-compiler-notes ,C ])
      ("Cross Reference"
       [ "Who Calls..."            sly-who-calls ,C ]
       [ "Who References... "      sly-who-references ,C ]
       [ "Who Sets..."             sly-who-sets ,C ]
       [ "Who Binds..."            sly-who-binds ,C ]
       [ "Who Macroexpands..."     sly-who-macroexpands ,C ]
       [ "Who Specializes..."      sly-who-specializes ,C ]
       [ "List Callers..."         sly-list-callers ,C ]
       [ "List Callees..."         sly-list-callees ,C ]
       [ "Next Location"           sly-next-location t ])
      ("Editing"
       [ "Check Parens"            check-parens t]
       [ "Update Indentation"      sly-update-indentation ,C]
       [ "Select Buffer"           sly-selector t])
      ("Profiling"
       [ "Toggle Profiling..."     sly-toggle-profile-fdefinition ,C ]
       [ "Profile Package"         sly-profile-package ,C]
       [ "Profile by Substring"    sly-profile-by-substring ,C ]
       [ "Unprofile All"           sly-unprofile-all ,C ]
       [ "Show Profiled"           sly-profiled-functions ,C ]
       "--"
       [ "Report"                  sly-profile-report ,C ]
       [ "Reset Counters"          sly-profile-reset ,C ])
      ("Documentation"
       [ "Describe Symbol..."      sly-describe-symbol ,C ]
       [ "Lookup Documentation..." sly-documentation-lookup t ]
       [ "Apropos..."              sly-apropos ,C ]
       [ "Apropos all..."          sly-apropos-all ,C ]
       [ "Apropos Package..."      sly-apropos-package ,C ]
       [ "Hyperspec..."            sly-hyperspec-lookup t ])
      "--"
      [ "Interrupt Command"        sly-interrupt ,C ]
      [ "Abort Async. Command"     sly-quit ,C ]
      [ "Sync Package & Directory" sly-sync-package-and-default-directory ,C]
      )))

(defvar sly-sldb-easy-menu
  (let ((C '(sly-connected-p)))
    `("SLDB"
      [ "Next Frame" sldb-down t ]
      [ "Previous Frame" sldb-up t ]
      [ "Toggle Frame Details" sldb-toggle-details t ]
      [ "Next Frame (Details)" sldb-details-down t ]
      [ "Previous Frame (Details)" sldb-details-up t ]
      "--"
      [ "Eval Expression..." sly-interactive-eval ,C ]
      [ "Eval in Frame..." sldb-eval-in-frame ,C ]
      [ "Eval in Frame (pretty print)..." sldb-pprint-eval-in-frame ,C ]
      [ "Inspect In Frame..." sldb-inspect-in-frame ,C ]
      [ "Inspect Condition Object" sldb-inspect-condition ,C ]
      "--"
      [ "Restart Frame" sldb-restart-frame ,C ]
      [ "Return from Frame..." sldb-return-from-frame ,C ]
      ("Invoke Restart"
       [ "Continue" sldb-continue ,C ]
       [ "Abort"    sldb-abort ,C ]
       [ "Step"      sldb-step ,C ]
       [ "Step next" sldb-next ,C ]
       [ "Step out"  sldb-out ,C ]
       )
      "--"
      [ "Quit (throw)" sldb-quit ,C ]
      [ "Break With Default Debugger" sldb-break-with-default-debugger ,C ])))

(easy-menu-define menubar-sly sly-mode-map "SLY" sly-easy-menu)

(defun sly-add-easy-menu ()
  (easy-menu-add sly-easy-menu 'sly-mode-map))

(add-hook 'sly-mode-hook 'sly-add-easy-menu)

(defun sly-sldb-add-easy-menu ()
  (easy-menu-define menubar-sly-sldb
    sldb-mode-map "SLDB" sly-sldb-easy-menu)
  (easy-menu-add sly-sldb-easy-menu 'sldb-mode-map))

(add-hook 'sldb-mode-hook 'sly-sldb-add-easy-menu)


;;;; Utilities (no not Paul Graham style)

;;; FIXME: this looks almost sly `sly-alistify', perhaps the two
;;;        functions can be merged.
(defun sly-group-similar (similar-p list)
  "Return the list of lists of 'similar' adjacent elements of LIST.
The function SIMILAR-P is used to test for similarity.
The order of the input list is preserved."
  (if (null list)
      nil
    (let ((accumulator (list (list (car list)))))
      (dolist (x (cdr list))
        (if (funcall similar-p x (caar accumulator))
            (push x (car accumulator))
          (push (list x) accumulator)))
      (reverse (mapcar #'reverse accumulator)))))

(defun sly-alistify (list key test)
  "Partition the elements of LIST into an alist.
KEY extracts the key from an element and TEST is used to compare
keys."
  (let ((alist '()))
    (dolist (e list)
      (let* ((k (funcall key e))
	     (probe (cl-assoc k alist :test test)))
	(if probe
	    (push e (cdr probe))
          (push (cons k (list e)) alist))))
    ;; Put them back in order.
    (cl-loop for (key . value) in (reverse alist)
             collect (cons key (reverse value)))))

;;;;; Misc.

(defun sly-length= (seq n)
  "Return (= (length SEQ) N)."
  (cl-etypecase seq
    (list
     (cond ((zerop n) (null seq))
           ((let ((tail (nthcdr (1- n) seq)))
              (and tail (null (cdr tail)))))))
    (sequence
     (= (length seq) n))))

(defun sly-length> (seq n)
  "Return (> (length SEQ) N)."
  (cl-etypecase seq
    (list (nthcdr n seq))
    (sequence (> (length seq) n))))

(defun sly-trim-whitespace (str)
  "Chomp leading and tailing whitespace from STR."
  ;; lited from http://www.emacswiki.org/emacs/ElispCookbook
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

;;;;; Buffer related

(defun sly-buffer-narrowed-p (&optional buffer)
  "Returns T if BUFFER (or the current buffer respectively) is narrowed."
  (with-current-buffer (or buffer (current-buffer))
    (let ((beg (point-min))
          (end (point-max))
          (total (buffer-size)))
      (or (/= beg 1) (/= end (1+ total))))))

(defun sly-column-max ()
  (save-excursion
    (goto-char (point-min))
    (cl-loop for column = (prog2 (end-of-line) (current-column) (forward-line))
             until (= (point) (point-max))
             maximizing column)))

;;;;; CL symbols vs. Elisp symbols.

(defun sly-cl-symbol-name (symbol)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match ":\\([^:]*\\)$" n)
	(let ((symbol-part (match-string 1 n)))
          (if (string-match "^|\\(.*\\)|$" symbol-part)
              (match-string 1 symbol-part)
            symbol-part))
      n)))

(defun sly-cl-symbol-package (symbol &optional default)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match "^\\([^:]*\\):" n)
	(match-string 1 n)
      default)))

(defun sly-qualify-cl-symbol-name (symbol-or-name)
  "Return a package-qualified string for SYMBOL-OR-NAME.
If SYMBOL-OR-NAME doesn't already have a package prefix the
current package is used."
  (let ((s (if (stringp symbol-or-name)
               symbol-or-name
             (symbol-name symbol-or-name))))
    (if (sly-cl-symbol-package s)
        s
      (format "%s::%s"
              (let* ((package (sly-current-package)))
                ;; package is a string like ":cl-user"
                ;; or "CL-USER", or "\"CL-USER\"".
                (if package
                    (sly-pretty-package-name package)
                  "CL-USER"))
              (sly-cl-symbol-name s)))))

;;;;; Moving, CL idiosyncracies aware (reader conditionals &c.)

(defmacro sly-point-moves-p (&rest body)
  "Execute BODY and return true if the current buffer's point moved."
  (declare (indent 0))
  (let ((pointvar (cl-gensym "point-")))
    `(let ((,pointvar (point)))
       (save-current-buffer ,@body)
       (/= ,pointvar (point)))))

(defun sly-forward-sexp (&optional count)
  "Like `forward-sexp', but understands reader-conditionals (#- and #+),
and skips comments."
  (dotimes (_i (or count 1))
    (sly-forward-cruft)
    (forward-sexp)))

(defconst sly-reader-conditionals-regexp
  ;; #!+, #!- are SBCL specific reader-conditional syntax.
  ;; We need this for the source files of SBCL itself.
  (regexp-opt '("#+" "#-" "#!+" "#!-")))

(defun sly-forward-reader-conditional ()
  "Move past any reader conditional (#+ or #-) at point."
  (when (looking-at sly-reader-conditionals-regexp)
    (goto-char (match-end 0))
    (let* ((plus-conditional-p (eq (char-before) ?+))
           (result (sly-eval-feature-expression
                    (condition-case e
                        (read (current-buffer))
                      (invalid-read-syntax
                       (signal 'sly-unknown-feature-expression (cdr e)))))))
      (unless (if plus-conditional-p result (not result))
        ;; skip this sexp
        (sly-forward-sexp)))))

(defun sly-forward-cruft ()
  "Move forward over whitespace, comments, reader conditionals."
  (while (sly-point-moves-p (skip-chars-forward " \t\n")
                              (forward-comment (buffer-size))
                              (inline (sly-forward-reader-conditional)))))

(defun sly-keywordify (symbol)
  "Make a keyword out of the symbol SYMBOL."
  (let ((name (downcase (symbol-name symbol))))
    (intern (if (eq ?: (aref name 0))
                name
              (concat ":" name)))))

(put 'sly-incorrect-feature-expression
     'error-conditions '(sly-incorrect-feature-expression error))

(put 'sly-unknown-feature-expression
     'error-conditions '(sly-unknown-feature-expression
                         sly-incorrect-feature-expression
                         error))

;; FIXME: let it crash
;; FIXME: the length=1 constraint is bogus
(defun sly-eval-feature-expression (e)
  "Interpret a reader conditional expression."
  (cond ((symbolp e)
         (memq (sly-keywordify e) (sly-lisp-features)))
        ((and (consp e) (symbolp (car e)))
         (funcall (let ((head (sly-keywordify (car e))))
                    (cl-case head
                      (:and #'cl-every)
                      (:or #'cl-some)
                      (:not
                       (lexical-let ((feature-expression e))
                         (lambda (f l)
                           (cond
                            ((sly-length= l 0) t)
                            ((sly-length= l 1) (not (apply f l)))
                            (t (signal 'sly-incorrect-feature-expression
                                       feature-expression))))))
                      (t (signal 'sly-unknown-feature-expression head))))
                  #'sly-eval-feature-expression
                  (cdr e)))
        (t (signal 'sly-incorrect-feature-expression e))))

;;;;; Extracting Lisp forms from the buffer or user

(defun sly-defun-at-point ()
  "Return the text of the defun at point."
  (apply #'buffer-substring-no-properties
         (sly-region-for-defun-at-point)))

(defun sly-region-for-defun-at-point ()
  "Return the start and end position of defun at point."
  (save-excursion
    (save-match-data
      (end-of-defun)
      (let ((end (point)))
        (beginning-of-defun)
        (list (point) end)))))

(defun sly-beginning-of-symbol ()
  "Move to the beginning of the CL-style symbol at point."
  (while (re-search-backward "\\(\\sw\\|\\s_\\|\\s\\.\\|\\s\\\\|[#@|]\\)\\="
                             (when (> (point) 2000) (- (point) 2000))
                             t))
  (re-search-forward "\\=#[-+.<|]" nil t)
  (when (and (looking-at "@") (eq (char-before) ?\,))
    (forward-char)))

(defun sly-end-of-symbol ()
  "Move to the end of the CL-style symbol at point."
  (re-search-forward "\\=\\(\\sw\\|\\s_\\|\\s\\.\\|#:\\|[@|]\\)*"))

(put 'sly-symbol 'end-op 'sly-end-of-symbol)
(put 'sly-symbol 'beginning-op 'sly-beginning-of-symbol)

(defun sly-symbol-start-pos ()
  "Return the starting position of the symbol under point.
The result is unspecified if there isn't a symbol under the point."
  (save-excursion (sly-beginning-of-symbol) (point)))

(defun sly-symbol-end-pos ()
  (save-excursion (sly-end-of-symbol) (point)))

(defun sly-bounds-of-symbol-at-point ()
  "Return the bounds of the symbol around point.
The returned bounds are either nil or non-empty."
  (let ((bounds (bounds-of-thing-at-point 'sly-symbol)))
    (if (and bounds
             (< (car bounds)
                (cdr bounds)))
        bounds)))

(defun sly-symbol-at-point ()
  "Return the name of the symbol at point, otherwise nil."
  ;; (thing-at-point 'symbol) returns "" in empty buffers
  (let ((bounds (sly-bounds-of-symbol-at-point)))
    (if bounds
        (buffer-substring-no-properties (car bounds)
                                        (cdr bounds)))))

(defun sly-bounds-of-sexp-at-point ()
  "Return the bounds sexp at point as a pair (or nil)."
  (or (sly-bounds-of-symbol-at-point)
      (and (equal (char-after) ?\()
           (member (char-before) '(?\' ?\, ?\@))
           ;; hide stuff before ( to avoid quirks with '( etc.
           (save-restriction
             (narrow-to-region (point) (point-max))
             (bounds-of-thing-at-point 'sexp)))
      (bounds-of-thing-at-point 'sexp)))

(defun sly-sexp-at-point ()
  "Return the sexp at point as a string, otherwise nil."
  (let ((bounds (sly-bounds-of-sexp-at-point)))
    (if bounds
        (buffer-substring-no-properties (car bounds)
                                        (cdr bounds)))))

(defun sly-sexp-at-point-or-error ()
  "Return the sexp at point as a string, othwise signal an error."
  (or (sly-sexp-at-point) (error "No expression at point.")))

(defun sly-string-at-point ()
  "Returns the string at point as a string, otherwise nil."
  (let ((sexp (sly-sexp-at-point)))
    (if (eql (char-syntax (aref sexp 0)) ?\")
        sexp
      nil)))

(defun sly-string-at-point-or-error ()
  "Return the sexp at point as a string, othwise signal an error."
  (or (sly-string-at-point) (error "No string at point.")))

(defun sly-input-complete-p (start end)
  "Return t if the region from START to END contains a complete sexp."
  (save-excursion
    (goto-char start)
    (cond ((looking-at "\\s *['`#]?[(\"]")
           (ignore-errors
             (save-restriction
               (narrow-to-region start end)
               ;; Keep stepping over blanks and sexps until the end of
               ;; buffer is reached or an error occurs. Tolerate extra
               ;; close parens.
               (cl-loop do (skip-chars-forward " \t\r\n)")
                        until (eobp)
                        do (forward-sexp))
               t)))
          (t t))))


;;;; sly.el in pretty colors

(cl-loop for sym in (list 'sly-def-connection-var
                          'sly-define-channel-type
                          'sly-define-channel-method
                          'define-sly-contrib
                          'sly-defun-if-undefined
                          'sly-defmacro-if-undefined)
         for regexp = (format "(\\(%S\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"
                              sym)
         do (font-lock-add-keywords
             'emacs-lisp-mode
             `((,regexp (1 font-lock-keyword-face)
                        (2 font-lock-variable-name-face)))))

;;;; Finishing up

(eval-when-compile
  (require 'bytecomp))

(defun sly--byte-compile (symbol)
  (require 'bytecomp) ;; tricky interaction between autoload and let.
  (let ((byte-compile-warnings '()))
    (byte-compile symbol)))

(defun sly--compile-hotspots ()
  (mapc (lambda (sym)
          (cond ((fboundp sym)
                 (unless (byte-code-function-p (symbol-function sym))
                   (sly--byte-compile sym)))
                (t (error "%S is not fbound" sym))))
        '(sly-alistify
          sly-log-event
          sly-events-buffer
          sly-process-available-input
          sly-dispatch-event
          sly-net-filter
          sly-net-have-input-p
          sly-net-decode-length
          sly-net-read
          sly-print-apropos
          sly-insert-propertized
          sly-beginning-of-symbol
          sly-end-of-symbol
          sly-eval-feature-expression
          sly-forward-sexp
          sly-forward-cruft
          sly-forward-reader-conditional)))

(sly--compile-hotspots)

(run-hooks 'sly-load-hook)
(provide 'sly)

(sly-setup)

;; Local Variables:
;; outline-regexp: ";;;;+"
;; indent-tabs-mode: nil
;; coding: latin-1-unix
;; End:
;;; sly.el ends here
