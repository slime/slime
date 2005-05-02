;;; -*- mode: emacs-lisp; mode: outline-minor; outline-regexp: ";;;;+"; indent-tabs-mode: nil -*-
;; slime.el -- Superior Lisp Interaction Mode for Emacs
;;;; License
;;     Copyright (C) 2003  Eric Marsden, Luke Gorrie, Helmut Eller
;;     Copyright (C) 2004  Luke Gorrie, Helmut Eller
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
;;   Lisp.
;;
;;   The `slime-mode' minor-mode complementing `lisp-mode'. This new
;;   mode includes many commands for interacting with the Common Lisp
;;   process.
;;
;;   Common Lisp REPL (Read-Eval-Print Loop) written in Emacs Lisp,
;;   similar to `ielm'.
;;
;;   Common Lisp debugger written in Emacs Lisp. The debugger pops up
;;   an Emacs buffer similar to the Emacs/Elisp debugger.
;;
;;   Trapping compiler messages and creating annotations in the source
;;   file on the appropriate forms.
;;
;; SLIME is compatible with GNU Emacs 20 and 21 and XEmacs 21. In
;; order to run SLIME requires a supporting Lisp server called
;; Swank. Swank is distributed with slime.el and will automatically be
;; started in a normal installation.


;;;; Dependencies and setup

(eval-and-compile
  (require 'cl)
  (unless (fboundp 'define-minor-mode)
    (require 'easy-mmode)
    (defalias 'define-minor-mode 'easy-mmode-define-minor-mode)))
(require 'comint)
(require 'timer)
(require 'pp)
(require 'hideshow)
(require 'hyperspec)
(require 'font-lock)
(when (featurep 'xemacs)
  (require 'overlay))
(require 'easymenu)

(defvar slime-use-autodoc-mode nil
  "When non-nil always enabled slime-autodoc-mode in slime-mode.")

(defun* slime-setup (&key autodoc typeout-frame)
  "Setup Emacs so that lisp-mode buffers always use SLIME."
  (add-hook 'lisp-mode-hook 'slime-lisp-mode-hook)
  (when typeout-frame
    (add-hook 'slime-connected-hook 'slime-ensure-typeout-frame))
  (setq slime-use-autodoc-mode autodoc))

(defun slime-lisp-mode-hook ()
  (slime-mode 1)
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function)
  (when slime-use-autodoc-mode
    (slime-autodoc-mode 1)))

(eval-and-compile 
  (defvar slime-path
    (let ((path (or (locate-library "slime") load-file-name)))
      (and path (file-name-directory path)))
    "Directory containing the Slime package.
This is used to load the supporting Common Lisp library, Swank.
The default value is automatically computed from the location of the
Emacs Lisp package."))


;;;; Customize groups
;;
;;;;; slime

(defgroup slime nil
  "Interaction with the Superior Lisp Environment."
  :prefix "slime-"
  :group 'applications)

;;;;; slime-ui

(defgroup slime-ui nil
  "Interaction with the Superior Lisp Environment."
  :prefix "slime-"
  :group 'slime)

(defcustom slime-truncate-lines t
  "Set `truncate-lines' in popup buffers.
This applies to buffers that present lines as rows of data, such as
debugger backtraces and apropos listings."
  :type 'boolean
  :group 'slime-ui)

(defcustom slime-update-modeline-package t
  "Automatically update the Lisp package name in the minibuffer.
This is done with a text-search that runs on an idle timer."
  :type 'boolean
  :group 'slime-ui)

(defcustom slime-kill-without-query-p nil
  "If non-nil, kill SLIME processes without query when quitting Emacs.
This applies to the *inferior-lisp* buffer and the network connections."
  :type 'boolean
  :group 'slime-ui)

(defcustom slime-startup-animation t
  "Enable the startup animation."
  :type '(choice (const :tag "Enable" t) (const :tag "Disable" nil))
  :group 'slime-ui)

;;;;; slime-lisp

(defgroup slime-lisp nil
  "Lisp server configuration."
  :prefix "slime-"
  :group 'slime)

;; XXX How can we get rid of this? I think only CMUCL needs it.
;;     -luke (17/Jul/2004)
(defcustom slime-multiprocessing nil
  "Instruct the Lisp system to initialize multiprocessing on startup.
You may need to enable this in order to use threads with SLIME."
  :type 'boolean
  :group 'slime-lisp)

(defcustom slime-backend "swank-loader.lisp"
  "The name of the Lisp file that loads the Swank server.
This name is interpreted relative to the directory containing
slime.el, but could also be set to an absolute filename."
  :type 'string
  :group 'slime-lisp)

(defcustom slime-connected-hook nil
  "List of functions to call when SLIME connects to Lisp."
  :type 'hook
  :group 'slime-lisp)

(defcustom slime-translate-to-lisp-filename-function 'identity
  "Function to use for translating Emacs filenames to Lisp filenames.
The function recieves a string as argument and should return string.
No suitable functions are ready-made, you have to write one yourself."
  :type 'function
  :group 'slime-lisp)

(defcustom slime-translate-from-lisp-filename-function 'identity
  "Function to use for translating Lisp filenames to Emacs filenames.
See also `slime-translate-to-lisp-filename-function'."
  :type 'function
  :group 'slime-lisp)

;;;;; slime-mode

(defgroup slime-mode nil
  "Settings for slime-mode Lisp source buffers."
  :prefix "slime-"
  :group 'slime)

(defcustom slime-edit-definition-fallback-function nil
  "Function to call when edit-definition fails to find the source itself.
The function is called with the definition name, a string, as its argument.

If you want to fallback on TAGS you can set this to `find-tags' or
`slime-edit-definition-with-etags'."
  :type 'symbol
  :group 'slime-mode-mode
  :options '(nil 
             slime-edit-definition-with-etags
             find-tags))

(defcustom slime-compilation-finished-hook 'slime-maybe-list-compiler-notes
  "Hook called with a list of compiler notes after a compilation."
  :group 'slime-mode
  :type 'hook
  :options '(slime-maybe-list-compiler-notes
             slime-list-compiler-notes 
             slime-maybe-show-xrefs-for-notes))

(defcustom slime-complete-symbol-function 'slime-complete-symbol*
  "Function to perform symbol completion."
  :group 'slime-mode
  :type '(choice (const :tag "Simple" slime-simple-complete-symbol)
                 (const :tag "Compound" slime-complete-symbol*)
                 (const :tag "Fuzzy" slime-fuzzy-complete-symbol)))

(defcustom slime-complete-symbol*-fancy nil
  "Use information from argument lists for DWIM'ish symbol completion."
  :group 'slime-mode
  :type 'boolean)

(defcustom slime-space-information-p t
  "Have the SPC key offer arglist information."
  :type 'boolean
  :group 'slime-mode)

(defcustom slime-display-compilation-output t
  "Display the REPL buffer before compiling files."
  :type '(choice (const :tag "Enable" t) (const :tag "Disable" nil))
  :group 'slime-mode)

(defcustom slime-sbcl-manual-root "http://www.sbcl.org/manual/"
  "*The base URL of the SBCL manual, for documentation lookup."
  :type 'string
  :group 'slime-mode)

;;;;; slime-mode-faces

(defgroup slime-mode-faces nil
  "Faces in slime-mode source code buffers."
  :prefix "slime-"
  :group 'slime-mode)

(defun slime-underline-color (color)
  "Return a legal value for the :underline face attribute based on COLOR."
  ;; In XEmacs the :underline attribute can only be a boolean.
  ;; In GNU it can be the name of a colour.
  (if (featurep 'xemacs)
      (if color t nil)
    color))

(defface slime-error-face
  `((((class color) (background light))
     (:underline ,(slime-underline-color "red")))
    (((class color) (background dark))
     (:underline ,(slime-underline-color "red")))
    (t (:underline t)))
  "Face for errors from the compiler."
  :group 'slime-mode-faces)

(defface slime-warning-face
  `((((class color) (background light))
     (:underline ,(slime-underline-color "orange")))
    (((class color) (background dark))
     (:underline ,(slime-underline-color "coral")))
    (t (:underline t)))
  "Face for warnings from the compiler."
  :group 'slime-mode-faces)

(defface slime-style-warning-face
  `((((class color) (background light))
     (:underline ,(slime-underline-color "brown")))
    (((class color) (background dark))
     (:underline ,(slime-underline-color "gold")))
    (t (:underline t)))
  "Face for style-warnings from the compiler."
  :group 'slime-mode-faces)

(defface slime-note-face
  `((((class color) (background light))
     (:underline ,(slime-underline-color "brown4")))
    (((class color) (background dark))
     (:underline ,(slime-underline-color "light goldenrod")))
    (t (:underline t)))
  "Face for notes from the compiler."
  :group 'slime-mode-faces)

(defun slime-face-inheritance-possible-p ()
  "Return true if the :inherit face attribute is supported." 
  (assq :inherit custom-face-attributes))

(defface slime-highlight-face
  (if (slime-face-inheritance-possible-p)
      '((t (:inherit highlight :underline nil)))
    '((((class color) (background light))
       (:background "darkseagreen2"))
      (((class color) (background dark))
       (:background "darkolivegreen"))
      (t (:inverse-video t))))
  "Face for compiler notes while selected."
  :group 'slime-mode-faces)

;;;;; sldb

(defgroup slime-debugger nil
  "Backtrace options and fontification."
  :prefix "sldb-"
  :group 'slime)

(defcustom sldb-enable-styled-backtrace t "Enable faces in slime backtrace" 
  :type '(choice 
	  (const :tag "Enable" t)
	  (const :tag "Disable" nil))
  :group 'slime-debugger)

(defcustom sldb-show-catch-tags t "Show catch tags in frames" 
  :type '(choice 
	  (const :tag "Show" t)
	  (const :tag "Don't show" nil))
  :group 'slime-debugger)

(defmacro def-sldb-faces (&rest faces)
  "Define the set of SLDB faces.
Each face specifiation is (NAME DESCRIPTION &optional PROPERTIES).
NAME is a symbol; the face will be called sldb-NAME-face.
DESCRIPTION is a one-liner for the customization buffer.
PROPERTIES specifies any default face properties."
  `(progn ,@(loop for face in faces
                  collect `(def-sldb-face ,@face))))

(defmacro def-sldb-face (name description &optional default)
  (let ((facename (intern (format "sldb-%s-face" (symbol-name name)))))
    `(defface ,facename
       (list (list t ,default))
      ,(format "Face for %s." description)
      :group 'slime-debugger)))

(def-sldb-faces
  (topline        "the top line describing the error")
  (condition      "the condition class")
  (section        "the labels of major sections in the debugger buffer")
  (frame-label    "backtrace frame numbers")
  (restart-type   "restart names."
                  (if (slime-face-inheritance-possible-p)
                      '(:inherit font-lock-keyword-face)))
  (restart        "restart descriptions")
  (restart-number "restart numbers (correspond to keystrokes to invoke)"
                  '(:bold t))
  (frame-line     "function names and arguments in the backtrace")
  (detailed-frame-line
   "function names and arguments in a detailed (expanded) frame")
  (local-name     "local variable names")
  (local-value    "local variable values")
  (catch-tag      "catch tags")
  (reference      "documentation references" '(:underline t)))

;;;;; slime-repl

(defgroup slime-repl nil
  "The Read-Eval-Print Loop (*slime-repl* buffer)."
  :prefix "slime-repl-"
  :group 'slime)

(defcustom slime-repl-shortcut-dispatch-char ?\,
  "Character used to distinguish repl commands from lisp forms."
  :type '(character)
  :group 'slime-repl)

(defface slime-repl-prompt-face
  (if (slime-face-inheritance-possible-p)
      '((t (:inherit font-lock-keyword-face)))
    '((((class color) (background light)) (:foreground "Purple"))
      (((class color) (background dark)) (:foreground "Cyan"))
      (t (:weight bold))))
  "Face for the prompt in the SLIME REPL."
  :group 'slime-repl)

(defface slime-repl-output-face
  (if (slime-face-inheritance-possible-p)
      '((t (:inherit font-lock-string-face)))
    '((((class color) (background light)) (:foreground "RosyBrown"))
      (((class color) (background dark)) (:foreground "LightSalmon"))
      (t (:slant italic))))
  "Face for Lisp output in the SLIME REPL."
  :group 'slime-repl)

(defface slime-repl-input-face
  '((t (:bold t)))
  "Face for previous input in the SLIME REPL."
  :group 'slime-repl)

(defface slime-repl-result-face
  '((t ()))
  "Face for the result of an evaluation in the SLIME REPL."
  :group 'slime-repl)


;;;; Minor modes
;;;;; slime-mode

(define-minor-mode slime-mode
  "\\<slime-mode-map>\
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
\\[slime-edit-definition]	- Edit the definition of the function called at point.
\\[slime-pop-find-definition-stack]	- Pop the definition stack to go back from a definition.

Documentation commands:
\\[slime-describe-symbol]	- Describe symbol.
\\[slime-apropos]	- Apropos search.
\\[slime-disassemble-symbol]	- Disassemble a function.

Evaluation commands:
\\[slime-eval-defun]	- Evaluate top-level from containing point.
\\[slime-eval-last-expression]	- Evaluate sexp before point.
\\[slime-pprint-eval-last-expression]	- Evaluate sexp before point, pretty-print result.

Full set of commands:
\\{slime-mode-map}"
  nil
  nil
  ;; Fake binding to coax `define-minor-mode' to create the keymap
  '((" " 'undefined)))

(make-variable-buffer-local
 (defvar slime-modeline-package nil
   "The Lisp package to show in the modeline.
This is automatically updated based on the buffer/point."))

(defun slime-update-modeline-package ()
  (ignore-errors
    (when (and slime-update-modeline-package
               (eq major-mode 'lisp-mode)
               slime-mode)
      (let ((package (slime-current-package)))
        (when package
          (setq slime-modeline-package
                (slime-pretty-package-name package)))))))

(defun slime-pretty-package-name (name)
  "Return a pretty version of a package name designator (as a string)."
  (let ((name (cond ((string-match "^:\\(.*\\)$" name)    
                     (match-string 1 name))
                    ((string-match "^\"\\(.*\\)\"$" name) 
                     (match-string 1 name))
                    (t name))))
    (format "%s" (read name))))

(when slime-update-modeline-package
  (run-with-idle-timer 0.2 0.2 'slime-update-modeline-package))

;;;;; inferior-slime-mode
(define-minor-mode inferior-slime-mode
  "\\<slime-mode-map>\
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
		((slime-modeline-package (":" slime-modeline-package) "")
		 slime-state-name))))

(add-to-list 'minor-mode-alist
             '(inferior-slime-mode
               (" Inf-Slime" slime-state-name)))

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
  (save-excursion
    (goto-char start)
    (cond ((looking-at "\\s *['`#]?(")
           (ignore-errors
             (save-restriction
               (narrow-to-region start end)
               ;; Keep stepping over blanks and sexps until the end of
               ;; buffer is reached or an error occurs. Tolerate extra
               ;; close parens.
               (loop do (skip-chars-forward " \t\r\n)")
                     until (eobp)
                     do (forward-sexp))
               t)))
          (t t))))

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


;;;;; Key bindings

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
    ("\M-\C-i" slime-complete-symbol :inferior t)
    ("\C-i" slime-complete-symbol :prefixed t :inferior t)
    ("\M-i" slime-fuzzy-complete-symbol :prefixed t :inferior t)
    ("\M-." slime-edit-definition :inferior t :sldb t)
    ("\C-x4." slime-edit-definition-other-window :inferior t :sldb t)
    ("\C-x5." slime-edit-definition-other-frame :inferior t :sldb t)
    ("\M-," slime-pop-find-definition-stack :inferior t :sldb t)
    ("\M-*" slime-pop-find-definition-stack :inferior t :sldb t)
    ("\C-q" slime-close-parens-at-point :prefixed t :inferior t)
    ("\C-c\M-q" slime-reindent-defun :inferior t)
    ;; Evaluating
    ("\C-x\C-e" slime-eval-last-expression :inferior t)
    ("\C-x\M-e" slime-eval-last-expression-display-output :inferior t)
    ("\C-p" slime-pprint-eval-last-expression :prefixed t :inferior t)
    ("\C-r" slime-eval-region :prefixed t :inferior t)
    ("\C-\M-x" slime-eval-defun)
    (":"    slime-interactive-eval :prefixed t :sldb t)
    ("\C-e" slime-interactive-eval :prefixed t :sldb t :inferior t)
    ("E"    slime-edit-value :prefixed t :sldb t :inferior t)
    ("\C-z" slime-switch-to-output-buffer :prefixed t :sldb t)
    ("\C-b" slime-interrupt :prefixed t :inferior t :sldb t)
    ("\M-g" slime-quit :prefixed t :inferior t :sldb t)
    ;; Documentation
    (" " slime-space :inferior t)
    ("\C-s" slime-complete-form :prefixed t :inferior t)
    ("\C-f" slime-describe-function :prefixed t :inferior t :sldb t)
    ("\M-d" slime-disassemble-symbol :prefixed t :inferior t :sldb t)
    ("\C-t" slime-toggle-trace-fdefinition :prefixed t :sldb t)
    ("\C-u" slime-undefine-function :prefixed t)
    ("\C-m" slime-macroexpand-1 :prefixed t :inferior t)
    ("\M-m" slime-macroexpand-all :prefixed t :inferior t)
    ("\M-0" slime-restore-window-configuration :prefixed t :inferior t)
    ([(control meta ?\.)] slime-next-location :inferior t)
    ;; Emacs20 on LinuxPPC signals a 
    ;; "Invalid character: 400000040, 2147479172, 0xffffffd8"
    ;; for "\C- ".
    ;; ("\C- " slime-next-location :prefixed t :inferior t)
    ("~" slime-sync-package-and-default-directory :prefixed t :inferior t)
    ("\M-p" slime-repl-set-package :prefixed t :inferior t)
    ;; Cross reference
    ("<" slime-list-callers :prefixed t :inferior t :sldb t)
    (">" slime-list-callees :prefixed t :inferior t :sldb t)
    ;; "Other"
    ("\I"  slime-inspect :prefixed t :inferior t :sldb t)
    ("\C-]" slime-close-all-sexp :prefixed t :inferior t :sldb t)
    ("\C-xt" slime-list-threads :prefixed t :inferior t :sldb t)
    ("\C-xc" slime-list-connections :prefixed t :inferior t :sldb t)
    ;; Shadow unwanted bindings from inf-lisp
    ("\C-a" slime-nop :prefixed t :inferior t :sldb t)
    ("\C-v" slime-nop :prefixed t :inferior t :sldb t)))

(defun slime-nop ()
  "The null command. Used to shadow currently-unused keybindings."
  (interactive)
  (call-interactively 'undefined))

(defvar slime-doc-map (make-sparse-keymap)
  "Keymap for documentation commands. Bound to a prefix key.")

(defvar slime-doc-bindings
  '((?a slime-apropos)
    (?z slime-apropos-all)
    (?p slime-apropos-package)
    (?d slime-describe-symbol)
    (?f slime-describe-function)
    (?h slime-hyperspec-lookup)
    (?~ common-lisp-hyperspec-format)))
  
(defvar slime-who-map (make-sparse-keymap)
  "Keymap for who-xref commands. Bound to a prefix key.")

(defvar slime-who-bindings
  '((?c slime-who-calls)
    (?w slime-calls-who)
    (?r slime-who-references)
    (?b slime-who-binds)
    (?s slime-who-sets)
    (?m slime-who-macroexpands)
    (?a slime-who-specializes)))

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
    [(meta control ?m)] 'inferior-slime-closing-return)
  ;; Documentation
  (setq slime-doc-map (make-sparse-keymap))
  (loop for (key command) in slime-doc-bindings
        do (progn
             ;; We bind both unmodified and with control.
             (define-key slime-doc-map (string key) command)
             (unless (equal key ?h)     ; But don't bind C-h
               (let ((modified (slime-control-modified-char key)))
                 (define-key slime-doc-map (string modified) command)))))
  ;; C-c C-d is the prefix for the doc map.
  (slime-define-key "\C-d" slime-doc-map :prefixed t :inferior t)
  ;; Who-xref
  (setq slime-who-map (make-sparse-keymap))
  (loop for (key command) in slime-who-bindings
        do (progn
             ;; We bind both unmodified and with control.
             (define-key slime-who-map (string key) command)
             (let ((modified (slime-control-modified-char key)))
                 (define-key slime-who-map (string modified) command))))
  ;; C-c C-w is the prefix for the who-xref map.
  (slime-define-key "\C-w" slime-who-map :prefixed t :inferior t))

(defun slime-control-modified-char (char)
  "Return the control-modified version of CHAR."
  ;; Maybe better to just bitmask it?
  (car (read-from-string (format "?\\C-%c" char))))

(slime-init-keymaps)


;;;;; Pull-down menu

(defvar slime-easy-menu
  (let ((C '(slime-connected-p)))
    `("SLIME"
      [ "Edit Definition..."       slime-edit-definition ,C ]
      [ "Return From Definition"   slime-pop-find-definition-stack ,C ]
      [ "Complete Symbol"          slime-complete-symbol ,C ]
      [ "Fuzzy Complete Symbol"    slime-fuzzy-complete-symbol ,C ]
      [ "Complete Form"            slime-complete-form ,C ]
      [ "Show REPL"                slime-switch-to-output-buffer ,C ]
      "--"
      ("Evaluation"
       [ "Eval Defun"              slime-eval-defun ,C ]
       [ "Eval Last Expression"    slime-eval-last-expression ,C ]
       [ "Eval And Pretty-Print"   slime-pprint-eval-last-expression ,C ]
       [ "Eval Region"             slime-eval-region ,C ]
       [ "Scratch Buffer"          slime-scratch ,C ]
       [ "Interactive Eval..."     slime-interactive-eval ,C ]
       [ "Edit Lisp Value..."      slime-edit-value ,C ])
      ("Debugging"
       [ "Macroexpand Once..."     slime-macroexpand-1 ,C ]
       [ "Macroexpand All..."      slime-macroexpand-all ,C ]
       [ "Toggle Trace..."         slime-toggle-trace-fdefinition ,C ]
       [ "Disassemble..."          slime-disassemble-symbol ,C ]
       [ "Inspect..."              slime-inspect ,C ])
      ("Compilation"
       [ "Compile Defun"           slime-compile-defun ,C ]
       [ "Compile/Load File"       slime-compile-and-load-file ,C ]
       [ "Compile File"            slime-compile-file ,C ]
       [ "Compile Region"          slime-compile-region ,C ]
       [ "Compile System"          slime-load-system ,C ]
       "--"
       [ "Next Note"               slime-next-note t ]
       [ "Previous Note"           slime-previous-note t ]
       [ "Remove Notes"            slime-remove-notes t ]
       [ "List Notes"              slime-list-compiler-notes ,C ])
      ("Cross Reference"
       [ "Who Calls..."            slime-who-calls ,C ]
       [ "Who References... "      slime-who-references ,C ]
       [ "Who Sets..."             slime-who-sets ,C ]
       [ "Who Binds..."            slime-who-binds ,C ]
       [ "Who Macroexpands..."     slime-who-macroexpands ,C ]
       [ "Who Specializes..."      slime-who-specializes ,C ]
       [ "List Callers..."         slime-list-callers ,C ]
       [ "List Callees..."         slime-list-callees ,C ]
       [ "Next Location"           slime-next-location t ])
      ("Editing"
       [ "Close All Parens"        slime-close-all-sexp t]
       [ "Check Parens"            check-parens t]
       [ "Update Indentation"      slime-update-indentation ,C]
       [ "Select Buffer"           slime-selector t])
      ("Profiling"
       [ "Toggle Profiling..."     slime-toggle-profile-fdefinition ,C ]
       [ "Profile Package"         slime-profile-package ,C]
       [ "Unprofile All"           slime-unprofile-all ,C ]
       [ "Show Profiled"           slime-profiled-functions ,C ]
       "--"
       [ "Report"                  slime-profile-report ,C ]
       [ "Reset Counters"          slime-profile-reset ,C ])
      ("Documentation"
       [ "Describe Symbol..."      slime-describe-symbol ,C ]
       [ "Apropos..."              slime-apropos ,C ]
       [ "Apropos all..."          slime-apropos-all ,C ]
       [ "Apropos Package..."      slime-apropos-package ,C ]
       [ "Hyperspec..."            slime-hyperspec-lookup t ])
      "--"
      [ "Interrupt Command"        slime-interrupt ,C ]
      [ "Abort Async. Command"     slime-quit ,C ]
      [ "Sync Package & Directory" slime-sync-package-and-default-directory ,C]
      [ "Set Package in REPL"      slime-repl-set-package ,C]
      )))

(defvar slime-repl-easy-menu
  (let ((C '(slime-connected-p)))
    `("REPL"
      [ "Send Input"             slime-repl-return ,C ]
      [ "Close and Send Input "  slime-repl-closing-return ,C ]
      [ "Interrupt Lisp process" slime-interrupt ,C ]
      "--"
      [ "Previous Input"         slime-repl-previous-input t ]
      [ "Next Input"             slime-repl-next-input t ]
      [ "Goto Previous Prompt "  slime-repl-previous-prompt t ]
      [ "Goto Next Prompt "      slime-repl-next-prompt t ]
      [ "Clear Last Output"      slime-repl-clear-output t ]
      [ "Clear Buffer "          slime-repl-clear-buffer t ])))
      
(defvar slime-sldb-easy-menu
  (let ((C '(slime-connected-p)))
    `("SLDB"
      [ "Next Frame" sldb-down t ]
      [ "Previous Frame" sldb-up t ]
      [ "Toggle Frame Details" sldb-toggle-details t ]
      [ "List Locals" sldb-list-locals ,C ]
      [ "Next Frame (Details)" sldb-details-down t ]
      [ "Previous Frame (Details)" sldb-details-up t ]
      "--"
      [ "Eval Expression..." slime-interactive-eval ,C ]
      [ "Eval in Frame..." sldb-eval-in-frame ,C ]
      [ "Eval in Frame (pretty print)..." sldb-pprint-eval-in-frame ,C ]
      [ "Inspect In Frame..." sldb-inspect-in-frame ,C ]
      [ "Inspect Condition Object" sldb-inspect-condition ,C ]
      [ "Print Condition to REPL" sldb-print-condition t ]
      "--"
      [ "Restart Frame" sldb-restart-frame ,C ]
      [ "Return from Frame..." sldb-return-from-frame ,C ]
      ("Invoke Restart"
       [ "Continue" sldb-continue ,C ]
       [ "Abort"    sldb-abort ,C ]
       [ "Step"     sldb-step ,C ])
      "--"
      [ "Quit (throw)" sldb-quit ,C ]
      [ "Break With Default Debugger" sldb-break-with-default-debugger ,C ])))


(easy-menu-define menubar-slime slime-mode-map "SLIME" slime-easy-menu)

(add-hook 'slime-mode-hook
          (defun slime-add-easy-menu ()
            (easy-menu-add slime-easy-menu 'slime-mode-map)))

(add-hook 'slime-repl-mode-hook
          (defun slime-repl-add-easy-menu ()
            (easy-menu-define menubar-slime-repl slime-repl-mode-map
              "REPL" slime-repl-easy-menu)
            (easy-menu-add slime-repl-easy-menu 'slime-repl-mode-map)))

(add-hook 'sldb-mode-hook
          (defun slime-sldb-add-easy-menu ()
            (easy-menu-define menubar-slime-sldb sldb-mode-map
              "SLDB" slime-sldb-easy-menu)
            (easy-menu-add slime-sldb-easy-menu 'sldb-mode-map)))


;;;; Setup initial `slime-mode' hooks

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
  (when (and slime-mode (slime-connected-p))
    (slime-process-available-input)))

(defun slime-setup-command-hooks ()
  "Setup a buffer-local `pre-command-hook' to call `slime-pre-command-hook'."
  (make-local-hook 'pre-command-hook)
  (make-local-hook 'post-command-hook)
  (add-hook 'pre-command-hook 'slime-pre-command-hook)
  (add-hook 'post-command-hook 'slime-post-command-hook))

(add-hook 'slime-mode-hook 'slime-setup-command-hooks)


;;;; Framework'ey bits
;;;
;;; This section contains some standard SLIME idioms: basic macros,
;;; ways of showing messages to the user, etc. All the code in this
;;; file should use these functions when applicable.
;;;
;;;;; Syntactic sugar

(defmacro* when-let ((var value) &rest body)
  "Evaluate VALUE, and if the result is non-nil bind it to VAR and
evaluate BODY.

\(fn (VAR VALUE) &rest BODY)"
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
  "Define keys in KEYMAP. Each KEY-COMMAND is a list of (KEY COMMAND)."
  `(progn . ,(mapcar (lambda (k-c) `(define-key ,keymap . ,k-c))
		     key-command)))

(put 'slime-define-keys 'lisp-indent-function 1)

(defmacro* with-struct ((conc-name &rest slots) struct &body body)
  "Like with-slots but works only for structs.
\(fn (CONC-NAME &rest SLOTS) STRUCT &body BODY)"
  (flet ((reader (slot) (intern (concat (symbol-name conc-name)
					(symbol-name slot)))))
    (let ((struct-var (gensym "struct")))
      `(let ((,struct-var ,struct))
	 (symbol-macrolet
	     ,(mapcar (lambda (slot)
			(etypecase slot
			  (symbol `(,slot (,(reader slot) ,struct-var)))
			  (cons `(,(first slot) (,(reader (second slot)) 
						 ,struct-var)))))
		      slots)
	   . ,body)))))

(put 'with-struct 'lisp-indent-function 2)

;;;;; Very-commonly-used functions

;; Interface
(defun slime-message (format &rest args)
  "Like `message' but with special support for multi-line messages.
Single-line messages use the echo area."
  (if (slime-typeout-active-p)
      (apply #'slime-typeout-message format args)
    (if (or (featurep 'xemacs)
            (= emacs-major-version 20))
        (slime-display-message (apply #'format format args) "*SLIME Note*")
      (apply 'message format args))))

(defun slime-display-message (message buffer-name) 
  "Display MESSAGE in the echo area or in BUFFER-NAME.
Use the echo area if MESSAGE needs only a single line.  If the MESSAGE
requires more than one line display it in BUFFER-NAME and add a hook
to `slime-pre-command-actions' to remove the window before the next
command."
  (when (get-buffer-window buffer-name) (delete-windows-on buffer-name))
  (cond ((or (string-match "\n" message)
             (> (length message) (1- (frame-width))))
         (if (slime-typeout-active-p)
             (slime-typeout-message "%s" message)
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
                   slime-pre-command-actions))))
        (t (message "%s" message))))

(defun slime-create-message-window ()
  "Create a window at the bottom of the frame, above the minibuffer."
  (let ((previous (previous-window (minibuffer-window))))
    (when (<= (window-height previous) (* 2 window-min-height))
      (save-selected-window 
        (select-window previous)
        (enlarge-window (- (1+ (* 2 window-min-height))
                           (window-height previous)))))
    (split-window previous)))

;; Interface
(defun slime-background-message (format-string &rest format-args)
  "Display a message in passing.
This is like `slime-message', but less distracting because it
will never pop up a buffer or display multi-line messages.
It should be used for \"background\" messages such as argument lists."
  (if (slime-typeout-active-p)
      (slime-typeout-message (apply #'format format-string format-args))
    (let* ((msg (apply #'format format-string format-args)))
      (unless (minibuffer-window-active-p (minibuffer-window))
        (message  "%s" (slime-oneliner msg))))))

(defun slime-oneliner (string)
  "Return STRING truncated to fit in a single echo-area line."
  (substring string 0 (min (length string)
                           (or (position ?\n string) most-positive-fixnum)
                           (1- (frame-width)))))

;; Interface
(defun slime-set-truncate-lines ()
  "Apply `slime-truncate-lines' to the current buffer."
  (when slime-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

;; Interface
(defun slime-read-package-name (prompt &optional initial-value)
  "Read a package name from the minibuffer, prompting with PROMPT."
  (let ((completion-ignore-case t))
    (completing-read prompt (slime-bogus-completion-alist 
                             (slime-eval 
                              `(swank:list-all-package-names t)))
		     nil nil initial-value)))

;; Interface
(defun slime-read-symbol-name (prompt &optional query)
  "Either read a symbol name or choose the one at point.
The user is prompted if a prefix argument is in effect, if there is no
symbol at point, or if QUERY is non-nil.

This function avoids mistaking the REPL prompt for a symbol."
  (cond ((or current-prefix-arg query (not (slime-symbol-name-at-point)))
         (slime-read-from-minibuffer prompt (slime-symbol-name-at-point)))
        (t (slime-symbol-name-at-point))))

;; Interface
(defmacro slime-propertize-region (props &rest body)
  "Execute BODY and add PROPS to all the text it inserts.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY."
  (let ((start (gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
	 (add-text-properties ,start (point) ,props)))))

(put 'slime-propertize-region 'lisp-indent-function 1)

;; Interface
(defsubst slime-insert-propertized (props &rest args)
  "Insert all ARGS and then add text-PROPS to the inserted text."
  (slime-propertize-region props (apply #'insert args)))

(defun slime-indent-and-complete-symbol ()
  "Indent the current line and perform symbol completion.  First
indent the line. If indenting doesn't move point, complete the
symbol. If there's no symbol at the point, show the arglist for the
most recently enclosed macro or function."
  (interactive)
  (let ((pos (point)))
    (unless (get-text-property (line-beginning-position) 'slime-repl-prompt)
      (lisp-indent-line))
    (when (= pos (point))
      (cond ((save-excursion (re-search-backward "[^() \n\t\r]+\\=" nil t))
             (slime-complete-symbol))
            ((memq (char-before) '(?\t ?\ ))
             (slime-echo-arglist))))))

(defmacro slime-with-rigid-indentation (level &rest body)
  "Execute BODY and then rigidly indent its text insertions.
Assumes all insertions are made at point."
  (let ((start (gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
         (indent-rigidly ,start (point) ,level)))))

(put 'slime-with-rigid-indentation 'lisp-indent-function 1)

;;;;; Temporary popup buffers

(make-variable-buffer-local
 (defvar slime-temp-buffer-saved-window-configuration nil
   "The window configuration before the temp-buffer was displayed.
Buffer local in temp-buffers."))

(make-variable-buffer-local
 (defvar slime-temp-buffer-fingerprint nil
   "The window config \"fingerprint\" after displaying the buffer."))

;; Interface
(defun* slime-get-temp-buffer-create (name &key mode noselectp)
  "Return a fresh temporary buffer called NAME in MODE.
The buffer also uses the minor-mode `slime-temp-buffer-mode'. Pressing
`q' in the buffer will restore the window configuration to the way it
is when the buffer was created, i.e. when this function was called.

If NOSELECTP is true then the buffer is shown by `display-buffer',
otherwise it is shown and selected by `pop-to-buffer'."
  (let ((window-config (current-window-configuration)))
    (when (get-buffer name) (kill-buffer name))
    (with-current-buffer (get-buffer-create name)
      (when mode (funcall mode))
      (slime-temp-buffer-mode 1)
      (setq slime-temp-buffer-saved-window-configuration window-config)
      (let ((window (if noselectp
                        (display-buffer (current-buffer) t)
                      (pop-to-buffer (current-buffer))
                      (selected-window))))
        (setq slime-temp-buffer-fingerprint (slime-window-config-fingerprint)))
      (current-buffer))))

;; Interface
(defmacro* slime-with-output-to-temp-buffer ((name &optional mode)
                                             package &rest body)
  "Similar to `with-output-to-temp-buffer'.
Also saves the window configuration, and inherits the current
`slime-connection' in a buffer-local variable."
  `(let ((connection (slime-connection))
         (standard-output (slime-get-temp-buffer-create ,name :mode ',mode)))
     (prog1 (with-current-buffer standard-output ,@body)
       (with-current-buffer standard-output
         (setq slime-buffer-connection connection)
         (setq slime-buffer-package ,package)
         (goto-char (point-min))
         (slime-mode 1)
         (set-syntax-table lisp-mode-syntax-table)
         (setq buffer-read-only t)))))

(put 'slime-with-output-to-temp-buffer 'lisp-indent-function 2)

(define-minor-mode slime-temp-buffer-mode 
  "Mode for displaying read only stuff"
  nil
  " temp"
  '(("q" . slime-temp-buffer-quit)))

;; Interface
(defun slime-temp-buffer-quit ()
  "Kill the current buffer and restore the old window configuration.
See `slime-temp-buffer-dismiss'."
  (interactive)
  (let ((buf (current-buffer)))
    (slime-dismiss-temp-buffer)
    (kill-buffer buf)))

;; Interface
(defun slime-dismiss-temp-buffer ()
  "Dismiss the current temp buffer and restore previous window config.
Don't change the window configuration if it has been significantly
changed since the temp buffer was displayed."
  (when (equalp (slime-window-config-fingerprint)
                slime-temp-buffer-fingerprint)
    (set-window-configuration slime-temp-buffer-saved-window-configuration)))

(defun slime-window-config-fingerprint (&optional frame)
  "Return a fingerprint of the current window configuration.
Fingerprints are `equalp' if and only if they represent window
configurations that are very similar (same windows and buffers.)

Unlike window-configuration objects fingerprints are not sensitive to
the point moving and they can't be restored."
  (mapcar (lambda (window) (list window (window-buffer window)))
          (slime-frame-windows frame)))

(defun slime-frame-windows (&optional frame)
  "Return the list of windows in FRAME."
  (loop with last-window = (previous-window (frame-first-window frame))
        for window = (frame-first-window frame) then (next-window window)
        collect window
        until (eq window last-window)))

;;;;; Filename translation
;;;
;;; Filenames passed between Emacs and Lisp should be translated using
;;; these functions. This way users who run Emacs and Lisp on separate
;;; machines have a chance to integrate file operations somehow.

(defun slime-to-lisp-filename (filename)
  "Translate the string FILENAME to a Lisp filename.
See `slime-translate-to-lisp-filename-function'."
  (funcall slime-translate-to-lisp-filename-function
           ;; expand-file-name so that Lisp doesn't see ~foo/bar, etc
           (expand-file-name filename)))

(defun slime-from-lisp-filename (filename)
  "Translate the Lisp filename FILENAME to an Emacs filename.
See `slime-translate-from-lisp-filename-function'."
  (funcall slime-translate-from-lisp-filename-function filename))


;;;; Starting SLIME
;;;
;;; This section covers starting an inferior-lisp, compiling and
;;; starting the server, initiating a network connection.

;;;;; Entry points

(defvar slime-inferior-lisp-program-history '()
  "History list of command strings.  Used by `slime'.")

;; XXX: inferior-lisp-program isn't preloaded in XEmacs. maybe we
;; should use something else.
(defvar inferior-lisp-program "lisp" 
  "*Program name for invoking an inferior Lisp with for Inferior Lisp mode.")

(defun slime (&optional command buffer coding-system)
  "Start an inferior^_superior Lisp and connect to its Swank server."
  (interactive (list (if current-prefix-arg
			 (read-string "Run lisp: " inferior-lisp-program
                                      'slime-inferior-lisp-program-history))
                     "*inferior-lisp*"
                     (if (eq 16 (prefix-numeric-value current-prefix-arg))
                         (read-coding-system "set slime-coding-system: "
                                             slime-net-coding-system))))
  (let ((command (or (slime-find-lisp-implementation command)
                     inferior-lisp-program))
        (buffer (or buffer "*inferior-lisp*"))
        (coding-system (or coding-system slime-net-coding-system)))
    (let ((symbolic-lisp-name (slime-symbolic-lisp-name-p command)))
      (slime-check-coding-system coding-system)
      (setq slime-net-coding-system coding-system)
      (when (or (not (slime-bytecode-stale-p))
                (slime-urge-bytecode-recompile))
        (let ((proc (slime-maybe-start-lisp command buffer)))
          (slime-inferior-connect proc nil symbolic-lisp-name)
          (pop-to-buffer (process-buffer proc)))))))

(defun slime-connect (host port &optional kill-old-p symbolic-lisp-name)
  "Connect to a running Swank server."
  (interactive (list (read-from-minibuffer "Host: " "127.0.0.1")
                     (read-from-minibuffer "Port: " "4005" nil t)
                     (if (null slime-net-processes)
                         t
                       (y-or-n-p "Close old connections first? "))))
  (slime-check-coding-system)
  (when kill-old-p (slime-disconnect))
  (message "Connecting to Swank on port %S.." port)
  (let* ((process (slime-net-connect host port))
         (slime-dispatching-connection process))
    (slime-setup-connection process symbolic-lisp-name)))

(defun slime-start-and-load (filename &optional package)
  "Start Slime, if needed, load the current file and set the package."
  (interactive (list (expand-file-name (buffer-file-name))
                     (slime-find-buffer-package)))
  (cond ((slime-connected-p)
         (slime-load-file-set-package filename package))
        (t
         (lexical-let ((hook nil) (package package) (filename filename))
           (setq hook (lambda ()
                        (remove-hook 'slime-connected-hook hook)
                        (slime-load-file-set-package filename package)))
           (add-hook 'slime-connected-hook hook)
           (slime)))))

(defun slime-load-file-set-package (filename package)
  (let ((filename (slime-to-lisp-filename filename)))
    (slime-eval-async `(swank:load-file-set-package ,filename ,package)
                      (lambda (package)
                        (when package
                          (slime-repl-set-package (second package)))))))

(defmacro define-slime-dialect (name &optional program hook)
  "Define a command slime-dialect-NAME to start a specific Lisp.
PROGRAM is the command to start the inferior process.
HOOK is function which is run before the process is started."
  (let ((funsym (intern (format "slime-dialect-%s" name)))
        (hooksym (intern (format "slime-dialect-%s-hook" name)))
        (progsym (intern (format "slime-dialect-%s-program" name))))
    `(progn
       (defvar ,progsym ,program)
       (defvar ,hooksym ,hook)
       (defun ,funsym ()
         ,(format "Start up slime according to `%s'." progsym)
         (interactive)
         (let ((inferior-lisp-program ,progsym))
           (run-hooks ',hooksym)
           (call-interactively 'slime))))))

;;;;; Start inferior lisp
;;;
;;; Here is the protocol for starting SLIME:
;;;
;;;   0. Emacs recompiles/reloads slime.elc if it exists and is stale.
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

(defvar slime-connect-retry-timer nil
  "Timer object while waiting for an inferior-lisp to start.")

;;; Recompiling bytecode:

(defun slime-bytecode-stale-p ()
  "Return true if slime.elc is older than slime.el."
  (when-let (libfile (locate-library "slime"))
    (let* ((basename (file-name-sans-extension libfile))
           (sourcefile (concat basename ".el"))
           (bytefile (concat basename ".elc")))
      (and (file-exists-p bytefile)
           (file-newer-than-file-p sourcefile bytefile)))))

(defun slime-recompile-bytecode ()
  "Recompile and reload slime.
Warning: don't use this in XEmacs, it seems to crash it!"
  (interactive)
  (let ((sourcefile (concat (file-name-sans-extension (locate-library "slime"))
                            ".el"))
        (byte-compile-warning-types (remove 'cl-functions 
                                            byte-compile-warning-types)))
    (byte-compile-file sourcefile t)))

(defun slime-urge-bytecode-recompile ()
  "Urge the user to recompile slime.elc.
Return true if we have been given permission to continue."
  (if (featurep 'xemacs)
      ;; My XEmacs crashes and burns if I recompile/reload an elisp
      ;; file from itself. So they have to do it themself.
      (y-or-n-p "slime.elc is older than slime.el. Continue? ")
    (if (y-or-n-p "slime.elc is older than slime.el. Recompile/reload first? ")
        (progn (slime-recompile-bytecode) t)
      nil)))

(defun slime-abort-connection ()
  "Abort connection the current connection attempt."
  (interactive)
  (if (null slime-connect-retry-timer)
      (error "Not connected")
    (cancel-timer slime-connect-retry-timer)
    (message "Cancelled connection attempt.")))

;;; Starting the inferior Lisp and loading Swank:

(defun slime-maybe-start-lisp (command buffername)
  "Start an inferior lisp. Instruct it to load Swank."
  (cond ((not (comint-check-proc buffername))
         (slime-start-lisp command buffername (slime-init-command)))
        ((y-or-n-p "Create an additional *inferior-lisp*? ")
         (slime-start-lisp command (generate-new-buffer-name buffername)
                           (slime-init-command)))
        (t 
         (when-let (conn (find (get-buffer-process buffername)
                               slime-net-processes 
                               :key #'slime-inferior-process))
           (slime-net-close conn))
         (get-buffer-process buffername))))

(defun slime-init-command ()
  "Return a string to initialize Lisp."
  (let ((swank (slime-to-lisp-filename (if (file-name-absolute-p slime-backend)
                                           slime-backend
                                         (concat slime-path slime-backend))))
        (mp (if slime-multiprocessing "(swank:startup-multiprocessing)\n" "")))
    (format "(load %S :verbose t)\n%s" swank mp)))

(defun slime-start-lisp (command buffername init-string)
  "Start Lisp with COMMAND in BUFFERNAME and send INIT-STRING to it.
Return the new process."
  (let ((proc (slime-inferior-lisp command buffername)))
    (when slime-kill-without-query-p
      (process-kill-without-query proc))
    (when init-string
      (comint-send-string proc init-string)
    proc)))

(defun slime-inferior-lisp (command buffername)
  "Does the same as `inferior-lisp' but less ugly.
Return the created process."
  (let ((args (split-string command)))  ; XXX consider: cmucl -eval '(+ 1 2)'
    (with-current-buffer (get-buffer-create buffername)
      (comint-mode)
      (comint-exec (current-buffer) "inferior-lisp" (car args) nil (cdr args))
      (lisp-mode-variables t)
      (get-buffer-process (current-buffer)))))

(defun slime-inferior-connect (process &optional retries symbolic-lisp-name)
  "Start a Swank server in the inferior Lisp and connect."
  (when (file-regular-p (slime-swank-port-file))
    (delete-file (slime-swank-port-file)))
  (slime-start-swank-server process)
  (slime-read-port-and-connect process retries symbolic-lisp-name))

(defun slime-start-swank-server (process)
  "Start a Swank server on the inferior lisp."
  (let* ((encoding (slime-coding-system-cl-name slime-net-coding-system))
         (file (slime-to-lisp-filename (slime-swank-port-file))))
    (comint-send-string process 
                        (format "(swank:start-server %S :external-format %s)\n"
                                file encoding))))

(defun slime-swank-port-file ()
  "Filename where the SWANK server writes its TCP port number."
  (concat (file-name-as-directory
           (cond ((fboundp 'temp-directory) (temp-directory))
                 ((boundp 'temporary-file-directory) temporary-file-directory)
                 (t "/tmp/")))
          (format "slime.%S" (emacs-pid))))

(defun slime-read-port-and-connect (inferior-process retries &optional symbolic-lisp-name)
  (lexical-let ((process inferior-process)
                (retries retries)
                (attempt 0)
                (lisp-name symbolic-lisp-name))
    (labels
        ;; A small one-state machine to attempt a connection with
        ;; timer-based retries.
        ((attempt-connection
          ()
          (unless (active-minibuffer-window)
            (message "\
Polling %S.. (Abort with `M-x slime-abort-connection'.)"
                     (slime-swank-port-file)))
          (unless (slime-connected-p)
            (slime-set-state (format "[polling:%S]" (incf attempt))))
          (when slime-connect-retry-timer
            (cancel-timer slime-connect-retry-timer))
          (setq slime-connect-retry-timer nil) ; remove old timer
          (cond ((file-exists-p (slime-swank-port-file))
                 (let ((port (slime-read-swank-port)))
                   (delete-file (slime-swank-port-file))
                   (let ((c (slime-connect "127.0.0.1" port nil lisp-name)))
                     (slime-set-inferior-process c process))))
                ((and retries (zerop retries))
                 (message "Failed to connect to Swank."))
                (t
                 (when retries (decf retries))
                 (setq slime-connect-retry-timer
                       (run-with-timer 1 nil #'attempt-connection))))))
      (attempt-connection))))

(defun slime-read-swank-port ()
  "Read the Swank server port number from the `slime-swank-port-file'."
  (save-excursion
    (with-temp-buffer
      (insert-file-contents (slime-swank-port-file))
      (goto-char (point-min))
      (let ((port (read (current-buffer))))
        (assert (integerp port))
        port))))

(defun slime-hide-inferior-lisp-buffer ()
  "Display the REPL buffer instead of the *inferior-lisp* buffer."
  (let* ((buffer (if (slime-process) 
                     (process-buffer (slime-process))))
         (window (if buffer (get-buffer-window buffer)))
         (repl-buffer (slime-output-buffer t))
         (repl-window (get-buffer-window repl-buffer)))
    (when buffer
      (bury-buffer buffer))
    (cond (repl-window
           (when window
             (delete-window window)))
          (window
           (set-window-buffer window repl-buffer))
          (t
           (pop-to-buffer repl-buffer)
           (goto-char (point-max))))))

;;; Words of encouragement

(defun slime-user-first-name ()
  (let ((name (if (string= (user-full-name) "")
                  (user-login-name)
                (user-full-name))))
    (string-match "^[^ ]*" name)
    (capitalize (match-string 0 name))))

(defvar slime-words-of-encouragement
  `("Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    "Your hacking starts... NOW!"
    "May the source be with you!"
    "Take this REPL, brother, and may it serve you well."
    "Lemonodor-fame is but a hack away!"
    ,(format "%s, this could be the start of a beautiful program."
             (slime-user-first-name)))
  "Scientifically-proven optimal words of hackerish encouragement.")

(defun slime-random-words-of-encouragement ()
  "Return a string of hackerish encouragement."
  (eval (nth (random (length slime-words-of-encouragement))
             slime-words-of-encouragement)))


;;;; Networking
;;;
;;; This section covers the low-level networking: establishing
;;; connections and encoding/decoding protocol messages.
;;;
;;; Each SLIME protocol message beings with a 3-byte length header
;;; followed by an S-expression as text. The sexp must be readable
;;; both by Emacs and by Common Lisp, so if it contains any embedded
;;; code fragments they should be sent as strings.
;;;
;;; The set of meaningful protocol messages are not specified
;;; here. They are defined elsewhere by the event-dispatching
;;; functions in this file and in swank.lisp.

(defvar slime-net-processes nil
  "List of processes (sockets) connected to Lisps.")

(defvar slime-net-process-close-hooks '()
  "List of functions called when a slime network connection closes.
The functions are called with the process as their argument.")

(defvar slime-net-coding-system
  (find-if (cond ((featurep 'xemacs)
                  (if (fboundp 'find-coding-system) 
                      #'find-coding-system
                    (lambda (x) (eq x 'binary))))
                 (t #'coding-system-p))
           '(iso-latin-1-unix iso-8859-1-unix binary))
  "*Coding system used for network connections.
See also `slime-net-valid-coding-systems'.")

(defvar slime-net-valid-coding-systems
  '((iso-latin-1-unix nil :iso-latin-1-unix)
    (iso-8859-1-unix  nil :iso-latin-1-unix)
    (binary           nil :iso-latin-1-unix)
    (utf-8-unix       t   :utf-8-unix)
    (emacs-mule-unix  t   :emacs-mule-unix))
  "A list of valid coding systems. 
Each element is of the form: (NAME MULTIBYTEP CL-NAME)")

(defun slime-secret ()
  "Finds the magic secret from the user's home directory.
Returns nil if the file doesn't exist or is empty; otherwise the first
line of the file."
  (condition-case err
      (with-temp-buffer
	(insert-file-contents "~/.slime-secret")
	(goto-char (point-min))
	(buffer-substring (point-min) (line-end-position)))
    (file-error nil)))

;;; Interface
(defun slime-net-connect (host port)
  "Establish a connection with a CL."
  (let* ((inhibit-quit nil)
         (proc (open-network-stream "SLIME Lisp" nil host port))
         (buffer (slime-make-net-buffer " *cl-connection*")))
    (push proc slime-net-processes)
    (set-process-buffer proc buffer)
    (set-process-filter proc 'slime-net-filter)
    (set-process-sentinel proc 'slime-net-sentinel)
    (when slime-kill-without-query-p
      (process-kill-without-query proc))
    (when (fboundp 'set-process-coding-system)
      (set-process-coding-system proc 
                                 slime-net-coding-system
                                 slime-net-coding-system))
    (when-let (secret (slime-secret))
      (slime-net-send secret proc))
    proc))

(defun slime-make-net-buffer (name)
  "Make a buffer suitable for a network process."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (when (fboundp 'set-buffer-multibyte)
        (set-buffer-multibyte 
         (slime-coding-system-mulibyte-p slime-net-coding-system)))
      (buffer-disable-undo))
    buffer))

(defun slime-find-coding-system (&optional coding-system)
  (let* ((coding-system (or coding-system slime-net-coding-system))
         (props (assq coding-system slime-net-valid-coding-systems)))  
    (check-coding-system coding-system)
    (unless props
      (error "Invalid slime-net-coding-system: %s. %s"
             coding-system (mapcar #'car slime-net-valid-coding-systems)))
    props))
  
(defun slime-check-coding-system (&optional coding-system)
  (interactive)
  (slime-find-coding-system coding-system))

(defun slime-coding-system-mulibyte-p (coding-system)
  (second (slime-find-coding-system coding-system)))

(defun slime-coding-system-cl-name (coding-system)
  (third (slime-find-coding-system coding-system)))

;;; Interface
(defun slime-net-send (sexp proc)
  "Send a SEXP to Lisp over the socket PROC.
This is the lowest level of communication. The sexp will be READ and
EVAL'd by Lisp."
  (let* ((msg (concat (slime-prin1-to-string sexp) "\n"))
         (string (concat (slime-net-encode-length (length msg)) msg))
         (coding-system (cdr (process-coding-system proc))))
    (slime-log-event sexp)
    (cond ((slime-safe-encoding-p coding-system string)
           (process-send-string proc string))
          (t (error "Coding system %s not suitable for %S"
                    coding-system string)))))

(defun slime-safe-encoding-p (coding-system string)
  "Return true iff CODING-SYSTEM can safely encode STRING."
  (if (featurep 'xemacs)
      ;; FIXME: XEmacs encodes non-encodeable chars as ?~ automatically
      t
    (or (let ((candidates (find-coding-systems-string string))
              (base (coding-system-base coding-system)))
          (or (equal candidates '(undecided))
              (memq base candidates)))
        (and (not (multibyte-string-p string))
             (not (slime-coding-system-mulibyte-p coding-system))))))

(defun slime-net-close (process)
  (setq slime-net-processes (remove process slime-net-processes))
  (when (eq process slime-default-connection)
    (setq slime-default-connection nil))
  (run-hook-with-args 'slime-net-process-close-hooks process)
  (ignore-errors (kill-buffer (process-buffer process))))

(defun slime-net-sentinel (process message)
  (message "Lisp connection closed unexpectedly: %s" message)
  (slime-net-close process)
  (slime-set-state "[not connected]" process))

;;; Socket input is handled by `slime-net-filter', which decodes any
;;; complete messages and hands them off to the event dispatcher.

(defun slime-net-filter (process string)
  "Accept output from the socket and input all complete messages."
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert string))
    (slime-process-available-input)))

(defun slime-run-when-idle (function)
  "Call FUNCTION as soon as Emacs is idle."
  (cond ((featurep 'xemacs)
         (run-at-time itimer-short-interval nil
                      (lambda (f) (funcall f)) function))
        (t (run-at-time 0 nil function))))

(defun slime-process-available-input ()
  "Process all complete messages that have arrived from Lisp."
  (unwind-protect
      (dolist (proc slime-net-processes)
        (with-current-buffer (process-buffer proc)
          (while (slime-net-have-input-p)
            (let ((event (condition-case error
                             (slime-net-read)
                           (error 
                            (message "net-read error: %S" error)
                            (ding)
                            (sleep-for 2)
                            (ignore-errors (slime-net-close proc))
                            (error "PANIC!")))))
              (save-current-buffer
                (slime-log-event event)
                (slime-dispatch-event event proc))))))
    (dolist (p slime-net-processes)
      (with-current-buffer (process-buffer p)
        (when (slime-net-have-input-p)
          (slime-run-when-idle 'slime-process-available-input))))))

(defun slime-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (slime-net-decode-length))))

(defun slime-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (slime-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length)))
    (let ((string (buffer-substring start end)))
      (prog1 (read string)
        (delete-region (point-min) end)))))

(defun slime-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring (point) (+ (point) 6)) 16))

(defun slime-net-encode-length (n)
  "Encode an integer into a 24-bit hex string."
  (format "%06x" n))

(defun slime-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let ((print-escape-nonascii nil)
          (print-escape-newlines nil))
      (prin1 sexp (current-buffer))
      (buffer-string))))


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
;;;   `slime-dispatching-connection' if dynamically bound, or
;;;   `slime-buffer-connection' if this is set buffer-local, or
;;;   `slime-default-connection' otherwise. 
;;;
;;; When you're invoking commands in your source files you'll be using
;;; `slime-default-connection'. This connection can be interactively
;;; reassigned via the connection-list buffer.
;;;
;;; When a command creates a new buffer it will set
;;; `slime-buffer-connection' so that commands in the new buffer will
;;; use the connection that the buffer originated from. For example,
;;; the apropos command creates the *Apropos* buffer and any command
;;; in that buffer (e.g. `M-.') will go to the same Lisp that did the
;;; apropos search. REPL buffers are similarly tied to their
;;; respective connections.
;;;
;;; When Emacs is dispatching some network message that arrived from a
;;; connection it will dynamically bind `slime-dispatching-connection'
;;; so that the event will be processed in the context of that
;;; connection.
;;;
;;; This is mostly transparent. The user should be aware that he can
;;; set the default connection to pick which Lisp handles commands in
;;; Lisp-mode source buffers, and slime hackers should be aware that
;;; they can tie a buffer to a specific connection. The rest takes
;;; care of itself.

(defvar slime-dispatching-connection nil
  "Network process currently executing.
This is dynamically bound while handling messages from Lisp; it
overrides `slime-buffer-connection' and `slime-default-connection'.")

(make-variable-buffer-local
 (defvar slime-buffer-connection nil
   "Network connection to use in the current buffer.
This overrides `slime-default-connection'."))

(defvar slime-default-connection nil
  "Network connection to use by default.
Used for all Lisp communication, except when overridden by
`slime-dispatching-connection' or `slime-buffer-connection'.")

(defun slime-connection ()
  "Return the connection to use for Lisp interaction."
  (let ((conn (or slime-dispatching-connection
                  slime-buffer-connection
                  slime-default-connection)))
    (cond ((and (not conn) slime-net-processes)
           (error "No default connection selected."))
          ((not conn)
           (error "Not connected."))
          ((not (eq (process-status conn) 'open))
           (error "Connection closed."))
          (t conn))))

(defun slime-select-connection (process)
  "Make PROCESS the default connection."
  (setq slime-default-connection process))

(defmacro* slime-with-connection-buffer ((&optional process) &rest body)
  "Execute BODY in the process-buffer of PROCESS.
If PROCESS is not specified, `slime-connection' is used.

\(fn (&optional PROCESS) &body BODY))"
  `(with-current-buffer
       (process-buffer (or ,process (slime-connection)
                           (error "No connection")))
     ,@body))

(put 'slime-with-connection-buffer 'lisp-indent-function 1)

(defvar slime-state-name "[??]"
  "Name of the current state of `slime-default-connection'.
Just used for informational display in the mode-line.")

(defun slime-set-state (name &optional connection)
  "Set the current connection's informational state name.
If this is the default connection then the state will be displayed in
the modeline."
  (when (or (not (slime-connected-p))
            (eq (or connection (slime-connection)) slime-default-connection))
    (setq slime-state-name name)
    (force-mode-line-update)))

;;; Connection-local variables:

(defmacro slime-def-connection-var (varname &rest initial-value-and-doc)
  "Define a connection-local variable.
The value of the variable can be read by calling the function of the
same name (it must not be accessed directly). The accessor function is
setf-able.

The actual variable bindings are stored buffer-local in the
process-buffers of connections. The accessor function refers to
the binding for `slime-connection'."
  (let ((real-var (intern (format "%s:connlocal" varname))))
    `(progn
       ;; Variable
       (make-variable-buffer-local
        (defvar ,real-var ,@initial-value-and-doc))
       ;; Accessor
       (defun ,varname (&optional process)
         (slime-with-connection-buffer (process) ,real-var))
       ;; Setf
       (defsetf ,varname (&optional process) (store)
         `(slime-with-connection-buffer (,process)
            (setq (\, (quote (\, real-var))) (\, store))
            (\, store)))
       '(\, varname))))

(put 'slime-def-connection-var 'lisp-indent-function 2)

;; Let's indulge in some pretty colours.
(unless (featurep 'xemacs)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(slime-def-connection-var\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face)))))

(slime-def-connection-var slime-connection-number nil
  "Serial number of a connection.
Bound in the connection's process-buffer.")

(slime-def-connection-var slime-lisp-features '()
  "The symbol-names of Lisp's *FEATURES*.
This is automatically synchronized from Lisp.")

(slime-def-connection-var slime-lisp-package
    "COMMON-LISP-USER"
  "The current package name of the Superior lisp.
This is automatically synchronized from Lisp.")

(slime-def-connection-var slime-lisp-package-prompt-string
    "CL-USER"
  "The current package name of the Superior lisp.
This is automatically synchronized from Lisp.")

(slime-def-connection-var slime-pid nil
  "The process id of the Lisp process.")

(slime-def-connection-var slime-lisp-implementation-type nil
  "The implementation type of the Lisp process.")

(slime-def-connection-var slime-lisp-implementation-version nil
  "The implementation type of the Lisp process.")

(slime-def-connection-var slime-lisp-implementation-type-name nil
  "The short name for the implementation type of the Lisp process.")

(slime-def-connection-var slime-connection-name nil
  "The short name for connection.")

(slime-def-connection-var slime-symbolic-lisp-name nil
  "The symbolic name passed to slime when starting connection.")

(slime-def-connection-var slime-inferior-process nil
  "The inferior process for the connection if any.")

(slime-def-connection-var slime-communication-style nil
  "The communication style.")

(slime-def-connection-var slime-machine-instance nil
  "The name of the (remote) machine running the Lisp process.")

;;;;; Connection setup

(defvar slime-connection-counter 0
  "The number of SLIME connections made. For generating serial numbers.")

;;; Interface
(defun slime-setup-connection (process symbolic-lisp-name)
  "Make a connection out of PROCESS."
  (let ((slime-dispatching-connection process))
    (slime-init-connection-state process symbolic-lisp-name)
    (slime-select-connection process)
    process))

(defun slime-init-connection-state (proc symbolic-lisp-name)
  "Initialize connection state in the process-buffer of PROC."
  ;; To make life simpler for the user: if this is the only open
  ;; connection then reset the connection counter.
  (when (equal slime-net-processes (list proc))
    (setq slime-connection-counter 0))
  (slime-with-connection-buffer ()
    (setq slime-buffer-connection proc))
  (setf (slime-connection-number proc) (incf slime-connection-counter))
  (setf (slime-symbolic-lisp-name proc)
        (slime-generate-symbolic-lisp-name symbolic-lisp-name))
  ;; We do our initialization asynchronously. The current function may
  ;; be called from a timer, and if we setup the REPL from a timer
  ;; then it mysteriously uses the wrong keymap for the first command.
  (slime-eval-async '(swank:connection-info)
                    (lambda (info)
                      (slime-set-connection-info proc info))))

(defun slime-set-connection-info (connection info)
  "Initialize CONNECTION with INFO received from Lisp."
  (destructuring-bind (pid type name features style version host) info
    (setf (slime-pid) pid
          (slime-lisp-implementation-type) type
          (slime-lisp-implementation-type-name) name
          (slime-connection-name) (slime-generate-connection-name name)
          (slime-lisp-features) features
          (slime-communication-style) style
          (slime-lisp-implementation-version) version
          (slime-machine-instance) host))
  (setq slime-state-name "")            ; FIXME
  (slime-hide-inferior-lisp-buffer)
  (slime-init-output-buffer connection)
  (run-hooks 'slime-connected-hook)
  (message "Connected. %s" (slime-random-words-of-encouragement)))

(defun slime-generate-connection-name (lisp-name)
  (loop for i from 1
        for name = lisp-name then (format "%s<%d>" lisp-name i)
        while (find name slime-net-processes 
                    :key #'slime-connection-name :test #'equal)
        finally (return name)))

(defun slime-generate-symbolic-lisp-name (lisp-name)
  (if lisp-name
    (loop for i from 1
       for name = lisp-name then (format "%s<%d>" lisp-name i)
       while (find name slime-net-processes 
                   :key #'slime-symbolic-lisp-name :test #'equal)
       finally (return name))))


(defun slime-connection-close-hook (process)
  (when (eq process slime-default-connection)
    (when slime-net-processes
      (slime-select-connection (car slime-net-processes))
      (message "Default connection closed; switched to #%S (%S)"
               (slime-connection-number)
               (slime-connection-name)))))

(add-hook 'slime-net-process-close-hooks 'slime-connection-close-hook)

;;;;; Commands on connections

(defun slime-disconnect ()
  "Disconnect all connections."
  (interactive)
  (mapc #'slime-net-close slime-net-processes))

(defun slime-make-default-connection ()
  "Make the current connection the default connection."
  (interactive)
  (slime-select-connection (slime-connection))
  (message "Connection #%S (%s) now default SLIME connection."
           (slime-connection-number)
           (slime-connection-name)))

(defun slime-choose-connection ()
  "Return an established connection chosen by the user."
  (let ((default (slime-connection-name)))
    (slime-find-connection-by-name
     (completing-read (format "Connection name (default %s): " default)
                      (slime-bogus-completion-alist
                       (mapcar #'slime-connection-name slime-net-processes))
                      nil
                      t
                      nil
                      nil
                      default))))

(defun slime-find-connection-by-name (name)
  (find name slime-net-processes 
        :test #'string= :key #'slime-connection-name))

(defun slime-connection-port (connection)
  "Return the remote port number of CONNECTION."
  (if (featurep 'xemacs)
      (car (process-id connection))
    (cadr (process-contact connection))))

(defun slime-process (&optional connection)
  "Return the Lisp process for CONNECTION (default `slime-connection').
Can return nil if there's no process object for the connection."
  (let ((proc (slime-inferior-process connection)))
    (if (and proc 
             (memq (process-status proc) '(run stop)))
        proc)))

;; Non-macro version to keep the file byte-compilable. 
(defun slime-set-inferior-process (connection process)
  (setf (slime-inferior-process connection) process))

(defun slime-use-sigint-for-interrupt (&optional connection)
  (let ((c (or connection (slime-connection))))
    (ecase (slime-communication-style c)
      ((:fd-handler nil) t)
      ((:spawn :sigio) nil))))

(defvar slime-inhibit-pipelining t
  "*If true, don't send background requests if Lisp is already busy.")

(defun slime-background-activities-enabled-p ()
  (and (slime-connected-p)
       (or (not (slime-busy-p))
           (not slime-inhibit-pipelining))))


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
;;; You should use asynchronous evaluations (`slime-eval-async') for
;;; most things. Reserve synchronous evaluations (`slime-eval') for
;;; the cases where blocking Emacs is really appropriate (like
;;; completion) and that shouldn't trigger errors (e.g. not evaluate
;;; user-entered code).
;;;
;;; We have the concept of the "current Lisp package". RPC requests
;;; always say what package the user is making them from and the Lisp
;;; side binds that package to *BUFFER-PACKAGE* to use as it sees
;;; fit. The current package is defined as the buffer-local value of
;;; `slime-buffer-package' if set, and otherwise the package named by
;;; the nearest IN-PACKAGE as found by text search (first backwards,
;;; then forwards).
;;;
;;; Similarly we have the concept of the current thread, i.e. which
;;; thread in the Lisp process should handle the request. The current
;;; thread is determined solely by the buffer-local value of
;;; `slime-current-thread'. This is usually bound to t meaning "no
;;; particular thread", but can also be used to nominate a specific
;;; thread. The REPL and the debugger both use this feature to deal
;;; with specific threads.

(make-variable-buffer-local
 (defvar slime-current-thread t
   "The id of the current thread on the Lisp side.  
t means the \"current\" thread;
:repl-thread the thread that executes REPL requests;
fixnum a specific thread."))

(make-variable-buffer-local
 (defvar slime-buffer-package nil
   "The Lisp package associated with the current buffer.
This is set only in buffers bound to specific packages."))

;;; `slime-rex' is the RPC primitive which is used to implement both
;;; `slime-eval' and `slime-eval-async'. You can use it directly you
;;; need to but the others are usually more convenient.

(defmacro* slime-rex ((&rest saved-vars)
                      (sexp &optional 
                            (package '(slime-current-package))
                            (thread 'slime-current-thread))
                      &rest continuations)
  "(slime-rex (VAR ...) (SEXP &optional PACKAGE THREAD) CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each
VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the princed version is sent to Lisp.

PACKAGE is evaluated and Lisp binds *BUFFER-PACKAGE* to this package.
The default value is (slime-current-package).

CLAUSES is a list of patterns with same syntax as `destructure-case'.
The result of the evaluation is dispatched on CLAUSES.  The result is
either a sexp of the form (:ok VALUE) or (:abort).  CLAUSES is
executed asynchronously.

Note: don't use backquote syntax for SEXP, because Emacs20 cannot
deal with that."
  (let ((result (gensym)))
    `(lexical-let ,(loop for var in saved-vars
                         collect (etypecase var
                                   (symbol (list var var))
                                   (cons var)))
       (slime-dispatch-event 
        (list :emacs-rex ,sexp ,package ,thread
              (lambda (,result)
                (destructure-case ,result
                  ,@continuations)))))))

(put 'slime-rex 'lisp-indent-function 2)

;;; Interface
(defun slime-current-package ()
  "Return the Common Lisp package in the current context.
If `slime-buffer-package' has a value then return that, otherwise
search for and read an `in-package' form.

The REPL buffer is a special case: it's package is `slime-lisp-package'."
  (or (and (eq major-mode 'slime-repl-mode) (slime-lisp-package))
      slime-buffer-package
      (save-restriction
        (widen)
        (slime-find-buffer-package))))

(defvar slime-find-buffer-package-function nil
  "Function to use instead of `slime-find-buffer-package'.  
The result should be a string.  The string will be READ at the Lisp
side.")

(defun slime-find-buffer-package ()
  "Figure out which Lisp package the current buffer is associated with."
  (if slime-find-buffer-package-function
      (funcall slime-find-buffer-package-function)
    (save-excursion
      (when (let ((case-fold-search t)
                  (regexp "^(\\(cl:\\|common-lisp:\\)?in-package\\>"))
              (or (re-search-backward regexp nil t)
                  (re-search-forward regexp nil t)))
        (goto-char (match-end 0))
        (skip-chars-forward " \n\t\f\r#'")
        (let ((pkg (ignore-errors (read (current-buffer)))))
          (if pkg (format "%S" pkg)))))))

;;; Synchronous requests is implemented in terms of asynchronous
;;; ones. We make an asynchronous request with a continuation function
;;; that `throw's its result up to a `catch' and then enter a loop of
;;; handling I/O until that happens.

(defvar slime-stack-eval-tags nil
  "List of stack-tags of continuations waiting on the stack.")

(defun slime-eval (sexp &optional package)
  "Evaluate EXPR on the superior Lisp and return the result."
  (when (null package) (setq package (slime-current-package)))
  (let* ((tag (gensym "slime-result-"))
	 (slime-stack-eval-tags (cons tag slime-stack-eval-tags)))
    (apply
     #'funcall 
     (catch tag
       (slime-rex (tag sexp)
           (sexp package)
         ((:ok value)
          (unless (member tag slime-stack-eval-tags)
            (error "tag = %S eval-tags = %S sexp = %S"
                   tag slime-stack-eval-tags sexp))
          (throw tag (list #'identity value)))
         ((:abort)
          (throw tag (list #'error "Synchronous Lisp Evaluation aborted."))))
       (let ((debug-on-quit t)
             (inhibit-quit nil))
         (while t (accept-process-output nil 0 10000)))))))

(defun slime-eval-async (sexp &optional cont package)
  "Evaluate EXPR on the superior Lisp and call CONT with the result."
  (slime-rex (cont)
      (sexp (or package (slime-current-package)))
    ((:ok result) 
     (when cont (funcall cont result)))
    ((:abort) 
     (message "Evaluation aborted."))))

;;; These functions can be handy too:

(defun slime-connected-p ()
  "Return true if the Swank connection is open."
  (not (null slime-net-processes)))

(defun slime-check-connected ()
  "Signal an error if we are not connected to Lisp."
  (unless (slime-connected-p)
    (error "Not connected. Use `%s' to start a Lisp."
           (substitute-command-keys "\\[slime]"))))

(defun slime-busy-p ()
  "True if Lisp has outstanding requests.
Debugged requests are ignored."
  (let ((debugged (sldb-debugged-continuations (slime-connection))))
    (remove-if (lambda (id) 
                 (memq id debugged))
               (slime-rex-continuations)
               :key #'car)))

(defun slime-reading-p ()
  "True if Lisp is currently reading input from the REPL."
  (with-current-buffer (slime-output-buffer)
    slime-repl-read-mode))

(defun slime-sync ()
  "Block until the most recent request has finished."
  (when (slime-rex-continuations)
    (let ((tag (caar (slime-rex-continuations))))
      (while (find tag (slime-rex-continuations) :key #'car)
        (accept-process-output nil 0 100000)))))

(defun slime-ping ()
  "Check that communication works."
  (interactive)
  (message "%s" (slime-eval "PONG")))
 
;;;;; Protocol event handler (the guts)
;;;
;;; This is the protocol in all its glory. The input to this function
;;; is a protocol event that either originates within Emacs or arrived
;;; over the network from Lisp.
;;;
;;; Each event is a list beginning with a keyword and followed by
;;; arguments. The keyword identifies the type of event. Events
;;; originating from Emacs have names starting with :emacs- and events
;;; from Lisp don't.

(slime-def-connection-var slime-rex-continuations '()
  "List of (ID . FUNCTION) continuations waiting for RPC results.")

(slime-def-connection-var slime-continuation-counter 0
  "Continuation serial number counter.")

(defun slime-dispatch-event (event &optional process)
  (let ((slime-dispatching-connection (or process (slime-connection))))
    (destructure-case event
      ((:read-output output)
       (slime-output-string output))
      ;;
      ((:emacs-rex form package thread continuation)
       (slime-set-state "|eval...")
       (when (and (slime-use-sigint-for-interrupt) (slime-busy-p))
         (message "; pipelined request... %S" form))
       (let ((id (incf (slime-continuation-counter))))
         (push (cons id continuation) (slime-rex-continuations))
         (slime-send `(:emacs-rex ,form ,package ,thread ,id))))
      ((:return value id)
       (let ((rec (assq id (slime-rex-continuations))))
         (cond (rec (setf (slime-rex-continuations )
                          (remove rec (slime-rex-continuations)))
                    (when (null (slime-rex-continuations))
                      (slime-set-state ""))
                    (funcall (cdr rec) value))
               (t
                (error "Unexpected reply: %S %S" id value)))))
      ((:debug-activate thread level)
       (assert thread)
       (sldb-activate thread level))
      ((:debug thread level condition restarts frames conts)
       (assert thread)
       (sldb-setup thread level condition restarts frames conts))
      ((:debug-return thread level &optional stepping)
       (assert thread)
       (sldb-exit thread level stepping))
      ((:emacs-interrupt thread)
       (cond ((slime-use-sigint-for-interrupt) (slime-send-sigint))
             (t (slime-send `(:emacs-interrupt ,thread)))))
      ((:read-string thread tag)
       (assert thread)
       (slime-repl-read-string thread tag))
      ((:evaluate-in-emacs string thread tag)
       (assert thread)
       (evaluate-in-emacs (car (read-from-string string)) thread tag))
      ((:read-aborted thread tag)
       (assert thread)
       (slime-repl-abort-read thread tag))
      ((:emacs-return-string thread tag string)
       (slime-send `(:emacs-return-string ,thread ,tag ,string)))
      ;;
      ((:new-package package prompt-string)
       (setf (slime-lisp-package) package)
       (setf (slime-lisp-package-prompt-string) prompt-string))
      ((:new-features features)
       (setf (slime-lisp-features) features))
      ((:indentation-update info)
       (slime-handle-indentation-update info))
      ((:open-dedicated-output-stream port)
       (slime-open-stream-to-lisp port))
      ((:eval-no-wait fun args)
       (apply (intern fun) args))
      ((:eval thread tag fun args)
       (slime-eval-for-lisp thread tag (intern fun) args))
      ((:emacs-return thread tag value)
       (slime-send `(:emacs-return ,thread ,tag ,value)))
      ((:ed what)
       (slime-ed what))
      ((:debug-condition thread message)
       (assert thread)
       (message "%s" message)))))

(defun slime-send (sexp)
  "Send SEXP directly over the wire on the current connection."
  (slime-net-send sexp (slime-connection)))

(defun slime-reset ()
  "Clear all pending continuations."
  (interactive)
  (setf (slime-rex-continuations) '())
  (mapc #'kill-buffer (sldb-buffers)))

(defconst +slime-sigint+ 2)

(defun slime-send-sigint ()
  (interactive)
  (signal-process (slime-pid) +slime-sigint+))

;;;;; Event logging to *slime-events*
;;;
;;; The *slime-events* buffer logs all protocol messages for debugging
;;; purposes. Optionally you can enable outline-mode in that buffer,
;;; which is convenient but slows things down significantly.

(defvar slime-log-events t
  "*Log protocol events to the *slime-events* buffer.")

(defvar slime-outline-mode-in-events-buffer nil
  "*Non-nil means use outline-mode in *slime-events*.")

(defvar slime-event-buffer-name "*slime-events*"
  "The name of the slime event buffer.")

(defun slime-log-event (event)
  "Record the fact that EVENT occurred."
  (when slime-log-events
    (with-current-buffer (slime-events-buffer)
      ;; trim?
      (when (> (buffer-size) 100000)
        (goto-char (/ (buffer-size) 2))
        (re-search-forward "^(" nil t)
        (delete-region (point-min) (point)))
      (goto-char (point-max))
      (save-excursion
        (slime-pprint-event event (current-buffer)))
      (when (and (boundp 'outline-minor-mode)
                 outline-minor-mode)
        (hide-entry))
      (goto-char (point-max)))))

(defun slime-pprint-event (event buffer)
  "Pretty print EVENT in BUFFER with limited depth and width."
  (let ((print-length 20)
	(print-level 6)
	(pp-escape-newlines t))
    (pp event buffer)))

(defun slime-events-buffer ()
  (or (get-buffer slime-event-buffer-name)
      (let ((buffer (get-buffer-create slime-event-buffer-name)))
        (with-current-buffer buffer
          (set (make-local-variable 'outline-regexp) "^(")
          (set (make-local-variable 'comment-start) ";")
          (set (make-local-variable 'comment-end) "")
          (when slime-outline-mode-in-events-buffer
            (outline-minor-mode)))
        buffer)))


;;;; Stream output

(defcustom slime-header-line-p t
  "If non-nil, display a header line in Slime buffers."
  :type 'boolean
  :group 'slime-repl)

(make-variable-buffer-local
 (defvar slime-output-start nil
   "Marker for the start of the output for the evaluation."))

(make-variable-buffer-local
 (defvar slime-output-end nil
   "Marker for end of output. New output is inserted at this mark."))

(defun slime-reset-repl-markers ()
  (dolist (markname '(slime-output-start
                      slime-output-end
                      slime-repl-prompt-start-mark
                      slime-repl-input-start-mark
                      slime-repl-input-end-mark
                      slime-repl-last-input-start-mark))
    (set markname (make-marker))
    (set-marker (symbol-value markname) (point)))
  (set-marker-insertion-type slime-repl-input-end-mark t)
  (set-marker-insertion-type slime-output-end t)
  (set-marker-insertion-type slime-repl-prompt-start-mark t))

(defun slime-output-buffer (&optional noprompt)
  "Return the output buffer, create it if necessary."
  (or (slime-repl-buffer)
      (let ((connection (slime-connection)))
        (with-current-buffer (slime-repl-buffer t)
          (slime-repl-mode)
          (setq slime-buffer-connection connection)
          (slime-reset-repl-markers)
          (unless noprompt (slime-repl-insert-prompt "" 0))
          (current-buffer)))))

(defun slime-repl-update-banner ()
  (let* ((banner (format "%s  Port: %s  Pid: %s"
                         (slime-lisp-implementation-type)
                         (slime-connection-port (slime-connection))
                         (slime-pid)))
         ;; Emacs21 has the fancy persistent header-line.
         (use-header-p (and slime-header-line-p
                            (boundp 'header-line-format)))
         ;; and dancing text
         (animantep (and (fboundp 'animate-string)
                         slime-startup-animation
                         (zerop (buffer-size)))))
    (when use-header-p
      (setq header-line-format banner))
    (when animantep
      (pop-to-buffer (current-buffer))
      (animate-string (format "; SLIME %s" (or (slime-changelog-date) 
                                               "- ChangeLog file not found"))
                      0 0))
    (slime-repl-insert-prompt (if use-header-p "" (concat "; " banner)))))

(defun slime-changelog-date ()
  "Return the datestring of the latest entry in the ChangeLog file.
Return nil if the ChangeLog file cannot be found."
  (let ((changelog (concat slime-path "ChangeLog")))
    (if (file-exists-p changelog)
        (with-temp-buffer 
          (insert-file-contents changelog nil 0 100)
          (goto-char (point-min))
          (symbol-name (read (current-buffer))))
      nil)))

(defun slime-init-output-buffer (connection)
  (with-current-buffer (slime-output-buffer t)
    (setq slime-buffer-connection connection)
    ;; set the directory stack
    (setq slime-repl-directory-stack 
          (list (expand-file-name default-directory)))
    (setq slime-repl-package-stack (list (slime-lisp-package)))
    (slime-repl-update-banner)))

(defvar slime-show-last-output-function 
  'slime-maybe-display-output-buffer
  "*This function is called when a evaluation request is finished.
It is called in the slime-output buffer and receives the region of the
output as arguments.")

(defun slime-show-last-output-region (start end)
  (when (< start end)
    (slime-display-buffer-region (current-buffer) (1- start)
                                 slime-repl-input-start-mark)))

(defun slime-maybe-display-output-buffer (start end)
  (when (and (< start end)
             (not (get-buffer-window (current-buffer) t)))
    (display-buffer (current-buffer)))
  (when (eobp)
    (slime-repl-show-maximum-output t)))
      
(defun slime-flush-output ()
  (while (accept-process-output nil 0 20)))

(defun slime-show-last-output ()
  "Show the output from the last Lisp evaluation."
  (with-current-buffer (slime-output-buffer)
    (slime-flush-output)
    (let ((start slime-output-start)
          (end slime-output-end))
      (funcall slime-show-last-output-function start end))))

(defun slime-display-output-buffer ()
  "Display the output buffer and scroll to bottom."
  (with-current-buffer (slime-output-buffer)
    (goto-char (point-max))
    (unless (get-buffer-window (current-buffer) t)
      (display-buffer (current-buffer) t))
    (slime-repl-show-maximum-output)))

(defsetf marker-insertion-type set-marker-insertion-type)

(defmacro slime-with-output-end-mark (&rest body)
  "Execute BODY at `slime-output-end'.  

If point is initially at `slime-output-end' and the buffer is visible
update window-point afterwards.  If point is initially not at
`slime-output-end, execute body inside a `save-excursion' block."
  `(let ((body.. (lambda () ,@body))
         (updatep.. (and (eobp) (pos-visible-in-window-p))))
     (cond ((= (point) slime-output-end)
            (let ((start.. (point)))
              (funcall body..)
              (when (= start.. slime-repl-input-start-mark) 
                (set-marker slime-repl-input-start-mark (point)))))
           (t 
            (save-excursion 
              (goto-char slime-output-end)
              (funcall body..))))
     (when updatep..
       (slime-repl-show-maximum-output 
        (> (- slime-output-end slime-output-start) 1000)))))

(defun slime-output-filter (process string)
  (when (and (slime-connected-p)
             (plusp (length string)))
    (with-current-buffer (process-buffer process)
      (slime-output-string string))))

(defun slime-open-stream-to-lisp (port)
  (let ((stream (open-network-stream "*lisp-output-stream*" 
                                     (slime-with-connection-buffer ()
                                       (current-buffer))
				     "127.0.0.1" port)))
    (when slime-kill-without-query-p
      (process-kill-without-query stream))
    (set-process-filter stream 'slime-output-filter)
    (set-process-coding-system stream 
                               slime-net-coding-system 
                               slime-net-coding-system)
    (when-let (secret (slime-secret))
      (slime-net-send secret stream))
    stream))

(defun slime-output-string (string)
  (with-current-buffer (slime-output-buffer)
    (slime-with-output-end-mark
     (slime-propertize-region '(face slime-repl-output-face)
       (insert string))
     (when (and (= (point) slime-repl-prompt-start-mark)
                (not (bolp)))
       (insert "\n")
       (set-marker slime-output-end (1- (point)))))))

(defun slime-switch-to-output-buffer (&optional connection)
  "Select the output buffer, preferably in a different window."
  (interactive (list (if prefix-arg (slime-choose-connection))))
  (let ((slime-dispatching-connection (or connection 
                                          slime-dispatching-connection)))
    (set-buffer (slime-output-buffer))
    (unless (eq (current-buffer) (window-buffer))
      (pop-to-buffer (current-buffer) t))
    (goto-char (point-max))))


;;;; REPL
;;
;; The REPL uses some markers to separate input from output.  The
;; usual configuration is as follows:
;; 
;;    ... output ...    ... result ...    prompt> ... input ...
;;    ^            ^                      ^       ^           ^
;;    output-start output-end  prompt-start       input-start input-end  
;;
;; output-start and input-start are right inserting markers;
;; output-end and input-end left inserting.
;;
;; We maintain the following invariant:
;;
;;  output-start <= output-end <= input-start <= input-end.
;;
;; This invariant is important, because we must be prepared for
;; asynchronous output and asynchronous reads.  ("Asynchronous" means,
;; triggered by Lisp and not by Emacs.)
;;
;; All output is inserted at the output-end marker.  Some care must be
;; taken when output-end and input-start are at the same position: if
;; we blindly insert at that point, we break the invariant stated
;; above, because the output-end marker is left inserting.  The macro
;; `slime-with-output-end-mark' handles this complication by moving
;; the input-start marker to an appropriate place.  The macro also
;; updates window-point if necessary, and tries to keep the prompt in
;; the first column by inserting a newline.
;;
;; A "synchronous" evaluation request proceeds as follows: the user
;; inserts some text between input-start and input-end and then hits
;; return.  We send the text between the input markers to Lisp, move
;; the output and input makers to the line after the input and wait.
;; When we receive the result, we insert it together with a prompt
;; between the output-end and input-start mark.
;; `slime-repl-insert-prompt' does this.
;;
;; It is possible that some output for such an evaluation request
;; arrives after the result.  This output is inserted before the
;; result (and before the prompt).  Output that doesn't belong the
;; evaluation request should not be inserted before the result, but
;; immediately before the prompt.  To achieve this, we move the
;; output-end mark to prompt-start after a short delay (by starting a
;; timer in `slime-repl-insert-prompt').  In summary: synchronous
;; output should go before the result, asynchronous before the prompt.
;;
;; If we are in "reading" state, e.g., during a call to Y-OR-N-P,
;; there is no prompt between output-end and input-start.
;;

(make-variable-buffer-local
 (defvar slime-repl-package-stack nil
   "The stack of packages visited in this repl."))

(make-variable-buffer-local
 (defvar slime-repl-directory-stack nil
   "The stack of default directories associated with this repl."))

;; Small helper.
(defun slime-make-variables-buffer-local (&rest variables)
  (mapcar #'make-variable-buffer-local variables))

(slime-make-variables-buffer-local
 ;; Local variables in the REPL buffer.
 (defvar slime-repl-input-history '()
   "History list of strings read from the REPL buffer.")
 
 (defvar slime-repl-input-history-position 0
   "Newer items have smaller indices.")

 (defvar slime-repl-prompt-start-mark)
 (defvar slime-repl-input-start-mark)
 (defvar slime-repl-input-end-mark)
 (defvar slime-repl-last-input-start-mark)
 (defvar slime-repl-old-input-counter 0
   "Counter used to generate unique `slime-repl-old-input' properties.
This property value must be unique to avoid having adjacent inputs be
joined together."))

(defvar slime-repl-mode-map)

(defun slime-repl-buffer (&optional create connection)
  "Get the REPL buffer for the current connection; optionally create."
  (funcall (if create #'get-buffer-create #'get-buffer)
           (format "*slime-repl %s*"
                   (or (slime-symbolic-lisp-name connection)
                       (slime-connection-name connection)))))

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
  (setq slime-current-thread :repl-thread)
  (set (make-local-variable 'scroll-conservatively) 20)
  (set (make-local-variable 'scroll-margin) 0)
  (slime-setup-command-hooks)
  (run-hooks 'slime-repl-mode-hook))

(defun slime-repl-insert-prompt (result &optional time)
  "Goto to point max, insert RESULT and the prompt.  Set
slime-output-end to start of the inserted text slime-input-start to
end end."
  (slime-flush-output)
  (goto-char (point-max))
  (let ((start (point)))
    (unless (bolp) (insert "\n"))
    (slime-insert-propertized '(face slime-repl-result-face) result)
    (unless (bolp) (insert "\n"))
    (let ((prompt-start (point))
          (prompt (format "%s> " (slime-lisp-package-prompt-string))))
      (slime-propertize-region
          '(face slime-repl-prompt-face
                 read-only t
                 intangible t
                 slime-repl-prompt t
                 ;; emacs stuff
                 rear-nonsticky (slime-repl-prompt read-only face intangible)
                 ;; xemacs stuff
                 start-open t end-open t)
        (insert prompt))
      ;; FIXME: we could also set beginning-of-defun-function
      (setq defun-prompt-regexp (concat "^" prompt))
      (set-marker slime-output-end start)
      (set-marker slime-repl-prompt-start-mark prompt-start)
      (slime-mark-input-start)
      (let ((time (or time 0.2)))
        (cond ((zerop time)
               (slime-repl-move-output-mark-before-prompt (current-buffer)))
              (t 
               (run-at-time time nil 'slime-repl-move-output-mark-before-prompt
                            (current-buffer)))))))
  (slime-repl-show-maximum-output))

(defun slime-repl-move-output-mark-before-prompt (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion 
        (goto-char slime-repl-prompt-start-mark)
        (slime-mark-output-start)))))

(defun slime-repl-show-maximum-output (&optional force)
  "Put the end of the buffer at the bottom of the window."
  (assert (eobp))
  (let ((win (get-buffer-window (current-buffer))))
    (when (and win (or force (not (pos-visible-in-window-p))))
      (save-selected-window
        (save-excursion
          (select-window win)
          (goto-char (point-max))
          (recenter -1))))))

(defun slime-repl-current-input ()
  "Return the current input as string.  The input is the region from
after the last prompt to the end of buffer."
  (buffer-substring-no-properties slime-repl-input-start-mark
                                  slime-repl-input-end-mark))

(defun slime-repl-add-to-input-history (string)
  (when (and (plusp (length string))
             (eq ?\n (aref string (1- (length string)))))
    (setq string (substring string 0 -1)))
  (unless (equal string (car slime-repl-input-history))
    (push string slime-repl-input-history))
  (setq slime-repl-input-history-position -1))
  
(defun slime-repl-eval-string (string)
  (slime-rex ()
      ((list 'swank:listener-eval string) (slime-lisp-package))
    ((:ok result) (with-current-buffer (slime-output-buffer)
                    (slime-repl-insert-prompt result)))
    ((:abort) (slime-repl-show-abort))))

(defun slime-repl-send-string (string &optional command-string)
  (slime-repl-add-to-input-history (or command-string string))
  (cond (slime-repl-read-mode
         (slime-repl-return-string string))
        (t (slime-repl-eval-string string))))
  
(defun slime-repl-show-abort ()
  (with-current-buffer (slime-output-buffer)
    (slime-with-output-end-mark 
     (unless (bolp) (insert-before-markers "\n"))
     (insert-before-markers "; Evaluation aborted\n"))
    (slime-rex ()
        ((list 'swank:listener-eval "") nil)
      ((:ok result) (with-current-buffer (slime-output-buffer)
                      (slime-repl-insert-prompt ""))))))
  
(defun slime-mark-input-start ()
  (set-marker slime-repl-last-input-start-mark
              (marker-position slime-repl-input-start-mark))
  (set-marker slime-repl-input-start-mark (point) (current-buffer))
  (set-marker slime-repl-input-end-mark (point) (current-buffer)))

(defun slime-mark-output-start (&optional position)
  (let ((position (or position (point))))
    (set-marker slime-output-start position)
    (set-marker slime-output-end position)))

(defun slime-mark-output-end ()
  (add-text-properties slime-output-start slime-output-end
                       '(face slime-repl-output-face rear-nonsticky (face))))

(defun slime-repl-bol ()
  "Go to the beginning of line or the prompt."
  (interactive)
  (if (and (>= (point) slime-repl-input-start-mark)
           (slime-same-line-p (point) slime-repl-input-start-mark))
      (goto-char slime-repl-input-start-mark)
    (beginning-of-line 1))
  (slime-preserve-zmacs-region))

(defun slime-repl-eol ()
  "Go to the end of line or the prompt."
  (interactive)
  (if (and (<= (point) slime-repl-input-end-mark)
           (slime-same-line-p (point) slime-repl-input-end-mark))
      (goto-char slime-repl-input-end-mark)
    (end-of-line 1))
  (slime-preserve-zmacs-region))

(defun slime-preserve-zmacs-region ()
  "In XEmacs, ensure that the zmacs-region stays active after this command."
  (when (boundp 'zmacs-region-stays)
    (set 'zmacs-region-stays t)))

(defun slime-repl-in-input-area-p ()
   (and (<= slime-repl-input-start-mark (point))
        (<= (point) slime-repl-input-end-mark)))
  
(defun slime-repl-beginning-of-defun ()
  "Move to beginning of defun."
  (interactive)
  (if (slime-repl-in-input-area-p)
      (goto-char slime-repl-input-start-mark)
    (beginning-of-defun)))

(defun slime-repl-end-of-defun ()
  "Move to next of defun."
  (interactive)
  (if (slime-repl-in-input-area-p)
      (goto-char slime-repl-input-end-mark)
    (end-of-defun)))

(defun slime-repl-at-prompt-end-p ()
  (and (get-char-property (max 1 (1- (point))) 'slime-repl-prompt)
       (not (get-char-property (point) 'slime-repl-prompt))))
 
(defun slime-repl-find-prompt (move)
  (let ((origin (point)))
    (loop (funcall move)
          (when (or (slime-repl-at-prompt-end-p) (bobp) (eobp))
            (return)))
    (unless (slime-repl-at-prompt-end-p)
      (goto-char origin))))

(defun slime-search-property-change-fn (prop &optional backward)
  (with-lexical-bindings (prop)
    (if backward 
        (lambda () 
          (goto-char
           (previous-single-char-property-change (point) prop)))
        (lambda () 
          (goto-char
           (next-single-char-property-change (point) prop))))))

(defun slime-repl-previous-prompt ()
  "Move backward to the previous prompt."
  (interactive)
  (slime-repl-find-prompt 
   (slime-search-property-change-fn 'slime-repl-prompt t)))

(defun slime-repl-next-prompt ()
  "Move forward to the next prompt."
  (interactive)
  (slime-repl-find-prompt
   (slime-search-property-change-fn 'slime-repl-prompt)))

(defun slime-repl-return (&optional end-of-input)
  "Evaluate the current input string, or insert a newline.  
Send the current input ony if a whole expression has been entered,
i.e. the parenthesis are matched. 

With prefix argument send the input even if the parenthesis are not
balanced."
  (interactive "P")
  (slime-check-connected)
  (assert (<= (point) slime-repl-input-end-mark))
  (cond ((and (get-text-property (point) 'slime-repl-old-input)
              (< (point) slime-repl-input-start-mark))
         (slime-repl-grab-old-input end-of-input)
         (unless (pos-visible-in-window-p slime-repl-input-end-mark)
           (save-excursion
             (goto-char slime-repl-input-end-mark)
             (recenter -1))))
        (end-of-input
         (slime-repl-send-input))
        (slime-repl-read-mode ; bad style?
         (slime-repl-send-input t))
        ((slime-input-complete-p slime-repl-input-start-mark 
                                 slime-repl-input-end-mark)
         (slime-repl-send-input t))
        (t 
         (slime-repl-newline-and-indent)
         (message "[input not complete]"))))

(defun slime-repl-send-input (&optional newline)
  "Goto to the end of the input and send the current input.
If NEWLINE is true then add a newline at the end of the input."
  (when (< (point) slime-repl-input-start-mark)
    (error "No input at point."))
  (goto-char slime-repl-input-end-mark)
  (when newline 
    (insert "\n")
    (slime-repl-show-maximum-output))
  (add-text-properties slime-repl-input-start-mark (point)
                       `(slime-repl-old-input
                         ,(incf slime-repl-old-input-counter)))
  (let ((overlay (make-overlay slime-repl-input-start-mark (point))))
    ;; These properties are on an overlay so that they won't be taken
    ;; by kill/yank.
    (overlay-put overlay 'read-only t)
    (overlay-put overlay 'face 'slime-repl-input-face)
    (overlay-put overlay 'rear-nonsticky '(face slime-repl-old-input-counter)))
  (let ((input (slime-repl-current-input)))
    (goto-char slime-repl-input-end-mark)
    (slime-mark-input-start)
    (slime-mark-output-start)
    (slime-repl-send-string input)))

(defun slime-repl-grab-old-input (replace)
  "Resend the old REPL input at point.  
If replace it non-nil the current input is replaced with the old
input; otherwise the new input is appended.  The old input has the
text property `slime-repl-old-input'."
  (let ((prop 'slime-repl-old-input))
    (let* ((beg (save-excursion
                  ;; previous-single-char-property-change searches for
                  ;; a property change from the previous character,
                  ;; but we want to look for a change from the
                  ;; point. We step forward one char to avoid doing
                  ;; the wrong thing if we're at the beginning of the
                  ;; old input. -luke (18/Jun/2004)
                  (ignore-errors (forward-char))
                  (previous-single-char-property-change (point) prop)))
           (end (save-excursion
                  (goto-char (next-single-char-property-change (point) prop))
                  (skip-chars-backward "\n \t\r" beg)
                  (point)))
           (old-input (buffer-substring-no-properties beg end))
           (offset (- (point) beg)))
      ;; Append the old input or replace the current input
      (cond (replace (goto-char slime-repl-input-start-mark))
            (t (goto-char slime-repl-input-end-mark)
               (unless (eq (char-before) ?\ )
                 (insert " "))))
      (delete-region (point) slime-repl-input-end-mark)
      (save-excursion (insert old-input))
      (forward-char offset))))

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

(defun slime-repl-input-line-beginning-position ()
  (save-excursion
    (goto-char slime-repl-input-start-mark)
    (line-beginning-position)))

(defun slime-repl-clear-buffer ()
  "Delete the entire output generated by the Lisp process."
  (interactive)
  (set-marker slime-repl-last-input-start-mark nil)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (slime-repl-input-line-beginning-position))
    (goto-char slime-repl-input-start-mark)))

(defun slime-repl-clear-output ()
  "Delete the output inserted since the last input."
  (interactive)
  (let ((start (save-excursion 
                 (slime-repl-previous-prompt)
                 (ignore-errors (forward-sexp))
                 (forward-line)
                 (point)))
        (end (1- (slime-repl-input-line-beginning-position))))
    (when (< start end)
      (delete-region start end)
      (save-excursion
        (goto-char start)
        (insert ";;; output flushed")))))

(defun slime-repl-set-package (package)
  "Set the package of the REPL buffer to PACKAGE."
  (interactive (list (slime-read-package-name "Package: " 
					      (slime-find-buffer-package))))
  (with-current-buffer (slime-output-buffer)
    (let ((unfinished-input (slime-repl-current-input)))
      (destructuring-bind (name prompt-string)
          (slime-eval `(swank:set-package ,package))
        (setf (slime-lisp-package) name)
        (setf (slime-lisp-package-prompt-string) prompt-string)
        (slime-repl-insert-prompt "" 0)
        (insert unfinished-input)))))


;;;;; History

(defcustom slime-repl-wrap-history nil
  "T to wrap history around when the end is reached."
  :type 'boolean
  :group 'slime-repl)

(defvar slime-repl-history-pattern nil
  "The regexp most recently used for finding input history.")

(defun slime-repl-history-replace (direction regexp &optional delete-at-end-p)
  "Replace the current input with the next line in DIRECTION matching REGEXP.
DIRECTION is 'forward' or 'backward' (in the history list).
If DELETE-AT-END-P is non-nil then remove the string if the end of the
history is reached."
  (setq slime-repl-history-pattern regexp)
  (let ((pos (slime-repl-position-in-history direction regexp))
        (forward (eq direction 'forward)))
    (cond (pos
           (slime-repl-replace-input (nth pos slime-repl-input-history))
           (setq slime-repl-input-history-position pos)
           (message "History item: %d" pos))
          ((and delete-at-end-p (not slime-repl-wrap-history))
           (cond (forward (slime-repl-replace-input "")
                          (message "End of history"))
                 (t (message "Beginning of history")))
           (setq slime-repl-input-history-position
                 (if forward -1 (length slime-repl-input-history))))
          ((and delete-at-end-p slime-repl-wrap-history)
           (slime-repl-replace-input "")
           (setq slime-repl-input-history-position
                 (if forward (length slime-repl-input-history) -1)))
          (t
           (message "End of history; no matching item")))))

(defun slime-repl-position-in-history (direction regexp)
  "Return the position of the history item matching regexp.
Return nil of no item matches"
  ;; Loop through the history list looking for a matching line
  (let* ((step (ecase direction
                 (forward -1)
                 (backward 1)))
         (history-pos0 slime-repl-input-history-position))
    (loop for pos = (+ history-pos0 step) then (+ pos step)
          while (and (<= 0 pos)
                     (< pos (length slime-repl-input-history)))
          do (let ((string (nth pos slime-repl-input-history)))
               (when (and (string-match regexp string)
                          (not (string= string (slime-repl-current-input))))
                 (return pos))))))

(defun slime-repl-matching-input-regexp ()
  (if (memq last-command
            '(slime-repl-previous-input slime-repl-next-input))
      slime-repl-history-pattern
    (concat "^" (regexp-quote (slime-repl-current-input)))))

(defun slime-repl-previous-input ()
  (interactive)
  (slime-repl-history-replace 'backward (slime-repl-matching-input-regexp) t))

(defun slime-repl-next-input ()
  (interactive)
  (slime-repl-history-replace 'forward (slime-repl-matching-input-regexp) t))

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
  ([home] 'slime-repl-bol)
  ("\C-e" 'slime-repl-eol)
  ("\M-p" 'slime-repl-previous-input)
  ((kbd "C-<up>") 'slime-repl-previous-input)
  ("\M-n" 'slime-repl-next-input)
  ((kbd "C-<down>") 'slime-repl-next-input)
  ("\M-r" 'slime-repl-previous-matching-input)
  ("\M-s" 'slime-repl-next-matching-input)
  ("\C-c\C-c" 'slime-interrupt)
  ("\C-c\C-b" 'slime-interrupt)
  ("\C-c:"    'slime-interactive-eval)
  ("\C-c\C-e" 'slime-interactive-eval)
  ("\C-cE"     'slime-edit-value)
  ;("\t"   'slime-complete-symbol)
  ("\t"   'slime-indent-and-complete-symbol)
  (" "    'slime-space)
  ("\C-c\C-d" slime-doc-map)
  ("\C-c\C-w" slime-who-map)
  ("\C-\M-x" 'slime-eval-defun)
  ("\C-c\C-o" 'slime-repl-clear-output)
  ("\C-c\C-t" 'slime-repl-clear-buffer)
  ("\C-c\C-n" 'slime-repl-next-prompt)
  ("\C-c\C-p" 'slime-repl-previous-prompt)
  ("\M-\C-a" 'slime-repl-beginning-of-defun)
  ("\M-\C-e" 'slime-repl-end-of-defun)
  ("\C-c\C-l" 'slime-load-file)
  ("\C-c\C-k" 'slime-compile-and-load-file)
  ("\C-c\C-z" 'slime-nop))

(define-key slime-repl-mode-map
  (string slime-repl-shortcut-dispatch-char) 'slime-handle-repl-shortcut)

(define-minor-mode slime-repl-read-mode 
  "Mode the read input from Emacs
\\{slime-repl-read-mode-map}"
  nil
  "[read]"
  '(("\C-m" . slime-repl-return)
    ("\C-c\C-b" . slime-repl-read-break)
    ("\C-c\C-c" . slime-repl-read-break)))

(make-variable-buffer-local
 (defvar slime-read-string-threads nil))

(make-variable-buffer-local
 (defvar slime-read-string-tags nil))

(defun slime-repl-read-string (thread tag)
  (slime-switch-to-output-buffer)
  (push thread slime-read-string-threads)
  (push tag slime-read-string-tags)
  (goto-char (point-max))
  (slime-mark-output-end)
  (slime-mark-input-start)
  (slime-repl-read-mode 1))

(defun evaluate-in-emacs (expr thread tag)
  (push thread slime-read-string-threads)
  (push tag slime-read-string-tags)
  (slime-repl-return-string (eval expr)))

(defun slime-repl-return-string (string)
  (slime-dispatch-event `(:emacs-return-string 
                          ,(pop slime-read-string-threads)
                          ,(pop slime-read-string-tags)
                          ,string))
  (slime-repl-read-mode -1))

(defun slime-repl-read-break ()
  (interactive)
  (slime-eval-async `(cl:break)))

(defun slime-repl-abort-read (thread tag)
  (with-current-buffer (slime-output-buffer)
    (pop slime-read-string-threads)
    (pop slime-read-string-tags)
    (slime-repl-read-mode -1)
    (message "Read aborted")))


;;;;; REPL handlers

(defstruct (slime-repl-shortcut (:conc-name slime-repl-shortcut.))
  symbol names handler one-liner)

(defvar slime-repl-shortcut-table nil
  "A list of slime-repl-shortcuts")

(defvar slime-repl-shortcut-history '()
  "History list of shortcut command names.")

(defun slime-handle-repl-shortcut ()
  (interactive)
  (if (save-excursion
        (goto-char slime-repl-input-start-mark)
        (looking-at " *$"))
      (let ((shortcut (slime-lookup-shortcut
                       (completing-read "Command: " 
                                        (slime-bogus-completion-alist
                                         (slime-list-all-repl-shortcuts))
                                        nil t nil
                                        'slime-repl-shortcut-history))))
        (call-interactively (slime-repl-shortcut.handler shortcut)))
      (insert (string slime-repl-shortcut-dispatch-char))))

(defun slime-list-all-repl-shortcuts ()
  (loop for shortcut in slime-repl-shortcut-table
        append (slime-repl-shortcut.names shortcut)))

(defun slime-lookup-shortcut (name)
  (find-if (lambda (s) (member name (slime-repl-shortcut.names s)))
           slime-repl-shortcut-table))

(defmacro defslime-repl-shortcut (elisp-name names &rest options)
  "Define a new repl shortcut. ELISP-NAME is a symbol specifying
  the name of the interactive function to create, or NIL if no
  function should be created. NAMES is a list of (full-name .
  aliases). OPTIONS is an olist specifying the handler and the
  help text."
  `(progn
     ,(when elisp-name
        `(defun ,elisp-name ()
           (interactive)
           (call-interactively ,(second (assoc :handler options)))))
     (let ((new-shortcut (make-slime-repl-shortcut
                          :symbol ',elisp-name
                          :names (list ,@names)
                          ,@(apply #'append options))))
       (setq slime-repl-shortcut-table
             (remove-if (lambda (s)
                          (member ',(car names) (slime-repl-shortcut.names s)))
                        slime-repl-shortcut-table))
       (push new-shortcut slime-repl-shortcut-table)
       ',elisp-name)))

(defun slime-list-repl-short-cuts ()
  (interactive)
  (slime-with-output-to-temp-buffer ("*slime-repl-help*") nil
    (let ((table (sort* (copy-list slime-repl-shortcut-table) #'string<
                        :key (lambda (x) 
                               (car (slime-repl-shortcut.names x))))))
      (dolist (shortcut table)
        (let ((names (slime-repl-shortcut.names shortcut)))
          (insert (pop names)) ;; first print the "full" name
          (when names
            ;; we also have aliases
            (insert " (aka ")
            (while (cdr names)
              (insert (pop names) ", "))
            (insert (car names) ")"))
        (insert "\n     " (slime-repl-shortcut.one-liner shortcut)
                "\n"))))))
  
(defslime-repl-shortcut slime-repl-shortcut-help ("help" "?")
  (:handler 'slime-list-repl-short-cuts)
  (:one-liner "Display the help."))

(defslime-repl-shortcut nil ("change-directory" "!d" "cd")
  (:handler 'slime-set-default-directory)
  (:one-liner "Change the current directory."))

(defslime-repl-shortcut nil ("pwd")
  (:handler (lambda () 
              (interactive)
              (let ((dir (slime-eval `(swank:default-directory))))
                (message "Directory %s" dir))))
  (:one-liner "Show the current directory."))

(defslime-repl-shortcut slime-repl-push-directory ("push-directory" "+d" 
                                                   "pushd")
  (:handler (lambda (directory)
              (interactive
               (list (read-directory-name 
                      "Push directory: "
                      (slime-eval '(swank:default-directory)) nil nil "")))
              (push directory slime-repl-directory-stack)
              (slime-set-default-directory directory)))
  (:one-liner "Push a new directory onto the directory stack."))

(defslime-repl-shortcut slime-repl-pop-directory ("pop-directory" "-d")
  (:handler (lambda ()
              (interactive)
              (unless (= 1 (length slime-repl-directory-stack))
                (pop slime-repl-directory-stack))
              (slime-set-default-directory (car slime-repl-directory-stack))))
  (:one-liner "Pop the current directory."))

(defslime-repl-shortcut nil ("change-package" "!p")
  (:handler 'slime-repl-set-package)
  (:one-liner "Change the current package."))

(defslime-repl-shortcut slime-repl-push-package ("push-package" "+p")
  (:handler (lambda (package)
              (interactive (list (slime-read-package-name "Package: ")))
              (push package slime-repl-package-stack)
              (slime-repl-set-package package)))
  (:one-liner "Push a package onto the package stack."))

(defslime-repl-shortcut slime-repl-pop-package ("pop-package" "-p")
  (:handler (lambda ()
              (interactive)
              (unless (= 1 (length slime-repl-package-stack))
                (pop slime-repl-package-stack))
              (slime-repl-set-package (car slime-repl-package-stack))))
  (:one-liner "Pop the top of the package stack."))

(defslime-repl-shortcut slime-repl-resend ("resend-form")
  (:handler (lambda ()
              (interactive)
              (insert (car slime-repl-input-history))
              (insert "\n")
              (slime-repl-send-input)))
  (:one-liner "Resend the last form."))

(defslime-repl-shortcut slime-repl-sayoonara ("sayoonara")
  (:handler (lambda ()
              (interactive)
              (when (slime-connected-p)
                (slime-quit-lisp))
              (slime-kill-all-buffers)))
  (:one-liner "Quit all Lisps and close all SLIME buffers."))

(defslime-repl-shortcut slime-repl-quit ("quit")
  (:handler 'slime-quit-lisp)
  (:one-liner "Quit the current Lisp."))

(defslime-repl-shortcut slime-repl-defparameter ("defparameter" "!")
  (:handler (lambda (name value)
              (interactive (list (slime-read-symbol-name "Name (symbol): " t)
                                 (slime-read-from-minibuffer "Value: " "*")))
              (insert "(cl:defparameter " name " " value 
                      " \"REPL generated global variable.\")")
              (slime-repl-send-input)))
  (:one-liner "Define a new global, special, variable."))

(defslime-repl-shortcut slime-repl-compile-and-load ("compile-and-load" "cl")
  (:handler (lambda (filename)
              (interactive (list (expand-file-name
                                  (read-file-name "File: " nil nil nil nil))))
              (save-some-buffers)
              (slime-eval-async 
               `(swank:compile-file-if-needed 
                 ,(slime-to-lisp-filename filename) t)
               (slime-compilation-finished-continuation))))
  (:one-liner "Compile (if neccessary) and load a lisp file."))

(defslime-repl-shortcut slime-repl-load/force-system ("force-load-system")
  (:handler (lambda ()
              (interactive)
              (slime-oos (slime-read-system-name) "LOAD-OP" :force t)))
  (:one-liner "Recompile and load an ASDF system."))

(defslime-repl-shortcut slime-repl-load-system ("load-system")
  (:handler (lambda ()
              (interactive)
              (slime-oos (slime-read-system-name) "LOAD-OP")))
  (:one-liner "Compile (as needed) and load an ASDF system."))

(defslime-repl-shortcut slime-repl-compile-system ("compile-system")
  (:handler (lambda ()
              (interactive)
              (slime-oos (slime-read-system-name) "COMPILE-OP")))
  (:one-liner "Compile (but not load) an ASDF system."))

(defslime-repl-shortcut slime-repl-compile/force-system 
  ("force-compile-system")  
  (:handler (lambda ()
              (interactive)
              (slime-oos (slime-read-system-name) "COMPILE-OP" :force t)))
  (:one-liner "Recompile (but not load) an ASDF system."))

(defslime-repl-shortcut slime-restart-inferior-lisp ("restart-inferior-lisp")
  (:handler 'slime-restart-inferior-lisp-aux)
  (:one-liner "Restart *inferior-lisp* and reconnect SLIME."))

(defun slime-restart-inferior-lisp-aux ()
  (interactive)
  (slime-eval-async '(swank:quit-lisp))
  (set-process-sentinel (slime-connection) 'slime-restart-sentinel))
  
(defun slime-restart-sentinel (process message)
  "Restart the inferior lisp process.
Also rearrange windows."
  (assert (process-status process) 'closed)
  (let* ((proc (slime-inferior-process process))
         (args (mapconcat #'identity (process-command proc) " "))
         (buffer (buffer-name (process-buffer proc)))
         (buffer-window (get-buffer-window buffer))
         (new-proc (slime-start-lisp args buffer (slime-init-command)))
         (repl-buffer (slime-repl-buffer nil process))
         (repl-window (and repl-buffer (get-buffer-window repl-buffer))))
    (slime-net-close process)
    (slime-inferior-connect new-proc)
    (cond ((and repl-window (not buffer-window))
           (set-window-buffer repl-window buffer)
           (select-window repl-window))
          (repl-window
           (select-window repl-window))
          (t 
           (pop-to-buffer buffer)))
    (switch-to-buffer buffer)
    (goto-char (point-max))))


;;;;; Cleanup after a quit

(defun slime-kill-all-buffers ()
  "Kill all the slime related buffers. This is only used by the
  repl command sayoonara."
  (dolist (buf (buffer-list))
    (when (or (string= (buffer-name buf) slime-event-buffer-name)
              (string-match "^\\*inferior-lisp*" (buffer-name buf))
              (string-match "^\\*slime-repl .*\\*$" (buffer-name buf))
              (string-match "^\\*sldb .*\\*$" (buffer-name buf)))
      (kill-buffer buf))))


;;;; Scratch

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


;;;; Compilation and the creation of compiler-note annotations

(defun slime-compile-and-load-file ()
  "Compile and load the buffer's file and highlight compiler notes.

Each source location that is the subject of a compiler note is
underlined and annotated with the relevant information. The commands
`slime-next-note' and `slime-previous-note' can be used to navigate
between compiler notes and to display their full details."
  (interactive)
  (slime-compile-file t))

(defvar slime-lisp-modes '(lisp-mode))

(defun slime-compile-file (&optional load)
  "Compile current buffer's file and highlight resulting compiler notes.

See `slime-compile-and-load-file' for further details."
  (interactive)
  (unless (memq major-mode slime-lisp-modes)
    (error "Only valid in lisp-mode"))
  (unless buffer-file-name
    (error "Buffer %s is not associated with a file." (buffer-name)))
  (when (and (buffer-modified-p)
             (y-or-n-p (format "Save file %s? " (buffer-file-name))))
    (save-buffer))
  (let ((lisp-filename (slime-to-lisp-filename (buffer-file-name))))
    (slime-insert-transcript-delimiter
     (format "Compile file %s" lisp-filename))
    (when slime-display-compilation-output
      (slime-display-output-buffer))
    (slime-eval-async
     `(swank:compile-file-for-emacs ,lisp-filename ,(if load t nil))
     (slime-compilation-finished-continuation))
    (message "Compiling %s.." lisp-filename)))

(defun slime-find-asd ()
  (let ((asdf-systems-in-directory 
         (directory-files (file-name-directory (or default-directory
                                                   (buffer-file-name)))
                          nil "\.asd$")))
    (and asdf-systems-in-directory
         (file-name-sans-extension (car asdf-systems-in-directory)))))

(defun slime-load-system (&optional system-name)
  "Compile and load an ASDF system.  

Default system name is taken from first file matching *.asd in current
buffer's working directory"
  (interactive (list (slime-read-system-name)))
  (slime-oos system-name "LOAD-OP"))

(defun slime-read-system-name (&optional prompt initial-value)
  "Read a system name from the minibuffer, prompting with PROMPT."
  (setq prompt (or prompt "System: "))
  (let ((completion-ignore-case nil)
        (alist (slime-bogus-completion-alist
                (mapcar #'file-name-sans-extension
                        (slime-eval 
                         `(swank:list-all-systems-in-central-registry))))))
    (completing-read prompt alist nil nil
                     (or initial-value (slime-find-asd) ""))))

(defun slime-oos (system-name operation &rest keyword-args)
  (save-some-buffers)
  (slime-display-output-buffer)
  (message "Performing ASDF %S%s on system %S"
           operation (if keyword-args (format " %S" keyword-args) "")
           system-name)
  (slime-eval-async
   `(swank:operate-on-system-for-emacs ,system-name ,operation ,@keyword-args)
   (slime-compilation-finished-continuation)))

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
   `(swank:compile-string-for-emacs
     ,string
     ,(buffer-name)
     ,start-offset
     ,(if (buffer-file-name) (file-name-directory (buffer-file-name))))
   (slime-compilation-finished-continuation)))

(defvar slime-hide-style-warning-count-if-zero t)

(defun slime-note-count-string (severity count &optional suppress-if-zero)
  (cond ((and (zerop count) suppress-if-zero)
         "")
        (t (format "%2d %s%s " count severity (if (= count 1) "" "s")))))

(defun slime-show-note-counts (notes &optional secs)
  (let ((nerrors 0) (nwarnings 0) (nstyle-warnings 0) (nnotes 0))
    (dolist (note notes)
      (ecase (slime-note.severity note)
	((:error :read-error) (incf nerrors))
        (:warning             (incf nwarnings))
        (:style-warning       (incf nstyle-warnings))
        (:note                (incf nnotes))))
    (message
     "Compilation finished:%s%s%s%s%s"
     (slime-note-count-string "error" nerrors)
     (slime-note-count-string "warning" nwarnings)
     (slime-note-count-string "style-warning" nstyle-warnings 
                              slime-hide-style-warning-count-if-zero)
     (slime-note-count-string "note" nnotes)
     (if secs (format "[%s secs]" secs) ""))))

(defun slime-xrefs-for-notes (notes)
  (let ((xrefs))
    (dolist (note notes)
      (let* ((location (getf note :location))
             (fn (cadr (assq :file (cdr location))))
             (file (assoc fn xrefs))
             (node
              (cons (format "%s: %s" 
                            (getf note :severity)
                            (slime-one-line-ify (getf note :message)))
                    location)))
        (when fn
          (if file
              (push node (cdr file))
              (setf xrefs (acons fn (list node) xrefs))))))
    xrefs))

(defun slime-one-line-ify (string)
  "Return a single-line version of STRING.
Each newlines and following indentation is replaced by a single space."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "\n[\n \t]*" nil t)
      (replace-match " "))
    (buffer-string)))

(defun slime-length> (list n)
  "Test if (length LIST) is greater than N."
  (while (and (> n 0) list)
    (setq list (cdr list))
    (decf n))
  list)

(defun slime-compilation-finished (result buffer)
  (let ((notes (slime-compiler-notes)))
    (with-current-buffer buffer
      (multiple-value-bind (result secs) result
        (slime-show-note-counts notes secs)
        (slime-highlight-notes notes)))
    (run-hook-with-args 'slime-compilation-finished-hook notes)))

(defun slime-compilation-finished-continuation ()
  (lexical-let ((buffer (current-buffer)))
    (lambda (result) 
      (slime-compilation-finished result buffer))))

(defun slime-highlight-notes (notes)
  "Highlight compiler notes, warnings, and errors in the buffer."
  (interactive (list (slime-compiler-notes)))
  (save-excursion
    (slime-remove-old-overlays)
    (mapc #'slime-overlay-note (slime-merge-notes-for-display notes))))

(defun slime-compiler-notes ()
  "Return all compiler notes, warnings, and errors."
  (slime-eval `(swank:compiler-notes-for-emacs)))

(defun slime-remove-old-overlays ()
  "Delete the existing Slime overlays in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (dolist (o (overlays-at (point)))
        (when (overlay-get o 'slime)
          (delete-overlay o)))
      (goto-char (next-overlay-change (point))))))


;;;;; Merging together compiler notes in the same location.

(defun slime-merge-notes-for-display (notes)
  "Merge together notes that refer to the same location.
This operation is \"lossy\" in the broad sense but not for display purposes."
  (mapcar #'slime-merge-notes
          (slime-group-similar 'slime-notes-in-same-location-p notes)))

(defun slime-merge-notes (notes)
  "Merge NOTES together. Keep the highest severity, concatenate the messages."
  (let* ((new-severity (reduce #'slime-most-severe notes
                               :key #'slime-note.severity))
         (new-message (mapconcat #'slime-note.short-message notes "\n"))
         (new-references (reduce #'append notes :key #'slime-note.references)))
    (let ((new-note (copy-list (car notes))))
      (setf (getf new-note :message) new-message)
      (setf (getf new-note :severity) new-severity)
      (setf (getf new-note :references) new-references)
      new-note)))

;; XXX: unused function
(defun slime-intersperse (element list)
  "Intersperse ELEMENT between each element of LIST."
  (if (null list) 
      '()
    (cons (car list)
          (mapcan (lambda (x) (list element x)) (cdr list)))))

(defun slime-notes-in-same-location-p (a b)
  (equal (slime-note.location a) (slime-note.location b)))

(defun slime-group-similar (similar-p list)
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


;;;;; Compiler notes list

(defun slime-maybe-show-xrefs-for-notes (&optional notes)
  "Show the compiler notes NOTES if they come from more than one file."
  (let* ((notes (or notes (slime-compiler-notes))) 
         (xrefs (slime-xrefs-for-notes notes)))
    (when (> (length xrefs) 1)          ; >1 file
      (slime-show-xrefs
       xrefs 'definition "Compiler notes" (slime-current-package)))))

(defun slime-note-has-location-p (note)
  (not (eq ':error (car (slime-note.location note)))))

(defun slime-maybe-list-compiler-notes (notes)
  "Show the compiler notes if appropriate."
  ;; don't pop up a buffer if all notes will are already annotated in
  ;; the buffer itself
  (unless (every #'slime-note-has-location-p notes)
    (slime-list-compiler-notes notes)))

(defun slime-list-compiler-notes (&optional notes)
  "Show the compiler notes NOTES in tree view."
  (interactive)
  (let ((notes (or notes (slime-compiler-notes))))
    (with-current-buffer
        (slime-get-temp-buffer-create "*compiler notes*"
                                      :mode 'slime-compiler-notes-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when (null notes)
          (insert "[no notes]"))
        (dolist (tree (slime-compiler-notes-to-tree notes))
          (slime-tree-insert tree "")
          (insert "\n")))
      (setq buffer-read-only t)
      (goto-char (point-min)))))

(defun slime-alistify (list key test)
  "Partition the elements of LIST into an alist.  KEY extracts the key
from an element and TEST is used to compare keys."
  (declare (type function key))
  (let ((alist '()))
    (dolist (e list)
      (let* ((k (funcall key e))
	     (probe (assoc* k alist :test test)))
	(if probe
	    (push e (cdr probe))
            (push (cons k (list e)) alist))))
    ;; Put them back in order.
    (loop for (key . value) in alist
          collect (cons key (reverse value)))))

(defun slime-note.severity (note)
  (plist-get note :severity))

(defun slime-note.message (note)
  (plist-get note :message))

(defun slime-note.short-message (note)
  (or (plist-get note :short-message)
      (plist-get note :message)))

(defun slime-note.references (note)
  (plist-get note :references))

(defun slime-note.location (note)
  (plist-get note :location))

(defun slime-severity-label (severity)
  (ecase severity
    (:note "Notes")
    (:warning "Warnings")
    (:error "Errors")
    (:read-error "Read Errors")
    (:style-warning "Style Warnings")))

(defun slime-tree-for-note (note)
  (make-slime-tree :item (slime-note.message note)
                   :plist (list 'note note)
                   :print-fn (if (slime-note.references note)
                                 'slime-tree-print-with-references
                               'slime-tree-default-printer)))

(defun slime-tree-for-severity (severity notes collapsed-p)
  (make-slime-tree :item (format "%s (%d)" 
                                 (slime-severity-label severity)
                                 (length notes))
                   :kids (mapcar #'slime-tree-for-note notes)
                   :collapsed-p collapsed-p))

(defun slime-compiler-notes-to-tree (notes)
  (let* ((alist (slime-alistify notes #'slime-note.severity #'eq))
         (collapsed-p (slime-length> alist 1)))
    (loop for (severity . notes) in alist
          collect (slime-tree-for-severity severity notes 
                                           collapsed-p))))

(defvar slime-compiler-notes-mode-map)

(define-derived-mode slime-compiler-notes-mode fundamental-mode 
  "Compiler Notes"
  "\\<slime-compiler-notes-mode-map>\
\\{slime-compiler-notes-mode-map}"
  (slime-set-truncate-lines))

(slime-define-keys slime-compiler-notes-mode-map
  ((kbd "RET") 'slime-compiler-notes-default-action-or-show-details)
  ([mouse-2] 'slime-compiler-notes-default-action-or-show-details/mouse)
  ("q" 'slime-compiler-notes-quit))

(defun slime-compiler-notes-default-action-or-show-details/mouse (event)
  "Invoke the action pointed at by the mouse, or show details."
  (interactive "e")
  (destructuring-bind (mouse-2 (w pos &rest _) &rest __) event
    (save-excursion
      (goto-char pos)
      (let ((fn (get-text-property (point) 
                                   'slime-compiler-notes-default-action)))
	(if fn (funcall fn) (slime-compiler-notes-show-details))))))

(defun slime-compiler-notes-default-action-or-show-details ()
  "Invoke the action at point, or show details."
  (interactive)
  (let ((fn (get-text-property (point) 'slime-compiler-notes-default-action)))
    (if fn (funcall fn) (slime-compiler-notes-show-details))))

(defun slime-compiler-notes-quit ()
  (interactive)
  (let ((config slime-temp-buffer-saved-window-configuration))
    (kill-buffer (current-buffer))
    (set-window-configuration config)))

(defun slime-compiler-notes-show-details ()
  (interactive)
  (let* ((tree (slime-tree-at-point))
         (note (plist-get (slime-tree.plist tree) 'note))
         (inhibit-read-only t))
    (cond ((not (slime-tree-leaf-p tree))
           (slime-tree-toggle tree))
          (t
           (slime-show-source-location (slime-note.location note))))))


;;;;;; Tree Widget

(defstruct (slime-tree (:conc-name slime-tree.))
  item
  (print-fn #'slime-tree-default-printer :type function)
  (kids '() :type list)
  (collapsed-p t :type boolean)
  (prefix "" :type string)
  (start-mark nil)
  (end-mark nil)
  (plist '() :type list))

(defun slime-tree-leaf-p (tree)
  (not (slime-tree.kids tree)))

(defun slime-tree-default-printer (tree)
  (princ (slime-tree.item tree) (current-buffer)))

(defun slime-tree-print-with-references (tree)
  ;; for SBCL-style references
  (slime-tree-default-printer tree)
  (when-let (note (plist-get (slime-tree.plist tree) 'note))
    (when-let (references (slime-note.references note))
      (terpri (current-buffer))
      (princ "See also:" (current-buffer))
      (terpri (current-buffer))
      (slime-tree-insert-references references))))

(defun slime-tree-insert-references (references)
  "Insert documentation references from a condition.
See SWANK-BACKEND:CONDITION-REFERENCES for the datatype."
  (loop for refs on references
        for ref = (car refs)
        do
        (destructuring-bind (where type what) ref
          ;; FIXME: this is poorly factored, and shares some code and
          ;; data with sldb that it shouldn't: notably
          ;; sldb-reference-face.  Probably the names of
          ;; sldb-reference-foo should be altered to be not sldb
          ;; specific.
          (insert "  " (sldb-format-reference-source where) ", ")
          (slime-insert-propertized (sldb-reference-properties ref)
                                    (sldb-format-reference-node what))
          (insert (format " [%s]" (slime-cl-symbol-name type)))
          (when (cdr refs)
            (terpri (current-buffer))))))

(defun slime-tree-decoration (tree)
  (cond ((slime-tree-leaf-p tree) "-- ")
	((slime-tree.collapsed-p tree) "[+] ")
	(t "-+  ")))

(defun slime-tree-insert-list (list prefix)
  "Insert a list of trees."
  (loop for (elt . rest) on list 
	do (cond (rest
		  (insert prefix " |")
		  (slime-tree-insert elt (concat prefix " |"))
                  (insert "\n"))
		 (t
		  (insert prefix " `")
		  (slime-tree-insert elt (concat prefix "  "))))))

(defun slime-tree-insert-decoration (tree)
  (insert (slime-tree-decoration tree)))

(defun slime-tree-indent-item (start end prefix)
  "Insert PREFIX at the beginning of each but the first line.
This is used for labels spanning multiple lines."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (while (< start (point))
      (insert-before-markers prefix)
      (forward-line -1))))

(defun slime-tree-insert (tree prefix)
  "Insert TREE prefixed with PREFIX at point."
  (with-struct (slime-tree. print-fn kids collapsed-p start-mark end-mark) tree
    (let ((line-start (line-beginning-position)))
      (setf start-mark (point-marker))
      (slime-tree-insert-decoration tree)
      (funcall print-fn tree)
      (slime-tree-indent-item start-mark (point) (concat prefix "   "))
      (add-text-properties line-start (point) (list 'slime-tree tree))
      (set-marker-insertion-type start-mark t)
      (when (and kids (not collapsed-p))
        (terpri (current-buffer))
        (slime-tree-insert-list kids prefix))
      (setf (slime-tree.prefix tree) prefix)
      (setf end-mark (point-marker)))))

(defun slime-tree-at-point ()
  (cond ((get-text-property (point) 'slime-tree))
        (t (error "No tree at point"))))

(defun slime-tree-delete (tree)
  "Delete the region for TREE."
  (delete-region (slime-tree.start-mark tree)
                 (slime-tree.end-mark tree)))

(defun slime-tree-toggle (tree)
  "Toggle the visibility of TREE's children."
  (with-struct (slime-tree. collapsed-p start-mark end-mark prefix) tree
    (setf collapsed-p (not collapsed-p))
    (slime-tree-delete tree)
    (insert-before-markers " ") ; move parent's end-mark
    (backward-char 1)
    (slime-tree-insert tree prefix)
    (delete-char 1)
    (goto-char start-mark)))


;;;;; Adding a single compiler note

(defun slime-overlay-note (note)
  "Add a compiler note to the buffer as an overlay.
If an appropriate overlay for a compiler note in the same location
already exists then the new information is merged into it. Otherwise a
new overlay is created."
  (multiple-value-bind (start end) (slime-choose-overlay-region note)
    (when start
      (goto-char start)
      (let ((severity (plist-get note :severity))
            (message (plist-get note :message))
            (appropriate-overlay (slime-note-at-point)))
        (if appropriate-overlay
            (slime-merge-note-into-overlay appropriate-overlay severity message)
            (slime-create-note-overlay note start end severity message))))))

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
      (putp 'face (slime-severity-face severity))
      (putp 'severity severity)
      (unless (slime-emacs-20-p)
	(putp 'mouse-face 'highlight))
      (putp 'help-echo message)
      overlay)))

;; XXX Obsolete due to `slime-merge-notes-for-display' doing the
;; work already -- unless we decide to put several sets of notes on a
;; buffer without clearing in between, which only this handles.
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
  (let ((location (slime-note.location note)))
    (destructure-case location
      ((:error msg) )                       ; do nothing
      ((:location _file pos _hints)
       (destructure-case pos
         ((:position pos &optional alignp)
          (if (eq (slime-note.severity note) :read-error)
              (values pos (1+ pos))
            (slime-choose-overlay-for-sexp location)))
         (t 
          (slime-choose-overlay-for-sexp location)))))))
          
(defun slime-choose-overlay-for-sexp (location)
  (slime-goto-source-location location)
  (skip-chars-forward "'#`")
  (let ((start (point)))
    (ignore-errors (slime-forward-sexp))
    (if (slime-same-line-p start (point))
        (values start (point))
      (values (1+ start)
              (progn (goto-char (1+ start))
                     (or (forward-sexp 1)
                         (point)))))))

(defun slime-same-line-p (pos1 pos2)
  "Return t if buffer positions POS1 and POS2 are on the same line."
    (save-excursion (goto-char (min pos1 pos2))
                    (<= (max pos1 pos2) (line-end-position))))

(defun slime-severity-face (severity)
  "Return the name of the font-lock face representing SEVERITY."
  (ecase severity
    (:error         'slime-error-face)
    (:read-error    'slime-error-face)
    (:warning       'slime-warning-face)
    (:style-warning 'slime-style-warning-face)
    (:note          'slime-note-face)))

(defun slime-most-severe (sev1 sev2)
  "Return the most servere of two conditions.
Severity is ordered as :NOTE < :STYLE-WARNING < :WARNING < :ERROR."
                                        ; Well, not exactly Smullyan..
  (let ((order '(:note :style-warning :warning :error :read-error)))
    (if (>= (position sev1 order) 
            (position sev2 order))
        sev1
      sev2)))

;; XXX: unused function
(defun slime-visit-source-path (source-path)
  "Visit a full source path including the top-level form."
  (goto-char (point-min))
  (slime-forward-source-path source-path))

(defun slime-forward-positioned-source-path (source-path)
  "Move forward through a sourcepath from a fixed position.
The point is assumed to already be at the outermost sexp, making the
first element of the source-path redundant."
  (ignore-errors 
    (slime-forward-sexp)
    (beginning-of-defun))
  (when-let (source-path (cdr source-path))
    (down-list 1)
    (slime-forward-source-path source-path)))

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

(defun slime-goto-location-buffer (buffer)
  (destructure-case buffer
    ((:file filename)
     (set-buffer (find-file-noselect (slime-from-lisp-filename filename) t))
     (goto-char (point-min)))
    ((:buffer buffer)
     (set-buffer buffer)
     (goto-char (point-min)))
    ((:source-form string)
     (set-buffer (get-buffer-create "*SLIME Source Form*"))
     (erase-buffer)
     (insert string)
     (goto-char (point-min)))))

(defun slime-goto-location-position (position)
  (destructure-case position
    ((:position pos &optional align-p)
     (goto-char pos)
     (when align-p
       (slime-forward-sexp)
       (beginning-of-sexp)))
    ((:line start &optional end)
     (goto-line start))
    ((:function-name name)
     (let ((case-fold-search t)
           (name (regexp-quote name)))
       (or 
        (re-search-forward 
         (format "\\s *(def\\(\\s_\\|\\sw\\)*\\s +%s\\S_" name) nil t)
        (re-search-forward 
         ;; FIXME: Isn't this far to general?
         (format "[( \t]%s\\>\\(\\s \\|$\\)" name) nil t)))
     (goto-char (match-beginning 0)))
    ;; Looks for a sequence of words (def<something> method name
    ;; qualifers specializers don't look for "T" since it isn't
    ;; requires (arg without t) as class is taken as such.
    ((:method name specializers &rest qualifiers)
     (let* ((case-fold-search t)
            (name (regexp-quote name))
            (qualifiers (mapconcat (lambda (el) (concat ".+?\\<" el "\\>"))
                                   qualifiers ""))
            (specializers (mapconcat (lambda (el) (concat ".+?\\<" el "\\>"))
                                     (remove "T" specializers) ""))
            (regexp (format "\\s *(def\\(\\s_\\|\\sw\\)*\\s +%s\\>%s%s" name
                            qualifiers specializers)))
       (or (and (re-search-forward regexp  nil t)
                (goto-char (match-beginning 0)))
           ;;	(slime-goto-location-position `(:function-name ,name))
           )))
    ((:source-path source-path start-position)
     (cond (start-position
            (goto-char start-position)
            (slime-forward-positioned-source-path source-path))
           (t
            (slime-forward-source-path source-path))))
    ;; Goes to "start" then looks for the anchor text, then moves
    ;; delta from that position.
    ((:text-anchored start text delta)
     (goto-char start)
     (slime-isearch text)
     (forward-char delta))))

(defun slime-search-call-site (fname)
  "Move to the place where FNAME called.
Don't move if there are multiple or no calls in the current defun."
  (save-restriction 
    (narrow-to-defun)
    (let ((start (point))
          (regexp (concat "(" fname "[\n \t]")))
      (cond ((and (re-search-forward regexp nil t)
                  (not (re-search-forward regexp nil t)))
             (goto-char (match-beginning 0)))
            (t (goto-char start))))))


(defun slime-goto-source-location (location &optional noerror)
  "Move to the source location LOCATION.  Several kinds of locations
are supported:

<location> ::= (:location <buffer> <position> <hints>)
             | (:error <message>) 

<buffer>   ::= (:file <filename>)
             | (:buffer <buffername>)
             | (:source-form <string>)

<position> ::= (:position <fixnum> [<align>]) ; 1 based
             | (:line <fixnum> [<fixnum>])
             | (:function-name <string>)
             | (:source-path <list> <start-position>) 
             | (:text-anchored <fixnum> <string> <fixnum>) 
             | (:method <name string> <specializer strings> . <qualifiers strings>)"
  (destructure-case location
    ((:location buffer position hints)
     (slime-goto-location-buffer buffer)
     (slime-goto-location-position position)
     (when-let (snippet (getf hints :snippet))
       (slime-isearch snippet))
     (when-let (fname (getf hints :call-site))
       (slime-search-call-site fname)))
    ((:error message)
     (if noerror
         (slime-message "%s" message)
       (error "%s" message)))))

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

(defun slime-to-feature-keyword (symbol)
  (let ((name (downcase (symbol-name symbol))))
    (intern (if (eq ?: (aref name 0))
                name
              (concat ":" name)))))

(defun slime-eval-feature-conditional (e)
  "Interpret a reader conditional expression."
  (if (symbolp e)
      (memq (slime-to-feature-keyword e) (slime-lisp-features))
    (funcall (ecase (slime-to-feature-keyword (car e))
               (:and #'every)
               (:or #'some)
               (:not (lambda (f l) (not (apply f l)))))
             #'slime-eval-feature-conditional
             (cdr e))))


;;;;; Incremental search
;;
;; Search for the longest match of a string in either direction.
;;
;; This is for locating text that is expected to be near the point and
;; may have been modified (but hopefully not near the beginning!)

(defun slime-isearch (string)
  "Find the longest occurence of STRING either backwards of forwards.
If multiple matches exist the choose the one nearest to point."
  (goto-char
   (let* ((start (point))
          (len1 (slime-isearch-with-function 'search-forward string))
          (pos1 (point)))
     (goto-char start)
     (let* ((len2 (slime-isearch-with-function 'search-backward string))
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

(defun slime-isearch-with-function (search-fn string)
  "Search for the longest substring of STRING using SEARCH-FN.
SEARCH-FN is either the symbol `search-forward' or `search-backward'."
  (unless (string= string "")
    (loop for i from 1 to (length string)
          while (funcall search-fn (substring string 0 i) nil t)
          for match-data = (match-data)
          do (case search-fn
               (search-forward  (goto-char (match-beginning 0)))
               (search-backward (goto-char (1+ (match-end 0)))))
          finally (return (if (null match-data)
                              nil
                            ;; Finish based on the last successful match
                            (store-match-data match-data)
                            (goto-char (match-beginning 0))
                            (- (match-end 0) (match-beginning 0)))))))


;;;;; Visiting and navigating the overlays of compiler notes

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


;;;;; Overlay lookup operations

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


;;;; Arglist Display

(defun slime-space (n)
  "Insert a space and print some relevant information (function arglist).
Designed to be bound to the SPC key.  Prefix argument can be used to insert
more than one space."
  (interactive "p")
  (unwind-protect
      (when (and slime-space-information-p
                 (slime-background-activities-enabled-p))
        (slime-echo-arglist))
  (self-insert-command n)))

(defun slime-echo-arglist ()
  "Display the arglist of the current form in the echo area."
  (let ((names (slime-enclosing-operator-names)))
    (when names
      (slime-eval-async 
       `(swank:arglist-for-echo-area (quote ,names))
       (lexical-let ((buffer (current-buffer)))
         (lambda (message)
           (if message
               (with-current-buffer buffer
                 (slime-message "%s" message)))))))))

(defun slime-arglist (name)
  "Show the argument list for NAME."
  (interactive (list (slime-read-symbol-name "Arglist of: ")))
  (slime-eval-async 
   `(swank:arglist-for-echo-area (quote (,name)))
   (lambda (arglist)
     (message "%s" arglist))))

(defun slime-insert-arglist (name)
  "Insert the argument list for NAME behind the symbol point is
currently looking at."
  (interactive (list (slime-read-symbol-name "Arglist of: ")))
  (let ((arglist (slime-eval `(swank:arglist-for-insertion ',name))))
    (cond ((eq arglist :not-available)
           (error "Arglist not available"))
          ((string-match "^(" arglist)
           (insert " ")
           (save-excursion 
             (insert (substring arglist 1))))
          (t
           (save-excursion
             (insert arglist))))))

(defun slime-complete-form ()
  "Complete the form at point.  This is a superset of the
functionality of `slime-insert-arglist'."
  (interactive)
  ;; Find the (possibly incomplete) form around point.
  (let* ((start (save-excursion (backward-up-list 1) (point)))
         (end (point)) ; or try to find end (tricky)?
         (form-string
          (concat (buffer-substring-no-properties start end) ")")))
    (let ((result (slime-eval `(swank:complete-form ,form-string))))
      (if (eq result :not-available)
          (error "Arglist not available")
          (progn
            (just-one-space)
            (save-excursion
              (insert result)))))))

(defun slime-get-arglist (symbol-name)
  "Return the argument list for SYMBOL-NAME."
  (slime-eval `(swank:arglist-for-echo-area (quote (,symbol-name)))))


;;;; Autodocs (automatic context-sensitive help)

(defvar slime-autodoc-mode nil
  "*When non-nil, print documentation about symbols as the point moves.")

(defvar slime-autodoc-cache-type 'last
  "*Cache policy for automatically fetched documentation.
Possible values are:
 nil  - none.
 last - cache only the most recently-looked-at symbol's documentation.
        The values are stored in the variable `slime-autodoc-cache'.

More caching means fewer calls to the Lisp process, but at the risk of
using outdated information.")

(defvar slime-autodoc-cache nil
  "Cache variable for when `slime-autodoc-cache-type' is 'last'.
The value is (SYMBOL-NAME . DOCUMENTATION).")

(defun slime-autodoc-mode (&optional arg)
  "Enable `slime-autodoc'."
  (interactive "P")
  (cond ((< (prefix-numeric-value arg) 0) (setq slime-autodoc-mode nil))
        (arg (setq slime-autodoc-mode t))
        (t (setq slime-autodoc-mode (not slime-autodoc-mode))))
  (if slime-autodoc-mode
      (slime-autodoc-start-timer)
    (slime-autodoc-stop-timer)))

(defun slime-autodoc ()
  "Print some apropos information about the code at point, if applicable."
  (when-let (name (or (slime-autodoc-global-at-point)
                      (slime-function-called-at-point/line)))
    (let ((cache-key (slime-qualify-cl-symbol-name name)))
      (or (when-let (documentation (slime-get-cached-autodoc cache-key))
            (slime-background-message "%s" documentation)
            t)
          ;; Asynchronously fetch, cache, and display documentation
          (slime-eval-async
           (if (slime-global-variable-name-p name)
               `(swank:variable-desc-for-echo-area ,name)
             `(swank:arglist-for-echo-area '(,name)))
           (with-lexical-bindings (cache-key name)
             (lambda (doc)
               (when (null doc)
                 (setq doc ""))
               (slime-update-autodoc-cache cache-key doc)
               (slime-background-message "%s" doc))))))))

(defun slime-autodoc-global-at-point ()
  "Return the global variable name at point, if any."
  (when-let (name (slime-symbol-name-at-point))
    (if (slime-global-variable-name-p name) name)))

(defun slime-global-variable-name-p (name)
  "Is NAME a global variable?
Globals are recognised purely by *this-naming-convention*."
  (string-match "^\\(.*::?\\)?[*+].*[*+]$" name))

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


;;;;; Asynchronous message idle timer

(defvar slime-autodoc-idle-timer nil
  "Idle timer for the next autodoc message.")

(defvar slime-autodoc-delay 0.2
  "*Delay before autodoc messages are fetched and displayed, in seconds.")

(defun slime-autodoc-start-timer ()
  "(Re)start the timer that prints autodocs every `slime-autodoc-delay' seconds."
  (interactive)
  (when slime-autodoc-idle-timer
    (cancel-timer slime-autodoc-idle-timer))
  (setq slime-autodoc-idle-timer
        (run-with-idle-timer slime-autodoc-delay slime-autodoc-delay
                             'slime-autodoc-timer-hook)))

(defun slime-autodoc-stop-timer ()
  "Stop the timer that prints autodocs.
See also `slime-autodoc-start-timer'."
  (when slime-autodoc-idle-timer
    (cancel-timer slime-autodoc-idle-timer)
    (setq slime-autodoc-idle-timer nil)))

(defun slime-autodoc-timer-hook ()
  "Function to be called after each Emacs becomes idle.
When `slime-autodoc-mode' is non-nil, print apropos information about
the symbol at point if applicable."
  (when (slime-autodoc-message-ok-p)
    (condition-case err
        (slime-autodoc)
      (error
       (setq slime-autodoc-mode nil)
       (message "Error: %S; slime-autodoc-mode now disabled." err)))))

(defun slime-autodoc-message-ok-p ()
  "Return true if printing a message is currently okay (shouldn't
annoy the user)."
  (and slime-mode
       slime-autodoc-mode
       (null (current-message))
       (not executing-kbd-macro)
       (not (and (boundp 'edebug-active) (symbol-value 'edebug-active)))
       (not cursor-in-echo-area)
       (not (eq (selected-window) (minibuffer-window)))
       (slime-background-activities-enabled-p)))


;;;; Typeout frame

;; When a "typeout frame" exists it is used to display certain
;; messages instead of the echo area or pop-up windows.

(defvar slime-typeout-window nil
  "The current typeout window.")

(defvar slime-typeout-frame-properties
  '((height . 10) (minibuffer . nil))
  "The typeout frame properties (passed to `make-frame').")

(defun slime-typeout-active-p ()
  (and slime-typeout-window
       (window-live-p slime-typeout-window)))

(defun slime-typeout-message (format-string &rest format-args)
  (assert (slime-typeout-active-p))
  (with-current-buffer (window-buffer slime-typeout-window)
    (erase-buffer)
    (insert (apply #'format format-string format-args))))

(defun slime-make-typeout-frame ()
  "Create a frame for displaying messages (e.g. arglists)."
  (interactive)
  (let ((frame (make-frame slime-typeout-frame-properties)))
    (save-selected-window
      (select-window (frame-selected-window frame))
      (switch-to-buffer "*SLIME-Typeout*")
      (setq slime-typeout-window (selected-window)))))

(defun slime-ensure-typeout-frame ()
  "Create the typeout frame unless it already exists."
  (interactive)
  (unless (slime-typeout-active-p)
    (slime-make-typeout-frame)))


;;;; Completion

;; XXX those long names are ugly to read; long names an indicator for
;; bad factoring?

(defvar slime-completions-buffer-name "*Completions*")

(make-variable-buffer-local
 (defvar slime-complete-saved-window-configuration nil
   "Window configuration before we show the *Completions* buffer.
This is buffer local in the buffer where the completion is
performed."))

(make-variable-buffer-local
 (defvar slime-completions-window nil
   "The window displaying *Completions* after saving window configuration.
If this window is no longer active or displaying the completions
buffer then we can ignore `slime-complete-saved-window-configuration'."))

(defun slime-complete-maybe-save-window-configuration ()
  "Maybe save the current window configuration.
Return true if the configuration was saved."
  (unless (or slime-complete-saved-window-configuration
              (get-buffer-window slime-completions-buffer-name))
    (setq slime-complete-saved-window-configuration
          (current-window-configuration))
    t))

(defun slime-complete-delay-restoration ()
  (make-local-hook 'pre-command-hook)
  (add-hook 'pre-command-hook
            'slime-complete-maybe-restore-window-configuration))

(defun slime-complete-forget-window-configuration ()
  (setq slime-complete-saved-window-configuration nil)
  (setq slime-completions-window nil))

(defun slime-complete-restore-window-configuration ()
  "Restore the window config if available."
  (remove-hook 'pre-command-hook
               'slime-complete-maybe-restore-window-configuration)
  (when (and slime-complete-saved-window-configuration
             (slime-completion-window-active-p))
    ;; XEmacs does not allow us to restore a window configuration from
    ;; pre-command-hook, so we do it asynchronously.
    (slime-run-when-idle
     (lambda ()
       (save-excursion
         (set-window-configuration
          slime-complete-saved-window-configuration))
       (setq slime-complete-saved-window-configuration nil)
       (when (buffer-live-p slime-completions-buffer-name)
         (kill-buffer slime-completions-buffer-name))))))

(defun slime-complete-maybe-restore-window-configuration ()
  "Restore the window configuration, if the following command
terminates a current completion."
  (remove-hook 'pre-command-hook
               'slime-complete-maybe-restore-window-configuration)
  (condition-case err
      (cond ((find last-command-char "()\"'`,# \r\n:")
             (slime-complete-restore-window-configuration))
            ((not (slime-completion-window-active-p))
             (slime-complete-forget-window-configuration))
            (t
             (slime-complete-delay-restoration)))
    (error
     ;; Because this is called on the pre-command-hook, we mustn't let
     ;; errors propagate.
     (message "Error in slime-complete-restore-window-configuration: %S" err))))

(defun slime-completion-window-active-p ()
  "Is the completion window currently active?"
  (and (window-live-p slime-completions-window)
       (equal (buffer-name (window-buffer slime-completions-window))
              slime-completions-buffer-name)))

(defun slime-display-completion-list (completion-list)
  (let ((savedp (slime-complete-maybe-save-window-configuration)))
    (with-output-to-temp-buffer slime-completions-buffer-name
      (display-completion-list completion-list)
      (with-current-buffer standard-output
        (set-syntax-table lisp-mode-syntax-table)))
    (when savedp
      (setq slime-completions-window
            (get-buffer-window slime-completions-buffer-name)))))
  
(defun slime-complete-symbol ()
  "Complete the symbol at point.

Completion is performed by `slime-complete-symbol-function'."
  (interactive)
  (funcall slime-complete-symbol-function))

(defun* slime-complete-symbol* ()
  "Expand abbreviations and complete the symbol at point."
  ;; NB: It is only the name part of the symbol that we actually want
  ;; to complete -- the package prefix, if given, is just context.
  (when (save-excursion (re-search-backward "\"[^ \t\n]+\\=" nil t))
    (return-from slime-complete-symbol* 
      (let ((comint-completion-addsuffix '("/" . "\"")))
        (comint-dynamic-complete-as-filename))))
  (let* ((end (move-marker (make-marker) (slime-symbol-end-pos)))
         (beg (move-marker (make-marker) (slime-symbol-start-pos)))
         (prefix (buffer-substring-no-properties beg end))
         (completion-result (slime-completions prefix))
         (completion-set (first completion-result))
         (completed-prefix (second completion-result)))
    (if (null completion-set)
        (progn (slime-minibuffer-respecting-message
                "Can't find completion for \"%s\"" prefix)
               (ding)
               (slime-complete-restore-window-configuration))
      (goto-char end)
      (insert-and-inherit completed-prefix)
      (delete-region beg end)
      (goto-char (+ beg (length completed-prefix)))
      (cond ((and (member completed-prefix completion-set)
                  (= (length completion-set) 1))
             (slime-minibuffer-respecting-message "Sole completion")
             (when slime-complete-symbol*-fancy
               (slime-complete-symbol*-fancy-bit))
             (slime-complete-restore-window-configuration))
            ;; Incomplete
            (t
             (when (member completed-prefix completion-set)
               (slime-minibuffer-respecting-message "Complete but not unique"))
             (let ((unambiguous-completion-length
                    (loop for c in completion-set
                          minimizing (or (mismatch completed-prefix c)
                                         (length completed-prefix)))))
               (goto-char (+ beg unambiguous-completion-length))
               (slime-display-completion-list completion-set)
               (slime-complete-delay-restoration)))))))

(defun slime-complete-symbol*-fancy-bit ()
  "Do fancy tricks after completing a symbol.
\(Insert a space or close-paren based on arglist information.)"
  (let ((arglist (slime-get-arglist (slime-symbol-name-at-point))))
    (when arglist
      (let ((args
             ;; Don't intern these symbols
             (let ((obarray (make-vector 10 0)))
               (cdr (read arglist))))
            (function-call-position-p
             (save-excursion
                (backward-sexp)
                (equal (char-before) ?\())))
        (when function-call-position-p
          (if (null args)
              (insert-and-inherit ")")
            (insert-and-inherit " ")
            (when (and slime-space-information-p
                       (slime-background-activities-enabled-p)
                       (not (minibuffer-window-active-p (minibuffer-window))))
              (slime-echo-arglist))))))))

(defun* slime-simple-complete-symbol ()
  "Complete the symbol at point.  
Perform completion more similar to Emacs' complete-symbol."
  (when (save-excursion (re-search-backward "\"[^ \t\n]+\\=" nil t))
    (return-from slime-simple-complete-symbol 
      (comint-dynamic-complete-as-filename)))
  (let* ((end (point))
         (beg (slime-symbol-start-pos))
         (prefix (buffer-substring-no-properties beg end)))
    (destructuring-bind (completion-set completed-prefix)
        (slime-simple-completions prefix)
      (if (null completion-set)
          (progn (slime-minibuffer-respecting-message
                  "Can't find completion for \"%s\"" prefix)
                 (ding)
                 (slime-complete-restore-window-configuration))
        (insert-and-inherit (substring completed-prefix (length prefix)))
        (cond ((= (length completion-set) 1)
               (slime-minibuffer-respecting-message "Sole completion")
               (slime-complete-restore-window-configuration))
              ;; Incomplete
              (t
               (slime-minibuffer-respecting-message "Complete but not unique")
               (slime-display-completion-list completion-set)
               (slime-complete-delay-restoration)))))))

(defun slime-minibuffer-respecting-message (format &rest format-args)
  "Display TEXT as a message, without hiding any minibuffer contents."
  (let ((text (format " [%s]" (apply #'format format format-args))))
    (if (minibuffer-window-active-p (minibuffer-window))
        (if (fboundp 'temp-minibuffer-message) ;; XEmacs
            (temp-minibuffer-message text)
          (minibuffer-message text))
      (message "%s" text))))

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
         (cons (lexical-let ((package (slime-current-package))
                             (connection (slime-connection)))
                 (lambda ()
                   (setq slime-buffer-package package)
                   (setq slime-buffer-connection connection)
                   (set-syntax-table lisp-mode-syntax-table)))
	       minibuffer-setup-hook)))
    (read-from-minibuffer prompt initial-value slime-read-expression-map
			  nil 'slime-read-expression-history)))

(defun slime-bogus-completion-alist (list)
  "Make an alist out of list.
The same elements go in the CAR, and nil in the CDR. To support the
apparently very stupid `try-completions' interface, that wants an
alist but ignores CDRs."
  (mapcar (lambda (x) (cons x nil)) list))

(defun slime-completions (prefix)
  (slime-eval `(swank:completions ,prefix ,(slime-current-package))))

(defun slime-simple-completions (prefix)
  (slime-eval `(swank:simple-completions ,prefix ,(slime-current-package))))


;;;; Fuzzy completion

(defvar slime-fuzzy-target-buffer nil
  "The buffer that is the target of the completion activities.")
(defvar slime-fuzzy-saved-window-configuration nil
  "The saved window configuration before the fuzzy completion
buffer popped up.")
(defvar slime-fuzzy-start nil
  "The beginning of the completion slot in the target buffer.
This is a non-advancing marker.")
(defvar slime-fuzzy-end nil
  "The end of the completion slot in the target buffer.
This is an advancing marker.")
(defvar slime-fuzzy-original-text nil
  "The original text that was in the completion slot in the
target buffer.  This is what is put back if completion is
aborted.")
(defvar slime-fuzzy-text nil
  "The text that is currently in the completion slot in the
target buffer.  If this ever doesn't match, the target buffer has
been modified and we abort without touching it.")
(defvar slime-fuzzy-first nil
  "The position of the first completion in the completions buffer.
The descriptive text and headers are above this.")
(defvar slime-fuzzy-current-completion nil
  "The current completion object.  If this is the same before and
after point moves in the completions buffer, the text is not
replaced in the target for efficiency.")

(define-derived-mode slime-fuzzy-completions-mode 
  fundamental-mode "Fuzzy Completions"
  "Major mode for presenting fuzzy completion results.

\\<slime-fuzzy-completions-map>\
\\{slime-fuzzy-completions-map}"
  (use-local-map slime-fuzzy-completions-map))

(defvar slime-fuzzy-completions-map  
  (let* ((map (make-sparse-keymap)))
    
    (define-key map "q" 'slime-fuzzy-abort)
    (define-key map "\r" 'slime-fuzzy-select)
    
    (define-key map "n" 'slime-fuzzy-next)
    (define-key map "\M-n" 'slime-fuzzy-next)
    
    (define-key map "p" 'slime-fuzzy-prev)
    (define-key map "\M-p" 'slime-fuzzy-prev)
    
    (define-key map "\d" 'scroll-down)
    (define-key map " " 'scroll-up)
    
    (define-key map [mouse-2] 'slime-fuzzy-select/mouse)
    
    map)
  "Keymap for slime-fuzzy-completions-mode.")

(defun slime-fuzzy-completions (prefix &optional default-package)
  "Get the list of sorted completion objects from completing
`prefix' in `package' from the connected Lisp."
  (let ((prefix (etypecase prefix
		  (symbol (symbol-name prefix))
		  (string prefix))))
    (slime-eval `(swank:fuzzy-completions ,prefix 
                                          ,(or default-package
                                               (slime-find-buffer-package)
                                               (slime-current-package))))))

(defun slime-fuzzy-selected (prefix completion)
  "Tell the connected Lisp that the user selected completion
`completion' as the completion for `prefix'."
  (let ((no-properties (copy-sequence prefix)))
    (set-text-properties 0 (length no-properties) nil no-properties)
    (slime-eval `(swank:fuzzy-completion-selected ,no-properties 
                                                  ',completion))))

(defun* slime-fuzzy-complete-symbol ()
  "Fuzzily completes the abbreviation at point into a symbol."
  (interactive)
  (when (save-excursion (re-search-backward "\"[^ \t\n]+\\=" nil t))
    (return-from slime-fuzzy-complete-symbol 
      (comint-dynamic-complete-as-filename)))
  (let* ((end (move-marker (make-marker) (slime-symbol-end-pos)))
         (beg (move-marker (make-marker) (slime-symbol-start-pos)))
         (prefix (buffer-substring-no-properties beg end))
         (completion-set (slime-fuzzy-completions prefix)))
    (if (null completion-set)
        (progn (slime-minibuffer-respecting-message
                "Can't find completion for \"%s\"" prefix)
               (ding)
               (slime-complete-restore-window-configuration))
      (goto-char end)
      (cond ((= (length completion-set) 1)
             (insert-and-inherit (caar completion-set))
             (delete-region beg end)
             (goto-char (+ beg (length (caar completion-set))))
             (slime-minibuffer-respecting-message "Sole completion"))
            ;; Incomplete
            (t
             (slime-minibuffer-respecting-message "Complete but not unique")
             (slime-fuzzy-choices-buffer completion-set beg end))))))


(defun slime-get-fuzzy-buffer ()
  (get-buffer-create "*Fuzzy Completions*"))

(defvar slime-fuzzy-explanation
  "Click <mouse-2> on a completion to select it.
In this buffer, type n and p to navigate between completions.
Type RET to select the completion near point.  Type q to abort.
Flags: boundp fboundp generic-function class macro special-operator
\n"
  "The explanation that gets inserted at the beginning of the
*Fuzzy Completions* buffer.")

(defun slime-fuzzy-insert-completion-choice (completion max-length)
  "Inserts the completion object `completion' as a formatted
completion choice into the current buffer, and mark it with the
proper text properties."
  (let ((start (point))
        (symbol (first completion))
        (score (second completion))
        (chunks (third completion))
        (flags (fourth completion)))
    (insert symbol)
    (let ((end (point)))
      (dolist (chunk chunks)
        (put-text-property (+ start (first chunk)) 
                           (+ start (first chunk) 
                              (length (second chunk)))
                           'face 'bold))
      (put-text-property start (point) 'mouse-face 'highlight)
      (dotimes (i (- max-length (- end start)))
        (insert " "))
      (insert (format " %s%s%s%s%s%s %8.2f"
                      (if (member :boundp flags) "b" "-")
                      (if (member :fboundp flags) "f" "-")
                      (if (member :generic-function flags) "g" "-")
                      (if (member :class flags) "c" "-")
                      (if (member :macro flags) "m" "-")
                      (if (member :special-operator flags) "s" "-")
                      score))
      (insert "\n")
      (put-text-property start (point) 'completion completion))))

(defun slime-fuzzy-insert (text)
  "Inserts `text' into the target buffer in the completion slot.
If the buffer has been modified in the meantime, abort the
completion process.  Otherwise, update all completion variables
so that the new text is present."
  (with-current-buffer slime-fuzzy-target-buffer
    (cond 
     ((not (string-equal slime-fuzzy-text 
                         (buffer-substring slime-fuzzy-start
                                           slime-fuzzy-end)))
      (slime-fuzzy-done)
      (beep)
      (message "Target buffer has been modified!"))
     (t
      (goto-char slime-fuzzy-start)
      (delete-region slime-fuzzy-start slime-fuzzy-end)
      (insert-and-inherit text)
      (setq slime-fuzzy-text text)
      (goto-char slime-fuzzy-end)))))

(defun slime-fuzzy-choices-buffer (completions start end)
  "Creates (if neccessary), populates, and pops up the *Fuzzy
Completions* buffer with the completions from `completions' and
the completion slot in the current buffer bounded by `start' and
`end'.  This saves the window configuration before popping the
buffer so that it can possibly be restored when the user is
done."
  (setq slime-fuzzy-target-buffer (current-buffer))
  (setq slime-fuzzy-start (move-marker (make-marker) start))
  (setq slime-fuzzy-end (move-marker (make-marker) end))
  (set-marker-insertion-type slime-fuzzy-end t)
  (setq slime-fuzzy-original-text (buffer-substring start end))
  (setq slime-fuzzy-text slime-fuzzy-original-text)
  (slime-fuzzy-save-window-configuration)
  (with-current-buffer (slime-get-fuzzy-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (slime-fuzzy-completions-mode)
    (insert slime-fuzzy-explanation)
    (let ((max-length 12))
      (dolist (completion completions)
        (setf max-length (max max-length (length (first completion)))))
      (insert "Completion:")
      (dotimes (i (- max-length 10)) (insert " "))
      (insert "Flags: Score:\n")
      (dotimes (i max-length) (insert "-"))
      (insert " ------ --------\n")
      (setq slime-fuzzy-first (point))
      (dolist (completion completions)
        (slime-fuzzy-insert-completion-choice completion max-length))
      (setq buffer-read-only t))
    (setq slime-fuzzy-current-completion
          (caar completions))
    (slime-fuzzy-insert (caar completions))
    (goto-char slime-fuzzy-first)
    (pop-to-buffer (current-buffer))
    (add-hook (make-local-variable 'post-command-hook)
              'slime-fuzzy-post-command-hook)))

(defun slime-fuzzy-insert-from-point ()
  "Inserts the completion that is under point in the completions
buffer into the target buffer.  If the completion in question had
already been inserted, it does nothing."
  (with-current-buffer (slime-get-fuzzy-buffer)
    (let ((current-completion (get-text-property (point) 'completion)))
      (when (and current-completion
                 (not (eq slime-fuzzy-current-completion 
                          current-completion)))
        (slime-fuzzy-insert 
         (first (get-text-property (point) 'completion)))
        (setq slime-fuzzy-current-completion
              current-completion)))))

(defun slime-fuzzy-post-command-hook ()
  "The post-command-hook for the *Fuzzy Completions* buffer.
This makes sure the completion slot in the target buffer matches
the completion that point is on in the completions buffer."
  (condition-case err
      (when slime-fuzzy-target-buffer
        (slime-fuzzy-insert-from-point))
    (error
     ;; Because this is called on the post-command-hook, we mustn't let
     ;; errors propagate.
     (message "Error in slime-fuzzy-post-command-hook: %S" err))))

(defun slime-fuzzy-next ()
  "Moves point directly to the next completion in the completions
buffer."
  (interactive)
  (goto-char 
   (next-single-char-property-change (point) 'completion)))

(defun slime-fuzzy-prev ()
  "Moves point directly to the previous completion in the
completions buffer."
  (interactive)
  (goto-char (previous-single-char-property-change 
              (point) 'completion
              nil slime-fuzzy-first)))

(defun slime-fuzzy-abort ()
  "Aborts the completion process, setting the completions slot in
the target buffer back to its original contents."
  (interactive)
  (when slime-fuzzy-target-buffer
    (slime-fuzzy-insert slime-fuzzy-original-text)
    (slime-fuzzy-done)))

(defun slime-fuzzy-select ()
  "Selects the current completion, making sure that it is inserted 
into the target buffer.  This tells the connected Lisp what completion
was selected."
  (interactive)
  (when slime-fuzzy-target-buffer
    (with-current-buffer (slime-get-fuzzy-buffer)
      (let ((completion (get-text-property (point) 'completion)))
        (when completion
          (slime-fuzzy-insert (first completion))
          (slime-fuzzy-selected slime-fuzzy-original-text
                                completion)
          (slime-fuzzy-done))))))

(defun slime-fuzzy-select/mouse (event)
  "Handle a mouse-2 click on a completion choice as if point were
on the completion choice and the slime-fuzzy-select command was
run."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (save-excursion
      (goto-char (posn-point (event-end event)))
      (when (get-text-property (point) 'mouse-face)
        (slime-fuzzy-insert-from-point)
        (slime-fuzzy-select)))))

(defun slime-fuzzy-done ()
  "Cleans up after the completion process.  This removes all hooks,
and attempts to restore the window configuration.  If this fails,
it just burys the completions buffer and leaves the window
configuration alone."
  (set-buffer slime-fuzzy-target-buffer)
  (remove-hook 'post-command-hook
               'slime-fuzzy-post-command-hook)
  (if (slime-fuzzy-maybe-restore-window-configuration)
      (bury-buffer (slime-get-fuzzy-buffer))
    ;; We couldn't restore the windows, so just bury the fuzzy
    ;; completions buffer and let something else fill it in.
    (pop-to-buffer (slime-get-fuzzy-buffer))
    (bury-buffer))
  (pop-to-buffer slime-fuzzy-target-buffer)
  (goto-char slime-fuzzy-end)
  (setq slime-fuzzy-target-buffer nil))

(defun slime-fuzzy-save-window-configuration ()
  "Saves the current window configuration, and (if the
window-configuration-change-hook variable exists) sets up for the
saved configuration to be nullified if the user changes the
window configuration further.  Adding the nullification routine
to window-configuration-change-hook is delayed so that the
windows stabalize before we start listening on the hook."
  (setq slime-fuzzy-saved-window-configuration 
        (current-window-configuration))
  (when (boundp 'window-configuration-change-hook)
    (run-with-timer 
     0.5 nil 'slime-fuzzy-window-configuration-change-add-hook)))

(defun slime-fuzzy-maybe-restore-window-configuration ()
  "Restores the saved window configuration if it has not been
nullified."
  (when (boundp 'window-configuration-change-hook)
    (remove-hook 'window-configuration-change-hook
                 'slime-fuzzy-window-configuration-change))
  (if (not slime-fuzzy-saved-window-configuration)
      nil
    (set-window-configuration slime-fuzzy-saved-window-configuration)
    (setq slime-fuzzy-saved-window-configuration nil)
    t))

(defun slime-fuzzy-window-configuration-change-add-hook ()
  "Sets up slime-fuzzy-window-configuration-change on
window-configuration-change-hook."
  (add-hook 'window-configuration-change-hook
            'slime-fuzzy-window-configuration-change))

(defun slime-fuzzy-window-configuration-change ()
  "Called on window-configuration-change-hook.  Since the window
configuration was changed, we nullify our saved configuration."
  (remove-hook 'window-configuration-change-hook
               'slime-fuzzy-window-configuration-change)
  (setq slime-fuzzy-saved-window-configuration nil))


;;;; Edit definition

(defvar slime-find-definition-history-ring (make-ring 20)
  "History ring recording the definition-finding \"stack\".")

(defun slime-push-definition-stack (&optional mark)
  "Add MARKER to the edit-definition history stack.
If MARKER is nil, use the point."
  (ring-insert-at-beginning slime-find-definition-history-ring 
                            (or mark (point-marker))))

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

(defstruct (slime-definition (:conc-name slime-definition.)
                             (:type list))
  dspec location)

(defun slime-edit-definition (name &optional where)
  "Lookup the definition of the symbol at point.  
If there's no symbol at point, or a prefix argument is given, then the
function name is prompted."
  (interactive (list (slime-read-symbol-name "Symbol: ")))
  (let ((definitions (slime-eval `(swank:find-definitions-for-emacs ,name))))
    (if (null definitions)
        (if slime-edit-definition-fallback-function
            (funcall slime-edit-definition-fallback-function name)
          (error "No known definition for: %s" name))
      (slime-goto-definition name definitions where))))

(defun slime-goto-definition (name definitions &optional where)
  (slime-push-definition-stack)
  (if (slime-length> definitions 1)
      (slime-show-definitions name definitions)
    (let ((def (car definitions)))
      (destructure-case (slime-definition.location def)
        ;; Take care of errors before switching any windows/buffers.
        ((:error message)
         (error "%s" message))
        (t
         (cond ((equal where 'window)
                (slime-goto-definition-other-window (car definitions)))
               ((equal where 'frame)
                (let ((pop-up-frames t))
                  (slime-goto-definition-other-window (car definitions))))
               (t
                (slime-goto-source-location (slime-definition.location
                                             (car definitions)))
                (switch-to-buffer (current-buffer)))))))))

(defun slime-goto-definition-other-window (definition)
  (slime-pop-to-other-window)
  (slime-goto-source-location (slime-definition.location definition))
  (switch-to-buffer (current-buffer)))

(defun slime-pop-to-other-window ()
  "Pop to the other window, but not to any particular buffer."
  (pop-to-buffer (current-buffer) t))

(defun slime-edit-definition-other-window (name)
  "Like `slime-edit-definition' but switch to the other window."
  (interactive (list (slime-read-symbol-name "Symbol: ")))
  (slime-edit-definition name 'window))

(defun slime-edit-definition-other-frame (name)
  "Like `slime-edit-definition' but switch to the other window."
  (interactive (list (slime-read-symbol-name "Symbol: ")))
  (slime-edit-definition name 'frame))

(defun slime-edit-definition-with-etags (name)
  (interactive (list (slime-read-symbol-name "Symbol: ")))
  (let ((tagdefs (slime-etags-definitions name)))
    (cond (tagdefs 
           (message "Using tag file...")
           (slime-goto-definition name tagdefs))
          (t
           (error "No known definition for: %s" name)))))

(defun slime-etags-definitions (name)
  "Search definitions matching NAME in the tags file.
The result is a (possibly empty) list of definitions."
  (require 'etags)
  (let ((defs '()))
    (save-excursion
      (let ((first-time t))
        (while (visit-tags-table-buffer (not first-time))
          (setq first-time nil)
          (goto-char (point-min))
          (while (search-forward name nil t)
            (beginning-of-line)
            (destructuring-bind (hint line &rest pos) (etags-snarf-tag)
              (unless (eq hint t)       ; hint==t if we are in a filename line
                (let ((file (expand-file-name (file-of-tag))))
                  (let ((loc `(:location (:file ,file)
                                         (:line ,line)
                                         (:snippet ,hint))))
                    (push (list hint loc) defs))))))))
      (reverse defs))))

(defun slime-show-definitions (name definitions)
  (slime-show-xrefs 
   `((,name . ,(loop for (dspec location) in definitions
                     collect (cons dspec location))))
   'definition
   name
   (slime-current-package)))

;;;;; first-change-hook

(defun slime-first-change-hook ()
  "Notify Lisp that a source file's buffer has been modified."
  ;; Be careful not to disturb anything!
  ;; In particular if we muck up the match-data then query-replace
  ;; breaks. -luke (26/Jul/2004)
  (save-excursion
    (save-match-data
      (when (and (buffer-file-name)
                 (slime-connected-p))
        (let ((filename (slime-to-lisp-filename (buffer-file-name))))
          (slime-eval-async `(swank:buffer-first-change ,filename)))))))

(defun slime-setup-first-change-hook ()
  (add-hook (make-local-variable 'first-change-hook)
            'slime-first-change-hook))

(add-hook 'slime-mode-hook 'slime-setup-first-change-hook)


;;;; Eval for Lisp

(defun slime-eval-for-lisp (thread tag fun args)
  (let ((ok nil) 
        (value nil)
        (c (slime-connection)))
    (unwind-protect (progn 
                      (setq value (apply fun args))
                      (setq ok t))
      (let ((result (if ok `(:ok ,value) `(:abort))))
        (slime-dispatch-event `(:emacs-return ,thread ,tag ,result))))))


;;;; `ED'

(defvar slime-ed-frame nil
  "The frame used by `slime-ed'.")

(defvar slime-ed-use-dedicated-frame t
  "*When non-nil, `slime-ed' will create and reuse a dedicated frame.")

(defun slime-ed (what)
  "Edit WHAT.

WHAT can be:
  A filename (string),
  A list (FILENAME LINE [COLUMN]),
  A function name (symbol),
  nil.

This for use in the implementation of COMMON-LISP:ED."
  ;; Without `save-excursion' very strange things happen if you call
  ;; (swank:ed-in-emacs X) from the REPL. -luke (18/Jan/2004)
  (save-excursion
    (when slime-ed-use-dedicated-frame
      (unless (and slime-ed-frame (frame-live-p slime-ed-frame))
        (setq slime-ed-frame (make-frame)))
      (select-frame slime-ed-frame))
    (cond ((stringp what)
           (find-file (slime-from-lisp-filename what)))
          ((listp what) 
           (find-file (first (slime-from-lisp-filename what)))
           (goto-line (second what))
           ;; Find the correct column, without going past the end of
           ;; the line.
           (let ((col (third what)))
             (while (and col
                         (< (point) (point-at-eol))
                         (/= (decf col) -1))
               (forward-char 1))))
          ((and what (symbolp what))
           (slime-edit-definition (symbol-name what)))
          (t nil))))                    ; nothing in particular


;;;; Interactive evaluation.

(defun slime-interactive-eval (string)
  "Read and evaluate STRING and print value in minibuffer.

Note: If a prefix argument is in effect then the result will be
inserted in the current buffer."
  (interactive (list (slime-read-from-minibuffer "Slime Eval: ")))
  (slime-insert-transcript-delimiter string)
  (cond ((not current-prefix-arg)
         (slime-eval-with-transcript `(swank:interactive-eval ,string) 
                                     'slime-display-eval-result))
        (t
         (slime-eval-print string))))

(defun slime-display-eval-result (value)
  (slime-message "%s" value))

(defun slime-eval-print (string)
  "Eval STRING in Lisp; insert any output and the result at point."
  (slime-eval-async `(swank:eval-and-grab-output ,string)
                    (lexical-let ((buffer (current-buffer)))
                      (lambda (result)
                        (with-current-buffer buffer
                          (destructuring-bind (output value) result
                            (insert output value)))))))

(defun slime-eval-with-transcript (form &optional fn wait)
  "Send FROM and PACKAGE to Lisp and pass the result to FN.
Display the result in the message area, if FN is nil.
Show the output buffer if the evaluation causes any output."
  (with-current-buffer (slime-output-buffer)
    (slime-with-output-end-mark (slime-mark-output-start)))
  (with-lexical-bindings (fn)
    (slime-eval-async form
                      (lambda (value)
                        (with-current-buffer (slime-output-buffer)
                          (cond (fn (funcall fn value))
                                (t (message "%s" value)))
                          (slime-show-last-output))))))

(defun slime-eval-describe (form)
  "Evaluate FORM in Lisp and display the result in a new buffer."
  (lexical-let ((package (slime-current-package)))
    (slime-eval-with-transcript
     form (lambda (string) (slime-show-description string package)))))

(defun slime-insert-transcript-delimiter (string)
  (with-current-buffer (slime-output-buffer)
    (slime-with-output-end-mark
     (unless (bolp) (insert "\n"))
     (slime-insert-propertized
      '(slime-transcript-delimiter t)
      ";;;; " (subst-char-in-string ?\n ?\ 
                                    (substring string 0 
                                               (min 60 (length string))))
      " ...\n"))))

(defun slime-display-buffer-region (buffer start end &optional other-window)
  "Like `display-buffer', but only display the specified region."
  (let ((window-min-height 1))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (goto-char start)
          (beginning-of-line)
          (narrow-to-region (point) end)
          (let ((window (display-buffer buffer other-window t)))
            (set-window-start window (point))
            (unless (or (one-window-p t)
                        (/= (frame-width) (window-width)))
              (set-window-text-height window (/ (1- (frame-height)) 2)))
            (shrink-window-if-larger-than-buffer window)
            window))))))
  
(defun slime-last-expression ()
  (buffer-substring-no-properties (save-excursion (backward-sexp) (point))
				  (point)))

(defun slime-eval-last-expression ()
  "Evaluate the expression preceding point."
  (interactive)
  (slime-interactive-eval (slime-last-expression)))

(defun slime-eval-last-expression-display-output ()
  "Display output buffer and evaluate the expression preceding point."
  (interactive)
  (slime-display-output-buffer)
  (slime-interactive-eval (slime-last-expression)))
  
(defun slime-eval-defun ()
  "Evaluate the current toplevel form.
Use `slime-re-evaluate-defvar' if the from starts with '(defvar'"
  (interactive)
  (let ((form (slime-defun-at-point)))
    (cond ((string-match "^(defvar " form)
           (slime-re-evaluate-defvar form))
          (t
           (slime-interactive-eval form)))))

(defun slime-eval-region (start end)
  "Evaluate region."
  (interactive "r")
  (slime-eval-with-transcript
   `(swank:interactive-eval-region 
     ,(buffer-substring-no-properties start end))))

(defun slime-eval-buffer ()
  "Evaluate the current buffer.
The value is printed in the echo area."
  (interactive)
  (slime-eval-region (point-min) (point-max)))

(defun slime-re-evaluate-defvar (form)
  "Force the re-evaluaton of the defvar form before point.  

First make the variable unbound, then evaluate the entire form."
  (interactive (list (slime-last-expression)))
  (slime-eval-with-transcript `(swank:re-evaluate-defvar ,form)))

(defun slime-pprint-eval-last-expression ()
  "Evaluate the form before point; pprint the value in a buffer."
  (interactive)
  (slime-eval-describe `(swank:pprint-eval ,(slime-last-expression))))

(defun slime-eval-print-last-expression (string)
  "Evaluate sexp before point; print value into the current buffer"
  (interactive (list (slime-last-expression)))
  (insert "\n")
  (slime-eval-print string))

;;;; Edit Lisp value
;;;
(defun slime-edit-value (form-string)
  "\\<slime-edit-value-mode-map>\
Edit the value of a setf'able form in a new buffer.
The value is inserted into a temporary buffer for editing and then set
in Lisp when committed with \\[slime-edit-value-commit]."
  (interactive 
   (list (slime-read-from-minibuffer "Edit value (evaluated): "
				     (slime-sexp-at-point))))
  (slime-eval-async `(swank:value-for-editing ,form-string)
                    (lexical-let ((form-string form-string)
                                  (package (slime-current-package)))
                      (lambda (result)
                        (slime-edit-value-callback form-string result package)))))

(make-variable-buffer-local
 (defvar slime-edit-form-string nil
   "The form being edited by `slime-edit-value'."))

(define-minor-mode slime-edit-value-mode
  "Mode for editing a Lisp value."
  nil
  " edit"
  '(("\C-c\C-c" . slime-edit-value-commit)))

(defun slime-edit-value-callback (form-string current-value package)
  (let ((name (generate-new-buffer-name (format "*Edit %s*" form-string))))
    (with-current-buffer (slime-get-temp-buffer-create name :mode 'lisp-mode)
    (slime-mode 1)
    (slime-temp-buffer-mode -1)         ; don't want binding of 'q'
    (slime-edit-value-mode 1)
    (setq slime-edit-form-string form-string)
    (setq slime-buffer-connection (slime-connection))
    (setq slime-buffer-package package)
    (insert current-value)
    (pop-to-buffer (current-buffer)))))

(defun slime-edit-value-commit ()
  "Commit the edited value to the Lisp image.
\\(See `slime-edit-value'.)"
  (interactive)
  (if (null slime-edit-form-string)
      (error "Not editing a value.")
    (let ((value (buffer-substring-no-properties (point-min) (point-max))))
      (lexical-let ((buffer (current-buffer)))
        (slime-eval-async `(swank:commit-edited-value ,slime-edit-form-string
                                                      ,value)
                          (lambda (_)
                            (with-current-buffer buffer
                              (slime-dismiss-temp-buffer)
                              (kill-buffer buffer))))))))

;;;; Tracing

(defun slime-untrace-all ()
  "Untrace all functions."
  (interactive)
  (slime-eval `(swank:untrace-all)))

(defun slime-toggle-trace-fdefinition (&optional using-context-p)
  "Toggle trace."
  (interactive "P")
  (let ((spec (if using-context-p
                  (slime-extract-context)
                (slime-symbol-at-point))))
    (let ((spec (slime-trace-query spec)))
      (message "%s" (slime-eval `(swank:swank-toggle-trace ,spec))))))

(defun slime-trace-query (spec)
  "Ask the user which function to trace; SPEC is the default.
The result is a string."
  (cond ((null spec)
         (slime-read-from-minibuffer "(Un)trace: "))
        ((symbolp spec)
         (slime-read-from-minibuffer "(Un)trace: " (symbol-name spec)))
        (t
         (destructure-case spec
           ((setf n)
            (slime-read-from-minibuffer "(Un)trace: " (prin1-to-string spec)))
           (((:defun :defmacro) n)
            (slime-read-from-minibuffer "(Un)trace: " (prin1-to-string n)))
           ((:defgeneric n)
            (let* ((name (prin1-to-string n))
                   (answer (slime-read-from-minibuffer "(Un)trace: " name)))
              (cond ((and (string= name answer)
                          (y-or-n-p (concat "(Un)trace also all " 
                                            "methods implementing " 
                                            name "? ")))
                     (prin1-to-string `(:defgeneric ,n)))
                    (t
                     answer))))
           ((:defmethod &rest _)
            (slime-read-from-minibuffer "(Un)trace: " (prin1-to-string spec)))
           ((:call caller callee)
            (let* ((callerstr (prin1-to-string caller))
                   (calleestr (prin1-to-string callee))
                   (answer (slime-read-from-minibuffer "(Un)trace: " 
                                                       calleestr)))
              (cond ((and (string= calleestr answer)
                          (y-or-n-p (concat "(Un)trace only when " calleestr
                                            " is called by " callerstr "? ")))
                     (prin1-to-string `(:call ,caller ,callee)))
                    (t
                     answer))))
           (((:labels :flet) &rest _)
            (slime-read-from-minibuffer "(Un)trace local function: "
                                        (prin1-to-string spec)))))))

(defun slime-extract-context ()
  "Parse the context for the symbol at point.  
Nil is returned if there's no symbol at point.  Otherwise we detect
the following cases (the . shows the point position):

 (defun n.ame (...) ...)                 -> (:defun name)
 (defun (setf n.ame) (...) ...)          -> (:defun (setf name))
 (defmethod n.ame (...) ...)             -> (:defmethod name (...))
 (defun ... (...) (labels ((n.ame (...)  -> (:labels (:defun ...) name)
 (defun ... (...) (flet ((n.ame (...)    -> (:flet (:defun ...) name)
 (defun ... (...) ... (n.ame ...) ...)   -> (:call (:defun ...) name)
 (defun ... (...) ... (setf (n.ame ...)  -> (:call (:defun ...) (setf name))

For other contexts we return the symbol at point."
  (let ((name (slime-symbol-name-at-point)))
    (if name
        (let ((symbol (read name)))
          (or (progn ;;ignore-errors 
                (slime-parse-context symbol))
              symbol)))))

(defun slime-parse-context (name)
  (save-excursion 
    (cond ((slime-in-expression-p '(defun *))          `(:defun ,name))
          ((slime-in-expression-p '(defmacro *))       `(:defmacro ,name))
          ((slime-in-expression-p '(defgeneric *))     `(:defgeneric ,name))
          ((slime-in-expression-p '(setf *))
           ;;a setf-definition, but which?
           (backward-up-list 1)
           (slime-parse-context `(setf ,name)))
          ((slime-in-expression-p '(defmethod *))
           (unless (looking-at "\\s ")
             (forward-sexp 1)) ; skip over the methodname
           (let (qualifiers arglist)
             (loop for e = (read (current-buffer))
                   until (listp e) do (push e qualifiers)
                   finally (setq arglist e))
             `(:defmethod ,name ,@qualifiers
                          ,(slime-arglist-specializers arglist))))
          ((and (symbolp name) 
                (slime-in-expression-p `(,name)))
           ;; looks like a regular call
           (let ((toplevel (ignore-errors (slime-parse-toplevel-form))))
             (cond ((slime-in-expression-p `(setf (*)))  ;a setf-call
                    (if toplevel
                        `(:call ,toplevel (setf ,name))
                      `(setf ,name)))
                   ((not toplevel)
                    name)
                   ((slime-in-expression-p `(labels ((*))))
                    `(:labels ,toplevel ,name))
                   ((slime-in-expression-p `(flet ((*))))
                    `(:flet ,toplevel ,name))
                   (t
                    `(:call ,toplevel ,name)))))
          (t 
           name))))

(defun slime-in-expression-p (pattern)
  "A helper function to determine the current context.
The pattern can have the form:
 pattern ::= ()    ;matches always
           | (*)   ;matches insde a list
           | (<symbol> <pattern>)   ;matches if the first element in
				    ; current the list is <symbol> and
                                    ; if <pattern> matches.
           | ((<pattern>))          ;matches if are in a nested list."
  (save-excursion
    (let ((path (reverse (slime-pattern-path pattern))))
      (loop for p in path
            always (ignore-errors 
                     (etypecase p
                       (symbol (slime-beginning-of-list) 
                               (looking-at (symbol-name p)))
                       (number (backward-up-list p)
                               t)))))))

(defun slime-pattern-path (pattern)
  ;; Compute the path to the * in the pattern to make matching
  ;; easier. The path is a list of symbols and numbers.  A number
  ;; means "(down-list <n>)" and a symbol "(look-at <sym>)")
  (if (null pattern)
      '()
    (etypecase (car pattern)
      ((member *) '())
      (symbol (cons (car pattern) (slime-pattern-path (cdr pattern))))
      (cons (cons 1 (slime-pattern-path (car pattern)))))))

(defun slime-beginning-of-list (&optional up)
  "Move backward the the beginning of the current expression.
Point is placed before the first expression in the list."
  (backward-up-list (or up 1))
  (down-list 1)
  (skip-syntax-forward " "))

(defun slime-parse-toplevel-form ()
  (save-excursion
    (beginning-of-defun)
    (down-list 1)
    (forward-sexp 1)
    (slime-parse-context (read (current-buffer)))))
		 
(defun slime-arglist-specializers (arglist)
  (cond ((or (null arglist)
	     (member (first arglist) '(&optional &key &rest &aux)))
	 (list))
	((consp (first arglist))
	 (cons (second (first arglist))
	       (slime-arglist-specializers (rest arglist))))
	(t
	 (cons 't 
	       (slime-arglist-specializers (rest arglist))))))

(defun slime-disassemble-symbol (symbol-name)
  "Display the disassembly for SYMBOL-NAME."
  (interactive (list (slime-read-symbol-name "Disassemble: ")))
  (slime-eval-describe `(swank:disassemble-symbol ,symbol-name)))

(defun slime-undefine-function (symbol-name)
  "Unbind the function slot of SYMBOL-NAME."
  (interactive (list (slime-read-symbol-name "fmakunbound: " t)))
  (slime-eval-async `(swank:undefine-function ,symbol-name)
                    (lambda (result) (message "%s" result))))

(defun slime-load-file (filename)
  "Load the Lisp file FILENAME."
  (interactive (list 
		(read-file-name "Load file: " nil nil
				nil (if (buffer-file-name)
                                        (file-name-sans-extension
                                         (file-name-nondirectory 
                                          (buffer-file-name)))))))
  (let ((lisp-filename (slime-to-lisp-filename (expand-file-name filename))))
    (slime-eval-with-transcript `(swank:load-file ,lisp-filename))))


;;;; Profiling

(defun slime-toggle-profile-fdefinition (fname-string)
  "Toggle profiling for FNAME-STRING."
  (interactive (list (slime-read-from-minibuffer
		      "(Un)Profile: " (slime-symbol-name-at-point))))
  (slime-eval-async `(swank:toggle-profile-fdefinition ,fname-string)
                    (lambda (r) (message "%s" r))))

(defun slime-unprofile-all ()
  "Unprofile all functions."
  (interactive)
  (slime-eval-async '(swank:unprofile-all)
                    (lambda (r) (message "%s" r))))

(defun slime-profile-report ()
  "Print profile report."
  (interactive)
  (slime-eval-with-transcript '(swank:profile-report)))

(defun slime-profile-reset ()
  "Reset profile counters."
  (interactive)
  (slime-eval-async (slime-eval `(swank:profile-reset))
                    (lambda (r) (message "%s" r))))

(defun slime-profiled-functions ()
  "Return list of names of currently profiled functions."
  (interactive)
  (slime-eval-async `(swank:profiled-functions)
                    (lambda (r) (message "%s" r))))

(defun slime-profile-package (package callers methods)
  "Profile all functions in PACKAGE.  
If CALLER is non-nil names have counts of the most common calling
functions recorded. 
If METHODS is non-nil, profile all methods of all generic function
having names in the given package."
  (interactive (list (slime-read-package-name "Package: ")
                     (y-or-n-p "Record the most common callers? ")
                     (y-or-n-p "Profile methods? ")))
  (slime-eval-async `(swank:profile-package ,package ,callers ,methods)
                    (lambda (r) (message "%s" r))))



;;;; Documentation

(defun slime-hyperspec-lookup (symbol-name)
  "A wrapper for `hyperspec-lookup'"
  (interactive (list (let ((symbol-at-point (slime-symbol-name-at-point)))
                       (if (and symbol-at-point
                                (intern-soft (downcase symbol-at-point)
                                             common-lisp-hyperspec-symbols))
                           symbol-at-point
                         (completing-read
                          "Look up symbol in Common Lisp HyperSpec: "
                          common-lisp-hyperspec-symbols #'boundp
                          t symbol-at-point
                          'common-lisp-hyperspec-history)))))
  (hyperspec-lookup symbol-name))
  
(defun slime-show-description (string package)
  (slime-with-output-to-temp-buffer ("*SLIME Description*") package (princ string)))

(defun slime-describe-symbol (symbol-name)
  "Describe the symbol at point."
  (interactive (list (slime-read-symbol-name "Describe symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (slime-eval-describe `(swank:describe-symbol ,symbol-name)))

(defun slime-documentation (symbol-name)
  "Display function- or symbol-documentation for SYMBOL-NAME."
  (interactive (list (slime-read-symbol-name "Documentation for symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (slime-eval-describe 
   `(swank:documentation-symbol ,symbol-name "(not documented)")))

(defun slime-describe-function (symbol-name)
  (interactive (list (slime-read-symbol-name "Describe symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (slime-eval-describe `(swank:describe-function ,symbol-name)))

(defun slime-apropos-summary (string case-sensitive-p package only-external-p)
  "Return a short description for the performed apropos search."
  (concat (if case-sensitive-p "Case-sensitive " "")
          "Apropos for "
          (format "%S" string)
          (if package (format " in package %S" package) "")
          (if only-external-p " (external symbols only)" "")))

(defun slime-apropos (string &optional only-external-p package 
                             case-sensitive-p)
  "Show all bound symbols whose names match STRING, a regular expression."
  (interactive
   (if current-prefix-arg
       (list (read-string "SLIME Apropos: ")
             (y-or-n-p "External symbols only? ")
             (let ((pkg (slime-read-package-name "Package: ")))
               (if (string= pkg "") nil pkg))
             (y-or-n-p "Case-sensitive? "))
     (list (read-string "SLIME Apropos: ") t nil nil)))
  (let ((buffer-package (or package (slime-current-package))))
    (slime-eval-async
     `(swank:apropos-list-for-emacs ,string ,only-external-p
                                    ,case-sensitive-p ,package)
     (lexical-let ((string string)
                   (package buffer-package)
                   (summary (slime-apropos-summary string case-sensitive-p
                                                   package only-external-p)))
       (lambda (r) (slime-show-apropos r string package summary))))))

(defun slime-apropos-all ()
  "Shortcut for (slime-apropos <pattern> nil nil)"
  (interactive)
  (slime-apropos (read-string "SLIME Apropos: ") nil nil))

(defun slime-apropos-package (package &optional internal)
  "Show apropos listing for symbols in PACKAGE.
With prefix argument include internal symbols."
  (interactive (list (let ((pkg (slime-read-package-name "Package: ")))
                       (if (string= pkg "") (slime-current-package) pkg))
                     current-prefix-arg))
  (slime-apropos "" (not internal) package))

(defun slime-show-apropos (plists string package summary)
  (if (null plists)
      (message "No apropos matches for %S" string)
    (slime-with-output-to-temp-buffer ("*SLIME Apropos*" apropos-mode) package
      (set-syntax-table lisp-mode-syntax-table)
      (slime-mode t)
      (if (boundp 'header-line-format)
          (setq header-line-format summary)
        (insert summary "\n\n"))
      (slime-set-truncate-lines)
      (slime-print-apropos plists))))

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
      (assert designator)
      (slime-insert-propertized (list 'face apropos-symbol-face
                                      'item designator
                                      'action 'slime-describe-symbol)
                                designator))
    (terpri)
    (let ((apropos-label-properties slime-apropos-label-properties))
      (loop for (prop namespace) 
	    in '((:variable "Variable")
		 (:function "Function")
		 (:generic-function "Generic Function")
		 (:setf "Setf")
		 (:type "Type")
		 (:class "Class")
                 (:alien-type "Alien type")
                 (:alien-struct "Alien struct")
                 (:alien-union "Alien type")
                 (:alien-enum "Alien enum"))
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
		(put-text-property start (point) 'type prop)
		(put-text-property start (point) 'action 'slime-call-describer)
		(terpri)))))))

(defun slime-call-describer (item)
  (let ((type (get-text-property (point) 'type)))
    (slime-eval-describe `(swank:describe-definition-for-emacs ,item ,type))))


;;;; XREF: cross-referencing

(defvar slime-xref-mode-map)
(defvar slime-xref-saved-window-configuration nil
  "Buffer local variable in xref windows.")

(define-derived-mode slime-xref-mode lisp-mode "xref"
  "slime-xref-mode: Major mode for cross-referencing.
\\<slime-xref-mode-map>\
The most important commands:
\\[slime-xref-quit]	- Dismiss buffer.
\\[slime-show-xref]	- Display referenced source and keep xref window.
\\[slime-goto-xref]	- Jump to referenced source and dismiss xref window.

\\{slime-xref-mode-map}"
  (setq font-lock-defaults nil)
  (setq delayed-mode-hooks nil)
  (slime-mode -1))

(slime-define-keys slime-xref-mode-map 
  ((kbd "RET") 'slime-show-xref)
  ("\C-m" 'slime-show-xref)
  (" " 'slime-goto-xref)
  ("q" 'slime-xref-quit)
  ("n" 'slime-next-line/not-add-newlines)
  ("p" 'previous-line))

(defun slime-next-line/not-add-newlines ()
  (interactive)
  (let ((next-line-add-newlines nil))
    (next-line 1)))

;; FIXME: binding SLDB keys in xref buffer? -luke
(dolist (spec slime-keys)
  (destructuring-bind (key command &key sldb prefixed &allow-other-keys) spec
    (when sldb
      (let ((key (if prefixed (concat slime-prefix-key key) key)))
        (define-key slime-xref-mode-map key command)))))


;;;;; XREF results buffer and window management

(defun slime-xref-buffer ()
  "Return the XREF results buffer.
If CREATE is non-nil, create it if necessary."
  (or (find-if (lambda (b) (string-match "*XREF\\[" (buffer-name b)))
               (buffer-list))
      (error "No XREF buffer")))

(defun slime-init-xref-buffer (package ref-type symbol)
  "Initialize the current buffer for displaying XREF information."
  (slime-xref-mode)
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq slime-buffer-package package)
  (slime-set-truncate-lines))

;; XXX: unused function
(defun slime-display-xref-buffer ()
  "Display the XREF results buffer in a window and select it."
  (let* ((buffer (slime-xref-buffer))
         (window (get-buffer-window buffer)))
    (if (and window (window-live-p window))
        (select-window window)
      (select-window (display-buffer buffer t))
      (shrink-window-if-larger-than-buffer))))

(defmacro* slime-with-xref-buffer ((package ref-type symbol) &body body)
  "Execute BODY in a xref buffer, then show that buffer."
  (let ((type (gensym))
        (sym (gensym)))
    `(let ((,type ,ref-type)
           (,sym ,symbol))
       (with-current-buffer (get-buffer-create 
                             (format "*XREF[%s: %s]*" ,type ,sym))
         (prog2 (progn
                  (slime-init-xref-buffer ,package ,type ,sym)
                  (make-local-variable 'slime-xref-saved-window-configuration)
                  (setq slime-xref-saved-window-configuration
                        (current-window-configuration)))
             (progn ,@body)
           (setq buffer-read-only t)
           (select-window (or (get-buffer-window (current-buffer) t)
                              (display-buffer (current-buffer) t)))
           (shrink-window-if-larger-than-buffer))))))

(put 'slime-with-xref-buffer 'lisp-indent-function 1)

(defun slime-insert-xrefs (xrefs)
  "Insert XREFS in the current-buffer.
XREFS is a list of the form ((GROUP . ((LABEL . LOCATION) ...)) ...)
GROUP and LABEL are for decoration purposes.  LOCATION is a source-location."
  (unless (bobp) (insert "\n"))
  (loop for (group . refs) in xrefs do 
        (progn
          (slime-insert-propertized '(face bold) group "\n")
          (loop for (label . location) in refs do
                (slime-insert-propertized 
                 (list 'slime-location location
                       'face 'font-lock-keyword-face)
                 "  " (slime-one-line-ify label) "\n"))))
  ;; Remove the final newline to prevent accidental window-scrolling
  (backward-char 1)
  (delete-char 1))

(defun slime-show-xrefs (xrefs type symbol package)
  "Show the results of an XREF query."
  (if (null xrefs)
      (message "No references found for %s." symbol)
    (setq slime-next-location-function 'slime-goto-next-xref)
    (slime-with-xref-buffer (package type symbol)
      (slime-insert-xrefs xrefs)
      (goto-char (point-min))
      (forward-line)
      (skip-chars-forward " \t"))))


;;;;; XREF commands

(defun slime-who-calls (symbol)
  "Show all known callers of the function SYMBOL."
  (interactive (list (slime-read-symbol-name "Who calls: " t)))
  (slime-xref :calls symbol))

(defun slime-calls-who (symbol)
  "Show all known functions called by the function SYMBOL."
  (interactive (list (slime-read-symbol-name "Who calls: " t)))
  (slime-xref :calls-who symbol))

(defun slime-who-references (symbol)
  "Show all known referrers of the global variable SYMBOL."
  (interactive (list (slime-read-symbol-name "Who references: " t)))
  (slime-xref :references symbol))

(defun slime-who-binds (symbol)
  "Show all known binders of the global variable SYMBOL."
  (interactive (list (slime-read-symbol-name "Who binds: " t)))
  (slime-xref :binds symbol))

(defun slime-who-sets (symbol)
  "Show all known setters of the global variable SYMBOL."
  (interactive (list (slime-read-symbol-name "Who sets: " t)))
  (slime-xref :sets symbol))

(defun slime-who-macroexpands (symbol)
  "Show all known expanders of the macro SYMBOL."
  (interactive (list (slime-read-symbol-name "Who macroexpands: " t)))
  (slime-xref :macroexpands symbol))

(defun slime-who-specializes (symbol)
  "Show all known methods specialized on class SYMBOL."
  (interactive (list (slime-read-symbol-name "Who specializes: " t)))
  (slime-xref :specializes symbol))

(defun slime-list-callers (symbol-name)
  "List the callers of SYMBOL-NAME in a xref window."
  (interactive (list (slime-read-symbol-name "List callers: ")))
  (slime-xref :callers symbol-name))

(defun slime-list-callees (symbol-name)
  "List the callees of SYMBOL-NAME in a xref window."
  (interactive (list (slime-read-symbol-name "List callees: ")))
  (slime-xref :callees symbol-name))

(defun slime-xref (type symbol)
  "Make an XREF request to Lisp."
  (slime-eval-async
   `(swank:xref ',type ',symbol)
   (lexical-let ((type type)
                 (symbol symbol)
                 (package (slime-current-package)))
     (lambda (result)
       (slime-show-xrefs result type symbol package)))))


;;;;; XREF navigation

(defun slime-xref-location-at-point ()
  (save-excursion
    ;; When the end of the last line is at (point-max) we can't find
    ;; the text property there. Going to bol avoids this problem.
    (beginning-of-line 1)
    (or (get-text-property (point) 'slime-location)
        (error "No reference at point."))))

(defun slime-goto-xref ()
  "Goto the cross-referenced location at point."
  (interactive)
  (let ((location (slime-xref-location-at-point)))
    (slime-xref-cleanup)
    (slime-goto-source-location location)
    (switch-to-buffer (current-buffer))))

(defun slime-show-xref ()
  "Display the xref at point in the other window."
  (interactive)
  (let ((location (slime-xref-location-at-point)))
    (slime-show-source-location location)))
      
(defun slime-goto-next-xref ()
  "Goto the next cross-reference location."
  (let ((location (with-current-buffer (slime-xref-buffer)
                    (let ((w (display-buffer (current-buffer) t)))
                      (goto-char (1+ (next-single-char-property-change 
                                      (point) 'slime-location)))
                      (set-window-point w (point)))
                    (cond ((eobp)
                           (message "No more xrefs.")
                           nil)
                          (t 
                           (slime-xref-location-at-point))))))
    (when location
      (slime-goto-source-location location)
      (switch-to-buffer (current-buffer)))))

(defvar slime-next-location-function nil
  "Function to call for going to the next location.")

(defun slime-next-location ()
  "Go to the next location, depending on context.
When displaying XREF information, this goes to the next reference."
  (interactive)
  (when (null slime-next-location-function)
    (error "No context for finding locations."))
  (funcall slime-next-location-function))

(defun slime-xref-quit ()
  "Kill the current xref buffer and restore the window configuration."
  (interactive)
  (let ((config slime-xref-saved-window-configuration))
    (slime-xref-cleanup)
    (set-window-configuration config)))

(defun slime-xref-cleanup ()
  "Delete overlays created by xref mode and kill the xref buffer."
  (sldb-delete-overlays)
  (let ((buffer (current-buffer)))
    (delete-windows-on buffer)
    (kill-buffer buffer)))
  

;;;; Macroexpansion

(defun slime-eval-macroexpand (expander)
  (let ((string (slime-sexp-at-point)))
    (slime-eval-describe `(,expander ,string))))

(defun slime-macroexpand-1 (&optional repeatedly)
  "Display the macro expansion of the form at point.  The form is
expanded with CL:MACROEXPAND-1 or, if a prefix argument is given, with
CL:MACROEXPAND."
  (interactive "P")
  (slime-eval-macroexpand
   (if repeatedly 'swank:swank-macroexpand 'swank:swank-macroexpand-1)))

(defun slime-macroexpand-all ()
  "Display the recursively macro expanded sexp at point."
  (interactive)
  (slime-eval-macroexpand 'swank:swank-macroexpand-all))


;;;; Subprocess control

(defun slime-interrupt ()
  "Interrupt Lisp."
  (interactive)
  (slime-dispatch-event `(:emacs-interrupt ,slime-current-thread)))

(defun slime-quit ()
  (error "Not implemented properly.  Use `slime-interrupt' instead."))

(defun slime-quit-lisp (&optional keep-buffers)
  "Quit lisp, kill the inferior process and associated buffers."
  (interactive)
  (slime-eval-async '(swank:quit-lisp))
  (kill-buffer (slime-output-buffer))
  (set-process-sentinel (slime-connection) 'slime-quit-sentinel))

(defun slime-quit-sentinel (process message)
  (assert (process-status process) 'closed)
  (let* ((inferior (slime-inferior-process process))
         (inferior-buffer (if inferior (process-buffer inferior))))
    (when inferior (delete-process inferior))
    (when inferior-buffer (kill-buffer inferior-buffer))
    (slime-net-close process)
    (slime-set-state "[not connected]" process)
    (message "Connection closed.")))

(defun slime-set-package (package)
  (interactive (list (slime-read-package-name "Package: " 
					      (slime-find-buffer-package))))
  (message "*package*: %s" (slime-eval `(swank:set-package ,package))))

(defun slime-set-default-directory (directory)
  "Make DIRECTION become Lisp's current directory."
  (interactive (list (read-directory-name "Directory: " nil nil t)))
  (message "default-directory: %s"
           (slime-from-lisp-filename
            (slime-eval `(swank:set-default-directory
                          ,(slime-to-lisp-filename directory)))))
  (with-current-buffer (slime-output-buffer)
    (setq default-directory (expand-file-name directory))
    (when (boundp 'header-line-format)
      (slime-repl-update-banner))))

(defun slime-sync-package-and-default-directory ()
  "Set Lisp's package and directory to the values in current buffer."
  (interactive)
  (let ((package (slime-eval `(swank:set-package 
			       ,(slime-find-buffer-package))))
	(directory (slime-from-lisp-filename
                    (slime-eval `(swank:set-default-directory 
                                  ,(slime-to-lisp-filename
                                    default-directory))))))
    (let ((dir default-directory))
      ;; Sync REPL dir
      (with-current-buffer (slime-output-buffer)
        (setq default-directory dir))
      ;; Sync *inferior-lisp* dir
      (let* ((proc (slime-process))
             (buffer (and proc (process-buffer proc))))
        (when buffer 
          (with-current-buffer buffer
            (setq default-directory dir)))))
    (message "package: %s  default-directory: %s" (car package) directory)))
	

;;;; Debugger (SLDB)

(defvar sldb-hook nil
  "Hook run on entry to the debugger.")

(defun slime-add-face (face string)
  (add-text-properties 0 (length string) (list 'face face) string)
  string)
  
(defmacro in-sldb-face (name string)
  "Return STRING propertised with face sldb-NAME-face.
If `sldb-enable-styled-backtrace' is nil, just return STRING."
  (let ((facename (intern (format "sldb-%s-face" (symbol-name name))))
	(var (gensym "string")))
    `(let ((,var ,string))
      (sldb-add-face ',facename ,var)
      ,var)))


;;;;; Local variables in the debugger buffer

(make-variable-buffer-local
 (defvar sldb-condition nil
   "List of (DESCRIPTION TYPE) strings describing the condition being debugged."))

(make-variable-buffer-local
 (defvar sldb-saved-window-configuration nil
   "Window configuration before the debugger was initially entered."))

(make-variable-buffer-local
 (defvar sldb-restarts nil
   "List of (NAME DESCRIPTION) for each available restart."))

(make-variable-buffer-local
 (defvar sldb-level nil
   "Current debug level (recursion depth) displayed in buffer."))

(make-variable-buffer-local
 (defvar sldb-backtrace-start-marker nil
   "Marker placed at the beginning of the backtrace text."))

(make-variable-buffer-local
 (defvar sldb-continuations nil
   "List of ids for pending continuation."))
   

;;;;; sldb-mode

(define-derived-mode sldb-mode fundamental-mode "sldb" 
  "Superior lisp debugger mode. In addition to ordinary SLIME commands,
the following are available:\\<sldb-mode-map>

Commands to examine the selected frame:
   \\[sldb-toggle-details]   - toggle details (local bindings, CATCH tags)
   \\[sldb-show-source]   - view source for the frame
   \\[sldb-eval-in-frame]   - eval in frame
   \\[sldb-pprint-eval-in-frame]   - eval in frame, pretty-print result
   \\[sldb-disassemble]   - disassemble
   \\[sldb-inspect-in-frame]   - inspect
   \\[sldb-list-locals]   - list locals

Commands to invoke restarts:
   \\[sldb-quit]   - quit
   \\[sldb-abort]   - abort
   \\[sldb-continue]   - continue
   \\[sldb-invoke-restart-0]-\\[sldb-invoke-restart-9] - restart shortcuts

Commands to navigate frames:
   \\[sldb-down]   - down
   \\[sldb-up]   - up
   \\[sldb-details-down] - down, with details
   \\[sldb-details-up] - up, with details

Miscellaneous commands:
   \\[sldb-restart-frame]   - restart frame
   \\[sldb-return-from-frame]   - return from frame
   \\[sldb-step]   - step
   \\[sldb-break-with-default-debugger]   - switch to default debugger
   \\[slime-interactive-eval]   - eval

Full list of commands:

\\{sldb-mode-map}"
  (erase-buffer)
  (set-syntax-table lisp-mode-syntax-table)
  (slime-set-truncate-lines)
  ;; Make original slime-connection "sticky" for SLDB commands in this buffer
  (setq slime-buffer-connection (slime-connection))
  (make-local-variable 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'sldb-delete-overlays))

(defun sldb-help-summary ()
  "Show summary of important sldb commands"
  (interactive)
  (message
   (mapconcat
    #'(lambda (list)
        (destructuring-bind (cmd letter name name-with-letter) list
          (let ((where-is (where-is-internal cmd sldb-mode-map)))
            (if (or (member (vector (intern letter)) where-is)
                    (member (vector (string-to-char letter)) where-is))
                name-with-letter
              (substitute-command-keys
               (format "\\<sldb-mode-map>\\[%s] %s" cmd name))))))
    '((sldb-down           "n" "next"           "n-ext")
      (sldb-up             "p" "prev"           "p-rev")
      (sldb-toggle-details "t" "toggle details" "t-oggle details")
      (sldb-eval-in-frame  "e" "eval"           "e-val")
      (sldb-continue       "c" "continue"       "c-ontinue")
      (sldb-abort          "a" "abort"          "a-bort")
      (sldb-show-source    "v" "view source"    "v-iew source")
      (describe-mode       "h" "help"           "h-elp"))
    ", ")))

(slime-define-keys sldb-mode-map 
  ("?"    'sldb-help-summary)
  ("h"    'describe-mode)
  ("v"    'sldb-show-source)
  ((kbd "RET") 'sldb-default-action)
  ("\C-m"      'sldb-default-action)
  ([mouse-2]  'sldb-default-action/mouse)
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
  ("l"    'sldb-list-locals)
  ("t"    'sldb-toggle-details)
  ("r"    'sldb-restart-frame)
  ("R"    'sldb-return-from-frame)
  ("c"    'sldb-continue)
  ("s"    'sldb-step)
  ("b"    'sldb-break-on-return)
  ("a"    'sldb-abort)
  ("q"    'sldb-quit)
  ("B"    'sldb-break-with-default-debugger)
  ("P"    'sldb-print-condition)
  ("C"    'sldb-inspect-condition)
  (":"    'slime-interactive-eval)
  ("\C-c\C-d" slime-doc-map))

;; Inherit bindings from slime-mode
(dolist (spec slime-keys)
  (destructuring-bind (key command &key sldb prefixed &allow-other-keys) spec
    (when sldb
      (let ((key (if prefixed (concat slime-prefix-key key) key)))
        (define-key sldb-mode-map key command)))))

;; Keys 0-9 are shortcuts to invoke particular restarts.
(defmacro define-sldb-invoke-restart-key (number key)
  (let ((fname (intern (format "sldb-invoke-restart-%S" number)))
        (docstring (format "Invoke restart numbered %S." number)))
    `(progn
       (defun ,fname ()
         ,docstring
	 (interactive)
	 (sldb-invoke-restart ,number))
       (define-key sldb-mode-map ,key ',fname))))

(defmacro define-sldb-invoke-restart-keys (from to)
  `(progn
     ,@(loop for n from from to to
	     collect `(define-sldb-invoke-restart-key ,n 
			,(number-to-string n)))))

(define-sldb-invoke-restart-keys 0 9)


;;;;; SLDB buffer creation & update

(defvar sldb-overlays '()
  "List of overlays created in source code buffers to highlight expressions.")

(defun sldb-buffers ()
  (remove-if-not (lambda (buffer) 
                   (with-current-buffer buffer
                     (eq major-mode 'sldb-mode)))
                 (buffer-list)))

(defun sldb-find-buffer (thread &optional connection)
  (let ((connection (or connection (slime-connection))))
    (find-if (lambda (buffer)
               (with-current-buffer buffer
                 (and (eq slime-buffer-connection connection)
                      (eq slime-current-thread thread))))
             (sldb-buffers))))

(defun sldb-get-default-buffer ()
  "Get a sldb buffer.  
The buffer is chosen more or less randomly."
  (car (sldb-buffers)))

(defun sldb-get-buffer (thread &optional connection)
  "Find or create a sldb-buffer for THREAD."
  (let ((connection (or connection (slime-connection))))
    (or (sldb-find-buffer thread connection)
        (let ((name (format "*sldb %s/%s*" (slime-connection-name) thread)))
          (with-current-buffer (generate-new-buffer name)
            (setq slime-buffer-connection connection 
                  slime-current-thread thread)
            (current-buffer))))))

(defun sldb-debugged-continuations (connection)
  "Return the debugged continuations for CONNECTION."
  (lexical-let ((accu '()))
    (dolist (b (sldb-buffers))
      (with-current-buffer b
        (when (eq slime-buffer-connection connection)
          (setq accu (append sldb-continuations accu)))))
    accu))

(defun sldb-setup (thread level condition restarts frames conts)
  "Setup a new SLDB buffer.
CONDITION is a string describing the condition to debug.
RESTARTS is a list of strings (NAME DESCRIPTION) for each available restart.
FRAMES is a list (NUMBER DESCRIPTION) describing the initial
portion of the backtrace. Frames are numbered from 0.
CONTS is a list of pending Emacs continuations."
  (with-current-buffer (sldb-get-buffer thread)
    (unless (equal sldb-level level)
      (setq buffer-read-only nil)
      (sldb-mode)
      (unless sldb-saved-window-configuration
        (setq sldb-saved-window-configuration (current-window-configuration)))
      (setq slime-current-thread thread)
      (setq sldb-level level)
      (setq mode-name (format "sldb[%d]" sldb-level))
      (setq sldb-condition condition)
      (setq sldb-restarts restarts)
      (setq sldb-continuations conts)
      (sldb-insert-condition condition)
      (insert (in-sldb-face section "Restarts:") "\n")
      (sldb-insert-restarts restarts)
      (insert "\n" (in-sldb-face section "Backtrace:") "\n")
      (setq sldb-backtrace-start-marker (point-marker))
      (sldb-insert-frames (sldb-prune-initial-frames frames) nil)
      (run-hooks 'sldb-hook)
      (pop-to-buffer (current-buffer))
      (setq buffer-read-only t)
      (when (and slime-stack-eval-tags
                 ;; (y-or-n-p "Enter recursive edit? ")
                 )
        (message "Entering recursive edit..")
        (recursive-edit)))))

(defun sldb-activate (thread level)
  (with-current-buffer (sldb-find-buffer thread)
    (unless (equal sldb-level level)
      (with-lexical-bindings (thread level)
        (slime-eval-async `(swank:debugger-info-for-emacs 0 1)
                          (lambda (result)
                            (apply #'sldb-setup thread level result)))))))

(defun sldb-exit (thread level &optional stepping)
  (when-let (sldb (sldb-find-buffer thread))
    (with-current-buffer sldb
      (unless stepping
        (set-window-configuration sldb-saved-window-configuration))
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq sldb-level nil))
    (when (and (= level 1) (not stepping))
      (kill-buffer sldb))))

(defun sldb-insert-condition (condition)
  (destructuring-bind (message type references extras) condition
    (slime-insert-propertized '(sldb-default-action sldb-inspect-condition)
                              (in-sldb-face topline message)
                              "\n" 
                              (in-sldb-face condition type)
                              "\n\n")
    (when references
      (insert "See also:\n")
      (slime-with-rigid-indentation 2
        (sldb-insert-references references))
      (insert "\n"))
    (sldb-dispatch-extras extras)))

(defun sldb-insert-references (references)
  "Insert documentation references from a condition.
See SWANK-BACKEND:CONDITION-REFERENCES for the datatype."
  (loop for ref in references do
        (destructuring-bind (where type what) ref
          (insert (sldb-format-reference-source where) ", ")
          (slime-insert-propertized (sldb-reference-properties ref)
                                    (sldb-format-reference-node what))
          (insert (format " [%s]" (slime-cl-symbol-name type)) "\n"))))

(defun sldb-reference-properties (reference)
  "Return the properties for a reference.
Only add clickability to properties we actually know how to lookup."
  (destructuring-bind (where type what) reference
    (if (or (and (eq where :sbcl) (eq type :node))
            (and (eq where :ansi-cl)
                 (symbolp type)
                 (member (slime-cl-symbol-name type)
                         '("function" "special-operator" "macro" 
                           "section" "glossary" "issue"))))
        `(sldb-default-action 
          sldb-lookup-reference
          ;; FIXME: this is a hack!  slime-compiler-notes and sldb are a
          ;; little too intimately entwined.
          slime-compiler-notes-default-action sldb-lookup-reference
          sldb-reference ,reference
          face sldb-reference-face
          mouse-face highlight))))

(defun sldb-format-reference-source (where)
  (case where
    (:amop    "The Art of the Metaobject Protocol")
    (:ansi-cl "Common Lisp Hyperspec")
    (:sbcl    "SBCL Manual")
    (t        (format "%S" where))))

(defun sldb-format-reference-node (what)
  (if (symbolp what)
      (upcase (slime-cl-symbol-name what))
    (if (listp what)
        (mapconcat (lambda (x) (format "%S" x)) what ".")
      what)))

(defun sldb-lookup-reference ()
  "Browse the documentation reference at point."
  (destructuring-bind (where type what)
      (get-text-property (point) 'sldb-reference)
    (case where
      (:ansi-cl
       (case type
         (:section
          (browse-url (funcall common-lisp-hyperspec-section-fun what)))
         (:glossary
          (browse-url (funcall common-lisp-glossary-fun what)))
         (:issue
          (browse-url (funcall 'common-lisp-issuex what)))
         (t
          (hyperspec-lookup (if (symbolp what)
                                (slime-cl-symbol-name what)
                              what)))))
      (t
       (let ((url (format "%s%s.html" slime-sbcl-manual-root 
                          (subst-char-in-string ?\  ?\- what))))
         (browse-url url))))))

(defun sldb-dispatch-extras (extras)
  (dolist (extra extras)
    (destructure-case extra
      ((:short-frame-source n)
       (sldb-show-frame-source n)))))
  
(defun sldb-insert-restarts (restarts)
  (loop for (name string) in restarts
        for number from 0 
        do (progn (slime-insert-propertized
                   `(restart-number ,number
                                    sldb-default-action sldb-invoke-restart
                                    mouse-face highlight)
                   "  "
                   (in-sldb-face restart-number (number-to-string number))
                   ": ["  (in-sldb-face restart-type name) "] " 
                   (in-sldb-face restart string))
                  (insert "\n"))))
  
(defun sldb-add-face (face string)
  (if sldb-enable-styled-backtrace
      (add-text-properties 0 (length string) (list 'face face) string)
      string))

(defun sldb-prune-initial-frames (frames)
  "Return the prefix of FRAMES to initially present to the user.
Regexp heuristics are used to avoid showing SWANK-internal frames."
  (or (loop for frame in frames
            for (number string) = frame
            until (string-match "^(*\\(SWANK\\|swank\\)\\>" string)
            collect frame)
      frames))

(defun sldb-insert-frame (frame &optional detailedp)
  (destructuring-bind (number string) frame
    (slime-insert-propertized
     `(frame ,frame sldb-default-action sldb-toggle-details)
     " " (in-sldb-face frame-label (format "%2d" number)) ": "
     (if detailedp
         (in-sldb-face detailed-frame-line string)
       (in-sldb-face frame-line string))
     "\n")))

(defun sldb-insert-frames (frames maximum-length)
  "Insert FRAMES into buffer.
MAXIMUM-LENGTH is the total number of frames in the Lisp stack."
  (unless (null frames)
    (when maximum-length
      (assert (<= (length frames) maximum-length)))
    (save-excursion
      (mapc #'sldb-insert-frame frames)
      (let ((number (sldb-previous-frame-number)))
        (cond ((and maximum-length (< (length frames) maximum-length)))
              (t
               (slime-insert-propertized 
                `(sldb-default-action 
                  sldb-fetch-more-frames
                  point-entered sldb-fetch-more-frames
                  start-open t
                  sldb-previous-frame-number ,number)
                (in-sldb-face section " --more--\n"))))))))

(defun sldb-fetch-more-frames (&rest ignore)
  "Fetch more backtrace frames.
Called on the `point-entered' text-property hook."
  (let ((inhibit-point-motion-hooks t)
        (inhibit-read-only t))
      (when-let (previous (get-text-property (point) 
                                             'sldb-previous-frame-number))
        (beginning-of-line)
        (let ((start (point)))
          (goto-char (point-max))
          (delete-region start (point)))
        (let ((start (1+ previous))
              (end (+ previous 40)))
          (sldb-insert-frames (slime-eval `(swank:backtrace ,start ,end))
                              (- end start))))))


;;;;; SLDB commands

(defvar sldb-highlight t
  "When non-nil use temporary face attributes to mark buffer expressions.")

(defvar sldb-show-location-recenter-arg nil
  "Argument to pass to `recenter' when displaying a source location.")

(defun sldb-default-action/mouse (event)
  "Invoke the action pointed at by the mouse."
  (interactive "e")
  (destructuring-bind (mouse-1 (w pos &rest _)) event
    (save-excursion
      (goto-char pos)
      (let ((fn (get-text-property (point) 'sldb-default-action)))
	(if fn (funcall fn))))))

(defun sldb-default-action ()
  "Invoke the action at point."
  (interactive)
  (let ((fn (get-text-property (point) 'sldb-default-action)))
    (if fn (funcall fn))))

(defun sldb-delete-overlays ()
  (mapc #'delete-overlay sldb-overlays)
  (setq sldb-overlays '()))

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

(defun sldb-show-source ()
  "Highlight the frame at point's expression in a source code buffer."
  (interactive)
  (sldb-show-frame-source (sldb-frame-number-at-point)))

(defun sldb-show-frame-source (frame-number)
  (sldb-delete-overlays)
  (slime-eval-async
   `(swank:frame-source-location-for-emacs ,frame-number)
   (lambda (source-location)
     (destructure-case source-location
       ((:error message)
        (message "%s" message)
        (ding))
       (t
        (slime-show-source-location source-location))))))

(defun slime-show-source-location (source-location)
  (slime-goto-source-location source-location)
  (when sldb-highlight (sldb-highlight-sexp))
  (slime-show-buffer-position (point)))

(defun slime-show-buffer-position (position)
  "Ensure sure that the POSITION in the current buffer is visible."
  (save-selected-window
    (let ((w (select-window (or (get-buffer-window (current-buffer) t)
                                (display-buffer (current-buffer) t)))))
      (goto-char position)
      (push-mark)
      (unless (pos-visible-in-window-p)
        (slime-recenter-window w sldb-show-location-recenter-arg)))))

(defun slime-recenter-window (window line)
  "Set window-start in WINDOW LINE lines before point."
  (let* ((line (if (not line)
                   (/ (window-height window) 2)
                 line))
         (start (save-excursion
                  (loop repeat line do (forward-line -1))
                  (point))))
    (set-window-start window start)))

(defun sldb-highlight-sexp (&optional start end)
  "Highlight the first sexp after point."
  (sldb-delete-overlays)
  (let ((start (or start (point)))
	(end (or end (save-excursion (ignore-errors (forward-sexp)) (point)))))
    (push (make-overlay start (1+ start)) sldb-overlays)
    (push (make-overlay (1- end) end) sldb-overlays)
    (dolist (overlay sldb-overlays)
      (overlay-put overlay 'face 'secondary-selection))))


(defun sldb-toggle-details (&optional on)
  "Toggle display of details for the current frame.
The details include local variable bindings and CATCH-tags."
  (interactive)
  (sldb-frame-number-at-point)
  (let ((inhibit-read-only t)
        (column (current-column)))
    (if (or on (not (sldb-frame-details-visible-p)))
	(sldb-show-frame-details)
      (sldb-hide-frame-details))
    (move-to-column column)))

(defun sldb-frame-details-visible-p ()
  (and (get-text-property (point) 'frame)
       (get-text-property (point) 'details-visible-p)))

(defun sldb-show-frame-details ()
  (multiple-value-bind (start end) (sldb-frame-region)
    (save-excursion
      (goto-char start)
      (let* ((props (text-properties-at (point)))
	     (frame (plist-get props 'frame))
	     (frame-number (car frame))
	     (standard-output (current-buffer))
             (indent1 "      ")
             (indent2 "        "))
	(delete-region start end)
	(slime-propertize-region `(frame ,frame details-visible-p t)
          (sldb-insert-frame frame t)
          (insert indent1 (in-sldb-face section "Locals:") "\n")
          (sldb-insert-locals frame-number indent2)
	  (when sldb-show-catch-tags
	    (let ((catchers (sldb-catch-tags frame-number)))
	      (cond ((null catchers)
		     (insert indent1
                             (in-sldb-face catch-tag "[No catch-tags]\n")))
		    (t
		     (insert indent1 "Catch-tags:\n")
                     (dolist (tag catchers)
                        (slime-insert-propertized  
                         '(catch-tag ,tag)
                         indent2 (in-sldb-face catch-tag 
                                               (format "%s\n" tag))))))))

	  (unless sldb-enable-styled-backtrace (terpri))
	  (point)))))
  (apply #'sldb-maybe-recenter-region (sldb-frame-region)))

(defun sldb-frame-region ()
  (save-excursion
    (goto-char (next-single-property-change (point) 'frame nil (point-max)))
    (backward-char)
    (values (previous-single-property-change (point) 'frame)
	    (next-single-property-change (point) 'frame nil (point-max)))))

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
      (goto-char start)
      (let* ((props (text-properties-at (point)))
	     (frame (plist-get props 'frame)))
	(delete-region start end)
	(slime-propertize-region (plist-put props 'details-visible-p nil)
          (sldb-insert-frame frame))))))


(defun sldb-eval-in-frame (string)
  "Prompt for an expression and evaluate it in the selected frame."
  (interactive (list (slime-read-from-minibuffer "Eval in frame: ")))
  (let* ((number (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:eval-string-in-frame ,string ,number)
                      (if current-prefix-arg
                          'slime-output-string
                        'slime-display-eval-result))))

(defun sldb-pprint-eval-in-frame (string)
  "Prompt for an expression, evaluate in selected frame, pretty-print result."
  (interactive (list (slime-read-from-minibuffer "Eval in frame: ")))
  (let* ((number (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:pprint-eval-string-in-frame ,string ,number)
		      (lambda (result)
			(slime-show-description result nil)))))

(defun sldb-inspect-in-frame (string)
  "Prompt for an expression and inspect it in the selected frame."
  (interactive (list (slime-read-from-minibuffer 
                      "Inspect in frame (evaluated): " 
                      (slime-sexp-at-point))))
  (let ((number (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:inspect-in-frame ,string ,number)
                      'slime-open-inspector)))

(defun sldb-inspect-condition ()
  "Inspect the current debugger condition."
  (interactive)
  (slime-eval-async '(swank:inspect-current-condition)
                    'slime-open-inspector))

(defun sldb-forward-frame ()
  (goto-char (next-single-char-property-change (point) 'frame)))

(defun sldb-backward-frame ()
  (goto-char (previous-single-char-property-change
	      (point) 'frame 
	      nil sldb-backtrace-start-marker)))

(defun sldb-goto-last-frame ()
  (goto-char (point-max))
  (while (not (get-text-property (point) 'frame))
    (goto-char (previous-single-property-change (point) 'frame))))

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

(defun sldb-frame-locals (frame)
  (slime-eval `(swank:frame-locals-for-emacs ,frame)))

(defun sldb-insert-locals (frame prefix)
  (loop for i from 0 
        for var in (sldb-frame-locals frame) do
        (destructuring-bind (&key name id value) var
          (slime-propertize-region (list 'sldb-default-action 'sldb-inspect-var
                                         'var i)
            (insert prefix (in-sldb-face local-name name))
            (unless (zerop id) 
              (insert (in-sldb-face local-name (format "#%d" id))))
            (insert " = " (in-sldb-face local-value value)))
          (insert "\n"))))

(defun sldb-inspect-var ()
  (let ((frame (sldb-frame-number-at-point))
        (var (sldb-var-number-at-point)))
    (slime-eval-async `(swank:inspect-frame-var ,frame ,var) 
                      'slime-open-inspector)))

(defun sldb-list-locals ()
  "List local variables in selected frame."
  (interactive)
  (let ((frame (sldb-frame-number-at-point))
        (thread slime-current-thread))
    (slime-message "%s" (with-temp-buffer
                          (let ((slime-current-thread thread))
                            (sldb-insert-locals frame "")
                            (buffer-string))))))

(defun sldb-catch-tags (frame)
  (slime-eval `(swank:frame-catch-tags-for-emacs ,frame)))

(defun sldb-list-catch-tags ()
  (interactive)
  (slime-message "%s" (sldb-catch-tags (sldb-frame-number-at-point))))

(defun sldb-fetch-all-frames ()
  (interactive)
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (sldb-goto-last-frame)
    (let ((last (sldb-frame-number-at-point)))
      (goto-char (next-single-char-property-change (point) 'frame))
      (delete-region (point) (point-max))
      (sldb-insert-frames (slime-eval `(swank:backtrace ,(1+ last) nil))
                          nil))))

(defun sldb-end-of-backtrace ()
  "Fetch the entire backtrace and move point to the last frame."
  (interactive)
  (sldb-fetch-all-frames)
  (sldb-goto-last-frame))

(defun sldb-beginning-of-backtrace ()
  "Goto the first frame."
  (interactive)
  (goto-char sldb-backtrace-start-marker))


(defun sldb-quit ()
  "Quit to toplevel."
  (interactive)
  (slime-eval-async '(swank:throw-to-toplevel) 
                    (lambda (_) (error "sldb-quit returned"))))

(defun sldb-continue ()
  "Invoke the \"continue\" restart."
  (interactive)
  (slime-rex ()
      ('(swank:sldb-continue))
    ((:ok _) 
     (message "No restart named continue")
     (ding))
    ((:abort) )))

(defun sldb-abort ()
  "Invoke the \"abort\" restart."
  (interactive)
  (slime-eval-async '(swank:sldb-abort)
                    (lambda (v) (message "Restart returned: %S" v))))

(defun sldb-invoke-restart (&optional number)
  "Invoke a restart.
Optional NUMBER specifies the restart to invoke, otherwise 
use the restart at point."
  (interactive)
  (let ((restart (or number (sldb-restart-at-point))))
    (slime-rex ()
        ((list 'swank:invoke-nth-restart-for-emacs sldb-level restart))
      ((:ok value) (message "Restart returned: %s" value))
      ((:abort)))))

(defun sldb-restart-at-point ()
  (or (get-text-property (point) 'restart-number)
      (error "No restart at point")))

(defun sldb-break-with-default-debugger ()
  "Enter default debugger."
  (interactive)
  (slime-rex ()
      ('(swank:sldb-break-with-default-debugger) nil slime-current-thread)
    ((:abort))))

(defun sldb-step ()
  "Select the \"continue\" restart and set a new break point."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:sldb-step ,frame))))

(defun sldb-break-on-return ()
  "Set a breakpoint at the current frame. 
The debugger is entered when the frame exits."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:sldb-break-on-return ,frame)
                      (lambda (msg) (message "%s" msg)))))

(defun sldb-break (name)
  "Set a breakpoint at the start of the function NAME."
  (interactive (list (slime-read-symbol-name "Function: " t)))
  (slime-eval-async `(swank:sldb-break ,name) 
                    (lambda (msg) (message "%s" msg))))

(defun sldb-disassemble ()
  "Disassemble the code for the current frame."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:sldb-disassemble ,frame)
                      (lambda (result)
			(slime-show-description result nil)))))

(defun sldb-return-from-frame (string)
  "Reads an expression in the minibuffer and causes the function to
return that value, evaluated in the context of the frame."
  (interactive (list (slime-read-from-minibuffer "Return from frame: ")))
  (let* ((number (sldb-frame-number-at-point)))
    (slime-rex ()
        ((list 'swank:sldb-return-from-frame number string))
      ((:ok value) (message "%s" value))
      ((:abort)))))

(defun sldb-restart-frame ()
  "Causes the frame to restart execution with the same arguments as it
was called originally."
  (interactive)
  (let* ((number (sldb-frame-number-at-point)))
    (slime-rex ()
        ((list 'swank:restart-frame number))
      ((:ok value) (message "%s" value))
      ((:abort)))))

(defun sldb-print-condition ()
  "Print the condition SLDB is handling in the REPL.
This way you can still see what the error was after exiting SLDB."
  (interactive)
  (when (null sldb-condition)
    (error "No condition known (wrong buffer?)"))
  (slime-output-string (format "%s\n%s\n"
                               (first sldb-condition)
                               (second sldb-condition))))


;;;; Thread control panel

(defun slime-list-threads ()
  "Display a list of threads."
  (interactive)
  (slime-eval-async 
   '(swank:list-threads)
   (lambda (threads)
      (with-current-buffer (get-buffer-create "*slime-threads*")
       (slime-thread-control-mode)
       (let ((inhibit-read-only t))
         (erase-buffer)
         (loop for idx from 0 
               for (name status id) in threads
               do (slime-thread-insert idx name status id))
         (goto-char (point-min))
         (setq buffer-read-only t)
         (pop-to-buffer (current-buffer)))))))

(defun slime-thread-insert (idx name summary id)
  (slime-propertize-region `(thread-id ,idx)
    (insert (format "%3s: " id))
    (slime-insert-propertized '(face bold) name)
    (insert-char ?\040 (- 30 (current-column)))
    (let ((summary-start (point)))
      (insert " " summary)
      (unless (bolp) (insert "\n"))
      (indent-rigidly summary-start (point) 2))))


;;;;; Major mode

(define-derived-mode slime-thread-control-mode fundamental-mode
  "thread-control"
  "SLIME Thread Control Panel Mode.

\\{slime-thread-control-mode-map}"
  (when slime-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

(slime-define-keys slime-thread-control-mode-map
  ("a"         'slime-thread-attach)
  ("d"         'slime-thread-debug)
  ("g"         'slime-list-threads)
  ("k"         'slime-thread-kill)
  ("q"         'slime-thread-quit))

(defun slime-thread-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun slime-thread-kill ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-id)))
    (slime-eval `(swank:kill-nth-thread ,id)))
  (call-interactively 'slime-list-threads))

(defun slime-thread-attach ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-id))
        (file (slime-swank-port-file)))
    (slime-eval-async `(swank:start-swank-server-in-thread ,id ,file)))
  (slime-read-port-and-connect nil nil))

(defun slime-thread-debug ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-id)))
    (slime-eval-async `(swank:debug-nth-thread ,id))))


;;;;; Connection listing

(defvar slime-registered-lisp-implementations ())

(defun slime-register-lisp-implementation (name command)
  (interactive "sName: \nfCommand: ")
  (let ((cons (assoc name slime-registered-lisp-implementations)))
    (if cons
      (setf (cdr cons) command)
      (push (cons name command) slime-registered-lisp-implementations)))
  (if (string= inferior-lisp-program "lisp")
    (slime-select-lisp-implementation name)))

(defun slime-select-lisp-implementation (name)
  (interactive "sName: ")
  (setq inferior-lisp-program
        (cdr (assoc name slime-registered-lisp-implementations))))

(defun slime-find-lisp-implementation (name)
  (let ((cons (or (assoc name slime-registered-lisp-implementations)
                  (rassoc name slime-registered-lisp-implementations))))
    (if cons (cdr cons) name)))

;; XXX: unused function
(defun slime-find-lisp-implementation-name (command)
  (cdr (rassoc command slime-registered-lisp-implementations)))

(defun slime-symbolic-lisp-name-p (name)
  (let ((cons (or (assoc name slime-registered-lisp-implementations)
                  (rassoc name slime-registered-lisp-implementations))))
    (if cons (car cons))))


(define-derived-mode slime-connection-list-mode fundamental-mode
  "connection-list"
  "SLIME Connection List Mode.

\\{slime-connection-list-mode-map}"
  (when slime-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

(slime-define-keys slime-connection-list-mode-map
  ((kbd "RET") 'slime-goto-connection)
  ("d"         'slime-connection-list-make-default)
  ("g"         'slime-update-connection-list)
  ((kbd "C-k") 'slime-quit-connection-at-point))

(defun slime-connection-at-point ()
  (or (get-text-property (point) 'slime-connection)
      (error "No connection at point")))

(defun slime-goto-connection ()
  "Switch to the REPL buffer for the connection at point."
  (interactive)
  (let ((slime-dispatching-connection (slime-connection-at-point)))
    (switch-to-buffer (slime-output-buffer))))

(defun slime-quit-connection-at-point (connection)
  (interactive (list (slime-connection-at-point)))
  (let ((slime-dispatching-connection connection))
    (slime-quit-lisp)
    (while (memq connection slime-net-processes)
      (sit-for 0 100)))
  (slime-update-connection-list))
  
(defun slime-connection-list-make-default ()
  "Make the connection at point the default connection."
  (interactive)
  (slime-select-connection (slime-connection-at-point))
  (slime-update-connection-list))

(defun slime-list-connections ()
  "Display a list of all connections."
  (interactive)
  (when (get-buffer "*SLIME connections*")
    (kill-buffer "*SLIME connections*"))
  (with-current-buffer
      (slime-get-temp-buffer-create "*SLIME connections*"
                                    :mode 'slime-connection-list-mode)
    (slime-draw-connection-list)
    (setq buffer-read-only t)
    (pop-to-buffer (current-buffer))))

(defun slime-update-connection-list ()
 "Display a list of all connections."
 (interactive)
 (let ((pos (point))
       (inhibit-read-only t))
   (erase-buffer)
   (slime-draw-connection-list)
   (goto-char pos)))

(defun slime-draw-connection-list ()
  (let ((default-pos nil)
        (default slime-default-connection)
        (fstring "%s%2s  %-10s  %-17s  %-7s %-s\n"))
    (insert (format fstring " " "Nr" "Name" "Port" "Pid" "Type")
            (format fstring " " "--" "----" "----" "---" "----"))
    (dolist (p (reverse slime-net-processes))
      (when (eq default p) (setf default-pos (point)))
      (slime-insert-propertized 
       (list 'slime-connection p)
       (format fstring
               (if (eq default p) "*" " ")
               (slime-connection-number p)
               (slime-connection-name p)
               (or (process-id p) (process-contact p))
               (slime-pid p)
               (slime-lisp-implementation-type p))))
    (when default 
      (goto-char default-pos))))


;;;; Inspector

(defgroup slime-inspector nil
  "Inspector faces."
  :prefix "slime-inspector-"
  :group 'slime)

(defface slime-inspector-topline-face
  '((t ()))
  "Face for top line describing object."
  :group 'slime-inspector)

(defface slime-inspector-label-face
  '((t (:inherit font-lock-constant-face)))
  "Face for labels in the inspector."
  :group 'slime-inspector)

(defface slime-inspector-value-face
  (if (slime-face-inheritance-possible-p)
      '((t (:inherit font-lock-builtin-face)))
    '((((background light)) (:foreground "MediumBlue" :bold t))
      (((background dark)) (:foreground "LightGray" :bold t))))
  "Face for things which can themselves be inspected."
  :group 'slime-inspector)

(defface slime-inspector-action-face
  (if (slime-face-inheritance-possible-p)
      '((t (:inherit font-lock-warning-face)))
    '((t (:foreground "OrangeRed"))))
  "Face for labels of inspector actions."
  :group 'slime-inspector)

(defface slime-inspector-type-face
    '((t (:inherit font-lock-type-face)))
  "Face for type description in inspector."
  :group 'slime-inspector)

(defvar slime-inspector-mark-stack '())
(defvar slime-saved-window-config)

(defun slime-inspect (string)
  "Eval an expression and inspect the result."
  (interactive 
   (list (slime-read-from-minibuffer "Inspect value (evaluated): "
				     (slime-sexp-at-point))))
  (slime-eval-async `(swank:init-inspector ,string) 'slime-open-inspector))

(define-derived-mode slime-inspector-mode fundamental-mode "Slime-Inspector"
  (set-syntax-table lisp-mode-syntax-table)
  (slime-set-truncate-lines)
  (setq buffer-read-only t))

(defun slime-inspector-buffer ()
  (or (get-buffer "*Slime Inspector*")
      (with-current-buffer (get-buffer-create "*Slime Inspector*")
	(setq slime-inspector-mark-stack '())
        (slime-mode t)
	(slime-inspector-mode)
        (make-local-variable 'slime-saved-window-config)
        (setq slime-saved-window-config (current-window-configuration))
	(current-buffer))))

(defmacro slime-inspector-fontify (face string)
  `(slime-add-face ',(intern (format "slime-inspector-%s-face" face)) ,string))

(defun slime-open-inspector (inspected-parts &optional point)
  "Display INSPECTED-PARTS in a new inspector window.
Optionally set point to POINT."
  (with-current-buffer (slime-inspector-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (destructuring-bind (&key title type content) inspected-parts
        (macrolet ((fontify (face string) 
                            `(slime-inspector-fontify ,face ,string)))
          (insert (fontify topline title))
          (while (eq (char-before) ?\n)
            (backward-delete-char 1))
          (insert "\n [" (fontify label "type:") " " (fontify type type) "]\n"
                  (fontify label "--------------------") "\n")
          (save-excursion 
            (mapc #'slime-inspector-insert-ispec content))
          (pop-to-buffer (current-buffer))
          (when point 
            (goto-char (min (point-max) point))))))))

(defun slime-inspector-insert-ispec (ispec)
  (if (stringp ispec)
      (insert ispec)
    (destructure-case ispec
      ((:value string id)
       (slime-insert-propertized (list 'slime-part-number id 
                                       'mouse-face 'highlight
                                       'face 'slime-inspector-value-face)
                                 string))
      ((:action string id)
       (slime-insert-propertized (list 'slime-action-number id
                                       'mouse-face 'highlight
                                       'face 'slime-inspector-action-face)
                                 string)))))

(defun slime-inspector-operate-on-point ()
  "If point is on a value then recursivly call the inspcetor on
  that value. If point is on an action then call that action."
  (interactive)
  (let ((part-number (get-text-property (point) 'slime-part-number))
        (action-number (get-text-property (point) 'slime-action-number)))
    (cond (part-number
           (slime-eval-async `(swank:inspect-nth-part ,part-number)
                             'slime-open-inspector)
           (push (point) slime-inspector-mark-stack))
          (action-number 
           (slime-eval-async `(swank::inspector-call-nth-action ,action-number)
                             (lexical-let ((point (point)))
                               (lambda (parts)
                                 (slime-open-inspector parts point))))))))

(defun slime-inspector-operate-on-click (event)
  "Inspect the value at the clicked-at position or invoke an action."
  (interactive "@e")
  (let ((point (posn-point (event-end event))))
    (cond ((and point
                (or (get-text-property point 'slime-part-number)
                    (get-text-property point 'slime-action-number)))
           (goto-char point)
           (slime-inspector-operate-on-point))
          (t
           (error "No clickable part here")))))

(defun slime-inspector-copy-down (number)
  "Evaluate the slot at point via the REPL (to set `*')."
  (interactive (list (or (get-text-property (point) 'slime-part-number)
                         (error "No part at point"))))
  (slime-repl-send-string (format "%s" `(swank:inspector-nth-part ,number)))
  (slime-repl))

(defun slime-inspector-pop ()
  (interactive)
  (slime-eval-async 
   `(swank:inspector-pop)
   (lambda (result)
     (cond (result
	    (slime-open-inspector result (pop slime-inspector-mark-stack)))
	   (t 
	    (message "No previous object")
	    (ding))))))

(defun slime-inspector-next ()
  (interactive)
  (let ((result (slime-eval `(swank:inspector-next))))
    (cond (result 
	   (push (point) slime-inspector-mark-stack)
	   (slime-open-inspector result))
	  (t (message "No next object")
	     (ding)))))
  
(defun slime-inspector-quit ()
  (interactive)
  (slime-eval-async `(swank:quit-inspector))
  (set-window-configuration slime-saved-window-config)
  (kill-buffer (current-buffer)))

(defun slime-inspector-next-inspectable-object (arg)
  "Move point to the next inspectable object.
With optional ARG, move across that many objects.
If ARG is negative, move backwards."
  (interactive "p")
  (or (bobp) (> arg 0) (backward-char))
  (let ((wrapped 0)
	(number arg)
	(old (get-text-property (point) 'slime-part-number))
	new)
    ;; Forward.
    (while (> arg 0)
      (cond ((eobp)
	     (goto-char (point-min))
	     (setq wrapped (1+ wrapped)))
	    (t
	     (goto-char (or (next-single-property-change (point) 
                                                         'slime-part-number)
                            (point-max)))))
      (and (= wrapped 2)
	   (eq arg number)
	   (error "No inspectable objects"))
      (let ((new (get-text-property (point) 'slime-part-number)))
	(when new
	  (unless (eq new old)
	    (setq arg (1- arg))
	    (setq old new)))))
    ;; Backward.
    (while (< arg 0)
      (cond ((bobp)
	     (goto-char (point-max))
	     (setq wrapped (1+ wrapped)))
	    (t
	     (goto-char (or (previous-single-property-change 
                             (point) 'slime-part-number)
                            (point-min)))))
      (and (= wrapped 2)
	   (eq arg number)
	   (error "No inspectable objects"))
      (let ((new (get-text-property (point) 'slime-part-number)))
	(when new
	  (unless (eq new old)
	    (setq arg (1+ arg))))))
    (let ((new (get-text-property (point) 'slime-part-number)))
      (while (eq (get-text-property (point) 'slime-part-number) new)
	(backward-char)))
    (forward-char)))

(defun slime-inspector-previous-inspectable-object (arg)
  "Move point to the previous inspectable object.
With optional ARG, move across that many objects.
If ARG is negative, move forwards."
  (interactive "p")
  (slime-inspector-next-inspectable-object (- arg)))
  
(defun slime-inspector-describe ()
  (interactive)
  (slime-eval-describe `(swank:describe-inspectee)))

(defun slime-inspector-reinspect ()
  (interactive)
  (slime-eval-async `(swank::inspect-object swank::*inspectee*) 'slime-open-inspector))

(slime-define-keys slime-inspector-mode-map
  ([return] 'slime-inspector-operate-on-point)
  ([(meta return)] 'slime-inspector-copy-down)
  ("\C-m"   'slime-inspector-operate-on-point)
  ([mouse-2] 'slime-inspector-operate-on-click)
  ("l" 'slime-inspector-pop)
  ("n" 'slime-inspector-next)
  (" " 'slime-inspector-next)
  ("d" 'slime-inspector-describe)
  ("q" 'slime-inspector-quit)
  ("g" 'slime-inspector-reinspect)
  ("\C-i" 'slime-inspector-next-inspectable-object)
  ([(shift tab)] 'slime-inspector-previous-inspectable-object)
  ("\M-." 'slime-edit-definition))


;;;; classes browser

(defun slime-expand-class-node (widget)
  (or (widget-get widget :args)
      (let ((name (widget-get widget :tag)))
	(loop for kid in (slime-eval `(swank:mop :subclasses ,name))
	      collect `(tree-widget :tag ,kid
				    :dynargs slime-expand-class-node
				    :has-children t)))))

(defun slime-browse-classes (name)
  "Read the name of a class and show its subclasses."
  (interactive (list (slime-read-symbol-name "Class Name: ")))
  (slime-call-with-browser-setup 
   "*slime class browser*" (slime-current-package) "Class Browser"
   (lambda ()
     (widget-create 'tree-widget :tag name 
                    :dynargs 'slime-expand-class-node 
                    :has-echildren t))))

(defvar slime-browser-map nil
  "Keymap for tree widget browsers")

(require 'tree-widget)
(unless slime-browser-map
  (setq slime-browser-map (make-sparse-keymap))
  (set-keymap-parent slime-browser-map widget-keymap)
  (define-key slime-browser-map "q" 'bury-buffer))

(defun slime-call-with-browser-setup (buffer package title fn)
  (switch-to-buffer buffer)
  (kill-all-local-variables)
  (setq slime-buffer-package package)
  (let ((inhibit-read-only t)) (erase-buffer))
  (widget-insert title "\n\n")
  (save-excursion
    (funcall fn))
  (lisp-mode-variables t)
  (slime-mode t)
  (use-local-map slime-browser-map)
  (widget-setup))


;;;; Xref browser

(defun slime-fetch-browsable-xrefs (type name)
  "Return a list ((LABEL DSPEC)).
LABEL is just a string for display purposes. 
DSPEC can be used to expand the node."
  (let ((xrefs '()))
    (loop for (_file . specs) in (slime-eval `(swank:xref ,type ,name)) do
          (loop for (dspec . _location) in specs do
                (let ((exp (ignore-errors (read (downcase dspec)))))
                  (cond ((and (consp exp) (eq 'flet (car exp)))
                         ;; we can't expand FLET references so they're useless
                         )
                        ((and (consp exp) (eq 'method (car exp)))
                         ;; this isn't quite right, but good enough for now
                         (push (list dspec (string (second exp))) xrefs))
                        (t
                         (push (list dspec dspec) xrefs))))))
    xrefs))

(defun slime-expand-xrefs (widget)
  (or (widget-get widget :args)
      (let* ((type (widget-get widget :xref-type))
             (dspec (widget-get widget :xref-dspec))
             (xrefs (slime-fetch-browsable-xrefs type dspec)))
        (loop for (label dspec) in xrefs
              collect `(tree-widget :tag ,label
                                    :xref-type ,type
                                    :xref-dspec ,dspec
                                    :dynargs slime-expand-xrefs
                                    :has-children t)))))

(defun slime-browse-xrefs (name type)
  "Show the xref graph of a function in a tree widget."
  (interactive 
   (list (slime-read-from-minibuffer "Name: "
                                     (slime-symbol-name-at-point))
         (read (completing-read "Type: " (slime-bogus-completion-alist
                                          '(":callers" ":callees" ":calls"))
                                nil t ":"))))
  (slime-call-with-browser-setup 
   "*slime xref browser*" (slime-current-package) "Xref Browser"
   (lambda ()
     (widget-create 'tree-widget :tag name :xref-type type :xref-dspec name 
                    :dynargs 'slime-expand-xrefs :has-echildren t))))


;;;; Buffer selector

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
           (sleep-for 1)
           (discard-input)
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


(def-slime-selector-method ?? "Selector help buffer."
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
  "SLIME Read-Eval-Print-Loop."
  (slime-output-buffer))

(def-slime-selector-method ?s
  "*slime-scratch* buffer."
  (slime-scratch-buffer))

(def-slime-selector-method ?i
  "*inferior-lisp* buffer."
  (cond ((and (slime-connected-p) (slime-process))
         (process-buffer (slime-process)))
        (t
         "*inferior-lisp*")))

(def-slime-selector-method ?v
  "*slime-events* buffer."
  slime-event-buffer-name)

(def-slime-selector-method ?l
  "most recently visited lisp-mode buffer."
  (slime-recently-visited-buffer 'lisp-mode))

(def-slime-selector-method ?d
  "*sldb* buffer for the current connection."
  (unless (sldb-get-default-buffer)
    (error "No debugger buffer"))
  (sldb-get-default-buffer))

(def-slime-selector-method ?e
  "most recently visited emacs-lisp-mode buffer."
  (slime-recently-visited-buffer 'emacs-lisp-mode))

(def-slime-selector-method ?c
  "SLIME connections buffer."
  (slime-list-connections)
  "*SLIME connections*")

(def-slime-selector-method ?t
  "SLIME threads buffer."
  (slime-list-threads)
  "*slime-threads*")

(defun slime-recently-visited-buffer (mode)
  "Return the most recently visited buffer whose major-mode is MODE.
Only considers buffers that are not already visible."
  (loop for buffer in (buffer-list)
        when (and (with-current-buffer buffer (eq major-mode mode))
                  (null (get-buffer-window buffer 'visible)))
        return buffer
        finally (error "Can't find unshown buffer in %S" mode)))


;;;; Editing commands

(defvar *slime-comment-start-regexp*
  "\\(\\(^\\|[^\n\\\\]\\)\\([\\\\][\\\\]\\)*\\);+[ \t]*"
  "Regexp to match the start of a comment.")

(defun slime-beginning-of-comment ()
  "Move point to beginning of comment.
If point is inside a comment move to beginning of comment and return point.
Otherwise leave point unchanged and return NIL."
  (let ((boundary (point)))
    (beginning-of-line)
    (cond ((re-search-forward *slime-comment-start-regexp* boundary t)
           (point))
          (t (goto-char boundary) 
             nil))))

(defun slime-close-all-sexp (&optional region)
  "Balance parentheses of open s-expressions at point.
Insert enough right parentheses to balance unmatched left parentheses.
Delete extra left parentheses.  Reformat trailing parentheses 
Lisp-stylishly.

If REGION is true, operate on the region. Otherwise operate on
the top-level sexp before point."
  (interactive "P")
  (let ((sexp-level 0)
        point)
    (save-excursion
      (save-restriction
        (when region
          (narrow-to-region (region-beginning) (region-end))
          (goto-char (point-max)))
        ;; skip over closing parens, but not into comment
        (skip-chars-backward ") \t\n")
        (when (slime-beginning-of-comment)
          (forward-line)
          (skip-chars-forward " \t"))
        (setq point (point))
        ;; count sexps until either '(' or comment is found at first column
        (while (and (not (looking-at "^[(;]"))
                  (ignore-errors (backward-up-list 1) t))
          (incf sexp-level))))
    (when (> sexp-level 0)
      ;; insert correct number of right parens
      (goto-char point)
      (dotimes (i sexp-level) (insert ")"))
      ;; delete extra right parens
      (setq point (point))
      (skip-chars-forward " \t\n)")
      (skip-chars-backward " \t\n")
      (delete-region point (point)))))

(defun slime-insert-balanced-comments (arg)
  "Insert a set of balanced comments around the s-expression
containing the point.  If this command is invoked repeatedly
\(without any other command occurring between invocations), the
comment progressively moves outward over enclosing expressions.
If invoked with a positive prefix argument, the s-expression arg
expressions out is enclosed in a set of balanced comments."
  (interactive "*p")
  (save-excursion
    (when (eq last-command this-command)
      (when (search-backward "#|" nil t)
        (save-excursion
          (delete-char 2)
          (while (and (< (point) (point-max)) (not (looking-at " *|#")))
            (forward-sexp))
          (replace-match ""))))
    (while (> arg 0)
      (backward-char 1)
      (cond ((looking-at ")") (incf arg))
            ((looking-at "(") (decf arg))))
    (insert "#|")
    (forward-sexp)
    (insert "|#")))

(defun slime-remove-balanced-comments ()
  "Remove a set of balanced comments enclosing point."
  (interactive "*")
  (save-excursion
    (when (search-backward "#|" nil t)
      (delete-char 2)
      (while (and (< (point) (point-max)) (not (looking-at " *|#")))
      (forward-sexp))
      (replace-match ""))))

(defun slime-pretty-lambdas ()
  "Show `lambda' as a lambda character, via font-lock.
This can be called from slime-mode-hook.

Warning: Some people have had this insert funny characters in their
source files, for reasons unknown."
  (interactive)
  (font-lock-add-keywords
   nil `(("(\\(lambda\\>\\)"
        (0 (progn (compose-region (match-beginning 1) (match-end 1)
                            ,(make-char 'greek-iso8859-7 107))
                nil))))))

(defvar slime-close-parens-limit 16
  "Maxmimum parens for `slime-close-parens-at-point' to insert.")

(defun slime-close-parens-at-point ()
  "Close parenthesis at point to complete the top-level-form.  Simply
inserts ')' characters at point until `beginning-of-defun' and
`end-of-defun' execute without errors, or `slime-close-parens-limit'
is exceeded."
  (interactive)
  (loop for i from 1 to slime-close-parens-limit
        until (save-excursion
                (beginning-of-defun)
                (ignore-errors (end-of-defun) t))
        do (insert ")")))


;;;; Font Lock

(defcustom slime-highlight-suppressed-forms t
  "Display forms disabled by reader conditionals as comments."
  :type '(choice (const :tag "Enable" t) (const :tag "Disable" nil))
  :group 'slime-mode)

(defface slime-reader-conditional-face
  (if (slime-face-inheritance-possible-p)
    '((t (:inherit font-lock-comment-face)))
  '((((background light)) (:foreground "DimGray" :bold t))
    (((background dark)) (:foreground "LightGray" :bold t))))
  "Face for compiler notes while selected."
  :group 'slime-mode-faces)

(defun slime-search-suppressed-forms (limit)
  "Find reader conditionalized forms where the test is false."
  (when (and slime-highlight-suppressed-forms
             (slime-connected-p)
	     (re-search-forward "^\\([^;\n]*[ \t(]\\)?#[-+]" limit t))
    (ignore-errors
      (let* ((start (- (point) 2))
             (char (char-before))
             (e (read (current-buffer)))
             (val (slime-eval-feature-conditional e)))
        (when (<= (point) limit)
          (if (or (and (eq char ?+) (not val))
                  (and (eq char ?-) val))
              (progn 
                (forward-sexp)
                (assert (<= (point) limit))
                (let ((md (match-data)))
                  (fill md nil)
                  (setf (first md) start)
                  (setf (second md) (point))
                  (set-match-data md)
                  t))
            (slime-search-suppressed-forms limit)))))))

(defun slime-activate-font-lock-magic ()
  (if (featurep 'xemacs)
      (let ((pattern `((slime-search-suppressed-forms
                        (0 slime-reader-conditional-face t)))))
        (dolist (sym '(lisp-font-lock-keywords
                       lisp-font-lock-keywords-1
                       lisp-font-lock-keywords-2))
          (set sym (append (symbol-value sym) pattern))))
    (font-lock-add-keywords
     'lisp-mode
     `((slime-search-suppressed-forms 0 ,''slime-reader-conditional-face t)))))

(when slime-highlight-suppressed-forms
  (slime-activate-font-lock-magic))


;;;; Indentation

(defcustom slime-conservative-indentation nil
  "If true then don't discover indentation of \"with-\" or \"def\" symbols."
  :type 'boolean
  :group 'slime-mode)

(defun slime-update-indentation ()
  "Update indentation for all macros defined in the Lisp system."
  (interactive)
  (slime-eval-async '(swank:update-indentation-information)))

(defun slime-handle-indentation-update (alist)
  "Update Lisp indent information.

ALIST is a list of (SYMBOL-NAME . INDENT-SPEC) of proposed indentation
settings for `common-lisp-indent-function'. The appropriate property
is setup, unless the user already set one explicitly."
  (dolist (info alist)
    (let ((symbol-name (car info)))
      (unless (and slime-conservative-indentation
                   (string-match "^\\(def\\|\\with-\\)" symbol-name))
        (let ((symbol (intern symbol-name))
              (indent (cdr info)))
          ;; Does the symbol have an indentation value that we set?
          (when (equal (get symbol 'common-lisp-indent-function)
                       (get symbol 'slime-indent))
            (put symbol 'slime-indent indent)
            (put symbol 'common-lisp-indent-function indent)))))))

(defun slime-reindent-defun (&optional force-text-fill)
  "Reindent the current defun, or refill the current paragraph.
If point is inside a comment block, the text around point will be
treated as a paragraph and will be filled with `fill-paragraph'.
Otherwise, it will be treated as Lisp code, and the current defun
will be reindented.  If the current defun has unbalanced parens,
an attempt will be made to fix it before reindenting.

When given a prefix argument, the text around point will always
be treated as a paragraph.  This is useful for filling docstrings."
  (interactive "P")
  (save-excursion
    (if (or force-text-fill (slime-beginning-of-comment))
        (fill-paragraph nil)
      (let ((start (progn (unless (and (zerop (current-column))
                                       (eq ?\( (char-after)))
                            (if (and (boundp 'slime-repl-input-start-mark)
                                     slime-repl-input-start-mark)
                                (slime-repl-beginning-of-defun)
                              (beginning-of-defun)))
                          (point)))
            (end (ignore-errors (end-of-defun) (point))))
        (unless end
          (forward-paragraph)
          (slime-close-all-sexp)
          (end-of-defun)
          (setf end (point)))
        (indent-region start end nil)))))


;;;; Test suite

(defstruct (slime-test (:conc-name slime-test.))
  name fname args doc inputs fails-for)
  
(defvar slime-tests '()
  "Names of test functions.")

(defvar slime-test-debug-on-error nil
  "*When non-nil debug errors in test cases.")

(defvar slime-total-tests nil
  "Total number of tests executed during a test run.")

(defvar slime-failed-tests nil
  "Total number of failed tests during a test run.")

(defvar slime-expected-failures nil
  "Total number of expected failures during a test run")

(defvar slime-test-buffer-name "*Tests*"
  "The name of the buffer used to display test results.")


;; dynamically bound during a single test
(defvar slime-current-test)
(defvar slime-unexpected-failures)


;;;;; Execution engine

(defun slime-run-tests ()
  "Run the test suite.
The results are presented in an outline-mode buffer, with the tests
that succeeded initially folded away."
  (interactive)
  (assert (not (slime-busy-p)))
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

(defun slime-test-should-fail-p (test)
  (member (slime-lisp-implementation-type-name)
          (slime-test.fails-for test)))

(defun slime-execute-tests ()
  "Execute each test case with each input.
Return the number of failed tests."
  (save-window-excursion
    (let ((slime-total-tests 0)
          (slime-expected-passes 0)
          (slime-unexpected-failures 0)
          (slime-expected-failures 0))
      (dolist (slime-current-test slime-tests)
        (with-struct (slime-test. name (function fname) inputs) 
            slime-current-test
          (slime-test-heading 1 "%s" name)
          (dolist (input inputs)
            (incf slime-total-tests)
            (slime-test-heading 2 "input: %s" input)
            (if slime-test-debug-on-error
                (let ((debug-on-error t)
                      (debug-on-quit t))
                  (apply function input))
              (condition-case err
                  (apply function input)
                (error
                 (cond ((slime-test-should-fail-p slime-current-test)
                        (incf slime-expected-failures)
                        (slime-test-failure "ERROR (expected)"
                                            (format "%S" err)))
                       (t
                        (incf slime-unexpected-failures)
                        (slime-print-check-error err)))))))))
      (let ((summary (cond ((and (zerop slime-expected-failures)
                                 (zerop slime-unexpected-failures))
                            (format "All %S tests completed successfully."
                                    slime-total-tests))
                           (t
                            (format "Failed on %S (%S expected) of %S tests."
                                    (+ slime-expected-failures
                                       slime-unexpected-failures)
                                    slime-expected-failures
                                    slime-total-tests)))))
        (save-excursion
          (with-current-buffer slime-test-buffer-name
            (goto-char (point-min))
            (insert summary "\n\n")))
        (message "%s" summary)
        slime-unexpected-failures))))

(defun slime-batch-test (results-file)
  "Run the test suite in batch-mode.
Exits Emacs when finished. The exit code is the number of failed tests."
  (let ((slime-test-debug-on-error nil))
    (slime)
    ;; Block until we are up and running.
    (while (not (slime-connected-p))
      (sit-for 1))
    (slime-sync-to-top-level 5)
    (switch-to-buffer "*scratch*")
    (let ((failed-tests (slime-run-tests)))
      (with-current-buffer slime-test-buffer-name
        (slime-delete-hidden-outline-text)
        (goto-char (point-min))
        (insert "-*- outline -*-\n\n")
        (write-file results-file))
      (kill-emacs failed-tests))))


;;;;; Results buffer creation and output

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


;;;;; Macros for defining test cases

(defmacro def-slime-test (name args doc inputs &rest body)
  "Define a test case.
NAME ::= SYMBOL | (SYMBOL (FAILS-FOR*)) is a symbol naming the test.
ARGS is a lambda-list.
DOC is a docstring.
INPUTS is a list of argument lists, each tested separately.
BODY is the test case. The body can use `slime-check' to test
conditions (assertions)."
  (multiple-value-bind (name fails-for) (etypecase name
                                          (symbol (values name '()))
                                          (cons name))
    (let ((fname (intern (format "slime-test-%s" name))))
      `(progn
         (defun ,fname ,args
           ,doc
           (slime-sync)
           ,@body)
         (setq slime-tests 
               (append (remove* ',name slime-tests :key 'slime-test.name)
                       (list (make-slime-test :name ',name :fname ',fname
                                              :fails-for ',fails-for
                                              :inputs ,inputs))))))))

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
         (cond ((slime-test-should-fail-p slime-current-test)
                (incf slime-expected-failures)
                (slime-test-failure "FAIL (expected)" ,check-name))
               (t
                (incf slime-unexpected-failures)
                (slime-print-check-failed ,check-name)))
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


;;;;; Test case definitions

;; Clear out old tests.
(setq slime-tests nil)

(defun slime-check-top-level (&optional test-name)
  (slime-check "At the top level (no debugging or pending RPCs)"
    (slime-at-top-level-p)))

(defun slime-at-top-level-p ()
  (and (not (sldb-get-default-buffer))
       (null (slime-rex-continuations))))

(defun slime-wait-condition (name predicate timeout)
  (let ((end (time-add (current-time) (seconds-to-time timeout))))
    (while (not (funcall predicate))
      (cond ((time-less-p end (current-time))
             (error "Timeout waiting for condition: %S" name))
            (t
             (accept-process-output nil 0 100000))))))

(defun slime-sync-to-top-level (timeout)
  (slime-wait-condition "top-level" #'slime-at-top-level-p timeout))

;; XXX: unused function
(defun slime-check-sldb-level (expected)
  (let ((sldb-level (when-let (sldb (sldb-get-default-buffer))
                      (with-current-buffer sldb
                        sldb-level))))
    (slime-check ("SLDB level (%S) is %S" expected sldb-level)
      (equal expected sldb-level))))

(defun slime-test-expect (name expected actual &optional test)
  (when (stringp expected) (setq expected (substring-no-properties expected)))
  (when (stringp actual)   (setq actual (substring-no-properties actual)))
  (slime-check ("%s:\nexpected: [%S]\n  actual: [%S]" name expected actual)
    (funcall (or test #'equal) expected actual)))

(defun sldb-level ()
  (when-let (sldb (sldb-get-default-buffer))
    (with-current-buffer sldb
      sldb-level)))

(defun slime-sldb-level= (level)
  (when-let (sldb (sldb-get-default-buffer))
    (with-current-buffer sldb
      (equal sldb-level level))))

(def-slime-test find-definition
    (name buffer-package)
    "Find the definition of a function or macro in swank.lisp."
    '((read-from-emacs "SWANK")
      (swank::read-from-emacs "CL-USER")
      (swank:start-server "CL-USER"))
  (switch-to-buffer "*scratch*")        ; not buffer of definition
  (slime-check-top-level)
  (let ((orig-buffer (current-buffer))
        (orig-pos (point))
        (enable-local-variables nil)    ; don't get stuck on -*- eval: -*-
        (slime-buffer-package buffer-package))
    (slime-edit-definition (symbol-name name))
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
           (= orig-pos (point)))))
    (slime-check-top-level))

(def-slime-test complete-symbol
    (prefix expected-completions)
    "Find the completions of a symbol-name prefix."
    '(("cl:compile" (("cl:compile" "cl:compile-file" "cl:compile-file-pathname"
                      "cl:compiled-function" "cl:compiled-function-p" "cl:compiler-macro"
                      "cl:compiler-macro-function")
                     "cl:compile"))
      ("cl:foobar" (nil ""))
      ("swank::compile-file" (("swank::compile-file" 
                               "swank::compile-file-for-emacs"
                               "swank::compile-file-if-needed"
                               "swank::compile-file-pathname")
                              "swank::compile-file"))
      ("cl:m-v-l" (("cl:multiple-value-list" "cl:multiple-values-limit")
                   "cl:multiple-value-li")))
  (let ((completions (slime-completions prefix)))
    (slime-test-expect "Completion set" expected-completions completions)))

(def-slime-test arglist
    ;; N.B. Allegro apparently doesn't return the default values of
    ;; optional parameters. Thus the regexp in the start-server
    ;; expected value. In a perfect world we'd find a way to smooth
    ;; over this difference between implementations--perhaps by
    ;; convincing Franz to provide a function that does what we want.
    (function-name expected-arglist)
    "Lookup the argument list for FUNCTION-NAME.
Confirm that EXPECTED-ARGLIST is displayed."
    '(("swank:start-server"
       "(swank:start-server port-file &key \\((style \\*communication-style\\*)\\|style\\)[ \n]+dont-close[ \n]+(external-format \\*coding-system\\*))")
      ("swank::compound-prefix-match"
       "(swank::compound-prefix-match prefix target)")
      ("swank::create-socket"
       "(swank::create-socket host port)")
      ("swank::emacs-connected"
       "(swank::emacs-connected)")
      ("swank::compile-string-for-emacs"
       "(swank::compile-string-for-emacs string buffer position directory)")
      ("swank::connection.socket-io"
       "(swank::connection.socket-io \\(struct\\(ure\\)?\\|object\\|instance\\))")
      ("cl:lisp-implementation-type"
       "(cl:lisp-implementation-type)")
      ("cl:class-name" 
       "(cl:class-name \\(class\\|object\\|instance\\|structure\\))"))
  (slime-check-top-level)
  (let ((arglist (slime-get-arglist function-name))) ;
    (slime-test-expect "Argument list is as expected"
                       expected-arglist arglist
                       #'string-match))
  (slime-check-top-level))

(def-slime-test (compile-defun ("allegro" "lispworks" "clisp"))
    (program subform)
    "Compile PROGRAM containing errors.
Confirm that SUBFORM is correctly located."
    '(("(defun cl-user::foo () (cl-user::bar))" (cl-user::bar))
      ("(defun cl-user::foo () 
          #\\space
          ;;Sdf              
          (cl-user::bar))"
       (cl-user::bar))
      ("(defun cl-user::foo () 
             #+(or)skipped
             #| #||#
                #||# |#
             (cl-user::bar))"
       (cl-user::bar))
      ("(defun cl-user::foo () 
           (list `(1 ,(random 10) 2 ,@(random 10) 3 ,(cl-user::bar))))"
       (cl-user::bar))
      ("(defun cl-user::foo ()
          \"\\\" bla bla \\\"\"
          (cl-user::bar))"
       (cl-user::bar)))
  (slime-check-top-level)
  (with-temp-buffer 
    (lisp-mode)
    (insert program)
    (slime-compile-defun)
    (slime-sync)
    (goto-char (point-max))
    (slime-previous-note)
    (slime-check error-location-correct
      (equal (read (current-buffer))
             subform)))
  (slime-check-top-level))

(def-slime-test async-eval-debugging (depth)
  "Test recursive debugging of asynchronous evaluation requests."
  '((1) (2) (3))
  (slime-check-top-level)
  (lexical-let ((depth depth)
                (debug-hook-max-depth 0))
    (let ((debug-hook
           (lambda ()
             (with-current-buffer (sldb-get-default-buffer)
               (when (> sldb-level debug-hook-max-depth)
                 (setq debug-hook-max-depth sldb-level)
                 (if (= sldb-level depth)
                     ;; We're at maximum recursion - time to unwind
                     (sldb-quit)
                   ;; Going down - enter another recursive debug
                   ;; Recursively debug.
                   (slime-eval-async 'no-such-variable)))))))
      (let ((sldb-hook (cons debug-hook sldb-hook)))
        (slime-eval-async 'no-such-variable)
        (slime-sync-to-top-level 15)
        (slime-check-top-level)
        (slime-check ("Maximum depth reached (%S) is %S."
                      debug-hook-max-depth depth)
          (= debug-hook-max-depth depth))))))

(def-slime-test loop-interrupt-quit
    ()
    "Test interrupting a loop."
    '(())
  (slime-check-top-level)
  (slime-eval-async '(cl:loop) (lambda (_) ) "CL-USER")
  (accept-process-output nil 1)
  (slime-check "In eval state." (slime-busy-p))
  (slime-interrupt)
  (slime-wait-condition "First interrupt" (lambda () (slime-sldb-level= 1)) 5)
  (with-current-buffer (sldb-get-default-buffer) 
    (sldb-quit))
  (slime-sync-to-top-level 5)
  (slime-check-top-level))

(def-slime-test loop-interrupt-continue-interrupt-quit
    ()
    "Test interrupting a previously interrupted but continued loop."
    '(())
  (slime-check-top-level)
  (slime-eval-async '(cl:loop) (lambda (_) ) "CL-USER")
  (sleep-for 1)
  (slime-wait-condition "running" #'slime-busy-p 5)
  (slime-interrupt)
  (slime-wait-condition "First interrupt" (lambda () (slime-sldb-level= 1)) 5)
  (with-current-buffer (sldb-get-default-buffer)
    (sldb-continue))
  (slime-wait-condition "running" (lambda () 
                                    (and (slime-busy-p)
                                         (not (sldb-get-default-buffer)))) 5)
  (slime-interrupt)
  (slime-wait-condition "Second interrupt" (lambda () (slime-sldb-level= 1)) 5)
  (with-current-buffer (sldb-get-default-buffer)
    (sldb-quit))
  (slime-sync-to-top-level 5)
  (slime-check-top-level))
 
(def-slime-test interactive-eval 
    ()
    "Test interactive eval and continuing from the debugger."
    '(())
  (slime-check-top-level)
  (lexical-let ((done nil))
    (let ((sldb-hook (lambda () (sldb-continue) (setq done t))))
      (slime-interactive-eval 
       "(progn(cerror \"foo\" \"restart\")(cerror \"bar\" \"restart\")(+ 1 2))")
      (while (not done) (accept-process-output))
      (slime-sync-to-top-level 5)
      (slime-check-top-level)
      (let ((message (current-message)))
        (slime-check "Minibuffer contains: \"3\""
          (equal "3 (#x3, #o3, #b11)" message))))))

(def-slime-test interrupt-bubbling-idiot 
    ()
    "Test interrupting a loop that sends a lot of output to Emacs."
    '(())
  (slime-check-top-level)
  (slime-eval-async '(cl:loop :for i :from 0 :do (cl:progn (cl:print i) 
                                                           (cl:force-output)))
                    (lambda (_) ) "CL-USER")
  (accept-process-output nil 1)
  (slime-wait-condition "running" #'slime-busy-p 5)
  (slime-interrupt)
  (slime-wait-condition "Debugger visible" 
                        (lambda () 
                          (and (slime-sldb-level= 1)
                               (get-buffer-window (sldb-get-default-buffer))))
                        5)
  (with-current-buffer (sldb-get-default-buffer)
    (sldb-quit))
  (slime-sync-to-top-level 5))

(def-slime-test package-updating
    (package-name nicknames)
    "Test if slime-lisp-package is updated."
    '(("COMMON-LISP" ("CL"))
      ("KEYWORD" ("" "KEYWORD"))
      ("COMMON-LISP-USER" ("CL-USER")))
  (with-current-buffer (slime-output-buffer)
    (let ((p (slime-eval 
              `(swank:listener-eval 
                ,(format 
                  "(cl:setq cl:*package* (cl:find-package %S))
                   (cl:package-name cl:*package*)" package-name))
              (slime-lisp-package))))
      (slime-check ("In %s package." package-name)
        (equal (format "\"%s\"" package-name) p))
      (slime-check ("slime-lisp-package is %S." package-name)
        (equal (slime-lisp-package) package-name))
      (slime-check ("slime-lisp-package-prompt-string is in %S." nicknames)
        (member (slime-lisp-package-prompt-string) nicknames)))))

(def-slime-test repl-test
    (input result-contents)
    "Test simple commands in the minibuffer."
    '(("(+ 1 2)" "SWANK> (+ 1 2)
3
SWANK> ")
      ("(princ 10)" "SWANK> (princ 10)
10
10
SWANK> "
      )
      ("(princ 10)(princ 20)" "SWANK> (princ 10)(princ 20)
1020
20
SWANK> "
      )
      ("(dotimes (i 10 77) (princ i) (terpri))" 
       "SWANK> (dotimes (i 10 77) (princ i) (terpri))
0
1
2
3
4
5
6
7
8
9
77
SWANK> "
      )
      )
  (with-current-buffer (slime-output-buffer)
    (setf (slime-lisp-package-prompt-string) "SWANK"))
  (kill-buffer (slime-output-buffer))
  (with-current-buffer (slime-output-buffer)
    (insert input)
    (slime-test-expect "Buffer contains input" 
                       (concat "SWANK> " input)
                       (buffer-string))
    (call-interactively 'slime-repl-return)
    (slime-sync-to-top-level 5)
    (slime-test-expect "Buffer contains result" 
                       result-contents (buffer-string))))

(def-slime-test repl-return 
    (before after result-contents)
    "Test if slime-repl-return sends the correct protion to Lisp even
if point is not at the end of the line."
    '(("(+ 1 2)" "" "SWANK> (+ 1 2)
3
SWANK> ")
("(+ 1 " "2)" "SWANK> (+ 1 2)
3
SWANK> ")

("(+ 1\n" "2)" "SWANK> (+ 1
2)
3
SWANK> ")

)
  (with-current-buffer (slime-output-buffer)
    (setf (slime-lisp-package-prompt-string) "SWANK"))
  (kill-buffer (slime-output-buffer))
  (with-current-buffer (slime-output-buffer)
    (insert before)
    (save-excursion (insert after))
    (slime-test-expect "Buffer contains input" 
                       (concat "SWANK> " before after)
                       (buffer-string))
    (call-interactively 'slime-repl-return)
    (slime-sync-to-top-level 5)
    (slime-test-expect "Buffer contains result" 
                       result-contents (buffer-string))))
  
(def-slime-test repl-read
    (prompt input result-contents)
    "Test simple commands in the minibuffer."
    '(("(read-line)" "foo" "SWANK> (values (read-line))
foo
\"foo\"
SWANK> ")
      ("(read-char)" "1" "SWANK> (values (read-char))
1
#\\1
SWANK> ")
      ("(read)" "(+ 2 3
4)" "SWANK> (values (read))
\(+ 2 3
4)
\(+ 2 3 4)
SWANK> ")
      )
  (with-current-buffer (slime-output-buffer)
    (setf (slime-lisp-package-prompt-string) "SWANK"))
  (kill-buffer (slime-output-buffer))
  (with-current-buffer (slime-output-buffer)
    (insert (format "(values %s)" prompt))
    (call-interactively 'slime-repl-return)
    (slime-wait-condition "reading" #'slime-reading-p 5)
    (insert input)
    (call-interactively 'slime-repl-return)
    (slime-sync-to-top-level 5)
    (slime-check "Buffer contains result"
      (equal result-contents (buffer-string)))))

(def-slime-test repl-read-lines
    (command inputs final-contents)
    "Test reading multiple lines from the repl."
    '(("(list (read-line) (read-line) (read-line))" 
       ("a" "b" "c")
       "SWANK> (list (read-line) (read-line) (read-line))
a
b
c
\(\"a\" \"b\" \"c\")
SWANK> "))
  (when (slime-output-buffer)
    (kill-buffer (slime-output-buffer)))
  (with-current-buffer (slime-output-buffer)
    (setf (slime-lisp-package-prompt-string) "SWANK")
    (insert command)
    (call-interactively 'slime-repl-return)
    (dolist (input inputs) 
      (slime-wait-condition "reading" #'slime-reading-p 5)
      (insert input)
      (call-interactively 'slime-repl-return))
    (slime-sync-to-top-level 5)
    (slime-check "Buffer contains result"
      (equal final-contents (buffer-string)))))

(def-slime-test interactive-eval-output
    (input result-contents visiblep)
    "Test simple commands in the minibuffer."
    '(("(+ 1 2)" ";;;; (+ 1 2) ...
SWANK> " nil)
      ("(princ 10)" ";;;; (princ 10) ...
10
SWANK> " t))
  (with-current-buffer (slime-output-buffer)
    (setf (slime-lisp-package-prompt-string) "SWANK"))
  (kill-buffer (slime-output-buffer))
  (with-current-buffer (slime-output-buffer)
    (slime-interactive-eval input) 
    (slime-sync-to-top-level 5)
    (slime-test-expect "Buffer contains result" 
                       result-contents (buffer-string))
    (slime-test-expect "Buffer visible?" 
                       visiblep
                       (not (not (get-buffer-window (current-buffer)))))))

(def-slime-test break 
    ()
    "Test if BREAK invokes SLDB."
    '(())
  (slime-compile-string (prin1-to-string '(cl:defun cl-user::foo () 
                                                    (cl:break))) 
                        0)
  (slime-eval-async '(cl-user::foo))
  (slime-wait-condition "Debugger visible" 
                        (lambda () 
                          (and (slime-sldb-level= 1)
                               (get-buffer-window (sldb-get-default-buffer))))
                        10)
  (with-current-buffer (sldb-get-default-buffer)
    (sldb-quit))
  (slime-sync-to-top-level 5))
      

;;;; Utilities

;;;;; Common Lisp-style package-qualified symbols

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

;; XXX: unused function
(defun slime-cl-symbol-external-ref-p (symbol)
  "Does SYMBOL refer to an external symbol?
FOO:BAR is an external reference.
FOO::BAR is not, and nor is BAR."
  (let ((name (if (stringp symbol) symbol (symbol-name symbol))))
    (and (string-match ":" name)
         (not (string-match "::" name)))))

;; XXX: unused function
(defun slime-qualify-cl-symbol (symbol-or-name)
  "Like `slime-qualify-cl-symbol-name', but interns the result."
  (intern (slime-qualify-cl-symbol-name symbol-or-name)))

(defun slime-qualify-cl-symbol-name (symbol-or-name)
  "Return a package-qualified symbol-name that indicates the CL symbol
SYMBOL. If SYMBOL doesn't already have a package prefix the current
package is used."
  (let ((s (if (stringp symbol-or-name)
               symbol-or-name
             (symbol-name symbol-or-name))))
    (if (slime-cl-symbol-package s)
        s
      (format "%s::%s"
              (slime-current-package)
              (slime-cl-symbol-name s)))))


;;;;; Extracting Lisp forms from the buffer or user

(defun slime-defun-at-point ()
  "Return the text of the defun at point."
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (buffer-substring-no-properties (point) end))))

(defun slime-beginning-of-symbol ()
  "Move point to the beginning of the current symbol."
  (and (minusp (skip-syntax-backward "w_"))
       (when (eq (char-before) ?#) ; special case for things like "#<foo"
         (forward-char))))

(defun slime-end-of-symbol ()
  "Move point to the end of the current symbol."
  (skip-syntax-forward "w_"))

(put 'slime-symbol 'end-op 'slime-end-of-symbol)
(put 'slime-symbol 'beginning-op 'slime-beginning-of-symbol)

(defun slime-symbol-start-pos ()
  "Return the starting position of the symbol under point.
The result is unspecified if there isn't a symbol under the point."
  (save-excursion (slime-beginning-of-symbol) (point)))

(defun slime-symbol-end-pos ()
  (save-excursion (slime-end-of-symbol) (point)))

(defun slime-symbol-name-at-point ()
  "Return the name of the symbol at point, otherwise nil."
  (save-restriction
    ;; Don't be tricked into grabbing the REPL prompt.
    (when (and (eq major-mode 'slime-repl-mode)
               (>= (point) slime-repl-input-start-mark))
      (narrow-to-region slime-repl-input-start-mark (point-max)))
    (save-excursion
      (let ((string (thing-at-point 'slime-symbol)))
        (and string
             ;; In Emacs20 (thing-at-point 'symbol) returns "" instead
             ;; of nil when called from an empty (or
             ;; narrowed-to-empty) buffer.
             (not (equal string ""))
             (substring-no-properties string))))))

(defun slime-symbol-at-point ()
  "Return the symbol at point, otherwise nil."
  (let ((name (slime-symbol-name-at-point)))
    (and name (intern name))))

(defun slime-sexp-at-point ()
  "Return the sexp at point as a string, otherwise nil."
  (let ((string (thing-at-point 'sexp)))
    (if string (substring-no-properties string) nil)))

(defun slime-function-called-at-point/line ()
  "Return the name of the function being called at point, provided the
function call starts on the same line at the point itself."
  (and (ignore-errors
         (slime-same-line-p (save-excursion (backward-up-list 1) (point))
                            (point)))
       (slime-function-called-at-point)))

(defun slime-function-called-at-point ()
  "Return a function around point or else called by the list containing point.
Return the symbol-name, or nil."
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
        (slime-symbol-name-at-point)))))

(defun slime-enclosing-operator-names ()
  "Return the list of operator names of the forms containing point."
  (let ((result '()))
    (ignore-errors
      (save-restriction
        (narrow-to-region (save-excursion (beginning-of-defun) (point))
                          (point))
        (save-excursion
          (while t
            (backward-up-list 1)
            (when (looking-at "(")
              (forward-char 1)
              (when-let (name (slime-symbol-name-at-point))
                (push name result))
              (backward-up-list 1))))))
    (nreverse result)))


;;;;; Portability library

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
  (defmacro slime-defun-if-undefined (name &rest rest)
    `(unless (fboundp ',name)
       (defun ,name ,@rest))))

(put 'slime-defun-if-undefined 'lisp-indent-function 2)

(slime-defun-if-undefined next-single-char-property-change
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

(slime-defun-if-undefined previous-single-char-property-change 
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

(slime-defun-if-undefined substring-no-properties (string &optional start end)
  (let* ((start (or start 0))
	 (end (or end (length string)))
	 (string (substring string start end)))
    (set-text-properties start end nil string)
    string))

(slime-defun-if-undefined set-window-text-height (window height)
  (let ((delta (- height (window-text-height window))))
    (unless (zerop delta)
      (let ((window-min-height 1))
	(if (and window (not (eq window (selected-window))))
	    (save-selected-window
	      (select-window window)
	      (enlarge-window delta))
	  (enlarge-window delta))))))

(slime-defun-if-undefined window-text-height (&optional window)
  (1- (window-height window)))

(slime-defun-if-undefined subst-char-in-string (fromchar tochar string 
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

(slime-defun-if-undefined count-screen-lines 
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

(slime-defun-if-undefined seconds-to-time (seconds)
  "Convert SECONDS (a floating point number) to a time value."
  (list (floor seconds 65536)
	(floor (mod seconds 65536))
	(floor (* (- seconds (ffloor seconds)) 1000000))))

(slime-defun-if-undefined time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(slime-defun-if-undefined time-add (t1 t2)
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

(slime-defun-if-undefined line-beginning-position (&optional n)
  (save-excursion
    (beginning-of-line n)
    (point)))

(slime-defun-if-undefined line-end-position (&optional n)
  (save-excursion
    (end-of-line n)
    (point)))

(slime-defun-if-undefined check-parens ()
    "Verify that parentheses in the current buffer are balanced.
If they are not, position point at the first syntax error found."
    (interactive)
    (let ((saved-point (point))
	  (state (parse-partial-sexp (point-min) (point-max) -1)))
      (destructuring-bind (depth innermost-start last-terminated-start
				 in-string in-comment after-quote 
				 minimum-depth comment-style 
				 comment-or-string-start &rest _) state
	(cond ((and (zerop depth) 
		    (not in-string) 
		    (or (not in-comment) 
			(and (eq comment-style nil) 
			     (eobp)))
		    (not after-quote))
	       (goto-char saved-point)
	       (message "All parentheses appear to be balanced."))
	      ((plusp depth)
	       (goto-char innermost-start)
	       (error "Missing )"))
	      ((minusp depth)
	       (error "Extra )"))
	      (in-string
	       (goto-char comment-or-string-start)
	       (error "String not terminated"))
	      (in-comment
	       (goto-char comment-or-string-start)
	       (error "Comment not terminated"))
	      (after-quote
	       (error "After quote"))
	      (t (error "Shouldn't happen: parsing state: %S" state))))))

(slime-defun-if-undefined read-directory-name (prompt 
                                               &optional dir default-dirname
                                               mustmatch initial)
  (unless dir
    (setq dir default-directory))
  (unless default-dirname
    (setq default-dirname
	  (if initial (concat dir initial) default-directory)))
  (let ((file (read-file-name prompt dir default-dirname mustmatch initial)))
    (setq file (file-name-as-directory (expand-file-name file)))
    (cond ((file-directory-p file)
           file)
          (t 
           (error "Not a directory: %s" file)))))

(slime-defun-if-undefined check-coding-system (coding-system)
  (or (eq coding-system 'binary)
      (error "No such coding system: %S" coding-system)))

(slime-defun-if-undefined process-coding-system (process)
  '(binary . binary))

(slime-defun-if-undefined set-process-coding-system 
    (process &optional decoding encoding))

(unless (boundp 'temporary-file-directory)
  (defvar temporary-file-directory
    (file-name-as-directory
     (cond ((memq system-type '(ms-dos windows-nt))
            (or (getenv "TEMP") (getenv "TMPDIR") (getenv "TMP") "c:/temp"))
           ((memq system-type '(vax-vms axp-vms))
            (or (getenv "TMPDIR") (getenv "TMP") 
                (getenv "TEMP") "SYS$SCRATCH:"))
           (t
            (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") "/tmp"))))
    "The directory for writing temporary files."))

(defun slime-emacs-20-p ()
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


;;;; Finishing up

(require 'bytecomp)
(let ((byte-compile-warnings '()))
  (mapc #'byte-compile
        '(slime-alistify
          slime-log-event
          slime-events-buffer
          slime-output-string 
          slime-output-buffer
          slime-output-filter
          slime-repl-show-maximum-output
          slime-process-available-input 
          slime-dispatch-event 
          slime-net-filter 
          slime-net-have-input-p
          slime-net-decode-length
          slime-net-read
          slime-print-apropos
          slime-show-note-counts
          slime-insert-propertized
          slime-tree-insert)))

(run-hooks 'slime-load-hook)

(provide 'slime)

;;; slime.el ends here
