;;; -*- mode: emacs-lisp; mode: outline-minor; outline-regexp: ";;;;*"; indent-tabs-mode: nil -*-
;; slime.el -- Superior Lisp Interaction Mode for Emacs
;;; License
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


;;; Commentary

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


;;; Dependencies, major global variables and constants

(eval-and-compile
  (require 'cl)
  (unless (fboundp 'define-minor-mode)
    (require 'easy-mmode)
    (defalias 'define-minor-mode 'easy-mmode-define-minor-mode)))
(require 'inf-lisp)
(require 'pp)
(require 'hideshow)
(require 'hyperspec)
(require 'font-lock)
(when (featurep 'xemacs)
  (require 'overlay))
(require 'easymenu)

(eval-and-compile 
  (defvar slime-path
    (let ((path (or (locate-library "slime") load-file-name)))
      (and path (file-name-directory path)))
    "Directory containing the Slime package.
This is used to load the supporting Common Lisp library, Swank.
The default value is automatically computed from the location of the
Emacs Lisp package."))

(defvar slime-swank-connection-retries nil
  "Number of times to try connecting to the Swank server before aborting.
Nil means never give up.")

(defvar slime-backend "swank-loader.lisp"
  "*The name of the Lisp file that loads the Swank server.
This name is interpreted relative to the directory containing
slime.el, but could also be set to an absolute filename.")

(make-variable-buffer-local
 (defvar slime-buffer-package nil
   "The Lisp package associated with the current buffer.
Don't access this value directly in a program. Call the function with
the same name instead."))

(make-variable-buffer-local
 (defvar slime-repl-package-stack nil
   "The stack of packages visited in this repl."))

(make-variable-buffer-local
 (defvar slime-repl-directory-stack nil
   "The stack of default directories associated with this repl."))

(defvar slime-dont-prompt nil
  "* When true, don't prompt the user for input during startup.
This is used for batch-mode testing.")

(defvar slime-truncate-lines t
  "* When true, set `truncate-lines' in certain popup buffers.
This applies to buffers that present lines as rows of data, such as
debugger backtraces and apropos listings.")

(defvar slime-multiprocessing nil
  "* When true, enable multiprocessing in Lisp.")

(defvar slime-translate-to-lisp-filename-function 'identity
  "Function to use for translating Emacs filenames to Lisp filenames.

The function recieves a string as argument and should return string.")

(defvar slime-translate-from-lisp-filename-function 'identity
  "Function to use for translating Lisp filenames to Emacs filenames.
See also `slime-translate-to-lisp-filename-function'.")

(defvar slime-event-buffer-name "*slime-events*"
  "The name of the Slime event buffer.")

(defvar slime-space-information-p t
  "Whether the SPC key should offer information or not.")

(defvar slime-reply-update-banner-p t
  "Whether Slime should keep a repl banner updated or not.")

(defvar slime-edit-definition-fallback-function nil
  "Function to call when edit-definition fails to find the source itself.
The function is called with the definition name, a string, as its argument.

If you want to fallback on TAGS you can set this to `find-tag'.")

(defvar slime-kill-without-query-p t
  "If non-nil, kill Slime processes without query when quitting Emacs.")

(defvar slime-sbcl-manual-root "http://www.sbcl.org/manual/"
  "*The base URL of the SBCL manual, for documentation lookup.")


;;; Customize group

(defgroup slime nil
  "Interfaction with the Superior Lisp Environment."
  :prefix "slime-"
  :group 'applications)

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
  :group 'slime)

(defface slime-repl-prompt-face
  (if (slime-face-inheritance-possible-p)
      '((t (:inherit font-lock-keyword-face)))
    '((((class color) (background light)) (:foreground "Purple"))
      (((class color) (background dark)) (:foreground "Cyan"))
      (t (:weight bold))))
  "Face for the prompt in the SLIME REPL."
  :group 'slime)

(defface slime-repl-output-face
  (if (slime-face-inheritance-possible-p)
      '((t (:inherit font-lock-string-face)))
    '((((class color) (background light)) (:foreground "RosyBrown"))
      (((class color) (background dark)) (:foreground "LightSalmon"))
      (t (:slant italic))))
  "Face for Lisp output in the SLIME REPL."
  :group 'slime)

(defface slime-repl-input-face
  '((t (:bold t)))
  "Face for previous input in the SLIME REPL."
  :group 'slime)

(defface slime-repl-result-face
  '((t ()))
  "Face for the result of an evaluation in the SLIME REPL."
  :group 'slime)

;; inspector
;; Try  '(slime-inspector-label-face ((t (:weight bold))))
;;      '(slime-inspector-topline-face ((t (:foreground "brown" :weight bold :height 1.2))))
;;      '(slime-inspector-type-face ((t (:foreground "DarkRed" :weight bold))))

(defface slime-inspector-topline-face
  '((t ()))
  "Face for top line describing object."
  :group 'slime)

(defface slime-inspector-label-face
  '((t (:bold t)))
  "Face for labels in the inspector."
  :group 'slime)

(defface slime-inspector-value-face
  '((t ()))
  "Face for things which can themselves be inspected."
  :group 'slime)

(defface slime-inspector-type-face
  '((t ()))
  "Face for type description in inspector."
  :group 'slime)

(defgroup slime-debugger nil
  "Backtrace options and fontification."
  :prefix "sldb-"
  :group 'slime)

(defmacro def-sldb-face (name description &optional default)
  (let ((facename (intern (format "sldb-%s-face" (symbol-name name)))))
    `(defface ,facename
      '((t ,default))
      ,(format "Face for %s." description)
      :group 'slime-debugger)))

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

(def-sldb-face topline "top line describing error"
  (:bold t))
(def-sldb-face condition "condition class"
  (:bold t))
(def-sldb-face section "labels for major sections of backtrace")
(def-sldb-face frame-label "Backtrace frame number")
(def-sldb-face restart-type "restart types"
  (:bold t))
(def-sldb-face restart "restart descriptions")
(def-sldb-face restart-number "restart numbers (correspond to keystrokes to invoke)"
  (:bold t))
(def-sldb-face frame-line "function names and arguments in backtrace")
(def-sldb-face detailed-frame-line "function names and arguments in backtrace for detailed frame")
(def-sldb-face local-name "label for local variable")
(def-sldb-face local-value "local variable values")
(def-sldb-face catch-tag "catch tags")
(def-sldb-face reference "documentation reference"
  (:underline t))

(defcustom slime-compilation-finished-hook 'slime-maybe-list-compiler-notes
  "Hook called with a list of compiler notes after a compilation."
  :group 'slime
  :type 'hook
  :options '(slime-maybe-list-compiler-notes
             slime-list-compiler-notes 
             slime-maybe-show-xrefs-for-notes))

(defcustom slime-complete-symbol-function 'slime-complete-symbol*
  "Function to perform symbol completion."
  :group 'slime
  :type 'function
  :options '(slime-complete-symbol* slime-simple-complete-symbol))
  
(defcustom slime-connected-hook nil
  "List of functions to call when SLIME connects to Lisp."
  :group 'slime
  :type 'hook)

(defcustom slime-startup-animation t
  "Enable the startup animation."
  :type '(choice (const :tag "Enable" t) (const :tag "Disable" nil))
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
\\[slime-edit-definition]	- Edit the definition of the function called at point.
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


;;;;; inferior-slime-mode
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
    ("\M-." slime-edit-definition :inferior t :sldb t)
    ("\M-," slime-pop-find-definition-stack :inferior t :sldb t)
    ("\C-q" slime-close-parens-at-point :prefixed t :inferior t)
    ("\C-\M-q" slime-reindent-defun :inferior t)
    ;; Evaluating
    ("\C-x\C-e" slime-eval-last-expression :inferior t)
    ("\C-x\M-e" slime-eval-last-expression-display-output :inferior t)
    ("\C-p" slime-pprint-eval-last-expression :prefixed t :inferior t)
    ("\C-r" slime-eval-region :prefixed t :inferior t)
    ("\C-\M-x" slime-eval-defun)
    (":"    slime-interactive-eval :prefixed t :sldb t)
    ("\C-e" slime-interactive-eval :prefixed t :sldb t :inferior t)
    ("\C-z" slime-switch-to-output-buffer :prefixed t :sldb t)
    ("\C-g" slime-interrupt :prefixed t :inferior t :sldb t)
    ;; NB: XEmacs dosn't like \C-g.  Use \C-b as "break" key.
    ("\C-b" slime-interrupt :prefixed t :inferior t :sldb t)
    ("\M-g" slime-quit :prefixed t :inferior t :sldb t)
    ;; Documentation
    (" " slime-space :inferior t)
    ("\C-s" slime-insert-arglist :prefixed t :inferior t)
    ("\C-d" slime-describe-symbol :prefixed t :inferior t :sldb t)
    ("\C-f" slime-describe-function :prefixed t :inferior t :sldb t)
    ("\M-d" slime-disassemble-symbol :prefixed t :inferior t :sldb t)
    ("\C-t" slime-toggle-trace-fdefinition :prefixed t :sldb t)
    ("\C-u" slime-undefine-function :prefixed t)
    ("\C-a" slime-apropos :prefixed t :inferior t :sldb t)
    ("\M-a" slime-apropos-all :prefixed t :inferior t :sldb t)
    ;; Kinda crappy binding. Maybe we should introduce some extra
    ;; prefixes for documentation commands. -luke (17/Jan/2004)
    ("P"    slime-apropos-package :prefixed t :inferior t :sldb t)
    ("\C-m" slime-macroexpand-1 :prefixed t :inferior t)
    ("\M-m" slime-macroexpand-all :prefixed t :inferior t)
    ("\M-0" slime-restore-window-configuration :prefixed t :inferior t)
    ("\C-h" slime-hyperspec-lookup :prefixed t :inferior t :sldb t)
    ([(control meta ?\.)] slime-next-location :inferior t)
    ;; Emacs20 on LinuxPPC signals a 
    ;; "Invalid character: 400000040, 2147479172, 0xffffffd8"
    ;; for "\C- ".
    ;; ("\C- " slime-next-location :prefixed t :inferior t)
    ("~" slime-sync-package-and-default-directory :prefixed t :inferior t)
    ("\M-p" slime-repl-set-package :prefixed t :inferior t)
    ;; Cross reference
    ("\C-wc" slime-who-calls :prefixed t :inferior t :sldb t)
    ("\C-wr" slime-who-references :prefixed t :inferior t :sldb t)
    ("\C-wb" slime-who-binds :prefixed t :inferior t :sldb t)
    ("\C-ws" slime-who-sets :prefixed t :inferior t :sldb t)
    ("\C-wm" slime-who-macroexpands :prefixed t :inferior t :sldb t)
    ("<" slime-list-callers :prefixed t :inferior t :sldb t)
    (">" slime-list-callees :prefixed t :inferior t :sldb t)
    ;; "Other"
    ("\I"  slime-inspect :prefixed t :inferior t :sldb t)
    ("\C-]" slime-close-all-sexp :prefixed t :inferior t :sldb t)
    ("\C-xt" slime-list-threads :prefixed t :inferior t :sldb t)
    ("\C-xc" slime-list-connections :prefixed t :inferior t :sldb t)))

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


;;;;; Pull-down menu

(defvar slime-easy-menu
  (let ((C '(slime-connected-p)))
    `("SLIME"
      [ "Edit Definition..."       slime-edit-definition ,C ]
      [ "Return From Definition"   slime-pop-find-definition-stack ,C ]
      [ "Complete Symbol"          slime-complete-symbol ,C ]
      [ "Show REPL"                slime-switch-to-output-buffer ,C ]
      "--"
      ("Evaluation"
       [ "Eval Defun"              slime-eval-defun ,C ]
       [ "Eval Last Expression"    slime-eval-last-expression ,C ]
       [ "Eval And Pretty-Print"   slime-pprint-eval-last-expression ,C ]
       [ "Eval Region"             slime-eval-region ,C ]
       [ "Interactive Eval"        slime-interactive-eval ,C ]
       [ "Scratch Buffer"          slime-scratch ,C ])
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
       [ "Apropos Package..."      slime-apropos-package ,C ]
       [ "Hyperspec..."            slime-hyperspec-lookup t ])
      "--"
      [ "Interrupt Command"        slime-interrupt ,C ]
      [ "Abort Async. Command"     slime-quit ,C ]
      [ "Sync Package & Directory" slime-sync-package-and-default-directory ,C]
      [ "Set Package in REPL"      slime-repl-set-package ,C]
      )))

(easy-menu-define menubar-slime slime-mode-map "SLIME" slime-easy-menu)

(defun slime-add-easy-menu ()
  (easy-menu-add slime-easy-menu 'slime-mode-map))

(add-hook 'slime-mode-hook 'slime-add-easy-menu)
(add-hook 'slime-repl-mode-hook 'slime-add-easy-menu)


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
  (when (and slime-mode (slime-connected-p))
    (slime-process-available-input)))

(defun slime-setup-command-hooks ()
  "Setup a buffer-local `pre-command-hook' to call `slime-pre-command-hook'."
  (make-local-hook 'pre-command-hook)
  (make-local-hook 'post-command-hook)
  (add-hook 'pre-command-hook 'slime-pre-command-hook)
  (add-hook 'post-command-hook 'slime-post-command-hook))

(add-hook 'slime-mode-hook 'slime-setup-command-hooks)
(add-hook 'slime-mode-hook 'slime-buffer-package)


;;; Common utility functions and macros

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

(defmacro* slime-with-chosen-connection ((&optional 
                                          (prefix-arg 'current-prefix-arg))
                                         &body body)
  "Make the connection choosen by PREFIX-ARG current.

\(fn (&optional (PREFIX-ARG 'current-prefix-arg)) &body BODY)"
  `(let ((slime-dispatching-connection 
          (slime-get-named-connection ,prefix-arg)))
     ,@body))

(put 'slime-with-chosen-connection 'lisp-indent-function 1)

(defun slime-get-named-connection (prefix-arg)
  "Get a connection based on PREIFX-ARG."
  (cond ((not prefix-arg) 
         (slime-connection))
        ((equal prefix-arg '(4))
         (slime-find-connection-by-name (slime-read-connection-name)))
        (t (error "Invalid prefix argument: %S" prefix-arg))))

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
  (or (and (eq major-mode 'slime-repl-mode) (slime-lisp-package))
      (let ((string (slime-find-buffer-package)))
        (cond (string
               (cond (dont-cache)
                     ((equal string slime-buffer-package))
                     (t 
                      (setq slime-buffer-package string)
                      (force-mode-line-update)))
               string)
              (t
               (if dont-cache
                   "COMMON-LISP-USER"
                 slime-buffer-package))))))

(defun slime-find-buffer-package ()
  "Figure out which Lisp package the current buffer is associated with."
  (save-excursion
    (when (let ((case-fold-search t)
                (regexp "^(\\(cl:\\|common-lisp:\\)?in-package\\>"))
	    (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t)))
      (goto-char (match-end 0))
      (skip-chars-forward " \n\t\f\r#")
      (let ((pkg (ignore-errors (read (current-buffer)))))
        (if pkg
            (format "%S" pkg))))))

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

;; defun slime-message
(if (or (featurep 'xemacs)
	(= emacs-major-version 20))
    ;; XEmacs truncates multi-line messages in the echo area.
    (defun slime-message (fmt &rest args)
      (slime-display-message (apply #'format fmt args) "*SLIME Note*"))
  (defun slime-message (fmt &rest args)
    (apply 'message fmt args)))

(defun slime-background-message (format-string &rest format-args)
  "Display a message in passing.
This is like `slime-message', but less distracting because it
will never pop up a buffer.
It should be used for \"background\" messages such as argument lists."
  (apply (if (slime-typeout-active-p) #'slime-typeout-message #'message)
         format-string
         format-args))

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

(defun slime-symbol-name-at-point ()
  "Return the name of the symbol at point, otherwise nil."
  (save-restriction
    ;; Don't be tricked into grabbing the REPL prompt.
    (when (and (eq major-mode 'slime-repl-mode)
               (>= (point) slime-repl-input-start-mark))
      (narrow-to-region slime-repl-input-start-mark (point-max)))
    (save-excursion
      (skip-syntax-forward "w_")
      (skip-syntax-backward "-") 
      (let ((string (thing-at-point 'symbol)))
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

(defun slime-read-symbol-name (prompt &optional query)
  "Either read a symbol name or choose the one at point.
The user is prompted if a prefix argument is in effect, if there is no
symbol at point, or if QUERY is non-nil.

Avoids thinking the REPL prompt is a symbol."
  (cond ((or current-prefix-arg query (not (slime-symbol-name-at-point)))
         (slime-read-from-minibuffer prompt (slime-symbol-name-at-point)))
        (t (slime-symbol-name-at-point))))

(defun slime-read-symbol (prompt)
  "Either read a symbol or choose the one at point.
The user is prompted if a prefix argument is in effect or there is no
symbol at point."
  (intern (slime-read-symbol-name prompt)))

(defvar slime-saved-window-configurations nil
  "Stack of configurations before the last changes SLIME made.")

(defun slime-save-window-configuration ()
  "Save the current window configuration.
This should be called before modifying the user's window configuration.

`slime-restore-window-configuration' restores the saved configuration."
  (push (current-window-configuration) slime-saved-window-configurations))

(defun slime-restore-window-configuration ()
  "Restore the most recently saved window configuration."
  (interactive)
  (when slime-saved-window-configurations
    (set-window-configuration (pop slime-saved-window-configurations))))

(defvar slime-temp-buffer-saved-window-configuration nil
  "The window configuration before the temp-buffer was displayed.
Buffer local in temp-buffers.")

(defun slime-temp-buffer-quit ()
  "Kill the current buffer and restore the old window configuration."
  (interactive)
  (let ((config slime-temp-buffer-saved-window-configuration))
    (kill-buffer (current-buffer))
    (when config
      (set-window-configuration config))))
  
(defvar slime-temp-buffer-map)

(define-minor-mode slime-temp-buffer-mode 
  "Mode for displaying read only stuff"
  nil
  " temp"
  '(("q" . undefined)))

(slime-define-keys slime-temp-buffer-mode-map
  ("q" 'slime-temp-buffer-quit))

(defmacro slime-with-output-to-temp-buffer (name package &rest body)
  "Similar to `with-output-to-temp-buffer'.
Also saves the window configuration, and inherits the current
`slime-connection' in a buffer-local variable."
  (let ((config (gensym)))
  `(let ((,config (current-window-configuration))
         (connection (slime-connection))
         (standard-output (with-current-buffer (get-buffer-create ,name)
                            (setq buffer-read-only nil)
                            (erase-buffer)
                            (current-buffer))))
     (prog1 (with-current-buffer standard-output ,@body)
       (with-current-buffer standard-output
         (setq slime-buffer-connection connection)
         (set (make-local-variable 
               'slime-temp-buffer-saved-window-configuration)
              ,config)
         (goto-char (point-min))
         (slime-mode 1)
         (set-syntax-table lisp-mode-syntax-table)
         (slime-temp-buffer-mode 1)
         (setq buffer-read-only t)
         (setq slime-buffer-package ,package)
         (unless (get-buffer-window (current-buffer) t)
           (switch-to-buffer-other-window (current-buffer))))))))

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

(defun slime-read-package-name (prompt &optional initial-value)
  "Read a package name from the minibuffer, prompting with PROMPT."
  (let ((completion-ignore-case t))
    (completing-read prompt (slime-bogus-completion-alist 
                             (slime-eval 
                              `(swank:list-all-package-names t)))
		     nil nil initial-value)))

(defmacro slime-propertize-region (props &rest body)
  (let ((start (gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
	 (add-text-properties ,start (point) ,props)))))

(put 'slime-propertize-region 'lisp-indent-function 1)

(defsubst slime-insert-propertized (props &rest args)
  "Insert all ARGS and then add text-PROPS to the inserted text."
  (slime-propertize-region props (apply #'insert args)))

(defun slime-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let ((print-escape-nonascii nil)
          (print-escape-newlines nil))
      (prin1 sexp (current-buffer))
      (buffer-string))))

(defun slime-close-buffer (buffer-name)
  "Kills buffer BUFFER-NAME if it exists."
  (when (buffer-live-p (get-buffer buffer-name))
    (kill-buffer buffer-name)))

(defun slime-indent-and-complete-symbol ()
  "Indent the current line and perform symbol completion.
First indent the line; if indenting doesn't move point, complete the
symbol."
  (interactive)
  (let ((pos (point)))
    (lisp-indent-line)
    (when (and (= pos (point))
               (save-excursion 
                 (re-search-backward "[^ \n\t\r]+\\=" nil t)))
      (slime-complete-symbol))))

(defmacro slime-with-rigid-indentation (level &rest body)
  "Execute BODY and then rigidly indent its text insertions.
Assumes all insertions are made at point."
  (let ((start (gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
         (indent-rigidly ,start (point) ,level)))))

(put 'slime-with-rigid-indentation 'lisp-indent-function 1)


;;; Inferior CL Setup: compiling and connecting to Swank

(defvar slime-connect-retry-timer nil
  "Timer object while waiting for an inferior-lisp to start.")

(defun slime ()
  "Start an inferior^_superior Lisp and connect to its Swank server."
  (interactive)
  (when (or (not (slime-bytecode-stale-p))
            (slime-urge-bytecode-recompile))
    (cond ((and current-prefix-arg
                (slime-connected-p)
                (get-buffer "*inferior-lisp*"))
           (unless (slime-maybe-rearrange-inferior-lisp)
             (slime-disconnect)))
          (t (slime-disconnect)))
    (slime-maybe-start-lisp)
    (slime-read-port-and-connect)))

(defun slime-start-and-load ()
  "Start Slime, load the current file and set the package."
  (interactive)
  (let ((package (slime-find-buffer-package)))
    (when (not package)
      (error "No package to load"))
    (lexical-let ((hook nil)
                  (package package)
                  (filename (expand-file-name (buffer-file-name))))
      (setq hook (lambda ()
                   (remove-hook 'slime-connected-hook hook)
                   (slime-load-file filename)
                   (slime-repl-set-package package)))
      (add-hook 'slime-connected-hook hook)
      (slime))))

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
  (let ((sourcefile (concat (file-name-sans-extension (locate-library "slime"))
                            ".el")))
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

(defun slime-maybe-rearrange-inferior-lisp ()
  "Offer to rename *inferior-lisp* so that another can be started."
  (when (y-or-n-p "Create an additional *inferior-lisp*? ")
    (with-current-buffer "*inferior-lisp*"
      (rename-buffer (generate-new-buffer-name (buffer-name)))
      t)))

(defun slime-maybe-start-lisp ()
  "Start an inferior lisp unless one is already running."
  (unless (get-buffer-process (get-buffer "*inferior-lisp*"))
    (call-interactively 'inferior-lisp)
    (when slime-kill-without-query-p
      (process-kill-without-query (inferior-lisp-proc)))
    (comint-send-string (inferior-lisp-proc)
                        (format "(load %S)\n"
                                (slime-to-lisp-filename
                                 (if (file-name-absolute-p slime-backend)
                                     slime-backend
                                   (concat slime-path slime-backend)))))
    (slime-maybe-start-multiprocessing)))

(defun slime-maybe-start-multiprocessing ()
  (when slime-multiprocessing
    (comint-send-string (inferior-lisp-proc)
                        "(swank:startup-multiprocessing)\n")))

(defun slime-start-swank-server ()
  "Start a Swank server on the inferior lisp."
  (comint-send-string (inferior-lisp-proc) 
                      (format "(swank:start-server %S)\n"
                                (slime-to-lisp-filename
                                 (slime-swank-port-file)))))

(defun slime-swank-port-file ()
  "Filename where the SWANK server writes its TCP port number."
  (concat (file-name-as-directory
           (cond ((fboundp 'temp-directory) (temp-directory))
                 ((boundp 'temporary-file-directory) temporary-file-directory)
                 (t "/tmp/")))
          (format "slime.%S" (emacs-pid))))

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
  (slime-read-port-and-connect-to-running-swank retries))

(defun slime-read-port-and-connect-to-running-swank (retries)
  (lexical-let ((retries (or retries slime-swank-connection-retries))
                (attempt 0))
    (labels
        ;; A small one-state machine to attempt a connection with
        ;; timer-based retries.
        ((attempt-connection
          ()
          (unless (active-minibuffer-window)
            (message "\
Polling %S.. (Abort with `M-x slime-abort-connection'.)"
                     (slime-swank-port-file)))
          (setq slime-state-name (format "[polling:%S]" (incf attempt)))
          (force-mode-line-update)
          (when slime-connect-retry-timer
            (cancel-timer slime-connect-retry-timer))
          (setq slime-connect-retry-timer nil) ; remove old timer
          (cond ((file-exists-p (slime-swank-port-file))
                 (let ((port (slime-read-swank-port)))
                   (delete-file (slime-swank-port-file))
                   (slime-connect "localhost" port)))
                ((and retries (zerop retries))
                 (message "Failed to connect to Swank."))
                (t
                 (when retries (decf retries))
                 (setq slime-connect-retry-timer
                       (run-with-timer 1 nil #'attempt-connection))))))
      (attempt-connection))))

(defun slime-connect (host port &optional kill-old-p)
  "Connect to a running Swank server"
  (interactive (list (read-from-minibuffer "Host: " "localhost")
                     (read-from-minibuffer "Port: " "4005" nil t)
                     (if (null slime-net-processes)
                         t
                       (y-or-n-p "Close old connections first? "))))
  (when kill-old-p (slime-disconnect))
  (message "Connecting to Swank on port %S.." port)
  (let* ((process (slime-net-connect host port))
         (slime-dispatching-connection process))
    (message "Initial handshake..." port)
    (slime-init-connection process)))

(defun slime-disconnect ()
  "Disconnect all connections."
  (interactive)
  (mapc #'slime-net-close slime-net-processes))

(defun slime-abort-connection ()
  "Abort connection the current connection attempt."
  (interactive)
  (if (null slime-connect-retry-timer)
      (error "Not connected.")
    (cancel-timer slime-connect-retry-timer)
    (message "Cancelled connection attempt.")))
;; FIXME: used to delete *lisp-output-stream*

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


;;; Networking

(defvar slime-net-processes nil
  "List of processes (sockets) connected to Lisps.")

(defvar slime-net-process-close-hooks '()
  "List of functions called when a slime network connection closes.
The functions are called with the process as their argument.")

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
      (set-process-coding-system proc 'no-conversion 'no-conversion))
    proc))

(defun slime-make-net-buffer (name)
  "Make a buffer suitable for a network process."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (when (fboundp 'set-buffer-multibyte)
	(set-buffer-multibyte nil))
      (buffer-disable-undo))
    buffer))

(defun slime-net-send (sexp proc)
  "Send a SEXP to Lisp over the socket PROC.
This is the lowest level of communication. The sexp will be READ and
EVAL'd by Lisp."
  (let* ((msg (concat (slime-prin1-to-string sexp) "\n"))
         (string (concat (slime-net-enc3 (length msg)) msg)))
    (process-send-string proc (string-make-unibyte string))))

(defun slime-net-close (process)
  (setq slime-net-processes (remove process slime-net-processes))
  (when (eq process slime-default-connection)
    (setq slime-default-connection nil))
  (run-hook-with-args 'slime-net-process-close-hooks process)
  (ignore-errors (kill-buffer (process-buffer process))))

(defun slime-net-sentinel (process message)
  (when (ignore-errors (eq (process-status (inferior-lisp-proc)) 'open))
    (message "Lisp connection closed unexpectedly: %s" message))
  (when (eq process slime-default-connection)
    (setq slime-state-name "[not connected]"))
  (force-mode-line-update)
  (slime-net-close process))

(defun slime-net-filter (process string)
  "Accept output from the socket and input all complete messages."
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert string))
    (slime-process-available-input)))

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
              (save-current-buffer (slime-dispatch-event event proc))))))
    (dolist (p slime-net-processes)
      (with-current-buffer (process-buffer p)
        (when (slime-net-have-input-p)
          (run-at-time 0 nil 'slime-process-available-input))))))

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


;;; Connections

;; High-level network connection management.
;; Handles multiple connections and "context-switching" between them.

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

(defvar slime-connection-counter 0
  "Number of SLIME connections made, for generating serial numbers.")

(make-variable-buffer-local
 (defvar slime-connection-number nil
   "Serial number of a connection.
Bound in the connection's process-buffer."))

(defun slime-connection ()
  "Return the connection to use for Lisp interaction."
  (let ((conn (or slime-dispatching-connection
                  slime-buffer-connection
                  slime-default-connection)))
    (cond ((null conn)
           (error "Not connected."))
          ((not (eq (process-status conn) 'open))
           (error "Connection closed."))
          (t conn))))

(defvar slime-state-name "[??]"
  "Name of the current state of `slime-default-connection'.
For display in the mode-line.")

(defmacro* slime-with-connection-buffer ((&optional process) &rest body)
  "Execute BODY in the process-buffer of PROCESS.
If PROCESS is not specified, `slime-connection' is used.

\(fn (&optional PROCESS) &body BODY))"
  `(with-current-buffer
       (process-buffer (or ,process (slime-connection)
                           (error "No connection")))
     ,@body))

(put 'slime-with-connection-buffer 'lisp-indent-function 1)

(defun slime-select-connection (process)
  (setq slime-default-connection process))

(defun slime-connection-close-hook (process)
  (when (eq process slime-default-connection)
    (when slime-net-processes
      (slime-select-connection (car slime-net-processes))
      (message "Default connection closed; switched to #%S (%S)"
               (slime-connection-number)
               (slime-connection-name)))))

(defun slime-connection-number (&optional connection)
  (slime-with-connection-buffer (connection)
    slime-connection-number))

(add-hook 'slime-net-process-close-hooks 'slime-connection-close-hook)

(defun slime-next-connection ()
  "Use the next available Swank connection.
This command is mostly intended for debugging the multi-session code."
  (interactive)
  (when (null slime-net-processes)
    (error "Not connected."))
  (let ((conn (nth (mod (1+ (or (position slime-default-connection 
                                          slime-net-processes)
                                0))
                        (length slime-net-processes))
                   slime-net-processes)))
    (slime-select-connection conn)
    (message "Selected connection #%S (%s)"
             (slime-connection-number)
             (slime-connection-name))))

(defun slime-make-default-connection ()
  "Make the current connection the default connection."
  (interactive)
  (slime-select-connection (slime-connection))
  (message "Connection #%S (%s) now default SLIME connection."
           (slime-connection-number)
           (slime-connection-name)))

(defun slime-find-connection-by-name (name)
  (find name slime-net-processes 
        :test #'string= :key #'slime-connection-name))

(defun slime-read-connection-name ()
  (let ((default (slime-connection-name)))
    (completing-read
     (format "Name (default %s): " default)
     (slime-bogus-completion-alist
      (mapcar #'slime-connection-name slime-net-processes))
     nil
     t
     nil
     nil
     default)))


;;;;; Connection-local variables

;; Variables whose values are tied to a particular connection are
;; stored as buffer-local inside the connection's process-buffer,
;; and only read/written through accessor functions.

(defmacro slime-def-connection-var (varname &rest initial-value-and-doc)
  "Define a connection-local variable.
The value of the variable can be read by calling the function of
the same name (it must not be accessed directly). The accessor
function is setf-able.

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
       (defsetf ,varname () (store)
         `(slime-with-connection-buffer ()
            (setq (\, (quote (\, real-var))) (\, store))
            (\, store)))
       '(\, varname))))

(put 'slime-def-connection-var 'lisp-indent-function 2)

(slime-def-connection-var slime-lisp-features '()
  "The symbol-names of Lisp's *FEATURES*.
This is automatically synchronized from Lisp.")

(slime-def-connection-var slime-lisp-package
    "CL-USER"
  "The current package name of the Superior lisp.
This is automatically synchronized from Lisp.")

(slime-def-connection-var slime-pid nil
  "The process id of the Lisp process.")

(slime-def-connection-var slime-lisp-implementation-type nil
  "The implementation type of the Lisp process.")

(slime-def-connection-var slime-lisp-implementation-type-name nil
  "The short name for the implementation type of the Lisp process.")

(slime-def-connection-var slime-connection-name nil
  "The short name for connection.")

(slime-def-connection-var slime-use-sigint-for-interrupt nil
  "If non-nil use a SIGINT for interrupting.")



;; XXX pending continuations are not removed if Lisp crashes.
;; Multiple sessions complicate the issue.  Better make this a
;; connection variable?
(defvar slime-rex-continuations '()
  "List of (ID . FUNCTION) continuations waiting for RPC results.")

(defvar slime-continuation-counter 0)

(make-variable-buffer-local
 (defvar slime-current-thread t
   "The id of the current thread on the Lisp side.  
t means the \"current\" thread
:repl-thread the thread to execute repl requests
fixnum a specific thread."))

(defun slime-dispatch-event (event &optional process)
  (let ((slime-dispatching-connection (or process (slime-connection))))
    (slime-log-event event)
    (destructure-case event
      ((:read-output output)
       (slime-output-string output))
      ;;
      ((:emacs-rex form package thread continuation)
       (when (and slime-rex-continuations (slime-use-sigint-for-interrupt))
         (message "; pipelined request... %S" form))
       (let ((id (incf slime-continuation-counter)))
         (push (cons id continuation) slime-rex-continuations)
         (slime-send `(:emacs-rex ,form ,package ,thread ,id))))
      ((:return value id)
       (let ((rec (assq id slime-rex-continuations)))
         (cond (rec (setq slime-rex-continuations 
                          (remove rec slime-rex-continuations))
                    (funcall (cdr rec) value))
               (t
                (error "Unexpected reply: %S %S" id value)))))
      ((:debug-activate thread level)
       (sldb-activate thread level))
      ((:debug thread level condition restarts frames)
       (sldb-setup thread level condition restarts frames))
      ((:debug-return thread level)
       (sldb-exit thread level))
      ((:emacs-interrupt thread)
       (cond ((slime-use-sigint-for-interrupt) (slime-send-sigint))
             (t (slime-send `(:emacs-interrupt ,thread)))))
      ((:read-string thread tag)
       (slime-repl-read-string thread tag))
      ((:read-aborted thread tag)
       (slime-repl-abort-read thread tag))
      ((:emacs-return-string thread tag string)
       (slime-send `(:emacs-return-string ,thread ,tag ,string)))
      ;;
      ((:new-package package)
       (setf (slime-lisp-package) package))
      ((:new-features features)
       (setf (slime-lisp-features) features))
      ((:indentation-update info)
       (slime-handle-indentation-update info))
      ((:open-dedicated-output-stream port)
       (slime-open-stream-to-lisp port))
      ((:use-sigint-for-interrupt)
       (setf (slime-use-sigint-for-interrupt) t))
      ((:%apply fn args)
       (apply (intern fn) args))
      ((:ed what)
       (slime-ed what))
      ((:debug-condition thread message)
       (apply 'ignore thread) ; XEmacs warns about unused variable
       (message "%s" message)))))

(defun slime-reset ()
  "Clear all pending continuations."
  (interactive)
  (setq slime-rex-continuations '())
  (when-let (sldb (sldb-get-buffer))
    (kill-buffer sldb)))
                        
(defun slime-nyi ()
  (error "Not yet implemented!"))


;;;;; Connection initialization

(defun slime-init-connection (proc)
  "Initialize the stack machine."
  (setq slime-rex-continuations '())
  (let ((slime-dispatching-connection proc))
    (slime-init-connection-state proc)
    (slime-select-connection proc)
    proc))

(defun slime-generate-connection-name (lisp-name)
  (loop for i from 1
        for name = lisp-name then (format "%s<%d>" lisp-name i)
        while (find name slime-net-processes 
                    :key #'slime-connection-name :test #'equal)
        finally (return name)))

(defun slime-connection-port (connection)
  "Return the remote port number of CONNECTION."
  (cond ((featurep 'xemacs)
         (car (process-id connection)))
        (t
         (cadr (process-contact connection)))))

(defun slime-init-connection-state (proc)
  ;; To make life simpler for the user: if this is the only open
  ;; connection then reset the connection counter.
  (when (equal slime-net-processes (list proc))
    (setq slime-connection-counter 0))
  (slime-with-connection-buffer ()
    (setq slime-connection-number (incf slime-connection-counter)))
  (with-lexical-bindings (proc)
    (slime-eval-async '(swank:connection-info) nil
                      (lambda (info)
                        (slime-set-connection-info proc info)))))

(defun slime-set-connection-info (connection info)
  "Initialize CONNECTION with INFO received from Lisp."
  (destructuring-bind (pid type name features) info
    (setf (slime-pid) pid
          (slime-lisp-implementation-type) type
          (slime-lisp-implementation-type-name) name
          (slime-connection-name) (slime-generate-connection-name name)
          (slime-lisp-features) features))
  (setq slime-state-name "")
  (slime-hide-inferior-lisp-buffer)
  (slime-init-output-buffer process)
  (message "Connected on port %S. %s" 
           (slime-connection-port connection)
           (slime-random-words-of-encouragement))
  (run-hooks 'slime-connected-hook))

(defun slime-hide-inferior-lisp-buffer ()
  "Display the REPL buffer instead of the *inferior-lisp* buffer."
  (let* ((buffer (get-buffer "*inferior-lisp*"))
         (window (if buffer (get-buffer-window buffer)))
         (repl (slime-output-buffer t)))
    (when buffer
      (bury-buffer buffer))
    (cond (window 
           (set-window-buffer window repl))
          ((not (get-buffer-window repl))
           (pop-to-buffer repl)))))

(defun slime-busy-p ()
  slime-rex-continuations)

(defun slime-reading-p ()
  (with-current-buffer (slime-output-buffer)
    slime-repl-read-mode))


;;;;;;; Event logging to *slime-events*

(defvar slime-log-events t
  "*Log protocol events to the *slime-events* buffer.")

(defvar slime-inhibit-outline-mode-in-events-buffer t
  "*Don't use outline-mode if true.")

(defun slime-pprint-event (object buffer)
  "Pretty print OBJECT in BUFFER with limited depth and width."
  (let ((print-length 20)
	(print-level 6)
	(pp-escape-newlines t))
    (pp event buffer)))

(defun slime-log-event (event)
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

(defun slime-events-buffer ()
  (or (get-buffer slime-event-buffer-name)
      (let ((buffer (get-buffer-create slime-event-buffer-name)))
        (with-current-buffer buffer
          (set (make-local-variable 'outline-regexp) "^(")
          (set (make-local-variable 'comment-start) ";")
          (set (make-local-variable 'comment-end) "")
          (unless slime-inhibit-outline-mode-in-events-buffer
            (outline-minor-mode)))
        buffer)))


;;;;; Utilities

(defun slime-check-connected ()
  (unless (slime-connected-p)
    (error "Not connected. Use `%s' to start a Lisp."
           (substitute-command-keys "\\[slime]"))))

(defun slime-connected-p ()
  "Return true if the Swank connection is open."
  (not (null slime-net-processes)))

(defconst +slime-sigint+ 2)

(defun slime-send-sigint ()
  (interactive)
  (signal-process (slime-pid) +slime-sigint+))


;;;;; Emacs Lisp programming interface

(defmacro* slime-rex ((&rest saved-vars)
                      (sexp &optional 
                            (package 'slime-buffer-package)
                            (thread 'slime-current-thread))
                      &rest continuations)
  "(slime-rex (VAR ...) (SEXP &optional PACKAGE THREAD) CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each
VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the princed version is sent to Lisp.

PACKAGE is evaluated and Lisp binds *BUFFER-PACKAGE* to this package.
The default value is `slime-buffer-package'.

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

(defvar slime-stack-eval-tags nil
  "List of stack-tags of continuations waiting on the stack.")

(defun slime-eval (sexp &optional package)
  "Evaluate EXPR on the superior Lisp and return the result."
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

(defun slime-eval-async (sexp package cont)
  "Evaluate EXPR on the superior Lisp and call CONT with the result."
  (slime-rex (cont)
      (sexp package)
    ((:ok result) 
     (funcall cont result))
    ((:abort) 
     (message "Evaluation aborted."))))

(defun slime-send (sexp)
  (slime-net-send sexp (slime-connection)))

(defun slime-sync ()
  "Block until the most recent request has finished."
  (when slime-rex-continuations
    (let ((tag (caar slime-rex-continuations)))
      (while (find tag slime-rex-continuations :key #'car)
        (accept-process-output nil 0 100000)))))

(defun slime-ping ()
  "Check that communication works."
  (interactive)
  (message "%s" (slime-eval "PONG")))


;;; Stream output

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
         (use-header-p (and (boundp 'header-line-format) 
                            slime-reply-update-banner-p))
         ;; and dancing text
         (animantep (and (fboundp 'animate-string)
                         slime-startup-animation
                         (zerop (buffer-size)))))
    (when use-header-p
      (setq header-line-format banner))
    (when animantep
      (pop-to-buffer (current-buffer))
      (animate-string "; SLIME: The Superior Lisp Interaction Mode for Emacs"
                      0 0))
    (slime-repl-insert-prompt (if (or (not slime-reply-update-banner-p)
                                      use-header-p)
                                  ""
                                (concat "; " banner)))))

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
  (when (and (not (get-buffer-window (current-buffer) t))
             (< start end))
    (display-buffer (current-buffer))))
      
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
      (display-buffer (current-buffer) t))))

(defsetf marker-insertion-type set-marker-insertion-type)

(defmacro slime-with-output-end-mark (&rest body)
  "Execute BODY at `slime-output-end'.  

If point is initially at `slime-output-end' and the buffer is visible
update window-point afterwards.  If point is initially not at
`slime-output-end, execute body inside a `save-excursion' block."
  `(progn
     (cond ((= (point) slime-output-end) 
            (let ((start (point)))
              ;; XXX Assertion is currently easy to break, by typeing
              ;; input while we're waiting for output
              ;;(assert (<= (point) slime-repl-input-start-mark))
              ,@body
              (when-let (w (get-buffer-window (current-buffer) t))
                (set-window-point w (point)))
              (when (= start slime-repl-input-start-mark) 
                (set-marker slime-repl-input-start-mark (point)))))
           (t 
            (save-excursion 
              (goto-char slime-output-end)
              ;;(assert (<= (point) slime-repl-input-start-mark))
              ,@body)))))

(defun slime-output-filter (process string)
  (when (and (slime-connected-p)
             (plusp (length string)))
    (slime-output-string string)))

(defun slime-open-stream-to-lisp (port)
  (let ((stream (open-network-stream "*lisp-output-stream*" 
                                     (slime-with-connection-buffer ()
                                       (current-buffer))
				     "localhost" port)))
    (when slime-kill-without-query-p
      (process-kill-without-query stream))
    (set-process-filter stream 'slime-output-filter)
    stream))

(defun slime-output-string (string)
  (with-current-buffer (slime-output-buffer)
    (slime-with-output-end-mark
     (slime-insert-propertized
      (list 'face 'slime-repl-output-face) 
      string)
     (when (and (= (point) slime-repl-prompt-start-mark)
                (not (bolp)))
       (insert "\n")
       (set-marker slime-output-end (1- (point)))))))

(defun slime-switch-to-output-buffer (&optional select-connection)
  "Select the output buffer, preferably in a different window."
  (interactive "P")
  (slime-with-chosen-connection (select-connection)
    (set-buffer (slime-output-buffer))
    (unless (eq (current-buffer) (window-buffer))
      (pop-to-buffer (current-buffer) t))
    (goto-char (point-max))))


;;; REPL
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

;; Small helper.
(defun slime-make-variables-buffer-local (&rest variables)
  (mapcar #'make-variable-buffer-local variables))

(slime-make-variables-buffer-local
 ;; Local variables in the REPL buffer.
 (defvar slime-repl-input-history '()
   "History list of strings read from the REPL buffer.")
 
 (defvar slime-repl-input-history-position 0)

 (defvar slime-repl-prompt-start-mark)
 (defvar slime-repl-input-start-mark)
 (defvar slime-repl-input-end-mark)
 (defvar slime-repl-last-input-start-mark))

(defcustom slime-repl-shortcut-dispatch-char ?\,
  "Character used to distinguish repl commands from lisp forms."
  :type '(character)
  :group 'slime)

(defvar slime-repl-mode-map)

(defun slime-repl-buffer (&optional create)
  "Get the REPL buffer for the current connection; optionally create."
  (funcall (if create #'get-buffer-create #'get-buffer)
           (format "*slime-repl[%S]*" (slime-connection-number))))

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
  ;;(set (make-local-variable 'scroll-conservatively) 20)
  ;;(set (make-local-variable 'scroll-margin) 0)
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
    (let ((prompt-start (point)))
      (slime-propertize-region
          '(face slime-repl-prompt-face
                 read-only t
                 intangible t
                 slime-repl-prompt t
                 ;; emacs stuff
                 rear-nonsticky (slime-repl-prompt read-only face intangible)
                 ;; xemacs stuff
                 start-open t end-open t)
        (insert (slime-lisp-package) "> "))
      (set-marker slime-output-end start)
      (set-marker slime-repl-prompt-start-mark prompt-start)
      (slime-mark-input-start)
      (let ((time (or time 0.2)))
        (cond ((zerop time)
               (slime-repl-move-output-mark-before-prompt (current-buffer)))
              (t 
               (run-at-time time nil 'slime-repl-move-output-mark-before-prompt
                            (current-buffer))))))))

(defun slime-repl-move-output-mark-before-prompt (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion 
        (goto-char slime-repl-prompt-start-mark)
        (slime-mark-output-start)))))

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
     (unless (bolp) (insert "\n"))
     (insert "; Evaluation aborted\n"))
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

(defun slime-repl-return ()
  "Evaluate the current input string, or insert a newline.  
Send the current input ony if a whole expression has been entered,
i.e. the parenthesis are matched. 

With prefix argument send the input even if the parenthesis are not
balanced."
  (interactive)
  (slime-check-connected)
  (assert (<= (point) slime-repl-input-end-mark))
  (cond ((get-text-property (point) 'slime-repl-old-input)
         (slime-repl-grab-old-input))
        (current-prefix-arg
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
  (let ((input (slime-repl-current-input)))
    (goto-char slime-repl-input-end-mark)
    (when newline (insert "\n"))
    (add-text-properties slime-repl-input-start-mark (point)
                         '(face slime-repl-input-face
                                rear-nonsticky (face)
                                slime-repl-old-input t))
    (slime-mark-input-start)
    (slime-mark-output-start)
    (slime-repl-send-string input)))

(defun slime-repl-grab-old-input ()
  "Resend the old REPL input at point.
The old input has the text property `slime-repl-old-input'."
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
           (end (next-single-char-property-change (point) prop))
           (old-input (buffer-substring beg end)))
      (goto-char slime-repl-input-start-mark)
      (delete-region (point) slime-repl-input-end-mark)
      (insert old-input)
      (while (eq (char-before) ?\n)
        (delete-char -1)))))

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
  (interactive)
  (set-marker slime-repl-last-input-start-mark nil)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (slime-repl-input-line-beginning-position))))

(defun slime-repl-clear-output ()
  (interactive)
  (let ((start (save-excursion 
                 (slime-repl-previous-prompt)
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
      (destructuring-bind (name nickname)
          (slime-eval `(swank:set-package ,package))
        (setf (slime-lisp-package) nickname)
        (slime-repl-insert-prompt "" 0)
        (insert unfinished-input)))))


;;;;; History

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
  ("\C-e" 'slime-repl-eol)
  ("\M-p" 'slime-repl-previous-input)
  ("\M-n" 'slime-repl-next-input)
  ("\M-r" 'slime-repl-previous-matching-input)
  ("\M-s" 'slime-repl-next-matching-input)
  ("\C-c\C-c" 'slime-interrupt)
  ("\C-c\C-g" 'slime-interrupt)
  ("\C-c:"    'slime-interactive-eval)
  ("\C-c\C-e" 'slime-interactive-eval)
  ;("\t"   'slime-complete-symbol)
  ("\t"   'slime-indent-and-complete-symbol)
  (" "    'slime-space)
  ("\C-\M-x" 'slime-eval-defun)
  ("\C-c\C-o" 'slime-repl-clear-output)
  ("\C-c\C-t" 'slime-repl-clear-buffer)
  ("\C-c\C-n" 'slime-repl-next-prompt)
  ("\C-c\C-p" 'slime-repl-previous-prompt)
  ("\M-\C-a" 'slime-repl-beginning-of-defun)
  ("\M-\C-e" 'slime-repl-end-of-defun)
  )

(define-key slime-repl-mode-map
  (string slime-repl-shortcut-dispatch-char) 'slime-handle-repl-shortcut)

(define-minor-mode slime-repl-read-mode 
  "Mode the read input from Emacs
\\{slime-repl-read-mode-map}"
  nil
  "[read]"
  '(("\C-m" . slime-repl-return)
    ("\C-c\C-b" . slime-repl-read-break)
    ("\C-c\C-c" . slime-repl-read-break)
    ("\C-c\C-g" . slime-repl-read-break)))

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

(defun slime-repl-return-string (string)
  (assert (plusp (length string)))
  (slime-dispatch-event `(:emacs-return-string 
                          ,(pop slime-read-string-threads)
                          ,(pop slime-read-string-tags)
                          ,string))
  (slime-repl-read-mode -1))

(defun slime-repl-read-break ()
  (interactive)
  (slime-eval-async `(cl:break) nil (lambda (_))))

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
  (slime-with-output-to-temp-buffer "*slime-repl-help*" nil
    (let ((table (sort* slime-repl-shortcut-table #'string<
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
  (:one-liner "Change the current directory."))

(defslime-repl-shortcut slime-repl-push-directory ("push-directory" "+d" 
                                                   "pushd")
  (:handler (lambda (directory)
              (interactive
               (list (read-directory-name 
                      "Push directory: "
                      (slime-eval '(swank:default-directory)) nil nil ""))
               (push directory slime-repl-directory-stack)
               (slime-set-default-directory directory))))
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

(defslime-repl-shortcut slime-repl-sayoonara ("sayoonara" "quit")
  (:handler (lambda ()
              (interactive)
              (when (slime-connected-p)
                (slime-eval-async '(swank:quit-lisp) nil (lambda (_) nil)))
              (slime-kill-all-buffers)))
  (:one-liner "Quit the lisp and close all SLIME buffers."))

(defslime-repl-shortcut slime-repl-defparameter ("defparameter" "!")
  (:handler (lambda (name value)
              (interactive (list (slime-read-symbol-name "Name (symbol): " t)
                                 (slime-read-from-minibuffer "Value: " "nil")))
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
               nil
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


;;;;; Cleanup after a quit

(defun slime-kill-all-buffers ()
  "Kill all the slime related buffers. This is only used by the
  repl command sayoonara."
  (dolist (buf (buffer-list))
    (when (or (member (buffer-name buf) '("*inferior-lisp*" 
                                          slime-event-buffer-name))
              (string-match "^\\*slime-repl\\[[0-9]+\\]\\*$" (buffer-name buf))
              (string-match "^\\*sldb .*\\*$" (buffer-name buf)))
      (kill-buffer buf))))


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


;;; Filename translation

(defun slime-to-lisp-filename (filename)
  "Translate the string FILENAME to a Lisp filename.
See `slime-translate-to-lisp-filename-function'."
  (funcall slime-translate-to-lisp-filename-function filename))

(defun slime-from-lisp-filename (filename)
  "Translate the Lisp filename FILENAME to an Emacs filename.
See `slime-translate-from-lisp-filename-function'."
  (funcall slime-translate-from-lisp-filename-function filename))


;;; Compilation and the creation of compiler-note annotations

(defun slime-compile-and-load-file ()
  "Compile and load the buffer's file and highlight compiler notes.

Each source location that is the subject of a compiler note is
underlined and annotated with the relevant information. The commands
`slime-next-note' and `slime-previous-note' can be used to navigate
between compiler notes and to display their full details."
  (interactive)
  (slime-with-chosen-connection ()
    (slime-compile-file t)))

(defun slime-compile-file (&optional load)
  "Compile current buffer's file and highlight resulting compiler notes.

See `slime-compile-and-load-file' for further details."
  (interactive)
  (unless (eq major-mode 'lisp-mode)
    (error "Only valid in lisp-mode"))
  (unless buffer-file-name
    (error "Buffer %s is not associated with a file." (buffer-name)))
  (when (and (buffer-modified-p)
             (y-or-n-p (format "Save file %s? " (buffer-file-name))))
    (save-buffer))
  (let ((lisp-filename (slime-to-lisp-filename (buffer-file-name))))
    (slime-insert-transcript-delimiter
     (format "Compile file %s" lisp-filename))
    (slime-display-output-buffer)
    (slime-eval-async
     `(swank:compile-file-for-emacs ,lisp-filename ,(if load t nil))
     nil
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
   nil
   (slime-compilation-finished-continuation)))

(defun slime-compile-defun ()
  "Compile the current toplevel form."
  (interactive)
  (slime-with-chosen-connection ()
    (slime-compile-string
     (slime-defun-at-point)
     (save-excursion 
       (end-of-defun)
       (beginning-of-defun)
       (point)))))

(defun slime-compile-region (start end)
  "Compile the region."
  (interactive "r")
  (slime-with-chosen-connection ()
    (slime-compile-string (buffer-substring-no-properties start end) start)))

(defun slime-compile-string (string start-offset)
  (slime-eval-async 
   `(swank:compile-string-for-emacs ,string ,(buffer-name) ,start-offset)
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
         (new-message (mapconcat #'slime-note.message notes "\n")))
    (let ((new-note (copy-list (car notes))))
      (setf (getf new-note :message) new-message)
      (setf (getf new-note :severity) new-severity)
      new-note)))

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
       xrefs 'definition "Compiler notes" (slime-buffer-package)))))

(defun slime-note-has-location-p (note)
  (not (eq ':error (car (slime-note.location note)))))

(defun slime-maybe-list-compiler-notes (notes)
  "Show the compiler notes if appropriate.
Useful value for `slime-compilation-finished-hook'"
  (unless (or (null notes)
	      (and (eq last-command 'slime-compile-defun)
                   (every #'slime-note-has-location-p notes)))
    (slime-list-compiler-notes notes)))

(defun slime-list-compiler-notes (&optional notes)
  "Show the compiler notes NOTES in tree view."
  (interactive)
  (let ((notes (or notes (slime-compiler-notes)))
        ;; We have to grab the window configuration before switching
        ;; buffers in XEmacs.
        (window-config (current-window-configuration)))
    (with-current-buffer (get-buffer-create "*compiler notes*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when (null notes)
          (insert "[no notes]"))
        (dolist (tree (slime-compiler-notes-to-tree notes))
          (slime-tree-insert tree "")
          (insert "\n")))
      (slime-compiler-notes-mode)
      (setq buffer-read-only t)
      (make-local-variable 'slime-compiler-notes-saved-window-configuration)
      (setq slime-compiler-notes-saved-window-configuration
            window-config)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

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
    alist))

(defun slime-note.severity (note)
  (plist-get note :severity))

(defun slime-note.message (note)
  (plist-get note :message))

(defun slime-note.short-message (note)
  (or (plist-get note :short-message)
      (plist-get note :message)))

(defun slime-note.location (note)
  (plist-get note :location))

(defun slime-severity-label (severity)
  (ecase severity
    (:note "Notes")
    (:warning "Warnings")
    (:error "Errors")
    (:style-warning "Style Warnings")))

(defun slime-tree-for-note (note)
  (make-slime-tree :item (slime-note.short-message note)
                   :plist (list 'note note)))

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
  "\\<slime-compiler-notes-mode-map>
\\{slime-compiler-notes-mode-map}"
  (slime-set-truncate-lines))

(slime-define-keys slime-compiler-notes-mode-map
  ((kbd "RET") 'slime-compiler-notes-show-details)
  ([mouse-2] 'slime-compiler-notes-show-details/mouse)
  ("q" 'slime-compiler-notes-quit))

(defun slime-compiler-notes-quit ()
  (interactive)
  (let ((config slime-compiler-notes-saved-window-configuration))
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

(defun slime-compiler-notes-show-details/mouse (event)
  (interactive "e")
  (destructuring-bind (mouse-2 (w pos &rest _) &rest __) event
    (goto-char pos)
    (slime-compiler-notes-show-details)))
          

;;;;;;; Tree Widget

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
      (insert prefix)
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
  (let ((location (getf note :location)))
    (unless (eq (car location) :error) 
      (slime-goto-source-location location)
      (let ((start (point)))
        (ignore-errors (slime-forward-sexp))
        (if (slime-same-line-p start (point))
            (values start (point))
            (values (1+ start)
                    (progn (goto-char (1+ start))
                           (forward-sexp 1)
                           (point))))))))

(defun slime-same-line-p (pos1 pos2)
  "Return t if buffer positions POS1 and POS2 are on the same line."
    (save-excursion (goto-char (min pos1 pos2))
                    (<= (max pos1 pos2) (line-end-position))))

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
         (format "\\s *(def\\(\\s_\\|\\sw\\)*\\s +%s\\>" name) nil t)
        (re-search-forward 
         (format "[( \t]%s\\>\\(\\s \\|$\\)" name) nil t)))
     (goto-char (match-beginning 0)))
    ((:source-path source-path start-position)
     (cond (start-position
            (goto-char start-position)
            (slime-forward-positioned-source-path source-path))
           (t
            (slime-forward-source-path source-path))))))

(defun slime-goto-source-location (location &optional noerror)
  "Move to the source location LOCATION.  Several kinds of locations
are supported:

<location> ::= (:location <buffer> <position>)
             | (:error <message>) 

<buffer>   ::= (:file <filename>)
             | (:buffer <buffername>)
             | (:source-form <string>)

<position> ::= (:position <fixnum> [<align>]) ; 1 based
             | (:line <fixnum> [<fixnum>])
             | (:function-name <string>)
             | (:source-path <list> <start-position>) "
  (destructure-case location
    ((:location buffer position hints)
     (slime-goto-location-buffer buffer)
     (slime-goto-location-position position)
     (when-let (snippet (getf hints :snippet))
       (slime-isearch snippet)))
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
    (funcall (ecase (car e)
               (and #'every)
               (or  #'some)
               (not (lambda (f l) (not (apply f l)))))
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


;;; Arglist Display

(defun slime-space (n)
  "Insert a space and print some relevant information (function arglist).
Designed to be bound to the SPC key.  Prefix argument can be used to insert
more than one space."
  (interactive "p")
  (self-insert-command n)
  (when (and slime-space-information-p
             (slime-connected-p)
	     (or (not (slime-busy-p))
                 ;; XXX should we enable this?
                 ;; (not slime-use-sigint-for-interrupt))
                 ))
    (let ((names (slime-enclosing-operator-names)))
      (when names
        (slime-eval-async 
         `(swank:arglist-for-echo-area (quote ,names))
         (slime-buffer-package)
         (lambda (message)
           (slime-background-message "%s" message)))))))

(defun slime-arglist (name)
  "Show the argument list for NAME."
  (interactive (list (slime-read-symbol-name "Arglist of: ")))
  (slime-eval-async 
   `(swank:arglist-for-echo-area (quote (,name)))
   (slime-buffer-package)
   (lambda (arglist)
     (message "%s" arglist))))

(defun slime-insert-arglist (name)
  "Insert the argument list for NAME behind the symbol point is
currently looking at."
  (interactive (list (slime-read-symbol-name "Arglist of: ")))
  (insert (slime-eval `(swank:arglist-for-insertion ',name)
                      (slime-buffer-package))))

(defun slime-get-arglist (symbol-name)
  "Return the argument list for SYMBOL-NAME."
  (slime-eval `(swank:arglist-for-echo-area (quote (,symbol-name)))))


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
  (when-let (sym (slime-function-called-at-point/line))
    (let ((name (symbol-name sym))
          (cache-key (slime-qualify-cl-symbol-name sym)))
      (or (when-let (documentation (slime-get-cached-autodoc cache-key))
            (slime-background-message "%s" documentation)
            t)
          ;; Asynchronously fetch, cache, and display arglist
          (slime-eval-async
           `(swank:arglist-for-echo-area (quote (,name)))
           (slime-buffer-package)
           (with-lexical-bindings (cache-key name)
             (lambda (arglist)
               ;; FIXME: better detection of "no documentation available"
               (if (string-match "<not available>" arglist)
                   (setq arglist ""))
               (slime-update-autodoc-cache cache-key arglist)
               (slime-background-message "%s" arglist))))))))

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
       (slime-connected-p)
       (not (slime-busy-p))))


;;; Typeout frame

;; When a "typeout frame" exists it is used to display certain
;; messages instead of the echo area or pop-up windows.

(defvar slime-typeout-window nil
  "The current typeout window.")

(defvar slime-typeout-frame-properties
  '((width . 40) (height . 10) (minibuffer . nil)
    (left . -10) (top . 10) (name . "SLIME Typeout"))
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


;;; Completion

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
    (run-at-time
     0 nil
     (lambda ()
       (save-excursion
         (set-window-configuration
          slime-complete-saved-window-configuration))
       (setq slime-complete-saved-window-configuration nil)
       (slime-close-buffer slime-completions-buffer-name)))))

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
      (display-completion-list completion-set)
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
    (return-from slime-complete-symbol* (comint-dynamic-complete-as-filename)))
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
         (cons (lexical-let ((package (slime-buffer-package))
                             (connection (slime-connection)))
                 (lambda ()
                   (setq slime-buffer-package package)
                   (setq slime-buffer-connection connection)
                   (set-syntax-table lisp-mode-syntax-table)))
	       minibuffer-setup-hook)))
    (read-from-minibuffer prompt initial-value slime-read-expression-map
			  nil 'slime-read-expression-history)))

(defun slime-symbol-start-pos ()
  "Return the starting position of the symbol under point.
The result is unspecified if there isn't a symbol under the point."
  (save-excursion
    (skip-syntax-backward "w_") 
    (point)))

(defun slime-symbol-end-pos ()
  (save-excursion
    (skip-syntax-forward "w_")
    (point)))

(defun slime-bogus-completion-alist (list)
  "Make an alist out of list.
The same elements go in the CAR, and nil in the CDR. To support the
apparently very stupid `try-completions' interface, that wants an
alist but ignores CDRs."
  (mapcar (lambda (x) (cons x nil)) list))

(defun slime-completions (prefix)
  (slime-eval `(swank:completions ,prefix 
                                  ,(or (slime-find-buffer-package)
                                       (slime-buffer-package)))))

(defun slime-simple-completions (prefix)
  (slime-eval `(swank:simple-completions ,prefix 
                                         ,(or (slime-find-buffer-package)
                                              (slime-buffer-package)))))


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

(defun slime-push-definition-stack ()
  "Add MARKER to the edit-definition history stack.
If MARKER is nil, use the point."
  (ring-insert-at-beginning slime-find-definition-history-ring (point-marker)))

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

(defun slime-edit-definition (name &optional other-window)
  "Lookup the definition of the symbol at point.  
If there's no symbol at point, or a prefix argument is given, then the
function name is prompted."
  (interactive (list (slime-read-symbol-name "Symbol: ")))
  (let ((definitions (slime-eval `(swank:find-definitions-for-emacs ,name)
                                 (slime-buffer-package))))
    (if (null definitions)
        (if slime-edit-definition-fallback-function
            (funcall slime-edit-definition-fallback-function name)
          (error "No known definition for: %s" name))
      (slime-push-definition-stack)
      (cond ((slime-length> definitions 1)
             (slime-show-definitions name definitions))
            (t
             (slime-goto-source-location (slime-definition.location
                                          (car definitions)))
             (cond ((not other-window)
                    (switch-to-buffer (current-buffer)))
                   (t
                    (switch-to-buffer-other-window (current-buffer)))))))))

(defun slime-edit-definition-other-window (name)
  "Like `slime-edit-definition' but switch to the other window."
  (interactive (list (slime-read-symbol-name "Function name: ")))
  (slime-edit-definition name t))

(defun slime-show-definitions (name definitions)
  (slime-show-xrefs 
   `((,name . ,(loop for (dspec location) in definitions
                     collect (cons dspec location))))
   'definition
   name
   (slime-buffer-package)))


;;; `ED'

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
        (setq slime-ed-frame (new-frame)))
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


;;; Interactive evaluation.

(defun slime-eval-with-transcript (form package &optional fn)
  "Send FROM and PACKAGE to Lisp and pass the result to FN.
Display the result in the message area, if FN is nil.
Show the output buffer if the evaluation causes any output."
  (with-current-buffer (slime-output-buffer)
    (slime-with-output-end-mark (slime-mark-output-start)))
  (with-lexical-bindings (fn)
    (slime-eval-async form package 
                      (lambda (value)
                        (with-current-buffer (slime-output-buffer)
                          (cond (fn (funcall fn value))
                                (t (message "%s" value)))
                          (slime-show-last-output))))))

(defun slime-eval-describe (form)
  "Evalute FORM in Lisp and display the result in a new buffer."
  (lexical-let ((package (slime-buffer-package)))
    (slime-eval-with-transcript
     form package
     (lambda (string) (slime-show-description string package)))))

(defun slime-insert-transcript-delimiter (string)
  (with-current-buffer (slime-output-buffer)
    (slime-with-output-end-mark
     (unless (bolp) (insert "\n"))
     (slime-insert-propertized
      '(slime-transcript-delimiter t)
      ";;;; " (subst-char-in-string ?\n ?\040
                                    (substring string 0 
                                               (min 60 (length string))))
      " ...\n"))))

(defun slime-interactive-eval (string)
  "Read and evaluate STRING and print value in minibuffer. "
  (interactive (list (slime-read-from-minibuffer "Slime Eval: ")))
  (slime-insert-transcript-delimiter string)
  (slime-eval-with-transcript `(swank:interactive-eval ,string)
                              (slime-buffer-package t)))

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
  "Evalute region."
  (interactive "r")
  (slime-eval-with-transcript
   `(swank:interactive-eval-region ,(buffer-substring-no-properties start end))
   (slime-buffer-package)))

(defun slime-eval-buffer ()
  "Evalute the current buffer.
The value is printed in the echo area."
  (interactive)
  (slime-eval-region (point-min) (point-max)))

(defun slime-re-evaluate-defvar (form)
  "Force the re-evaluaton of the defvar form before point.  

First make the variable unbound, then evaluate the entire form."
  (interactive (list (slime-last-expression)))
  (slime-eval-with-transcript `(swank:re-evaluate-defvar ,form)
                              (slime-buffer-package)))

(defun slime-pprint-eval-last-expression ()
  "Evalute the form before point; pprint the value in a buffer."
  (interactive)
  (slime-eval-describe `(swank:pprint-eval ,(slime-last-expression))))

(defun slime-eval-print-last-expression (string)
  "Evalute sexp before point; print value into the current buffer"
  (interactive (list (slime-last-expression)))
  (lexical-let ((buffer (current-buffer)))
    (slime-eval-with-transcript    
     `(swank:interactive-eval ,string)
     (slime-buffer-package t)
     (lambda (result) (with-current-buffer buffer
                        (slime-show-last-output)
                        (insert "\n"
                                (format "%s" result)
                                "\n"))))))

(defun slime-eval/compile-defun-dwim (&optional arg)
  "Call the computation command you want (Do What I Mean).
Look at defun and determine whether to call `slime-eval-defun' or
`slime-compile-defun'.

A prefix of `-' forces evaluation, any other prefix forces
compilation."
  (interactive "P")
  (case arg
    ;; prefix is `-', evaluate defun
    ((-) (slime-eval-defun))
    ;; no prefix, automatically determine action
    ((nil) (let ((form (slime-defun-at-point)))
             (cond ((string-match "^(defvar " form)
                    (slime-re-evaluate-defvar form))
                   ((string-match "^(def" form)
                    (slime-compile-defun))
                   (t
                    (slime-eval-defun)))))
    ;; prefix is not `-', compile defun
    (otherwise (slime-compile-defun))))

(defun slime-toggle-trace-fdefinition (fname-string)
  "Toggle trace for FNAME-STRING."
  (interactive (list (slime-read-from-minibuffer
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

(defun slime-undefine-function (symbol-name)
  "Unbind the function slot of SYMBOL-NAME."
  (interactive (list (slime-read-symbol-name "fmakunbound: ")))
  (slime-eval-async `(swank:undefine-function ,symbol-name)
                    (slime-buffer-package t)
                    (lambda (result) (message "%s" result))))

(defun slime-load-file (filename)
  "Load the Lisp file FILENAME."
  (interactive (list 
		(read-file-name "Load file: " nil nil
				nil (file-name-sans-extension
				     (file-name-nondirectory 
				      (buffer-file-name))))))
  (let ((lisp-filename (slime-to-lisp-filename (expand-file-name filename))))
    (slime-eval-with-transcript `(swank:load-file ,lisp-filename) nil)))


;;; Profiling

(defun slime-toggle-profile-fdefinition (fname-string)
  "Toggle profiling for FNAME-STRING."
  (interactive (list (slime-read-from-minibuffer
		      "(Un)Profile: " (slime-symbol-name-at-point))))
  (slime-eval-async `(swank:toggle-profile-fdefinition ,fname-string)
                    (slime-buffer-package t)
                    (lambda (r) (message "%s" r))))

(defun slime-unprofile-all ()
  "Unprofile all functions."
  (interactive)
  (slime-eval-async '(swank:unprofile-all) (slime-buffer-package t)
                    (lambda (r) (message "%s" r))))

(defun slime-profile-report ()
  "Print profile report."
  (interactive)
  (slime-eval-with-transcript '(swank:profile-report) nil))

(defun slime-profile-reset ()
  "Reset profile counters."
  (interactive)
  (slime-eval-async (slime-eval `(swank:profile-reset)) nil 
                    (lambda (r) (message "%s" r))))

(defun slime-profiled-functions ()
  "Return list of names of currently profiled functions."
  (interactive)
  (slime-eval-async `(swank:profiled-functions) nil
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
  (slime-eval-async `(swank:profile-package ,package ,callers ,methods) nil
                    (lambda (r) (message "%s" r))))



;;; Documentation

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
  (slime-with-output-to-temp-buffer "*SLIME Description*" package (princ string)))

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

(defun slime-apropos-summary (case-sensitive-p package only-external-p)
  "Return a short description for the performed apropos search."
  (concat (if case-sensitive-p "Case-sensitive " "")
          "Apropos for "
          (format "%S" string)
          (if package (format " in package %S" package) "")
          (if only-external-p " (external symbols only)" "")))

(defun slime-apropos (string &optional only-external-p package 
                             case-sensitive-p)
  (interactive
   (if current-prefix-arg
       (list (read-string "SLIME Apropos: ")
             (y-or-n-p "External symbols only? ")
             (let ((pkg (slime-read-package-name "Package: ")))
               (if (string= pkg "") nil pkg))
             (y-or-n-p "Case-sensitive? "))
     (list (read-string "SLIME Apropos: ") t nil nil)))
  (let ((buffer-package (or package (slime-buffer-package t))))
    (slime-eval-async
     `(swank:apropos-list-for-emacs ,string ,only-external-p
                                    ,case-sensitive-p ,package)
     buffer-package
     (lexical-let ((string string)
                   (package (or package buffer-package))
                   (summary (slime-apropos-summary case-sensitive-p package
                                                   only-external-p)))
       (lambda (r) (slime-show-apropos r string package summary))))))

(defun slime-apropos-all ()
  "Shortcut for (slime-apropos <pattern> nil nil)"
  (interactive)
  (slime-apropos (read-string "SLIME Apropos: ") nil nil))

(defun slime-apropos-package (package &optional internal)
  "Show apropos listing for symbols in PACKAGE.
With prefix argument include internal symbols."
  (interactive (list (let ((pkg (slime-read-package-name "Package: ")))
                       (if (string= pkg "") (slime-buffer-package t) pkg))
                     current-prefix-arg))
  (slime-apropos "" (not internal) package))

(defun slime-show-apropos (plists string package summary)
  (if (null plists)
      (message "No apropos matches for %S" string)
    (slime-with-output-to-temp-buffer "*SLIME Apropos*" package
      (apropos-mode)
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


;;; XREF: cross-referencing

(defvar slime-xref-mode-map)
(defvar slime-xref-saved-window-configuration nil
  "Buffer local variable in xref windows.")

(define-derived-mode slime-xref-mode lisp-mode "xref"
  "\\<slime-xref-mode-map>
\\{slime-xref-mode-map}"
  (setq font-lock-defaults nil)
  (slime-mode -1))

(slime-define-keys slime-xref-mode-map 
  ((kbd "RET") 'slime-show-xref)
  ("\C-m" 'slime-show-xref)
  (" " 'slime-goto-xref)
  ("q" 'slime-xref-quit)
  ("n" 'slime-next-line/not-add-newlines)
  ("p" 'previous-line)
  )

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
                 "  " label "\n"))))
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
   (slime-buffer-package t)
   (lexical-let ((type type)
                 (symbol symbol)
                 (package (slime-buffer-package)))
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
  

;;; Macroexpansion

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

(defun slime-ir1-expand ()
  "Display the ir1 form of the sexp at point."
  (interactive)
  (slime-eval-macroexpand 'swank:print-ir1-converted-blocks))


;;; Subprocess control

(defun slime-interrupt ()
  (interactive)
  (slime-dispatch-event `(:emacs-interrupt ,slime-current-thread)))

(defun slime-quit ()
  (interactive)
  (if (slime-busy-p)
      (slime-dispatch-event '(:emacs-quit))
    (error "Not evaluating - nothing to quit.")))

(defun slime-set-package (package)
  (interactive (list (slime-read-package-name "Package: " 
					      (slime-find-buffer-package))))
  (message "*package*: %s" (slime-eval `(swank:set-package ,package))))

(defun slime-set-default-directory (directory)
  (interactive (list (read-directory-name "Directory: " nil nil t)))
  (message "default-directory: %s" 
	   (slime-eval `(swank:set-default-directory 
			 ,(expand-file-name directory))))
  (with-current-buffer (slime-output-buffer)
    (setq default-directory (expand-file-name directory))
    (when (boundp 'header-line-format)
      (slime-repl-update-banner))))

(defun slime-sync-package-and-default-directory ()
  (interactive)
  (let ((package (slime-eval `(swank:set-package 
			       ,(slime-find-buffer-package))))
	(directory (slime-eval `(swank:set-default-directory 
				 ,(expand-file-name default-directory)))))
    (message "package: %s  default-directory: %s" package directory)))
	

;;; Debugger (SLDB)

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
 (defvar sldb-restarts nil
   "List of (NAME DESCRIPTION) for each available restart."))

(make-variable-buffer-local
 (defvar sldb-level nil
   "Current debug level (recursion depth) displayed in buffer."))

(make-variable-buffer-local
 (defvar sldb-saved-window-configuration nil
   "Window configuration before the debugger was initially entered."))

(make-variable-buffer-local
 (defvar sldb-backtrace-start-marker nil
   "Marker placed at the beginning of the backtrace text."))
   

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
  ("l"    'sldb-list-locals)
  ("t"    'sldb-toggle-details)
  ("r"    'sldb-restart-frame)
  ("R"    'sldb-return-from-frame)
  ("c"    'sldb-continue)
  ("s"    'sldb-step)
  ("a"    'sldb-abort)
  ("q"    'sldb-quit)
  ("B"    'sldb-break-with-default-debugger)
  ("P"    'sldb-print-condition)
  (":"    'slime-interactive-eval))

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
  "Overlays created in source code buffers to temporarily highlight expressions.")

(defun sldb-get-buffer (&optional create)
  (let* ((number (slime-connection-number))
         (buffer-name (format "*sldb [connection #%S]*" number)))
    (funcall (if create #'get-buffer-create #'get-buffer)
             buffer-name)))

(defun sldb-setup (thread level condition restarts frames)
  "Setup a new SLDB buffer.
CONDITION is a string describing the condition to debug.
RESTARTS is a list of strings (NAME DESCRIPTION) for each available restart.
FRAMES is a list (NUMBER DESCRIPTION) describing the initial
portion of the backtrace. Frames are numbered from 0."
  (with-current-buffer (sldb-get-buffer t)
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
                 (y-or-n-p "Enter recursive edit? "))
        (recursive-edit)))))

(defun sldb-activate (thread level)
  (with-current-buffer (sldb-get-buffer t)
    (unless (equal sldb-level level)
      (with-lexical-bindings (thread level)
        (slime-eval-async `(swank:debugger-info-for-emacs 0 1) nil
                          (lambda (result)
                            (apply #'sldb-setup thread level result)))))))

;; XXX thread is ignored
(defun sldb-exit (thread level)
  (when-let (sldb (sldb-get-buffer))
    (with-current-buffer sldb
      (set-window-configuration sldb-saved-window-configuration)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq sldb-level nil))
    (when (= level 1)
      (kill-buffer sldb))))

(defun sldb-insert-condition (condition)
  (destructuring-bind (message type references) condition
    (insert (in-sldb-face topline message)
            "\n" 
            (in-sldb-face condition type)
            "\n\n")
    (when references
      (insert "See also:\n")
      (slime-with-rigid-indentation 2
        (sldb-insert-references references))
      (insert "\n"))))

(defun sldb-insert-references (references)
  "Insert documentation references from a condition.
See SWANK-BACKEND:CONDITION-REFERENCES for the datatype."
  (loop for ref in references
        do
        (destructuring-bind (where type what) ref
          (insert (sldb-format-reference-source where) ", ")
          (slime-insert-propertized (sldb-reference-properties where type what)
                                    (sldb-format-reference-node what))
          (insert (format " [%s]" (slime-cl-symbol-name type)) "\n"))))

(defun sldb-reference-properties (where type what)
  "Return the properties for a reference.
Only add clickability to properties we actually know how to lookup."
  (if (or (and (eq where :sbcl) (eq type :node))
          (and (eq where :ansi-cl)
               (symbolp type)
               (member (slime-cl-symbol-name type)
                       '("function" "special-operator" "macro"))))
      `(sldb-default-action sldb-lookup-reference
                            sldb-reference ,ref
                            face sldb-reference-face
                            mouse-face highlight)))

(defun sldb-format-reference-source (where)
  (case where
    (:ansi-cl "Common Lisp Hyperspec")
    (:sbcl    "SBCL Manual")
    (t        (format "%S" where))))

(defun sldb-format-reference-node (what)
  (if (symbolp what)
      (upcase (slime-cl-symbol-name what))
    what))

(defun sldb-lookup-reference ()
  "Browse the documentation reference at point."
  (destructuring-bind (where type what)
      (get-text-property (point) 'sldb-reference)
    (case where
      (:ansi-cl
       (hyperspec-lookup (if (symbolp what)
                             (slime-cl-symbol-name what)
                           what)))
      (t
       (let ((url (format "%s%s.html" slime-sbcl-manual-root (downcase what))))
         (browse-url url))))))

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
            until (string-match "[^(]*(\\(SWANK\\|swank\\):" string)
            collect frame)
      frames))

(defun sldb-insert-frame (frame)
  (destructuring-bind (number string) frame
    (slime-insert-propertized 
     `(frame ,frame) 
     "  " (in-sldb-face frame-label (format "%d" number)) ": "
     (in-sldb-face frame-line string)
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
                  sldb-previous-frame-number ,number)
                (in-sldb-face section " --more--\n"))))))))

(defun sldb-fetch-more-frames (&rest ignore)
  "Fetch more backtrace frames.
Called on the `point-entered' text-property hook."
  (let ((inhibit-point-motion-hooks t))
    (let ((inhibit-read-only t))
      (when-let (previous (get-text-property (point) 
                                             'sldb-previous-frame-number))
        (beginning-of-line)
        (let ((start (point)))
          (goto-char (point-max))
          (delete-region start (point)))
        (let ((start (1+ previous))
              (end (+ previous 40)))
          (sldb-insert-frames (slime-eval `(swank:backtrace ,start ,end))
                              (- end start)))))))


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

(defun sldb-previous-frame-number ()
  (save-excursion
    (sldb-backward-frame)
    (sldb-frame-number-at-point)))

(defun sldb-show-source ()
  "Highlight the frame at point's expression in a source code buffer."
  (interactive)
  (sldb-delete-overlays)
  (let* ((number (sldb-frame-number-at-point)))
    (slime-eval-async
     `(swank:frame-source-location-for-emacs ,number)
     nil
     (lambda (source-location)
       (destructure-case source-location
         ((:error message)
          (message "%s" message)
          (ding))
         (t
          (slime-show-source-location source-location)))))))

(defun slime-show-source-location (source-location)
  (slime-goto-source-location source-location)
  (when sldb-highlight (sldb-highlight-sexp))
  (let ((position (point)))
    (save-selected-window
      (let ((w (select-window (or (get-buffer-window (current-buffer) t)
                                  (display-buffer (current-buffer) t)))))
        (goto-char position)
        (unless (pos-visible-in-window-p)
          (slime-recenter-window w sldb-show-location-recenter-arg))))))

(defun slime-recenter-window (window line)
  "Set window-start in WINDOW LINE lines before point."
  (let* ((line (if (not line)
                   (/ (window-height window) 2)
                 line))
         (start (save-excursion
                  (loop repeat line do (forward-line -1))
                  (point))))
    (set-window-start w start)))

(defun sldb-highlight-sexp (&optional start end)
  "Highlight the first sexp after point."
  (sldb-delete-overlays)
  (let ((start (or start (point)))
	(end (or end (save-excursion (forward-sexp)  (point)))))
    (push (make-overlay start (1+ start)) sldb-overlays)
    (push (make-overlay (1- end) end) sldb-overlays)
    (dolist (overlay sldb-overlays)
      (overlay-put overlay 'face 'secondary-selection))))


(defun sldb-toggle-details (&optional on)
  "Toggle display of details for the current frame.
The details include local variable bindings and CATCH-tags."
  (interactive)
  (sldb-frame-number-at-point)
  (let ((inhibit-read-only t))
    (if (or on (not (sldb-frame-details-visible-p)))
	(sldb-show-frame-details)
      (sldb-hide-frame-details))))

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
	(slime-propertize-region (plist-put props 'details-visible-p t)
	  (insert "  " 
                  (in-sldb-face frame-label (format "%d" frame-number)) ": "
                  (in-sldb-face detailed-frame-line (second frame)) "\n"
                  indent1 (in-sldb-face section "Locals:") "\n")
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
		      (slime-buffer-package)
		      (lambda (reply) (slime-message "%s" reply)))))

(defun sldb-pprint-eval-in-frame (string)
  "Prompt for an expression, evaluate in selected frame, pretty-print result."
  (interactive (list (slime-read-from-minibuffer "Eval in frame: ")))
  (let* ((number (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:pprint-eval-string-in-frame ,string ,number)
		      nil
		      (lambda (result)
			(slime-show-description result nil)))))

(defun sldb-inspect-in-frame (string)
  "Prompt for an expression and inspect it in the selected frame."
  (interactive (list (slime-read-from-minibuffer 
                      "Inspect in frame (evaluated): " 
                      (slime-sexp-at-point))))
  (let ((number (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:inspect-in-frame ,string ,number)
                      (slime-buffer-package)
                      'slime-open-inspector)))

(defun sldb-inspect-condition ()
  "Inspect the current debugger condition."
  (interactive)
  (slime-eval-async '(swank:inspect-current-condition) (slime-buffer-package)
                    'slime-open-inspector))

(defun sldb-forward-frame ()
  (goto-char (next-single-char-property-change (point) 'frame)))

(defun sldb-backward-frame ()
  (goto-char (previous-single-char-property-change
	      (point) 'frame 
	      nil sldb-backtrace-start-marker)))

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
  (dolist (var (sldb-frame-locals frame))
    (destructuring-bind (&key name id value) var
      (insert prefix (in-sldb-face local-name name))
      (unless (zerop id) 
        (insert (in-sldb-face local-name (format "#%d" id))))
      (insert " = " (in-sldb-face local-value value) "\n"))))

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


(defun sldb-quit ()
  "Quit to toplevel."
  (interactive)
  (slime-eval-async '(swank:throw-to-toplevel) nil (lambda (_))))

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
                    nil
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
    (slime-eval-async `(swank:sldb-step ,frame) nil (lambda ()))))

(defun sldb-disassemble ()
  "Disassemble the code for the current frame."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:sldb-disassemble ,frame) nil 
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


;;; Thread control panel

(defun slime-list-threads ()
  "Display a list of threads."
  (interactive)
  (slime-eval-async 
   '(swank:list-threads)
   nil
   (lambda (threads)
      (with-current-buffer (get-buffer-create "*slime-threads*")
       (slime-thread-control-mode)
       (let ((inhibit-read-only t))
         (erase-buffer)
         (loop for id from 0 
               for (name status) in threads
               do (slime-thread-insert id name status))
         (goto-char (point-min))
         (setq buffer-read-only t)
         (pop-to-buffer (current-buffer)))))))

(defun slime-thread-insert (id name summary)
  (slime-propertize-region `(thread-id ,id)
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
  ((kbd "RET") 'slime-thread-goahead)
  ("q"         'slime-thread-quit))

(defun slime-thread-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun slime-thread-kill ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-id)))
    (slime-eval `(swank:kill-thread-by-id ,id)))
  (call-interactively 'slime-list-threads))

(defun slime-thread-attach ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-id))
        (file (slime-swank-port-file)))
    (slime-eval-async `(swank:start-swank-server-in-thread ,id ,file)
                      (slime-buffer-package)
                      (lambda (v) nil)))
  (slime-read-port-and-connect-to-running-swank nil))

(defun slime-thread-debug ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-id)))
    (slime-eval-async `(swank::debug-thread-by-id ,id)
                      (slime-buffer-package)
                      (lambda (v) nil))))


;;;;; Connection listing

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
  ("q"         'slime-temp-buffer-quit))

(defun slime-connection-at-point ()
  (or (get-text-property (point) 'slime-connection)
      (error "No connection at point")))

(defun slime-goto-connection ()
  (interactive)
  (let ((slime-dispatching-connection (slime-connection-at-point)))
    (slime-switch-to-output-buffer)))

(defun slime-connection-list-make-default ()
  (interactive)
  (slime-select-connection (slime-connection-at-point))
  (slime-update-connection-list))

(defun slime-list-connections ()
  "Display a list of all connections."
  (interactive)
  (when (get-buffer "*SLIME connections*")
    (kill-buffer "*SLIME connections*"))
  (with-current-buffer (get-buffer-create "*SLIME connections*")
    (slime-draw-connection-list)
    (slime-connection-list-mode)
    (setq buffer-read-only t)
    (setq slime-temp-buffer-saved-window-configuration
          (current-window-configuration))
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
        (default (slime-connection))
        (fstring "%s%2s  %-7s  %-17s  %-7s %-s\n"))
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
    (goto-char default-pos)))


;;; Inspector

(defvar slime-inspector-mark-stack '())

(defun slime-inspect (string)
  "Eval an expression and inspect the result."
  (interactive 
   (list (slime-read-from-minibuffer "Inspect value (evaluated): "
				     (slime-sexp-at-point))))
  (slime-eval-async `(swank:init-inspector ,string) (slime-buffer-package)
		    'slime-open-inspector))

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
	(current-buffer))))

(defun slime-inspector-fontify (face string)
  (slime-add-face (intern (format "slime-inspector-%s-face" face)) string))

(defun slime-open-inspector (inspected-parts &optional point)
  "Display INSPECTED-PARTS in a new inspector window.
Optionally set point to POINT."
  (with-current-buffer (slime-inspector-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (destructuring-bind (&key text type primitive-type parts) inspected-parts
        (macrolet ((fontify (face string)
                            `(slime-inspector-fontify ',face ,string)))
          (insert (fontify topline text))
          (while (eq (char-before) ?\n) (backward-delete-char 1))
          (insert "\n" 
                  "   [" (fontify label "type:") " " (fontify type type) "]\n"
                  "   [" (fontify type primitive-type) "]\n"
                  "\n"
                  (fontify label "Slots") ":\n")
        (save-excursion
          (loop for (label . value) in parts
                for i from 0
                do (slime-propertize-region `(slime-part-number ,i)
                     (insert (fontify label label) ": " 
                             (fontify value value) "\n"))))
        (pop-to-buffer (current-buffer))
        (when point (goto-char point))))
    t)))

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
  ("\C-m"   'slime-inspector-inspect-object-at-point)
  ("l" 'slime-inspector-pop)
  ("n" 'slime-inspector-next)
  (" " 'slime-inspector-next)
  ("d" 'slime-inspector-describe)
  ("q" 'slime-inspector-quit)
  ("\M-." 'slime-edit-definition))


;;; Buffer selector

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

(def-slime-selector-method ?s
  "the *slime-scratch* buffer."
  (slime-scratch-buffer))

(def-slime-selector-method ?i
  "the *inferior-lisp* buffer."
  "*inferior-lisp*")

(def-slime-selector-method ?v
  "the *slime-events* buffer."
  slime-event-buffer-name)

(def-slime-selector-method ?l
  "the most recently visited lisp-mode buffer."
  (slime-recently-visited-buffer 'lisp-mode))

(def-slime-selector-method ?d
  "the *sldb* buffer for the current connection."
  (unless (sldb-get-buffer)
    (error "No debugger buffer"))
  (sldb-get-buffer))

(def-slime-selector-method ?e
  "the most recently visited emacs-lisp-mode buffer."
  (slime-recently-visited-buffer 'emacs-lisp-mode))

(def-slime-selector-method ?c
  "the SLIME connections buffer."
  (slime-list-connections)
  "*SLIME connections*")

(def-slime-selector-method ?t
  "the SLIME threads buffer."
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


;;; Editing commands

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


;;; Indentation

(defun slime-update-indentation ()
  "Update indentation for all macros defined in the Lisp system."
  (interactive)
  (slime-eval-async '(swank:update-indentation-information) nil (lambda (x))))

(defun slime-handle-indentation-update (alist)
  "Update Lisp indent information.

ALIST is a list of (SYMBOL-NAME . INDENT-SPEC) of proposed indentation
settings for `common-lisp-indent-function'. The appropriate property
is setup, unless the user already set one explicitly."
  (dolist (info alist)
    (let* ((symbol-name (car info))
           (symbol (intern symbol-name))
           (indent (cdr info)))
      ;; Does the symbol have an indentation value that we set?
      (when (equal (get symbol 'common-lisp-indent-function)
                   (get symbol 'slime-indent))
        (put symbol 'slime-indent indent)
        (put symbol 'common-lisp-indent-function indent)))))

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
                            (beginning-of-defun))
                          (point)))
            (end (ignore-errors (end-of-defun) (point))))
        (unless end
          (forward-paragraph)
          (slime-close-all-sexp)
          (end-of-defun)
          (setf end (point)))
        (indent-region start end nil)))))


;;; Test suite

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

(defvar slime-test-buffer-name "*Tests*"
  "The name of the buffer used to display test results.")

(defvar slime-expected-failures
  '(("cmucl" 0)
    ("sbcl" 2)
    ("clisp" 13)
    ("lispworks" 7)
    ("allegro" 6))
  "The number of expected failed tests for each implementation.")

(defun slime-expected-failures ()
  "Return the numbers of expected failure for the current implementation."
  (or (cadr (assoc (slime-lisp-implementation-type-name)
                   slime-expected-failures))
      0))


;;;;; Execution engine

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
  (let ((slime-dont-prompt t)
        (slime-swank-port 4006)         ; different port than interactive use
        (slime-test-debug-on-error nil))
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
  (and (null (sldb-get-buffer))
       (null slime-rex-continuations)))

(defun slime-wait-condition (name predicate timeout)
  (let ((end (time-add (current-time) (seconds-to-time timeout))))
    (while (not (funcall predicate))
      (cond ((time-less-p end (current-time))
             (error "Timeout waiting for condition: %S" name))
            (t
             (accept-process-output nil 0 100000))))))

(defun slime-sync-to-top-level (timeout)
  (slime-wait-condition "top-level" #'slime-at-top-level-p timeout))

(defun slime-check-sldb-level (expected)
  (let ((sldb-level (when-let (sldb (sldb-get-buffer))
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
  (when-let (sldb (sldb-get-buffer))
    (with-current-buffer sldb
      sldb-level)))

(defun slime-sldb-level= (level)
  (when-let (sldb (sldb-get-buffer))
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
      ("cl::compile-file" (("cl::compile-file" "cl::compile-file-pathname")
                           "cl::compile-file"))
      ("cl:m-v-l" (("cl:multiple-value-list" "cl:multiple-values-limit")
                   "cl:multiple-value-li")))
  (let ((completions (slime-completions prefix)))
    (slime-test-expect "Completion set" expected-completions completions)))

(def-slime-test arglist
    (function-name expected-arglist)
    "Lookup the argument list for FUNCTION-NAME.
Confirm that EXPECTED-ARGLIST is displayed."
    '(("swank:start-server"
       "(swank:start-server port-file &optional (style *communication-style*) dont-close)")
      ("swank::compound-prefix-match"
       "(swank::compound-prefix-match prefix target)")
      ("swank::create-socket"
       "(swank::create-socket host port)")
      ("swank::emacs-connected"
       "(swank::emacs-connected stream)")
      ("swank::compile-string-for-emacs"
       "(swank::compile-string-for-emacs string buffer position)")
      ("swank::connection.socket-io"
       "(swank::connection.socket-io \\(struct\\(ure\\)?\\|object\\|instance\\))")
      ("cl:lisp-implementation-type"
       "(cl:lisp-implementation-type)")
      )
;;    Different arglists found in the wild.
;;      ("cl:class-name"
;;       "(cl:class-name structure)"))
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
             (with-current-buffer (sldb-get-buffer)
               (when (> sldb-level debug-hook-max-depth)
                 (setq debug-hook-max-depth sldb-level)
                 (if (= sldb-level depth)
                     ;; We're at maximum recursion - time to unwind
                     (sldb-quit)
                   ;; Going down - enter another recursive debug
                   ;; Recursively debug.
                   (slime-eval-async 'no-such-variable 
                                     nil (lambda (_) nil))))))))
      (let ((sldb-hook (cons debug-hook sldb-hook)))
        (slime-eval-async 'no-such-variable nil (lambda (_) nil))
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
  (slime-eval-async '(cl:loop) "CL-USER" (lambda (_) ))
  (accept-process-output nil 1)
  (slime-check "In eval state." (not (null slime-rex-continuations)))
  (slime-interrupt)
  (slime-wait-condition "First interrupt" (lambda () (slime-sldb-level= 1)) 5)
  (with-current-buffer (sldb-get-buffer) 
    (sldb-quit))
  (slime-sync-to-top-level 5)
  (slime-check-top-level))

(def-slime-test loop-interrupt-continue-interrupt-quit
    ()
    "Test interrupting a previously interrupted but continued loop."
    '(())
  (slime-check-top-level)
  (slime-eval-async '(cl:loop) "CL-USER" (lambda (_) ))
  (sleep-for 1)
  (slime-wait-condition "running" #'slime-busy-p 5)
  (slime-interrupt)
  (slime-wait-condition "First interrupt" (lambda () (slime-sldb-level= 1)) 5)
  (with-current-buffer (sldb-get-buffer)
    (sldb-continue))
  (slime-wait-condition "running" (lambda () (and (slime-busy-p)
                                                  (not (sldb-get-buffer)))) 5)
  (slime-interrupt)
  (slime-wait-condition "Second interrupt" (lambda () (slime-sldb-level= 1)) 5)
  (with-current-buffer (sldb-get-buffer)
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
          (equal "3" message))))))

(def-slime-test interrupt-bubbling-idiot 
    ()
    "Test interrupting a loop that sends a lot of output to Emacs."
    '(())
  (slime-check-top-level)
  (slime-eval-async '(cl:loop :for i :from 0 :do (cl:progn (cl:print i) 
                                                           (cl:force-output)))
                    "CL-USER" (lambda (_) ))
  (accept-process-output nil 1)
  (slime-wait-condition "running" #'slime-busy-p 5)
  (slime-interrupt)
  (slime-wait-condition "Debugger visible" 
                        (lambda () 
                          (and (slime-sldb-level= 1)
                               (get-buffer-window (sldb-get-buffer))))
                        5)
  (with-current-buffer (sldb-get-buffer)
    (sldb-quit))
  (slime-sync-to-top-level 5))

(def-slime-test package-updating
    (package-name nicknames)
    "Test if slime-lisp-package is updated."
    '(("COMMON-LISP" ("CL"))
      ("KEYWORD" ("" "KEYWORD"))
      ("COMMON-LISP-USER" ("CL-USER" "USER")))
  (with-current-buffer (slime-output-buffer)
    (let ((p (slime-eval 
              `(swank:listener-eval 
                ,(format 
                  "(cl:setq cl:*package* (cl:find-package %S))
                   (cl:package-name cl:*package*)" package-name))
              (slime-lisp-package))))
      (slime-check ("In %s package." package-name)
        (equal (format "\"%s\"" package-name) p))
      (slime-check ("slime-lisp-package is in %S." nicknames)
        (member (slime-lisp-package) nicknames)))))

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
    (setf (slime-lisp-package) "SWANK"))
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
    (setf (slime-lisp-package) "SWANK"))
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
    (setf (slime-lisp-package) "SWANK")
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
    (setf (slime-lisp-package) "SWANK"))
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
  (slime-eval-async '(cl-user::foo) nil (lambda (_)))
  (slime-wait-condition "Debugger visible" 
                        (lambda () 
                          (and (slime-sldb-level= 1)
                               (get-buffer-window (sldb-get-buffer))))
                        5)
  (with-current-buffer (sldb-get-buffer)
    (sldb-quit))
  (slime-sync-to-top-level 5))
  

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
  (defmacro slime-defun-if-undefined (name &rest rest)
    `(unless (fboundp ',name)
       (defun ,name ,@rest))))

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
    (forward-line n)
    (point)))

(slime-defun-if-undefined line-end-position (&optional n)
  (save-excursion
    (forward-line n)
    (end-of-line)
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

(slime-defun-if-undefined read-directory-name (prompt &optional dir default-dirname 
                                                      mustmatch initial)
  (unless dir
    (setq dir default-directory))
  (unless default-dirname
    (setq default-dirname
	  (if initial (concat dir initial) default-directory)))
  (let ((file (read-file-name prompt dir default-dirname mustmatch initial)))
    (setq file (expand-file-name file))
    (cond ((file-directory-p file)
           file)
          (t 
           (error "Not a directory: %s" file)))))

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


;;; Finishing up

(require 'bytecomp)
(let ((byte-compile-warnings '()))
  (mapc #'byte-compile
        '(slime-log-event
          slime-events-buffer
          slime-output-string 
          slime-output-buffer
          slime-output-filter
          slime-with-output-end-mark
          slime-process-available-input 
          slime-dispatch-event 
          slime-net-filter 
          slime-net-have-input-p
          slime-net-read3
          slime-net-read
          slime-print-apropos
          slime-show-note-counts
          slime-insert-propertized
          slime-tree-insert)))

(run-hooks 'slime-load-hook)

(provide 'slime)

;;; slime.el ends here
