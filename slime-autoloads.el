;;; slime-autoloads.el --- autoload definitions for SLIME -*- no-byte-compile: t; lexical-binding: t -*-

;; Copyright (C) 2007  Helmut Eller

;; This file is protected by the GNU GPLv2 (or later), as distributed
;; with GNU Emacs.

;;; Commentary:

;; This code defines the necessary autoloads, so that we don't need to
;; load everything from .emacs.
;;
;; JT@14/01/09: FIXME: This file should be auto-generated with autoload cookies.

;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))

(autoload 'slime "slime"
  "Start a Lisp subprocess and connect to its Swank server." t)

(autoload 'slime-mode "slime"
  "SLIME: The Superior Lisp Interaction (Minor) Mode for Emacs." t)

(autoload 'slime-connect "slime"
  "Connect to a running Swank server." t)

(autoload 'slime-selector "slime"
  "Select a new by type, indicated by a single character." t)

(autoload 'hyperspec-lookup "lib/hyperspec" nil t)

(autoload 'slime-lisp-mode-hook "slime")

(autoload 'slime-scheme-mode-hook "slime")

(defvar slime-contribs '(slime-fancy)
  "A list of contrib packages to load with SLIME.")

(autoload 'slime-setup "slime"
  "Setup some SLIME contribs.")

(define-obsolete-variable-alias 'slime-setup-contribs
  'slime-contribs "2.3.2")

(add-hook 'lisp-mode-hook 'slime-lisp-mode-hook)

(provide 'slime-autoloads)

;;; slime-autoloads.el ends here


;;; Generated autoloads from slime.el

(autoload 'slime-setup "slime" "\
Setup Emacs so that lisp-mode buffers always use SLIME.
CONTRIBS is a list of contrib packages to load. If `nil', use
`slime-contribs'. 

(fn &optional (CONTRIBS nil CONTRIBS-P))" t)
(autoload 'slime-mode "slime" "\
\\<slime-mode-map>SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode).

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
\\[slime-edit-definition]
- Edit the definition of the function called at point.
\\[slime-pop-find-definition-stack]
- Pop the definition stack to go back from a definition.

Documentation commands:
\\[slime-describe-symbol]	- Describe symbol.
\\[slime-apropos]	- Apropos search.
\\[slime-disassemble-symbol]	- Disassemble a function.

Evaluation commands:
\\[slime-eval-defun]	- Evaluate top-level from containing point.
\\[slime-eval-last-expression]	- Evaluate sexp before point.
\\[slime-pprint-eval-last-expression]	- Evaluate sexp before point, pretty-print result.

Full set of commands:
\\{slime-mode-map}

This is a minor mode.  If called interactively, toggle the `Slime mode'
mode.  If the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate the variable `slime-mode'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(autoload 'slime "slime" "\
Start an inferior^_superior Lisp and connect to its Swank server.

(fn &optional COMMAND CODING-SYSTEM)" t)
(autoload 'slime-connect "slime" "\
Connect to a running Swank server. Return the connection.

(fn HOST PORT &optional CODING-SYSTEM INTERACTIVE-P &rest PARAMETERS)" t)
(autoload 'slime-selector "slime" "\
Select a new buffer by type, indicated by a single character.
The user is prompted for a single character indicating the method by
which to choose a new buffer. The `?' character describes the
available methods.

See `def-slime-selector-method' for defining new methods.

(fn &optional OTHER-WINDOW)" t)
(register-definition-prefixes "slime" '("def-slime-selector-method" "define-sl" "defslimefun" "inferior-lisp-program" "make-slime-" "sldb-" "slime-" "topline"))
