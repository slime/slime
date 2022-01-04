;;; slime-autoloads.el --- autoload definitions for SLIME -*- no-byte-compile: t -*-

;; Copyright (C) 2007  Helmut Eller

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
