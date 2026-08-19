;; -*- lexical-binding: t; -*-
;;; slime-xterm-color.el --- Colorize ANSI sequences in SLIME REPL

(define-slime-contrib slime-xterm-color
  "Colorize ANSI escape sequences in SLIME REPL output using xterm-color."
  (:authors "Mark Evenson <evenson.not.org@gmail.com>")
  (:license "GPL")
  (:slime-dependencies slime-repl)
  (:on-load (slime-xterm-color-init))
  (:on-unload (slime-xterm-color-fini)))

(require 'slime) ;; kinda obvious:  do we really need to declare in a contrib?
(if (< emacs-major-version 29)
    (require 'xterm-color) ;; need to explicitly install locally via
                           ;; <https://github.com/atomontage/xterm-color/README.org>.
  (use-package xterm-color :ensure t))

(defun slime-xterm-color-init ()
  "Initialize xterm-color processing for SLIME output."
  
  (advice-add 'slime-repl-emit :filter-args
              #'slime-xterm-color--colorize-output)
  
  (message "slime-xterm-color initialized"))

(defun slime-xterm-color-fini ()
  "Disable xterm-color processing for SLIME output."
  (advice-remove 'slime-repl-emit
                 #'slime-xterm-color--colorize-output)
  
  (message "slime-xterm-color disabled"))

(defun slime-xterm-color--colorize-output (args)
  "Apply xterm-color to SLIME output before it's displayed."
  (let ((output (car args)))
    (when (stringp output)
      ;; Apply xterm-color and remove the last character if it's a newline
      ;; to preserve SLIME's output formatting
      (let ((colored (xterm-color-filter output)))
        (setcar args colored))))
  args)

(provide 'slime-xterm-color)
