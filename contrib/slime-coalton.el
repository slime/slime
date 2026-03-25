;;; slime-coalton.el --- Coalton support for SLIME  -*- lexical-binding: t; -*-

(require 'slime)

(define-slime-contrib slime-coalton
  "Coalton support."
  (:license "GPL")
  (:slime-dependencies slime-autodoc)
  (:swank-dependencies swank-coalton))

(provide 'slime-coalton)
