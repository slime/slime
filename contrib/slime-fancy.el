(require 'slime)

(define-slime-contrib slime-fancy
  "Make SLIME fancy."
  (:authors "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>"
            "Tobias C Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:slime-dependencies slime-repl
                       slime-autodoc
                       slime-c-p-c
                       slime-editing-commands
                       slime-fancy-inspector
                       slime-fancy-trace
                       slime-fuzzy
                       slime-presentations
                       slime-scratch
                       slime-references
                       slime-package-fu
                       slime-fontifying-fu))

(unless (version< emacs-version "24")
  (require 'slime-trace-dialog)
  (push 'slime-trace-dialog
        (slime-contrib-slime-dependencies
         (slime-find-contrib 'slime-fancy))))

(provide 'slime-fancy)
