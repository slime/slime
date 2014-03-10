(require 'sly)

(define-sly-contrib sly-fancy
  "Make SLY fancy."
  (:authors "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>"
            "Tobias C Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:sly-dependencies sly-repl
                       sly-autodoc
                       sly-c-p-c
                       sly-editing-commands
                       sly-fancy-inspector
                       sly-fancy-trace
                       sly-fuzzy
                       sly-presentations
                       sly-scratch
                       sly-references
                       sly-package-fu
                       sly-fontifying-fu
                       sly-trace-dialog)
  (:on-load
   (sly-trace-dialog-init)
   (sly-repl-init)
   (sly-autodoc-init)
   (sly-c-p-c-init)
   (sly-editing-commands-init)
   (sly-fancy-inspector-init)
   (sly-fancy-trace-init)
   (sly-fuzzy-init)
   (sly-presentations-init)
   (sly-scratch-init)
   (sly-references-init)
   (sly-package-fu-init)
   (sly-fontifying-fu-init)))

(provide 'sly-fancy)
