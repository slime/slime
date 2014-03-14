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
                     sly-trace-dialog))

(provide 'sly-fancy)
