;;; swank-presentation-streams.el --- Streams that allow attaching object identities
;;;                                   to portions of output
;;;
;;; Authors: Alan Ruttenberg  <alanr-l@mumble.net>
;;;          Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;;;          Helmut Eller  <heller@common-lisp.net>
;;;
;;; License: GNU GPL (same license as Emacs)
;;;
;;; Installation
;;
;; Add this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'slime-load-hook (lambda () (require 'slime-presentation-streams)))
;;


;;; Initialization

(require 'slime-presentations)

(defun slime-presentation-streams-init ()
  (slime-require :swank-presentation-streams))

(provide 'slime-presentation-streams)

