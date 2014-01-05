;;; -*- lisp -*-

;; ASDF system definition for loading the Swank server independently
;; of Emacs.
;;
;; This is only useful if you want to start a Swank server in a Lisp
;; processes that doesn't run under Emacs. Lisp processes created by
;; `M-x slime' automatically start the server.

;; Usage:
;;
;;   (require :swank)
;;   (swank:create-swank-server PORT) => ACTUAL-PORT
;;
;; (PORT can be zero to mean "any available port".)
;; Then the Swank server is running on localhost:ACTUAL-PORT. You can
;; use `M-x slime-connect' to connect Emacs to it.
;;
;; This code has been placed in the Public Domain.  All warranties
;; are disclaimed.

;; NB: This does NOT run init after loading.
;; ASDF is not for system construction, not for initialization.
;; Whoever calls (asdf:load-system :swank) is well-advised to afterwards call
;; (funcall (read-from-string "swank-loader::init") ...)
;; with proper flag (:delete :reload :load-contribs :setup :quiet)

(asdf:defsystem :swank
  :components ((:file "swank-loader")))
