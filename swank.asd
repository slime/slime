;;; -*- lisp -*-

;; ASDF system definition for loading the Swank server independently
;; of Emacs.
;;
;; This is only useful if you want to start a Swank server in a Lisp
;; processes that doesn't run under Emacs. Lisp processes created by
;; `M-x slime' automatically start the server.
;;
;; If Swank is already loaded (e.g. the Lisp is running under SLIME),
;; then attempts to load it via asdf do nothing, except for emitting a
;; warning if Swank is to be loaded from a location that's different
;; from the location where it was originally loaded from. This
;; behavior is intended to prevent loading a possibly incompatible
;; version of Swank with a running SLIME.

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

(defclass swank-loader-file (asdf:cl-source-file) ())

;;;; after loading run init

(defmethod asdf:perform ((o asdf:load-op) (f swank-loader-file))
  (let ((var (uiop:find-symbol* '#:*source-directory* '#:swank-loader nil)))
    (cond ((and var (boundp var))
           (let ((loaded (truename (symbol-value var)))
                 (requested (truename (asdf:system-source-directory "swank"))))
             (unless (equal requested loaded)
               (warn "~@<Not loading SWANK from ~S because it was ~
                      already loaded from ~S.~:@>"
                     requested loaded))))
          (t
           ;; swank-loader computes its own source/fasl relation based
           ;; on the TRUENAME of the loader file, so we need a "manual"
           ;; CL:LOAD invocation here.
           (load (asdf::component-pathname f))
           ;; After loading, run the swank-loader init routines.
           (funcall (read-from-string "swank-loader::init") :reload t)))))

(asdf:defsystem :swank
  :default-component-class swank-loader-file
  :components ((:file "swank-loader")))
