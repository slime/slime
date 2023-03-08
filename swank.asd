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
;;   (asdf:load-system "swank")
;;   (swank:create-swank-server PORT) => ACTUAL-PORT
;;
;; (PORT can be zero to mean "any available port".)
;; Then the Swank server is running on localhost:ACTUAL-PORT. You can
;; use `M-x slime-connect' to connect Emacs to it.
;;
;; This code has been placed in the Public Domain.  All warranties
;; are disclaimed.

(asdf:defsystem "swank"
  :components ((:file "swank-loader")
               (:file "packages")
               (:file "xref" :if-feature :clisp)
               (:file "metering" :if-feature (:or :clozure :clisp :clasp))
               (:module "backend"
                :pathname "swank"
                :components ((:file "backend")
                             (:file "source-path-parser" :if-feature (:or :cmu :scl :sbcl))
                             (:file "source-file-cache" :if-feature (:or :cmu :scl :sbcl))
                             (:file "cmucl" :if-feature :cmu)
                             (:file "scl" :if-feature :scl)
                             (:file "sbcl" :if-feature :sbcl)
                             (:file "ccl" :if-feature :clozure)
                             (:file "lispworks" :if-feature :lispworks)
                             (:file "allegro" :if-feature :allegro)
                             (:file "clisp" :if-feature :clisp)
                             (:file "abcl" :if-feature :armedbear)
                             (:file "corman" :if-feature :cormanlisp)
                             (:file "ecl" :if-feature :ecl)
                             (:file "clasp" :if-feature :clasp)
                             (:file "mkcl" :if-feature :mkcl)
                             (:file "mezzano" :if-feature :mezzano)
                             (:file "gray" :if-feature (:not :armedbear))
                             (:file "match")
                             (:file "rpc")))
               (:file "swank")))

(defmethod asdf:perform :around ((op asdf:load-op) (c (eql (asdf:find-system "swank"))))
  (let ((var (uiop:find-symbol* '#:*source-directory* '#:swank-loader nil)))
    (if (and var (boundp var))
        (let ((loaded (truename (symbol-value var)))
              (requested (truename (asdf:system-source-directory "swank"))))
          (unless (equal requested loaded)
            (warn "~@<Not loading SWANK from ~S because it was ~
                      already loaded from ~S.~:@>"
                  requested loaded)))
        (call-next-method))))

(asdf:defsystem "swank/exts"
  :depends-on ("swank")
  :pathname "contrib"
  :components ((:file "swank-util")
               (:file "swank-repl")
               (:file "swank-c-p-c")
               (:file "swank-arglists")
               (:file "swank-fuzzy")
               (:file "swank-fancy-inspector")
               (:file "swank-presentations")
               (:file "swank-presentation-streams")
               (:file "swank-asdf" :if-feature (:or :asdf2 :asdf3 :sbcl :ecl))
               (:file "swank-package-fu")
               (:file "swank-hyperdoc")
               (:file "swank-sbcl-exts" :if-feature :sbcl)
               (:file "swank-mrepl")
               (:file "swank-trace-dialog")
               (:file "swank-macrostep")
               (:file "swank-quicklisp")))
