;;; -*- lisp -*-

;; ASDF system definition for loading the Swank server independently
;; of Emacs.
;;
;; This is only useful if you want to start a Swank server in a Lisp
;; processes that doesn't run under Emacs. Lisp processes created by
;; `M-x slime' automatically start the server.

;; Usage:
;;
;;   (asdf:load-system :swank)
;;   (swank:create-server :dont-close t)
;;
;; After which, the Swank server is running on localhost:4005. You can
;; use `M-x slime-connect' to connect Emacs to it.
;;
;; This code has been placed in the Public Domain.  All warranties
;; are disclaimed.

(asdf:defsystem "swank"
  :perform (load-op :after (o c)
              (set (intern "*SOURCE-DIRECTORY*" 'swank-loader)
                   (asdf:system-source-directory :swank))
              (set (intern "*FASL-DIRECTORY*" 'swank-loader)
                   (asdf:apply-output-translations (asdf:system-source-directory :swank)))
              (uiop:symbol-call :swank :before-init
                 (uiop:symbol-call :swank-loader :slime-version-string)
                 (list
                  (uiop:symbol-call :swank-loader :contrib-dir
                     (symbol-value (intern "*FASL-DIRECTORY*" 'swank-loader)))
                  (uiop:symbol-call :swank-loader :contrib-dir
                     (symbol-value (intern "*SOURCE-DIRECTORY*" 'swank-loader))))))
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
                             (:file "gray")
                             (:file "match")
                             (:file "rpc")))
               (:file "swank")))

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
               (:file "swank-indentation")
               (:file "swank-sbcl-exts" :if-feature :sbcl)
               (:file "swank-mrepl")
               (:file "swank-trace-dialog")
               (:file "swank-macrostep")
               (:file "swank-quicklisp")))
