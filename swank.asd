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
;;   (swank-loader:init)
;;   (swank:create-server PORT) => ACTUAL-PORT
;;
;; (PORT can be zero to mean "any available port".)
;; Then the Swank server is running on localhost:ACTUAL-PORT. You can
;; use `M-x slime-connect' to connect Emacs to it.
;;
;; This code has been placed in the Public Domain.  All warranties
;; are disclaimed.

;; See https://github.com/slime/slime/pull/76 and related issues
;; for details on this defsystem.

(asdf:defsystem :swank/loader
  :components ((:file "swank-loader")))

(asdf:defsystem :swank
  :depends-on (:swank/loader)
  :components
  ((:file "swank-backend")
   ;; If/when we require ASDF3, we shall use :if-feature instead
   #+(or cmu sbcl scl)
   (:file "swank-source-path-parser" :depends-on ("swank-backend"))
   #+(or cmu ecl sbcl scl)
   (:file "swank-source-file-cache" :depends-on ("swank-backend"))
   #+clisp
   (:file "xref" :depends-on ("swank-backend"))
   #+(or clisp clozure)
   (:file "metering" :depends-on ("swank-backend"))
   #+allegro
   (:file "swank-allegro" :depends-on ("swank-backend"))
   #+armedbear
   (:file "swank-abcl" :depends-on ("swank-backend"))
   #+clisp
   (:file "swank-clisp" :depends-on ("swank-backend" "xref" "metering"))
   #+clozure
   (:file "swank-ccl" :depends-on ("swank-backend"))
   #+cmu
   (:file "swank-cmucl" :depends-on ("swank-backend"))
   #+cormanlisp
   (:file "swank-corman" :depends-on ("swank-backend"))
   #+ecl
   (:file "swank-ecl" :depends-on ("swank-backend"))
   #+lispworks
   (:file "swank-lispworks" :depends-on ("swank-backend"))
   #+sbcl
   (:file "swank-sbcl" :depends-on ("swank-backend"))
   #+scl
   (:file "swank-scl" :depends-on ("swank-backend"))
   #+(or allegro clisp clozure cormanlisp ecl lispworks)
   (:file "swank-gray" :depends-on ("swank-backend"))
   (:file "swank-match")
   (:file "swank-rpc" :depends-on ("swank-backend"))
   (:file "swank" :depends-on ("swank-backend" "swank-match" "swank-rpc"))))

;;; TODO: we should either use the convention that works of naming secondary systems swank/foo
;;; or have each contrib in its own .asd file, otherwise ASDF will not be able to find them
;;; by name unless swank has been loaded first.

(asdf:defsystem :swank-contribs
  :depends-on
  (:swank
   :swank-util :swank-repl
   :swank-c-p-c :swank-arglists :swank-fuzzy
   :swank-fancy-inspector
   :swank-presentations :swank-presentation-streams
   #+(or asdf2 asdf3 sbcl ecl) :swank-asdf
   :swank-package-fu
   :swank-hyperdoc
   #+sbcl :swank-sbcl-exts
   :swank-mrepl :swank-trace-dialog))

(asdf:defsystem :swank-util
  :components ((:file "contrib/swank-util")))

(asdf:defsystem :swank-repl
  :components ((:file "contrib/swank-repl")))

(asdf:defsystem :swank-c-p-c
  :components ((:file "contrib/swank-c-p-c")))

(asdf:defsystem :swank-arglists
  :components ((:file "contrib/swank-arglists")))

(asdf:defsystem :swank-fuzzy
  :components ((:file "contrib/swank-fuzzy")))

(asdf:defsystem :swank-fancy-inspector
  :components ((:file "contrib/swank-fancy-inspector")))

(asdf:defsystem :swank-presentations
  :components ((:file "contrib/swank-presentations")))

(asdf:defsystem :swank-presentation-streams
  :components ((:file "contrib/swank-presentation-streams")))

(asdf:defsystem :swank-asdf
  :components ((:file "contrib/swank-asdf")))

(asdf:defsystem :swank-package-fu
  :components ((:file "contrib/swank-package-fu")))

(asdf:defsystem :swank-hyperdoc
  :components ((:file "contrib/swank-hyperdoc")))

(asdf:defsystem :swank-swank-sbcl-exts
  :components ((:file "contrib/swank-swank-sbcl-exts")))

(asdf:defsystem :swank-mrepl
  :components ((:file "contrib/swank-mrepl")))

(asdf:defsystem :swank-trace-dialog
  :components ((:file "contrib/swank-trace-dialog")))

(asdf:defsystem :swank-clipboard
  :components ((:file "contrib/swank-clipboard")))

(asdf:defsystem :swank-indentation
  :components ((:file "contrib/swank-indentation")))

(asdf:defsystem :swank-listener-hooks
  :components ((:file "contrib/swank-listener-hooks")))

(asdf:defsystem :swank-media
  :components ((:file "contrib/swank-media")))

(asdf:defsystem :swank-motd
  :components ((:file "contrib/swank-motd")))

(asdf:defsystem :swank-snapshot
  :components ((:file "contrib/swank-snapshot")))

(asdf:defsystem :swank-sprof
  :components ((:file "contrib/swank-sprof")))
