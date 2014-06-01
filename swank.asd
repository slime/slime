;;; -*- lisp -*-

;; ASDF system definition for loading the Swank server independently
;; of Emacs.
;;
;; Usage:
;;
;;   (asdf:load-system :swank)
;;   (swank:create-server PORT) => ACTUAL-PORT
;;
;; (PORT can be zero to mean "any available port".)
;; Then the Swank server is running on localhost:ACTUAL-PORT. You can
;; use `M-x slime-connect' to connect Emacs to it.
;;
;; This code has been placed in the Public Domain.  All warranties
;; are disclaimed.

(defsystem :swank
  :serial t
  :components
  ((:file "swank-backend")
   ;; If/when we require ASDF3, we shall use :if-feature instead
   #+(or cmu sbcl scl)
   (:file "swank-source-path-parser")
   #+(or cmu ecl sbcl scl)
   (:file "swank-source-file-cache")
   #+clisp
   (:file "xref")
   #+(or clisp clozure)
   (:file "metering")
   #+allegro
   (:file "swank-allegro")
   #+armedbear
   (:file "swank-abcl")
   #+clisp
   (:file "swank-clisp")
   #+clozure
   (:file "swank-ccl")
   #+cmu
   (:file "swank-cmucl")
   #+cormanlisp
   (:file "swank-corman")
   #+ecl
   (:file "swank-ecl")
   #+lispworks
   (:file "swank-lispworks")
   #+sbcl
   (:file "swank-sbcl")
   #+scl
   (:file "swank-scl")
   #+(or sbcl allegro clisp clozure cormanlisp ecl lispworks)
   (:file "swank-gray")
   (:file "swank-match")
   (:file "swank-rpc")
   (:file "swank")))

(defsystem :swank-util
  :components ((:file "contrib/swank-util")))

(defsystem :swank-repl
  :components ((:file "contrib/swank-repl")))

(defsystem :swank-c-p-c
  :components ((:file "contrib/swank-c-p-c")))

(defsystem :swank-arglists
  :components ((:file "contrib/swank-arglists")))

(defsystem :swank-fuzzy
  :components ((:file "contrib/swank-fuzzy")))

(defsystem :swank-fancy-inspector
  :components ((:file "contrib/swank-fancy-inspector")))

(defsystem :swank-presentations
  :components ((:file "contrib/swank-presentations")))

#+sbcl
(defsystem :swank/sbcl-pprint-patch
  :depends-on (:swank)
  :components ((:file "sbcl-pprint-patch")))

(defsystem :swank-presentation-streams
  :depends-on (#+sbcl :swank/sbcl-pprint-patch)
  :components ((:file "contrib/swank-presentation-streams")))

(defsystem :swank-asdf
  :components ((:file "contrib/swank-asdf")))

(defsystem :swank-package-fu
  :components ((:file "contrib/swank-package-fu")))

(defsystem :swank-hyperdoc
  :components ((:file "contrib/swank-hyperdoc")))

(defsystem :swank-sbcl-exts
  :components ((:file "contrib/swank-sbcl-exts")))

(defsystem :swank-mrepl
  :components ((:file "contrib/swank-mrepl")))

(defsystem :swank-trace-dialog
  :components ((:file "contrib/swank-trace-dialog")))

(defsystem :swank-clipboard
  :components ((:file "contrib/swank-clipboard")))

(defsystem :swank-indentation
  :components ((:file "contrib/swank-indentation")))

(defsystem :swank-listener-hooks
  :components ((:file "contrib/swank-listener-hooks")))

(defsystem :swank-media
  :components ((:file "contrib/swank-media")))

(defsystem :swank-motd
  :components ((:file "contrib/swank-motd")))

(defsystem :swank-snapshot
  :components ((:file "contrib/swank-snapshot")))

(defsystem :swank-sprof
  :components ((:file "contrib/swank-sprof")))

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

