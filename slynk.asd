;;; -*- lisp -*-
(in-package :asdf)

;; ASDF system definition for loading the Slynk server independently
;; of Emacs.
;;
;; Usage:
;;
;;   (asdf:load-system :slynk)
;;   (slynk:create-server :port PORT) => ACTUAL-PORT
;;
;; (PORT can be zero to mean "any available port".)
;; Then the Slynk server is running on localhost:ACTUAL-PORT. You can
;; use `M-x sly-connect' to connect Emacs to it.
;;
;; This code has been placed in the Public Domain.  All warranties
;; are disclaimed.

(defsystem :slynk
  :components
  ((:module "lib"
    :components
    ((:module "lisp"
      :serial t
      :components
      ((:file "slynk-backend")
       ;; If/when we require ASDF3, we shall use :if-feature instead
       #+(or cmu sbcl scl)
       (:file "slynk-source-path-parser")
       #+(or cmu ecl sbcl scl)
       (:file "slynk-source-file-cache")
       #+clisp
       (:file "xref")
       #+(or clisp clozure)
       (:file "metering")
       (:module "backend"
        :serial t
        :components (#+allegro
                     (:file "slynk-allegro")
                     #+armedbear
                     (:file "slynk-abcl")
                     #+clisp
                     (:file "slynk-clisp")
                     #+clozure
                     (:file "slynk-ccl")
                     #+cmu
                     (:file "slynk-cmucl")
                     #+cormanlisp
                     (:file "slynk-corman")
                     #+ecl
                     (:file "slynk-ecl")
                     #+lispworks
                     (:file "slynk-lispworks")
                     #+sbcl
                     (:file "slynk-sbcl")
                     #+scl
                     (:file "slynk-scl")))
       #+(or sbcl allegro scl cmucl armedbear clisp clozure cormanlisp ecl lispworks)
       (:file "slynk-gray")
       (:file "slynk-match")
       (:file "slynk-rpc")
       (:file "slynk")
       ))))))

(defmethod perform :after ((o load-op) (c (eql (find-system :slynk))))
  (format *error-output* "&SLYNK's ASDF loader finished")
  (funcall (read-from-string "slynk::init")))


;;; Contrib systems (should probably go into their own file one day)
;;;

(defsystem :slynk-util
  :components ((:file "contrib/slynk-util")))

(defsystem :slynk-arglists
  :components ((:file "contrib/slynk-arglists")))

(defsystem :slynk-c-p-c
  :components ((:file "contrib/slynk-c-p-c")))

(defsystem :slynk-fuzzy
  :components ((:file "contrib/slynk-fuzzy")))

(defsystem :slynk-fancy-inspector
  :components ((:file "contrib/slynk-fancy-inspector")))

(defsystem :slynk-asdf
  :components ((:file "contrib/slynk-asdf")))

(defsystem :slynk-package-fu
  :components ((:file "contrib/slynk-package-fu")))

(defsystem :slynk-hyperdoc
  :components ((:file "contrib/slynk-hyperdoc")))

(defsystem :slynk-mrepl
  :components ((:file "contrib/slynk-mrepl")))

(defsystem :slynk-trace-dialog
  :components ((:file "contrib/slynk-trace-dialog")))

(defsystem :slynk-clipboard
  :components ((:file "contrib/slynk-clipboard")))

(defsystem :slynk-indentation
  :components ((:file "contrib/slynk-indentation")))

(asdf:defsystem :slynk-contribs
    :depends-on
  (:slynk
   :slynk-util :slynk-repl
   :slynk-c-p-c :slynk-arglists :slynk-fuzzy
   :slynk-fancy-inspector
   #+(or asdf2 asdf3 sbcl ecl) :slynk-asdf
   :slynk-package-fu
   :slynk-hyperdoc
   #+sbcl :slynk-sbcl-exts
   :slynk-mrepl :slynk-trace-dialog))

