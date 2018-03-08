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

(defpackage #:swank-loader
  (:use #:common-lisp #:asdf)
  (:export #:init #:*source-directory*))
(in-package #:swank-loader)

(defun init (&key delete)
  (asdf:load-system :swank :force reload))

;; Gets initialized in asdf-init.lisp
(defvar *source-directory*)

(defun load-user-init-file ()
  "Load the user init file, return NIL if it does not exist."
  (load (uiop:subpathname (user-homedir-pathname) ".swank.lisp")
        :if-does-not-exist nil))

(defvar *source-directory*)

(defclass no-load-file (cl-source-file) ())

(defmethod perform ((op load-op) (c no-load-file)) nil)

(defmacro define-swank-system ((sysdep-files impl-files))
  `(defsystem :swank
     :description "Swank is the Common Lisp back-end to SLIME"
     :serial t
     :components ((:file "packages")
                  (:file "swank-backend" :pathname "swank/backend")
                  (:file "nregex")
                  ,@(mapcar (lambda (component)
                              `(:file ,component :pathname ,(concatenate 'string "swank/" component)))
                            sysdep-files)
                  (:file "swank-match" :pathname "swank/match")
                  (:file "swank-rpc" :pathname "swank/rpc")
                  (:file "swank")
                  ,@(mapcar (lambda (component)
                              `(:file ,component :pathname ,(concatenate 'string "swank/" component)))
                            impl-files)
                  (:file "asdf-init")
                  (:module "contrib"
                   :components ((:no-load-file "swank-util")
                                (:no-load-file "swank-c-p-c"
                                 :depends-on ("swank-util"))
                                (:no-load-file "swank-arglists"
                                 :depends-on ("swank-c-p-c"))
                                (:no-load-file "swank-asdf")
                                (:no-load-file "swank-clipboard")
                                (:no-load-file "swank-fancy-inspector"
                                 :depends-on ("swank-util"))
                                (:no-load-file "swank-fuzzy"
                                 :depends-on ("swank-util" "swank-c-p-c"))
                                (:no-load-file "swank-hyperdoc")
                                (:no-load-file "swank-indentation")
                                (:no-load-file "swank-repl")
                                (:no-load-file "swank-macrostep")
                                (:no-load-file "swank-mrepl")
                                (:no-load-file "swank-listener-hooks"
                                 :depends-on ("swank-repl"))
                                (:no-load-file "swank-media")
                                (:no-load-file "swank-package-fu")
                                (:no-load-file "swank-presentations"
                                 :depends-on ("swank-repl"))
                                (:no-load-file "swank-quicklisp")
                                (:no-load-file "swank-presentation-streams"
                                 :depends-on ("swank-presentations"))
                                (:no-load-file "swank-sbcl-exts"
                                 :depends-on ("swank-arglists"))
                                (:no-load-file "swank-snapshot")
                                (:no-load-file "swank-sprof"))))
     :depends-on (#+sbcl #:sb-bsd-sockets)
     :perform (load-op :after (op swank)
                (load-user-init-file))))

#+(or allegro armedbear clozurecl openmcl clisp cmu cormanlisp ecl lispworks sbcl scl)
(define-swank-system
  #+allegro (() ("allegro" "gray"))
  #+armedbear (() ("abcl"))
  #+(or clozurecl openmcl) (("metering" ) ("ccl" "gray"))
  #+clisp (("xref" "metering") ("clisp" "gray"))
  #+cmu (("source-path-parser" "source-file-cache") ("cmucl" "gray"))
  #+cormanlisp (() ("corman" "gray"))
  #+ecl (("source-path-parser" "source-file-cache") ("ecl" "gray"))
  #+lispworks (() ("lispworks" "gray"))
  #+sbcl (("source-path-parser" "source-file-cache") ("sbcl" "gray"))
  #+scl (("source-path-parser" "source-file-cache") ("scl" "gray")))

#-(or allegro armedbear clozurecl openmcl clisp cmu cormanlisp ecl lispworks sbcl scl)
(error "Your CL implementation is not supported !")
