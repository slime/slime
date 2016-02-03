;;; swank-quicklisp.lisp -- Quicklisp support
;;
;; Authors: Matthew Kennedy <burnsidemk@gmail.com>
;; License: Public Domain
;;

(in-package :swank)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (member :quicklisp *features*)
    (error "Could not find Quicklisp already loaded.")))

(defslimefun list-quicklisp-systems ()
  "Returns the Quicklisp systems list."
  (mapcar #'ql-dist:name (ql:system-list)))

(provide :swank-quicklisp)
