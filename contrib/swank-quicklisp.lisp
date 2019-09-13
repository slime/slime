;;; swank-quicklisp.lisp -- Quicklisp support
;;
;; Authors: Matthew Kennedy <burnsidemk@gmail.com>
;; License: Public Domain
;;

(in-package :swank)

(defslimefun list-quicklisp-systems (with-local-systems)
  "Returns the Quicklisp systems list."
  (if (member :quicklisp *features*)
      (let ((ql-dist-name (find-symbol "NAME" "QL-DIST"))
            (ql-system-list (find-symbol "SYSTEM-LIST" "QL"))
            (ql-local-system-list
             (find-symbol "LIST-LOCAL-SYSTEMS" "QL")))
        (append (when with-local-systems
                  (funcall ql-local-system-list))
                (mapcar ql-dist-name (funcall ql-system-list))))
      (error "Could not find Quicklisp already loaded.")))

(provide :swank-quicklisp)
