;;; swank-sbcl-exts.lisp --- Misc extensions for SBCL
;;
;; Authors: Tobias C. Rittweiler <tcr@freebits.de>
;;
;; License: Public Domain
;;

(in-package :swank)

(swank-require :swank-arglists)

;; We need to do this so users can place `slime-sbcl-exts' into their
;; ~/.emacs, and still use any implementation they want.
#+sbcl
(progn
  
;;; Display arglist of instructions.
;;;
(defmethod compute-enriched-decoded-arglist ((operator-form (eql 'sb-assem:inst))
                                             argument-forms)
  (if (null argument-forms)
      (call-next-method)
      (destructuring-bind (instruction &rest args) argument-forms
        (declare (ignore args))
        (let* ((instr-name
                (if (arglist-dummy-p instruction)
                    (string-upcase (arglist-dummy.string-representation instruction))
                    (symbol-name instruction)))
               (instr-fn (gethash instr-name sb-assem:*assem-instructions*)))
          (if (not instr-fn)
              (call-next-method)
              (with-available-arglist (instr-arglist) (arglist instr-fn)
                (let ((decoded-arglist (decode-arglist instr-arglist)))
                  ;; The arglist of INST is (instruction ...INSTR-ARGLIST...).
                  (push 'sb-assem::instruction (arglist.required-args decoded-arglist))
                  (values decoded-arglist
                          (list instr-name)
                          t))))))))


) ; PROGN

(provide :swank-sbcl-exts)
