;;; swank-sbcl-exts.lisp --- Misc extensions for SBCL
;;
;; Authors: Tobias C. Rittweiler <tcr@freebits.de>
;;
;; License: Public Domain
;;

(in-package :swank)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (swank-require :swank-arglists))

;; We need to do this so users can place `slime-sbcl-exts' into their
;; ~/.emacs, and still use any implementation they want.
#+sbcl
(progn

(defun find-instruction (name)
  #+(and
     #.(swank/backend:with-symbol '*inst-encoder* 'sb-assem)
     #.(swank/backend:with-symbol '*backend-instruction-set-package* 'sb-assem))
  (and name
       (or (gethash (find-symbol name sb-assem::*backend-instruction-set-package*)
                    sb-assem::*inst-encoder*)
           (find-symbol (format nil "M:~A" name)
                        sb-assem::*backend-instruction-set-package*))))

;;; Display arglist of instructions.
;;;
(defmethod compute-enriched-decoded-arglist ((operator-form (eql 'sb-assem:inst))
                                             argument-forms)
  (flet ((decode-instruction-arglist (instr-name instr-arglist)
           (let ((decoded-arglist (decode-arglist instr-arglist)))
             ;; The arglist of INST is (instruction ...INSTR-ARGLIST...).
             (push 'sb-assem::instruction (arglist.required-args decoded-arglist))
             (values decoded-arglist
                     (list (string-downcase instr-name))
                     t))))
    (if (null argument-forms)
        (call-next-method)
        (destructuring-bind (instruction &rest args) argument-forms
          (declare (ignore args))
          (let* ((instr-name
                   (typecase instruction
                     (arglist-dummy
                      (string-upcase (arglist-dummy.string-representation instruction)))
                     (symbol
                      (string-upcase instruction))))
                 (instr-fn
                   (find-instruction instr-name)))
            (when (consp instr-fn)
              (setf instr-fn (car instr-fn)))
            (cond ((functionp instr-fn)
                   (with-available-arglist (arglist) (arglist instr-fn)
                     (decode-instruction-arglist instr-name (cdr arglist))))
                  ((fboundp instr-fn)
                   (with-available-arglist (arglist) (arglist instr-fn)
                     ;; SB-ASSEM:INST invokes a symbolic INSTR-FN with
                     ;; current segment and current vop implicitly.
                     (decode-instruction-arglist instr-name
                                                 (if (or (get instr-fn :macro)
                                                         (macro-function instr-fn))
                                                     arglist
                                                     (cdr arglist)))))
                  (t
                   (call-next-method))))))))

(defslimefun inst-location (name)
  (let ((inst (find-instruction (string-upcase name))))
    (cond ((functionp inst)
           (find-definition-for-thing inst))
          ((fboundp inst)
           (let ((macro (macro-function inst)))
             (when macro
              (find-definition-for-thing macro)))))))
) ; PROGN

(provide :swank-sbcl-exts)
