;;; swank-quicklisp.lisp -- Quicklisp support
;;
;; Authors: Matthew Kennedy <burnsidemk@gmail.com>
;; License: Public Domain
;;

(in-package :swank)

(defslimefun list-quicklisp-systems ()
  "Returns the Quicklisp systems list."
  (if (member :quicklisp *features*)
      (let ((ql-dist-name (find-symbol "NAME" "QL-DIST"))
            (ql-system-list (find-symbol "SYSTEM-LIST" "QL")))
        (mapcar ql-dist-name (funcall ql-system-list)))
      (error "Could not find Quicklisp already loaded.")))

(defun read-system-index-file (path)
  "Read system-index.txt from the path and return the list of asd file
paths."
  (labels ((recur (stream acc)
             (let ((line (read-line stream nil nil)))
               (if line
                   (recur stream (cons line acc))
                   (nreverse acc)))))
    (with-open-file (ins (merge-pathnames "system-index.txt" path)
                         :if-does-not-exist nil)
      (when ins
        (mapcar #'(lambda (x) (merge-pathnames x path))
                (recur ins nil))))))

(defun defsystem-expression-p (expr)
  "Return T if the given expression is a defsystem expression."
  (when (consp expr)
    (let ((first-symbol (car expr)))
      (and (symbolp first-symbol)
           (string= (symbol-name first-symbol) "DEFSYSTEM")))))

(defun extract-system-names (asd-file)
  "Given asd file path, read the asd file and return the system names
it describes."
  (labels ((recur (stream acc)
             (let ((expr (read stream nil stream)))
               (cond ((streamp expr)
                      (nreverse acc))
                     ((defsystem-expression-p expr)
                      (recur stream
                             (cons (string-downcase
                                    (if (symbolp (cadr expr))
                                        (symbol-name (cadr expr))
                                        (cadr expr)))
                                   acc)))
                     (t
                      (recur stream acc))))))
    (with-open-file (ins asd-file :if-does-not-exist nil)
      (when ins
        (recur ins nil)))))

(defslimefun list-quicklisp-local-systems
    (&optional (local-system-dirs
                (symbol-value
                 (find-symbol "*LOCAL-PROJECT-DIRECTORIES*" "QL"))))
  "Returns the Quicklisp local systems list."
  (mapcan #'extract-system-names
          (mapcan #'read-system-index-file local-system-dirs)))

(provide :swank-quicklisp)
