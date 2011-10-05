;;; swank-asdf.el -- ASDF support
;;
;; Authors: Daniel Barlow  <dan@telent.net>
;;          Marco Baringer <mb@bese.it>
;;          Edi Weitz <edi@agharta.de>
;;          and others 
;; License: Public Domain
;;

(in-package :swank)

#-asdf
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(defun find-operation (operation)
  (or (find-symbol (symbol-name operation) :asdf)
      (error "Couldn't find ASDF operation ~S" operation)))

(defun map-defined-systems (fn)
  (loop for (nil . system) being the hash-values in asdf::*defined-systems*
        do (funcall fn system)))

;;; This is probably a crude hack, see ASDF's LP #481187.
(defslimefun who-depends-on (system)
  (flet ((system-dependencies (op system)
           (mapcar #'(lambda (dep)
                       (asdf::coerce-name (if (consp dep) (second dep) dep)))
                   (cdr (assoc op (asdf:component-depends-on op system))))))
    (let ((system-name (asdf::coerce-name system))
          (result))
      (map-defined-systems
       #'(lambda (system)
           (when (member system-name
                         (system-dependencies 'asdf:load-op system)
                         :test #'string=)
             (push (asdf:component-name system) result))))
      result)))

(defmethod xref-doit ((type (eql :depends-on)) thing)
  (when (typep thing '(or string symbol))
    (loop for dependency in (who-depends-on thing)
          for asd-file = (asdf:system-definition-pathname dependency)
          when asd-file
          collect (list dependency
                        (swank-backend::make-location
                         `(:file ,(namestring asd-file))
                         `(:position 1)
                         `(:snippet ,(format nil "(defsystem :~A" dependency)
                           :align t))))))


(defslimefun operate-on-system-for-emacs (system-name operation &rest keywords)
  "Compile and load SYSTEM using ASDF.
Record compiler notes signalled as `compiler-condition's."
  (collect-notes
   (lambda ()
     (apply #'operate-on-system system-name operation keywords))))

(defun operate-on-system (system-name operation-name &rest keyword-args)
  "Perform OPERATION-NAME on SYSTEM-NAME using ASDF.
The KEYWORD-ARGS are passed on to the operation.
Example:
\(operate-on-system \"swank\" 'compile-op :force t)"
  (handler-case
      (with-compilation-hooks ()
	(apply #'asdf:operate (find-operation operation-name)
               system-name keyword-args)
        t)
    (asdf:compile-error () nil)))

(defun asdf-central-registry ()
  (append asdf:*central-registry*
          #+asdf2 (asdf:ensure-source-registry)))

(defslimefun list-all-systems-in-central-registry ()
  "Returns a list of all systems in ASDF's central registry."
  (mapcar #'pathname-name
          (delete-duplicates
           (loop for dir in (asdf-central-registry)
                 for defaults = (eval dir)
                 when defaults
                   nconc (mapcar #'file-namestring
                                   (directory
                                     (make-pathname :defaults defaults
                                          :version :newest
                                          :type "asd"
                                          :name :wild
                                          :case :local))))
           :test #'string=)))

(defslimefun list-all-systems-known-to-asdf ()
  "Returns a list of all systems ASDF knows already."
  (let ((result))
    (map-defined-systems
     #'(lambda (system) (push (asdf:component-name system) result)))
    result))

(defslimefun list-asdf-systems ()
  "Returns the systems in ASDF's central registry and those which ASDF
already knows."
  (nunion (list-all-systems-known-to-asdf)
          (list-all-systems-in-central-registry)
          :test #'string=))

(defun asdf-module-files (module)
  (mapcan (lambda (component)
            (typecase component
              (asdf:source-file
               (list (asdf:component-pathname component)))
              (asdf:module
               (asdf-module-files component))))
          (asdf:module-components module)))

(defun asdf-module-output-files (module)
  (mapcan (lambda (component)
            (typecase component
              (asdf:source-file
               (asdf:output-files (make-instance 'asdf:compile-op)
                                  component))
              (asdf:module
               (asdf-module-output-files component))))
          (asdf:module-components module)))

(defslimefun asdf-system-files (name)
  (let* ((system (asdf:find-system name))
         (files (mapcar #'namestring
                        (cons
                         (asdf:system-definition-pathname system)
                         (asdf-module-files system))))
         (main-file (find name files
                          :test #'equalp :key #'pathname-name :start 1)))
    (if main-file
        (cons main-file (remove main-file files
                                :test #'equal :count 1))
        files)))

(defslimefun asdf-system-loaded-p (name)
  (and (gethash 'asdf:load-op
                (asdf::component-operation-times (asdf:find-system name)))
       t))

(defslimefun asdf-system-directory (name)
  (let ((truename
          (truename
           (asdf:system-definition-pathname (asdf:find-system name)))))
    (namestring
     (make-pathname :device (pathname-device truename)
                    :directory (pathname-directory truename)))))

(defun system-contains-file-p (module pathname pathname-name)
  (some #'(lambda (component)
	    (typecase component
	      (asdf:cl-source-file 
	       ;; We first compare the relative names because
	       ;; retrieving the full pathname is somewhat costy; this
	       ;; function is called a lot, and its performance
	       ;; translates directly into response time to the user.
	       (and (equal pathname-name
			   (pathname-name
			    (asdf:component-relative-pathname component)))
		    (equal pathname (asdf:component-pathname component))))
	      (asdf:module 
	       (system-contains-file-p component pathname pathname-name))))
	(asdf:module-components module)))

(defslimefun asdf-determine-system (file buffer-package-name)
  ;; First try to grovel through all defined systems to find a system
  ;; which contains FILE.
  (when file
    (let* ((pathname      (pathname file))
           (pathname-name (pathname-name pathname)))
      (map-defined-systems
       #'(lambda (system)
           (when (system-contains-file-p system pathname pathname-name)
             (return-from asdf-determine-system
               (asdf:component-name system)))))))
  ;; If we couldn't find a system by that, we now try if there's a
  ;; system that's named like BUFFER-PACKAGE-NAME.
  (let ((package (guess-buffer-package buffer-package-name)))
    (dolist (name (package-names package))
      (let ((system (asdf:find-system (asdf::coerce-name name) nil)))
        (when system
          (return-from asdf-determine-system
            (asdf:component-name system)))))))

(defslimefun delete-system-fasls (name)
  (let ((removed-count
         (loop for file in (asdf-module-output-files (asdf:find-system name))
               when (probe-file file) count it
               and do (delete-file file))))
    (format nil "~d file~:p ~:*~[were~;was~:;were~] removed" removed-count)))

(defvar *recompile-system* nil)

(defmethod asdf:operation-done-p
    #+#.(swank-backend:with-symbol 'around 'asdf) asdf:around
    #-#.(swank-backend:with-symbol 'around 'asdf) :around
    ((operation asdf:compile-op)
     component)
    (unless (eql *recompile-system*
                 (asdf:component-system component))
      (call-next-method)))

(defslimefun reload-system (name)
  (let ((*recompile-system* (asdf:find-system name)))
    (operate-on-system-for-emacs name 'asdf:load-op)))

;; Doing list-all-systems-in-central-registry might be quite slow
;; since it accesses a file-system, so run it once at the background
;; to initialize caches.
(eval-when (:load-toplevel :execute)
  (when (eql *communication-style* :spawn)
    (spawn (lambda ()
             (ignore-errors (list-all-systems-in-central-registry)))
           :name "init-asdf-fs-caches")))

(provide :swank-asdf)
