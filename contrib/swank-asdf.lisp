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
  asdf:*central-registry*)

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
  ;; ugh, yeah, it's unexported - but do we really expect this to
  ;; change anytime soon?
  (loop for name being the hash-keys of asdf::*defined-systems*
        collect name))

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
  (gethash 'asdf:load-op
           (asdf::component-operation-times (asdf:find-system name))))

(defslimefun asdf-system-directory (name)
  (cl:directory-namestring
   (cl:truename
    (asdf:system-definition-pathname (asdf:find-system name)))))

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
    (loop with pathname      = (pathname file)
          with pathname-name = (pathname-name pathname)
          for (nil . system) being the hash-value of asdf::*defined-systems*
          when (system-contains-file-p system pathname pathname-name)
            do (return-from asdf-determine-system
                 (asdf:component-name system))))
  ;; If we couldn't find a system by that, we now try if there's a
  ;; system that's named like BUFFER-PACKAGE-NAME.
  (let ((package (guess-buffer-package buffer-package-name)))
    (dolist (name (package-names package))
      (let ((system (asdf:find-system (string-downcase name) nil)))
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

(defmethod asdf:operation-done-p asdf:around ((operation asdf:compile-op)
                                              component)
  (unless (eql *recompile-system*
               (asdf:component-system component))
    (call-next-method)))

(defslimefun reload-system (name)
  (let* ((system (asdf:find-system name))
         (*recompile-system* system))
    (collect-notes
     (lambda ()
       (handler-case
           (with-compilation-hooks ()
             (asdf:oos 'asdf:load-op system)
             t)
         (asdf:compile-error () nil))))))

(provide :swank-asdf)
