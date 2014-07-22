(require 'sly)
(require 'cl-lib)
(eval-when-compile (require 'cl)) ; lexical-let*

(define-sly-contrib sly-sprof
  "Integration with SBCL's sb-sprof."
  (:authors "Juho Snellman"
            "Stas Boukarev")
  (:license "MIT")
  (:swank-dependencies swank-sprof)
  (:on-load
   (let ((C '(and (sly-connected-p)
              (equal (sly-lisp-implementation-type) "SBCL"))))
     (setf (cdr (last (assoc "Profiling" sly-easy-menu)))
           `("--"
             [ "Start sb-sprof"  sly-sprof-start ,C ]
             [ "Stop sb-sprof"   sly-sprof-stop ,C ]
             [ "Report sb-sprof" sly-sprof-report ,C ])))))

(defvar sly-sprof-exclude-swank nil
  "*Display swank functions in the report.")

(define-derived-mode sly-sprof-browser-mode fundamental-mode
  "slprof"
  "Mode for browsing profiler data\
\\<sly-sprof-browser-mode-map>\
\\{sly-sprof-browser-mode-map}"
  :syntax-table lisp-mode-syntax-table
  (setq buffer-read-only t)
  (sly-mode))

(set-keymap-parent sly-sprof-browser-mode-map sly-mode-map)

(sly-define-keys sly-sprof-browser-mode-map
  ("h" 'describe-mode)
  ("d" 'sly-sprof-browser-disassemble-function)
  ("g" 'sly-sprof-browser-go-to)
  ("v" 'sly-sprof-browser-view-source)
  ("s" 'sly-sprof-toggle-swank-exclusion)
  ((kbd "RET") 'sly-sprof-browser-toggle))

;; Start / stop profiling

(cl-defun sly-sprof-start (&optional (mode :cpu))
  (interactive)
  (sly-eval `(swank:swank-sprof-start :mode ,mode)))

(defun sly-sprof-start-alloc ()
  (interactive)
  (sly-sprof-start :alloc))

(defun sly-sprof-start-time ()
  (interactive)
  (sly-sprof-start :time))

(defun sly-sprof-stop ()
  (interactive)
  (sly-eval `(swank:swank-sprof-stop)))

;; Reporting

(defun sly-sprof-format (graph)
  (with-current-buffer (sly-buffer-name :sprof)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "%4s %-54s %6s %6s %6s\n"
                      "Rank"
                      "Name"
                      "Self%"
                      "Cumul%"
                      "Total%"))
      (dolist (data graph)
        (sly-sprof-browser-insert-line data 54))))
  (forward-line 2))

(cl-defun sly-sprof-update (&optional (exclude-swank sly-sprof-exclude-swank))
  (sly-eval-async `(swank:swank-sprof-get-call-graph
                      :exclude-swank ,exclude-swank)
    'sly-sprof-format))

(defalias 'sly-sprof-browser 'sly-sprof-report)

(defun sly-sprof-report ()
  (interactive)
  (sly-with-popup-buffer ((sly-buffer-name :sprof)
                            :connection t
                            :select t
                            :mode 'sly-sprof-browser-mode)
    (sly-sprof-update)))

(defun sly-sprof-toggle-swank-exclusion ()
  (interactive)
  (setq sly-sprof-exclude-swank
        (not sly-sprof-exclude-swank))
  (sly-sprof-update))

(defun sly-sprof-browser-insert-line (data name-length)
  (cl-destructuring-bind (index name self cumul total)
      data
    (if index
        (insert (format "%-4d " index))
      (insert "     "))
    (sly-insert-propertized
     (sly-sprof-browser-name-properties)
     (format (format "%%-%ds " name-length)
             (sly-sprof-abbreviate-name name name-length)))
    (insert (format "%6.2f " self))
    (when cumul
      (insert (format "%6.2f " cumul))
      (when total
        (insert (format "%6.2f" total))))
    (when index
      (sly-sprof-browser-add-line-text-properties
       `(profile-index ,index expanded nil)))
    (insert "\n")))

(defun sly-sprof-abbreviate-name (name max-length)
  (cl-subseq name 0 (min (length name) max-length)))

;; Expanding / collapsing

(defun sly-sprof-browser-toggle ()
  (interactive)
  (let ((index (get-text-property (point) 'profile-index)))
    (when index
      (save-excursion
        (if (sly-sprof-browser-line-expanded-p)
            (sly-sprof-browser-collapse)
            (sly-sprof-browser-expand))))))

(defun sly-sprof-browser-collapse ()
  (let ((inhibit-read-only t))
    (sly-sprof-browser-add-line-text-properties '(expanded nil))
    (forward-line)
    (cl-loop until (or (eobp)
                       (get-text-property (point) 'profile-index))
             do
             (delete-region (point-at-bol) (point-at-eol))
             (unless (eobp)
               (delete-char 1)))))

(defun sly-sprof-browser-expand ()
  (lexical-let* ((buffer (current-buffer))
                 (point (point))
                 (index (get-text-property point 'profile-index)))
    (sly-eval-async `(swank:swank-sprof-expand-node ,index)
                      (lambda (data)
                        (with-current-buffer buffer
                          (save-excursion 
                            (destructuring-bind (&key callers calls)
                                data
                              (sly-sprof-browser-add-expansion callers
                                                                   "Callers"
                                                                   0)
                              (sly-sprof-browser-add-expansion calls
                                                                   "Calls"
                                                                   0))))))))

(defun sly-sprof-browser-add-expansion (data type nesting)
  (when data
    (let ((inhibit-read-only t))
      (sly-sprof-browser-add-line-text-properties '(expanded t))
      (end-of-line)
      (insert (format "\n     %s" type))
      (dolist (node data)
        (cl-destructuring-bind (index name cumul) node
          (insert (format (format "\n%%%ds" (+ 7 (* 2 nesting))) ""))
          (sly-insert-propertized
           (sly-sprof-browser-name-properties)
           (let ((len (- 59 (* 2 nesting))))
             (format (format "%%-%ds " len)
                     (sly-sprof-abbreviate-name name len))))
          (sly-sprof-browser-add-line-text-properties
           `(profile-sub-index ,index))
          (insert (format "%6.2f" cumul)))))))

(defun sly-sprof-browser-line-expanded-p ()
  (get-text-property (point) 'expanded))

(defun sly-sprof-browser-add-line-text-properties (properties)
  (add-text-properties (point-at-bol)
                       (point-at-eol)
                       properties))

(defun sly-sprof-browser-name-properties ()
  '(face sldb-restart-number-face))

;; "Go to function"

(defun sly-sprof-browser-go-to ()                                           
  (interactive)
  (let ((sub-index (get-text-property (point) 'profile-sub-index)))
    (when sub-index
      (let ((pos (text-property-any
                  (point-min) (point-max) 'profile-index sub-index)))
        (when pos (goto-char pos))))))

;; Disassembly

(defun sly-sprof-browser-disassemble-function ()
  (interactive)
  (let ((index (or (get-text-property (point) 'profile-index)
                   (get-text-property (point) 'profile-sub-index))))
    (when index
      (sly-eval-describe `(swank:swank-sprof-disassemble
                             ,index)))))

;; View source

(defun sly-sprof-browser-view-source ()
  (interactive)
  (let ((index (or (get-text-property (point) 'profile-index)
                   (get-text-property (point) 'profile-sub-index))))
    (when index
      (sly-eval-async
       `(swank:swank-sprof-source-location ,index)
       (lambda (source-location)
         (destructure-case source-location
           ((:error message)
            (message "%s" message)
            (ding))
           (t
            (sly-show-source-location source-location))))))))

(provide 'sly-sprof)
