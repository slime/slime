(require 'sly)
(require 'cl-lib)

(define-sly-contrib sly-c-p-c
  "ILISP style Compound Prefix Completion."
  (:authors "Luke Gorrie  <luke@synap.se>"
            "Edi Weitz  <edi@agharta.de>"
            "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>"
            "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:sly-dependencies sly-parse sly-autodoc)
  (:swank-dependencies swank-c-p-c)
  (:on-load
   (setq sly-complete-symbol-function 'sly-complete-symbol*)
   (define-key sly-mode-map "\C-c\C-s" 'sly-complete-form))
  (:on-unload
   ;; FIXME: To properly support unloading, this contrib should be
   ;; made a minor mode with it's own keymap. The minor mode
   ;; activation function should be added to the proper sly-* hooks.
   ;; 
   ))

(defcustom sly-c-p-c-unambiguous-prefix-p t
  "If true, set point after the unambigous prefix.
If false, move point to the end of the inserted text."
  :type 'boolean
  :group 'sly-ui)

(defun sly-complete-symbol* ()
  "Expand abbreviations and complete the symbol at point."
  ;; NB: It is only the name part of the symbol that we actually want
  ;; to complete -- the package prefix, if given, is just context.
  (or (sly-maybe-complete-as-filename)
      (sly-expand-abbreviations-and-complete)))

;; FIXME: factorize
(defun sly-expand-abbreviations-and-complete ()
  (let* ((end (move-marker (make-marker) (sly-symbol-end-pos)))
         (beg (move-marker (make-marker) (sly-symbol-start-pos)))
         (prefix (buffer-substring-no-properties beg end))
         (completion-result (sly-contextual-completions beg end))
         (completion-set (cl-first completion-result))
         (completed-prefix (cl-second completion-result)))
    (if (null completion-set)
        (progn (sly-minibuffer-respecting-message
                "Can't find completion for \"%s\"" prefix)
               (ding)
               (sly-complete-restore-window-configuration))
      ;; some XEmacs issue makes this distinction necessary
      (cond ((> (length completed-prefix) (- end beg))
	     (goto-char end)
	     (insert-and-inherit completed-prefix)
	     (delete-region beg end)
	     (goto-char (+ beg (length completed-prefix))))
	    (t nil))
      (cond ((and (member completed-prefix completion-set)
                  (sly-length= completion-set 1))
             (sly-minibuffer-respecting-message "Sole completion")
             (sly-complete-restore-window-configuration))
            ;; Incomplete
            (t
             (when (member completed-prefix completion-set)
               (sly-minibuffer-respecting-message 
                "Complete but not unique"))
	     (when sly-c-p-c-unambiguous-prefix-p
	       (let ((unambiguous-completion-length
		      (cl-loop for c in completion-set
                               minimizing (or (cl-mismatch completed-prefix c)
                                              (length completed-prefix)))))
		 (goto-char (+ beg unambiguous-completion-length))))
             (sly-display-or-scroll-completions completion-set 
                                                completed-prefix))))))

(cl-defun sly-contextual-completions (beg end) 
  "Return a list of completions of the token from BEG to END in the
current buffer."
  (let ((token (buffer-substring-no-properties beg end)))
    (cond
     ((and (< beg (point-max))
           (string= (buffer-substring-no-properties beg (1+ beg)) ":"))
      ;; Contextual keyword completion
      (let ((completions 
             (sly-completions-for-keyword token
                                          (save-excursion 
                                            (goto-char beg)
                                            (sly-parse-form-upto-point)))))
        (when (cl-first completions)
          (cl-return-from sly-contextual-completions completions))
        ;; If no matching keyword was found, do regular symbol
        ;; completion.
        ))
     ((and (>= (length token) 2)
           (string= (cl-subseq token 0 2) "#\\"))
      ;; Character name completion
      (cl-return-from sly-contextual-completions
        (sly-completions-for-character token))))
    ;; Regular symbol completion
    (sly-completions token)))

(defun sly-completions (prefix)
  (sly-eval `(swank:completions ,prefix ',(sly-current-package))))

(defun sly-completions-for-keyword (prefix buffer-form)
  (sly-eval `(swank:completions-for-keyword ,prefix ',buffer-form)))

(defun sly-completions-for-character (prefix)
  (cl-labels ((append-char-syntax (string) (concat "#\\" string)))
    (let ((result (sly-eval `(swank:completions-for-character
                                ,(cl-subseq prefix 2)))))
      (when (car result)
        (list (mapcar 'append-char-syntax (car result))
              (append-char-syntax (cadr result)))))))


;;; Complete form

(defun sly-complete-form ()
  "Complete the form at point.  
This is a superset of the functionality of `sly-insert-arglist'."
  (interactive)
  ;; Find the (possibly incomplete) form around point.
  (let ((buffer-form (sly-parse-form-upto-point)))
    (let ((result (sly-eval `(swank:complete-form ',buffer-form))))
      (if (eq result :not-available)
          (error "Could not generate completion for the form `%s'" buffer-form)
          (progn
            (just-one-space (if (looking-back "\\s(" (1- (point)))
                                0
                                1))
            (save-excursion
              (insert result))
            (save-excursion
              (backward-up-list 1)
              (indent-sexp)))))))

(provide 'sly-c-p-c)

