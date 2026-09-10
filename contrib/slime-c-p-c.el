(require 'slime)
(require 'cl-lib)

(defvar slime-c-p-c-init-undo-stack nil)

(define-slime-contrib slime-c-p-c
  "ILISP style Compound Prefix Completion."
  (:authors "Luke Gorrie  <luke@synap.se>"
            "Edi Weitz  <edi@agharta.de>"
            "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>"
            "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:slime-dependencies slime-parse slime-editing-commands slime-autodoc)
  (:swank-dependencies swank-c-p-c)
  (:on-load
   (push
    `(progn
       ,@(when (featurep 'slime-repl)
               `((define-key slime-mode-map "\C-c\C-s"
                   ',(lookup-key slime-mode-map "\C-c\C-s"))
                 (define-key slime-repl-mode-map "\C-c\C-s"
                   ',(lookup-key slime-repl-mode-map "\C-c\C-s")))))
    slime-c-p-c-init-undo-stack)
   (define-key slime-mode-map "\C-c\C-s" 'slime-complete-form)
   (when (featurep 'slime-repl)
     (define-key slime-repl-mode-map "\C-c\C-s" 'slime-complete-form)))
  (:on-unload
   (while slime-c-p-c-init-undo-stack
     (eval (pop slime-c-p-c-init-undo-stack)))))


(defun slime-complete-symbol* ()
  "Expand abbreviations and complete the symbol at point."
  ;; NB: It is only the name part of the symbol that we actually want
  ;; to complete -- the package prefix, if given, is just context.
  (or (slime-filename-completion)
      (slime-expand-abbreviations-and-complete)))

(defun slime-c-p-c-completion-at-point ()
  (slime-complete-symbol*))

(defun slime-format-completions (completions)
  (list
   (cl-loop for (symbol-name classification-string symbol) in completions
            collect (propertize symbol-name
                                'slime-kind classification-string
                                'slime-symbol symbol)) 
   :company-kind (lambda (x)
                   (let ((prop (get-text-property 0 'slime-kind x)))
                     (when prop
                       (cl-loop for (char kind) in '((?g method)
                                                     (?f function)
                                                     (?b variable)
                                                     (?c class)
                                                     (?t class)
                                                     (?p module))
                                when (cl-find char prop)
                                return kind))))
   :company-docsig (lambda (x)
                     (let ((sym (get-text-property 0 'slime-symbol x)))
                       (when sym
                         (slime-eval `(swank:operator-arglist ,sym ,(slime-current-package))))))
   :company-doc-buffer (lambda (x)
                         (let ((sym (get-text-property 0 'slime-symbol x)))
                           (when sym
                             (slime-eval-describe `(swank:describe-symbol ,sym) nil)
                             (slime-buffer-name :description))))
   :annotation-function
   (lambda (x)
     (let ((kind (get-text-property 0 'slime-kind x)))
       (when kind
         (concat " " kind))))))

(defun slime-expand-abbreviations-and-complete ()
  (let* ((end (move-marker (make-marker) (slime-symbol-end-pos)))
         (beg (move-marker (make-marker) (slime-symbol-start-pos)))
         (prefix (buffer-substring-no-properties beg end)))
    (cl-list* beg end 
              (slime-format-completions (slime-contextual-completions beg end)))))

(cl-defun slime-contextual-completions (beg end)
  "Return a list of completions of the token from BEG to END in the
current buffer."
  (let ((token (buffer-substring-no-properties beg end)))
    (cond
     ((and (< beg (point-max))
           (string= (buffer-substring-no-properties beg (1+ beg)) ":"))
      ;; Contextual keyword completion
      (let ((completions 
             (slime-completions-for-keyword token
                                            (save-excursion 
                                              (goto-char beg)
                                              (slime-parse-form-upto-point)))))
        (when (cl-first completions)
          (cl-return-from slime-contextual-completions completions))
        ;; If no matching keyword was found, do regular symbol
        ;; completion.
        ))
     ((and (>= (length token) 2)
           (string= (cl-subseq token 0 2) "#\\"))
      ;; Character name completion
      (cl-return-from slime-contextual-completions
        (slime-completions-for-character token))))
    ;; Regular symbol completion
    (slime-completions token)))

(defun slime-completions (prefix)
  (slime-eval `(swank:completions ,prefix ',(slime-current-package))))

(defun slime-completions-for-keyword (prefix buffer-form)
  (slime-eval `(swank:completions-for-keyword ,prefix ',buffer-form)))

(defun slime-completions-for-character (prefix)
  (cl-labels ((append-char-syntax (string) (concat "#\\" string)))
    (let ((result (slime-eval `(swank:completions-for-character
                                ,(cl-subseq prefix 2)))))
      (when (car result)
        (list (mapcar #'append-char-syntax (car result))
              (append-char-syntax (cadr result)))))))


;;; Complete form

(defun slime-complete-form ()
  "Complete the form at point.  
This is a superset of the functionality of `slime-insert-arglist'."
  (interactive)
  ;; Find the (possibly incomplete) form around point.
  (let ((buffer-form (slime-parse-form-upto-point)))
    (let ((result (slime-eval `(swank:complete-form ',buffer-form))))
      (if (eq result :not-available)
          (error "Could not generate completion for the form `%s'" buffer-form)
          (progn
            (just-one-space (if (looking-back "\\s(" (1- (point)))
                                0
                                1))
            (save-excursion
              (insert result)
              (let ((slime-close-parens-limit 1))
                (slime-close-all-parens-in-sexp)))
            (save-excursion
              (backward-up-list 1)
              (indent-sexp)))))))

(provide 'slime-c-p-c)

