;;; slime-fontifying-fu.el --- Additional fontification tweaks.
;;
;; Author:  Tobias C. Rittweiler <tcr@freebits.de>
;;
;; License: GNU GPL (same license as Emacs)
;;


;;; Fontify WITH-FOO and DO-FOO like standard macros.
;;; Fontify CHECK-FOO like CHECK-TYPE.
(defvar slime-additional-font-lock-keywords
 '(("(\\(\\(\\s_\\|\\w\\)*:\\(define-\\|do-\\|with-\\)\\(\\s_\\|\\w\\)*\\)" 1 font-lock-keyword-face) 
   ("(\\(\\(define-\\|do-\\|with-\\)\\(\\s_\\|\\w\\)*\\)" 1 font-lock-keyword-face)
   ("(\\(check-\\(\\s_\\|\\w\\)*\\)" 1 font-lock-warning-face)))


;;;; Specially fontify forms suppressed by a reader conditional.

(defcustom slime-highlight-suppressed-forms t
  "Display forms disabled by reader conditionals as comments."
  :type '(choice (const :tag "Enable" t) (const :tag "Disable" nil))
  :group 'slime-mode)

(defface slime-reader-conditional-face
  (if (slime-face-inheritance-possible-p)
    '((t (:inherit font-lock-comment-face)))
  '((((background light)) (:foreground "DimGray" :bold t))
    (((background dark)) (:foreground "LightGray" :bold t))))
  "Face for compiler notes while selected."
  :group 'slime-mode-faces)

(defvar slime-search-suppressed-forms-match-data (list nil nil))

(defun slime-search-suppressed-forms-internal (limit)
  (when (search-forward-regexp slime-reader-conditionals-regexp limit t)
    (let ((start (match-beginning 0))   ; save match data
          (state (slime-current-parser-state)))
      (if (or (nth 3 state) (nth 4 state)) ; inside string or comment?
          (slime-search-suppressed-forms-internal limit)
        (let* ((char (char-before))
               (expr (read (current-buffer)))
               (val  (slime-eval-feature-expression expr)))
          (when (<= (point) limit)
            (if (or (and (eq char ?+) (not val))
                    (and (eq char ?-) val))
                (progn
                  (forward-sexp) (backward-sexp)
                  (slime-forward-sexp)
                  ;; There was an `ignore-errors' form around all this
                  ;; because the following assertion was triggered
                  ;; regularly (resulting in the "non-deterministic"
                  ;; behaviour mentioned in the comment further below.)
                  ;; With extending the region properly, this assertion
                  ;; would truly mean a bug now.
                  (assert (<= (point) limit))
                  (let ((md (match-data nil slime-search-suppressed-forms-match-data)))
                    (setf (first md) start)
                    (setf (second md) (point))
                    (set-match-data md)
                    t))
                (slime-search-suppressed-forms-internal limit))))))))

(defun slime-search-suppressed-forms (limit)
  "Find reader conditionalized forms where the test is false."
  (when (and slime-highlight-suppressed-forms
             (slime-connected-p))
    (condition-case condition
        (slime-search-suppressed-forms-internal limit)
      (end-of-file                      ; e.g. #+(
       nil) 
      ;; We found a reader conditional we couldn't process for some
      ;; reason; however, there may still be other reader conditionals
      ;; before `limit'.
      (invalid-read-syntax              ; e.g. #+#.foo
       (slime-search-suppressed-forms-internal limit))
      (scan-error                       ; e.g. #| #+(or) #|
       (slime-search-suppressed-forms-internal limit)) 
      (slime-unknown-feature-expression ; e.g. #+(foo)
       (slime-search-suppressed-forms-internal limit)) 
      (error 
       (slime-bug 
        (concat "Caught error during fontification while searching for forms\n"
                "that are suppressed by reader-conditionals. The error was: %S.")
        condition)))))


(defun slime-search-directly-preceding-reader-conditional ()
  "Search for a directly preceding reader conditional. Return its
position, or nil."
  ;;; We search for a preceding reader conditional. Then we check that
  ;;; between the reader conditional and the point where we started is
  ;;; no other intervening sexp, and we check that the reader
  ;;; conditional is at the same nesting level.
  (condition-case nil
      (let* ((orig-pt (point)))
        (when-let (reader-conditional-pt 
                   (search-backward-regexp slime-reader-conditionals-regexp
                                           ;; We restrict the search to the
                                           ;; beginning of the /previous/ defun.
                                           (save-excursion (beginning-of-defun) (point))
                                           t))
          (let* ((parser-state 
                  (parse-partial-sexp (progn (goto-char (+ reader-conditional-pt 2))
                                             (forward-sexp) ; skip feature expr.
                                             (point))
                                      orig-pt))
                 (paren-depth  (car  parser-state))
                 (last-sexp-pt (caddr  parser-state)))
            (if (and paren-depth (not (plusp paren-depth)) ; no opening parenthesis in between?
                     (not last-sexp-pt))                   ; no complete sexp in between?
                reader-conditional-pt
                nil))))
    (scan-error nil)))                                     ; improper feature expression


;;; We'll push this onto `font-lock-extend-region-functions'. In past,
;;; we didn't do so which made our reader-conditional font-lock magic
;;; pretty unreliable (it wouldn't highlight all suppressed forms, and
;;; worked quite non-deterministic in general.)
;;;
;;; Cf. _Elisp Manual_, 23.6.10 Multiline Font Lock Constructs.
;;;
;;; We make sure that `font-lock-beg' and `font-lock-end' always point
;;; to the beginning or end of a toplevel form. So we never miss a
;;; reader-conditional, or point in mid of one.
(defun slime-extend-region-for-font-lock ()
  (when slime-highlight-suppressed-forms
    (condition-case c
        (let (changedp)
          (multiple-value-setq (changedp font-lock-beg font-lock-end)
            (slime-compute-region-for-font-lock font-lock-beg font-lock-end))
          changedp)
      (error
       (slime-bug 
        (concat "Caught error when trying to extend the region for fontification.\n"
                "The error was: %S\n"
                "Further: font-lock-beg=%d, font-lock-end=%d.")
        c font-lock-beg font-lock-end)))))

(defun slime-beginning-of-tlf ()
  (let* ((state (slime-current-parser-state))
         (comment-start (nth 8 state)))
    (when comment-start                 ; or string
      (goto-char comment-start)
      (setq state (slime-current-parser-state)))
    (let ((depth (nth 0 state)))
      (if (plusp depth)
          (up-list (- depth))
          (when-let (upper-pt (nth 1 state)) 
            (goto-char upper-pt)
            (while (when-let (upper-pt (nth 1 (slime-current-parser-state)))
                     (goto-char upper-pt))))))))

(defun slime-compute-region-for-font-lock (orig-beg orig-end)
  (let ((beg orig-beg)
        (end orig-end))
    (goto-char beg)
    (inline (slime-beginning-of-tlf))
    (assert (not (plusp (nth 0 (slime-current-parser-state)))))
    (setq beg (let ((pt (point))) 
                (or (slime-search-directly-preceding-reader-conditional)
                    pt)))
    (goto-char end)
    (when (search-backward-regexp slime-reader-conditionals-regexp beg t)
      ;; Nested reader conditionals, yuck!
      (while (when-let (pt (slime-search-directly-preceding-reader-conditional))
               (goto-char pt)))
      (ignore-errors (slime-forward-reader-conditional))
      (setq end (max end (point))))
    (values (or (/= beg orig-beg) (/= end orig-end)) beg end)))


(defun slime-activate-font-lock-magic ()
  (if (featurep 'xemacs)
      (let ((pattern `((slime-search-suppressed-forms
                        (0 slime-reader-conditional-face t)))))
        (dolist (sym '(lisp-font-lock-keywords
                       lisp-font-lock-keywords-1
                       lisp-font-lock-keywords-2))
          (set sym (append (symbol-value sym) pattern))))
      (font-lock-add-keywords
       'lisp-mode
       `((slime-search-suppressed-forms 0 ,''slime-reader-conditional-face t)))

      (add-hook 'lisp-mode-hook 
                #'(lambda () 
                    (add-hook 'font-lock-extend-region-functions
                              'slime-extend-region-for-font-lock t t)))
      ))


(defun slime-fontifying-fu-init ()
  (font-lock-add-keywords
   'lisp-mode slime-additional-font-lock-keywords)
  (when slime-highlight-suppressed-forms
    (slime-activate-font-lock-magic)))

(defun slime-fontifying-fu-unload ()
  (font-lock-remove-keywords 
   'lisp-mode slime-additional-font-lock-keywords)
  ;;; FIXME: remove `slime-search-suppressed-forms', and remove the
  ;;; extend-region hook.
  )

(provide 'slime-fontifying-fu)

(let ((byte-compile-warnings '())) 
  (mapc #'byte-compile
        '(slime-extend-region-for-font-lock
          slime-compute-region-for-font-lock
          slime-search-directly-preceding-reader-conditional
          slime-search-suppressed-forms
          slime-beginning-of-tlf)))
