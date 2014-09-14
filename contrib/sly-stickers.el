;;; -*-lexical-binding:t-*-
(require 'sly)
(require 'sly-parse)
(require 'cl-lib)
(require 'hi-lock) ;; for the faces
(require 'color)

(define-sly-contrib sly-stickers
  "Mark expressions in source buffers and annotate return values."
  (:authors "João Távora <joaotavora@gmail.com>")
  (:license "GPL")
  (:slynk-dependencies slynk-stickers)
  (:on-load (add-hook 'sly-editing-mode-hook 'sly-stickers-enable)
            (add-hook 'sly-after-compile-functions 'sly-stickers-commit-stickers))
  (:on-unload (remove-hook 'sly-editing-mode-hook 'sly-stickers-enable)
              (remove-hook 'sly-after-compile-functions 'sly-stickers-commit-stickers)))

(defgroup sly-stickers nil
  "Mark expressions in source buffers and annotate return values."
  :prefix "sly-stickers-"
  :group 'sly)

(defface sly-stickers-placed-face
  '((t (:inherit hi-blue)))
  "Face for sticker just set")

(defface sly-stickers-armed-face
  '((t (:inherit hi-green)))
  "Face for sticker just set")

(defface sly-stickers-lit-face
  '((t (:inherit hi-pink)))
  "Face for sticker just set")

(defun sly-stickers-enable () (sly-stickers-mode 1))

(defvar sly-stickers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s C-s") 'sly-stickers-dwim)
    (define-key map (kbd "C-c C-s S") 'sly-stickers-fetch)
    map))

(define-minor-mode sly-stickers-mode
  "Mark expression in source buffers and annotate return values.")

(defun sly-stickers--stickers-in (beg end)
  "Return stickers overlapping positions BEG and END"
  (cl-remove-if-not #'(lambda (button)
                        (eq (button-type button) 'sly-stickers--sticker))
                    (overlays-in beg end)))

(defun sly-stickers--stickers-between (beg end)
  "Return stickers contained entirely between BEG and END"
  (cl-remove-if-not #'(lambda (button)
                        (and (>= (button-start button) beg)
                             (<= (button-end button) end)))
                    (sly-stickers--stickers-in beg end)))

(defun sly-stickers--stickers-exactly-at (beg end)
  "Return stickers exactly between BEG and END"
  (cl-remove-if-not #'(lambda (button)
                        (and (= (button-start button) beg)
                             (= (button-end button) end)))
                    (sly-stickers--stickers-in beg end)))

(defun sly-stickers--stickers-at (&optional point)
  "Return stickers near POINT"
  (interactive "d")
  (let ((point (or point (point))))
    (cl-sort (sly-stickers--stickers-in (1- point) (1+ point))
             #'> :key #'sly-stickers--sticker-priority)))

(defvar sly-stickers--counter 0)

(defvar sly-stickers--stickers (make-hash-table))

(defvar sly-stickers--sticker-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-RET") 'sly-mrepl-copy-part-to-repl)
    (define-key map [down-mouse-3] 'sly-button-popup-part-menu)
    (define-key map [mouse-3] 'sly-button-popup-part-menu)
    map))

(define-button-type 'sly-stickers--sticker :supertype 'sly-part
  'sly-button-inspect
  #'(lambda (id)
      (when (cl-minusp id)
        (sly-error "This sticker is not armed yet"))
      (sly-eval-for-inspector
       `(slynk-stickers:inspect-sticker-values ,id)))
  'sly-mrepl-copy-part-to-repl
  #'(lambda (_id)
      (error "Copy to REPL not implemented yet!"))
  'keymap sly-stickers--sticker-map)

(defun sly-stickers--set-tooltip (sticker &optional text)
  (let* ((help-base (button-get sticker 'sly-stickers--base-help-echo))
         (text (if text
                   (concat "[sly] " text "\n" help-base)
                 help-base)))
    (button-put sticker 'help-echo text)))

(defun sly-stickers--set-face (sticker &optional face)
  (let ((face (or face
                  (button-get sticker 'sly-stickers--base-face))))
    (button-put sticker 'sly-stickers--base-face face)
    (button-put sticker 'face
                `(:inherit ,face
                  :background
                  ,(color-darken-name (face-background face nil t)
                                      (* (sly-stickers--sticker-priority sticker)
                                         15))))))

(defun sly-stickers--flash-sticker (sticker)
  (sly-flash-region (button-start sticker) (button-end sticker)
                    :timeout 0.07
                    :times 2))

(defun sly-stickers--sticker (from to)
  (let* ((intersecting (sly-stickers--stickers-in from to))
         (contained (sly-stickers--stickers-between from to))
         (not-contained (cl-set-difference intersecting contained))
         (containers nil))
    (unless (cl-every #'(lambda (e)
                          (and (< (button-start e) from)
                               (> (button-end e) to)))
                      not-contained)
      (sly-error "Cannot place a sticker that partially overlaps other stickers"))
    (when (sly-stickers--stickers-exactly-at from to)
      (sly-error "There is already a sticker at those very coordinates"))
    ;; by now we know that other intersecting, non-contained stickers
    ;; are our containers.
    ;; 
    (setq containers not-contained)
    (let* ((label "Brand new sticker")
           (sticker (make-button from to :type 'sly-stickers--sticker
                                 'part-args (list -1)
                                 'part-label label
                                 'modification-hooks '(sly-stickers--sticker-modified)
                                 'sly-stickers--base-help-echo
                                 (button-type-get 'sly-stickers--sticker
                                                  'help-echo))))
      ;; choose a suitable priorty for ourselves and increase the
      ;; priority of those contained by us
      ;;
      (sly-stickers--set-sticker-piority
       sticker
       (1+ (cl-reduce #'max (mapcar #'sly-stickers--sticker-priority containers)
                      :initial-value -1)))
      (mapc #'sly-stickers--increase-prio contained)
      ;; finally, set face
      ;;
      (sly-stickers--set-tooltip sticker label)
      (sly-stickers--set-face sticker 'sly-stickers-placed-face)
      sticker)))

(defun sly-stickers--sticker-id (sticker)
  (button-get sticker 'sly-stickers-id))

(defun sly-stickers--arm-sticker (sticker)
  (let* ((id (incf sly-stickers--counter))
         (label (format "Sticker %d is armed" id)))
    (button-put sticker 'part-args (list id))
    (button-put sticker 'part-label label)
    (button-put sticker 'sly-stickers-id id)
    (button-put sticker 'sly-stickers-values nil)
    (sly-stickers--set-tooltip sticker label)
    (sly-stickers--set-face sticker 'sly-stickers-armed-face)
    (puthash id sticker sly-stickers--stickers)))

(defun sly-stickers--disarm-sticker (sticker)
  (let* ((id (sly-stickers--sticker-id sticker))
         (label (format "Sticker %d failed to stick" id)))
    (button-put sticker 'part-args (list -1))
    (button-put sticker 'part-label label)
    (sly-stickers--set-tooltip sticker label)
    (sly-stickers--set-face sticker 'sly-stickers-placed-face)
    (remhash id sly-stickers--stickers)))

(defun sly-stickers--populate-sticker (sticker values)
  (let* ((id (sly-stickers--sticker-id sticker))
         (values (append
                  (button-get sticker 'sly-stickers-values)
                  values)))
    (button-put sticker 'part-label (format "Sticker %d has %d recordings" id (length values)))
    (button-put sticker 'sly-stickers-values values)
    (sly-stickers--set-tooltip sticker
                               (format "%s recordings. Last value => %s"
                                       (length values)
                                       (first (car (last values)))))
    (sly-stickers--set-face sticker 'sly-stickers-lit-face)))

(defun sly-stickers--sticker-substickers (sticker)
  (let* ((retval
          (remove sticker
                  (sly-stickers--stickers-between (button-start sticker) (button-end sticker))))
         ;; To verify an important invariant, and warn (don't crash)
         ;; 
         (exactly-at
          (sly-stickers--stickers-exactly-at (button-start sticker) (button-end sticker))))
    (cond ((remove sticker exactly-at)
           (sly-warning "Something's fishy. More than one sticker at same position")
           (cl-set-difference retval exactly-at))
          (t
           retval))))

(defun sly-stickers--bring-out-yer-dead ()
  (let ((dead-ids))
    (maphash #'(lambda (id sticker)
                 ;; Put me down! I'm not dead
                 (unless (overlay-buffer sticker)
                   (push id dead-ids)
                   (remhash id sly-stickers--stickers)))
             sly-stickers--stickers)))

(defun sly-stickers--sticker-priority (sticker)
  (or (overlay-get sticker 'priority) 0))

(defun sly-stickers--set-sticker-piority (sticker prio)
  (overlay-put sticker 'priority prio))

(defun sly-stickers--decrease-prio (sticker)
  (mapc #'sly-stickers--decrease-prio
        (sly-stickers--sticker-substickers sticker))
  (let ((prio (sly-stickers--sticker-priority sticker)))
    (unless (and prio
                 (cl-plusp prio))
      (sly-error "Something's fishy with the sticker priorities"))
    (sly-stickers--set-sticker-piority sticker (decf prio))
    (sly-stickers--set-face sticker)))

(defun sly-stickers--increase-prio (sticker)
  (mapc #'sly-stickers--increase-prio
        (sly-stickers--sticker-substickers sticker))
  (let ((prio (sly-stickers--sticker-priority sticker)))
    (sly-stickers--set-sticker-piority sticker (incf prio))
    (sly-stickers--set-face sticker)))

(defun sly-stickers--delete (sticker)
  (mapc #'sly-stickers--decrease-prio
        (sly-stickers--sticker-substickers sticker))
  (if (sly-stickers--sticker-id sticker)
      (remhash (sly-stickers--sticker-id sticker) sly-stickers--stickers))
  (delete-overlay sticker))

(defun sly-stickers--sticker-modified (sticker after? _beg _end &optional _pre-change-len)
  (unless after?
    (let ((inhibit-modification-hooks t))
      (sly-message "Deleting sticker from %d to %d" (button-start sticker) (button-end sticker))
      (sly-stickers--delete sticker))))

(defun sly-stickers-clear-defun-stickers ()
  (interactive)
  (let* ((region (sly-region-for-defun-at-point)))
    (sly-stickers-clear-region-stickers (car region) (cadr region))))

(defun sly-stickers-clear-buffer-stickers ()
  (interactive)
  (sly-stickers-clear-region-stickers (point-min) (point-max)))

(defun sly-stickers-clear-region-stickers (&optional from to)
  (interactive "r")
  (let* ((from (or from (region-beginning)))
         (to (or to (region-end)))
         (stickers (sly-stickers--stickers-in from to)))
    (cond (stickers
           (mapc #'sly-stickers--delete stickers)
           (sly-message "%s stickers cleared" (length stickers)))
          (t
           (sly-message "no stickers to clear")))))

(defun sly-stickers-delete-sticker-at-point (&optional point)
  (interactive "d")
  (let ((stickers (sly-stickers--stickers-at (or point (point)))))
    (cond (stickers
           (sly-stickers--delete (car stickers))
           (if (cdr stickers)
               (sly-message "Deleted topmost sticker (%d remain at point)"
                            (length (cdr stickers)))
             (sly-message "Deleted sticker")))
          (t
           (sly-error "No stickers at point")))))

(defun sly-stickers-maybe-add-sticker (&optional point)
  (interactive "d")
  (save-excursion
    (goto-char (or point (point)))
    (let* ((bounds (sly-bounds-of-sexp-at-point))
           (matching (and bounds
                          (sly-stickers--stickers-exactly-at (car bounds) (cdr bounds)))))
      (cond ((not bounds)
             (sly-message "Nothing here to place sticker on, apparently"))
            (matching
             (sly-stickers--delete (car matching))
             (sly-message "Deleted sticker"))
            (t
             (sly-stickers--sticker (car bounds) (cdr bounds)))))))

(defun sly-stickers-dwim (prefix)
  "Set or remove stickers at point.
Set a sticker for the current sexp at point, or delete it if it
already exists.

If the region is active set a sticker in the current region.

With interactive prefix arg PREFIX always delete stickers.

- One C-u means delete the topmost sticker at point.
- Two C-u's means delete the current top-level form's stickers.
- Three C-u's means delete the current buffer's stickers"
  (interactive "p") 
  (cond
   ((= prefix 4)
    (sly-stickers-delete-sticker-at-point))
   ((= prefix 16)
    (if (region-active-p)
        (sly-stickers-clear-region-stickers)
      (sly-stickers-clear-defun-stickers)))
   ((>= prefix 64)
    (sly-stickers-clear-buffer-stickers))
   ((region-active-p)
    (sly-stickers--sticker (region-beginning) (region-end))
    (deactivate-mark t))
   ((not (sly-inside-string-or-comment-p))
    (sly-stickers-maybe-add-sticker))
   (t
    (sly-message "No point placing stickers in string literals or comments"))))

(defun sly-stickers-fetch ()
  (interactive)
  (sly-eval-async `(slynk-stickers:check-stickers)
    #'(lambda (result)
        (cl-loop for (id . values) in result
                 for sticker = (gethash id sly-stickers--stickers)
                 do (cond ((and sticker (overlay-buffer sticker))
                           (sly-stickers--flash-sticker sticker)
                           (when values
                             (sly-stickers--populate-sticker sticker values)))
                          (sticker
                           (sly-message "Sticker %s has been deleted" id))
                          ;; this is slightly more serious, so warn
                          (t
                           (sly-warning "Ooops sticker %s is not live anymore" id)))))))

(defun sly-stickers-commit-stickers (_result arg1 arg2)
  (let* ((original-buffer (current-buffer))
         (bufferp (bufferp arg1))
         (start (if bufferp (point-min) arg1))
         (end (if bufferp (point-max) arg2))
         (string (buffer-substring-no-properties start end))
         (stickers (sly-stickers--stickers-between start end))
         (dead-ids (append (mapcar #'sly-stickers--sticker-id stickers)
                           (sly-stickers--bring-out-yer-dead)))
         (current-package (sly-current-package)))
    (when stickers
      (if bufferp
          (sly-warning "Compiling buffers with stickers not supported yet.")
        (sly-with-popup-buffer ((sly-buffer-name :stickers :hidden t)
                                :select :hidden)
          (mapc #'delete-overlay (overlays-in (point-min) (point-max)))
          (insert string)
          ;; Use a second set of overlays placed just in the
          ;; pre-compilation buffer. We need this to correctly keep
          ;; track of the markers because in this buffer we are going
          ;; to change actual text
          ;; 
          (cl-loop for sticker in stickers
                   for overlay = (make-overlay (- (button-start sticker) (1- start))
                                               (- (button-end sticker) (1- start)))
                   do (overlay-put overlay 'sly-stickers--sticker sticker))
          (cl-loop for overlay in (overlays-in (point-min) (point-max))
                   for sticker = (overlay-get overlay 'sly-stickers--sticker)
                   do
                   (sly-stickers--arm-sticker sticker)
                   (goto-char (overlay-start overlay))
                   (insert (format "(slynk-stickers:record %d " (sly-stickers--sticker-id sticker)))
                   (goto-char (overlay-end overlay))
                   (insert ")"))
          (let ((instrumented (buffer-substring-no-properties (point-min) (point-max)))
                (new-ids (mapcar #'sly-stickers--sticker-id stickers)))
            (with-current-buffer original-buffer
              (sly-eval-async `(slynk-stickers:compile-for-stickers
                                ',new-ids
                                ',dead-ids
                                ,instrumented
                                ,(buffer-name)
                                ',(sly-compilation-position start)
                                ,(if (buffer-file-name) (sly-to-lisp-filename (buffer-file-name)))
                                ',sly-compilation-policy)
                #'(lambda (ids)
                    (cond ((null ids)
                           (sly-message "Some stickers failed to stick. Probably misplaced")
                           (mapc #'sly-stickers--disarm-sticker stickers))
                          (t
                           (cl-assert (equal ids new-ids)))))
                current-package))))))))

(provide 'sly-stickers)
