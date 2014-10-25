;;; -*-lexical-binding:t-*-
(require 'sly)
(require 'sly-parse "lib/sly-parse")
(require 'sly-buttons "lib/sly-buttons")
(require 'cl-lib)
(require 'hi-lock) ;; for the faces
(require 'color)

(define-sly-contrib sly-stickers
  "Mark expressions in source buffers and annotate return values."
  (:authors "João Távora <joaotavora@gmail.com>")
  (:license "GPL")
  (:slynk-dependencies slynk-stickers)
  (:on-load (add-hook 'sly-editing-mode-hook 'sly-stickers-enable)
            (add-hook 'sly-mode-hook 'sly-stickers-shortcuts-enable)
            (setq sly-compile-region-function 'sly-stickers-compile-region-aware-of-stickers)
            (add-hook 'sly-compilation-finished-hook 'sly-stickers-after-buffer-compilation t))
  (:on-unload (remove-hook 'sly-editing-mode-hook 'sly-stickers-enable)
              (remove-hook 'sly-mode-hook 'sly-stickers-shortcuts-enable)
              (setq sly-compile-region-function 'sly-compile-region-as-string)
              (remove-hook 'sly-compilation-finished-hook 'sly-stickers-after-buffer-compilation)))

(defgroup sly-stickers nil
  "Mark expressions in source buffers and annotate return values."
  :prefix "sly-stickers-"
  :group 'sly)

(when nil
  (cl-loop for sym in '(sly-stickers-placed-face
                        sly-stickers-armed-face
                        sly-stickers-empty-face
                        sly-stickers-recordings-face
                        sly-stickers-exited-non-locally-face)
           do
           (put sym 'face-defface-spec nil)))

(defface sly-stickers-placed-face
  '((((background dark)) (:background "light grey" :foreground "black"))
    (t (:background "light grey")))
  "Face for sticker just set")

(defface sly-stickers-armed-face
  '((t (:inherit hi-blue)))
  "Face for stickers that have been armed")

(defface sly-stickers-recordings-face
  '((t (:inherit hi-green)))
  "Face for stickers that have new recordings")

(defface sly-stickers-empty-face
  '((t (:inherit hi-pink)))
  "Face for stickers that have no recordings.")

(defface sly-stickers-exited-non-locally-face
  '((t (:strike-through t :inherit sly-stickers-empty-face)))
  "Face for stickers that have exited non-locally.")

(defun sly-stickers-enable ()
  (sly-stickers-mode 1))

(defun sly-stickers-shortcuts-enable ()
  (sly-stickers-shortcut-mode 1))

(defvar sly-stickers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s C-s") 'sly-stickers-dwim)
    map))

(defvar sly-stickers-shortcut-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c S") 'sly-stickers-fetch)
    (define-key map (kbd "C-c C-s S") 'sly-stickers-fetch)
    (define-key map (kbd "C-c C-s C-r") 'sly-stickers-fetch-and-replay)
    map))

(define-minor-mode sly-stickers-mode
  "Mark expression in source buffers and annotate return values.")

(define-minor-mode sly-stickers-shortcut-mode
  "Shortcuts for navigating sticker recordings.")

(sly-def-connection-var sly-stickers--counter 0)

(sly-def-connection-var sly-stickers--stickers (make-hash-table))

(defvar sly-stickers--sticker-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-RET") 'sly-mrepl-copy-part-to-repl)
    (define-key map [down-mouse-3] 'sly-button-popup-part-menu)
    (define-key map [mouse-3] 'sly-button-popup-part-menu)
    map))

;; (sly-button-define-part-action sly-stickers--inspect-sticker "Inspect sticker object" nil)

(define-button-type 'sly-stickers-sticker :supertype 'sly-part
  'sly-button-inspect
  #'(lambda (_id recording-id)
      (unless (and recording-id
                   (cl-plusp recording-id))
        (sly-error "This sticker doesn't seem to have any recordings"))
      (sly-eval-for-inspector
       `(slynk-stickers:inspect-sticker-recording ,recording-id)))
  ;; 'sly-stickers--inspect-sticker
  ;; #'(lambda (id _recording_id)
  ;;     (unless (and id (cl-plusp id))
  ;;       (sly-error "This sticker is not armed yet"))
  ;;     (sly-eval-for-inspector
  ;;      `(slynk-stickers:inspect-sticker ,id)))
  'sly-button-echo 'sly-stickers--echo-sticker
  'keymap sly-stickers--sticker-map)

(defun sly-stickers--set-tooltip (sticker &optional info)
  (let* ((help-base (button-get sticker 'sly-stickers--base-help-echo))
         (text (if info
                   (concat "[sly] " info "\n" help-base)
                 help-base)))
    (button-put sticker 'help-echo text)
    (button-put sticker 'sly-stickers--info info)))

(defun sly-stickers--echo-sticker (sticker &rest more)
  (cl-assert (null more) "Apparently two stickers at exact same location")
  (sly-message (button-get sticker 'sly-stickers--info))
  (sly-button-flash sticker))

(defcustom sly-stickers-max-nested-stickers 4
  "The maximum expected level expected of sticker nesting.
If you nest more than this number of stickers inside other
stickers, the overlay face will be very dark, and probably
render the underlying text unreadable."
  :type :integer)

(defvar sly-stickers-color-face-attribute :background
  "Color-capable attribute of sticker faces that represents nesting.")

(defun sly-stickers--guess-face-color (face)
  (face-attribute-specified-or
   (face-attribute face sly-stickers-color-face-attribute nil t)
   nil))

(defun sly-stickers--set-face (sticker &optional face)
  (let* ((face (or face
                   (button-get sticker 'sly-stickers--base-face)))
         (guessed-color (sly-stickers--guess-face-color face)))
    (button-put sticker 'sly-stickers--base-face face)
    (unless guessed-color
      (sly-error "sorry, can't guess color for face %s for sticker %s"))
    (button-put sticker 'face
                `(:inherit ,face
                           ,sly-stickers-color-face-attribute
                           ,(color-darken-name guessed-color
                                               (* 40
                                                  (/ (sly-button--overlay-priority sticker)
                                                     sly-stickers-max-nested-stickers
                                                     1.0)))))))

(defun sly-stickers--stickers-in (beg end)
  (sly-button--overlays-in beg end 'sly-stickers--sticker-id))
(defun sly-stickers--stickers-at (pos)
  (sly-button--overlays-at pos 'sly-stickers--sticker-id))
(defun sly-stickers--stickers-between (beg end)
  (sly-button--overlays-between beg end 'sly-stickers--sticker-id))
(defun sly-stickers--stickers-exactly-at (beg end)
  (sly-button--overlays-exactly-at beg end 'sly-stickers--sticker-id))


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
           (sticker (make-button from to :type 'sly-stickers-sticker
                                 'part-args (list -1 nil)
                                 'part-label label
                                 'sly-button-search-id (sly-button-next-search-id)
                                 'modification-hooks '(sly-stickers--sticker-modified)
                                 'sly-stickers-id (cl-incf (sly-stickers--counter))
                                 'sly-stickers--base-help-echo
                                 "mouse-3: Context menu")))
      ;; choose a suitable priorty for ourselves and increase the
      ;; priority of those contained by us
      ;;
      (sly-stickers--set-sticker-piority
       sticker
       (1+ (cl-reduce #'max (mapcar #'sly-button--overlay-priority containers)
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
  (let* ((id (sly-stickers--sticker-id sticker))
         (label (format "Sticker %d is armed" id)))
    (button-put sticker 'part-args (list id nil))
    (button-put sticker 'part-label label)
    (button-put sticker 'sly-stickers--last-known-recording nil)
    (sly-stickers--set-tooltip sticker label)
    (sly-stickers--set-face sticker 'sly-stickers-armed-face)
    (puthash id sticker (sly-stickers--stickers))))

(defun sly-stickers--disarm-sticker (sticker)
  (let* ((id (sly-stickers--sticker-id sticker))
         (label (if id
                    (format "Sticker %d failed to stick" id)
                  ;; is brand new, never tried to arm it
                  "Disarmed sticker")))
    (button-put sticker 'part-args (list -1 nil))
    (button-put sticker 'part-label label)
    (sly-stickers--set-tooltip sticker label)
    (sly-stickers--set-face sticker 'sly-stickers-placed-face)))

(defun sly-stickers--populate-sticker (sticker total recording-description)
  (let* ((id (sly-stickers--sticker-id sticker)))
    (cond ((cl-plusp total)
           (button-put sticker 'part-label (format "Sticker %d has new recordings" id))
           (when recording-description
             (cl-destructuring-bind (recording-id description-string exited-non-locally-p)
                 recording-description
               (button-put sticker 'sly-stickers--last-known-recording
                           (cons recording-id description-string))
               (button-put sticker 'part-args (list id recording-id))
               (sly-stickers--set-tooltip sticker
                                          (format "Newest of %s new recordings => %s"
                                                  total
                                                  description-string))
               (sly-stickers--set-face sticker
                                       (if exited-non-locally-p
                                           'sly-stickers-exited-non-locally-face
                                         'sly-stickers-recordings-face)))))
          (t
           (let ((last-known-recording (button-get sticker 'sly-stickers--last-known-recording)))
             (button-put sticker 'part-label (format "Sticker %d has no recordings" id))
             (when last-known-recording
               (sly-stickers--set-tooltip sticker
                                          (format "No new recordings. Last known => %s"
                                                  (cdr last-known-recording))))
             (sly-stickers--set-tooltip sticker "No new recordings")
             (sly-stickers--set-face sticker 'sly-stickers-empty-face))))))

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
                   (remhash id (sly-stickers--stickers))))
             (sly-stickers--stickers))
    dead-ids))

(defun sly-stickers--briefly-describe-sticker (sticker)
  (let ((beg (button-start sticker))
        (end (button-end sticker)))
    (if (< (- end beg) 20)
        (format "sticker around %s" (buffer-substring-no-properties beg end))
      (cl-labels ((word (point direction)
                        (apply #'buffer-substring-no-properties
                               (sort (list
                                      point
                                      (save-excursion (goto-char point)
                                                      (forward-word direction)
                                                      (point)))
                                     #'<))))
        (format "sticker from \"%s...\" to \"...%s\""
                (word beg 1)
                (word end -1))))))

(defun sly-stickers--set-sticker-piority (sticker prio)
  (overlay-put sticker 'priority prio))

(defun sly-stickers--decrease-prio (sticker)
  (let ((prio (sly-button--overlay-priority sticker)))
    (unless (and prio
                 (cl-plusp prio))
      (sly-error "Something's fishy with the sticker priorities"))
    (sly-stickers--set-sticker-piority sticker (cl-decf prio))
    (sly-stickers--set-face sticker)))

(defun sly-stickers--increase-prio (sticker)
  (let ((prio (sly-button--overlay-priority sticker)))
    (sly-stickers--set-sticker-piority sticker (cl-incf prio))
    (sly-stickers--set-face sticker)))

(defun sly-stickers--delete (sticker)
  (mapc #'sly-stickers--decrease-prio
        (sly-stickers--sticker-substickers sticker))
  ;; Notice that we *do* leave the sticker in the
  ;; `sly-stickers--stickers' hash table. This is so that it may be
  ;; reaped later by `sly-stickers--bring-out-yer-dead'
  (delete-overlay sticker))

(defun sly-stickers--sticker-modified (sticker after? _beg _end &optional _pre-change-len)
  (unless after?
    (let ((inhibit-modification-hooks t))
      (sly-message "Deleting %s" (sly-stickers--briefly-describe-sticker sticker))
      (sly-stickers--delete sticker))))

(defun sly-stickers-next-sticker (&optional n)
  (interactive "p")
  (sly-button-search n 'sly-stickers--sticker-id))

(defun sly-stickers-prev-sticker (&optional n)
  (interactive "p")
  (sly-button-search (- n) 'sly-stickers--sticker-id))

(defun sly-stickers-clear-defun-stickers ()
  "Clear all stickers in the current top-level form."
  (interactive)
  (let* ((region (sly-region-for-defun-at-point)))
    (sly-stickers-clear-region-stickers (car region) (cadr region))))

(defun sly-stickers-clear-buffer-stickers ()
  "Clear all the stickers in the current buffer."
  (interactive)
  (sly-stickers-clear-region-stickers (point-min) (point-max)))

(defun sly-stickers-clear-region-stickers (&optional from to)
  "Clear all the stickers between FROM and TO."
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
  "Delete the topmost sticker at point."
  (interactive "d")
  (let ((stickers (sly-stickers--stickers-at (or point (point)))))
    (cond (stickers
           (sly-stickers--delete (car stickers))
           (if (cdr stickers)
               (sly-message "Deleted topmost sticker (%d remain at point)"
                            (length (cdr stickers)))
             (sly-message "Deleted sticker %s"
                          (sly-stickers--briefly-describe-sticker (car stickers)))))
          (t
           (sly-error "No stickers at point")))))

(defun sly-stickers-maybe-add-sticker (&optional point)
  "Add of remove a sticker at POINT.
If point is currently at a sticker boundary, delete that sticker,
otherwise, add a sticker to the sexp at point."
  (interactive "d")
  (save-excursion
    (goto-char (or point (point)))
    (let* ((bounds (sly-bounds-of-sexp-at-point))
           (beg (car bounds))
           (end (cdr bounds))
           (matching (and bounds
                          (sly-stickers--stickers-exactly-at beg end))))
      (cond ((not bounds)
             (sly-message "Nothing here to place sticker on, apparently"))
            (matching
             (sly-stickers--delete (car matching))
             (sly-message "Deleted sticker"))
            (t
             (let ((sticker (sly-stickers--sticker beg end)))
               (sly-message "Added %s" (sly-stickers--briefly-describe-sticker sticker))))))))

(defun sly-stickers-dwim (prefix)
  "Set or remove stickers at point.
Set a sticker for the current sexp at point, or delete it if it
already exists.

If the region is active set a sticker in the current region.

With interactive prefix arg PREFIX always delete stickers.

- One C-u means delete the current top-level form's stickers.
- Two C-u's means delete the current buffer's stickers"
  (interactive "p") 
  (cond
   ((= prefix 4)
    (if (region-active-p)
        (sly-stickers-clear-region-stickers)
      (sly-stickers-clear-defun-stickers)))
   ((>= prefix 16)
    (sly-stickers-clear-buffer-stickers))
   ((region-active-p)
    (sly-stickers--sticker (region-beginning) (region-end))
    (deactivate-mark t))
   ((not (sly-inside-string-or-comment-p))
    (sly-stickers-maybe-add-sticker))
   (t
    (sly-message "No point placing stickers in string literals or comments"))))

(defvar sly-stickers--zombie-sticker-ids nil
  "Sticker ids reported by Lisp that no longer exist in Emacs.")

(defun sly-stickers--kill-zombies ()
  (when sly-stickers--zombie-sticker-ids
    (sly-temp-message 3 1  (format " (killing zombie stickers %s)" sly-stickers--zombie-sticker-ids))
    (sly-eval-async `(slynk-stickers:kill-stickers ',sly-stickers--zombie-sticker-ids))))

(defun sly-stickers--process-sticker-description
    (description &optional pop-to-sticker)
  (cl-destructuring-bind (sticker-id nrecordings &rest recording-description)
      description
    (let* ((sticker (gethash sticker-id (sly-stickers--stickers))))
      (cond ((and sticker (overlay-buffer sticker)
                  (buffer-live-p (overlay-buffer sticker)))
             (when pop-to-sticker
               (pop-to-buffer (overlay-buffer sticker))
               (goto-char (overlay-start sticker))
               (sly-button-flash sticker))
             (sly-stickers--populate-sticker sticker nrecordings recording-description))
            (sticker
             ;; pretty normal so don't add noise
             ;; (sly-message "Sticker %s has been deleted" id)
             )
            (t
             ;; this sticker
             (push sticker-id sly-stickers--zombie-sticker-ids))))))

(defun sly-stickers-fetch-and-replay ()
  "Interactively fetch and replay recordings from stickers"
  (interactive)
  (cl-loop with key = (cl-gensym "sticker-visitor-")
           with ignore-list = nil
           ;; with some-stickers-p = nil
           for (index total . sticker-description) = (sly-eval
                                                        `(slynk-stickers:visit-next ',key ,ignore-list))
           for description = (third sticker-description)
           while index
           do
           ;; (setq some-stickers-p t)
           (sly-stickers--process-sticker-description sticker-description 'pop-to-sticker)
           while (y-or-n-p (format "[sly] (%s/%s) => %s\nContinue?"
                                   index total description))
           finally
           (cond ;; (some-stickers-p
                 ;;  (when (y-or-n-p "[sly] forget about all recordings?")
                 ;;    (sly-eval '(slynk-stickers:forget))))
                 (t
                  (sly-message "no sticker recordings")))
           (sly-stickers--kill-zombies)))

(defun sly-stickers-fetch ()
  "Fetch and update stickers from Lisp."
  (interactive)
  (sly-eval-async `(slynk-stickers:fetch)
    #'(lambda (result)
        (let ((message (format "Fetched recordings for %s armed stickers" (length result))))
          (cl-loop for sticker-description in result
                   do (sly-stickers--process-sticker-description sticker-description))
          (sly-message message))
        (sly-stickers--kill-zombies))
    "CL_USER"))

(cl-defun sly-stickers--compile-region-aware-of-stickers-1
    (start end callback &key sync fallback flash)
  "Compile from START to END considering stickers.
After compilation call CALLBACK with the stickers and the
compilation result.  If SYNC, use `sly-eval' other wise use
`sly-eval-async'.  If FALLBACK, send the uninstrumneted region as
a fallback.  If FLASH, flash the compiled region."
  (let* ((uninstrumented (buffer-substring-no-properties start end))
         (stickers (sly-stickers--stickers-between start end))
         (dead-ids (append (mapcar #'sly-stickers--sticker-id stickers)
                           (sly-stickers--bring-out-yer-dead)))
         (original-buffer (current-buffer)))
    (cond (stickers
           (when flash
             (sly-flash-region start end :face 'sly-stickers-armed-face))
           (sly-with-popup-buffer ((sly-buffer-name :stickers :hidden t)
                                   :select :hidden)
             (mapc #'delete-overlay (overlays-in (point-min) (point-max)))
             (insert uninstrumented)
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
             ;; Now send both the instrumented and uninstrumented
             ;; string to the Lisp
             ;;
             (let ((instrumented (buffer-substring-no-properties (point-min) (point-max)))
                   (new-ids (mapcar #'sly-stickers--sticker-id stickers)))
               (with-current-buffer original-buffer
                 (let ((form `(slynk-stickers:compile-for-stickers
                               ',new-ids
                               ',dead-ids
                               ,instrumented
                               ,(when fallback uninstrumented)
                               ,(buffer-name)
                               ',(sly-compilation-position start)
                               ,(if (buffer-file-name) (sly-to-lisp-filename (buffer-file-name)))
                               ',sly-compilation-policy)))
                   (if sync
                       (funcall callback
                                stickers
                                (sly-eval form))
                       (sly-eval-async form
                         (lambda (result)
                           (funcall callback stickers result)))))))))
          (t
           (sly-compile-region-as-string start end)))))

(defun sly-stickers-compile-region-aware-of-stickers (start end)
  "Compile region from START to END aware of stickers.
Intended to be placed in `sly-compile-region-function'"
  (sly-stickers--compile-region-aware-of-stickers-1
   start end
   (lambda (stickers result-and-stuck-p)
     (cl-destructuring-bind (result &optional stuck-p)
         result-and-stuck-p
       (unless stuck-p
         (mapc #'sly-stickers--disarm-sticker stickers))
       (sly-compilation-finished
        result
        nil
        (if stuck-p
            (format " (%d stickers armed)" (length stickers))
          " (stickers failed to stick)"))))
   :fallback t
   :flash t))

(defun sly-stickers-after-buffer-compilation (success _notes buffer loadp)
  "After compilation, compile regions with stickers.
Intented to be placed in `sly-compilation-finished-hook'"
  (when (and buffer loadp success)
    (save-restriction
      (widen)
      (let* ((all-stickers (sly-stickers--stickers-between (point-min) (point-max)))
             (regions (cl-loop for sticker in all-stickers
                               for region = (sly-region-for-defun-at-point (overlay-start sticker))
                               unless (member region regions)
                               collect region into regions
                               finally (cl-return regions))))
        (when regions
          (cl-loop with successful
                   with unsuccessful
                   for region in regions
                   do
                   (sly-stickers--compile-region-aware-of-stickers-1
                    (car region) (cadr region)
                    (lambda (stickers result)
                      (cond (result
                             (push (cons region stickers) successful))
                            (t
                             (mapc #'sly-stickers--disarm-sticker stickers)
                             (push (cons region stickers) unsuccessful))))
                    :sync t)
                   finally
                   (sly-temp-message
                    3 3
                    "%s stickers stuck in %s regions, %s disarmed in %s regions"
                    (cl-reduce #'+ successful :key (lambda (x) (length (cdr x))))
                    (length successful)
                    (cl-reduce #'+ unsuccessful :key (lambda (x) (length (cdr x))))
                    (length unsuccessful))))))))

(provide 'sly-stickers)
