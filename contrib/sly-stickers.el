;;; sly-stickers.el --- Live-code annotations for SLY  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  João Távora

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: convenience, languages, lisp, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; TODO:
;;
;; Save and restore stickers on connection change.
;;
;; Breaking stickers

;;; Code:


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
  '((t (:strike-through nil :inherit hi-blue)))
  "Face for stickers that have been armed")

(defface sly-stickers-recordings-face
  '((t (:strike-through nil :inherit hi-green)))
  "Face for stickers that have new recordings")

(defface sly-stickers-empty-face
  '((t (:strike-through nil :inherit hi-pink)))
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
    (define-key map (kbd "C-c C-s F") 'sly-stickers-forget)
    (define-key map (kbd "C-c C-s C-r") 'sly-stickers-replay)
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
  "Place a new sticker from FROM to TO"
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
         (label (format "Sticker %d failed to stick" id)))
    (button-put sticker 'part-args (list -1 nil))
    (button-put sticker 'part-label label)
    (sly-stickers--set-tooltip sticker label)
    (sly-stickers--set-face sticker 'sly-stickers-placed-face)))

(defun sly-stickers--recording-pretty-description (recording)
  (let ((descs (sly-stickers--recording-value-descriptions recording)))
    (cond ((sly-stickers--recording-exited-non-locally-p recording)
           "exited non locally")
          ((null descs)
           "no values")
          (t
           (cl-loop for (v . rest) on descs
                    concat (format "=> %s" v)
                    when rest
                    concat "\n")))))

(defun sly-stickers--populate-sticker (sticker recording)
  (let* ((id (sly-stickers--sticker-id sticker))
         (total (sly-stickers--recording-sticker-total recording)))
    (cond ((cl-plusp total)
           (button-put sticker 'part-label (format "Sticker %d has %d recordings" id total))
           (unless (sly-stickers--recording-void-p recording)
             (button-put sticker 'sly-stickers--last-known-recording recording)
             (button-put sticker 'part-args (list id
                                                  (sly-stickers--recording-id recording)))
             (sly-stickers--set-tooltip sticker
                                        (format "Newest of %s recordings:\n%s"
                                                total
                                                (sly-stickers--recording-pretty-description recording)))
             (sly-stickers--set-face sticker
                                     (if (sly-stickers--recording-exited-non-locally-p recording)
                                         'sly-stickers-exited-non-locally-face
                                       'sly-stickers-recordings-face))))
          (t
           (let ((last-known-recording (button-get sticker 'sly-stickers--last-known-recording)))
             (button-put sticker 'part-label (format "Sticker %d has no recordings" id))
             (when last-known-recording
               (sly-stickers--set-tooltip sticker
                                          (format "No new recordings. Last known:\n%s"
                                                  (sly-stickers--recording-pretty-description last-known-recording))))
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
  ;; Delete the overlay and take care of priorities for contained and
  ;; containers, but note that a sticker might have no buffer anymore
  ;; if that buffer was killed, for example...
  ;; 
  (when (and (overlay-buffer sticker)
             (buffer-live-p (overlay-buffer sticker)))
    (mapc #'sly-stickers--decrease-prio
          (sly-stickers--sticker-substickers sticker))
    (delete-overlay sticker))
  ;; We also want to deregister it from the hashtable in case it's
  ;; there (it's not there if it has never been armed)
  ;; 
  (let ((id (sly-stickers--sticker-id sticker)))
    (when (gethash (sly-stickers--sticker-id sticker)
                   (sly-stickers--stickers))
      (remhash id (sly-stickers--stickers))
      (add-to-list 'sly-stickers--zombie-sticker-ids id))))

(defun sly-stickers--sticker-modified (sticker _after? beg end &optional _pre-change-len)
  (unless (save-excursion
            (goto-char beg)
            (skip-chars-forward "\t\n\s")
            (>= (point) end))
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
  "Sticker ids that might exist in Slynk but no longer in Emacs.")

(defun sly-stickers--kill-zombies ()
  (when sly-stickers--zombie-sticker-ids
    (sly-temp-message 3 1  (format " (killing zombie stickers %s)" sly-stickers--zombie-sticker-ids))
    (sly-eval-async `(slynk-stickers:kill-stickers ',sly-stickers--zombie-sticker-ids)
      #'(lambda (_result)
          (setq sly-stickers--zombie-sticker-ids nil)))))

(cl-defstruct (sly-stickers--recording
               (:constructor sly-stickers--make-recording-1)
               (:conc-name sly-stickers--recording-)
               (:copier sly-stickers--copy-recording))
  (sticker-id nil)
  (sticker-total nil)
  (id nil)
  (value-descriptions nil)
  (exited-non-locally-p nil))

(defun sly-stickers--recording-void-p (recording)
  (not (sly-stickers--recording-id recording)))

(defun sly-stickers--make-recording (description)
  "Make a `sly-stickers--recording' from DESCRIPTION.
A DESCRIPTION is how the Lisp side describes a sticker and
usually its most recent recording. If it doesn't, a recording
veryfying `sly-stickers--recording-void-p' is created."
  (cl-destructuring-bind (sticker-id sticker-total . recording-description)
      description
    (let ((recording (sly-stickers--make-recording-1
                      :sticker-id sticker-id
                      :sticker-total sticker-total)))
      (when recording-description
        (cl-destructuring-bind (recording-id value-descriptions exited-non-locally-p)
            recording-description
          (setf
           (sly-stickers--recording-id recording) recording-id
           (sly-stickers--recording-value-descriptions recording) value-descriptions
           (sly-stickers--recording-exited-non-locally-p recording) exited-non-locally-p)))
      recording)))

(defun sly-stickers--process-recording
    (recording &optional pop-to-sticker)
  (let* ((sticker-id (sly-stickers--recording-sticker-id recording))
         (sticker (gethash sticker-id (sly-stickers--stickers))))
    (cond ((and sticker (overlay-buffer sticker)
                (buffer-live-p (overlay-buffer sticker)))
           (when pop-to-sticker
             (pop-to-buffer (overlay-buffer sticker))
             (let ((orig (point)))
               (goto-char (overlay-start sticker))
               (sly-recenter orig)))
           (sly-button-flash sticker)
           (sly-stickers--populate-sticker sticker recording)
           t)
          (sticker
           ;; The recording mentions a sticker that hasn't been
           ;; deleted but whose overlay can't be found. So delete it
           ;; now and mark it a zombie.
           (sly-stickers--delete sticker)
           nil)
          (t
           ;; The sticker isn't in the `sly-stickers--stickers' hash
           ;; table, so it has probably already been marked zombie,
           ;; and possibly already deleted. We're probably just seeing
           ;; it because recording playback doesn't filter these
           ;; out. So add the id to the table anyway.
           ;; 
           (add-to-list 'sly-stickers--zombie-sticker-ids sticker-id)
           nil))))



;;; Replaying sticker recordings
;;; 
(defvar sly-stickers--replay-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") :next)
    (define-key map (kbd "SPC") :next)
    (define-key map (kbd "DEL") :prev)
    (define-key map (kbd "p") :prev)
    (define-key map (kbd "j") 'jump)
    (define-key map (kbd "h") 'sly-stickers--replay-help)
    (define-key map (kbd "C-h") 'sly-stickers--replay-help)
    (define-key map (kbd "q") 'quit)
    (define-key map (kbd "C-g") 'quit)
    (define-key map (kbd "i") 'ignore-sticker)
    (define-key map (kbd "R") 'reset-ignore-list)
    (define-key map (kbd "M-RET") 'sly-mrepl-copy-part-to-repl)
    map))

(defun sly-stickers--replay-help ()
  (sly-with-popup-buffer ("sly-stickers help" :mode 'help-mode)
    (insert (mapconcat #'identity
                       '("n, SPC         scan recordings forward"
                         "p, DEL         scan recordings backward"
                         "i              ignore current sticker"
                         "R              reset ignore list"
                         "h, C-h         this help"
                         "j              jump to recording"
                         "M-RET          return sticker values to REPL"
                         "q, C-g         quit")
                       "\n"))))

(cl-defstruct (sly-stickers--replay-state
               (:constructor sly-stickers--make-state)
               (:conc-name sly-stickers--state-)
               (:copier sly-stickers--copy-state))
  (key (cl-gensym "sticker-visitor-"))
  (binding 0)
  ignore-list
  (error nil)
  (total 0)
  (recording nil))

(defun sly-stickers--replay-prompt (state)
  "Produce a prompt and status string for STATE."
  (let* ((ignore-list (sly-stickers--state-ignore-list state))
         (recording (sly-stickers--state-recording state))
         (error (sly-stickers--state-error state)))
    (format "[sly] recording %s of %s in sticker %s\n  %s\n%s%s%s"
            (1+ (sly-stickers--recording-id recording))
            (sly-stickers--state-total state)
            (sly-stickers--recording-sticker-id recording)
            (replace-regexp-in-string
             "\n"
             "\n  "
             (sly-stickers--recording-pretty-description recording))
            (if error
                (format "error: %s\n" error)
              "")
            (if ignore-list
                (format "ignoring sticker%s %s.\n"
                        (if (cadr ignore-list) "s" "")
                        (concat (mapconcat #'pp-to-string
                                           (butlast ignore-list)
                                           ", ")
                                (and (cadr ignore-list) " and ")
                                (pp-to-string
                                 (car (last ignore-list)))))
              "")
            "(n)ext)/(p)revious/(i)gnore/h(elp)/(q)uit")))

(defun sly-stickers--replay-read-binding (state)
  "Read a binding from the user and modify STATE."
  (let ((prompt (sly-stickers--replay-prompt state))
        (recording (sly-stickers--state-recording state)))
    (setf (sly-stickers--state-binding state)
          (lookup-key sly-stickers--replay-map (vector (read-key prompt)) t))
    (let ((binding (sly-stickers--state-binding state)))
      (cond ((and (symbolp binding)
                  (fboundp binding)
                  (string-match "^sly-" (symbol-name binding)))
             (if (commandp binding)
                 (call-interactively binding)
               (funcall binding))
             (setf binding 'repeat))
            ((eq binding 'ignore-sticker)
             (push (sly-stickers--recording-sticker-id recording)
                   (sly-stickers--state-ignore-list state)))
            ((eq binding 'reset-ignore-list)
             (setf (sly-stickers--state-ignore-list state) nil))
            ((eq binding 'jump)
             (let ((read (read-number (format "Jump to which recording [1-%d]?"
                                              (sly-stickers--state-total state)))))
               (setq binding (1- (round read))))))
      (setf (sly-stickers--state-binding state) binding))
    state))

(defun sly-stickers--replay-starting-state ()
  (sly-stickers--make-state))

(defun sly-stickers--replay-fetch-next (state)
  "Update STATE from Slynk according to its bindings."
  (let ((result (sly-eval
                 `(slynk-stickers:search-for-recording
                   ',(sly-stickers--state-key state)
                   ',(sly-stickers--state-ignore-list state)
                   ,(sly-stickers--state-binding state)))))
    (cond ((car result)
           (setf (sly-stickers--state-error state) nil)
           (cl-destructuring-bind (total . sticker-description)
               result
             (setf (sly-stickers--state-recording state)
                   (sly-stickers--make-recording sticker-description))
             (setf (sly-stickers--state-total state) total)
             ;; Assert that the recording isn't void
             ;; 
             (when (sly-stickers--recording-void-p
                    (sly-stickers--state-recording state))
               (sly-error "Attempt to visit a void recording described by %s"
                          sticker-description))))
          (t
           (setf (sly-stickers--state-error state) (cadr result))))
    state))


(defun sly-stickers--replay-quit-state-p (state)
  (or
   (not (sly-stickers--state-recording state))
   (eq (sly-stickers--state-binding state) 'quit)))

(defvar sly-stickers--replay-last-state nil)

(defun sly-stickers-replay ()
  "Interactively replay sticker recordings fetched from Slynk.
See also `sly-stickers-fetch'."
  (interactive)
  (let ((state (sly-stickers--make-state)))
    (when sly-stickers--replay-last-state
      (let ((recording (sly-stickers--state-recording
                        sly-stickers--replay-last-state)))
        (and recording
             (y-or-n-p (format "[sly] Resume from last recording %s of sticker %s?"
                               (1+ (sly-stickers--recording-id recording))
                               (sly-stickers--recording-sticker-id recording))))
        (setf (sly-stickers--state-recording state) recording)
        (setf (sly-stickers--state-binding state) (sly-stickers--recording-id recording))))
    (unwind-protect
        (cl-loop for next-state = (if (or (memq (sly-stickers--state-binding state)
                                                '(:next :prev))
                                          (numberp (sly-stickers--state-binding state)))
                                      (sly-stickers--replay-fetch-next state)
                                    state)
                 
                 while (not (sly-stickers--replay-quit-state-p state))
                 do
                 ;; pop to and flash the sticker whatever the state was
                 ;;
                 (unless (sly-stickers--process-recording (sly-stickers--state-recording next-state)
                                                          'pop-to-sticker)
                   (setf (sly-stickers--state-error state)
                         "Can't find sticker (probably deleted!)"))
                 (setq state (sly-stickers--replay-read-binding next-state))
                 until (sly-stickers--replay-quit-state-p state))
      (cond ((sly-stickers--state-recording state)
             (setq sly-stickers--replay-last-state state)
             (sly-message "Quit sticker recording replay"))
            (t
             (sly-message "No sticker recordings. Run some sticker-aware code first.")))
      (sly-stickers--kill-zombies))))

(defun sly-stickers-fetch ()
  "Fetch recordings from Slynk and update stickers accordingly.
See also `sly-stickers-replay'."
  (interactive)
  (sly-eval-async `(slynk-stickers:fetch)
    #'(lambda (result)
        (let ((message (format "Fetched recordings for %s armed stickers" (length result))))
          (cl-loop for sticker-description in result
                   ;; Although we are analysing sticker descriptions
                   ;; here, recordings are made to pass to
                   ;; `sly-stickers--process-recording', although they
                   ;; possibly are `sly-stickers--recording-void-p' if
                   ;; the sticker has never been traversed.
                   do (sly-stickers--process-recording
                       (sly-stickers--make-recording sticker-description)))
          (sly-message message))
        (sly-stickers--kill-zombies))
    "CL_USER"))

(defun sly-stickers-forget ()
  "Forget about sticker recordings in the Slynk side."
  (interactive)
  (when (yes-or-no-p "[sly] Really forget about sticker recordings?")
    (sly-eval-async '(slynk-stickers:forget)
      #'(lambda (_result)
          (setq sly-stickers--replay-last-state nil)
          (sly-message "Forgot all about sticker recordings.")))))


;;; Sticker-aware compilation
;;; 

(cl-defun sly-stickers--compile-region-aware-of-stickers-1
    (start end callback &key sync fallback flash)
  "Compile from START to END considering stickers.
After compilation call CALLBACK with the stickers and the
compilation result.  If SYNC, use `sly-eval' other wise use
`sly-eval-async'.  If FALLBACK, send the uninstrumneted region as
a fallback.  If FLASH, flash the compiled region."
  (let* ((uninstrumented (buffer-substring-no-properties start end))
         (stickers (sly-stickers--stickers-between start end))
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
                               ',sly-stickers--zombie-sticker-ids
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
                           (setq sly-stickers--zombie-sticker-ids nil)
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
;;; sly-stickers.el ends here

