;;; -*-lexical-binding:t-*-
(require 'sly)
(require 'sly-parse)
(require 'cl-lib)
(require 'hi-lock) ;; for the faces

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

(defface sly-stickers-mouse-hover-face
  '((t (:box (:line-width 1 :color "grey75"))))
  "Face for when the mouse hovers over a sticker")

(defun sly-stickers-enable () (sly-stickers-mode 1))

(defvar sly-stickers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s C-s") 'sly-stickers-dwim)
    map))

(define-minor-mode sly-stickers-mode
  "Mark expression in source buffers and annotate return values.")

(defun sly-stickers--stickers-in (beg end)
  (cl-remove-if-not #'(lambda (ov)
                        (overlay-get ov 'sly-stickers))
                    (overlays-in beg end)))

(defun sly-stickers--stickers-at-point (&optional point)
  (interactive "d")
  (let ((point (or point (point))))
    (sly-stickers--stickers-in (1- point) (1+ point))))

(defvar sly-stickers--counter 0)

(defvar sly-stickers--stickers (make-hash-table))

(defvar sly-stickers--sticker-keymap
  (let ((map (make-sparse-keymap)))
    
    map))

(defun sly-stickers--make-sticker (from to)
  (let ((sticker (make-overlay from to)))
    (overlay-put sticker 'sly-stickers t)
    (overlay-put sticker 'face 'sly-stickers-placed-face)
    (overlay-put sticker 'mouse-face 'sly-stickers-mouse-hover-face)
    (overlay-put sticker 'keymap sly-stickers--sticker-keymap)
    (overlay-put sticker 'help-echo "Brand new sticker.")
    sticker))

(defun sly-stickers--sticker-id (sticker)
  (overlay-get sticker 'sly-stickers-id))

(defun sly-stickers--arm-sticker (sticker)
  (let ((id (incf sly-stickers--counter)))
    (overlay-put sticker 'sly-stickers-id id)
    (overlay-put sticker 'sly-stickers-values nil)
    (overlay-put sticker 'face 'sly-stickers-armed-face)
    (overlay-put sticker 'help-echo (format "Sticker %d is armed." id))
    (puthash id sticker sly-stickers--stickers)))

(defun sly-stickers--disarm-sticker (sticker)
  (let ((id (sly-stickers--sticker-id sticker)))
    (overlay-put sticker 'sly-stickers-id nil)
    (overlay-put sticker 'sly-stickers-values nil)
    (overlay-put sticker 'face 'sly-stickers-placed-face)
    (overlay-put sticker 'help-echo "Not brand new, but still placed")
    (remhash id sly-stickers--stickers)))

(defun sly-stickers--populate-sticker (sticker values)
  (let ((id (sly-stickers--sticker-id sticker)))
    (overlay-put sticker 'sly-stickers-values
                 (append
                  (overlay-get sticker 'sly-stickers-values)
                  values))
    (overlay-put sticker 'face 'sly-stickers-lit-face)
    (overlay-put sticker 'help-echo (format "Sticker %d has %d new values" id (length values)))))

(defun sly-stickers--bring-out-your-dead ()
  (let ((dead-ids))
    (maphash #'(lambda (id sticker)
                 (unless (overlay-buffer sticker)
                   (push id dead-ids)
                   (remhash id sly-stickers--stickers)))
             sly-stickers--stickers)))

(defun sly-stickers--delete (x) (delete-overlay x))

(defun sly-stickers-clear-defun-stickers ()
  (interactive)
  (let* ((region (sly-region-for-defun-at-point))
         (stickers (sly-stickers--stickers-in (car region) (cadr region))))
    (cond (stickers
           (sly-flash-region (car region) (cadr region) nil 'sly-stickers-set-face)
           (mapc #'sly-stickers--delete stickers)
           (sly-message "%s stickers cleared" (length stickers)))
          (t
           (sly-message "no stickers found in this defun")))))

(defun sly-stickers-clear-buffer-stickers ()
  (interactive)
  (let* ((stickers (sly-stickers--stickers-in (point-min) (point-max))))
    (cond (stickers
           (mapc #'sly-stickers--delete stickers)
           (sly-message "%s stickers cleared in buffer" (length stickers)))
          (t
           (sly-message "no stickers found in buffer")))))

(defun sly-stickers-dwim (raw-prefix)
  (interactive "p")
  (cond ((= raw-prefix 4)
         (sly-stickers-clear-defun-stickers))
        ((>= raw-prefix 16)
         (sly-stickers-clear-buffer-stickers))
        (t
         (let ((stickers (sly-stickers--stickers-at-point ())))
           (cond (stickers
                  (mapc #'sly-stickers--delete stickers))
                 ((region-active-p)
                  (sly-stickers--make-sticker (region-beginning) (region-end)))
                 ((not (sly-inside-string-or-comment-p))
                  (let ((bounds (sly-bounds-of-sexp-at-point)))
                    (sly-stickers--make-sticker (car bounds) (cdr bounds)))))))))

(defun sly-stickers-fetch ()
  (interactive)
  (sly-eval-async `(slynk-stickers:check-stickers)
    #'(lambda (result)
        (cl-loop for (id . values) in result
                 for sticker = (gethash id sly-stickers--stickers)
                 if sticker
                 do (sly-stickers--populate-sticker sticker values)
                 else
                 do (sly-warning "Ooops sticker %s is not live anymore" id)))))

(defun sly-stickers-commit-stickers (_result arg1 arg2)
  (let* ((original-buffer (current-buffer))
         (bufferp (bufferp arg1))
         (start (if bufferp (point-min) arg1))
         (end (if bufferp (point-max) arg2))
         (string (buffer-substring-no-properties start end))
         (stickers (sly-stickers--stickers-in start end))
         (dead-ids (append (mapcar #'sly-stickers--sticker-id stickers)
                           (sly-stickers--bring-out-your-dead)))
         (package (sly-current-package)))
    (when stickers
      (if bufferp
          (sly-warning "Compiling buffers with stickers not supported yet.")
        (with-temp-buffer 
          (lisp-mode)
          (setq sly-buffer-package package)
          (insert string)
          (cl-loop for sticker in stickers
                   for sexp-beg = (- (overlay-start sticker) (1- start))
                   for sexp-end = (- (overlay-end sticker) (1- start))
                   for sexp = (buffer-substring-no-properties sexp-beg
                                                              sexp-end)
                   do
                   (goto-char sexp-beg)
                   (sly-stickers--arm-sticker sticker)
                   (delete-region (point) sexp-end)
                   (insert (format "(slynk-stickers:record %d %s)"
                                   (sly-stickers--sticker-id sticker)
                                   sexp)))
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
                           (sly-warning "Some stickers failed to stick. Probably misplaced")
                           (mapcar #'sly-stickers--disarm-sticker stickers))
                          (t
                           (cl-assert (equal ids new-ids)))))))))))))

(provide 'sly-stickers)
