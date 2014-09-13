(require 'sly)
(require 'sly-parse)
(require 'cl-lib)
(require 'hi-lock) ;; for the faces

(define-sly-contrib sly-stickers
  "Mark expressions in source buffers and annotate return values."
  (:authors "João Távora <joaotavora@gmail.com>")
  (:license "GPL")
  (:sly-dependencies sly-trace-dialog)
  (:on-load (add-hook 'sly-mode-hook 'sly-stickers-enable)
            (add-hook 'sly-after-compile-functions 'sly-stickers-commit-stickers))
  (:on-unload (remove-hook 'sly-mode-hook 'sly-stickers-enable)
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
  '((t (:inherit hi-)))
  "Face for sticker just set")

(defun sly-stickers-enable () (sly-stickers-mode 1))

(define-minor-mode sly-stickers-mode
  "Mark expression in source buffers and annotate return values.")

(defvar sly-stickers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") 'sly-stickers-dwim)
    map))

(defun sly-stickers--stickers-in (beg end)
  (cl-remove-if-not #'(lambda (ov)
                        (overlay-get ov 'sly-stickers))
                    (overlays-in beg end)))

(defun sly-stickers--stickers-at-point (&optional point)
  (interactive "d")
  (let ((point (or point (point))))
    (sly-stickers--stickers-in (1- point) (1+ point))))

(defvar sly-stickers--counter 0)

(defun sly-stickers--make (from to)
  (let ((overlay (make-overlay from to)))
    (overlay-put overlay 'sly-stickers t)
    (overlay-put overlay 'face 'sly-stickers-placed-face)
    (overlay-put overlay 'id (incf sly-stickers--counter))
    overlay))

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
                  (sly-stickers--make (region-beginning) (region-end)))
                 ((not (sly-inside-string-or-comment-p))
                  (let ((bounds (sly-bounds-of-sexp-at-point)))
                    (sly-stickers--make (car bounds) (cdr bounds)))))))))

(defun sly-stickers-commit-stickers (result start end)
  (let ((string (buffer-substring-no-properties start end))
        (stickers (sly-stickers--stickers-in start end)))
    (when stickers
      (sly-with-popup-buffer ((sly-buffer-name :sticker) :package (sly-current-package)
                              :mode 'lisp-mode)
        (insert string)
        (cl-loop for sticker in stickers
                 for sexp-beg = (- (overlay-start sticker) (1- start))
                 for sexp-end = (- (overlay-end sticker) (1- start))
                 for sexp = (buffer-substring-no-properties sexp-beg
                                                            sexp-end)
                 do
                 (goto-char sexp-beg)
                 (delete-region (point) sexp-end)
                 (insert (format "(slynk-trace-dialog:instrument %s)" sexp)))))))

(provide 'sly-stickers)
