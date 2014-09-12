(require 'sly)
(require 'cl-lib)
(require 'hi-lock) ;; for the faces

(define-sly-contrib sly-sticker
  "Mark expressions in source buffers and annotate return values."
  (:authors "João Távora <joaotavora@gmail.com>")
  (:license "GPL")
  (:sly-dependencies sly-trace-dialog)
  (:on-load (add-hook 'sly-mode-hook 'sly-sticker-enable))
  (:on-unload (remove-hook 'sly-mode-hook 'sly-sticker-enable)))

(defgroup sly-sticker nil
  "Mark expressions in source buffers and annotate return values."
  :prefix "sly-sticker-"
  :group 'sly)

(defface sly-sticker-placed-face
  '((t (:inherit hi-blue)))
  "Face for sticker just set")

(defun sly-sticker-enable () (sly-sticker-mode 1))

(define-minor-mode sly-sticker-mode
  "Mark expression in source buffers and annotate return values.")

(defvar sly-sticker-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") 'sly-sticker-dwim)
    map))

(defun sly-sticker--stickers-at-point (&optional point)
  (interactive "d")
  (cl-remove-if-not #'(lambda (ov)
                        (overlay-get ov 'sly-sticker))
                    (overlays-at (or point (point)))))

(defvar sly-sticker--counter 0)

(defun sly-sticker--make (from to)
  (let ((overlay (make-overlay from to)))
    (overlay-put overlay 'sly-sticker t)
    (overlay-put overlay 'face 'sly-sticker-placed-face)
    (overlay-put overlay 'id (incf sly-sticker-counter))
    overlay))

(defun sly-sticker--delete (x) (delete-overlay x))

(defun sly-sticker-dwim ()
  (interactive)
  (let ((stickers (sly-sticker--stickers-at-point ())))
    (cond (stickers
           (mapc #'sly-sticker--delete stickers))
          ((region-active-p)
           (sly-sticker--make (region-beginning) (region-end)))
          (t
           (let ((bounds (sly-bounds-of-sexp-at-point)))
             (sly-sticker--make (car bounds) (cdr bounds)))))))


;;; debug section
;;;
(when nil
  (put 'sly-sticker-placed-face 'face-defface-spec nil))

(provide 'sly-sticker)
