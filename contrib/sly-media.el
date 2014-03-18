(eval-and-compile
  (require 'sly))

(define-sly-contrib sly-media
  "Display things other than text in SLY buffers"
  (:authors "Christophe Rhodes <csr21@cantab.net>")
  (:license "GPL")
  (:sly-dependencies sly-old-repl)
  (:swank-dependencies swank-media)
  (:on-load
   (add-hook 'sly-event-hooks 'sly-dispatch-media-event)))

(defun sly-dispatch-media-event (event)
  (destructure-case event
    ((:write-image image string)
     (let ((image (find-image image)))
       (sly-media-insert-image image string))
     t)
    ((:popup-buffer bufname string mode)
     (sly-with-popup-buffer (bufname :connection t :package t)
       (when mode (funcall mode))
       (princ string)
       (goto-char (point-min)))
     t)
    (t nil)))

(defun sly-media-insert-image (image string &optional bol)
  (with-current-buffer (sly-output-buffer)
    (let ((marker (sly-output-target-marker :repl-result)))
      (goto-char marker)
      (sly-propertize-region `(face sly-repl-result-face
                                      rear-nonsticky (face))
        (insert-image image string))
      ;; Move the input-start marker after the REPL result.
      (set-marker marker (point)))
    (sly-repl-show-maximum-output)))

(provide 'sly-media)
