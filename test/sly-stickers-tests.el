(require 'sly-stickers)
(require 'sly-tests "lib/sly-tests")
(require 'cl-lib)

(defvar sly-stickers--test-debug nil)

(defun sly-stickers--call-with-fixture (function forms sticker-prefixes)
  (let ((file (make-temp-file "sly-stickers--fixture"))
        (sly-flash-inhibit t)
        ;; important HACK so this doesn't fail with the `sly-retro'
        ;; contrib.
        (sly-net-send-translator nil))
    (sly-eval-async '(cl:ignore-errors (cl:delete-package :slynk-stickers-fixture)))
    (sly-sync-to-top-level 1)
    (unwind-protect
        (with-current-buffer
            (find-file file)
          (lisp-mode)
          (insert (mapconcat #'pp-to-string
                             (append '((defpackage :slynk-stickers-fixture (:use :cl))
                                       (in-package :slynk-stickers-fixture))
                                     forms)
                             "\n"))
          (write-file file)
          (cl-loop for prefix in sticker-prefixes
                   do
                   (goto-char (point-max))
                   (search-backward prefix)
                   (call-interactively 'sly-stickers-dwim))
          (funcall function)
          (sly-sync-to-top-level 1))
      (if sly-stickers--test-debug
          (sly-message "leaving file %s" file)
        (let ((visitor (find-buffer-visiting file)))
          (when visitor (kill-buffer visitor)))
        (delete-file file))
      )))

(cl-defmacro sly-stickers--with-fixture ((forms sticker-prefixes) &rest body)
  (declare (indent defun) (debug (sexp &rest form)))
  `(sly-stickers--call-with-fixture #'(lambda () ,@body) ,forms ,sticker-prefixes))

(defun sly-stickers--topmost-sticker ()
  (car (sly-button--overlays-at (point))))

(defun sly-stickers--base-face (sticker)
  (let ((face (button-get sticker 'face)))
    (if (atom face)
        face
      (plist-get face :inherit))))

(defun sly-stickers--face-p (face)
  (let* ((sticker (sly-stickers--topmost-sticker))
         (actual (sly-stickers--base-face sticker)))
    (eq face actual)))

(define-sly-ert-test stickers-basic-navigation ()
  "Test that setting stickers and navigating to them works"
  (sly-stickers--with-fixture ('((defun foo () (bar (baz)))
                                 (defun quux () (coiso (cena))))
                               '("(bar" "(baz" "(coiso"))
    (goto-char (point-min))
    (call-interactively 'sly-stickers-next-sticker)
    (save-excursion
      (should (equal (read (current-buffer)) '(bar (baz)))))
    (call-interactively 'sly-stickers-next-sticker)
    (save-excursion
      (should (equal (read (current-buffer)) '(baz))))
    (call-interactively 'sly-stickers-next-sticker)
    (save-excursion
      (should (equal (read (current-buffer)) '(coiso (cena)))))
    (should (eq 'sly-stickers-placed-face
                (sly-stickers--base-face (sly-stickers--topmost-sticker))))))

(define-sly-ert-test stickers-should-stick ()
  "Test trying to compile the buffer and checking that stickers stuck"
  (sly-stickers--with-fixture ('((defun foo () (bar (baz)))
                                 (defun quux () (coiso (cena))))
                               '("(bar" "(baz" "(coiso"))
    (call-interactively 'sly-compile-defun)
    (sly-sync-to-top-level 1)
    (unless (sly-stickers--face-p 'sly-stickers-armed-face)
      (ert-fail "Expected QUUX stickers to be armed"))
    (call-interactively 'sly-stickers-prev-sticker)
    (unless (sly-stickers--face-p 'sly-stickers-placed-face)
      (ert-fail "Compiled just the QUUX defun, didn't expect FOO stickers to arm."))
    (call-interactively 'sly-compile-defun)
    (sly-sync-to-top-level 1)
    (unless (sly-stickers--face-p 'sly-stickers-armed-face)
      (ert-fail "Expected innermost FOO sticker to be armed by now."))
    (call-interactively 'sly-stickers-prev-sticker)
    (unless (sly-stickers--face-p 'sly-stickers-armed-face)
      (ert-fail "Expected outermost FOO sticker to also be armed by now."))))

(define-sly-ert-test stickers-when-invalid-dont-stick ()
  "Test trying to make invalid stickers stick"
  (sly-stickers--with-fixture ('((defun foo () (bar (baz))))
                               '("(bar" "(baz" "foo"))
    (goto-char (point-min))
    (call-interactively 'sly-stickers-next-sticker)
    (unless (sly-stickers--face-p 'sly-stickers-placed-face)
      (ert-fail "Expected FOO sticker to be unarmed"))
    (call-interactively 'sly-compile-defun)
    (sly-sync-to-top-level 1)
    (unless (sly-stickers--face-p 'sly-stickers-placed-face)
      (ert-fail "Expected invalid FOO sticker to remain unarmed"))
    (call-interactively 'sly-stickers-next-sticker)
    (unless (sly-stickers--face-p 'sly-stickers-placed-face)
      (ert-fail "Expected valid FOO sticker to remain unarmed"))
    (call-interactively 'sly-stickers-next-sticker)
    (unless (sly-stickers--face-p 'sly-stickers-placed-face)
      (ert-fail "Expected valid FOO sticker to remain unarmed"))))

(define-sly-ert-test stickers-in-a-file
  "Test compiling a file with some valid and invalid stickers."
  (sly-stickers--with-fixture ('((defun foo () (bar (baz)))
                                 (defun bar (x) (values (list x) 'bar))
                                 (defun baz () 42)
                                 (defun xpto () (let ((coiso)) coiso)))
                               '("(bar" "(baz" "(coiso"))
    
    (goto-char (point-min))
    (call-interactively 'sly-compile-and-load-file)
    (sly-sync-to-top-level 1)
    (call-interactively 'sly-stickers-next-sticker)
    (unless (sly-stickers--face-p 'sly-stickers-armed-face)
      (ert-fail "Expected BAR sticker to be armed"))
    (call-interactively 'sly-stickers-next-sticker)
    (unless (sly-stickers--face-p 'sly-stickers-armed-face)
      (ert-fail "Expected BAZ sticker to be armed"))
    (call-interactively 'sly-stickers-next-sticker)
    (unless (sly-stickers--face-p 'sly-stickers-placed-face)
      (ert-fail "Didn't expect COISO sticker to be armed"))))

(define-sly-ert-test stickers-record-stuff ()
  "Test actually checking stickers' values."
  (sly-stickers--with-fixture ('((defun foo () (bar (baz)))
                                 (defun bar (x) (values (list x) 'bar))
                                 (defun baz () 42))
                               '("(bar" "(baz"))
    
    (goto-char (point-min))
    (call-interactively 'sly-compile-and-load-file)
    (sly-sync-to-top-level 1)
    (call-interactively 'sly-stickers-next-sticker)
    (unless (sly-stickers--face-p 'sly-stickers-armed-face)
      (ert-fail "Expected BAR sticker to be armed by now"))
    (sly-eval-async '(slynk-stickers-fixture::foo))
    (sly-sync-to-top-level 1)
    (call-interactively 'sly-stickers-fetch)
    (sly-sync-to-top-level 1)
    (unless (sly-stickers--face-p 'sly-stickers-recordings-face)
      (ert-fail "Expected BAR sticker to have some information"))

    ;; This part still needs work
    ;; 
    ;; (call-interactively 'sly-stickers-next-sticker)
    ;; (call-interactively 'sly-stickers-next-sticker)
    ;; (call-interactively 'sly-compile-defun)
    ;; (sly-sync-to-top-level 1)
    ;; (unless (sly-stickers--face-p 'sly-stickers-armed-face)
    ;;   (ert-fail "Expected QUUX sticker to be armed"))
    ;; (sly-eval-async '(cl:ignore-errors (slynk-stickers-fixture::quux)))
    ;; (call-interactively 'sly-stickers-fetch)
    ;; (sly-sync-to-top-level 1)
    ;; (unless (sly-stickers--face-p 'sly-stickers-exited-non-locally-face)
    ;;   (ert-fail "Expected QUXX sticker COISO to have exited non-locally"))
    ))

(provide 'sly-stickers-tests)
