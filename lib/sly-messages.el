;;; sly-messages.el --- Messages, errors, echo-area and visual feedback utils for SLY  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  João Távora

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: 

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

;; 

;;; Code:

(require 'cl-lib)

(defvar sly--last-message nil)

(defun sly-message (format-string &rest args)
  "Like `message', but use a prefix."
  (let ((body (apply #'format format-string args)))
    (setq sly--last-message (format "[sly] %s" body))
    (message "%s" sly--last-message)))

(defun sly-temp-message (wait sit-for format &rest args)
  (run-with-timer
   wait nil
   #'(lambda ()
       (let ((existing sly--last-message))
         (apply #'sly-message format args)
         (run-with-timer
          sit-for
          nil
          #'(lambda ()
              (message "%s" existing)))))))

(defun sly-warning (format-string &rest args)
  (display-warning '(sly warning) (apply #'format format-string args)))

(defun sly-error (format-string &rest args)
  (apply #'error (format "[sly] %s" format-string) args))

(defun sly-display-oneliner (format-string &rest format-args)
  (let* ((msg (apply #'format format-string format-args)))
    (unless (minibuffer-window-active-p (minibuffer-window))
      (sly-message (sly-oneliner msg)))))

(defun sly-oneliner (string)
  "Return STRING truncated to fit in a single echo-area line."
  (substring string 0 (min (length string)
                           (or (cl-position ?\n string) most-positive-fixnum)
                           (1- (window-width (minibuffer-window))))))

(defvar sly-completing-read-function 'ido-completing-read)

(defun sly-completing-read (prompt choices &optional
                                   predicate
                                   require-match
                                   initial-input
                                   hist
                                   def
                                   inherit-input-method)
  (funcall sly-completing-read-function
           prompt
           choices
           predicate
           require-match
           initial-input
           hist
           def
           inherit-input-method))


;;; Flashing the region
;;;
(defvar sly-flash-inhibit nil
  "If non-nil `sly-flash-region' does nothing")

(cl-defun sly-flash-region (start end &key timeout face times)
  "Temporarily highlight region from START to END."
  (unless sly-flash-inhibit
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'face (or face
                                     'highlight))
      (overlay-put overlay 'priority 1000)
      (run-with-timer
       (or timeout 0.2) nil
       #'(lambda ()
           (delete-overlay overlay)
           (when (and times
                      (> times 1))
             (run-with-timer
              (or timeout 0.2) nil
              #'(lambda ()
                  (sly-flash-region start end
                                    :timeout timeout
                                    :face face
                                    :times (1- times))))))))))


(provide 'sly-messages)
;;; sly-messages.el ends here
