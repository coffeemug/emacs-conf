;;; countdown.el --- Timer in mode-line -*- lexical-binding: t; -*-

;; Author: Syohei YOSHIDA(syohex@gmail.com)
;; Version: 0.01
;; URL: https://github.com:/syohex/emacs-countdown
;; Package-Requires: ((emacs "24.4"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; countdown.el provides showing timer in mode-line.
;;
;; Start timer
;;   M-x countdown-start
;;
;; Stop timer
;;   M-x countdown-stop

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup countdown nil
  "Simple timer"
  :prefix "countdown-"
  :group 'timer)

(defcustom countdown-mode-default-minutes 25
  "Default minutes"
  :type 'number)

(defcustom countdown-mode-line-sign "●"
  "Sign of timer"
  :type 'string)

(defcustom mode-line-expire-hook nil
  "Hook run after timer expired."
  :type 'hook)

(defface countdown-sign
  '((((class color) (min-colors 88) (background light))
     :foreground "blue")
    (((class color) (background dark))
     (:foreground "cyan"))
    (t nil))
  "mode-line-face")

(defface countdown-timer
  '((t (:weight bold)))
  "mode-line-face")

(defvar countdown--timer nil)
(defvar countdown--remainder-seconds 0)
(defvar countdown--mode-line "")

(defsubst countdown--time-to-string (seconds)
  (format "%02d:%02d" (/ seconds 60) (mod seconds 60)))

(defun countdown--propertize-mode-line ()
  (unless (string-empty-p countdown--mode-line)
    (concat (propertize countdown-mode-line-sign 'face 'countdown-sign)
            (propertize countdown--mode-line 'face 'countdown-timer))))

(defun countdown--set-mode-line ()
  (setq countdown--mode-line
        (countdown--time-to-string countdown--remainder-seconds)))

(defun countdown--tick ()
  (let ((remainder-seconds (1- countdown--remainder-seconds)))
    (if (< remainder-seconds 0)
        (progn
          (countdown-stop)
          (run-hooks 'mode-line-expire-hook))
      (cl-decf countdown--remainder-seconds)
      (countdown--set-mode-line)
      (countdown--propertize-mode-line)
      (force-mode-line-update))))

(defsubst countdown--set-remainder-second (minutes)
  (setq countdown--remainder-seconds (* 60 minutes)))

;;;###autoload
(cl-defun countdown-start (&optional minutes)
  (interactive "Pminutes")

  (unless (null minutes)
    (setq minutes (prefix-numeric-value minutes)))

  ;; if the timer already exists, pause it and bail
  (when countdown--timer
    (cancel-timer countdown--timer)
    (setq countdown--timer nil)
    (unless minutes
      (cl-return-from countdown-start)))

  ;; if the user passed an explicit length, set the timer
  (when minutes
    (countdown--set-remainder-second minutes))
  (when (= countdown--remainder-seconds 0)
    (countdown--set-remainder-second countdown-mode-default-minutes))

  ;; start the timer
  (setq countdown--timer (run-with-timer 0 1 'countdown--tick)))

(defun countdown-stop ()
  (interactive)
  (when countdown--timer
    (cancel-timer countdown--timer))
  (setq countdown--timer nil
        countdown--mode-line ""
	countdown--remainder-seconds 0)
  (force-mode-line-update)
  (run-hooks 'mode-line-expire-hook))

(unless (member '(:eval (countdown--propertize-mode-line)) mode-line-format)
  (setq-default mode-line-format
                (append mode-line-format '((:eval (countdown--propertize-mode-line))))))

(provide 'countdown)

;;; countdown.el ends here
