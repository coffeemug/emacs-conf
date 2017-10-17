
;;; twitter-burndown-mode.el --- show twitter-style burndown in modeline
(provide 'twitter-burndown-mode)

;; define our mode-line expression
(setq twitter-burndown-mode-mode-line-expr '(:eval (paragraph-burndown-modeline-str)))

;; twitter-style burndown -- count down from 140 chars/peragraph (+
;; how many 140 char sentences are written)
(defun paragraph-burndown-modeline-str ()
  (let* ((beg (if (use-region-p)
		  (point)
		(save-excursion
		  (move-to-left-margin)
		  (forward-paragraph -1)
		  (point))))
	 (end (if (use-region-p)
		  (mark)
		(save-excursion
		  (move-to-left-margin)
		  (forward-paragraph +1)
		  (point))))
	 (pwidth (string-width (buffer-substring-no-properties beg end)))
	 (ntweets (/ pwidth 140))
	 (nchars (- 140 (% pwidth 140))))
    (if (zerop (string-width (thing-at-point 'line t)))
	"0x/140"
      (concat
       (format "%dx/" ntweets)
       (if (<= nchars 20)
	   (propertize (format "%s" nchars) 'face 'warning)
	 (format "%d" nchars))))))

;; setup/cleanup
(defun setup-twitter-burndown-mode ()
  (setq mode-line-format
	(append mode-line-format
		(list twitter-burndown-mode-mode-line-expr))))

(defun cleanup-twitter-burndown-mode ()
    (setq mode-line-format (delete twitter-burndown-mode-mode-line-expr mode-line-format)))

;; Mode definition
(define-minor-mode twitter-burndown-mode "Show twitter-style burndown in modeline"
  nil nil nil
  (if twitter-burndown-mode
      (setup-twitter-burndown-mode)
    (cleanup-twitter-burndown-mode)))

