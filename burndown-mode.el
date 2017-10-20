
;;; burndown-mode.el --- show twitter-style burndown in modeline
(provide 'burndown-mode)

;; define our mode-line expression
(setq burndown-mode-mode-line-expr '(:eval (burndown-str)))

;; twitter-style burndown -- count down from 140 chars/paragraph (+
;; how many 140 char sentences are written total)
(defun burndown-char-str ()
  (let* ((char-cutoff 140)
	 bbeg bend pbeg pend)
    (if (use-region-p)
	(setq bbeg (point)
	      bend (mark)
	      pbeg bbeg
	      pend bend)
      (setq bbeg (point-min)
	    bend (point-max)
	    pbeg (save-excursion
		   (move-to-left-margin)
		   (forward-paragraph -1)
		   (point))
	    pend (save-excursion
		   (move-to-left-margin)
		   (forward-paragraph +1)
		   (point))))
    (let* ((pwidth (string-width (buffer-substring-no-properties pbeg pend)))
	   (bwidth (string-width (buffer-substring-no-properties bbeg bend)))
	   (ntweets (/ bwidth char-cutoff))
	   (nchars (- char-cutoff (% pwidth char-cutoff))))
      (if (or (= 0 (buffer-size))
	      (zerop (string-width (thing-at-point 'line t))))
	  (setq nchars 140))
      (concat
       " c:"
       (propertize (format "%s" nchars) 'face
		   (cond ((<= nchars 10) 'compilation-mode-line-fail)
			 ((<= nchars 35) 'compilation-warning)
			 ((<= nchars 70) 'compilation-error)
			 (t 'compilation-info)))
       (format "/%dx" ntweets)))))

;; count down buffer word burndown (from 500 + how many 500 words are
;; written)
(defun burndown-word-str ()
  (let* ((word-cutoff 250)
	 (beg (point-min))
	 (end (point-max)))
    (when (use-region-p)
      (setq beg (point)
	    end (mark)))
    (let* ((nwords (count-words beg end))
	   (nsections (/ nwords word-cutoff))
	   (nburndown (- word-cutoff (% nwords word-cutoff))))
      (format " w:%d/%dx" nburndown nsections))))

;; Get it all together
(defun burndown-str ()
  (concat
   (burndown-char-str)
   (burndown-word-str)))

;; setup/cleanup
(defun setup-burndown-mode ()
  (setq mode-line-format
	(append mode-line-format
		(list burndown-mode-mode-line-expr))))

(defun cleanup-burndown-mode ()
    (setq mode-line-format (delete burndown-mode-mode-line-expr mode-line-format)))

;; Mode definition
(define-minor-mode burndown-mode "Show twitter-style burndown in modeline"
  nil nil nil
  (if burndown-mode
      (setup-burndown-mode)
    (cleanup-burndown-mode)))

