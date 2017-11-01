(require 'lui)

(defface draft-prompt-info
  '((t (:foreground "lightblue" :weight bold)))
  "Draft prompt info" :group 'draft-mode)
(defface draft-prompt-warning
  '((t (:foreground "red" :weight bold)))
  "Draft prompt warning" :group 'draft-mode)
(defface draft-prompt-separator
  '((t (:foreground "lightgrey")))
  "Draft prompt separator" :group 'draft-mode)

(defun draft-init ()
  (lui-set-prompt
   (draft-propertize-prompt 140))
  (goto-char (point-max))
  (setq lui-input-function #'lui-insert)
  (add-hook 'after-change-functions #'draft-update-prompt nil t))

(defun draft-propertize-prompt (nchars)
  (concat
   (propertize (format "%s" nchars) 'face
	       (if (<= nchars 20)
		   'draft-prompt-warning
		 'draft-prompt-info))
   (propertize "> " 'face
	       'draft-prompt-separator)))

(defun draft-update-prompt (beg end len)
  (when (and lui-input-marker
	     (>= beg lui-input-marker))
    (lui-set-prompt
     (draft-propertize-prompt
      (- 140 (string-width (draft-lui-input)))))))

(defun draft-lui-input ()
  (buffer-substring lui-input-marker (point-max)))

(define-derived-mode draft-mode lui-mode "Draft"
  ""
  (draft-init))

(provide 'draft-mode)

