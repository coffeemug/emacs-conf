(require 'lui)

(defcustom draft-store-directory nil
  "Directory where to store drafts."
  :type 'string
  :group 'draft-mode)

(defcustom draft-num-words-file-name 5
  "How many words to use from the first system to name the draft"
  :type 'integer
  :group 'draft-mode)

(defface draft-prompt-info
  '((t (:foreground "#8ac6f2" :weight bold)))
  "Draft prompt info" :group 'draft-mode)

(defface draft-prompt-warning
  '((t (:foreground "red" :weight bold)))
  "Draft prompt warning" :group 'draft-mode)

(defface draft-separator
  '((t (:foreground "lightgrey")))
  "Draft separator" :group 'draft-mode)

(defun draft-init ()
  (lui-set-prompt
   (draft-propertize-prompt 140))
  (goto-char (point-max))
  (setq-local draft-para-count 0)
  (setq-local draft-file-name nil)
  (setq-local lui-input-function #'draft-insert)
  (setq-local lui-time-stamp-only-when-changed-p nil)
  (setq-local lui-fill-type 'variable)
  (set-face-attribute 'lui-time-stamp-face nil
		      :foreground "#e5786d"
		      :weight 'normal)
  (add-hook 'after-change-functions #'draft-update-prompt nil t))

(defun make-file-name (txt)
  (concat
   (format-time-string "%Y-%m-%d-")
   (mapconcat 'identity
	      (or (seq-take (split-string
			     (downcase
			      (replace-regexp-in-string
			       "[^[:alnum:][:space:]]"
			       ""
			       txt)))
			    draft-num-words-file-name)
		  (list (make-temp-name "")))
	      "-")
   ".draft"))

(defun draft-file-insert (txt)
  (when (null draft-file-name)
    (setq-local draft-file-name
		(concat
		 (file-name-as-directory draft-store-directory)
		 (make-file-name txt)))
    (message "Storing draft in %s" draft-file-name))
  (with-temp-message (or (current-message) "")
    (write-region (concat txt (list ?\n)) nil draft-file-name t)))

(defun draft-insert (txt)
  (draft-file-insert txt)
  (if (string= "" txt)
      (progn
	(setq-local lui-time-stamp-format "")
	(lui-insert ""))
    (progn
      (setq-local draft-para-count
		  (+ draft-para-count 1))
      (setq-local lui-time-stamp-format
		  (format "/%d" draft-para-count))
      (lui-insert
       (concat
	(propertize "* " 'face 'draft-separator)
	txt)))))

(defun draft-propertize-prompt (nchars)
  (concat
   (propertize (format "%03d" nchars) 'face
	       (if (<= nchars 20)
		   'draft-prompt-warning
		 'draft-prompt-info))
   (propertize "> " 'face
	       'draft-separator)))

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

