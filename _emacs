(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

(use-package use-package
  :init
  (setq use-package-always-ensure t))

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;; nice completion in every buffer
(use-package company
  :hook (after-init . global-company-mode))

;; Configure completion
(use-package ido
  :config
  (ido-mode 1)

  (setq confirm-nonexistent-file-or-buffer nil)
  (setq ido-enable-flex-matching t))

(use-package vertico
  :config
  (vertico-mode))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package consult
  :demand t
  :init
  (recentf-mode)

  :config
  (fset 'vanilla-grep #'grep)
  (fset 'grep #'consult-grep)

  (defun gb/consult--source-recentf-items ()
    (let ((ht (consult--buffer-file-hash))
          file-name-handler-alist ;; No Tramp slowdown please.
          items)
      (dolist (file recentf-list (nreverse items))
	;; Emacs 29 abbreviates file paths by default, see
	;; `recentf-filename-handlers'.
	(unless (eq (aref file 0) ?/)
          (setq file (expand-file-name file)))
	(unless (gethash file ht)
          (push (propertize
		 (file-name-nondirectory file)
		 'multi-category `(file . ,file))
		items)))))

  (plist-put consult--source-recent-file
             :items #'gb/consult--source-recentf-items)

  :bind (
	 ("C-x b" . consult-buffer)
	 ("M-g g" . consult-goto-line)
	 ("M-g M-g" . consult-goto-line)
	 ))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-flex)))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package emacs
  :ensure nil

  ;; Configure frame & window related stuff
  :config
  (menu-bar-mode 0)
  (setq frame-title-format "%b")
  (setq ring-bell-function 'ignore)
  (when (display-graphic-p)
    (tool-bar-mode 0)
    (scroll-bar-mode -1)
    (pixel-scroll-precision-mode)
    (setq-default cursor-type 'bar))

  :bind (("C-o" . other-window))

  ;; General emacs configuration
  :config
  (add-to-list 'load-path "~/emacs-conf/")

  (show-paren-mode t)
  (transient-mark-mode t)
  (electric-pair-mode t)

  (setq inhibit-startup-message t)
  (setq initial-scratch-message "")
  (setq sentence-end-double-space nil)
  (defalias 'yes-or-no-p 'y-or-n-p)

  (defun display-startup-echo-area-message ()
    (message "Let the hacking begin!"))
  
  ;; Specialize isearch
  :config
  (defun my-goto-match-beginning ()
    (when (and isearch-forward isearch-other-end)
      (goto-char isearch-other-end)))
  (defadvice isearch-exit (after my-goto-match-beginning activate)
    "Go to beginning of match."
    (when (and isearch-forward isearch-other-end)
      (goto-char isearch-other-end)))
  
  :hook (isearch-mode-end . my-goto-match-beginning)
  
  :bind (("C-s" . isearch-forward-regexp)
	 ("C-r" . isearch-backward-regexp)
	 ("M-%" . query-replace-regexp))

  ;; Add "unqill" command
  :config
  (defun unfill-paragraph (&optional region)
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override `fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
      (fill-paragraph nil region)))

  :bind (("C-q" . unfill-paragraph)))

(use-package dired
  :ensure nil
  :config
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-isearch-filenames t)

  (defun dired-view-file-other-window ()
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
	  (or (and (cdr dired-subdir-alist)
		   (dired-goto-subdir file))
	      (dired file))
	(view-file-other-window file))))
  (defun dired-quit-window ()
    (interactive)
    (quit-window t))
  
  :bind ("C-x C-d" . dired-other-window)
  
  :bind (:map dired-mode-map
	      ("k" . dired-previous-line)
	      ("j" . dired-next-line)
	      ("v" . dired-view-file-other-window)
	      ("o" . dired-find-file-other-window)
	      ("q" . dired-quit-window)))

(use-package which-key
  :config
  (which-key-mode))


