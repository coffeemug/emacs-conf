(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

(use-package use-package
  :init
  (setq use-package-always-ensure t))

(use-package emacs
  :ensure nil

  :custom
  (frame-title-format "%b")
  (ring-bell-function 'ignore)
  (inhibit-startup-message t)
  (initial-scratch-message "")
  (sentence-end-double-space nil)
  (completion-styles '(flex))

  :config
  (add-to-list 'load-path "~/emacs-conf/")

  (menu-bar-mode 0)
  (show-paren-mode t)
  (transient-mark-mode t)
  (electric-pair-mode t)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defun display-startup-echo-area-message ()
    (message "Let the hacking begin!"))

  (when (display-graphic-p)
    (tool-bar-mode 0)
    (scroll-bar-mode -1)
    (pixel-scroll-precision-mode)
    (setq-default cursor-type 'bar))

  ;; Platform specific
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))
  
  (when (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :font "Consolas-10"))
  
  :bind (("C-o" . other-window))

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

  ;; Add "unfill" command
  :config
  (defun unfill-paragraph (&optional region)
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override `fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
      (fill-paragraph nil region)))

  :bind (("C-q" . unfill-paragraph)))

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package corfu
  :init
  (global-corfu-mode)
  
  :custom
  (corfu-auto t)

  :hook
  ((before-save . corfu-quit)))

;; Configure completion
(use-package ido
  :config
  (ido-mode 'file)

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

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package nerd-icons)

(use-package nerd-icons-completion
  :if (featurep 'nerd-icons)
  :config
  (nerd-icons-completion-mode)

  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package dirvish
  :config
  (when (eq system-type 'darwin)
    (setq insert-directory-program "/usr/local/bin/gls"))
  
  (dirvish-override-dired-mode)

  :custom
  (dired-isearch-filenames t)
  (dirvish-reuse-session nil)
  (dirvish-subtree-state-style 'nerd)
  (dirvish-attributes '(nerd-icons subtree-state file-size file-time))
  (dired-listing-switches "-l --almost-all --ignore-backups --group-directories-first")

  :bind (("C-x d" . dirvish-dwim)
	 ("C-x C-d" . dirvish-dwim))
  :bind (:map dirvish-mode-map
	      ("q" . dirvish-quit)
	      ("C-g" . dirvish-quit)
 	      ("k" . dired-previous-line)
 	      ("j" . dired-next-line)
	      ("<backspace>" . dired-up-directory)
	      ("RET" . dired-find-file)
	      ("/" . isearch-forward-regexp)
	      ("C-o" . other-window)
	      ("s" . dirvish-quicksort))

  ;; toggle "boring" files
  :config
  (defun dired-omit-toggle-quiet ()
    (interactive)
    (dired-omit-mode (if dired-omit-mode -1 1)))
  :custom
  (dired-omit-files "\\`[.].*")
  (dired-omit-verbose . nil)
  :hook (dired-mode . dired-omit-mode)
  :bind (:map dirvish-mode-map
	      ("b" . dired-omit-toggle-quiet))

  ;; subtrees
  :custom
  (dirvish-subtree-always-show-state t)
  
  :config
  (defun dirvish-subtree-collapse ()
    (interactive)
    (when (dirvish-subtree--expanded-p)
      (dired-next-line 1)
      (dirvish-subtree-remove)))
  
  (defun dirvish-subtree-expand ()
    (interactive)
    (when (not (dirvish-subtree--expanded-p))
      (condition-case err (dirvish-subtree--insert)
	(file-error (dirvish-subtree--view-file))
	(error (message "%s" (cdr err))))
      ;; Expanding and collapsing subtrees doesn't respect the omit
      ;; mode. So we need to reomit the file.
      (when dired-omit-mode
	(dired-omit-mode 1))))
  
  :bind (:map dirvish-mode-map
	      ("<left>" . dirvish-subtree-collapse)
	      ("<right>" . dirvish-subtree-expand))

  ;; jump to directory from within dired
  :config
  (defun jump-to-directory ()
    (interactive)
    (dired-jump nil (ido-read-directory-name "Find dir: ")))
  :bind (:map dirvish-mode-map
	      ("C-x C-f" . jump-to-directory)
	      ("C-x d" . jump-to-directory)
	      ("C-x C-d" . jump-to-directory))
  )

(use-package which-key
  :config
  (which-key-mode))

(use-package doom-modeline
  :config (doom-modeline-mode 1))

(use-package magit)

(use-package display-line-numbers
  :ensure nil
  :bind ("C-x C-l" . global-display-line-numbers-mode))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim))
  )

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package proced
  :ensure nil
  :config
  (add-to-list
   'proced-format-alist
   '(custom pid user rss pmem pcpu time state comm))
  
  :custom
  (proced-format 'custom)
  (proced-auto-update-flag t)
  (proced-auto-update-interval 1)
  (proced-enable-color-flag t))

(use-package treesit
  :if (treesit-available-p)
  :demand t
  :ensure nil
  :custom
  (treesit-language-source-alist
   '((cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (c "https://github.com/tree-sitter/tree-sitter-c")))
  (treesit-load-name-override-list
   '((c++ "libtree-sitter-cpp")))
  (major-mode-remap-alist
   '((c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (c-or-c++-mode . c-or-c++-ts-mode)))
  )

(use-package eglot
  :ensure nil
  :hook ((c-ts-mode . eglot-ensure)
	 (c++-ts-mode . eglot-ensure)
	 (c-or-c++-ts-mode . eglot-ensure)))
