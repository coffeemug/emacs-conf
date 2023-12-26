(use-package package
  :init
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

(use-package use-package
  :init
  (setq use-package-always-ensure t))

;; My own custom stuff
(add-to-list 'load-path "~/emacs-conf/")

;; Some niceties
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(menu-bar-mode 0)                  ; turn off unnecessary UI
(show-paren-mode t)                ; enable paren-matching
(transient-mark-mode t)            ; make regions sane
(electric-pair-mode t)             ; type brackets in pairs
(setq-default cursor-type 'bar)

;; Some more niceties
(setq frame-title-format "%b")                   ; set frame title to file name
(setq inhibit-startup-message t)                 ; turn off splash screen
(setq initial-scratch-message "")                ; turn off initial scratch buffer message
(setq sentence-end-double-space nil)             ; make filling nicer
(setq ring-bell-function 'ignore)                ; turn off the bell
(defalias 'yes-or-no-p 'y-or-n-p)                ; make yes/no less annoying

;; intro msg
(defun display-startup-echo-area-message ()
  (message "Let the hacking begin!"))

;; nice completion in every buffer
(use-package company
  :hook (after-init . global-company-mode))

;; Configure completion
(use-package ido
  :init
  (ido-mode 1)

  (setq confirm-nonexistent-file-or-buffer nil)
  (setq ido-enable-flex-matching t))

(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :init
  (recentf-mode)
  (fset 'vanilla-grep #'grep)
  (fset 'grep #'consult-grep)

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

;; nicer keybindings
(global-set-key [(control s)] 'isearch-forward-regexp)
(global-set-key [(control r)] 'isearch-backward-regexp)
(global-set-key [(meta %)] 'query-replace-regexp)
(global-set-key [(control o)] 'other-window)
(global-set-key (kbd "M-=") 'count-words)

;; highlight the current line
(global-hl-line-mode 1)

;; customize isearch to always end at the beginning of search word
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))
(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

;; osx-specific instructions
(when (eq system-type 'darwin)
  ;; make apple-command be the meta modifier
  (setq mac-command-modifier 'meta))

;; graphics mode specific instructions
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode -1))

;; unfill paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(global-set-key [(control q)] 'unfill-paragraph)

;; playing with slime
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(when (boundp 'slime-contribs)
  (add-to-list 'slime-contribs 'slime-repl))

;; dired mode
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

(defun dired-upgrade-mode-map ()
  (define-key dired-mode-map [(control o)] 'other-window)
  (define-key dired-mode-map "k" 'dired-previous-line)
  (define-key dired-mode-map "j" 'dired-next-line)
  (define-key dired-mode-map "v" 'dired-view-file-other-window)
  (define-key dired-mode-map "o" 'dired-find-file-other-window)
  (define-key dired-mode-map "q" (lambda ()
				   (interactive)
				   (quit-window t))))

(add-hook 'dired-mode-hook 'dired-upgrade-mode-map)
(global-set-key (kbd "C-x C-d") 'dired-other-window)

;; while we're at it, make it easy to create new buffers
(global-set-key (kbd "C-x C-n")
		(lambda ()
		  (interactive)
		  (switch-to-buffer
		   (generate-new-buffer "*scratch*"))))

;; pixel perfect scrolling!
(pixel-scroll-precision-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-themes yaml-mode vertico-prescient smex sly-quicklisp sly-named-readtables orderless markdown-mode marginalia magit ido-grid-mode ido-completing-read+ ebdb crm-custom counsel consult company-prescient)))
