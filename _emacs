;; package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; My own custom stuff
(add-to-list 'load-path "~/projects/emacs-conf/")
(add-to-list 'load-path "~/emacs-conf/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company crm-custom markdown-mode smex ido-grid-mode ido-completing-read+)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "dark slate gray")))))

;; Some niceties
(load-theme 'wombat)               ; load theme
(menu-bar-mode 0)                  ; turn off unnecessary UI
(show-paren-mode t)                ; enable paren-matching
(transient-mark-mode t)            ; make regions sane
(electric-pair-mode t)             ; type brackets in pairs

;; Some more niceties
(setq frame-title-format "%b")                   ; set frame title to file name
(setq inhibit-startup-message t)                 ; turn off splash screen
(setq initial-scratch-message "")                ; turn off initial scratch buffer message
(setq sentence-end-double-space nil)             ; make filling nicer
(setq ring-bell-function 'ignore)                ; turn off the bell
(defalias 'yes-or-no-p 'y-or-n-p)                ; make yes/no less annoying

;; basic colors (for GUIs and evil terminals)
(set-background-color "black")
(set-foreground-color "green")
(set-face-foreground 'region "white")
(set-face-background 'region "SkyBlue4")

;; intro msg
(defun display-startup-echo-area-message ()
  (message "Let the hacking begin!"))

;; now that basic niceties are done, install missing packages if any
(package-install-selected-packages)

;; nice completion in every buffer
(add-hook 'after-init-hook 'global-company-mode)

;; Configure IDO
(ido-mode 1)
(ido-everywhere 1)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)
(setq ido-use-virtual-buffers t)
(setq ido-enable-flex-matching t)

;; Enable IDO in as many places as possible
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
(require 'crm-custom)
(crm-custom-mode 1)

;; Prettify it
(require 'ido-grid-mode)
(ido-grid-mode 1)
(setq ido-grid-mode-prefix-scrolls t)

(set-face-foreground 'ido-first-match "white")
(set-face-background 'ido-first-match "RoyalBlue3")
(set-face-attribute 'ido-first-match nil :weight 'normal)

(set-face-foreground 'ido-grid-mode-match "#a0a8b0")
(set-face-background 'ido-grid-mode-match "#384048")
(set-face-attribute 'ido-grid-mode-match nil :underline nil)

;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

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
  (scroll-bar-mode -1)
					; prettify vertical border color
  (set-face-background 'vertical-border "gray")
  (set-face-foreground 'vertical-border
		       (face-background 'vertical-border)))

;; markdown mode (err, I do write a lot of markdown)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; unfill paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(global-set-key [(control q)] 'unfill-paragraph)

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; rjsx/javascript
(setq js-indent-level 2)
(setq js2-strict-trailing-comma-warning nil)
(add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))

;; python
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter "/usr/local/bin/ipython")
(setq python-indent-offset 4)

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
