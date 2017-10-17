;; package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; My own custom stuff
(add-to-list 'load-path "~/projects/emacs-conf/")

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

;; Configure IDO
(ido-mode 1)
(ido-everywhere 1)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)

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
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; nicer keybindings
(global-set-key [(control s)] 'isearch-forward-regexp)
(global-set-key [(control r)] 'isearch-backward-regexp)
(global-set-key [(meta %)] 'query-replace-regexp)
(global-set-key [(control o)] 'other-window)
(global-set-key (kbd "M-=") 'count-words)

;; highlight the current line
(setq highlight-current-line-globally t)
(require 'highlight-current-line)
(highlight-current-line-set-bg-color "dark slate gray")

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
(when (string-match "darwin" system-configuration)
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

;; Setup burndown mode for markdown
(require 'twitter-burndown-mode)
(add-hook 'markdown-mode-hook 'twitter-burndown-mode)

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

;; moving buffers
(global-set-key [(meta left)] 'buf-move-left)
(global-set-key [(meta right)] 'buf-move-right)

