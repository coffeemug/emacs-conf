;; load path
(add-to-list 'load-path "~/projects/emacs-conf/")

;; load theme
(load-theme 'wombat)

;; set frame title to file name
(setq frame-title-format "%b")

;; turn off splash screen
(setq inhibit-startup-message t)

;; turn off initial scratch buffer message
(setq initial-scratch-message "")

;; turn off unnecessary UI
(menu-bar-mode 0)

;; make yes/no less annoying
(defalias 'yes-or-no-p 'y-or-n-p)

;; basic colors (for GUIs and evil terminals)
(set-background-color "black")
(set-foreground-color "green")
(set-face-foreground 'region "white")
(set-face-background 'region "SkyBlue4")

;; highlight the current line
;; TODO: turn it off for things like eshell, inferior modes, etc.
(setq highlight-current-line-globally t)
(require 'highlight-current-line)
(highlight-current-line-set-bg-color "dark slate gray")

;; enable paren-matching
(show-paren-mode t)

;; load IDO for quick file/buffer switching
(require 'ido)
(ido-mode t)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)

;; rebind incremental search to regex
(global-set-key [(control s)] 'isearch-forward-regexp)
(global-set-key [(control r)] 'isearch-backward-regexp)
(global-set-key [(meta %)] 'query-replace-regexp)
(global-set-key [(control meta s)] 'isearch-forward)
(global-set-key [(control meta r)] 'isearch-backward)
(global-set-key [(control meta %)] 'query-replace)

;; rebind C-x C-m to M-x (Steve Yegge was right)
(global-set-key [(control x) (control m)] 'execute-extended-command)

;; type brackets in pairs
;; TODO: only turn on pairing in programming modes
(setq skeleton-pair t)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)

;; customize isearch to always end at the beginning of search word
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))
(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

;; make regions behave like every other sane editor on the planet
;; (default in newever versions of emacs, but not everywhere)
(transient-mark-mode t)

;; osx-specific instructions
(defun darwinp ()
  (string-match "darwin" system-configuration))

(when (darwinp)
  ;; make apple-command be the meta modifier
  (setq mac-command-modifier 'meta))

;; graphics mode specific instructions
(when (display-graphic-p)
  (tool-bar-mode 0))

;; eshell shouldn't ask about saving history on exit
(setq eshell-save-history-on-exit t)

;; markdown mode (err, I do write a lot of markdown)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; other window
(global-set-key [(control o)] 'other-window)
