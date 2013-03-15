;; load path
(add-to-list 'load-path "~/emacs-conf/")

;; set frame title to file name
(setq frame-title-format "%b")

;; turn off splash screen
(setq inhibit-startup-message t)

;; turn off initial scratch buffer message
(setq initial-scratch-message "")

;; turn off unnecessary UI
(tool-bar-mode 0)
(menu-bar-mode 0)

;; basic colors (for GUIs and evil terminals)
(set-background-color "black")
(set-foreground-color "green")
(set-face-foreground 'region "white")
(set-face-background 'region "SkyBlue4")

;; highlight the current line
(setq highlight-current-line-globally t)
(require 'highlight-current-line)
(highlight-current-line-set-bg-color "dark slate gray")

;; enable paren-matching
(show-paren-mode t)

;; load IDO for quick file/buffer switching
(require 'ido)
(ido-mode t)
(setq confirm-nonexistent-file-or-buffer nil)

;; rebind incremental search to regex
(global-set-key [(control s)] 'isearch-forward-regexp)
(global-set-key [(control r)] 'isearch-backward-regexp)
(global-set-key [(meta %)] 'query-replace-regexp)
(global-set-key [(control meta s)] 'isearch-forward)
(global-set-key [(control meta r)] 'isearch-backward)
(global-set-key [(control meta %)] 'query-replace)

;; type brackets in pairs
(setq skeleton-pair t)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "'") 'skeleton-pair-insert-maybe)

;; osx-specific instructions
(defun darwinp ()
  (string-match "darwin" system-configuration))

(when (darwinp)
  ;; make apple-command be the meta modifier
  (setq mac-command-modifier 'meta))

