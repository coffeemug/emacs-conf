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
(ido-mode t)                       ; load IDO for quick file/buffer switching

;; Some more niceties
(setq frame-title-format "%b")                   ; set frame title to file name
(setq inhibit-startup-message t)                 ; turn off splash screen
(setq initial-scratch-message "")                ; turn off initial scratch buffer message
(setq confirm-nonexistent-file-or-buffer nil)    ; make IDO sane
(setq ido-create-new-buffer 'always)             ; make IDO sane
(setq sentence-end-double-space nil)             ; make filling nicer
(defalias 'yes-or-no-p 'y-or-n-p)                ; make yes/no less annoying

;; basic colors (for GUIs and evil terminals)
(set-background-color "black")
(set-foreground-color "green")
(set-face-foreground 'region "white")
(set-face-background 'region "SkyBlue4")

;; nicer keybindings
(global-set-key [(control s)] 'isearch-forward-regexp)
(global-set-key [(control r)] 'isearch-backward-regexp)
(global-set-key [(meta %)] 'query-replace-regexp)
(global-set-key [(control o)] 'other-window)

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

;; "twitter" mode -- show burndown of 140 chars/peragraph (+ how many
;; 140 char sentences are written)
(defun paragraph-burndown-modeline-str ()
  (let* ((beg (save-excursion
		(move-to-left-margin)
		(forward-paragraph -1)
		(point)))
	 (end (save-excursion
		(move-to-left-margin)
		(forward-paragraph +1)
		(point)))
	 (pwidth (string-width (buffer-substring-no-properties beg end)))
	 (ntweets (/ pwidth 140))
	 (nchars (- 140 (% pwidth 140))))
    (if (zerop (string-width (thing-at-point 'line t)))
	"0x/140"
      (concat
       (format "%sx/" ntweets)
       (if (<= nchars 20)
	   (propertize (format "%s" nchars) 'face 'warning)
	 (format "%s" nchars))))))
(defun paragraph-burndown-modeline-hook ()
  (setq mode-line-format
	(append mode-line-format '((:eval (paragraph-burndown-modeline-str))))))
(add-hook 'markdown-mode-hook 'paragraph-burndown-modeline-hook)

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

