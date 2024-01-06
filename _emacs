(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

(use-package use-package
  :init
  (setq use-package-always-ensure t))

(use-package emacs
  :ensure nil

  :init
  (recentf-mode)
  (add-to-list 'recentf-exclude "ido\\.last\\'")
  (savehist-mode)
  (global-prettify-symbols-mode)

  :custom
  (frame-title-format "%b")
  (ring-bell-function 'ignore)
  (inhibit-startup-message t)
  (initial-scratch-message "")
  (sentence-end-double-space nil)
  (completion-styles '(flex))
  (completion-ignore-case t)
  (frame-inhibit-implied-resize t)
  (backup-directory-alist `(("." . ,(concat user-emacs-directory "backup"))))
  (custom-file (concat user-emacs-directory "custom.el"))
  (tab-always-indent 'complete)

  :config
  (add-to-list 'load-path "~/emacs-conf/")
  (setq org-directory "~/Dropbox/Org/")

  (menu-bar-mode 0)
  (show-paren-mode t)
  (transient-mark-mode t)
  (electric-pair-mode t)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defun display-startup-echo-area-message ()
    (message "Let the hacking begin!"))
  (toggle-text-mode-auto-fill)

  (when (display-graphic-p)
    (tool-bar-mode 0)
    (scroll-bar-mode -1)
    (pixel-scroll-precision-mode)
    (setq-default cursor-type 'bar))

  ;; Platform specific
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))
  
  (when (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :font "Consolas-10")
    (bind-key "C-x <end>" #'eval-last-sexp))
  
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

  :bind (("C-q" . unfill-paragraph))

  ;; frequent shortcuts
  :config
  (defun jump-to-init-file ()
    (interactive)
    (find-file
     (file-truename user-init-file)))

  (defun find-org-file ()
    (interactive)
    (let ((default-directory org-directory))
      (ido-find-file)))

  (defun jump-to-todo-file ()
    (interactive)
    (find-file
     (concat org-directory "todo.org")))

  :bind (("C-c j ." . jump-to-init-file)
	 ("C-c j d" . jump-to-todo-file)
	 ("C-c j o" . find-org-file)
	 ("C-c j s" . scratch-buffer))
  )

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  
  :custom
  (corfu-auto t)
  (corfu-popupinfo-delay '(nil . 1.0))
  (corfu-preselect 'prompt)

  :config
  (add-to-list 'savehist-additional-variables 'corfu-history)

  :hook
  ((before-save . corfu-quit))

  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
	("C-h" . corfu-popupinfo-toggle)))

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

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

(use-package doom-modeline
  :config (doom-modeline-mode 1))

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
  (dirvish-mode-line-height doom-modeline-height)
  (dirvish-header-line-height doom-modeline-height)

  :bind (("C-x d" . dirvish-dwim)
	 ("C-x C-d" . dirvish-dwim))
  :bind (:map dirvish-mode-map
	      ("q" . dirvish-quit)
	      ("C-g" . dirvish-quit)
 	      ("k" . dired-previous-line)
 	      ("j" . dired-next-line)
	      ("<backspace>" . dired-up-directory)
	      ("<backtab>" . dired-up-directory)
	      ("RET" . dired-find-file)
	      ("TAB" . dired-find-file)
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
	      ("C-d" . jump-to-directory))
  )

(use-package which-key
  :config
  (which-key-mode))

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

  :custom
  (eldoc-echo-area-use-multiline-p nil)
  
  :hook ((c-ts-mode . eglot-ensure)
	 (c++-ts-mode . eglot-ensure)
	 (c-or-c++-ts-mode . eglot-ensure)))

(use-package eglot-x
  :demand t
  :load-path "~/eglot-x"
  :config (eglot-x-setup))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package sendmail
  ;; authentication goes to ~/.authinfo
  :ensure nil
  :custom
  (send-mail-function 'smtpmail-send-it)
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :init (exec-path-from-shell-initialize))

(use-package org
  :ensure nil

  ;; general stuff
  :custom
  (org-default-notes-file (concat org-directory "default.org"))
  (org-startup-folded 'overview)
  (org-fontify-done-headline nil)
  (org-highlight-latex-and-related '(native))

  ;; make latex rendering work nicely
  :custom
  (org-preview-latex-default-process 'dvisvgm)
  (org-preview-latex-image-directory (concat user-emacs-directory "ltximg/"))

  :config
  (plist-put org-format-latex-options :scale 1.65)
  (defun org-preview-latex-all ()
    (interactive)
    (org-latex-preview '(16)))
  (defun org-electric-pair-dollar ()
    (setq-local electric-pair-pairs (append electric-pair-pairs '((?$ . ?$)))))

  :hook ((org-mode . org-preview-latex-all)
	 (org-mode . org-electric-pair-dollar)
	 (org-ctrl-c-ctrl-c . org-toggle-latex-fragment))

  ;; style heading markup
  :config
  (setq org-heading-markup-alpha-factor 0.5)

  (defun blend-colors (color1 color2 alpha)
    (apply 'color-rgb-to-hex
           `(,@(cl-mapcar (lambda (x y) (+ (* x alpha) (* y (- 1 alpha))))
			  (color-name-to-rgb color1)
			  (color-name-to-rgb color2))
	     2)))

  (defun org-de-emphasized-face (level)
    (let ((org-face (nth (1- (min level org-n-level-faces)) org-level-faces)))
      `(:inherit ,org-face :foreground ,(blend-colors
					 (face-foreground org-face nil t)
					 (face-background 'default nil t)
					 org-heading-markup-alpha-factor))))
  
  :hook (org-mode . (lambda ()
  		      (font-lock-add-keywords
  		       nil
  		       '(("^\\(\\*+\\) "
  			  (0 (let ((level (- (match-end 0) (match-beginning 0) 1)))
  			       (put-text-property (match-beginning 0)
  						  (- (match-end 0) 1)
  						  'face
  						  (org-de-emphasized-face level)))))))))
  
  ;; style list bullet points and horizontal separators
  :hook (org-mode . (lambda ()
		      (font-lock-add-keywords
		       nil
		       '(("^[[:space:]]*\\(-\\) " 1 'shadow)
			 ("^-\\{5,\\}" 0 'shadow)))))

  ;; style emphasis markup
  :config
  (defun org-emphasis-markup-matcher (limit)
    (let* ((ok (re-search-forward org-emph-re limit t))
	   (start-pos (match-beginning 2))
	   (end-pos (match-end 2)))
      (when ok
	(set-match-data
	 (list start-pos end-pos
	       start-pos (+ start-pos 1)
	       (- end-pos 1) end-pos)))
      ok))

  :hook (org-mode . (lambda ()
		      (font-lock-add-keywords
		       nil
		       `((org-emphasis-markup-matcher
			  (1 'shadow)
			  (2 'shadow))))))
  )

(use-package org-capture
  :ensure nil

  :init
  (setq org-capture-templates-shared
	`(("s" "Shoebox" entry (file "shoebox.org")
	   "* %<%Y-%m-%d, %a %H:%M>\n%?"
	   :prepend t
	   :empty-lines-after 2
	   :kill-buffer t)))

  (setq org-capture-templates-home
	`(,@org-capture-templates-shared
	  ("t" "Quote" entry (file "quotes.org")
	   "* %<%Y-%m-%d, %a %H:%M>\n%?"
	   :prepend t
	   :empty-lines-after 2
	   :kill-buffer t)))

  (setq org-capture-templates-work
	`(,@org-capture-templates-shared
	  ("a" "Achieved" item (file+olp+datetree "achieved.org")
	   "%?"
	   :tree-type week
	   :empty-lines-after 2
	   :kill-buffer t)
	  ("m" "Meeting" entry (file+olp+datetree "meetings.org")
	   "* %<%H:%M>, %?"
	   :tree-type week
	   :empty-lines-after 2
	   :kill-buffer t)))

  :custom
  (org-capture-templates
   (if (is-work-p)
       org-capture-templates-work
     org-capture-templates-home))

  :bind (("C-c c" . org-capture))
  )

(use-package org-ql
  :config
  (defun org-fold-done-headings ()
    (interactive)
    (org-with-wide-buffer
     (dolist (headline (org-ql-query :select 'element
                                     :from (current-buffer)
                                     :where '(or (done))  
                                     :order-by 'date))
       (let ((start (org-element-property :begin headline)))
	 (goto-char start)
	 (outline-hide-subtree)))))
  :hook (org-mode . org-fold-done-headings))

;; latex completion in org mode
(use-package cape)
(use-package math-symbol-lists
  :config
  (defun cape-latex ()
    (let ((bounds (cape--bounds 'word))
	  command-list)
      (setf command-list
	    (delete-dups
	     (append
	      (mapcar (lambda (cmd)
			(concat "\\" cmd))
		      math-symbol-list-latex-commands)
	      (mapcar #'cadr math-symbol-list-basic)
	      (mapcar #'cadr math-symbol-list-extended)
	      (mapcar #'caddr math-symbol-list-packages))))
      `(,(1- (car bounds)) ,(cdr bounds)
	,(cape--properties-table
	  command-list
	  :category 'cape-latex)
	:annotation-function (lambda (_) " TeX")
	:exclusive 'no)))
  (defun add-cape-latex-completion ()
    (add-to-list 'completion-at-point-functions #'cape-latex))
  :hook (org-mode . add-cape-latex-completion))

;; Some useful general-purpose functions
(defun is-work-p ()
  (file-exists-p
   (concat user-emacs-directory ".work")))
