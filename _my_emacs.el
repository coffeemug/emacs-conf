
;; First, define a function that loads libraries only if available
(defun require-if-available (&rest args)
  "require symbols, load-library strings, 
   fail silently if some aren't available"
  (let (lib)
    (condition-case err
      (mapc (lambda (e)
              (setq lib e)
              (cond
                ((stringp e) (load-library e))
                ((symbolp e) (require e)))) args)
      (file-error  (progn (message "Couldn't load extension: %s" lib) nil)))))

;; Start in viper mode
(setq viper-mode t)
(require-if-available 'viper)

;; colors
(set-background-color "black")
(set-foreground-color "green")
(set-face-foreground 'region "white")
(set-face-background 'region "SkyBlue4")

;; scratch buffer
(setq initial-scratch-message "")

;; turn off tabs once and for all
(setq-default indent-tabs-mode nil)

;; arch specific stuff
(defun darwinp ()
  (string-match "darwin" system-configuration))

(defun darwin-new-p ()
  (string-match "darwin9.6" system-configuration))

(defun linuxp ()
  (string-match "linux" system-configuration))

(defun win32p ()
  (string-match "mingw-nt" system-configuration))

(defun init-linux ()
  (setq default-frame-alist
        '((width . 110)
          (height . 67)))
 
  (setq initial-frame-alist
        '((top . 28)
          (left . 483)))
  (setq gnus-signature-file "~/.signature")
  (setq projects-root "/home/coffeemug")
  (set-scroll-bar-mode 'right)
  (when (string-match "23.0" (emacs-version))
    (set-default-font "Monospace-8.5"))
  (setq x-select-enable-clipboard t)
  (add-to-list 'load-path "~/.emacs.d")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m")
  (add-to-list 'load-path "~/emacs-conf/"))

(defun init-darwin ()
  (setq mac-command-modifier 'meta)
  (setq default-frame-alist
	'((width . 117)
	  (height . 45)))
  (setq initial-frame-alist
	'((top . 24)
	  (left . 162)))
  (setq gnus-signature-file "~/.signature")
  (when (boundp 'mac-input-method-mode)
    (mac-input-method-mode 0))
  (setq projects-root "/Users/coffeemug")
  (add-to-list 'load-path "~/projects/emacs-conf/")
  (add-to-list 'load-path "~/emacs-conf/")
  (add-to-list 'load-path "~/projects/emacs-conf/w3m")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp")
  (when (darwin-new-p)
    (setq default-frame-alist
          '((width . 117)
            (height . 53)))
    (setq initial-frame-alist
          '((top . 24)
            (left . 575)))))

(defun init-win32 ()
  (setq projects-root "c:/"))

(cond ((linuxp) (init-linux))
      ((darwinp) (init-darwin))
      ((win32p) (init-win32)))

;; always save eshell history without asking
(setq eshell-save-history-on-exit t)
(setq eshell-ask-to-save-history 'always)

;; highlight current line
(setq highlight-current-line-globally t)
(require-if-available "highlight-current-line")
(when (featurep 'highlight-current-line)
  (highlight-current-line-set-bg-color "dark slate gray")
  ;; We get an issue with slime on ppc, so turn off
  (setq highlight-current-line-ignore-regexp
        "Faces\\|Colors\\| \\*Mini\\|\\*slime-repl.* \\|\\*sldb.*\\|\\*Python.*\\|\\*prolog\\*"))

;; ;; haskell mode (commenting out to speed up startup)
;; (require-if-available "haskell-mode/haskell-site-file")
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
;; (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
;; (add-hook 'haskell-mode-hook 'eldoc-mode)

;; ;; custom haskell shortcuts
;; (defun custom-haskell-shortcuts ()
;;   (local-set-key [?\C-c ?\=] 'haskell-indent-insert-equal)
;;   (local-set-key [?\C-c ?\\] 'haskell-indent-insert-guard)
;;   (local-set-key [?\C-c ?\o] 'haskell-indent-insert-otherwise)
;;   (local-set-key [?\C-c ?\w] 'haskell-indent-insert-where)
;;   (local-set-key [?\C-c ?\.] 'haskell-indent-align-guards-and-rhs)
;;   (local-set-key [(control meta down-mouse-3)] 'imenu))

;; (add-hook 'haskell-mode-hook 'custom-haskell-shortcuts)

;; set up slime for lisp development
(defun init-swank (port-file _)
  (format "(swank:start-server %S)\n\n" port-file))

(setq slime-lisp-implementations
      `(,@(when (darwinp)
	    `((openmcl ("openmcl"))))
	(sbcl ("sbcl")
	      :init ,#'init-swank)
	(acl ("alisp")
	     :init ,#'init-swank)))
(require-if-available 'slime)
(if (featurep 'slime)
    (progn
      (slime-setup)
      (setq slime-startup-animation nil)))

;; set up LaTeX mode (commenting out to speed up startup)
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
(load "auctex.el" t t t)
(setq-default TeX-PDF-mode t)
(require-if-available 'doc-view)
(when (featurep 'doc-view)
  (add-hook 'doc-view-mode-hook 'auto-revert-mode))

;; ibuffer
(global-set-key [?\C-x ?\C-b] 'ibuffer)

;; custom shortcuts for window management
(global-set-key [?\C-x ?\\] 'split-window-horizontally)
(global-set-key [?\C-x ?\-] 'split-window-vertically)

;; htmlize package (commenting out to speed up startup)
;; (require-if-available "htmlize")

;; allow font locking for all files
(global-font-lock-mode 1)

;; turn off unnecessary UI
(tool-bar-mode 0)
(menu-bar-mode 0)

;; load IDO for quick file/buffer switching
(require-if-available 'ido)
(ido-mode t)

;; IDO for M-x
;; (when (featurep 'ido)
;;   (setq ido-execute-command-cache nil)

;;   (defun ido-execute-command ()
;;     (interactive)
;;     (call-interactively
;;      (intern
;;       (ido-completing-read
;;        "M-x "
;;        (progn
;;          (unless ido-execute-command-cache
;;            (mapatoms (lambda (s)
;;                        (when (commandp s)
;;                          (setq ido-execute-command-cache
;;                                (cons (format "%S" s) ido-execute-command-cache))))))
;;          ido-execute-command-cache)))))
    
;;   (global-set-key "\M-x" 'ido-execute-command))

;; nxhtml (commenting out to speed up startup)
;; (require-if-available "nxml-mode/rng-auto.el")
;; (require-if-available "nxhtml/nxhtml-autoload")

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(erb-delim-face ((t nil)))
 '(erb-face ((t nil)))
 '(erb-out-delim-face ((t (:inherit erb-delim-face :foreground "darkred" :weight bold))))
 '(message-cited-text ((((class color) (background dark)) (:foreground "chocolate1"))))
 '(message-header-name ((((class color) (background dark)) (:foreground "LightSkyBlue"))))
 '(message-header-other ((((class color) (background dark)) (:foreground "LightSalmon"))))
 '(message-separator ((((class color) (background dark)) (:foreground "cyan1"))))
 '(nxml-attribute-local-name-face ((t (:inherit font-lock-variable-name-face))))
 '(nxml-attribute-value-face ((t (:inherit font-lock-string-face))))
 '(nxml-comment-content-face ((t (:inherit font-lock-comment-face :slant italic))))
 '(nxml-comment-delimiter-face ((t (:inherit font-lock-comment-delimiter-face))))
 '(nxml-delimiter-face ((((class color) (background dark)) (:inherit nxml-element-local-name-face))))
 '(nxml-element-local-name-face ((t (:inherit font-lock-function-name-face))))
 '(nxml-name-face ((((class color) (background dark)) (:inherit font-lock-function-name-face))))
 '(viper-minibuffer-insert ((((class color)) nil))))

;; enable paren-matching
(show-paren-mode t)

;; transient region mode
(transient-mark-mode t)

;; disable various keys to avoid temptation
(global-unset-key [\left])
(global-unset-key [\right])
(global-unset-key [\up])
(global-unset-key [\down])
(global-unset-key [\home])
(global-unset-key [\end])
(global-unset-key [\next])
(global-unset-key [\prior])

;; set title to buffer name
(setq frame-title-format "%b")

;; turn off splash screen
(setq inhibit-startup-message t)

;; Enable some "dangerous" commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-insert-query nil)
 '(completion-ignored-extensions (quote ("CM/" ".svn/" "CVS/" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".ufasl")))
 '(ido-create-new-buffer (quote always))
 '(ido-ignore-extensions t)
 '(org-agenda-files (quote ("~/todo.org")))
 '(python-guess-indent nil)
 '(python-python-command "/sw/bin/python")
 '(safe-local-variable-values (quote ((Syntax . ANSI-Common-Lisp) (Base . 10))))
 '(viper-ESC-moves-cursor-back nil)
 '(viper-case-fold-search t)
 '(w3m-default-display-inline-images t))

(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))

(setq hippie-expand-try-functions-list
  '(try-complete-abbrev
    try-complete-file-name
    try-expand-dabbrev))

;; rails stuff
;(add-hook 'ruby-mode-hook 'viper-mode)
;(require-if-available 'rails)

;; Some ex customizations (we must do this after we load ido)
(setq viper-read-buffer-function
     (cond 
      ((featurep 'ido) 'ido-read-buffer)
      ((featurep 'iswitchb) 'iswitchb-read-buffer)
      (t 'read-buffer)))

;; load w3m and set it to accept cookies
(setq w3m-use-cookies t)
(setq w3m-home-page "http://www.google.com")
(require-if-available 'w3m-load)
(require-if-available 'w3m-type-ahead)
(if (featurep 'w3m-type-ahead)
    (add-hook 'w3m-mode-hook 'w3m-type-ahead-mode))

;; use internal browser if available (commenting out to speed up startup)
;; (if (featurep 'w3m)
;;     (setq browse-url-browser-function 'w3m-browse-url))

;; Always end searches at the beginning of the matching expression.
(add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)
(defun custom-goto-match-beginning ()
 "Use with isearch hook to end search at first char of match."
 (when (and isearch-forward
	    isearch-other-end
	    (null isearch-mode-end-hook-quit))
  (goto-char isearch-other-end)))

;; Type brackets in pairs
(setq skeleton-pair t)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)

;; Do bracket translation
(keyboard-translate ?\( ?\[)
(keyboard-translate ?\[ ?\()
(keyboard-translate ?\) ?\])
(keyboard-translate ?\] ?\))

;; Moving lines with meta-n, meta-p
(global-set-key [(meta p)] 'move-line-up)
(global-set-key [(meta n)] 'move-line-down)

(defun move-line (&optional n)
 "Move current line N (1) lines up/down leaving point in place."
 (interactive "p")
 (when (null n)
   (setq n 1))
 (let ((col (current-column)))
   (beginning-of-line)
   (next-line 1)
   (transpose-lines n)
   (previous-line 1)
   (forward-char col)))

(defun move-line-up (n)
 "Moves current line N (1) lines up leaving point in place."
 (interactive "p")
 (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
 "Moves current line N (1) lines down leaving point in place."
 (interactive "p")
 (move-line (if (null n) 1 n)))

;; Improve copying and killing lines (m-w, c-w on an empty region)
(defadvice kill-ring-save (before slickcopy activate compile)
 "When called interactively with no active region, copy a single line instead."
 (interactive
  (if mark-active (list (region-beginning) (region-end))
    (list (line-beginning-position)
          (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
 "When called interactively with no active region, kill a single line instead."
 (interactive
  (if mark-active (list (region-beginning) (region-end))
    (list (line-beginning-position)
          (line-beginning-position 2)))))

;; Turn off startup message for gnus
(setq gnus-inhibit-startup-message t)

;; Initialize bbdb (commenting out to speed up startup)
;; (require-if-available 'bbdb)
;; (if (featurep 'bbdb)
;;     (progn
;;       (bbdb-initialize 'gnus 'message 'w3)
;;       (setq bbdb-offer-save 'save-without-asking)
;;       (setq bbdb-complete-name-allow-cycling t)
;;       (autoload 'bbdb/send-hook "moy-bbdb" "Save contact on send" t)
;;       (add-hook 'message-send-hook 'bbdb/send-hook)))

;; Set russian as the toggle input method
(setq default-input-method "cyrillic-translit")

;; Set the russian font on linux to a normal size
;; Not the best solution, replace when smtg better comes along
;(if (string-match "linux" system-configuration)
;    (set-fontset-font "-unknown-dejavu sans mono-medium-r-normal--13-*-*-*-*-*-fontset-startup"
;		      'cyrillic-iso8859-5
;		      "-*-*-*-*-*--*-*-*-*-*-*-*"))

;; turn cursor into a bar (not required for viper)
;;(require-if-available 'bar-cursor)
;;(bar-cursor-mode 1)
;;(set-cursor-color "gray")

;; Activate org mode (commenting out to speed up startup)
;; (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; (setq org-cycle-include-plain-lists t)

;; stroustrup-style indentation for C
(setq c-default-style "stroustrup")

;; tcc extension
(setq auto-mode-alist (cons '("\\.tcc$" . c++-mode) auto-mode-alist))

;; Let \c-c \c-c do compile in C mode
(defun my-cc-mode-hook ()
  (local-set-key "\C-c\C-c" 'compile))

(add-hook 'c-mode-common-hook 'my-cc-mode-hook)

;; Kernel hacking style blessed by Linus
(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8))

(setq auto-mode-alist (cons '("/usr/src/linux.*/.*\\.[ch]$" . linux-c-mode)
                        auto-mode-alist))

;; SML hook
(defun my-sml-mode-hook ()
  (local-set-key "\C-c\C-z" 'switch-to-sml))

(add-hook 'sml-mode-hook 'my-sml-mode-hook)

;; Dim parens
(require-if-available 'parenface)

(when (linuxp)
  ;; set final size
  (set-frame-size (selected-frame) 110 67))

;; Snippets
(require-if-available 'yasnippet-bundle)
(when (featurep 'yasnippet-bundle)
  (yas/define 'lisp-mode "rwb" "(defmethod render-widget-body ((obj ${class-name}) &rest args)
$>(declare (ignore args))
$>${0:&body})")
  (yas/define 'lisp-mode "iia" "(defmethod initialize-instance :after ((obj ${class-name}) &rest args)
$>(declare (ignore args))
$>${0:&body})"))


;; Traversing files
(require-if-available 'traverselisp)

;; Turn off the beep
(setq ring-bell-function (lambda ()))

;; Better tag selection
(require-if-available 'etags-select)
(when (featurep 'etags-select)
  (global-set-key "\M-." 'etags-select-find-tag))

;; More C goodness (remove compilation window annoyances)
(setq compilation-window-height 16)

(defun custom-compilation-window-finish (buf str)
  (save-current-buffer
    (set-buffer buf)
    (let ((buf-text (buffer-string)))
      (when (string-match "Compilation" buf-text)
        (if (or (string-match "error" buf-text)
                (string-match "warning" buf-text)
                (string-match "abnormally" buf-text))
            (message "Compilation problems, press C-x ` to visit")
          (run-at-time 1.5 nil 'delete-windows-on buf)
          (message "No compilation problems"))))))

(push 'custom-compilation-window-finish
      compilation-finish-functions)

;; Full screen mode
(defun mac-toggle-max-window (arg)
  (interactive "P")
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen)
                           (progn
                             (scroll-bar-mode 1)
                             (setq mode-line-format (default-value 'mode-line-format))
                             nil)
                         (progn
                           (scroll-bar-mode -1)
                           (when arg
                             (setf mode-line-format nil))
                           'fullboth))))

(global-set-key [(meta return)] 'mac-toggle-max-window)

;; Autocomplete stuff
(require-if-available 'auto-complete)
(when (featurep 'auto-complete)
  (global-auto-complete-mode t)
  (setq ac-auto-start nil)
  ;; Some autocomplete keys
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)
  (define-key ac-complete-mode-map [return] 'ac-complete)
  ;; Complete
  (defun do-tab-complete ()
    ;; Try autocomplete, if fail, indent.
    (interactive)
    (if (active-minibuffer-window)
        (minibuffer-complete)
      (if (eql (ac-start) nil)
          (indent-for-tab-command))))
  (global-set-key [tab] 'do-tab-complete))

;; Outline mode
(setq-default outline-regexp "[  ]*- ")
(require-if-available 'outline-magic)

;; CC mode customizations/extenions
;(load "cc-mode-ext.el")

;; Make tramp faster
(setq vc-ignore-dir-regexp
                (format "\\(%s\\)\\|\\(%s\\)"
                        vc-ignore-dir-regexp
                        tramp-file-name-regexp))

(setq tramp-verbose 0)

;; Fix me mode (for highlight todo/fixme/etc)
(require-if-available 'fixme-mode)

;; We want our cursor to blink
(blink-cursor-mode 1)

;; yaml
(require-if-available 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
