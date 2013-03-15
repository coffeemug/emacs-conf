
 ;; autoinsert C/C++ header
(require 'autoinsert)
(auto-insert-mode)
(define-auto-insert
  (cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "My C / C++ header")
  ['(nil
     "\n"
     (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
            (nopath (file-name-nondirectory noext))
            (ident (concat "__" (upcase nopath) "_HPP__")))
       (concat "#ifndef " ident "\n"
               "#define " ident  "\n\n"
               "\n\n#endif // " ident "\n"))
     "\n")
   (lambda ()
     (previous-line 4))])

;; Toggle source and header files
(defun source-file-p (filename)
  (when (string-match ".*\\.cc$" filename)
    t))

(defun header-file-p (filename)
  (when (string-match ".*\\.hpp$" filename)
    t))

(defun toggle-source/header (&optional other-window)
  (interactive)
  (let ((filename (buffer-file-name)))
    (cond
     ((source-file-p filename)
      (let* ((dirname (replace-regexp-in-string "/src/" "/src/include/" filename))
             (name (replace-regexp-in-string "cc$" "hpp" dirname)))
        (setq filename name)))
     ((header-file-p filename)
      (let* ((dirname (replace-regexp-in-string "/src/include/" "/src/" filename))
             (name (replace-regexp-in-string "hpp$" "cc" dirname)))
        (setq filename name))))
    (if (or (equalp filename (buffer-file-name))
            (not (file-exists-p filename)))
        (message
         (if (source-file-p filename)
             "Header file could not be found"
           "Source file could not be found"))
      (if other-window
          (find-file-other-window filename)
        (find-file filename)))))

(defun toggle-source/header-other-window ()
  (interactive)
  (toggle-source/header 1))

;; Toggle source files and unit tests
(defun unit-test-p (filename)
  (when (string-match ".*/test/.*\\.cc$" filename)
    t))

(defun toggle-source/unit-test (&optional other-window)
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (unit-test-p filename)
        (progn
          (setq filename (replace-regexp-in-string "/test/" "/src/" filename))
          (unless (file-exists-p filename)
            (setq filename (replace-regexp-in-string "/src/" "/src/include/" filename))
            (setq filename (replace-regexp-in-string "cc$" "hpp" filename))))
      (progn
        (setq filename (replace-regexp-in-string "/src/" "/test/" filename))
        (when (header-file-p filename)
          (setq filename (replace-regexp-in-string "/include" "" filename))
          (setq filename (replace-regexp-in-string "hpp$" "cc" filename)))))
    (if (or (equalp filename (buffer-file-name))
            (not (file-exists-p filename)))
        (message
         (if (unit-test-p filename)
             "Source file could not be found"
           "Unit test could not be found"))
      (if other-window
          (find-file-other-window filename)
        (find-file filename)))))

(defun toggle-source/unit-test-other-window ()
  (interactive)
  (toggle-source/unit-test 1))

(add-hook 'c-initialization-hook
          (lambda ()
            (define-key c++-mode-map "\C-cu"  'toggle-source/unit-test)
            (define-key c++-mode-map "\C-c\C-u" 'toggle-source/unit-test-other-window)
            (define-key c++-mode-map "\C-ct"  'toggle-source/header)
            (define-key c++-mode-map "\C-c\C-t" 'toggle-source/header-other-window)))

