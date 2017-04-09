;;; lang-markdown.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(use-package markdown-mode
  :ensure t
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
  :config
  ;; some minor-mode hooks since it doesn't inherit from prog-mode
  (add-hook 'markdown-mode-hook 'wrap-region-mode)
  (add-hook 'markdown-mode-hook 'turn-off-auto-fill)
  (add-hook 'markdown-mode-hook 'yas-minor-mode)
  (add-hook 'markdown-mode-hook 'reftex-mode)

  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
  )

;; set up mmm-mode to automatically load major modes for code highlighting
(defun my-mmm-markdown-auto-class (lang &optional submode)
  "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
If SUBMODE is not provided, use `LANG-mode' by default."
  (let ((class (intern (concat "markdown-" lang)))
        (submode (or submode (intern (concat lang "-mode"))))
        (front (concat "^```" lang "[\n\r]+"))
        (back "^```"))
    (mmm-add-classes (list (list class :submode submode :front front :back back)))
    (mmm-add-mode-ext-class 'markdown-mode nil class)))

;; mmm-mode highlights code in markdown code blocks
(use-package mmm-mode
  :ensure t
  :defer t
  :init
  ;; set up mmm-modes automatically
  (setq mmm-global-mode 'maybe)
  (mapc 'my-mmm-markdown-auto-class
        '("awk" "bibtex" "c" "clojure" "cpp" "css" "haskell" "html" "lisp"
          "makefile" "markdown" "python" "ruby" "rust" "sql" "stata" "xml"))
  ;; Mode names that differ from the language name
  (my-mmm-markdown-auto-class "fortran" 'f90-mode)
  (my-mmm-markdown-auto-class "perl" 'cperl-mode)
  (my-mmm-markdown-auto-class "shell" 'shell-script-mode)
  (my-mmm-markdown-auto-class "r" 'ess)
  (my-mmm-markdown-auto-class "latex" 'LaTeX-mode)
  (my-mmm-markdown-auto-class "javascript" 'js2-mode)

  :config
  ;; parse code buffers when idle
  (setq mmm-parse-when-idle 't)

  ;; parse buffer upon this command
  (global-set-key (kbd "C-c m") 'mmm-parse-buffer))

(provide 'lang-markdown)
;; lang-markdown.el ends here
