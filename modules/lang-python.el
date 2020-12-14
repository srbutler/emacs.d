;;; lang-python.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(use-package python
  :mode (("\\.py\\'" . python-mode)
         ("\\.wsgi$" . python-mode))
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (setq tab-width 4)
                                (setq fill-column 88)))
  :config
  (setq indent-tabs-mode nil
        python-indent-offset 4)

  ;; use ipython instead of standard interpreter if found
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt"))

  ;; inferior python shell setup
  (defun srb/inferior-python-config ()
    (indent-tabs-mode t)
    (rainbow-delimiters-mode t)
    (smartparens-strict-mode t))
  (add-hook 'inferior-python-mode-hook 'srb/inferior-python-config)

  (defun send-input-and-indent()
    (interactive)
    (comint-send-input)
    (indent-for-tab-command))
  (bind-key "<enter>" 'send-input-and-indent inferior-python-mode-map)
  (bind-key "C-j" 'send-input-and-indent inferior-python-mode-map)

  ;; set custom keywords for python-mode
  (font-lock-add-keywords
   'python-mode
   '(("[ \t]*\\<\\(from\\)\\>.*" 1 'font-lock-preprocessor-face)
     ("[ \t]*\\<\\(from\\)\\>.*\\<import\\>" 1 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<\\(from\\)\\>.*\\)?\\<\\(import\\)\\>" 3 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<from\\>.*\\)?\\<\\(import\\)\\>.*\\<\\(as\\)\\>" 2 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<from\\>.*\\)?\\<import\\>.*\\<\\(as\\)\\>" 2 'font-lock-preprocessor-face)
     ("\\<[\\+-]?[0-9]+\\(.[0-9]+\\|L\\)?\\>" 0 'font-lock-constant-face)
     ("\\([][{}()~^<>:=,.\\+*/%-]\\)" 0 'widget-inactive-face))))


;; major mode for requirements.txt
(use-package pip-requirements
  :ensure t
  :config
  ;; if a local pip
  (if (boundp '*pip-repo-url*)
      (setq pip-requirements-index-url *pip-repo-url*)))


;; pytest intgration
(use-package python-pytest
  :ensure t
  :after projectile
  :bind (:map python-mode-map ("C-c C-p t" . python-pytest-dispatch)))


;; better LSP server
(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t)
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))


;; no config deterministic formatting
(use-package blacken
  :ensure t
  :config (setq blacken-executable "/usr/bin/black"))


;; sort imports
(use-package py-isort
  :ensure t)


;; clean imports
(use-package pyimport
  :ensure t
  :config (setq pyimport-pyflakes-path "/usr/bin/pyflakes"))


(defun srb/python-cleanup-imports ()
  "Remove unused imports and sort."
  (interactive)
  (progn
    (pyimport-remove-unused)
    (py-isort-buffer)))
(bind-key "C-c C-p i" 'srb/python-cleanup-imports python-mode-map)


(defun srb/python-format-buffer ()
  "Clean up imports and format with black."
  (interactive)
  (progn
    (srb/python-cleanup-imports)
    (blacken-buffer)))
(bind-key "C-c C-p f" 'srb/python-format-buffer python-mode-map)


(provide 'lang-python)
;;; lang-python.el ends here
