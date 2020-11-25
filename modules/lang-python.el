;;; lang-python.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(defvar *python-use-lsp* t)

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


;; no config deterministic formatting
(use-package blacken
  :ensure t
  ;; :hook (python-mode . blacken-mode)
  :bind (:map python-mode-map ("C-c C-f" . blacken-buffer))
  :config (setq blacken-executable "/usr/bin/black"))


;; sort imports
(use-package py-isort
  :ensure t)


;; pytest intgration
(use-package python-pytest
  :ensure t
  :after projectile
  :bind ("C-c C-t" . python-pytest-popup))


(use-package lsp-python-ms
  :if *python-use-lsp*
  :init (setq lsp-python-ms-auto-install-server t)
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))


(provide 'lang-python)
;;; lang-python.el ends here
