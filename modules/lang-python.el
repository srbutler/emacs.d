;;; lang-python.el --- Summary:
;;
;;; Commentary:
;;
;; this python and elpy setup taken from
;; https://github.com/seanfarley/dot-files/blob/master/emacs-python.org
;;
;;; Code:

(use-package python
  :mode (("\\.py\\'" . python-mode)
         ("\\.wsgi$" . python-mode))
  :interpreter ("python" . python-mode)
  :ensure-system-package
  ((pyls    . "pip install \"python-language-server[all]\"")
   (ipython . "pip install ipython"))
  :custom
  (indent-tabs-mode nil)
  (python-indent-offset 4)

  :config
  ;; use ipython3 instead of standard interpreter if found
  (when (executable-find "ipython")
      (progn
        ;; helps to prevent issues with ipython/jupyter shells
        ;; https://github.com/jorgenschaefer/elpy/issues/908
        (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
        (setenv "JUPYTER_CONSOLE_TEST" "1")

        ;; set as ipython
        (setq python-shell-interpreter "ipython"
              python-shell-interpreter-args "-i --simple-prompt")))

  ;; set up LSP-Python manually
  (require 'lsp-mode)
  (lsp-define-stdio-client lsp-python "python"
                           #'projectile-project-root
                           '("pyls"))
  (add-hook 'python-mode-hook
            (lambda () (lsp-python-enable)))

  ;; add smarparens to inferior-python mode
  (add-hook 'inferior-python-mode-hook 'smartparens-mode)

  ;; set custom keywords for python-mode
  (font-lock-add-keywords
   'python-mode
   '(
     ;; ("[ \t]*\\<\\(from\\)\\>" 1 'font-lock-preprocessor-face)
     ("[ \t]*\\<\\(from\\)\\>.*\\<import\\>" 1 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<\\(from\\)\\>.*\\)?\\<\\(import\\)\\>" 3 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<from\\>.*\\)?\\<\\(import\\)\\>.*\\<\\(as\\)\\>" 2 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<from\\>.*\\)?\\<import\\>.*\\<\\(as\\)\\>" 2 'font-lock-preprocessor-face)
     ("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0 'font-lock-constant-face)
     ("\\([][{}()~^<>:=,.\\+*/%-]\\)" 0 'widget-inactive-face))))


;; disabling (using LSP instead)
(use-package elpy
  :disabled t
  :ensure t
  :commands elpy-enable
  :init (with-eval-after-load 'python (elpy-enable))
  :bind (:map elpy-mode-map
              ("C-x C-e" . python-shell-send-defun)
              ("C-c C-r e" . elpy-multiedit-python-symbol-at-point))
  :custom
  ;; set refactoring backend ("rope" or "jedi")
  (elpy-rpc-backend "jedi")

  ;; set RPC backend and interpreter using pyenv values
  (elpy-rpc-python-command "~/.pyenv/shims/python3")
  :config

  ;; set up elpy modules
  (setq elpy-modules '(elpy-module-sane-defaults
                       elpy-module-company
                       elpy-module-eldoc
                       elpy-module-yasnippet
                       elpy-module-django
                       ;; elpy-module-highlight-indentation
                       ;; elpy-module-pyvenv
                       )))

;; set up pyenv
(use-package pyenv-mode
  :ensure t
  :defer t
  :init (add-hook 'python-mode-hook 'pyenv-mode)
  :config
  (define-key pyenv-mode-map (kbd "C-c C-s") nil)

  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))
  (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set))


;; cython-mode configuration
(use-package cython-mode
  :ensure t
  :mode (("\\.pyx\\'"  . cython-mode)
         ("\\.spyx\\'" . cython-mode)
         ("\\.pxd\\'"  . cython-mode)
         ("\\.pxi\\'"  . cython-mode))
  :config
  (font-lock-add-keywords
   'cython-modep
   '(
     ("[ \t]*\\<\\(from\\)\\>.*\\<import\\>" 1 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<\\(from\\)\\>.*\\)?\\<\\(import\\)\\>" 3 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<from\\>.*\\)?\\<\\(import\\)\\>.*\\<\\(as\\)\\>" 2 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<from\\>.*\\)?\\<import\\>.*\\<\\(as\\)\\>" 2 'font-lock-preprocessor-face)
     ("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0 'font-lock-constant-face)
     ("\\([][{}()~^<>:=,.\\+*/%-]\\)" 0 'widget-inactive-face)
     )))


(provide 'lang-python)
;;; lang-python.el ends here
