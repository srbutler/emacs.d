;;; lang-python.el --- Summary:
;;
;;; Commentary:
;;
;; this python and elpy setup taken from
;; https://github.com/seanfarley/dot-files/blob/master/emacs-python.org
;;
;;; Code:

(use-package python
  :mode ("\\.py\\'" . python-mode)
        ("\\.wsgi$" . python-mode)
  :interpreter ("python" . python-mode)
  :init (setq-default indent-tabs-mode nil)
  :config
  (setq python-indent-offset 4)

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

(use-package elpy
  :ensure t
  :commands elpy-enable
  :init (with-eval-after-load 'python (elpy-enable))
  :bind (:map elpy-mode-map
              ("C-x C-e" . python-shell-send-defun)
              ("C-c C-r e" . elpy-multiedit-python-symbol-at-point))
  :config
  ;; set refactoring backend ("rope" or "jedi")
  (setq elpy-rpc-backend "jedi")

  ;; set RPC backend and interpreter using pyenv values
  (setq elpy-rpc-python-command "~/.pyenv/shims/python3")

  ;; use ipython3 instead of standard interpreter if found
  (if (executable-find "ipython")
      (progn
        ;; helps to prevent issues with ipython/jupyter shells
        ;; https://github.com/jorgenschaefer/elpy/issues/908
        (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
        (setenv "JUPYTER_CONSOLE_TEST" "1")

        ;; set as ipython
        (setq python-shell-interpreter "ipython"
              python-shell-interpreter-args "-i --simple-prompt"))

    ;; just use the standard interpreter otherwise
    (setq python-shell-interpreter "python"
          python-shell-interpreter-args "-i"))

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
  (progn
    (defun my-pyenv-mode-set ()
      (let ((target-file (expand-file-name ".python-version" (projectile-project-root))))
        (when (file-exists-p target-file)
          (pyenv-mode-set (with-temp-buffer
                            (insert-file-contents target-file)
                            (current-word))))))
    (add-hook 'projectile-switch-project-hook 'my-pyenv-mode-set)))


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
