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

  ;; set custom keywords for python-mode
  (font-lock-add-keywords 'python-mode
                          '(
                            ("[ \t]*\\<\\(from\\)\\>" 1 'font-lock-preprocessor-face)
                            ("[ \t]*\\<\\(from\\)\\>.*\\<import\\>" 1 'font-lock-preprocessor-face)
                            ("[ \t]*\\(\\<\\(from\\)\\>.*\\)?\\<\\(import\\)\\>" 3 'font-lock-preprocessor-face)
                            ("[ \t]*\\(\\<from\\>.*\\)?\\<\\(import\\)\\>.*\\<\\(as\\)\\>" 2 'font-lock-preprocessor-face)
                            ("[ \t]*\\(\\<from\\>.*\\)?\\<import\\>.*\\<\\(as\\)\\>" 2 'font-lock-preprocessor-face)
                            ("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0 'font-lock-constant-face)
                            ("\\([][{}()~^<>:=,.\\+*/%-]\\)" 0 'widget-inactive-face)
                            )))

(use-package elpy
  :ensure t
  :commands elpy-enable
  :init (with-eval-after-load 'python (elpy-enable))
  :config
  ;; set refactoring backend ("rope" or "jedi")
  (setq elpy-rpc-python-command "python3"
        elpy-rpc-backend "jedi")

  (setq python-shell-interpreter "ipython3"
        python-shell-interpreter-args "--simple-prompt --pprint")

  ;; helps to prevent issues with ipython/jupyter shells
  ;; https://github.com/jorgenschaefer/elpy/issues/908
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  (setenv "JUPYTER_CONSOLE_TEST" "1")

  
  ;; use ipython3 instead of standard interpreter
  (when (executable-find "ipython3")
    (elpy-use-ipython "ipython3"))

  ;; set up elpy modules
  (setq elpy-modules '(elpy-module-sane-defaults
                       elpy-module-company
                       elpy-module-eldoc
                       elpy-module-yasnippet
                       ;; elpy-module-highlight-indentation
                       ;; elpy-module-pyvenv
                       )))

;; set up pyenv
(use-package pyenv-mode
  :ensure t
  :defer t
  :init (add-hook 'python-mode-hook 'pyenv-mode)
  :config
  (progn
    (defun my-pyenv-mode-set ()
      (let ((target-file (expand-file-name ".python-version" (projectile-project-root))))
        (when (file-exists-p target-file)
          (pyenv-mode-set (with-temp-buffer
                            (insert-file-contents target-file)
                            (current-word))))))

    (add-hook 'projectile-switch-project-hook 'my-pyenv-mode-set)))

;; enable autopep8 formatting on save
;; currently disabled because it's kind of annoying
(use-package py-autopep8
  :ensure t
  :disabled t
  :init (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

;; cython-mode configuration
(use-package cython-mode
  :ensure t
  :mode (("\\.pyx\\'"  . cython-mode)
         ("\\.spyx\\'" . cython-mode)
         ("\\.pxd\\'"  . cython-mode)
         ("\\.pxi\\'"  . cython-mode))
  :config
  (font-lock-add-keywords 'cython-mode
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
