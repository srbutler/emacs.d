;;; lang-python.el --- Summary:
;;
;;; Commentary:
;;
;; this python and elpy setup taken from
;; https://github.com/seanfarley/dot-files/blob/master/emacs-python.org
;;
;;; Code:

(defvar *python-use-lsp* nil)
;; install: pip install \"python-language-server[all]\"

(use-package python
  :mode (("\\.py\\'" . python-mode)
         ("\\.wsgi$" . python-mode))
  :init
  (when *python-use-lsp*
    (add-hook 'python-mode-hook 'lsp))

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
    (counsel-gtags-mode -1)
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
   '(("[ \t]*\\<\\(from\\)\\>.*\\<import\\>" 1 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<\\(from\\)\\>.*\\)?\\<\\(import\\)\\>" 3 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<from\\>.*\\)?\\<\\(import\\)\\>.*\\<\\(as\\)\\>" 2 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<from\\>.*\\)?\\<import\\>.*\\<\\(as\\)\\>" 2 'font-lock-preprocessor-face)
     ("\\<[\\+-]?[0-9]+\\(.[0-9]+\\|L\\)?\\>" 0 'font-lock-constant-face)
     ("\\([][{}()~^<>:=,.\\+*/%-]\\)" 0 'widget-inactive-face))))


;; major mode for requirements.txt
(use-package pip-requirements
  :disabled t  ;; seems to cause some freezes
  :ensure t
  :config
  ;; if a local pip
  (if (boundp '*pip-repo-url*)
      (setq pip-requirements-index-url *pip-repo-url*)))


(use-package anaconda-mode
  :disabled t
  :ensure t
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :init
  (setq anaconda-mode-installation-directory
           (expand-file-name "anaconda-mode" *savefile-dir*)))


(use-package company-anaconda
  :disabled t
  :ensure t
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends '(company-anaconda :with company-capf))))


;; no config deterministic formatting
(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode)
  :config (setq blacken-executable "/usr/local/bin/black"))


;; sort imports
(use-package py-isort
  :ensure t)


;; pytest intgration
(use-package python-pytest
  :ensure t
  :after projectile
  :bind ("C-c C-t" . python-pytest-popup))


;; install: pip install -U jedi rope pyflakes yapf
(use-package elpy
  ;; :disabled t
  :unless *python-use-lsp*
  :ensure t
  :after python
  :commands elpy-enable
  :bind (:map elpy-mode-map
              ("C-x C-e" . python-shell-send-defun)
              ("C-c C-f" . elpy-format-code))
  :hook (python-mode . elpy-mode)
  :config
  ;; refactoring backend (jedi vs. rope)
  (setq elpy-rpc-backend "jedi")

  ;; set up elpy modules
  (setq elpy-modules
        '(elpy-module-sane-defaults
          elpy-module-company
          elpy-module-eldoc
          elpy-module-yasnippet
          elpy-module-django)))


;; set up pyenv
(use-package pyenv-mode
  :when (executable-find "pyenv")
  :ensure t
  :hook python-mode
  :config
  (define-key pyenv-mode-map (kbd "C-c C-s") nil)

  (defun setup-pyenv-mode ()
    (let ((target-file (expand-file-name ".python-version" (projectile-project-root))))
      (when (file-exists-p target-file)
        (pyenv-mode-set (with-temp-buffer
                          (insert-file-contents target-file)
                          (current-word))))))
  (add-hook 'projectile-switch-project-hook 'setup-pyenv-mode))


;; cython-mode configuration
(use-package cython-mode
  :ensure t
  :mode (("\\.pyx\\'"  . cython-mode)
         ("\\.spyx\\'" . cython-mode)
         ("\\.pxd\\'"  . cython-mode)
         ("\\.pxi\\'"  . cython-mode))
  :config
  (font-lock-add-keywords
   'python-mode
   '(("[ \t]*\\<\\(from\\)\\>.*\\<import\\>" 1 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<\\(from\\)\\>.*\\)?\\<\\(import\\)\\>" 3 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<from\\>.*\\)?\\<\\(import\\)\\>.*\\<\\(as\\)\\>" 2 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<from\\>.*\\)?\\<import\\>.*\\<\\(as\\)\\>" 2 'font-lock-preprocessor-face)
     ("\\<[\\+-]?[0-9]+\\(.[0-9]+\\|L\\)?\\>" 0 'font-lock-constant-face)
     ("\\([][{}()~^<>:=,.\\+*/%-]\\)" 0 'widget-inactive-face))))


(provide 'lang-python)
;;; lang-python.el ends here
