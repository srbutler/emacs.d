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
   '(("[ \t]*\\<\\(from\\)\\>.*" 1 'font-lock-preprocessor-face)
     ("[ \t]*\\<\\(from\\)\\>.*\\<import\\>" 1 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<\\(from\\)\\>.*\\)?\\<\\(import\\)\\>" 3 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<from\\>.*\\)?\\<\\(import\\)\\>.*\\<\\(as\\)\\>" 2 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<from\\>.*\\)?\\<import\\>.*\\<\\(as\\)\\>" 2 'font-lock-preprocessor-face)
     ("\\<[\\+-]?[0-9]+\\(.[0-9]+\\|L\\)?\\>" 0 'font-lock-constant-face)
     ("\\([][{}()~^<>:=,.\\+*/%-]\\)" 0 'widget-inactive-face))))


;; major mode for requirements.txt
(use-package pip-requirements
  :straight t
  :config
  ;; if a local pip
  (if (boundp '*pip-repo-url*)
      (setq pip-requirements-index-url *pip-repo-url*)))


(use-package anaconda-mode
  :disabled t
  :straight t
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :init
  (setq anaconda-mode-installation-directory
           (expand-file-name "anaconda-mode" *savefile-dir*)))


(use-package company-anaconda
  :disabled t
  :straight t
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends '(company-anaconda :with company-capf))))


;; no config deterministic formatting
(use-package blacken
  :straight t
  ;; :hook (python-mode . blacken-mode)
  :bind (:map python-mode-map ("C-c C-f" . blacken-buffer))
  :config (setq blacken-executable "/usr/bin/black"))


;; sort imports
(use-package py-isort
  :straight t)


;; pytest intgration
(use-package python-pytest
  :straight t
  :after projectile
  :bind ("C-c C-t" . python-pytest-popup))


(use-package lsp-python-ms
  :if *python-use-lsp*
  :straight t
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))


;; install: pip install -U jedi rope pyflakes yapf
(use-package elpy
  :disabled t
  :unless *python-use-lsp*
  :straight t
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
          elpy-module-django))

  ;; fallback to rgrep if goto-def fails
  (defun elpy-goto-definition-or-rgrep ()
    "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
    (interactive)
    (ring-insert find-tag-marker-ring (point-marker))
    (condition-case nil (elpy-goto-definition)
      (error (elpy-rgrep-symbol
              (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))
  (bind-key "M-." 'elpy-goto-definition-or-rgrep elpy-mode-map))


;; set up pyenv
(use-package pyenv-mode
  :disabled t
  :when (executable-find "pyenv")
  :straight t
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
  :straight t
  :mode (("\\.pyx\\'"  . cython-mode)
         ("\\.spyx\\'" . cython-mode)
         ("\\.pxd\\'"  . cython-mode)
         ("\\.pxi\\'"  . cython-mode))
  :config
  (font-lock-add-keywords
   'cython-mode
   '(("[ \t]*\\<\\(from\\)\\>.*" 1 'font-lock-preprocessor-face)
     ("[ \t]*\\<\\(from\\)\\>.*\\<import\\>" 1 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<\\(from\\)\\>.*\\)?\\<\\(import\\)\\>" 3 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<from\\>.*\\)?\\<\\(import\\)\\>.*\\<\\(as\\)\\>" 2 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<from\\>.*\\)?\\<import\\>.*\\<\\(as\\)\\>" 2 'font-lock-preprocessor-face)
     ("\\<[\\+-]?[0-9]+\\(.[0-9]+\\|L\\)?\\>" 0 'font-lock-constant-face)
     ("\\([][{}()~^<>:=,.\\+*/%-]\\)" 0 'widget-inactive-face))))


(provide 'lang-python)
;;; lang-python.el ends here
