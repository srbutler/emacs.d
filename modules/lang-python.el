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
  :interpreter ("python" . python-mode)
  :init (when *python-use-lsp* (add-hook 'python-mode-hook 'lsp))
  :config
  (setq indent-tabs-mode nil
        python-indent-offset 4)
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  ;; use ipython instead of standard interpreter if found
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt"))

  ;; set custom keywords for python-mode
  (font-lock-add-keywords
   'python-mode
   '(("[ \t]*\\<\\(from\\)\\>.*\\<import\\>" 1 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<\\(from\\)\\>.*\\)?\\<\\(import\\)\\>" 3 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<from\\>.*\\)?\\<\\(import\\)\\>.*\\<\\(as\\)\\>" 2 'font-lock-preprocessor-face)
     ("[ \t]*\\(\\<from\\>.*\\)?\\<import\\>.*\\<\\(as\\)\\>" 2 'font-lock-preprocessor-face)
     ("\\<[\\+-]?[0-9]+\\(.[0-9]+\\|L\\)?\\>" 0 'font-lock-constant-face)
     ("\\([][{}()~^<>:=,.\\+*/%-]\\)" 0 'widget-inactive-face))))


;; install: pip install -U jedi rope pyflakes yapf
(use-package elpy
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
  :ensure t
  :defer t
  :init (add-hook 'python-mode-hook 'pyenv-mode)
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
