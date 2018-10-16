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
  :custom
  (indent-tabs-mode nil)
  (python-indent-offset 4)
  :config
    ;; use ipython instead of standard interpreter if found
  (when (executable-find "ipython")
      (setq python-shell-interpreter "ipython"
            python-shell-interpreter-args "-i --simple-prompt"))

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
              ("C-c C-r e" . elpy-multiedit-python-symbol-at-point)
              ("C-c C-f" . elpy-format-code))
  :custom
  ;; set refactoring backend ("rope" or "jedi")
  (elpy-rpc-backend "jedi")

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
