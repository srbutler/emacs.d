;;; lang-python.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(use-package python
  :mode (("\\.py\\'" . python-mode)
         ("\\.wsgi$" . python-mode))
  :hook (python-mode . srb/python-config)
  :hook (python-mode . srb/set-python-interpreter)
  :hook (inferior-python-mode . srb/inferior-python-config)
  :preface
  ;; this will get used when venvs are activated as well
  (defun srb/set-python-interpreter (&optional venv-dir)
    "Set the python interpretor, prepending VENV-DIR if supplied."
    (let* ((venv-bin-dir (if venv-dir (concat venv-dir "bin/") nil))
           (ipy3-exe (concat venv-bin-dir "ipython3"))
           (py3-exe (concat venv-bin-dir "python3"))
           (ipy-exe (concat venv-bin-dir "ipython")))
      (cond
       ((executable-find ipy3-exe)
        (setq python-shell-interpreter ipy3-exe
              python-shell-interpreter-args "-i --simple-prompt"))
       ((executable-find py3-exe)
        (setq python-shell-interpreter py3-exe))
       ((executable-find ipy-exe)
        (setq python-shell-interpreter ipy-exe
              python-shell-interpreter-args "-i --simple-prompt"))
       (t (user-error
           "Virtualenv does not appear to be active and/or have an interpreter")))))

  (defun srb/python-config ()
    (setq-local tab-width 4
                fill-column 88
                indent-tabs-mode nil))

  (defun srb/inferior-python-config ()
    (indent-tabs-mode t)
    (rainbow-delimiters-mode t)
    (smartparens-strict-mode t))

  :init (setq python-indent-guess-indent-offset-verbose nil)
  :config
  ;; interferes with smartparens
  (define-key python-mode-map (kbd "DEL") nil)
  (sp-local-pair 'python-mode "'" nil
                 :unless '(sp-point-before-word-p
                           sp-point-after-word-p
                           sp-point-before-same-p)))


;; major mode for requirements.txt
(use-package pip-requirements
  :ensure t
  :config
  ;; if a local pip
  (if (boundp '*pip-repo-url*)
      (setq pip-requirements-index-url *pip-repo-url*)))


;; manage virtualenvs
(use-package pyvenv
  :ensure t
  :after python-mode
  :hook (python-mode . pyvenv-mode)
  :bind (:map python-mode-map ("C-c C-x v" . pyvenv-workon))
  :config
  (setq pyvenv-post-activate-hooks (list (srb/set-python-interpreter pyvenv-virtual-env)))
  (setq pyvenv-post-deactivate-hooks (list (srb/set-python-interpreter))))


;; pytest intgration
(use-package python-pytest
  :ensure t
  :bind (:map python-mode-map ("C-c C-x t" . python-pytest-dispatch)))


;; better LSP server
(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :preface
  (defun srb/lsp-python-init ()
    (require 'lsp-python-ms)
    (lsp))
  :hook (python-mode . srb/lsp-python-init))


;; no config deterministic formatting
(use-package blacken
  :ensure t
  :config (setq blacken-executable "/usr/bin/black"))


;; sort imports
(use-package py-isort
  :ensure t
  :config (setq py-isort-options "--profile black"))


;; clean imports
(use-package pyimport
  :ensure t
  :config (setq pyimport-pyflakes-path "/usr/bin/pyflakes"))


(defun srb/python-cleanup-imports ()
  "Remove unused imports and sort."
  (interactive)
  (pyimport-remove-unused)
  (py-isort-buffer))
(bind-key "C-c C-x i" 'srb/python-cleanup-imports python-mode-map)


(defun srb/python-format-buffer ()
  "Clean up imports and format with black."
  (interactive)
  (srb/python-cleanup-imports)
  (blacken-buffer))
(bind-key "C-c C-x f" 'srb/python-format-buffer python-mode-map)


(provide 'lang-python)
;;; lang-python.el ends here
