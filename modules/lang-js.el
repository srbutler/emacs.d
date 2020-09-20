;;; config-js.el -- Summary
;;
;;; Commentary:
;;
;;; Code:

(defvar *js-use-lsp* nil)
;; install: npm i -g javascript-typescript-langserver


;; from https://github.com/seagle0128/.emacs.d
;; Improved JavaScript editing mode
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'"  . js2-mode)
  :interpreter ("node" . js2-mode)
  :bind (:map js2-mode-map ("M-." . nil))  ;; don't conflict with xref
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode))
  :init (when *js-use-lsp* (add-hook 'js2-mode-hook 'lsp))
  :config

  (setq js2-basic-offset 2
        js-switch-indent-offset 2)  ;; indent switch/case separately

  (defun srb/toggle-js-offset ()
    "Switch Javascript indentation between 2 and 4 spaces."
    (interactive)
    (cond
     ((eq js2-basic-offset 2)
      (setq js2-basic-offset 4
            js-switch-indent-offset 4))
     ((eq js2-basic-offset 4)
      (setq js2-basic-offset 2
            js-switch-indent-offset 2))
     (t (setq js2-basic-offset 2
              js-switch-indent-offset 2))))

  (with-eval-after-load 'flycheck
    (if (executable-find "eslint")
        (setq js2-mode-show-strict-warnings nil)
      (setq flycheck-javascript-eslint-executable "eslint"))))


;; for jsx
(use-package rjsx-mode
  :ensure t
  :mode (("\\.jsx\\'" . rjsx-mode)
         ("components\\/.*\\.js\\'" . rjsx-mode))
  :interpreter ("node" . rjsx-mode)
  :hook ((rjsx-mode . js2-imenu-extras-mode)
         (rjsx-mode . js2-highlight-unused-variables-mode))
  :bind (:map js2-mode-map ("M-." . nil))  ;; don't conflict with xref
  :init (when *js-use-lsp* (add-hook 'rjsx-mode-hook 'lsp))
  :config
  (setq js2-basic-offset 4)

  (with-eval-after-load 'flycheck
    (if (executable-find "eslint")
        (setq js2-mode-show-strict-warnings nil)
      (setq flycheck-javascript-eslint-executable "eslint"))))


;; for XREF when LSP is not being used
(use-package xref-js2
  :when (not *js-use-lsp*)
  :ensure t
  :init (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))

;; REPL/dev environment
(use-package indium
  :ensure t
  :after js2-mode
  :bind (:map js2-mode-map ("C-c C-l" . indium-eval-buffer))
  :hook ((js2-mode . indium-interaction-mode))
  :config (setq indium-chrome-data-dir *savefile-dir*))


(provide 'lang-js)
;;; lang-js.el ends here
