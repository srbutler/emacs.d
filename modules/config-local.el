;;; config-local.el -- Summary
;;
;;; Commentary:
;;
;;; Code:

;; start the daemon if not already running
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))


;; setup PYIM Chinese input method
(use-package pyim
  :ensure t
  :demand t
  :diminish (pyim-isearch-mode . "pyim")
  :bind (("C-x p" . pyim-convert-string-at-point)
         ("C-;" . pyim-delete-word-from-personal-buffer))
  :config
  (pyim-isearch-mode 1)
  (setq default-input-method "pyim"
        pyim-default-scheme 'quanpin
        pyim-page-tooltip 'popup
        pyim-page-length 5
        pyim-dcache-directory (expand-file-name "pyim/dcache/" *savefile-dir*))

  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation)))


(use-package pyim-basedict
  :ensure t
  :after pyim
  :config (pyim-basedict-enable))

(provide 'config-local)
;;; config-local.el ends here
