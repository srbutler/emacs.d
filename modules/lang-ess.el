;;; lang-ess.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(use-package ess-site
  :ensure ess
  :commands R
  :mode ("\\.R$" . R-mode)
  :bind (:map ess-mode-map
              ("C-;" . (lambda () (interactive) (insert " <- "))))
  :config
  (use-package smartparens-ess
    :after smartparens-mode)
  :custom
  (ess-ask-for-ess-directory nil)
  (ess-local-process-name "R")
  (ess-set-style 'RStudio)
  (ess-fancy-comments nil)
  (ansi-color-for-comint-mode 'filter)
  (comint-scroll-to-bottom-on-input t)
  (comint-scroll-to-bottom-on-output t)
  (comint-move-point-for-output t)
  (ess-toggle-underscore nil))

(provide 'lang-ess)
;;; lang-ess.el ends here
