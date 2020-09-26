;;; lang-ess.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(use-package ess-site
  :straight ess
  :commands R
  :mode ("\\.[Rr]$" . R-mode)
  :bind (:map ess-mode-map
              ("C-;" . (lambda () (interactive) (insert " <- "))))
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
