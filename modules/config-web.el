;;; package --- Summary:
;; config-web.el
;;
;;; Commentary:
;;
;;; Code:

;; setup emmet
(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'html-mode-hook 'emmet-mode)
  :config
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  )

;; set up skewer browser REPL
(use-package skewer-mode
  :ensure t
  :commands skewer-mode run-skewer
  :config (skewer-setup))

;; call subword mode for camelCase items
(add-hook 'html-mode-hook 'subword-mode)
