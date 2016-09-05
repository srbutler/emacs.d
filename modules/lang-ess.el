;;; lang-ess.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

(use-package ess-site
  :ensure ess
  :mode ("\\.R$" . R-mode)
  :config

  (add-hook 'R-mode-hook 'yas-minor-mode)
  (add-hook 'R-mode-hook 'smartparens-mode)
  (add-hook 'R-mode-hook 'rainbow-delimiters-mode)
  
  (add-hook 'R-mode-hook
       (lambda ()
         (setq ;;ess-indent-offset tab-width
               ess-ask-for-ess-directory nil
               ess-local-process-name "R"
               ansi-color-for-comint-mode 'filter
               comint-scroll-to-bottom-on-input t
               comint-scroll-to-bottom-on-output t
               comint-move-point-for-output t)
         ))

  ;; set font lock
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:fun-calls)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters)
          (ess-fl-keyword:=)
          (ess-R-fl-keyword:F&T . t)
          (ess-R-fl-keyword:%op% . t)))

  ;; set inferior-mode font lock
  (setq inferior-R-font-lock-keywords
        '((ess-S-fl-keyword:prompt . t)
          (ess-R-fl-keyword:messages . t)
          (ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:matrix-labels . t)
          (ess-fl-keyword:fun-calls)
          (ess-fl-keyword:numbers)
          (ess-fl-keyword:operators)
          (ess-fl-keyword:delimiters)
          (ess-fl-keyword:=)
          (ess-R-fl-keyword:F&T . t)))
  )

(provide 'lang-ess)
;;; lang-ess.el ends here
