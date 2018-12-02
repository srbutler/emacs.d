;;; lang-markdown.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown")
  (markdown-fontify-code-blocks-natively t)
  :init
  ;; some minor-mode hooks since it doesn't inherit from prog-mode
  (add-hook 'markdown-mode-hook 'wrap-region-mode)
  (add-hook 'markdown-mode-hook 'turn-off-auto-fill)
  (add-hook 'markdown-mode-hook 'yas-minor-mode)
  :config
  (use-package smartparens-markdown
    :after smartparens-mode))


;; allows editing of code blocks using major mode in other window
;; C-c C-c or C-c ' commits, C-c C-k aborts
(use-package edit-indirect
  :ensure t)


(provide 'lang-markdown)
;; lang-markdown.el ends here
