;;; lang-markdown.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:


(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook ((markdown-mode . turn-on-auto-fill)
         (markdown-mode . wrap-region-mode)
         (markdown-mode . yas-minor-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t)

  (let ((cmd "multimarkdown"))
    (when (executable-find cmd)
      (setq markdown-command cmd))))


;; allows editing of code blocks using major mode in other window
;; C-c C-c or C-c ' commits, C-c C-k aborts
(use-package edit-indirect
  :ensure t)


(provide 'lang-markdown)
;;; lang-markdown.el ends here
