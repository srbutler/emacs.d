;;; lang-markdown.el --- Summary:
;;
;;; Commentary:
;;  sudo dnf install pandoc
;;
;;; Code:


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :hook ((markdown-mode . turn-on-auto-fill)
         (markdown-mode . wrap-region-mode)
         (markdown-mode . yas-minor-mode))
  :config

  ;; taken from doom-emacs
  (defun srb/markdown-compile-to-html (beg end output-buffer)
    "Compiles markdown with the pandoc program, if available. Returns its exit code."
    (when (executable-find "pandoc")
      (call-process-region beg end "pandoc" nil output-buffer nil
                           "-f" "markdown"
                           "-t" "html"
                           "--mathjax"
                           "--highlight-style=pygments")))

  (setq markdown-command                      #'srb/markdown-compile-to-html
        markdown-fontify-code-blocks-natively t
        markdown-enable-math                  t
        markdown-enable-wiki-links            t
        markdown-italic-underscore            t
        markdown-asymmetric-header            t
        markdown-gfm-additional-languages     '("sh")
        markdown-make-gfm-checkboxes-buttons  t
        markdown-content-type                 "application/xhtml+xml")

  (sp-local-pair '(markdown-mode gfm-mode) "`" "`"
                 :unless '(:add sp-point-before-word-p sp-point-before-same-p)))


;; allows editing of code blocks using major mode in other window
;; C-c C-c or C-c ' commits, C-c C-k aborts
(use-package edit-indirect
  :ensure t)


(provide 'lang-markdown)
;;; lang-markdown.el ends here
