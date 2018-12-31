;;; lang-latex.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:

;; Basic settings
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  ;; set up xelatex
  (add-to-list 'TeX-command-list
               '("xelatexmk" "latexmk -synctex=1 -shell-escape -xelatex %s"
                 TeX-run-TeX nil t :help "Process file with xelatexmk"))

  (add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)

  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-command-default "xelatexmk"
        TeX-open-quote "“"
        TeX-close-quote "”"
        default-justification 'left
        reftex-use-fonts t
        LaTeX-fill-break-at-separators nil)

  ;; call whatever the terminal default is to open files
  (when (eq system-type 'darwin)
    (setq TeX-view-program-selection
          '((output-dvi "Default Open")
            (output-pdf "Default Open")
            (output-html "Default Open")))
    (setq TeX-view-program-list
          '(("Default Open" "open %o")))))


(use-package auctex-latexmk
  :ensure t
  :init (add-hook 'LaTeX-mode-hook 'auctex-latexmk-setup))


(use-package company-auctex
  :ensure t
  :init (add-hook 'LaTeX-mode-hook 'company-auctex-init))


;; integrate reftex
(use-package reftex
  :defer t
  :diminish (reftex-mode . "ref")
  :init (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  :config
  (setq reftex-plug-into-AUCTeX t
        reftex-enable-partial-scans t
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t
        reftex-cite-prompt-optional-args nil
        reftex-cite-cleanup-optional-args t))


(use-package bibtex
  :mode ("\\.bib" . bibtex-mode)
  :init
  (setq bibtex-align-at-equal-sign t)
  (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120))))


(provide 'lang-latex)
;;; lang-latex.el ends here
