;;; lang-org.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:


(use-package org
  :ensure org-plus-contrib
  :bind (("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda)
         ("C-c o b" . org-iswitchb)
         :map org-mode-map
         ("S-<right>" . org-shiftright)
         ("S-<left>" . org-shiftleft)
         ("S-<up>" . org-shiftup)
         ("S-<down>" . org-shiftdown))
  :init
  (setq org-export-backends '(ascii beamer html latex md))
  :config
  (setq org-directory "~/Dropbox/Org"
        org-mobile-directory "~/Dropbox/Org"
        org-log-done 'time
        ;; org-replace-disputed-keys t
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  ;; add a custom sequence of TODO states
  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s)" "DELAYED(y)" "|"
                    "DONE(d)" "CANCELLED(c)")))
  (setq org-todo-keyword-faces '(("STARTED" . org-priority)))

  ;; make yasnippet work properly with org-mode
  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

  (add-hook 'org-mode-hook
            (lambda ()
              (make-variable-buffer-local 'yas/trigger-key)
              (setq yas/trigger-key [tab])
              (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
              (define-key yas/keymap [tab] 'yas/next-field)))

  ;; ensure R blocks are called correctly
  (add-to-list 'org-src-lang-modes '("r" . ess-mode)))



(use-package org-ref
  :when (file-exists-p "~/Dropbox/Bib/references.bib")
  :ensure t
  :after org
  :init
  (setq reftex-default-bibliography '("~/Dropbox/Bib/references.bib"))
  (setq org-ref-bibliography-notes "~/Dropbox/Bib/notes/notes.org"
        org-ref-default-bibliography '("~/Dropbox/Bib/references.bib")
        org-ref-pdf-directory "~/Bib/papers/"))


;; Fancy bullet rendering.
(use-package org-bullets
  :ensure t
  :defer t
  :hook (org-mode . org-bullets-mode))


(provide 'lang-org)
;;; lang-org.el ends here
