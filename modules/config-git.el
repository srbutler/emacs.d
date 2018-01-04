;;; config-git.el --- Summary:
;;
;;; Commentary:
;;
;;; Code:


;; set up magit for git
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))


;; have git indications in gutter
(use-package git-gutter
  :ensure t
  :defer t
  :init (global-git-gutter-mode t)
  :diminish git-gutter-mode
  :config
  
  ;; change the indicator colors to something nicer
  (when (or (eq current-theme-name "solarized-dark")
            (eq current-theme-name "solarized-light"))
    (progn
      (set-face-foreground 'git-gutter:added "#859900")
      (set-face-foreground 'git-gutter:deleted "#dc322f")
      (set-face-foreground 'git-gutter:modified "#b58900"))))


;; navigate through git commit history
(use-package git-timemachine
  :ensure t
  :defer t)


;; major mode for .gitconfig files
(use-package gitattributes-mode
  :ensure t
  :defer t)


;; major mode for .gitconfig files
(use-package gitconfig-mode
  :ensure t
  :defer t)


;; major mode for .gitignore files
(use-package gitignore-mode
  :ensure t
  :defer t)


;; add git-flow menu to magit
(use-package magit-gitflow
  :disabled t
  :ensure t
  :defer t
  :hook magit-mode)

;;; config-git.el ends here
