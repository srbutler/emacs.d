;;; config-ivy.el -- Summary
;;
;;; Commentary:
;;    Load this in `init.el' if to use it instead of helm.
;;; Code:

;; adds usage ordering to counsel-M-x
(use-package smex
  :ensure t
  :config (setq smex-save-file
                (expand-file-name ".smex-items" *savefile-dir*)))


;; testing out ivy/counsel as replacement for helm
(use-package counsel
  :ensure t
  :after smex
  :demand
  :bind  (("M-x" . counsel-M-x)
          ("M-y" . counsel-yank-pop)
          ("C-s" . counsel-grep-or-swiper)
          ("C-r" . counsel-grep-or-swiper)
          ("C-h a" . counsel-apropros)
          ("C-h b" . counsel-descbinds)
          ("C-h f" . counsel-describe-function)
          ("C-h F" . counsel-describe-face)
          ("C-h v" . counsel-describe-variable)
          ("C-c i" . counsel-imenu)
          ("C-x l" . counsel-locate)
          ("C-x C-f" . counsel-find-file)
          ("C-x C-r" . counsel-recentf)
          ("C-c C-u" . counsel-unicode-char)
          ("C-h C-l" . counsel-find-library)
          ("C-h SPC" . counsel-mark-ring))
  :config
  (setq swiper-action-recenter t
        counsel-find-file-at-point t
        counsel-find-file-ignore-regexp "\\.DS_Store\\|.git\\|\.*~undo-tree~\\|.*.elc")

  ;; rg > ag > grep, set command appropriately
  (cond
   ((executable-find "rg")
    (progn
      (bind-key "C-c k" 'counsel-rg)

      (setq counsel-grep-base-command
            "rg -i -M 120 --no-heading --line-number --color never '%s' %s")))
   ((executable-find "ag")
    (progn
      (bind-key "C-c k" 'counsel-ag)
      (setq counsel-grep-base-command
            "ag -i --noheading --nocolor --nofilename --numbers '%s' %s")))
   (t (bind-key "C-c k" 'counsel-git-grep)))

  (counsel-mode 1))

;; provides sorting for ivy
(use-package flx
  :ensure t)


(use-package ivy
  :ensure t
  :demand
  :after flx
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x C-b" . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map  ;; mimic helm reflexes
         ("C-l" . ivy-backward-delete-char)
         ("C-j" . ivy-alt-done)
         ("<return>" . ivy-alt-done))
  :custom
  (ivy-initial-inputs-alist nil)  ;; don't start ivy with ^
  (ivy-use-selectable-prompt t)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-height 10)
  (ivy-wrap t)
  ;; configure regexp engine
  (ivy-re-builders-alist
   '((t . ivy--regex-ignore-order)))
  :config (ivy-mode 1))


;; a better version of ivy-switch-buffer
(use-package ivy-rich
  :ensure t
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-path-style 'abbrev)
  :config (ivy-rich-mode 1))


;; hydra presents menus for ivy commands.
(use-package ivy-hydra
  :ensure t
  :after ivy)


;; use ivy for xref candidates
(use-package ivy-xref
  :ensure t
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))


;; project browsing
(use-package counsel-projectile
  :after (counsel projectile)
  :ensure t
  :bind (("C-c p p" . counsel-projectile-switch-project)
         ("C-c p f" . counsel-projectile-find-file)
         ("C-c p g" . counsel-projectile-find-file-dwim)
         ("C-c p d" . counsel-projectile-find-dir)
         ("C-c p b" . counsel-projectile-switch-to-buffer)
         ("C-c p s a" . counsel-projectile-ag)
         ("C-c p s g" . counsel-projectile-grep)
         ("C-c p s i" . counsel-projectile-git-grep)
         ("C-c p s r" . counsel-projectile-rg)
         ("C-c p SPC" . counsel-projectile)
         ("C-c p o" . counsel-projectile-org-capture))
  :config (counsel-projectile-mode)

  ;; taken from doom-emacs
  (add-to-list 'counsel-compile-root-functions #'projectile-project-root)

  (defun srb/compile-project ()
    "Execute a compile command from the current project's root."
    (interactive)
    (counsel-compile (projectile-project-root)))
  (bind-key "C-c C-c" 'srb/compile-project)
  (bind-key "C-c p c" 'srb/compile-project))


;; ivy interface for yasnippet
(use-package ivy-yasnippet
  :ensure t
  :after (ivy yasnippet)
  :bind ("C-c y" . ivy-yasnippet))


(provide 'config-ivy)
;;; config-ivy.el ends here
