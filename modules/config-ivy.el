;;; config-ivy.el -- Summary
;;
;;; Commentary:
;;    Load this in `init.el' if to use it instead of helm.
;;; Code:

(use-package smex
  :ensure t
  :config (setq smex-save-file
                (expand-file-name ".smex-items" *savefile-dir*)))

;; testing out ivy/counsel as replacement for helm
(use-package counsel
  :ensure t
  :after smex
  :demand
  :diminish
  :bind  (("M-x" . counsel-M-x)
          ("M-y" . counsel-yank-pop)
          ("C-x C-f" . counsel-find-file)
          ("C-x C-r" . counsel-recentf)

          ("C-s" . counsel-grep-or-swiper)
          ("C-x l" . counsel-locate)
          ("C-c k" . counsel-rg)
          ("C-c i" . counsel-imenu)

          ("C-h b" . counsel-descbinds)
          ("C-h f" . counsel-apropos)
          ("C-h C-l" . counsel-find-library)
          ("C-h SPC" . counsel-mark-ring))
  :custom
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp
   "\\.DS_Store\\|.git\\|\.*~undo-tree~\\|GPATH\\|GRTAGS\\|GTAGS\\|.*.elc")
  (counsel-grep-base-command
   "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (counsel-)
  :config (counsel-mode 1))


;; provides sorting for ivy
(use-package flx
  :ensure t)


(use-package ivy
  :ensure t
  :demand
  :after flx
  :diminish
  :bind (("C-x C-b" . ivy-switch-buffer)
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
   '((t   . ivy--regex-ignore-order)))
  :config (ivy-mode 1))

;; hydra presents menus for ivy commands.
(use-package ivy-hydra
  :ensure t
  :after ivy)


(use-package counsel-projectile
  :after (counsel projectile)
  :ensure t
  :config (counsel-projectile-mode))


(use-package counsel-gtags
  :ensure t
  :after counsel
  :diminish (counsel-gtags-mode . "gtags")
  :bind (:map counsel-gtags-mode-map
         ("M-." . counsel-gtags-dwim)
         ("C-c C-t c" . counsel-gtags-create-tags)
         ("C-c C-t u" . counsel-gtags-update-tags)
         ("C-c C-t d" . counsel-gtags-find-definition)
         ("C-c C-t r" . counsel-gtags-find-reference)
         ("C-c C-t s" . counsel-gtags-find-symbol)
         ("C-c C-t f" . counsel-gtags-go-forward)
         ("C-c C-t b" . counsel-gtags-go-backward)))

(provide 'config-ivy)
;;; config-ivy.el ends here
