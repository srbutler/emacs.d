;;; config-helm.el -- Summary
;;
;;; Commentary:
;;    Load this in `init.el' if to use it instead of ivy.
;;; Code:

;; setup helm for as many things as possible
(use-package helm
  :ensure helm
  :diminish helm-mode
  :init
  (require 'helm-config)

  (setq helm-split-window-in-side-p               t
        helm-M-x-fuzzy-match                      t
        helm-move-to-line-cycle-in-source         t
        helm-ff-search-library-in-sexp            t
        helm-ff-file-name-history-use-recentf     t
        helm-ff-skip-boring-files                 t
        helm-autoresize-max-height                40
        helm-autoresize-min-height                40
        helm-use-frame-when-more-than-two-windows nil

        ;; fuzzy matching
        helm-buffers-fuzzy-matching               t
        helm-completion-in-region-fuzzy-match     t
        helm-M-x-fuzzy-match                      t
        helm-apropos-fuzzy-match                  t
        helm-imenu-fuzzy-match                    t
        helm-lisp-fuzzy-completion                t
        helm-locate-fuzzy-match                   t
        helm-mode-fuzzy-match                     t
        helm-recentf-fuzzy-match                  t
        helm-semantic-fuzzy-match                 t
        )

  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))

  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c")) ;; old key is risky

  :bind (("C-x b"   . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)

         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)

         ("C-c i" . helm-imenu)
         ("C-c h i" . helm-imenu-in-all-buffers)
         ("C-c h o" . helm-occur)
         ("C-c h x" . helm-register)
         ("C-c h w" . helm-wikipedia-suggest)

         ("C-h b" . helm-descbinds)
         ("C-h f" . helm-apropos)
         ("C-h r" . helm-info-emacs)
         ("C-h C-l" . helm-locate-library)
         ("C-h SPC" . helm-all-mark-rings))

  :config
  (helm-mode)
  (helm-autoresize-mode nil)
  (helm-descbinds-mode))


(use-package helm-projectile
  :ensure t
  :after helm projectile
  :init
  :custom
  (helm-projectile-fuzzy-match t)
  (projectile-completion-system 'helm)
  :config (helm-projectile-on))


;; use ag for searching in helm
(use-package helm-ag
  :disabled t
  :ensure t
  :after helm
  :commands helm-do-ag
  :bind (("C-c h g" . helm-do-ag)))


(use-package helm-rg
  :ensure t
  :after helm
  :bind (("C-c k" . helm-rg)))


(use-package swiper-helm
  :ensure t
  :after helm
  :bind (("C-s" . swiper-helm)
         ("C-r" . swiper-helm)))


;; use GNU global
(use-package helm-gtags
  :ensure t
  :after helm
  :diminish (helm-gtags-mode . "gtags")
  :bind (:map helm-gtags-mode-map
              ("M-." . helm-gtags-dwim)
              ("C-c C-t c" . helm-gtags-create-tags)
              ("C-c C-t u" . helm-gtags-update-tags)
              ("C-c C-t s" . helm-gtags-select))
  :custom
  (helm-gtags-prefix-key "C-c C-t")
  (helm-gtags-suggested-key-mapping t)
  (helm-gtags-path-style 'relative)
  (helm-gtags-ignore-case t)
  (helm-gtags-auto-update t)
  (helm-gtags--label-option "pygments")
  :init (add-hook 'prog-mode-hook 'helm-gtags-mode))


(provide 'config-helm)
;;; config-helm.el ends here
