;;; config-appearance.el -- Summary
;;
;;; Commentary:
;;
;;; Code:


;; WINDOW SETTINGS ---------------------------
;; set larger frame size
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 100))

;; THEME SETTINGS -----------------------------

(defvar *current-theme-name* 'default)

;; only load themes when opened in a window system
(when window-system

  (use-package solarized-theme
    :disabled t
    :ensure t
    :init
    (progn
      ;; these variables need to be preset
      (setq solarized-distinct-doc-face t
            solarized-distinct-fringe-background nil
            solarized-emphasize-indicators nil
            solarized-high-contrast-mode-line t
            solarized-scale-org-headlines t
            solarized-use-variable-pitch nil
            solarized-use-less-bold t
            solarized-use-more-italic nil)

      ;; make the mode-line underlining disappear
      (setq x-underline-at-descent-line t)

      ;; finally load the theme
      (load-theme 'solarized-dark t))

    :config
    ;; just a variable for calling later face changes
    (setq *current-theme-name* 'solarized-dark)

    ;; general font locking
    (set-face-foreground 'font-lock-preprocessor-face "#cb4b16")
    (set-face-foreground 'font-lock-constant-face "#6c71c4")
    (set-face-attribute 'font-lock-constant-face nil :bold nil)
    (set-face-attribute 'font-lock-builtin-face nil :bold t)

    ;; fix info fringe for flycheck
    (with-eval-after-load 'flycheck
      (set-face-foreground 'flycheck-fringe-info "#268bd2")))

  (use-package zenburn-theme
    :disabled t
    :ensure t
    :init (load-theme 'zenburn t)
    :config
    (setq *current-theme-name* 'zenburn))

  (use-package material-theme
    ;; :disabled t
    :ensure t
    :init (load-theme 'material t)
    :config (setq *current-theme-name* 'material))

  (use-package leuven-theme
    :disabled t
    :ensure t
    :init (load-theme 'leuven t)
    :config (setq *current-theme-name* 'leuven))

  (use-package darkokai-theme
    :disabled t
    :ensure t
    :init (load-theme 'darkokai t)
    :config (setq *current-theme-name* 'darkokai)))


;; make the mode-line nice and simple
;; needs to be loaded after the theme
(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)
  :config (sml/setup))


;; FONT SETTINGS ------------------------------

;; check if a font exists
(defun font-existsp (font)
  "Check to see if the named FONT is available."
  (if (null (x-list-fonts font))
      nil t))

;; set a default here, override below if needed
(setq-default line-spacing 2)

;; set default font--first one found is selected
(cond
 ((eq window-system nil) nil)
 ((font-existsp "IosevkaX")
  (set-face-attribute 'default nil :height 151 :font "IosevkaX" :weight 'light)
  (setq-default line-spacing 0.06))
 ((font-existsp "Iosevka")
  (set-face-attribute 'default nil :height 151 :font "Iosevka" :weight 'light)
  (setq-default line-spacing 0.06))
 ((font-existsp "PragmataPro")
  (set-face-attribute 'default nil :height 151 :font "PragmataPro")
  (setq-default line-spacing 0.06))
 ((font-existsp "InconsolataGo")
  (set-face-attribute 'default nil :height 161 :font "InconsolataGo"))
 ((font-existsp "Hasklig")
  (set-face-attribute 'default nil :height 141 :font "Hasklig"))
 ((font-existsp "Fira Code")
  (set-face-attribute 'default nil :height 141 :font "Fira Code"))
 ((font-existsp "Source Code Pro")
  (set-face-attribute 'default nil :height 141 :font "Source Code Pro"))
 ((font-existsp "Monaco")
  (set-face-attribute 'default nil :height 131 :font "Monaco"))
 ((font-existsp "Menlo")
  (set-face-attribute 'default nil :height 141 :font "Menlo"))
 ((font-existsp "Consolas")
  (set-face-attribute 'default nil :height 151 :font "Consolas")))


(provide 'config-appearance)
;;; config-appearance.el ends here
