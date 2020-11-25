;;; config-appearance.el -- Summary
;;
;;; Commentary:
;;
;;; Code:


;;;; WINDOW SETTINGS

;; set larger frame size
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 108))

;; set dark theme on frame (if applicable)
(defun srb/set-frame-dark-theme (&optional frame)
  "Force the created FRAME to have a dark variant GTK theme."
  (interactive)
  (unless frame
    (setq frame (selected-frame)))
  (with-selected-frame frame
    (let ((frame-name (if (framep frame)
                          (cdr (assq 'name (frame-parameters frame)))
                        (error "Function `srb/set-frame-dark-theme': Argument not a frame: `%s'" frame))))
      (call-process-shell-command (concat "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"dark\" -name \""
                                          frame-name
                                          "\"")))))

(if (string= window-system "x")
    (progn
      (srb/set-frame-dark-theme (selected-frame))
      (add-hook 'after-make-frame-functions 'srb/set-frame-dark-theme)))


;;;; THEMES
;; only load themes when opened in a window system

(use-package base16-theme
  :ensure t
  :config
  (set-face-foreground 'persp-selected-face "#81a2be")
  (setq base16-distinct-fringe-background t
        base16-highlight-mode-line t
        base16-theme-256-color-source 'base16-shell)
  (load-theme 'base16-tomorrow-night t))


;;;; MODE LINE
;; make the mode-line nice and simple
;; needs to be loaded after the theme
(use-package smart-mode-line
  :disabled t
  :ensure t
  :demand t
  :commands sml/apply-theme
  :init
  (setq sml/no-confirm-load-theme t)
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)
  :config
  (sml/setup)
  (sml/apply-theme "respectful"))


;; create a menu for minor-modes
(use-package minions
  :ensure t
  :config (minions-mode 1))


;; a fancy modeline
(use-package doom-modeline
  :ensure t
  :after minions
  :demand t
  :config
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-height 24
        doom-modeline-bar-width 3
        doom-modeline-project-detection 'projectile
        doom-modeline-icon (display-graphic-p)
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-minor-modes t
        doom-modeline-enable-word-count nil
        doom-modeline-persp-name t
        doom-modeline-display-default-persp-name nil
        doom-modeline-lsp t
        doom-modeline-env-version t)
  (doom-modeline-mode t))

;;;; FONTS

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
 ((font-existsp "Iosevka Curly")
  (set-face-attribute 'default nil :height 131 :font "Iosevka Curly" :weight 'regular)
  (setq-default line-spacing 0.05))
 ((font-existsp "Iosevka")
  (set-face-attribute 'default nil :height 131 :font "Iosevka" :weight 'regular)
  (setq-default line-spacing 0.05))
 ((font-existsp "PragmataPro")
  (set-face-attribute 'default nil :height 131 :font "PragmataPro")
  (setq-default line-spacing 0.05))
 ((font-existsp "Roboto Mono")
  (set-face-attribute 'default nil :height 131 :font "Roboto Mono"))
 ((font-existsp "InconsolataGo")
  (set-face-attribute 'default nil :height 161 :font "InconsolataGo"))
 ((font-existsp "Inconsolata")
  (set-face-attribute 'default nil :height 161 :font "Inconsolata"))
 ((font-existsp "Hasklig")
  (set-face-attribute 'default nil :height 141 :font "Hasklig"))
 ((font-existsp "Fira Code")
  (set-face-attribute 'default nil :height 131 :font "Fira Code"))
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
