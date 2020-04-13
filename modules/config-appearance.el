;;; config-appearance.el -- Summary
;;
;;; Commentary:
;;
;;; Code:


;;;; WINDOW SETTINGS

;; set larger frame size
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 100))

;;;; THEMES
;; only load themes when opened in a window system

(use-package material-theme
  :disabled t
  :ensure t
  :init (load-theme 'material t))

(use-package solarized-theme
  :disabled t
  :ensure t
  :init
  (progn
    ;; these variables need to be preset
    (setq solarized-distinct-doc-face t
          solarized-distinct-fringe-background nil
          solarized-emphasize-indicators nil
          solarized-high-contrast-mode-line nil
          solarized-scale-org-headlines t
          solarized-use-variable-pitch nil
          solarized-use-less-bold t
          solarized-use-more-italic nil)

    ;; make the mode-line underlining disappear
    (setq x-underline-at-descent-line t)

    ;; finally load the theme
    (load-theme 'solarized-dark t))

  :config
  ;; custom font-lock setup
  (set-face-foreground 'font-lock-preprocessor-face "#cb4b16")
  (set-face-foreground 'font-lock-constant-face "#6c71c4")
  (set-face-foreground 'font-lock-variable-name-face "#b58900")
  (set-face-foreground 'font-lock-doc-face "#d33682")
  (set-face-attribute 'font-lock-constant-face nil :bold nil)
  (set-face-attribute 'font-lock-builtin-face nil :bold t)

  ;; get rid of nasty underlining
  (with-eval-after-load 'org-mode
    (set-face-attribute 'org-block-begin-line nil :underline nil)
    (set-face-attribute 'org-block-end-line nil :underline nil))

  ;; make rainbow delimiters less monotonous
  (with-eval-after-load 'rainbow-delimiters
    (set-face-foreground 'rainbow-delimiters-depth-1-face "#cb4b16")
    (set-face-foreground 'rainbow-delimiters-depth-6-face "#d33682"))

  ;; fix info fringe for flycheck
  (with-eval-after-load 'flycheck
    (set-face-foreground 'flycheck-fringe-info "#268bd2"))

  ;; make the indicators more readable
  (with-eval-after-load 'git-gutter
    (progn
      (set-face-foreground 'git-gutter:added "#859900")
      (set-face-foreground 'git-gutter:deleted "#dc322f")
      (set-face-foreground 'git-gutter:modified "#b58900"))))

(use-package zenburn-theme
  :disabled t
  :ensure t
  :init (load-theme 'zenburn t))

(use-package nord-theme
  ;; :disabled t
  :ensure t
  :init (load-theme 'nord t)
  :config
  (with-eval-after-load 'persp-mode
    (set-face-foreground 'persp-selected-face "#5E81AC")))

(use-package leuven-theme
  :disabled t
  :ensure t
  :init (load-theme 'leuven t))

(use-package darkokai-theme
  :disabled t
  :ensure t
  :init (load-theme 'darkokai t))


;;;; MODE LINE
;; make the mode-line nice and simple
;; needs to be loaded after the theme
(use-package smart-mode-line
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
 ((font-existsp "IosevkaX")
  (set-face-attribute 'default nil :height 141 :font "IosevkaX" :weight 'light)
  (setq-default line-spacing 0.06))
 ((font-existsp "Iosevka")
  (set-face-attribute 'default nil :height 141 :font "Iosevka" :weight 'light)
  (setq-default line-spacing 0.06))
 ((font-existsp "PragmataPro")
  (set-face-attribute 'default nil :height 151 :font "PragmataPro")
  (setq-default line-spacing 0.06))
 ((font-existsp "InconsolataGo")
  (set-face-attribute 'default nil :height 161 :font "InconsolataGo"))
 ((font-existsp "Inconsolata")
  (set-face-attribute 'default nil :height 161 :font "Inconsolata"))
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


;; Enable emoji, and stop the UI from freezing when trying to display them.
(if (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))


;; setup ligature in certain situations
(cond
 ;; only run the following in the railwaycat version of emacs
 ((fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode)))

(when window-system
  (let ((alist '((33 . ".\\(?:\\(?:==\\)\\|[!=]\\)")
                 (35 . ".\\(?:[(?[_{]\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|\\+\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (58 . ".\\(?:[:=]\\)")
                 (59 . ".\\(?:;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:[:=?]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:[=@~-]\\)"))))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))


(provide 'config-appearance)
;;; config-appearance.el ends here
