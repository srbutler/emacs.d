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

(defvar current-theme-name 'default)

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
    ;; just a variable for calling later face changes
    (setq current-theme-name 'solarized-dark)

    ;; general font locking
    (set-face-foreground 'font-lock-preprocessor-face "#cb4b16")
    (set-face-foreground 'font-lock-constant-face "#6c71c4")
    (set-face-attribute 'font-lock-constant-face nil :bold nil)
    (set-face-attribute 'font-lock-builtin-face nil :bold t))

  (use-package zenburn-theme
    :disabled t
    :ensure t
    :init (load-theme 'zenburn t)
    :config
    (setq current-theme-name 'zenburn)

    ;; hide the fringe
    (set-face-attribute 'fringe nil
                        :foreground (face-foreground 'default)
                        :background (face-background 'default)))

  (use-package material-theme
    ;; :disabled t
    :ensure t
    :init (load-theme 'material t)
    :config (setq current-theme-name 'material))

  (use-package leuven-theme
    :disabled t
    :ensure t
    :init (load-theme 'leuven t)
    :config (setq current-theme-name 'leuven))

  (use-package darkokai-theme
    :disabled t
    :ensure t
    :init (load-theme 'darkokai t)
    :config (setq current-theme-name 'darkokai)))


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

;; set default font--first one found is selected
(cond
 ((eq window-system nil) nil)
 ((font-existsp "IosevkaX")
  (set-face-attribute 'default nil :height 151 :font "IosevkaX")
  (setq-default line-spacing 0.06))
 ((font-existsp "Iosevka")
  (set-face-attribute 'default nil :height 151 :font "Iosevka")
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
  (set-face-attribute 'default nil :height 131 :font "Consolas")))

;; setup ligatures
(when window-system
  ;; comment out #46 for Clojure's CIDER mode if there are problems
  (let ((alist '((33 . ".\\(?:\\(?:==\\)\\|[!=]\\)")
                 (35 . ".\\(?:[(?[_{]\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 ;; (42 . ".\\(?:\\(?:\\*\\*\\)\\|[*/]\\)")    ;; disabled for org-mode sanity
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
                 (126 . ".\\(?:[=@~-]\\)")
                 )
               ))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))


(provide 'config-appearance)
;;; config-appearance.el ends here
