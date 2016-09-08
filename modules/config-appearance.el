;;; config-appearance.el -- Summary
;;
;;; Commentary:
;;
;;; Code:


;; WINDOW SETTINGS ---------------------------
;; set larger frame size
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 100))

;; remove the redundant scroll-bars
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; If launching Emacs as in windowing system, show the menu. If
;; launching in a tty/terminal window, don't display the menu.
(if window-system
    (menu-bar-mode t)
  (menu-bar-mode -1))

;; disable toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; THEME SETTINGS -----------------------------

(defvar current-theme-name 'default)

(use-package solarized-theme
  ;; :disabled nil
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

;; (use-package material-theme
;;   ;;:disabled nil
;;   :ensure t
;;   :init (load-theme 'material-light t)
;;   :config (setq current-theme-name 'material-theme))

;; (use-package leuven-theme
;;   :ensure t
;;   :init (load-theme 'leuven t)
;;   :config (setq current-theme-name 'leuven))


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
 ((font-existsp "Hasklig")
  (set-face-attribute 'default nil :height 141 :font "Hasklig"))
 ((font-existsp "PragmataPro")
  (set-face-attribute 'default nil :height 151 :font "PragmataPro"))
 ((font-existsp "Fira Code")
  (set-face-attribute 'default nil :height 141 :font "Fira Code"))
 ((font-existsp "Monaco")
  (set-face-attribute 'default nil :height 131 :font "Monaco"))
 ((font-existsp "Menlo")
  (set-face-attribute 'default nil :height 131 :font "Menlo"))
 ((font-existsp "Source Code Pro")
  (set-face-attribute 'default nil :height 131 :font "Source Code Pro"))
 ((font-existsp "Consolas")
  (set-face-attribute 'default nil :height 131 :font "Consolas"))
 )

;; Line-spacing tweak: Set this to a different number depending on
;; taste and the font selected. The value can be a integer or decimal
;; number. if integer: it means pixels, added below each line. if
;; float (e.g 0.02): a scaling factor relative to current window's
;; default line height. if nil: add no extra spacing.

;; (setq-default line-spacing 0.06) ;; tuned for Pragmata Pro
(setq-default line-spacing 2)


;; Enable emoji, and stop the UI from freezing when trying to display them.
(if (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))


;; setup ligatures
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
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(provide 'config-appearance)
;;; config-appearance.el ends here
