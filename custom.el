(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("130319ab9b4f97439d1b8fd72345ab77b43301cf29dddc88edb01e2bc3aff1e7" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" "70403e220d6d7100bae7775b3334eddeb340ba9c37f4b39c189c2c29d458543b" "a2e7b508533d46b701ad3b055e7c708323fb110b6676a8be458a758dd8f24e27" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(fci-rule-color "#37474f")
 '(helm-gtags--label-option "pygments")
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(helm-gtags-prefix-key "C-c C-t")
 '(helm-gtags-suggested-key-mapping t)
 '(highlight-changes-colors (quote ("#ff8eff" "#ab7eff")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#424748" . 0)
     ("#63de5d" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#424748" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-sexp-background-color "#efebe9")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c")))
 '(package-selected-packages
   (quote
    (base16-theme zenburn-theme yaml-mode wrap-region which-key web-mode web-beautify use-package typescript-mode swift-mode sr-speedbar solarized-theme smartparens smart-mode-line slime skewer-mode rtags rainbow-mode rainbow-delimiters racer pyenv-mode pcre2el pandoc-mode pallet org-plus-contrib mmm-mode material-theme markdown-mode magit leuven-theme key-chord json-mode js2-refactor js-comint irony-eldoc intero idle-highlight-mode helm-projectile helm-gtags helm-descbinds helm-ag go-projectile git-gutter geiser flycheck-stack flycheck-rust flycheck-pos-tip flycheck-irony flycheck-hdevtools flycheck-haskell flycheck-gometalinter flycheck-clojure flycheck-cask flatland-theme exec-path-from-shell ess erlang emmet-mode elpy elixir-mode dash-at-point darkokai-theme cython-mode csv-mode crux company-tern company-racer company-irony-c-headers company-irony company-go company-ghc color-theme-sanityinc-tomorrow coffee-mode cmake-mode cmake-ide clj-refactor clang-format cdlatex cargo avy auctex anti-zenburn-theme)))
 '(pdf-view-midnight-colors (quote ("#232333" . "#c7c7c7")))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#242728" "#424748" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff"))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
