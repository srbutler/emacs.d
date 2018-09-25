;;; config-js.el -- Summary
;;
;;; Commentary:
;;
;;; Code:


;; from https://github.com/seagle0128/.emacs.d
;; Improved JavaScript editing mode
(use-package js2-mode
  :ensure t
  :defines flycheck-javascript-eslint-executable
  :ensure-system-package (eslint_d . "npm install -g eslint_d")
  :mode (("\\.js\\'"  . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :bind (:map js2-mode-map ("M-." . nil))  ;; don't conflict with xref
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode))
  :config
  (setq js-basic-offset 2
        ;; mode-name "JS2"
        js-basic-indent 2
        js2-basic-offset 2)

  (with-eval-after-load 'flycheck
    (if (or (executable-find "eslint_d")
            (executable-find "eslint")
            (executable-find "jshint"))
        (setq js2-mode-show-strict-warnings nil))
    (if (executable-find "eslint_d")
        ;; https://github.com/mantoni/eslint_d.js
        ;; npm -i -g eslint_d
        (setq flycheck-javascript-eslint-executable "eslint_d"))))


;; for typescript
(use-package typescript-mode
  :ensure t
  :mode ("\\.tsx?$" . typescript-mode))


;; from https://github.com/seagle0128/.emacs.d
(use-package js2-refactor
  :ensure t
  :hook (js2-mode . js2-refactor-mode)
  :diminish (js2-refactor-mode . "js2r")
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m")
  (with-eval-after-load 'js2-mode
    (bind-key "C-k" 'js2r-kill js2-mode-map)))


;; from https://github.com/seagle0128/.emacs.d
(use-package lsp-javascript-typescript
  :ensure t
  :ensure-system-package
  (javascript-typescript-langserver . "npm i -g javascript-typescript-langserver")
  :commands lsp-javascript-typescript-enable
  :hook ((typescript-mode js2-mode) . lsp-javascript-typescript-enable))


(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))


;; formatting/beatufication for HTML/CSS/JS
(use-package web-beautify
  :ensure t
  :ensure-system-package (js-beautify . "sudo npm -g install js-beautify")
  :init
  (with-eval-after-load 'js-mode
    (bind-key "C-c C-f" #'web-beautify-js js-mode-map))
  (with-eval-after-load 'js2-mode
    (bind-key "C-c C-f" #'web-beautify-js js2-mode-map))
  (with-eval-after-load 'json-mode
    (bind-key "C-c C-f" #'web-beautify-js json-mode-map))
  (with-eval-after-load 'web-mode
    (bind-key "C-c C-f" #'web-beautify-html web-mode-map))
  (with-eval-after-load 'sgml-mode
    (bind-key "C-c C-f" #'web-beautify-html html-mode-map))
  (with-eval-after-load 'css-mode
    (bind-key "C-c C-f" #'web-beautify-css css-mode-map))
  :config
  ;; Set indent size to 2
  (setq web-beautify-args '("-s" "2" "-f" "-")))


;; REPL/dev environment
(use-package indium
  :ensure t
  :after js2-mode
  :bind (:map js2-mode-map
              ("C-c C-l" . indium-eval-buffer))
  :hook ((js2-mode . indium-interaction-mode)))


(provide 'lang-js.el)
;;; lang-js.el ends here
