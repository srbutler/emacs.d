;;; config-osx.el --- Summary:
;;
;;; Commentary:
;;  Stuff only to be run on OSX
;;
;;; Code:


;; get the PATH variable working correctly
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))


;; set up dash integration
(use-package dash-at-point
  :ensure t
  :defer t
  :config
  (dolist
      (pair
       '('(python-mode . "python3") '(sh-mode . "bash") '(emacs-lisp-mode . "elisp")
         '(LaTeX-mode . "latex") '(js2-mode . "javascript") '(rjsx-mode . "javascript")
         '(tuareg-mode . "ocaml") '(ess-mode . "r")))
    (add-to-list 'dash-at-point-mode-alist pair)))


;; only run the following in the railwaycat version of emacs
(when (eq window-system 'mac)
  ;; turn on all ligatures
  (mac-auto-operator-composition-mode))


;; Enable emoji, and stop the UI from freezing when trying to display them.
(if (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))


(provide 'config-osx)
;;; config-osx.el ends here
