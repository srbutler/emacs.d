;;; init.el -- Summary
;;
;;; Commentary:
;;
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; cask/pallet setup
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; go to debug on error
;; (setq debug-on-error t)

;; Always load newest byte code
(setq load-prefer-newer +1)

;; get all the directory names
(defvar dotfiles-dir (file-name-directory load-file-name)
  "The emacs.d root directory.")
(defvar modules-dir (expand-file-name "modules" dotfiles-dir)
  "A directory for configuration files.")
(defvar vendor-dir (expand-file-name "vendor" dotfiles-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar savefile-dir (expand-file-name "savefile" dotfiles-dir)
  "This folder stores all the automatically generated save/history-files.")

;; create the savefile dir if it doesn't exist
(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

;; add the needed directories to the load-path
(add-to-list 'load-path modules-dir)
(add-to-list 'load-path vendor-dir)

;; set the custom file
(setq-default custom-file (expand-file-name "custom.el" dotfiles-dir))

;; setup savefiles/backups in a way that's not annoying
(setq backup-directory-alist `(("." . "~/.emacs.d/savefile/"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Load package managment directories
(require 'package)
(setq package-archives
      '(
        ("org"          . "http://orgmode.org/elpa/")
        ("gnu"          . "http://elpa.gnu.org/packages/")
        ("melpa"        . "http://melpa.milkbox.net/packages/")
        ;; ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
        ;; ("marmalade"    . "http://marmalade-repo.org/packages/")
        ))


;; make adding new module files easy
(defun load-file-list (format-string files)
  "Load a list of FILES in the modules dir using FORMAT-STRING."
  (dolist (f files)
    (load (expand-file-name (format format-string f) modules-dir))))


;; load use-package extensions
(use-package use-package-ensure-system-package
  :ensure t)


;; load the settings files
(load-file-list "config-%s.el"
                '("appearance" "functions" "keybindings"
                  "programming" "ui"))

;; load the language modules
(load-file-list "lang-%s.el"
                '("c" "clojure" "ess" "go" "haskell" "latex" "lisp"
                  "markdown" "ocaml" "org" "python" "rust" "web-js"))

;;; init.el ends here


