;;; init.el -- Summary
;;
;;; Commentary:
;;
;;; Code:

;; get all the directory names set up
(defvar *dotfiles-dir* (file-name-directory load-file-name)
  "The emacs.d root directory.")
(defvar *modules-dir* (expand-file-name "modules" *dotfiles-dir*)
  "A directory for configuration files.")
(defvar *vendor-dir* (expand-file-name "vendor" *dotfiles-dir*)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar *savefile-dir* (expand-file-name "savefile" *dotfiles-dir*)
  "This folder stores all the automatically generated save/history-files.")

;; add the needed directories to the load-path
(add-to-list 'load-path *modules-dir*)
(add-to-list 'load-path *vendor-dir*)

;; need this function really early so let's handle it here
(defun load-if-exists (filename dir)
  "Load FILENAME in DIR if it exists."
  (let ((target-file (expand-file-name filename dir)))
    (if (file-exists-p target-file)
        (load target-file)
      (message
       (format
        "File does not exist, skipping: %s"
        target-file)))))

;; Load package managment directories
(require 'package)
(setq package-archives
      '(("org"          . "http://orgmode.org/elpa/")
        ("gnu"          . "http://elpa.gnu.org/packages/")
        ("melpa"        . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(when (string-equal system-type "darwin")
  (progn
    ;; set up cask
    (use-package cask
      :load-path "/usr/local/share/emacs/site-lisp/cask/"
      :config (cask-initialize))

    ;; add pallet to manage packages
    (use-package pallet
      :ensure t
      :config (pallet-mode t))

    ;; load use-package extensions
    (use-package use-package-ensure-system-package
      :ensure t)))

;; Always load newest byte code
(setq load-prefer-newer +1)

;; create the savefile dir if it doesn't exist
(unless (file-exists-p *savefile-dir*)
  (make-directory *savefile-dir*))

;; set the custom file
(setq-default custom-file (expand-file-name "custom.el" *savefile-dir*))
(when (file-exists-p custom-file)
  (load custom-file))

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

;; garbage collect when Emacs loses focus
(add-hook 'focus-out-hook 'garbage-collect)

;; make adding new module files easy
(defun load-file-list (format-string files)
  "Load a list of FILES in the modules dir using FORMAT-STRING."
  (dolist (f files)
    (load (expand-file-name (format format-string f) *modules-dir*))))

;; load the settings files
(load-file-list "config-%s.el"
                '(
                  "ui"
                  "ivy"
                  ;; "helm"
                  "appearance"
                  "functions"
                  "git"
                  "programming"
                  "lsp"
                  ))

;; load OS-specific stuff
(cond
 ((string-equal system-type "darwin")
  (load-if-exists "config-osx.el" *modules-dir*))
 ((string-equal system-type "gnu/linux")
  (load-if-exists "config-linux.el" *modules-dir*))
 ((string-equal system-type "windows-nt")
  (load-if-exists "config-windows.el" *modules-dir*)))

;; anything needed outside of VC goes here
(load-if-exists "secrets.el" *dotfiles-dir*)

;; (load-if-exists "exordium-bbextensions-tap/after-init.el" *vendor-dir*)

;; load the language modules
(load-file-list "lang-%s.el"
                '(
                  ;; "cc"
                  "cc-lsp"
                  "clojure"
                  "ess"
                  ;; "go"
                  "go-lsp"
                  "haskell"
                  "java-lsp"
                  ;; "js"
                  "js-lsp"
                  "latex"
                  "lisp"
                  "markdown"
                  ;; "ocaml"
                  "ocaml-lsp"
                  "org"
                  ;; "python"
                  "python-lsp"
                  ;; "rust"
                  "rust-lsp"
                  "scala"
                  ;; "web"
                  "web-lsp"
                  ))

;;; init.el ends here
