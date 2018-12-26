;;; init.el -- Summary
;;
;;; Commentary:
;;
;;; Code:

;; Load package managment directories
(require 'package)
(setq package-archives
      '(("org"          . "https://orgmode.org/elpa/")
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(when (file-exists-p "/usr/local/share/emacs/site-lisp/cask/")
  (progn
    ;; set up cask
    (use-package cask
      :load-path "/usr/local/share/emacs/site-lisp/cask/"
      :config (cask-initialize))

    ;; add pallet to manage packages
    (use-package pallet
      :ensure t
      :config (pallet-mode t))))

;; Always load newest byte code
(setq load-prefer-newer +1)

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

;; create the savefile dir if it doesn't exist
(unless (file-exists-p *savefile-dir*)
  (make-directory *savefile-dir*))

;; set the custom file
(setq-default custom-file (expand-file-name "custom.el" *savefile-dir*))
(when (file-exists-p custom-file)
  (load custom-file))

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

;; make adding new module files easy
(defun load-file-list (format-string files)
  "Load a list of FILES in the modules dir using FORMAT-STRING."
  (dolist (f files)
    (load-if-exists (format format-string f) *modules-dir*)))

(require 'config-general)
(require 'config-ivy)
(require 'config-appearance)

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

;; load language-specific config files
(require 'lang-cc)
(require 'lang-clojure)
(require 'lang-elisp)
(require 'lang-ess)
(require 'lang-go)
(require 'lang-haskell)
(require 'lang-java)
(require 'lang-js)
(require 'lang-latex)
(require 'lang-lisp)
(require 'lang-markdown)
(require 'lang-ocaml)
(require 'lang-org)
(require 'lang-python)
(require 'lang-rust)
(require 'lang-scala)
(require 'lang-web)

;;; init.el ends here
