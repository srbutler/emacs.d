;;; init.el -- Summary
;;
;;; Commentary:
;;
;;; Code:

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

;; sometimes need different config files on different machines
(defun load-if-exists (filename dir)
  "Load FILENAME in DIR if it exists."
  (let ((target-file (expand-file-name filename dir)))
    (if (file-exists-p target-file)
        (load target-file)
      (message
       (format
        "File does not exist, skipping: %s"
        target-file)))))

;; setup proxies etc. if needed
(load-if-exists "before-init.el" *dotfiles-dir*)

;; Load package managment directories
(require 'package)
(setq package-archives
      '(("org"          . "https://orgmode.org/elpa/")
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; set up use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-enable-imenu-support t)
(require 'use-package)
(setq use-package-compute-statistics t)

;; set up cask
(use-package cask
  :ensure t
  :config (cask-initialize))

;; for the Cask file
(use-package cask-mode
  :ensure t)

;; add pallet to manage packages
(use-package pallet
  :ensure t
  :config (pallet-mode t))

(require 'config-general)
(require 'config-ivy)

;; this is needed to correctly load window-dependent code when starting as a
;; daemon
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (when (eq (length (frame-list)) 2)
                  (progn
                    (with-selected-frame frame
                      (require 'config-appearance))
                    ))))
  (require 'config-appearance))

;; anything needed outside of VC goes here
(load-if-exists "secrets.el" *dotfiles-dir*)

;; load language-specific config files
(require 'lang-cc)
;; (require 'lang-clojure)
(require 'lang-elisp)
;; (require 'lang-ess)
;; (require 'lang-go)
;; (require 'lang-haskell)
;; (require 'lang-java)
(require 'lang-js)
(require 'lang-latex)
;; (require 'lang-lisp)
(require 'lang-markdown)
;; (require 'lang-ocaml)
(require 'lang-org)
(require 'lang-python)
;; (require 'lang-rust)
;; (require 'lang-scala)
(require 'lang-web)

;; anything needed locally (work, etc.) not in before-init.el
(load-if-exists "config-local.el" *modules-dir*)


;;; init.el ends here
