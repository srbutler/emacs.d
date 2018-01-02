;;; config-functions.el -- Summary
;;
;;; Commentary:
;;
;;; Code:

;; open my work org file
(defun open-kasisto-org ()
  "Open the Kasisto org file."
  (interactive)
  (find-file "~/Dropbox/Kasisto/kasisto.org"))

;; open kasr1 in a dired buffer (in office/VPN)
(defun connect-kasr1-inside ()
  (interactive)
  (dired "/kasr1.kitsys.net:/home/srbutler/"))

;; open kasr1 from out of office
(defun connect-kasr1-outside ()
  (interactive)
  (dired "/office.kitsys.net:/home/srbutler/"))

;; add highlighting for general annotations
;; taken from emacs-prelude
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for
programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(provide 'config-fuctions)
;;; config-functions.el ends here
