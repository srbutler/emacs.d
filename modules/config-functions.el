;;; config-functions.el -- Summary
;;
;;; Commentary:
;;
;;; Code:


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


(defun byte-compile-modules ()
  "Byte-compile everything in the modules dir."
  (interactive)
  (byte-recompile-directory *modules-dir* 0))

(provide 'config-fuctions)
;;; config-functions.el ends here
