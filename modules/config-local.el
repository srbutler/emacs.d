;;; config-local.el -- Summary
;;
;;; Commentary:
;;
;;; Code:

;; start the daemon if not already running
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'config-local)
;;; config-local.el ends here
