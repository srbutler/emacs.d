;;; custom-functions.el -- Summary
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

(provide 'custom-fuctions)
;;; custom-functions.el ends here
