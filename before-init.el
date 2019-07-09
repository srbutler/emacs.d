;;; before-init.el -- Summary
;;
;;; Commentary:
;;
;;; Code:

(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
        ("http"     . "proxyam-ny.bloomberg.com:81")
        ("https"    . "proxyam-ny.bloomberg.com:81")))

(defvar *pip-repo-url*
  "https://artprod.dev.bloomberg.com/artifactory/api/pypi/bloomberg-pypi/simple")


(provide 'before-init)
;;; before-init.el ends here
