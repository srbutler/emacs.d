;; Set up Bloomberg proxies
(if (eq system-type 'darwin)
    ;; Assume the local nodeproxy is configured as in
    ;; https://bbgithub.dev.bloomberg.com/bbvpn/docs/blob/master/docs/nodeproxy.md
    (setq url-proxy-services
      '(("http" . "127.0.0.1:8888")
        ("https" . "127.0.0.1:8888")))
  (setq url-proxy-services
	'(("no_proxy" . "^\\(localhost\\|10.*\\)")
          ("http" . "devproxy.bloomberg.com:82")
          ("https" . "devproxy.bloomberg.com:82"))))
