;;; init-restclient.el -- Configures `restclient-mode' preferences.
;;; Commentary:
;;; Code:

;; set restclient-mode to files with .restclient extension
(add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))

(provide 'init-restclient)
;;; init-restclient.el ends here
