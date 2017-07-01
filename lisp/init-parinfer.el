;;; init-parinfer.el -- Configures `parinfer-mode' preferences.
;;; Commentary:
;;; Code:

(add-hook 'clojure-mode-hook #'parinfer-mode)
(add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
(add-hook 'common-lisp-mode-hook #'parinfer-mode)
(add-hook 'scheme-mode-hook #'parinfer-mode)
(add-hook 'lisp-mode-hook #'parinfer-mode)

(provide 'init-parinfer)
;;; init-parinfer.el ends here
