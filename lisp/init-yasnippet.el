;;; init-yasnippet.el -- Configures `yasnippet-mode' preferences.
;;; Commentary:
;;; Code:

;; Yasnippet
(add-to-list 'load-path
              "~/.emacs.d/snippets")
(require 'yasnippet)
(yas-global-mode 1)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
