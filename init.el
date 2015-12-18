;;; init.el --- Start Emacs :)
;;; Commentary:
;;; Code:
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-packages)
(require 'init-defaults)
(require 'init-custom-defuns)

;; package-specific
(require 'init-package-helm)
(require 'init-package-magit)
(require 'init-package-git-gutter)
(require 'init-package-multiple-cursors)
(require 'init-package-neotree)
(require 'init-package-helm-ag)
(require 'init-package-ag)
(require 'init-package-swiper)

(require 'init-project-utils)
(require 'init-view-mode)
(require 'init-ruby)
(require 'init-smartparens)
(require 'init-yasnippet)
(require 'init-dired)
(require 'init-javascript)
(require 'init-eshell)

(require 'init-appearance)
(require 'init-keybindings)
(require 'init-org)

(require 'init-package-diminish) ;; Should stay at last position

(provide 'init)
;;; init.el ends here
