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
(require 'init-package-telephone-line)
(require 'init-package-git-gutter)
(require 'init-package-multiple-cursors)

(require 'init-project-utils)
(require 'init-ruby)
(require 'init-smartparens)
(require 'init-isearch)
(require 'init-yasnippet)
(require 'init-dired)
(require 'init-javascript)
(require 'init-eshell)

(require 'init-appearance)
(require 'init-keybindings)

(require 'init-package-diminish) ;; Should stay at last position


;;; init.el ends here
