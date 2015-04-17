;;; init.el --- Start Emacs :)
;;; Commentary:
;;; Code:
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-packages)
(require 'init-defaults)
(require 'init-custom-defuns)

;; package-specific
(require 'init-helm)
(require 'init-magit)
(require 'init-project-utils)
(require 'init-ruby)
(require 'init-smartparens)
(require 'init-isearch)
(require 'init-yasnippet)
(require 'init-dired)

(require 'init-appearance)
(require 'init-window-management)
(require 'init-keybindings)

;;; init.el ends here
