;;; init.el --- Start Emacs :)
;;; Commentary:
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-packages)
(require 'init-defaults)
(require 'init-custom-defuns)

;; package-specific
(require 'init-package-helm)
(require 'init-package-magit)
(require 'init-package-git-gutter)
(require 'init-package-multiple-cursors)
(require 'init-package-helm-ag)
(require 'init-package-ag)
(require 'init-package-swiper)
(require 'init-package-undo-tree)

(require 'init-project-utils)
(require 'init-view-mode)
(require 'init-ruby)
(require 'init-ssh)
(require 'init-elixir)
(require 'init-clojure)
(require 'init-smartparens)
(require 'init-yasnippet)
(require 'init-dired)
(require 'init-javascript)
(require 'init-eshell)
(require 'init-git-pr)
(require 'init-sql)
(require 'init-docker)
(require 'init-restclient)
(require 'init-parinfer)

(require 'init-appearance)
(require 'init-keybindings)
(require 'init-org)

(require 'init-package-diminish) ;; Should stay at last position

(provide 'init)
;;; init.el ends here
