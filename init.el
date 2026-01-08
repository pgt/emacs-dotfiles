;;; init.el --- Start Emacs :)
;;; Commentary:
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-packages)
(require 'init-defaults)
(require 'init-custom-defuns)
(require 'init-splash-screen)
(require 'init-navigation)
(require 'init-tree-sitter)

;; package-specific
(require 'init-package-magit)
(require 'init-package-git-gutter)
(require 'init-package-multiple-cursors)
;; (require 'init-package-helm-ag)
;; (require 'init-package-ag)
;; (require 'init-package-swiper)
(require 'init-package-undo-tree)

;; ;; (require 'init-project-utils)
(require 'init-view-mode)
(require 'init-ruby)
(require 'init-ssh)
(require 'init-elixir)
(require 'init-clojure)
(require 'init-rust)
(require 'init-yasnippet)
(require 'init-dired)
(require 'init-javascript)
(require 'init-typescript)
(require 'init-eshell)
(require 'init-sql)
(require 'init-docker)
(require 'init-terraform)
(require 'init-copilot)
(require 'init-restclient)
(require 'init-parinfer)
(require 'init-web-mode)

(require 'init-appearance)
(require 'init-keybindings)
(require 'init-highlight)
(require 'init-org)
(require 'init-lsp)

;; ;; ; (require 'init-package-diminish) ;; Should stay at last position

(setq debug-on-error t)
(setq debug-on-quit t)
(toggle-debug-on-quit)

;; (provide 'init)
;; ;;; init.el ends here
