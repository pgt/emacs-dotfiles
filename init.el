;;; init.el --- Start Emacs :)
;;; Commentary:
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-packages)
(require 'init-defaults)
(require 'init-custom-defuns)
(require 'init-splash-screen)
(require 'init-navigation)

;; package-specific
(require 'init-package-magit)
(require 'init-package-git-gutter)
(require 'init-package-multiple-cursors)
;; (require 'init-package-helm-ag)
;; (require 'init-package-ag)
;; (require 'init-package-swiper)
(require 'init-package-undo-tree)

;; (require 'init-project-utils)
(require 'init-view-mode)
(require 'init-ruby)
(require 'init-ssh)
(require 'init-elixir)
(require 'init-clojure)
(require 'init-rust)
(require 'init-yasnippet)
(require 'init-dired)
(require 'init-javascript)
(require 'init-eshell)
(require 'init-sql)
(require 'init-docker)
(require 'init-restclient)
;;(require 'init-parinfer)
(require 'init-web-mode)

(require 'init-appearance)
(require 'init-keybindings)
(require 'init-org)
(require 'init-lsp)

; (require 'init-package-diminish) ;; Should stay at last position

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(custom-safe-themes
   '("ca1b398ceb1b61709197478dc7f705b8337a0a9631e399948e643520c5557382" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d" "4eb9462a8fff9153bfe88a9ef53aa043aec8b79c5298d2873e887e0c3a8b03de" "0c860c4fe9df8cff6484c54d2ae263f19d935e4ff57019999edbda9c7eda50b8" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "1b623b81f373d49bcf057315fe404b30c500c3b5a387cf86c699d83f2f5763f4" "d43860349c9f7a5b96a090ecf5f698ff23a8eb49cd1e5c8a83bb2068f24ea563" "5ec088e25ddfcfe37b6ae7712c9cb37fd283ea5df7ac609d007cafa27dab6c64" "0f220ea77c6355c411508e71225680ecb3e308b4858ef6c8326089d9ea94b86f" "75d807376ac43e6ac6ae3892f1f377a4a3ad2eb70b14223b4ed0355e62116814" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a655f17225ad0a7190c79602593563191b7640ddebbb8c8fbd80c9d82faff1c6" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" default))
 '(initial-buffer-choice t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq debug-on-error t)
(setq debug-on-quit t)
(toggle-debug-on-quit)
