;;; init-appearance.el -- Configures global keybindings and other keybinding-related stuff.
;;; Commentary:
;;; Code:

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;; theme
(load-theme 'gruvbox t)

;;; Powerline
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)

;; Set font
(set-default-font "Source Code Pro")

;; Cursor type
(setq default-cursor-type 'bar)

;; Put trasnparency in other buffer not used
(set-frame-parameter (selected-frame) 'alpha '(100 65))
(add-to-list 'default-frame-alist '(alpha 100 65))

(provide 'init-appearance)
;;; init-appearance.el ends here
