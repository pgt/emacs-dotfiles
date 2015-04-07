;;; init-appearance.el -- Configures global keybindings and other keybinding-related stuff.
;;; Commentary:
;;; Code:

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;; theme
(load-theme 'gruvbox t)

(custom-set-faces
 '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))

;;; Powerline
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")

(require 'powerline)
(setq powerline-color1 "grey22")
(setq powerline-color2 "grey40")
(setq powerline-arrow-shape 'arrow)   ;; give your mode-line curves

(set-face-attribute 'mode-line nil
                    :foreground "Black"
                    :background "DarkOrange"
                    :box nil)

;; Set font
(set-default-font "Source Code Pro")

;; Cursor type
(setq default-cursor-type 'bar)

;;; init-appearance.el ends here
