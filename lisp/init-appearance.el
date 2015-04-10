;;; init-appearance.el -- Configures global keybindings and other keybinding-related stuff.
;;; Commentary:
;;; Code:

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;; theme
(load-theme 'gruvbox t)

;;; Powerline
(add-to-list 'load-path "~/.emacs.d/vendor/powerline")
(require 'powerline)
(setq projectile-mode-line
      '(:eval (format " [%s]" (projectile-project-name))))

;;; Diminish
(require 'diminish)
(eval-after-load "flycheck" '(diminish 'flycheck-mode))
(eval-after-load "helm" '(diminish 'helm-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "rubocop" '(diminish 'rubocop-mode))
(eval-after-load "robe" '(diminish 'robe-mode))
(eval-after-load "rspec-mode" '(diminish 'rspec-mode))
(eval-after-load "rainbow-mode" '(diminish 'rainbow-mode))
(eval-after-load "git-gutter-mode" '(diminish 'git-gutter-mode))
(eval-after-load "google-this-mode" '(diminish 'google-this-mode))
(eval-after-load "anzu-mode" '(diminish 'anzu-mode))
(eval-after-load "magit" '(diminish 'magit-auto-revert-mode))
(diminish 'yas-minor-mode)
(diminish 'auto-complete-mode)
(diminish 'undo-tree-mode)
(diminish 'smartparens-mode)
(diminish 'git-gutter-mode)
(diminish 'anzu-mode)
(diminish 'google-this-mode)

;; Cursor type
(setq default-cursor-type 'bar)

;; Put trasnparency in other frame not used
(set-frame-parameter (selected-frame) 'alpha '(100 65))
(add-to-list 'default-frame-alist '(alpha 100 65))

(provide 'init-appearance)
;;; init-appearance.el ends here
