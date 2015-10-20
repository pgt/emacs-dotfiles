;;; init-appearance.el -- Configures global keybindings and other keybinding-related stuff.
;;; Commentary:
;;; Code:

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;; theme
(load-theme 'gruvbox t)

;; Cursor type
(setq default-cursor-type 'bar)

;; Put transparency in other frame not used
;; (set-frame-parameter (selected-frame) 'alpha '(100 95))
;; (add-to-list 'default-frame-alist '(alpha 100 90))

;; special font option
(defun custom/use-smaller-font ()
  "Make font smaller for current buffer."
  (interactive)
  (setq buffer-face-mode-face '(:family "Inconsolata" :height 120))
  (buffer-face-mode))

;;; When using the compilation-mode, for example on rspec-mode use a
;;; smaller font to see more information
(add-hook 'compilation-mode-hook 'custom/use-smaller-font)

(provide 'init-appearance)
;;; init-appearance.el ends here
