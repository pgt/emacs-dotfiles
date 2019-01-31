;;; init-appearance.el -- Configures global keybindings and other keybinding-related stuff.
;;; Commentary:
;;; Code:

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq
 ;; better startup
 inhibit-splash-screen t
 inhibit-startup-message t
 ;; show column number at bottom bar
 column-number-mode t
 ;; disable anoying beep
 ring-bell-function 'ignore
 ;; improve rendering performance
 redisplay-dont-pause t
 )

;;; theme
(load-theme 'solarized-dark t)

;; Set font
(set-default-font "B612 Mono")

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

;; modeline
(defun branch-name ()
  ;; for powerline patched fonts, the unicode char \ue0a0 is cooler!
  (when vc-mode (concat "@ " (substring vc-mode 5))))

(setq-default mode-line-format
              (list
               ;; add padding to mode-line (hacky solution)
               (propertize "\u200b" 'display '((raise -0.15) (height 1.2)))
               "[" mode-line-modified "]"
               "  "
               (propertize "%b" 'face 'bold)
               "  |  "
               'mode-name
               "  |  "
               '(:eval (projectile-project-name))
               " "
               '(:eval (branch-name))
               "  |  "
               "%p (%l:%c)"
               ))

;; default window size
(when window-system (set-frame-size (selected-frame) 140 35))

;; fix theme switching
(defadvice load-theme (before smooth-theme-switching activate)
  (ad-set-arg 1 t)
  (mapcar #'disable-theme custom-enabled-themes))

;; nice fonts in OS X
(setq mac-allow-anti-aliasing t)

(provide 'init-appearance)
;;; init-appearance.el ends here
