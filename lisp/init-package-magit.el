;;; init-package-magit.el -- Configures the amazing and magic magit interface to git.
;;; Commentary:
;;; Code:

;;; Delete trailing space automatically on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Auto reload buffer when file change
;;; Necessary when switch between branches of git
(global-auto-revert-mode t)

(setq magit-last-seen-setup-instructions "1.4.0")

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(global-set-key (kbd "C-c y") 'magit-status)

;; Github integrations of Pull requests
;;(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;; Use Ctrl-TAB to change buffer
(add-hook 'magit-status-mode-hook
	  (lambda () (local-set-key [(control tab)] 'other-window)))

(provide 'init-package-magit)
;;; init-package-magit.el ends here
