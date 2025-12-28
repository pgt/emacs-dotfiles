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
(defun my/magit-status-fullscreen (orig-fun &rest args)
  (window-configuration-to-register :magit-fullscreen)
  (apply orig-fun args)
  (delete-other-windows))

(advice-add 'magit-status :around #'my/magit-status-fullscreen)

(global-set-key (kbd "C-c y") #'magit-status)

;; Github integrations of Pull requests
;;(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

(provide 'init-package-magit)
;;; init-package-magit.el ends here
