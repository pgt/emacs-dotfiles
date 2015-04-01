;;; init-magit.el -- Configures the amazing and magic magit interface to git.
;;; Commentary:
;;; Code:

;;; Delete trailing space automatically on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Auto reload buffer when file change
;;; Necessary when switch between branches of git
(global-auto-revert-mode t)


;;; init-magit.el ends here
