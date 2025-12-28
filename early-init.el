;;; early-init.el --- Early initialization for Emacs 30+
;;; Commentary:
;;; Ensure package.el does not load when using straight.el
;;; Code:

(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
