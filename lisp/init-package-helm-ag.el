;;; init-package-helm-ag.el --
;;; Commentary:
;;; Code:

(setq helm-ag-base-command "/usr/local/bin/ag --nocolor --nogroup --ignore-case")
(setq helm-ag-command-option "--all-text")
(setq helm-ag-insert-at-point 'symbol)
(defun projectile-helm-ag ()
  (interactive)
  (helm-ag (projectile-project-root)))

(provide 'init-package-helm-ag)
;;; init-package-helm-ag.el ends here
