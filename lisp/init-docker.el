;;; init-docker.el -- Configures `docker-mode' preferences.
;;; Commentary:
;;; Code:

;;; Use dockerfile-mode for `Dockerfile'
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(provide 'init-docker)
;;; init-docker.el ends here
