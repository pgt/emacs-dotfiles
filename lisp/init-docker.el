;;; init-docker.el -- Configures `docker-mode' preferences.
;;; Commentary:
;;; Code:

;;; Use dockerfile-mode for `Dockerfile'
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;;; Docker configs
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
;; Use "docker-machine env box" command to find out your environment variables
(setenv "DOCKER_TLS_VERIFY" "1")
(setenv "DOCKER_HOST" "tcp://10.11.12.13:2376")
(setenv "DOCKER_CERT_PATH" "/Users/pgt/.docker/machine/machines/box")
(setenv "DOCKER_MACHINE_NAME" "box")

(provide 'init-docker)
;;; init-docker.el ends here
