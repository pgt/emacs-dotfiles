;;; init-project-utils.el -- Personal configuration and enhancements to project management.
;;; Commentary:
;;; Code:

(projectile-global-mode 1)

(defvar default-project-source
  "~/code")

;; =====================================
;; -- extensions to projectile keymap --
;; =====================================

;; The new projectile changes this keymap to C-c C-p, so to keep my
;; old keys I made this
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(let ((map projectile-command-map))
  ;; general utils
  (define-key map "n" 'rr-show-file-name)
  (define-key map "\C-n" 'rr-new-git-project)
  (define-key map "\C-g" 'rr-add-gitignore-file)

  (define-key map "m" 'git-timemachine)

  (define-key map "y" 'projectile-find-implementation-or-test-other-window)
  (define-key map "a" 'projectile-test-project)

;; Projectile enable caching
(setq projectile-enable-caching t)

(provide 'init-project-utils)
;;; init-project-utils.el ends here
