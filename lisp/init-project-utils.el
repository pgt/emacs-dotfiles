;;; init-project-utils.el -- Personal configuration and enhancements to project management.
;;; Commentary:
;;; Code:
(require 'helm-projectile)

(projectile-global-mode 1)

(defvar default-project-source
  "~/code")

(defvar project-sources
  (list
   default-project-source
   "~/gocode/src/github.com/pgt"
   "~/gocode/src/bitbucket.org/pgtnetwork"))

;; helm integration for opening projects
(defun helm-rr-open-project ()
  "Bring up a Project search interface in helm."
  (interactive)
  (helm :sources '(helm-source-list-projects)
     :buffer "*helm-list-projects*"))

(defvar helm-source-list-projects
  '((name . "Open Project")
    (volatile)
    (delayed)
    (candidates . rr-list-projects)
    (action-transformer . rr-open-project)))

(defun rr-list-projects ()
  "Lists all projects given project sources."
  (cl-labels ((dir-to-files (dir)
                   (if (file-exists-p dir)
                    (directory-files dir t directory-files-no-dot-files-regexp)))
           (flatten (x)
                 (cond ((null x) nil)
                    ((listp x) (append (car x) (flatten (cdr x)))))))
    (progn (flatten (mapcar #'dir-to-files  project-sources)))))

(defun rr-open-project (actions path)
  "Do nothing with ACTIONS. Open project given PATH."
  ;; TODO: Add default file get.
  (cl-flet ((find-default-file () (if (file-exists-p (expand-file-name "Gemfile" path))
                          (expand-file-name "Gemfile" path)
                        path)))
    (find-file (find-default-file))))

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

  ;; ag
  (define-key map "s" 'projectile-helm-ag)
  (define-key map "\C-s" 'ag-project-regexp)

  (define-key map "h" 'hl-highlight-thingatpt-local)
  (define-key map "u" 'hl-unhighlight-all-local)

  (define-key map "y" 'projectile-find-implementation-or-test-other-window)
  (define-key map "a" 'projectile-test-project)
  (define-key map "F" 'helm-projectile-find-file-in-known-projects))

;; Projectile enable caching
(setq projectile-enable-caching t)

(provide 'init-project-utils)
;;; init-project-utils.el ends here
