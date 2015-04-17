;;; init-dired.el -- Sets default global configurations and general Emacs behavior.
;;; Commentary:
;;; Code:

;; Reload dired after making changes
(--each '(dired-do-rename
          dired-do-copy
          dired-create-directory
          wdired-abort-changes)
        (eval `(defadvice ,it (after revert-buffer activate)
                 (revert-buffer))))

(provide 'init-dired)
;;; init-dired.el ends here
