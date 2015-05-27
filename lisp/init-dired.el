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

;;; Mark files and show the amount size
(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
		 (match-string 1))))))

(define-key dired-mode-map (kbd "?") 'dired-get-size)

;;; Show Kbytes on dired
(setq dired-listing-switches "-alh")

;;; Open dired with `a' key
(put 'dired-find-alternate-file 'disabled nil)

(provide 'init-dired)
;;; init-dired.el ends here
