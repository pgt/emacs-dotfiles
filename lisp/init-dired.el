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

;;; Always recursively delete directory
(setq dired-recursive-deletes 'always)

;;; Always recursively copy directory
(setq dired-recursive-copies 'always)

;;; Delete by moving to trash
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash")

;;; Open by the default Mac programs
(defun open-default-mac-program ()
	(interactive)
	(save-window-excursion
	  (let ((files (dired-get-marked-files nil current-prefix-arg))
			command)
		;; the open command
		(setq command "open ")
		(dolist (file files)
		  (setq command (concat command (shell-quote-argument file) " ")))
		(message command)
		;; execute the command
		(async-shell-command command))))
(define-key dired-mode-map (kbd "C-M-o") 'open-default-mac-program)

(defun open-current-directory-in-finder ()
	"Open the current directory in Finder"
	(interactive)
	(save-window-excursion
	  (dired-do-async-shell-command
	   "open .")))
(define-key dired-mode-map (kbd "C-S-o") 'open-current-directory-in-finder)

(provide 'init-dired)
;;; init-dired.el ends here
