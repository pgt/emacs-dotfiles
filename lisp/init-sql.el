;;; init-sql.el -- Configures emacs as a IDE SQL client
;;; Commentary:
;;; Code:

;; Do not truncate lines, to show tables better
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; My list of postgres servers to connect
(setq sql-connection-alist
      '((postgres-pgt-LOCAL (sql-product 'postgres)
			    (sql-port 5432)
			    (sql-server "localhost")
			    (sql-user "pgt")
			    (sql-password "")
			    (sql-database "pgt"))
        (example (sql-product 'postgres)
		 (sql-port 5432)
		 (sql-server "localhost")
		 (sql-user "user")
		 (sql-password "password")
		 (sql-database "db2"))))

;; Upcase all sql keywords
(defun upcase-sql-keywords ()
    (interactive)
    (save-excursion
      (dolist (keywords sql-mode-postgres-font-lock-keywords)
        (goto-char (point-min))
        (while (re-search-forward (car keywords) nil t)
          (goto-char (+ 1 (match-beginning 0)))
          (when (eql font-lock-keyword-face (face-at-point))
            (backward-char)
            (upcase-word 1)
            (forward-char))))))

(add-hook 'sql-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'upcase-sql-keywords)))


(provide 'init-sql)
;;; init-sql.el ends here
