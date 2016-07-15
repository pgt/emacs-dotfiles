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

(provide 'init-sql)
;;; init-sql.el ends here
