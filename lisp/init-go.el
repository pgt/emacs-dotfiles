;;; init-go.el -- Configures `go-mode' preferences.
;;; Commentary:
;;; Code:

(require 'go-mode)
(setenv "GOPATH" (concat (getenv "HOME") "/gocode"))
(setq exec-path (cons "/usr/local/go/bin" exec-path))
(add-to-list 'exec-path (concat (getenv "HOME") "/gocode/bin"))

;;; Formating
(add-hook 'before-save-hook 'gofmt-before-save)

;;; Go tools
(add-hook 'go-mode-hook '(lambda ()
			   (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))

(add-hook 'go-mode-hook '(lambda ()
			   (local-set-key (kbd "C-c C-g") 'go-goto-imports)))

(add-hook 'go-mode-hook '(lambda ()
			   (local-set-key (kbd "C-c C-f") 'gofmt)))

(add-hook 'go-mode-hook '(lambda ()
			   (local-set-key (kbd "C-c C-k") 'godoc)))

;;; Go Oracle
(load "$GOPATH/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")
(add-hook 'go-mode-hook 'go-oracle-mode)

;;; Flymake
;;; TODO: I will use this or flycheck? https://github.com/dougm/goflymake, If so, I need to add on dot-files the goflymake package
(add-to-list 'load-path "$GOPATH/src/github.com/dougm/goflymake")
(require 'go-flymake')

;;; Company-mode - autocomplete
(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
			  (set (make-local-variable 'company-backends) '(company-go))
			  (company-mode)))

;;; Go run
(defun go-run-buffer()
  (interactive)
  (shell-command (concat "go run " (buffer-name))))

(define-key go-mode-map (kbd "C-x f") 'go-test-current-file)
(define-key go-mode-map (kbd "C-x t") 'go-test-current-test)
(define-key go-mode-map (kbd "C-x p") 'go-test-current-project)
(define-key go-mode-map (kbd "C-x x") 'go-run)

(provide 'init-go)
;;; init-go.el ends here
