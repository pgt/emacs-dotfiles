;;; init-go.el -- Configures `go-mode' preferences.
;;; Commentary:
;;; Code:

(with-eval-after-load 'go-mode
  (setenv "GOPATH" (concat (getenv "HOME") "/gocode"))
  (setq exec-path (cons "/usr/local/go/bin" exec-path))
  (add-to-list 'exec-path (concat (getenv "HOME") "/gocode/bin"))

  (define-key go-mode-map (kbd "C-x f") #'go-test-current-file)
  (define-key go-mode-map (kbd "C-x t") #'go-test-current-test)
  (define-key go-mode-map (kbd "C-x p") #'go-test-current-project)
  (define-key go-mode-map (kbd "C-x x") #'go-run))

;;; Formatting
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'gofmt-before-save nil t)))

;;; Go tools keybindings
(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-r") #'go-remove-unused-imports)
            (local-set-key (kbd "C-c C-g") #'go-goto-imports)
            (local-set-key (kbd "C-c C-f") #'gofmt)
            (local-set-key (kbd "C-c C-k") #'godoc)))

;;; Company-mode for completion
(add-hook 'go-mode-hook #'company-mode)

;;; Go run
(defun go-run-buffer ()
  "Run the current Go buffer."
  (interactive)
  (shell-command (concat "go run " (buffer-name))))

(provide 'init-go)
;;; init-go.el ends here
