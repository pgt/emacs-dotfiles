;;; init-go.el -- Configures `go-mode' preferences.
;;; Commentary:
;;; Code:

(defvar go-ts-mode-hook nil)

(defun my/go--bootstrap-env ()
  (setenv "GOPATH" (concat (getenv "HOME") "/gocode"))
  (add-to-list 'exec-path "/usr/local/go/bin")
  (add-to-list 'exec-path (concat (getenv "HOME") "/gocode/bin")))

(defun my/go--install-shared-keybindings (map)
  (define-key map (kbd "C-x f") #'go-test-current-file)
  (define-key map (kbd "C-x t") #'go-test-current-test)
  (define-key map (kbd "C-x p") #'go-test-current-project)
  (define-key map (kbd "C-x x") #'go-run))

(dolist (feature '(go-mode go-ts-mode))
  (with-eval-after-load feature
    (my/go--bootstrap-env)
    (let* ((map-symbol (intern (format "%s-map" feature)))
           (map (and (boundp map-symbol) (symbol-value map-symbol))))
      (when map
        (my/go--install-shared-keybindings map)))))

(defun my/go--format-before-save ()
  (add-hook 'before-save-hook #'gofmt-before-save nil t))

(defun my/go--local-tools ()
  (local-set-key (kbd "C-c C-r") #'go-remove-unused-imports)
  (local-set-key (kbd "C-c C-g") #'go-goto-imports)
  (local-set-key (kbd "C-c C-f") #'gofmt)
  (local-set-key (kbd "C-c C-k") #'godoc))

(dolist (hook '(go-mode-hook go-ts-mode-hook))
  (add-hook hook #'my/go--format-before-save)
  (add-hook hook #'my/go--local-tools)
  (add-hook hook #'company-mode))

;;; Go run
(defun go-run-buffer ()
  "Run the current Go buffer."
  (interactive)
  (shell-command (concat "go run " (buffer-name))))

(provide 'init-go)
;;; init-go.el ends here
