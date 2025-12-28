;;; init-python.el -- Configures `python-mode' preferences.
;;; Commentary:
;;; Code:

;; Python configuration
(setq python-indent-offset 4
      python-indent-guess-indent-offset nil
      python-shell-interpreter "python3"
      python-shell-completion-native-enable nil)

;; Set proper shell for Python commands
(setq shell-file-name "/opt/homebrew/bin/bash")
(setq explicit-shell-file-name "/opt/homebrew/bin/bash")

;; Python project management configuration
(defun python-detect-project-manager ()
  "Detect if project uses poetry, uv, or falls back to pip."
  (cond
   ((file-exists-p (expand-file-name "pyproject.toml" (projectile-project-root)))
    (cond
     ((file-exists-p (expand-file-name "uv.lock" (projectile-project-root))) "uv")
     ((file-exists-p (expand-file-name "poetry.lock" (projectile-project-root))) "poetry")
     (t "uv"))) ; default to uv for pyproject.toml
   (t "pip")))

(defun python-get-test-command ()
  "Get the appropriate test command based on project manager."
  (let ((manager (python-detect-project-manager)))
    (cond
     ((string= manager "poetry") "poetry run pytest")
     ((string= manager "uv") "uv run pytest")
     (t "pytest"))))

(defun python-get-run-command (cmd)
  "Get the appropriate run command based on project manager."
  (let ((manager (python-detect-project-manager)))
    (cond
     ((string= manager "poetry") (format "poetry run %s" cmd))
     ((string= manager "uv") (format "uv run %s" cmd))
     (t cmd))))

(defvar python-ts-mode-hook nil)

(defun my/python-mode-setup ()
  ;; Enable LSP (already configured in init-packages.el)
  (when (featurep 'lsp-pyright)
    (lsp-deferred))

  ;; Configure python-pytest dynamically
  (when (featurep 'python-pytest)
    (setq-local python-pytest-executable (python-get-test-command)))

  ;; Smart indentation with electric chars
  (setq-local electric-indent-chars (append electric-indent-chars '(?: ?\) ?\] ?\})))

  ;; Better RET behavior
  (local-set-key (kbd "RET") #'python-newline-and-indent)

  ;; Improved C-c i behavior for Python
  (local-set-key (kbd "C-c i") #'python-smart-indent-region-or-buffer)

  ;; Python testing keybindings
  (local-set-key (kbd "C-c , a") #'python-pytest-dispatch)
  (local-set-key (kbd "C-c , v") #'python-pytest-file-dwim)
  (local-set-key (kbd "C-c , s") #'python-pytest-function-dwim))

(defun python-newline-and-indent ()
  "Insert newline and indent intelligently."
  (interactive)
  (newline)
  (python-indent-line)
  ;; Auto-dedent for certain keywords
  (when (python-info-dedenter-statement-p)
    (python-indent-dedent-line)))

(defun python-smart-indent-region-or-buffer ()
  "Smart indentation for Python - format with black or fallback to standard indentation."
  (interactive)
  (cond
   ;; If region is active, indent region
   ((use-region-p)
    (python-indent-region (region-beginning) (region-end))
    (message "Indented selected Python region."))
   ;; If python-black is available, use it
   ((and (featurep 'python-black) (executable-find python-black-command))
    (python-black-buffer)
    (message "Formatted Python buffer with black."))
   ;; Fall back to standard Python indentation
   (t
    (python-indent-buffer)
    (message "Indented Python buffer."))))

(defun python-indent-buffer ()
  "Indent entire Python buffer properly."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (unless (python-info-current-line-empty-p)
        (python-indent-line))
      (forward-line 1))))

;; Custom Python utility functions
(defun python-run-buffer ()
  "Run current Python buffer with appropriate project manager."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (cmd (python-get-run-command (format "python %s" (shell-quote-argument file-name)))))
    (compile cmd)))

(defun python-run-script (script-name)
  "Run a Python script with appropriate project manager."
  (interactive "sScript name: ")
  (let ((cmd (python-get-run-command (format "python %s" script-name))))
    (compile cmd)))

(defun python-install-dependencies ()
  "Install dependencies using the appropriate project manager."
  (interactive)
  (let ((manager (python-detect-project-manager)))
    (cond
     ((string= manager "poetry") (compile "poetry install"))
     ((string= manager "uv") (compile "uv sync"))
     (t (compile "pip install -r requirements.txt")))))

(defun python-show-project-info ()
  "Show information about the current Python project."
  (interactive)
  (let ((manager (python-detect-project-manager))
        (root (projectile-project-root)))
    (message "Python project: %s | Manager: %s | Root: %s"
             (file-name-nondirectory (directory-file-name root))
             manager
             root)))

;; Override compilation-mode settings for Python
(defun my/python-set-compile-command ()
  ;; Set compilation command for Python files
  (setq-local compile-command
              (python-get-run-command
               (format "python %s" (shell-quote-argument (buffer-file-name))))))

(dolist (hook '(python-mode-hook python-ts-mode-hook))
  (add-hook hook #'my/python-mode-setup)
  (add-hook hook #'my/python-set-compile-command))


(provide 'init-python)
;;; init-python.el ends here
