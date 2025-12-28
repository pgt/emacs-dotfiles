;;; init-typescript.el -- TypeScript and TSX configuration.
;;; Commentary:
;;; Code:

(defvar my/typescript-backend 'tide
  "Preferred backend for TypeScript buffers. Accepts `tide or `lsp.")

(defvar typescript-ts-mode-hook nil)
(defvar tsx-ts-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(setq typescript-indent-level 2)

(defun my/typescript--enable-completion-and-linting ()
  "Enable completion and linting helpers used across TypeScript buffers."
  (company-mode 1)
  (flycheck-mode 1))

(defun my/typescript--ensure-tide-before-save ()
  "Format buffers with Tide before saving when available."
  (when (bound-and-true-p tide-mode)
    (tide-format-before-save)))

(defun my/typescript--setup-tide ()
  "Configure Tide as the active backend."
  (tide-setup)
  (tide-hl-identifier-mode 1)
  (setq tide-format-options '((indentSize . 2)
                              (tabSize . 2)))
  (add-hook 'before-save-hook #'my/typescript--ensure-tide-before-save nil t))

(defun my/typescript--setup-backend ()
  "Choose the appropriate TypeScript backend."
  (pcase my/typescript-backend
    ('tide (my/typescript--setup-tide))
    ('lsp  (remove-hook 'before-save-hook #'my/typescript--ensure-tide-before-save t)
           (lsp-deferred))
    (_     (my/typescript--setup-tide))))

(defun my/typescript--buffer-init ()
  "Common initialization shared by TypeScript-family buffers."
  (my/typescript--enable-completion-and-linting)
  (my/typescript--setup-backend))

(dolist (hook '(typescript-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook))
  (add-hook hook #'my/typescript--buffer-init))

(with-eval-after-load 'web-mode
  (defun my/typescript--maybe-setup-web-mode ()
    "Configure TSX buffers that rely on web-mode."
    (when (and buffer-file-name
               (string-equal (file-name-extension buffer-file-name) "tsx"))
      (setq-local web-mode-code-indent-offset 2
                  web-mode-markup-indent-offset 2)
      (my/typescript--buffer-init)))
  (add-hook 'web-mode-hook #'my/typescript--maybe-setup-web-mode))

(provide 'init-typescript)
;;; init-typescript.el ends here
