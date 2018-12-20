;;; init-rust.el -- Configures `rust-mode' preferences.
;;; Commentary:
;;; Code:

(add-to-list 'load-path "/path/to/rust-mode/")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;;; Racer
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

;;; Completion
(add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;;; Rustfmt
(defun my-rust-mode-hooks ()
  (add-hook 'before-save-hook 'rust-format-buffer))

(add-hook 'rust-mode-hook 'my-rust-mode-hooks)

(define-key rust-mode-map (kbd "C-c i") #'rust-format-buffer)

;;; Cargo
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;;; Flycheck
(add-hook 'rust-mode-hook 'flycheck-rust-setup)

(provide 'init-rust)
;;; init-rust.el ends here
