;;; init-rust.el -- Configures `rust-mode' preferences.
;;; Commentary:
;;; Code:

(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (define-key rust-mode-map (kbd "C-c i") #'rust-format-buffer))

(setq company-tooltip-align-annotations t)

;;; Rustfmt on save
(add-hook 'rust-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'rust-format-buffer nil t)))

;;; Company-mode for completion
(add-hook 'rust-mode-hook #'company-mode)

;;; Cargo
(add-hook 'rust-mode-hook #'cargo-minor-mode)

(provide 'init-rust)
;;; init-rust.el ends here
