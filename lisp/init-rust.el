;;; init-rust.el -- Configures `rust-mode' preferences.
;;; Commentary:
;;; Code:

(defvar rust-ts-mode-hook nil)

(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(defun my/rust--install-shared-keybindings (map)
  (define-key map (kbd "TAB") #'company-indent-or-complete-common)
  (define-key map (kbd "C-c i") #'rust-format-buffer))

(dolist (feature '(rust-mode rust-ts-mode))
  (with-eval-after-load feature
    (let* ((map-symbol (intern (format "%s-map" feature)))
           (map (and (boundp map-symbol) (symbol-value map-symbol))))
      (when map
        (my/rust--install-shared-keybindings map)))))

(setq company-tooltip-align-annotations t)

(defun my/rust--format-before-save ()
  (add-hook 'before-save-hook #'rust-format-buffer nil t))

(dolist (hook '(rust-mode-hook rust-ts-mode-hook))
  (add-hook hook #'my/rust--format-before-save)
  (add-hook hook #'company-mode)
  (add-hook hook #'cargo-minor-mode))

(provide 'init-rust)
;;; init-rust.el ends here

