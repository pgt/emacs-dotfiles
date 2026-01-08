;;; init-tree-sitter.el --- Tree-sitter plumbing across languages -*- lexical-binding: t; -*-
;;; Commentary:
;;; Centralizes Tree-sitter bootstrapping so language modules can rely on
;;; `*-ts-mode' variants when available.
;;; Code:

(require 'treesit-auto nil t)

(when (and (fboundp 'treesit-available-p)
           (treesit-available-p)
           (featurep 'treesit-auto))
  (setq treesit-font-lock-level 4
        treesit-auto-install 'prompt
        treesit-auto-languages '(bash c c++ cmake css go html javascript json
                                   python ruby rust toml tsx typescript yaml))

  (treesit-auto-add-to-auto-mode-alist 'all)
  (when (fboundp 'treesit-auto-apply-remap)
    (treesit-auto-apply-remap 'simple))
  (global-treesit-auto-mode)

  (defvar my/treesit-mode-remap-table
    '((go-mode . go-ts-mode)
      (python-mode . python-ts-mode)
      (rust-mode . rust-ts-mode)
      (ruby-mode . ruby-ts-mode)
      (js-mode . js-ts-mode)
      (typescript-mode . typescript-ts-mode)
      (json-mode . json-ts-mode)
      (css-mode . css-ts-mode)
      (sh-mode . bash-ts-mode)
      (yaml-mode . yaml-ts-mode))
    "Modes that should transparently open in their Tree-sitter counterparts.")

  (dolist (mapping my/treesit-mode-remap-table)
    (let ((from (car mapping))
          (to   (cdr mapping)))
      (when (and (fboundp to)
                 (not (assoc from major-mode-remap-alist)))
        (add-to-list 'major-mode-remap-alist mapping))))

  (when (require 'combobulate nil t)
    (setq combobulate-key-prefix "C-c o")
    (dolist (hook '(python-ts-mode-hook
                    js-ts-mode-hook
                    tsx-ts-mode-hook
                    typescript-ts-mode-hook
                    css-ts-mode-hook
                    json-ts-mode-hook
                    yaml-ts-mode-hook))
      (add-hook hook #'combobulate-mode)))

  (message "Tree-sitter enabled for %s" treesit-auto-languages))

(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
