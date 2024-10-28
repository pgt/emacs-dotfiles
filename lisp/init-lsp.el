;; ;;; init-lsp.el --- LSP Config File
;; ;;; Commentary:
;; ;;; Code:
;; ;;
;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook (
;;          (ruby-mode . lsp)
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; (provide 'init-lsp)
;; ;;; init-lsp.el ends here
