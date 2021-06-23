;;; init-elixir.el -- Configures emacs as a IDE ELIXIR client
;;; Commentary:
;;; Code:

;;; Enable alchemist
(add-hook 'elixir-mode-hook 'alchemist-mode-hook)

(setq alchemist-mix-command "~/.asdf/shims/mix")
(setq alchemist-iex-program-name "~/.asdf/shims/iex") ;; default: iex
(setq alchemist-execute-command "~/.asdf/shims/elixir") ;; default: elixir
(setq alchemist-compile-command "~/.asdf/shims/elixirc") ;; default: elixirc

;; Keybindings
(define-key alchemist-mode-map (kbd "C-c , t") 'alchemist-project-toggle-file-and-tests)
(define-key alchemist-mode-map (kbd "C-c , y") 'alchemist-project-toggle-file-and-tests-other-window)
(define-key alchemist-mode-map (kbd "C-c , a") 'alchemist-mix-test)
(define-key alchemist-mode-map (kbd "C-c , s") 'alchemist-mix-test-at-point)
(define-key alchemist-mode-map (kbd "C-c , v") 'alchemist-project-run-tests-for-current-file)
(define-key alchemist-mode-map (kbd "C-c , r") 'alchemist-mix-rerun-last-test)
(define-key alchemist-mode-map (kbd "C-c , c") 'alchemist-mix-compile)
(define-key alchemist-mode-map (kbd "C-c ?") 'alchemist-help-search-at-point)

;; To use ruby end mode
(add-to-list 'elixir-mode-hook
             (defun auto-activate-ruby-end-mode-for-elixir-mode ()
               (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
               (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
               (ruby-end-mode +1)))

;; Comment here
;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

;; Flycheck configs
(eval-after-load 'flycheck
  '(flycheck-credo-setup))
(add-hook 'elixir-mode-hook 'flycheck-mode)

(setq flycheck-elixir-credo-strict t)

(provide 'init-elixir)
;;; init-elixir.el ends here
