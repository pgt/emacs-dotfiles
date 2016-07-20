;;; init-elixir.el -- Configures emacs as a IDE ELIXIR client
;;; Commentary:
;;; Code:

;; Flycheck!
(flycheck-mix-setup)

;;; Enable alchemist
(add-hook 'elixir-mode-hook 'alchemist-mode)

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

(provide 'init-elixir)
;;; init-elixir.el ends here
