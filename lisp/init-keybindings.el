;;; init-keybindings.el -- Configures global keybindings and other keybinding-related stuff.
;;; Commentary:
;;; Code:

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "s-N") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-l") 'helm-buffers-list)
(global-set-key (kbd "s-t") 'helm-projectile-find-file)

(global-set-key (kbd "C-c h") 'helm-command-prefix)

(global-set-key (kbd "s-f") 'isearch-forward)
(global-set-key (kbd "s-/") 'comment-dwim-line)

(global-set-key (kbd "C-j") 'backward-word) ;; navigation
(global-set-key (kbd "C-k") 'forward-word) ;; navigation

(global-set-key (kbd "C-o") 'kill-line) ;; kill line

(global-set-key (kbd "C-c y") 'magit-status)

(global-set-key (kbd "C-c C-f") 'helm-projectile-find-file)

(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "M-d") 'kill-word)
(global-set-key (kbd "M-h") 'backward-kill-word)

(global-set-key (kbd "M-/") 'auto-complete)

(global-set-key [(control shift return)] 'smart-open-line-above)

(global-set-key (kbd "s-d") 'duplicate-line)

(global-set-key (kbd "C-x 9") 'other-window-kill-buffer)

(global-set-key (kbd "C-c e") 'rr/split-module-nesting)

;; History minibuffer navigation
(define-key minibuffer-local-map (kbd "C-p") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "C-n") 'next-complete-history-element)

(global-set-key (kbd "C-c i") 'indent-region-or-buffer)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
