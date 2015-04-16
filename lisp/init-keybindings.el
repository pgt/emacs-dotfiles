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

;; Navigation
(global-set-key (kbd "C-j") 'backward-word)
(global-set-key (kbd "C-k") 'forward-word)

(global-set-key (kbd "C-o") 'kill-line) ;; kill line

;; Magit
(global-set-key (kbd "C-c y") 'magit-status)

(global-set-key (kbd "C-c C-f") 'helm-projectile-find-file)

(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "M-d") 'kill-word)
(global-set-key (kbd "M-h") 'backward-kill-word)

(global-set-key (kbd "M-/") 'auto-complete)

(global-set-key [(control shift return)] 'smart-open-line-above)

(global-set-key (kbd "s-d") 'duplicate-line)

(global-set-key (kbd "C-c e") 'rr/split-module-nesting)

(global-set-key (kbd "C-c i") 'indent-region-or-buffer)

;;;;;; keybindings
(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "C-q") 'comment-or-uncomment-region)
(global-set-key (kbd "M-d") 'kill-word)
(global-set-key (kbd "M-h") 'backward-kill-word)

;;; Change window with C-tab
(global-set-key [C-tab]
		(lambda ()
		  (interactive)
		  (other-window -1)))
(global-set-key (kbd "M-i") 'other-frame)
(global-set-key (kbd "s-K") 'delete-window)
(global-set-key (kbd "s-O") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "s-U") 'vsplit-last-buffer)
(global-set-key (kbd "s-P") 'hsplit-last-buffer)
(global-set-key (kbd "s-I") 'swap-buffers-in-windows)
(global-set-key (kbd "C-x 9") 'other-window-kill-buffer)
(global-set-key [s-shift-left] 'windmove-left) ;; will not work
(global-set-key [s-shift-right] 'windmove-right) ;; will not work
(global-set-key [s-shift-up] 'windmove-up)
(global-set-key [s-shift-down] 'windmove-down)

;; Find in project
(global-set-key (kbd "s-F") 'ag-project)

;; Smart kill-whole-line
(global-set-key [remap kill-whole-line] 'smart-kill-whole-line)

;; Dired open Wdired
(define-key dired-mode-map (kbd "e") 'wdired-change-to-wdired-mode)

;; Open buffer of this project
(global-set-key (kbd "M-L") 'helm-projectile-switch-to-buffer)

;; Open project
(global-set-key (kbd "s-o") 'helm-rr-open-project)

;; Eval-buffer
(define-key emacs-lisp-mode-map (kbd "C-c C-v") 'eval-buffer)

(provide 'init-keybindings)
;;; init-keybindings.el ends here