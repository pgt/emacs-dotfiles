;;; init-javascript.el -- Configures nice-to-have features for JS development.
;;; Commentary:
;;; Code:

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;; To set the amount of syntax highlighting to perform, change the
;; value of the variable to the level that you want
(setq js2-highlight-level 3)

(require 'flycheck)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

;;; JS2-Refactor
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)

;;; Tern
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

(add-hook 'jsx-mode-hook (lambda () (tern-mode t))) ;; Using Tern with JSX mode

(defun delete-tern-process ()
  "Fix error when tern does not auto refresh"
  (interactive)
  (delete-process "Tern"))

(provide 'init-javascript)
;;; init-javascript.el ends here
