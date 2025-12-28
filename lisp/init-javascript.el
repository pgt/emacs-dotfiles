;;; init-javascript.el -- Configures nice-to-have features for JS development.
;;; Commentary:
;;; Code:

;; Indent js files with 2 spaces
(setq js-indent-level 2)

;; Enable js2-minor-mode for enhanced JS support
(add-hook 'js-mode-hook #'js2-minor-mode)

;; Syntax highlighting level
(setq js2-highlight-level 3)

;;; JS2-Refactor
(add-hook 'js2-mode-hook #'js2-refactor-mode)

(provide 'init-javascript)
;;; init-javascript.el ends here
