;;; init-javascript.el -- Configures nice-to-have features for JS development.
;;; Commentary:
;;; Code:

;; indent js files with 2 spaces
(setq js-indent-level 2)

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;; To set the amount of syntax highlighting to perform, change the
;; value of the variable to the level that you want
(setq js2-highlight-level 3)

;;; JS2-Refactor
(add-hook 'js2-mode-hook #'js2-refactor-mode)

(add-to-list 'magic-mode-alist '("import React.+" . rjsx-mode))

(provide 'init-javascript)
;;; init-javascript.el ends here
