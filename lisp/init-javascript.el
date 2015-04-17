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

(provide 'init-javascript)
;;; init-javascript.el ends here
