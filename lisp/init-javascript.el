;;; init-javascript.el -- Configures nice-to-have features for JS development.
;;; Commentary:
;;; Code:

(defvar js-ts-mode-hook nil)

(defun my/js--apply-shared-preferences ()
  "Normalize indentation across JS-derived modes."
  (setq-local js-indent-level 2
              js-switch-indent-offset 2)
  (when (boundp 'js2-highlight-level)
    (setq js2-highlight-level 3))
  (when (boundp 'js-ts-mode-indent-offset)
    (setq-local js-ts-mode-indent-offset 2)))

(add-hook 'js-mode-hook #'my/js--apply-shared-preferences)
(add-hook 'js-ts-mode-hook #'my/js--apply-shared-preferences)

;; js2 goodies remain available whenever `js-mode' is used as a fallback.
(add-hook 'js-mode-hook #'js2-minor-mode)
(add-hook 'js2-mode-hook #'js2-refactor-mode)

(provide 'init-javascript)
;;; init-javascript.el ends here
