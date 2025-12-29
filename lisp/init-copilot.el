;;; init-copilot.el -- GitHub Copilot configuration.
;;; Commentary:
;;; Code:

(require 'copilot)

(setq copilot-max-char 300000)
(setq copilot-indent-offset-warning-disable t)
(add-to-list 'copilot-indentation-alist '(prog-mode standard-indent))
(add-to-list 'copilot-indentation-alist '(text-mode standard-indent))

(defun my/copilot-setup ()
  "Enable Copilot in programming buffers and set keybindings."
  (copilot-mode 1))

(add-hook 'prog-mode-hook #'my/copilot-setup)
(add-hook 'yaml-mode-hook #'my/copilot-setup)
(add-hook 'web-mode-hook #'my/copilot-setup)

(with-eval-after-load 'copilot
  (define-key copilot-completion-map (kbd "C-<return>") #'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-M-<return>") #'copilot-accept-completion-by-word)
  (define-key copilot-completion-map (kbd "M-[") #'copilot-previous-completion)
  (define-key copilot-completion-map (kbd "M-]") #'copilot-next-completion))

(provide 'init-copilot)
;;; init-copilot.el ends here
