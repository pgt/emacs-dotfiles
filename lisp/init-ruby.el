;;; init-ruby.el -- Configures nice-to-have features for Ruby development.
;;; Commentary:
;;; Code:

(require 'rbenv)
(global-rbenv-mode)

(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(setq rspec-use-rake-when-possible nil)
(setq compilation-scroll-output t)

;;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; Rubocop
(add-hook 'ruby-mode-hook 'rubocop-mode)

;;; Mutant
(add-hook 'ruby-mode-hook 'mutant-mode)

;; Ruby-mode :: Dont add utf-8 coding to files
(setq ruby-insert-encoding-magic-comment nil)

;; identation fixes
(setq
 ruby-align-chained-calls nil
 ruby-align-to-stmt-keywords nil
 ruby-deep-indent-paren nil
 ruby-deep-indent-paren-style nil
 ruby-use-smie nil)

(defadvice ruby-indent-line (after unindent-closing-paren activate)
  "Ensure desired behaviour for parenthesis indentation."
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))


(provide 'init-ruby)
;;; init-ruby.el ends here
