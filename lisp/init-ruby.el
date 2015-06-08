;;; init-ruby.el -- Configures nice-to-have features for Ruby development.
;;; Commentary:
;;; Code:

(require 'rbenv)
(global-rbenv-mode)

(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(setq rspec-use-rake-when-possible nil)
(setq compilation-scroll-output t)

;;;;;;;;; autocomplete
(require 'auto-complete-config)
(ac-config-default)

(setq ac-auto-start 4)
(setq ac-auto-show-menu 0.5)
(setq ac-ignore-case nil)

(add-hook 'ruby-mode-hook
          (lambda ()
            (rbenv-use-corresponding)
            (make-local-variable 'ac-ignores)
            ;; ruby keywords
            (add-to-list 'ac-ignores "do")
            (add-to-list 'ac-ignores "end")
            (add-to-list 'ac-ignores "begin")
            (add-to-list 'ac-ignores "true")
            (add-to-list 'ac-ignores "false")
            (add-to-list 'ac-ignores "for")
            (add-to-list 'ac-ignores "rescue")
            (add-to-list 'ac-ignores "fail")
            (add-to-list 'ac-ignores "while")))

(define-key ac-complete-mode-map "\C-n" 'ac-next)

;;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; Rubocop
(add-hook 'ruby-mode-hook 'rubocop-mode)

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

;;; Ruby Refactor
(require 'ruby-refactor)
(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)

(provide 'init-ruby)
;;; init-ruby.el ends here
