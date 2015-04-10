;;; init-ruby.el -- Configures nice-to-have features for Ruby development.
;;; Commentary:
;;; Code:

;;; RVM
(require 'rvm)
(rvm-use-default) ;;; use rvm’s default ruby for the current Emacs session

;;; rspec-mode
(require 'rspec-mode)
(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)

(setq-default rspec-use-rvm t)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(setq rspec-use-rake-when-possible nil)

;;;;;;;;; autocomplete
(require 'auto-complete-config)
(ac-config-default)

(setq ac-auto-start 4)
(setq ac-auto-show-menu 0.5)
(setq ac-ignore-case nil)

(add-hook 'ruby-mode-hook
          (lambda ()
	    (rvm-activate-corresponding-ruby)
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

(provide 'init-ruby)
;;; init-ruby.el ends here
