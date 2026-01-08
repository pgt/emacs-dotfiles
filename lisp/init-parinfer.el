;;; init-parinfer.el -- Configures `parinfer-mode' preferences.
;;; Commentary:
;;; Code:

(autoload 'parinfer-mode "parinfer" nil t)

(defvar emacs-lisp-ts-mode-hook nil)
(defvar clojure-ts-mode-hook nil)
(defvar scheme-ts-mode-hook nil)
(defvar lisp-data-mode-hook nil)

(defvar my/parinfer-lisp-hooks
  '(emacs-lisp-mode-hook
    emacs-lisp-ts-mode-hook
    lisp-mode-hook
    lisp-data-mode-hook
    lisp-interaction-mode-hook
    common-lisp-mode-hook
    scheme-mode-hook
    scheme-ts-mode-hook
    clojure-mode-hook
    clojure-ts-mode-hook)
  "Hooks that should enable `parinfer-mode'.")

(dolist (hook my/parinfer-lisp-hooks)
  (add-hook hook #'parinfer-mode))

(with-eval-after-load 'parinfer
  (setq parinfer-extensions '(defaults smart-tab smart-yank)))

(provide 'init-parinfer)
;;; init-parinfer.el ends here
