;;; init-package-diminish.el ---
;;; Commentary:
;;; Code:

(require 'diminish)
(eval-after-load "flycheck" '(diminish 'flycheck-mode))
(eval-after-load "helm" '(diminish 'helm-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "rubocop" '(diminish 'rubocop-mode))
(eval-after-load "robe" '(diminish 'robe-mode))
(eval-after-load "rspec-mode" '(diminish 'rspec-mode))
(eval-after-load "rainbow-mode" '(diminish 'rainbow-mode))
(eval-after-load "git-gutter-mode" '(diminish 'git-gutter-mode))
(eval-after-load "google-this-mode" '(diminish 'google-this-mode))
(eval-after-load "anzu-mode" '(diminish 'anzu-mode))

(diminish 'yas-minor-mode)
(diminish 'undo-tree-mode)
(diminish 'smartparens-mode)
(diminish 'git-gutter-mode)
(diminish 'anzu-mode)
(diminish 'company-mode)
(diminish 'google-this-mode)
(diminish 'ruby-refactor-mode)
(diminish 'guide-key-mode)
(diminish 'abbrev-mode)
(diminish 'hl-highlight-mode)
(diminish 'hl-paren-mode)

(provide 'init-package-diminish)
;;; init-package-diminish.el ends here
